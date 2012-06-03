// Kenton's Code Playground -- http://code.google.com/p/kentons-code
// Author: Kenton Varda (temporal@gmail.com)
// Copyright (c) 2012 Google, Inc. and contributors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <stdexcept>

#include "compiler.h"
#include "CodePrinter.h"

namespace modc {
namespace compiler {

using std::move;
using ast::Expression;
using ast::Statement;

class Compiler {
public:
  Compiler(Context& context, Scope& scope): context(context), scope(scope) {}

  CxxExpression asCxx(Thing&& thing);

  void dereference(Evaluation& eval, errors::Location location) {
    if (eval.result.getKind() == Thing::Kind::BINDING) {
      eval.bindingsUsed[eval.result.binding] |= Evaluation::BindingUsage::IMMUTABLY;
      eval.result = eval.result.binding->useImmutably(location)->get();
    } else if (eval.result.getKind() == Thing::Kind::RUNTIME_LVALUE) {
      for (Binding* binding: eval.result.runtimeLvalue.entangledBindings) {
        binding->useImmutably(location);
        eval.bindingsUsed[binding] |= Evaluation::BindingUsage::IMMUTABLY;
      }

      eval.result = Thing::fromRuntimeRvalue(
          move(eval.result.runtimeLvalue.descriptor),
          move(eval.result.runtimeLvalue.cxxExpression));
    }
  }

  Evaluation castTo(Evaluation&& thing, const Thing& type);
  ValueDescriptor findCommonSupertype(const ValueDescriptor& type1, const ValueDescriptor& type2);

  Thing applyBinaryOperator(Thing&& left, ast::BinaryOperator op, Thing&& right);

  Evaluation applyPrefixOperator(ast::PrefixOperator op, Evaluation&& operand,
                                 errors::Location location) {
    dereference(operand, location);

    switch (op) {
      case ast::PrefixOperator::POSITIVE:
        switch (operand.result.getKind()) {
          case Thing::Kind::INTEGER:
          case Thing::Kind::DOUBLE:
            return move(operand);

          case Thing::Kind::OBJECT:
            // TODO:  Apply operator+
            throw "unimplemented";

          case Thing::Kind::BINDING:
          case Thing::Kind::RUNTIME_LVALUE:
            throw "can't happen, should have dereferenced";

          case Thing::Kind::RUNTIME_RVALUE: {
            // TODO:  Verify that the type has operator+, and check what operator+ returns (for now
            //   we're assuming it returns the same type, but that's wrong).
            CxxExpression result(CxxExpression::Priority::PREFIX, false);
            result.append("+");
            result.append(move(operand.result.runtimeRvalue.cxxExpression));
            return Evaluation(
                Thing::fromRuntimeRvalue(
                    move(operand.result.runtimeRvalue.descriptor), move(result)),
                {&operand});
          }
        }
    }
  }

  Thing applyPostfixOperator(Thing&& operand, ast::PostfixOperator op);

  Evaluation applyTernaryOperator(
      Evaluation&& condition, const Expression& trueClause, const Expression& falseClause) {
    condition = castTo(move(condition), Thing::fromBuiltinType(BuiltinType::BOOLEAN));

    switch (condition.result.getKind()) {
      case Thing::Kind::ERROR:
        return move(condition);

      case Thing::Kind::BOOLEAN:
        // The condition is known at compile time.  We don't even compile the dead branch because
        // the branch may assume things that the condition disproves.
        if (condition.result.boolean) {
          return compileExpression(trueClause);
        } else {
          return compileExpression(falseClause);
        }

      case Thing::Kind::RUNTIME_RVALUE: {
        Evaluation trueThing = compileExpression(trueClause);
        Evaluation falseThing = compileExpression(falseClause);

        // TODO:  Coerce trueThing and falseThing to a common type and isPointer status.
        bool isPointer;
        CxxExpression trueCxx;
        CxxExpression falseCxx;

        CxxExpression result(CxxExpression::Priority::TERNARY, isPointer);

        if (isPointer) {
          result.append(move(condition.result.runtimeRvalue.cxxExpression))
                .append(" ? ").appendBreak()
                .append(move(trueCxx))
                .append(" : ").appendBreak()
                .append(move(falseCxx));
        } else {
          result.append(move(condition.result.runtimeRvalue.cxxExpression))
                .append(" ? ").appendBreak()
                .appendAsPointer(move(trueCxx))
                .append(" : ").appendBreak()
                .appendAsPointer(move(falseCxx));
        }
        return Evaluation(Thing::fromRuntimeRvalue(move(commonDescriptor), move(result)),
                          {&condition, &trueThing, &falseThing});
      }

      default:
        throw "can't happen";
    }
  }

  Evaluation compileExpression(const Expression& expression) {
    switch (expression.getType()) {
      case Expression::Type::ERROR: {
        for (auto& error: expression.error) {
          context.error(error);
        }
        return Evaluation(Thing::fromError());
      }

      case Expression::Type::VARIABLE: {
        Maybe<Binding&> binding = scope.lookupBinding(expression.variable);
        if (binding) {
          return binding->reference();
        } else {
          context.error(errors::error(expression.location, "Unknown identifier: ",
                                      expression.variable));
          return Evaluation(Thing::fromError());
        }
      }

      case Expression::Type::TUPLE: {
        // TODO
        throw "unimplemented";
      }

      case Expression::Type::LITERAL_INT:
        return Evaluation(Thing::fromInteger(expression.literalInt));

      case Expression::Type::LITERAL_DOUBLE:
        return Evaluation(Thing::fromDouble(expression.literalDouble));

      case Expression::Type::LITERAL_STRING:
        // TODO:  Generate code to construct the string class.
        throw "unimplemented";

      case Expression::Type::LITERAL_ARRAY:
        // TODO:  Generate code to construct the FixedArray class.
        throw "unimplemented";

      case Expression::Type::BINARY_OPERATOR:
        return applyBinaryOperator(
            compileExpression(*expression.binaryOperator.left),
            expression.binaryOperator.op,
            compileExpression(*expression.binaryOperator.right));

      case Expression::Type::PREFIX_OPERATOR:
        return applyPrefixOperator(
            expression.prefixOperator.op,
            compileExpression(*expression.prefixOperator.operand),
            expression.location);

      case Expression::Type::POSTFIX_OPERATOR:
        return applyPostfixOperator(
            compileExpression(*expression.postfixOperator.operand),
            expression.postfixOperator.op);

      case Expression::Type::TERNARY_OPERATOR: {
        return applyTernaryOperator(
            compileExpression(*expression.ternaryOperator.condition),
            *expression.ternaryOperator.trueClause,
            *expression.ternaryOperator.falseClause);
      }

      case Expression::Type::FUNCTION_CALL:
      case Expression::Type::SUBSCRIPT:
        // TODO
        throw "unimplemented";

      case Expression::Type::MEMBER_ACCESS: {
        Evaluation object = compileExpression(*expression.memberAccess.object);

        switch (expression.memberAccess.thisStyleAllowance) {
          case ast::StyleAllowance::VALUE:
          case ast::StyleAllowance::IMMUTABLE_REFERENCE:
          case ast::StyleAllowance::MUTABLE_REFERENCE:
          case ast::StyleAllowance::MOVE:
        }
        throw "TODO";
      }

      case Expression::Type::IMPORT:
      case Expression::Type::LAMBDA:
        // TODO
        throw "unimplemented";
    }

    throw "Can't get here.";
  }

  vector<CxxStatement> compileImperative(const vector<ast::Statement>& statements) {
    vector<CxxStatement> code;

    for (auto& statement: statements) {
      switch (statement.getType()) {
        case ast::Statement::Type::ERROR:
          for (auto& error: statement.error) {
            context.error(error);
          }
          break;

        case ast::Statement::Type::EXPRESSION: {
          Thing value = compileExpression(context, scope, statement.expression);
          if (value.descriptor.getKind() != Thing::Kind::RUNTIME_RVALUE) {
            context.error(errors::error(statement.location, "Statement has no effect."));
          } else {
            code.push_back(CxxStatement::addSemicolon(move(value.cxxExpression)));
          }
          break;
        }

        case ast::Statement::Type::BLOCK: {
          OwnedPtr<Scope> subscope = scope.startBlock();
          CxxStatement cxxStatement(CxxExpression("{"));
          cxxStatement.blocks.emplace_back(
              move(Compiler(context, *subscope).compileImperative(statement.block)),
              CxxExpression("}"));
          code.push_back(move(cxxStatement));
          break;
        }

        case ast::Statement::Type::DECLARATION: {
          //??
        }
      }
    }

    return move(code);
  }

private:
  Context& context;
  Scope& scope;
};

vector<CxxStatement> compileImperative(Context& context, Scope& scope,
                                       const vector<ast::Statement>& statements) {
  return Compiler(context, scope).compileImperative(statements);
}

}  // namespace compiler
}  // namespace modc
