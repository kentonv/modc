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

  CxxExpression asCxx(Thing&& value);

  struct Dereferenced {
    Thing thing;
    VariableUsageSet variablesUsed;

    VALUE_TYPE2(Dereferenced, Thing&&, thing, VariableUsageSet&&, variablesUsed);
  };

  Dereferenced dereference(Evaluation&& eval, errors::Location location) {
    switch (eval.getKind()) {
      case Evaluation::Kind::REFERENCE:
        switch (eval.reference.getKind()) {
          case Reference::Kind::NAMED:
            return Dereferenced(eval.reference.named->read(eval.variablesUsed, location),
                                move(eval.variablesUsed));

          case Reference::Kind::ANONYMOUS:
            for (Variable* variable: eval.reference.anonymous.entangledVariables) {
              variable->entangle(VariableUsageSet::Style::IMMUTABLE, eval.variablesUsed, location);
            }

            return Dereferenced(
                Thing::fromRuntimeValue(
                    move(eval.reference.anonymous.descriptor),
                    move(eval.reference.anonymous.cxxExpression)),
                move(eval.variablesUsed));
        }

        throw "can't get here";

      case Evaluation::Kind::THING:
        return Dereferenced(move(eval.thing), move(eval.variablesUsed));
    }

    throw "can't get here";
  }

  Dereferenced castTo(Evaluation&& eval, const Type& type);
  ValueDescriptor findCommonSupertype(const ValueDescriptor& type1, const ValueDescriptor& type2);

  Evaluation applyBinaryOperator(Evaluation&& left, ast::BinaryOperator op, Evaluation&& right);

  Evaluation applyPrefixOperator(ast::PrefixOperator op, Evaluation&& operand,
                                 errors::Location location) {
    Dereferenced derefOperand = dereference(move(operand), location);

    switch (op) {
      case ast::PrefixOperator::POSITIVE:
        switch (derefOperand.thing.getKind()) {
          case Thing::Kind::INTEGER:
          case Thing::Kind::DOUBLE:
            return Evaluation::fromThing(
                move(derefOperand.thing), move(derefOperand.variablesUsed));

          case Thing::Kind::OBJECT:
            // TODO:  Apply operator+
            throw "unimplemented";

          case Thing::Kind::RUNTIME_VALUE: {
            // TODO:  Verify that the type has operator+, and check what operator+ returns (for now
            //   we're assuming it returns the same type, but that's wrong).
            CxxExpression result(CxxExpression::Priority::PREFIX, false);
            result.append("+");
            result.append(move(derefOperand.thing.runtimeValue.cxxExpression));
            return Evaluation::fromThing(
                Thing::fromRuntimeValue(
                    move(derefOperand.thing.runtimeValue.descriptor), move(result)),
                move(derefOperand.variablesUsed));
          }
        }
    }
  }

  Evaluation applyPostfixOperator(Evaluation&& operand, ast::PostfixOperator op);

  Evaluation applyTernaryOperator(
      Evaluation&& condition, const Expression& trueClause, const Expression& falseClause) {
    Dereferenced boolCondition =
        castTo(move(condition), Type::fromBuiltinType(BuiltinType::BOOLEAN));

    switch (boolCondition.thing.getKind()) {
      case Thing::Kind::ERROR:
        return move(condition);

      case Thing::Kind::BOOLEAN: {
        // The condition is known at compile time.  We don't even compile the dead branch because
        // the branch may assume things that the condition disproves.
        const Expression* branchToCompile;
        if (boolCondition.thing.boolean) {
          branchToCompile = &trueClause;
        } else {
          branchToCompile = &falseClause;
        }

        // Merge variables used by the branches.  Branches are guaranteed to execute after
        // condition, so this is a sequential merge.
        Evaluation result = compileExpression(*branchToCompile);
        result.variablesUsed.mergeSequential(boolCondition.variablesUsed);

        return result;
      }

      case Thing::Kind::RUNTIME_VALUE: {
        // TODO(kenton):  Need to create sub-scopes or something to track the fact that only one
        //   of the two branches' side effects will occur.
        Evaluation trueThing = compileExpression(trueClause);
        Evaluation falseThing = compileExpression(falseClause);

        // TODO:  Coerce trueThing and falseThing to a common type and isPointer status.
        bool isPointer;
        CxxExpression trueCxx;
        CxxExpression falseCxx;
        ValueDescriptor commonDescriptor;

        CxxExpression result(CxxExpression::Priority::TERNARY, isPointer);

        if (isPointer) {
          result.append(move(boolCondition.thing.runtimeValue.cxxExpression))
                .append(" ? ").appendBreak()
                .append(move(trueCxx))
                .append(" : ").appendBreak()
                .append(move(falseCxx));
        } else {
          result.append(move(boolCondition.thing.runtimeValue.cxxExpression))
                .append(" ? ").appendBreak()
                .appendAsPointer(move(trueCxx))
                .append(" : ").appendBreak()
                .appendAsPointer(move(falseCxx));
        }

        // Since only one of the branches will actually be executed, we can consider them
        // sequential.
        boolCondition.variablesUsed.mergeSequential(trueThing.variablesUsed);
        boolCondition.variablesUsed.mergeSequential(falseThing.variablesUsed);

        // TODO(kenton):  If both branches are references, we actually want to return a reference...
        return Evaluation::fromThing(
            Thing::fromRuntimeValue(move(commonDescriptor), move(result)),
            move(boolCondition.variablesUsed));
      }

      default:
        throw "can't happen; cast to bool";
    }
  }

  Evaluation getMember(Evaluation&& object, const string& memberName,
                       errors::Location location) {
    switch (object.getKind()) {
      case Evaluation::Kind::REFERENCE:
        switch (object.reference.getKind()) {
          case Reference::Kind::NAMED: {
            Variable& variable = *object.reference.named;
            Maybe<Entity&> member = variable.getMember(memberName);

            if (member) {
              return Evaluation::fromReference(Reference::fromNamed(&*member),
                                               move(object.variablesUsed));
            } else {
              context.error(errors::error(location, "No such member: ", memberName));
              return Evaluation::fromError();
            }
          }

          case Reference::Kind::ANONYMOUS: {
            CxxExpression exp(CxxExpression::Priority::SUFFIX, false);
            exp.appendMemberAccess(move(object.reference.anonymous.cxxExpression), memberName);
            object.reference.anonymous.cxxExpression = move(exp);
            return move(object);
          }
        }
        throw "can't happen";

      case Evaluation::Kind::THING:
        switch (object.thing.getKind()) {
          case Thing::Kind::ERROR:
            return move(object);

          case Thing::Kind::BOOLEAN:
          case Thing::Kind::INTEGER:
          case Thing::Kind::DOUBLE:
          case Thing::Kind::TUPLE:
            context.error(errors::error(location, "Not an aggregate value."));
            return Evaluation::fromError();

          case Thing::Kind::OBJECT: {
            Maybe<Member&> member = object.thing.object.type->lookupMember(memberName);
            if (!member) {
              context.error(errors::error(location, "No such member: ", memberName));
              return Evaluation::fromError();
            }

            if (member->isField()) {
              auto iter = object.thing.object.fields.find(&*member);
              if (iter == object.thing.object.fields.end()) {
                context.error(errors::error(location, "Field is not set: ", memberName));
                return Evaluation::fromError();
              } else {
                return Evaluation::fromThing(move(iter->second), move(object.variablesUsed));
              }
            } else {
              // TODO:  It's an overload...  could still become a field later.
            }
          }

          //case Thing::Kind::TYPE:
          //case Thing::Kind::RUNTIME_VALUE:
          //case Thing::Kind::META_CONSTANT:
        }
    }
  }

  Evaluation compileExpression(const Expression& expression) {
    switch (expression.getType()) {
      case Expression::Type::ERROR: {
        for (auto& error: expression.error) {
          context.error(error);
        }
        return Evaluation::fromError();
      }

      case Expression::Type::VARIABLE: {
        Maybe<Entity&> binding = scope.lookupBinding(expression.variable);
        if (binding) {
          // Note that this doesn't actually *use* the binding, just forms a reference which may
          // be used later.
          // TODO:  Really?  Does it not use the binding's identity, at least?
          return Evaluation::fromReference(Reference::fromNamed(&*binding), VariableUsageSet());
        } else {
          context.error(errors::error(expression.location, "Unknown identifier: ",
                                      expression.variable));
          return Evaluation::fromError();
        }
      }

      case Expression::Type::TUPLE: {
        // TODO
        throw "unimplemented";
      }

      case Expression::Type::LITERAL_INT:
        return Evaluation::fromThing(Thing::fromInteger(expression.literalInt), VariableUsageSet());

      case Expression::Type::LITERAL_DOUBLE:
        return Evaluation::fromThing(Thing::fromDouble(expression.literalDouble),
                                     VariableUsageSet());

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
          Evaluation eval = compileExpression(statement.expression);

          switch (eval.getKind()) {
            case Evaluation::Kind::REFERENCE:
              if (eval.reference.getKind() != Reference::Kind::ANONYMOUS) {
                context.error(errors::error(statement.location, "Statement has no effect."));
              }
              code.push_back(CxxStatement::addSemicolon(
                  move(eval.reference.anonymous.cxxExpression)));
              break;
            case Evaluation::Kind::THING:
              if (eval.thing.getKind() != Thing::Kind::RUNTIME_VALUE) {
                context.error(errors::error(statement.location, "Statement has no effect."));
              }
              code.push_back(CxxStatement::addSemicolon(
                  move(eval.thing.runtimeValue.cxxExpression)));
              break;
          }

          // TODO(kenton):  Verify no conflicting variable usage.
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
