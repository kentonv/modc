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

Thing applyBinaryOperator(Thing&& left, ast::BinaryOperator op, Thing&& right);
Thing applyPrefixOperator(ast::PrefixOperator op, Thing&& operand);
Thing applyPostfixOperator(Thing&& operand, ast::PostfixOperator op);
Thing applyTernaryOperator(Thing&& condition, Thing&& trueClause, Thing&& falseClause);

Thing compileExpression(Context& context, Scope& scope, const Expression& expression) {
  switch (expression.getType()) {
    case Expression::Type::ERROR: {
      for (auto& error: expression.error) {
        context.error(error);
      }
      return Thing::fromError();
    }

    case Expression::Type::VARIABLE: {
      Maybe<Binding&> binding = scope.lookupBinding(expression.variable);
      if (!binding) {
        context.error(errors::error(expression.location, "Unknown identifier: ",
                                    expression.variable));
        return Thing::fromError();
      }

      // TODO:  useMutably, useImutably, getValue(), ...?

      return Thing::fromCxxExpression(binding->getReferenceCode());
    }

    case Expression::Type::TUPLE: {
      // TODO
      break;
    }

    case Expression::Type::LITERAL_INT:
      return Thing::fromValue(Value::fromInteger(expression.literalInt));

    case Expression::Type::LITERAL_DOUBLE:
      return Thing::fromValue(Value::fromDouble(expression.literalDouble));

    case Expression::Type::LITERAL_STRING:
      // TODO:  Generate code to construct the string class.
      break;

    case Expression::Type::LITERAL_ARRAY:
      // TODO:  Generate code to construct the FixedArray class.
      break;

    case Expression::Type::BINARY_OPERATOR:
      return applyBinaryOperator(
          compileExpression(context, scope, *expression.binaryOperator.left),
          expression.binaryOperator.op,
          compileExpression(context, scope, *expression.binaryOperator.right));

    case Expression::Type::PREFIX_OPERATOR:
      return applyPrefixOperator(
          expression.prefixOperator.op,
          compileExpression(context, scope, *expression.prefixOperator.operand));

    case Expression::Type::POSTFIX_OPERATOR:
      return applyPostfixOperator(
          compileExpression(context, scope, *expression.postfixOperator.operand),
          expression.postfixOperator.op);

    case Expression::Type::TERNARY_OPERATOR: {
      Thing condition = compileExpression(context, scope, *expression.ternaryOperator.condition);
      if (condition.getType() == Thing::Type::VALUE &&
          condition.value.getType() == Value::Type::BOOLEAN) {
        // TODO:  Type-check the branch we don't take.
        // TODO:  Error if condition wasn't based on meta parameters.
        if (condition.value.boolean) {
          return compileExpression(context, scope, *expression.ternaryOperator.trueClause);
        } else {
          return compileExpression(context, scope, *expression.ternaryOperator.falseClause);
        }
      } else {
        return applyTernaryOperator(move(condition),
            compileExpression(context, scope, *expression.ternaryOperator.trueClause),
            compileExpression(context, scope, *expression.ternaryOperator.falseClause));
      }
    }
  }

  throw "Can't get here.";
}

vector<CxxStatement> compileImperative(Context& context, Scope& scope,
                                       const vector<ast::Statement>& statements) {
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
        if (value.getType() != Thing::Type::CXX_EXPRESSION) {
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
            move(compileImperative(context, *subscope, statement.block)),
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

}  // namespace compiler
}  // namespace modc
