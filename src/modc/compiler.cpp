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



class CxxBinaryOperator: public CxxCode {
public:

  ~CxxBinaryOperator() {}

  void

  // implements CxxCode ----------------------------------------------

  void write(CodePrinter& printer) {

  }
};

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
      // TODO:  Chain left-hand side of same priority.
      break;
  }

  throw "Can't get here.";
}

class CxxBlock: public CxxCode {
public:
  void addBlankLine();
  void addStatement(OwnedPtr<CxxCode> statementCode);
  void addBlock(OwnedPtr<CxxCode> statementCode);

private:
};

OwnedPtr<CxxCode> compileImperative(Context& context, Scope& scope,
                                    const vector<ast::Statement>& statements) {
  OwnedPtr<CxxBlock> code = newOwned<CxxBlock>();

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
          code->addStatement(move(value.code));
        }
        break;
      }

      case ast::Statement::Type::BLOCK: {
        OwnedPtr<Scope> subscope = scope.startBlock();
        code->addBlock(compileImperative(context, *subscope, statement.block));
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
