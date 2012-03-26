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

#include "ast.h"

#include <utility>

namespace modc {
namespace ast {

using std::move;

#define FOR_ALL_EXPRESSIONS(HANDLE) \
  HANDLE(LITERAL_INT, literalInt, int) \
  HANDLE(LITERAL_DOUBLE, literalDouble, double) \
  HANDLE(LITERAL_STRING, literalString, string) \
  HANDLE(LITERAL_ARRAY, literalArray, std::vector<Expression>) \
  HANDLE(BINARY_OPERATOR, binaryOperator, BinaryOperator) \
  HANDLE(UNARY_OPERATOR, unaryOperator, UnaryOperator) \
  HANDLE(FUNCTION_CALL, functionCall, FunctionCall) \
  HANDLE(SUBSCRIPT, subscript, Subscript) \
  HANDLE(MEMBER_ACCESS, memberAccess, MemberAccess) \
  HANDLE(CONDITIONAL, conditional, Conditional) \
  HANDLE(LAMBDA, lambda, Lambda) \
  HANDLE(IMPORT, import, string)

Expression::Expression(Expression&& other): type(other.type) {
  switch (type) {
    case Type::ERROR:
      break;

#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
    FOR_ALL_EXPRESSIONS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT
  }
}

Expression::Expression(const Expression& other) {
  switch (type) {
    case Type::ERROR:
      break;

#define COPY_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(other.NAME); \
      break;
    FOR_ALL_EXPRESSIONS(COPY_CONSTRUCT)
#undef MOVE_CONSTRUCT
  }
}

Expression::~Expression() {
  switch (type) {
    case Type::ERROR:
      break;
    case Type::LITERAL_INT:
      break;
    case Type::LITERAL_DOUBLE:
      break;
    case Type::LITERAL_STRING:
      literalString.~string();
      break;
    case Type::LITERAL_ARRAY:
      literalArray.~vector<Expression>();
      break;
    case Type::BINARY_OPERATOR:
      binaryOperator.~BinaryOperator();
      break;
    case Type::UNARY_OPERATOR:
      unaryOperator.~UnaryOperator();
      break;
    case Type::FUNCTION_CALL:
      functionCall.~FunctionCall();
      break;
    case Type::SUBSCRIPT:
      subscript.~Subscript();
      break;
    case Type::MEMBER_ACCESS:
      memberAccess.~MemberAccess();
      break;
    case Type::CONDITIONAL:
      conditional.~Conditional();
      break;
    case Type::LAMBDA:
      lambda.~Lambda();
      break;
    case Type::IMPORT:
      import.~string();
      break;
  }
}

Expression& Expression::operator=(Expression&& other) {
  // Lazy.
  this->~Expression();
  new(this) Expression(move(other));
  return *this;
}

Expression& Expression::operator=(const Expression& other) {
  // Lazy.
  this->~Expression();
  new(this) Expression(other);
  return *this;
}

bool Expression::operator==(const Expression& other) const {
  if (type == other.type) {
    switch (type) {
      case Type::ERROR:
        return true;

#define COMPARE(ID, NAME, TYPE) \
      case Type::ID: \
        return NAME == other.NAME;
      FOR_ALL_EXPRESSIONS(COMPARE)
#undef MOVE_CONSTRUCT
    }
  }

  return false;
}


}  // namespace ast
}  // namespace modc
