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

#include "expressions.h"

#include <utility>
#include <functional>

#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"

namespace modc {
namespace expressions {

using std::move;

// =============================================================================

template <typename T>
void Destroy(T& obj) {
  obj.~T();
}

#define FOR_ALL_EXPRESSIONS(HANDLE) \
  HANDLE(ERROR, error, vector<errors::Error>) \
  HANDLE(VARIABLE, variable, string) \
  HANDLE(TUPLE, tuple, vector<Expression>) \
  HANDLE(LITERAL_INT, literalInt, int) \
  HANDLE(LITERAL_DOUBLE, literalDouble, double) \
  HANDLE(LITERAL_STRING, literalString, string) \
  HANDLE(LITERAL_ARRAY, literalArray, vector<Expression>) \
  HANDLE(BINARY_OPERATOR, binaryOperator, BinaryOperator) \
  HANDLE(PREFIX_OPERATOR, prefixOperator, PrefixOperator) \
  HANDLE(POSTFIX_OPERATOR, postfixOperator, PostfixOperator) \
  HANDLE(TERNARY_OPERATOR, ternaryOperator, TernaryOperator) \
  HANDLE(FUNCTION_CALL, functionCall, FunctionCall) \
  HANDLE(SUBSCRIPT, subscript, Subscript) \
  HANDLE(MEMBER_ACCESS, memberAccess, MemberAccess) \
  HANDLE(IMPORT, import, string)

Expression::Expression(Expression&& other): type(other.type) {
  switch (type) {
#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
    FOR_ALL_EXPRESSIONS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT
  }
}

Expression::Expression(const Expression& other): type(other.type) {
  switch (type) {
#define COPY_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(other.NAME); \
      break;
    FOR_ALL_EXPRESSIONS(COPY_CONSTRUCT)
#undef COPY_CONSTRUCT
  }
}

Expression::~Expression() {
  switch (type) {
#define DESTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      Destroy(NAME); \
      break;
    FOR_ALL_EXPRESSIONS(DESTRUCT)
#undef DESTRUCT
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
#define COMPARE(ID, NAME, TYPE) \
      case Type::ID: \
        return NAME == other.NAME;
      FOR_ALL_EXPRESSIONS(COMPARE)
#undef MOVE_CONSTRUCT
    }
  }

  return false;
}

// ---------------------------------------------------------------------------------------

Expression Expression::fromError(errors::Error&& error) {
  Expression result(Type::ERROR);
  new (&result.error) vector<errors::Error>;
  result.error.push_back(move(error));
  return result;
}
Expression Expression::fromError(vector<errors::Error>&& errors) {
  Expression result(Type::ERROR);
  new (&result.error) vector<errors::Error>(move(errors));
  return result;
}

Expression Expression::fromVariable(string&& name) {
  Expression result(Type::VARIABLE);
  new (&result.variable) string(move(name));
  return result;
}

Expression Expression::fromTuple(vector<Expression>&& elements) {
  Expression result(Type::TUPLE);
  new (&result.tuple) vector<Expression>(move(elements));
  return result;
}

Expression Expression::fromLiteralInt(int value) {
  Expression result(Type::LITERAL_INT);
  new (&result.literalInt) int(value);
  return result;
}
Expression Expression::fromLiteralDouble(double value) {
  Expression result(Type::LITERAL_DOUBLE);
  new (&result.literalDouble) double(value);
  return result;
}
Expression Expression::fromLiteralString(string&& value) {
  Expression result(Type::LITERAL_STRING);
  new (&result.literalString) string(move(value));
  return result;
}
Expression Expression::fromLiteralArray(vector<Expression>&& elements) {
  Expression result(Type::LITERAL_ARRAY);
  new (&result.literalArray) vector<Expression>(move(elements));
  return result;
}

Expression Expression::fromImport(string&& moduleName) {
  Expression result(Type::IMPORT);
  new (&result.import) string(moduleName);
  return result;
}

Expression Expression::fromSubscript(Expression&& container, Expression&& key) {
  Expression result(Type::SUBSCRIPT);
  new (&result.subscript) Subscript(move(container), move(key));
  return result;
}
Expression Expression::fromMemberAccess(Expression&& object, string&& member) {
  Expression result(Type::MEMBER_ACCESS);
  new (&result.memberAccess) MemberAccess(move(object), move(member));
  return result;
}
Expression Expression::fromFunctionCall(Expression&& function,
                                        vector<FunctionCall::Parameter>&& parameters) {
  Expression result(Type::FUNCTION_CALL);
  new (&result.functionCall) FunctionCall(move(function), move(parameters));
  return result;
}

Expression Expression::fromBinaryOperator(string&& op, Expression&& left, Expression&& right) {
  Expression result(Type::BINARY_OPERATOR);
  new (&result.binaryOperator) BinaryOperator(move(op), move(left), move(right));
  return result;
}
Expression Expression::fromPrefixOperator(string&& op, Expression&& exp) {
  Expression result(Type::PREFIX_OPERATOR);
  new (&result.prefixOperator) PrefixOperator(move(op), move(exp));
  return result;
}
Expression Expression::fromPostfixOperator(Expression&& exp, string&& op) {
  Expression result(Type::POSTFIX_OPERATOR);
  new (&result.postfixOperator) PostfixOperator(move(exp), move(op));
  return result;
}
Expression Expression::fromTernaryOperator(Expression&& condition, Expression&& trueClause,
                                           Expression&& falseClause) {
  Expression result(Type::TERNARY_OPERATOR);
  new (&result.ternaryOperator) TernaryOperator(
      move(condition), move(trueClause), move(falseClause));
  return result;
}

}  // namespace expressions
}  // namespace modc
