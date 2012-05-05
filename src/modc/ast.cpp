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
#include <functional>

#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"

namespace modc {
namespace ast {

using std::move;

template <typename T>
void Destroy(T& obj) {
  obj.~T();
}

// =======================================================================================
// Expression

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
  HANDLE(IMPORT, import, string) \
  HANDLE(LAMBDA, lambda, Lambda)

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

Expression::~Expression() noexcept {
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

// -------------------------------------------------------------------

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

Expression Expression::fromFunctionCall(Expression&& function,
                                        vector<FunctionCall::Parameter>&& parameters) {
  Expression result(Type::FUNCTION_CALL);
  new (&result.functionCall) FunctionCall(move(function), move(parameters));
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

Expression Expression::fromImport(string&& moduleName) {
  Expression result(Type::IMPORT);
  new (&result.import) string(move(moduleName));
  return result;
}

Expression Expression::fromLambda(Style style, vector<ParameterDeclaration>&& parameters,
                                  Expression&& body) {
  Expression result(Type::LAMBDA);
  new (&result.lambda) Lambda(style, move(parameters), move(body));
  return result;
}

// =======================================================================================
// Declaration

Declaration::Declaration(Kind kind): kind(kind), thisStyle(Style::VALUE), style(Style::VALUE) {}
Declaration::~Declaration() {}

bool Declaration::operator==(const Declaration& other) const {
  static_assert(sizeof(Declaration) == 152, "Please update Declaration::operator==.");
  return kind == other.kind &&
         thisStyle == other.thisStyle &&
         style == other.style &&
         name == other.name &&
         parameters == other.parameters &&
         annotations == other.annotations &&
         documentation == other.documentation &&
         definition == other.definition;
}

Declaration Declaration::fromError(vector<errors::Error>&& errors) {
  Declaration result(Kind::ERROR);
  result.definition = Definition::fromExpression(Expression::fromError(move(errors)));
  return result;
}

Declaration::Definition::Definition(Definition&& other): type(other.type) {
  switch (type) {
    case Type::EXPRESSION:
      new (&expression) Expression(move(other.expression));
      break;
    case Type::BLOCK:
      new (&block) vector<Statement>(move(other.block));
      break;
  }
}

Declaration::Definition::Definition(const Definition& other): type(other.type) {
  switch (type) {
    case Type::EXPRESSION:
      new (&expression) Expression(other.expression);
      break;
    case Type::BLOCK:
      new (&block) vector<Statement>(other.block);
      break;
  }
}

Declaration::Definition::~Definition() noexcept {
  switch (type) {
    case Type::EXPRESSION:
      Destroy(expression);
      break;
    case Type::BLOCK:
      Destroy(block);
      break;
  }
}

Declaration::Definition& Declaration::Definition::operator=(Definition&& other) {
  // Lazy.
  this->~Definition();
  new(this) Definition(move(other));
  return *this;
}

Declaration::Definition& Declaration::Definition::operator=(const Definition& other) {
  // Lazy.
  this->~Definition();
  new(this) Definition(other);
  return *this;
}

bool Declaration::Definition::operator==(const Definition& other) const {
  if (type == other.type) {
    switch (type) {
      case Type::EXPRESSION:
        return expression == other.expression;
      case Type::BLOCK:
        return block == other.block;
    }
  }

  return false;
}

Declaration::Definition Declaration::Definition::fromExpression(Expression&& expression) {
  Definition result(Type::EXPRESSION);
  new (&result.expression) Expression(move(expression));
  return result;
}
Declaration::Definition Declaration::Definition::fromBlock(vector<Statement>&& statements) {
  Definition result(Type::BLOCK);
  new (&result.block) vector<Statement>(move(statements));
  return result;
}

// -------------------------------------------------------------------
// ParameterDeclaration

ParameterDeclaration::ParameterDeclaration(ParameterDeclaration&& other): type(other.type) {
  switch (type) {
    case Type::CONSTANT:
      new (&constant) Expression(move(other.constant));
      break;
    case Type::VARIABLE:
      new (&variable) Declaration(move(other.variable));
      break;
  }
}

ParameterDeclaration::ParameterDeclaration(const ParameterDeclaration& other): type(other.type) {
  switch (type) {
    case Type::CONSTANT:
      new (&constant) Expression(other.constant);
      break;
    case Type::VARIABLE:
      new (&variable) Declaration(other.variable);
      break;
  }
}

ParameterDeclaration::~ParameterDeclaration() noexcept {
  switch (type) {
    case Type::CONSTANT:
      Destroy(constant);
      break;
    case Type::VARIABLE:
      Destroy(variable);
      break;
  }
}

ParameterDeclaration& ParameterDeclaration::operator=(ParameterDeclaration&& other) {
  // Lazy.
  this->~ParameterDeclaration();
  new(this) ParameterDeclaration(move(other));
  return *this;
}

ParameterDeclaration& ParameterDeclaration::operator=(const ParameterDeclaration& other) {
  // Lazy.
  this->~ParameterDeclaration();
  new(this) ParameterDeclaration(other);
  return *this;
}

bool ParameterDeclaration::operator==(const ParameterDeclaration& other) const {
  if (type == other.type) {
    switch (type) {
      case Type::CONSTANT:
        return constant == other.constant;
      case Type::VARIABLE:
        return variable == other.variable;
    }
  }

  return false;
}

ParameterDeclaration ParameterDeclaration::fromError(vector<errors::Error>&& errors) {
  // TODO:  Maybe there should be an ERROR Type.
  return fromConstant(Expression::fromError(move(errors)));
}
ParameterDeclaration ParameterDeclaration::fromConstant(Expression&& expression) {
  ParameterDeclaration result(Type::CONSTANT);
  new (&result.constant) Expression(move(expression));
  return result;
}
ParameterDeclaration ParameterDeclaration::fromVariable(Declaration&& declaration) {
  ParameterDeclaration result(Type::VARIABLE);
  new (&result.variable) Declaration(move(declaration));
  return result;
}

// =======================================================================================
// Statement

#define FOR_ALL_STATEMENTS(HANDLE) \
  HANDLE(ERROR, error, std::vector<errors::Error>) \
  HANDLE(EXPRESSION, expression, Expression) \
  HANDLE(BLOCK, block, vector<Statement>) \
  HANDLE(DECLARATION, declaration, Declaration) \
  HANDLE(ASSIGNMENT, assignment, Assignment) \
  HANDLE(UNION, union_, vector<Declaration>) \
  HANDLE(IF, if_, If) \
  HANDLE(ELSE, else_, Indirect<Statement>) \
  HANDLE(FOR, for_, For) \
  HANDLE(WHILE, while_, While) \
  HANDLE(LOOP, loop, Loop) \
  HANDLE(PARALLEL, parallel, vector<Statement>) \
  HANDLE(RETURN, return_, Expression) \
  HANDLE(BREAK, break_, Maybe<string>) \
  HANDLE(CONTINUE, continue_, Maybe<string>) \
  HANDLE(COMMENT, comment, string)

Statement::Statement(Statement&& other): type(other.type) {
  switch (type) {
#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
      FOR_ALL_STATEMENTS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT

    case Type::BLANK:
      break;
  }
}

Statement::Statement(const Statement& other): type(other.type) {
  switch (type) {
#define COPY_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(other.NAME); \
      break;
      FOR_ALL_STATEMENTS(COPY_CONSTRUCT)
#undef COPY_CONSTRUCT

    case Type::BLANK:
      break;
  }
}

Statement::~Statement() noexcept {
  switch (type) {
#define DESTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      Destroy(NAME); \
      break;
      FOR_ALL_STATEMENTS(DESTRUCT)
#undef DESTRUCT

    case Type::BLANK:
      break;
  }
}

Statement& Statement::operator=(Statement&& other) {
  // Lazy.
  this->~Statement();
  new(this) Statement(move(other));
  return *this;
}

Statement& Statement::operator=(const Statement& other) {
  // Lazy.
  this->~Statement();
  new(this) Statement(other);
  return *this;
}

bool Statement::operator==(const Statement& other) const {
  if (type == other.type) {
    switch (type) {
#define COMPARE(ID, NAME, TYPE) \
      case Type::ID: \
        return NAME == other.NAME;
        FOR_ALL_STATEMENTS(COMPARE)
#undef MOVE_CONSTRUCT

      case Type::BLANK:
        return true;
    }
  }

  return false;
}

// -------------------------------------------------------------------

Statement Statement::fromError(errors::Error&& error) {
  Statement result(Type::ERROR);
  new (&result.error) vector<errors::Error>;
  result.error.push_back(move(error));
  return result;
}
Statement Statement::fromError(vector<errors::Error>&& errors) {
  Statement result(Type::ERROR);
  new (&result.error) vector<errors::Error>(move(errors));
  return result;
}

Statement Statement::fromExpression(Expression&& expression) {
  Statement result(Type::EXPRESSION);
  new (&result.expression) Expression(move(expression));
  return result;
}
Statement Statement::fromBlock(vector<Statement>&& block) {
  Statement result(Type::BLOCK);
  new (&result.block) vector<Statement>(move(block));
  return result;
}

Statement Statement::fromDeclaration(Declaration&& declaration) {
  Statement result(Type::DECLARATION);
  new (&result.declaration) Declaration(move(declaration));
  return result;
}
Statement Statement::fromAssignment(Expression&& variable, Expression&& value) {
  Statement result(Type::ASSIGNMENT);
  new (&result.assignment) Assignment(move(variable), move(value));
  return result;
}

Statement Statement::fromUnion(vector<Declaration>&& declarations) {
  Statement result(Type::UNION);
  new (&result.union_) vector<Declaration>(move(declarations));
  return result;
}

Statement Statement::fromIf(Expression&& condition, Statement&& body) {
  Statement result(Type::IF);
  new (&result.if_) If(move(condition), move(body));
  return result;
}
Statement Statement::fromFor(vector<Declaration>&& range, Statement&& body) {
  Statement result(Type::FOR);
  new (&result.for_) For(move(range), move(body));
  return result;
}
Statement Statement::fromWhile(Expression&& condition, Statement&& body) {
  Statement result(Type::WHILE);
  new (&result.while_) While(move(condition), move(body));
  return result;
}
Statement Statement::fromLoop(Maybe<string>&& name, Statement&& body) {
  Statement result(Type::LOOP);
  new (&result.loop) Loop(move(name), move(body));
  return result;
}
Statement Statement::fromParallel(vector<Statement>&& statements) {
  Statement result(Type::PARALLEL);
  new (&result.parallel) vector<Statement>(move(statements));
  return result;
}

Statement Statement::fromReturn(Expression&& value) {
  Statement result(Type::RETURN);
  new (&result.return_) Expression(move(value));
  return result;
}
Statement Statement::fromBreak(Maybe<string>&& loopName) {
  Statement result(Type::BREAK);
  new (&result.break_) Maybe<string>(move(loopName));
  return result;
}
Statement Statement::fromContinue(Maybe<string>&& loopName) {
  Statement result(Type::CONTINUE);
  new (&result.continue_) Maybe<string>(move(loopName));
  return result;
}

Statement Statement::fromBlank() {
  return Statement(Type::BLANK);
}
Statement Statement::fromComment(string&& text) {
  Statement result(Type::COMMENT);
  new (&result.comment) string(move(text));
  return result;
}

}  // namespace ast
}  // namespace modc
