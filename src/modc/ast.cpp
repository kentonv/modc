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
#include <ostream>
#include <google/protobuf/stubs/strutil.h>

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

Expression Expression::fromError(Location location, errors::Error&& error) {
  Expression result(location, Type::ERROR);
  new (&result.error) vector<errors::Error>;
  result.error.push_back(move(error));
  return result;
}
Expression Expression::fromError(Location location, vector<errors::Error>&& errors) {
  Expression result(location, Type::ERROR);
  new (&result.error) vector<errors::Error>(move(errors));
  return result;
}

Expression Expression::fromVariable(Location location, string&& name) {
  Expression result(location, Type::VARIABLE);
  new (&result.variable) string(move(name));
  return result;
}

Expression Expression::fromTuple(Location location, vector<Expression>&& elements) {
  Expression result(location, Type::TUPLE);
  new (&result.tuple) vector<Expression>(move(elements));
  return result;
}

Expression Expression::fromLiteralInt(Location location, int value) {
  Expression result(location, Type::LITERAL_INT);
  new (&result.literalInt) int(value);
  return result;
}
Expression Expression::fromLiteralDouble(Location location, double value) {
  Expression result(location, Type::LITERAL_DOUBLE);
  new (&result.literalDouble) double(value);
  return result;
}
Expression Expression::fromLiteralString(Location location, string&& value) {
  Expression result(location, Type::LITERAL_STRING);
  new (&result.literalString) string(move(value));
  return result;
}
Expression Expression::fromLiteralArray(Location location, vector<Expression>&& elements) {
  Expression result(location, Type::LITERAL_ARRAY);
  new (&result.literalArray) vector<Expression>(move(elements));
  return result;
}

Expression Expression::fromBinaryOperator(Location location, string&& op,
                                          Expression&& left, Expression&& right) {
  Expression result(location, Type::BINARY_OPERATOR);
  new (&result.binaryOperator) BinaryOperator(move(op), move(left), move(right));
  return result;
}
Expression Expression::fromPrefixOperator(Location location, string&& op, Expression&& exp) {
  Expression result(location, Type::PREFIX_OPERATOR);
  new (&result.prefixOperator) PrefixOperator(move(op), move(exp));
  return result;
}
Expression Expression::fromPostfixOperator(Location location, Expression&& exp, string&& op) {
  Expression result(location, Type::POSTFIX_OPERATOR);
  new (&result.postfixOperator) PostfixOperator(move(exp), move(op));
  return result;
}
Expression Expression::fromTernaryOperator(Location location, Expression&& condition,
                                           Expression&& trueClause, Expression&& falseClause) {
  Expression result(location, Type::TERNARY_OPERATOR);
  new (&result.ternaryOperator) TernaryOperator(
      move(condition), move(trueClause), move(falseClause));
  return result;
}

Expression Expression::fromFunctionCall(Location location, Expression&& function,
                                        vector<FunctionCall::Parameter>&& parameters) {
  Expression result(location, Type::FUNCTION_CALL);
  new (&result.functionCall) FunctionCall(move(function), move(parameters));
  return result;
}
Expression Expression::fromSubscript(Location location, Expression&& container, Expression&& key) {
  Expression result(location, Type::SUBSCRIPT);
  new (&result.subscript) Subscript(move(container), move(key));
  return result;
}
Expression Expression::fromMemberAccess(Location location, Expression&& object, string&& member) {
  Expression result(location, Type::MEMBER_ACCESS);
  new (&result.memberAccess) MemberAccess(move(object), move(member));
  return result;
}

Expression Expression::fromImport(Location location, string&& moduleName) {
  Expression result(location, Type::IMPORT);
  new (&result.import) string(move(moduleName));
  return result;
}

Expression Expression::fromLambda(Location location, Style style,
                                  vector<ParameterDeclaration>&& parameters, Expression&& body) {
  Expression result(location, Type::LAMBDA);
  new (&result.lambda) Lambda(style, move(parameters), move(body));
  return result;
}

// =======================================================================================
// Declaration

Declaration::Declaration(Location location, Kind kind)
    : kind(kind), thisStyle(Style::VALUE), style(Style::VALUE), location(location) {}
Declaration::~Declaration() {}

bool Declaration::operator==(const Declaration& other) const {
  static_assert(sizeof(Declaration) == 200, "Please update Declaration::operator==.");
  return kind == other.kind &&
         thisStyle == other.thisStyle &&
         style == other.style &&
         name == other.name &&
         parameters == other.parameters &&
         annotations == other.annotations &&
         documentation == other.documentation &&
         definition == other.definition;
}

Declaration Declaration::fromError(Location location, vector<errors::Error>&& errors) {
  Declaration result(location, Kind::ERROR);
  result.definition = Definition::fromExpression(Expression::fromError(location, move(errors)));
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

ParameterDeclaration ParameterDeclaration::fromError(Location location,
                                                     vector<errors::Error>&& errors) {
  // TODO:  Maybe there should be an ERROR Type.
  return fromConstant(location, Expression::fromError(location, move(errors)));
}
ParameterDeclaration ParameterDeclaration::fromConstant(Location location,
                                                        Expression&& expression) {
  ParameterDeclaration result(Type::CONSTANT);
  new (&result.constant) Expression(move(expression));
  return result;
}
ParameterDeclaration ParameterDeclaration::fromVariable(Location location,
                                                        Declaration&& declaration) {
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

Statement Statement::fromError(Location location, errors::Error&& error) {
  Statement result(location, Type::ERROR);
  new (&result.error) vector<errors::Error>;
  result.error.push_back(move(error));
  return result;
}
Statement Statement::fromError(Location location, vector<errors::Error>&& errors) {
  Statement result(location, Type::ERROR);
  new (&result.error) vector<errors::Error>(move(errors));
  return result;
}

Statement Statement::fromExpression(Location location, Expression&& expression) {
  Statement result(location, Type::EXPRESSION);
  new (&result.expression) Expression(move(expression));
  return result;
}
Statement Statement::fromBlock(Location location, vector<Statement>&& block) {
  Statement result(location, Type::BLOCK);
  new (&result.block) vector<Statement>(move(block));
  return result;
}

Statement Statement::fromDeclaration(Location location, Declaration&& declaration) {
  Statement result(location, Type::DECLARATION);
  new (&result.declaration) Declaration(move(declaration));
  return result;
}
Statement Statement::fromAssignment(Location location, Expression&& variable, Expression&& value) {
  Statement result(location, Type::ASSIGNMENT);
  new (&result.assignment) Assignment(move(variable), move(value));
  return result;
}

Statement Statement::fromUnion(Location location, vector<Declaration>&& declarations) {
  Statement result(location, Type::UNION);
  new (&result.union_) vector<Declaration>(move(declarations));
  return result;
}

Statement Statement::fromIf(Location location, Expression&& condition, Statement&& body) {
  Statement result(location, Type::IF);
  new (&result.if_) If(move(condition), move(body));
  return result;
}
Statement Statement::fromFor(Location location, vector<Declaration>&& range, Statement&& body) {
  Statement result(location, Type::FOR);
  new (&result.for_) For(move(range), move(body));
  return result;
}
Statement Statement::fromWhile(Location location, Expression&& condition, Statement&& body) {
  Statement result(location, Type::WHILE);
  new (&result.while_) While(move(condition), move(body));
  return result;
}
Statement Statement::fromLoop(Location location, Maybe<string>&& name, Statement&& body) {
  Statement result(location, Type::LOOP);
  new (&result.loop) Loop(move(name), move(body));
  return result;
}
Statement Statement::fromParallel(Location location, vector<Statement>&& statements) {
  Statement result(location, Type::PARALLEL);
  new (&result.parallel) vector<Statement>(move(statements));
  return result;
}

Statement Statement::fromReturn(Location location, Expression&& value) {
  Statement result(location, Type::RETURN);
  new (&result.return_) Expression(move(value));
  return result;
}
Statement Statement::fromBreak(Location location, Maybe<string>&& loopName) {
  Statement result(location, Type::BREAK);
  new (&result.break_) Maybe<string>(move(loopName));
  return result;
}
Statement Statement::fromContinue(Location location, Maybe<string>&& loopName) {
  Statement result(location, Type::CONTINUE);
  new (&result.continue_) Maybe<string>(move(loopName));
  return result;
}

Statement Statement::fromBlank(Location location) {
  return Statement(location, Type::BLANK);
}
Statement Statement::fromComment(Location location, string&& text) {
  Statement result(location, Type::COMMENT);
  new (&result.comment) string(move(text));
  return result;
}

// =======================================================================================
// iostreams

namespace {

// TODO:  Put this somewhere more general?
template <typename T>
std::ostream& operator<<(std::ostream& os, const Indirect<T>& value) {
  return os << *value;
}

}

std::ostream& operator<<(std::ostream& os, Style style) {
  switch (style) {
    case Style::VALUE: break;
    case Style::IMMUTABLE_REFERENCE: os << "@"; break;
    case Style::MUTABLE_REFERENCE:   os << "&"; break;
    case Style::ENTANGLED_REFERENCE: os << "@&"; break;
    case Style::HEAP_VALUE:          os << "*"; break;
    case Style::CONSTANT:            os << "^"; break;
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, StyleAllowance allowance) {
  switch (allowance) {
    case StyleAllowance::VALUE: break;
    case StyleAllowance::IMMUTABLE_REFERENCE: os << "@"; break;
    case StyleAllowance::MUTABLE_REFERENCE:   os << "&"; break;
    case StyleAllowance::MOVE:                os << "<-"; break;
  }
  return os;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const vector<T>& vec) {
  bool first = true;
  for (auto& element: vec) {
    if (first) {
      first = false;
    } else {
      os << ", ";
    }
    os << element;
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const Expression::FunctionCall::Parameter& parameter) {
  return os << parameter.styleAllowance << parameter.expression;
}

std::ostream& operator<<(std::ostream& os, const Expression& expression) {
  switch (expression.getType()) {
    case Expression::Type::ERROR:
      os << "{{EXPRESSION ERROR: " << expression.error << "}}";
      break;

    case Expression::Type::VARIABLE:
      os << expression.variable;
      break;
    case Expression::Type::TUPLE:
      os << "(" << expression.tuple << ")";
      break;

    case Expression::Type::LITERAL_INT:
      os << expression.literalInt;
      break;
    case Expression::Type::LITERAL_DOUBLE: {
      string result = google::protobuf::SimpleDtoa(expression.literalDouble);
      if (result.find_first_of(".eE") == string::npos) {
        result += ".0";
      }
      os << result;
      break;
    }
    case Expression::Type::LITERAL_STRING:
      os << '\"' << google::protobuf::CEscape(expression.literalString) << '\"';
      break;
    case Expression::Type::LITERAL_ARRAY:
      os << "[" << expression.literalArray << "]";
      break;

    case Expression::Type::BINARY_OPERATOR:
      os << "(" << expression.binaryOperator.left
         << " " << expression.binaryOperator.op
         << " " << expression.binaryOperator.right
         << ")";
      break;
    case Expression::Type::PREFIX_OPERATOR:
      os << "(" << expression.prefixOperator.op
         << " " << expression.prefixOperator.operand
         << ")";
      break;
    case Expression::Type::POSTFIX_OPERATOR:
      os << "(" << expression.postfixOperator.operand
         << " " << expression.postfixOperator.op
         << ")";
      break;
    case Expression::Type::TERNARY_OPERATOR:
      os << "(" << expression.ternaryOperator.condition
         << " ? " << expression.ternaryOperator.trueClause
         << " : " << expression.ternaryOperator.falseClause
         << ")";
      break;

    case Expression::Type::FUNCTION_CALL: {
      os << expression.functionCall.function
         << "(" << expression.functionCall.parameters << ")";
      break;
    }
    case Expression::Type::SUBSCRIPT:
      os << expression.subscript.container
         << "[" << expression.subscript.subscript << "]";
      break;
    case Expression::Type::MEMBER_ACCESS:
      os << expression.memberAccess.object << "." << expression.memberAccess.member;
      break;

    case Expression::Type::IMPORT:
      os << "import \"" << google::protobuf::CEscape(expression.import) << '\"';
      break;

    case Expression::Type::LAMBDA:
      os << "((" << expression.lambda.parameters << ")"
         << expression.lambda.style << " => " << expression.lambda.body << ")";
      break;
  }

  return os;
}

void Declaration::print(std::ostream& os, int indent) const {
  switch (kind) {
    case Kind::ERROR:
      os << "{{DECLARATION ERROR: " << definition->expression << "}}";
      if (indent != -1) {
        os << ";\n";
      }
      return;

    case Kind::VARIABLE: if (indent != -1) { os << "var "; } break;
    case Kind::ENVIRONMENT: os << "env "; break;

    case Kind::FUNCTION:    os << "func "; break;
    case Kind::CONSTRUCTOR: os << "constructor "; break;
    case Kind::DESTRUCTOR:  os << "destructor "; break;
    case Kind::CONVERSION:  os << "conversion"; break;
    case Kind::DEFAULT_CONVERSION: os << "default conversion "; break;

    case Kind::CLASS:       os << "class "; break;
    case Kind::INTERFACE:   os << "interface "; break;
    case Kind::ENUM:        os << "enum "; break;
  }

  os << thisStyle;
  if (name) {
    os << name->value;
  }
  if (parameters) {
    os << "(" << *parameters << ")";
  }
  os << style;

  for (auto& ann: annotations) {
    switch (ann.relationship) {
      case Annotation::Relationship::IS_A: os << ":"; break;
      case Annotation::Relationship::SUBCLASS_OF: os << " <:"; break;
      case Annotation::Relationship::SUPERCLASS_OF: os << " :>"; break;
      case Annotation::Relationship::ANNOTATION: os << " ::"; break;
    }

    if (ann.param) {
      os << " " << *ann.param;
    }
  }

  if (definition) {
    switch (definition->getType()) {
      case Declaration::Definition::Type::EXPRESSION:
        os << " = " << definition->expression;
        if (indent != -1) {
          os << ";\n";
        }
        break;
      case Declaration::Definition::Type::BLOCK:
        if (indent == -1) {
          os << "{ ... }";
        } else {
          os << " {\n";
          for (auto& statement: definition->block) {
            statement.print(os, indent + 1);
          }
          os << "}\n";
        }
        break;
    }
  } else {
    if (indent != -1) {
      os << ";\n";
    }
  }
}

std::ostream& operator<<(std::ostream& os, const ParameterDeclaration& param) {
  switch (param.getType()) {
    case ParameterDeclaration::Type::CONSTANT:
      os << param.constant;
      break;
    case ParameterDeclaration::Type::VARIABLE:
      param.variable.print(os, -1);
      break;
  }

  return os;
}

void Statement::print(std::ostream& os, int indent) const {
  for (int i = 0; i < indent; i++) {
    os << "  ";
  }

  printInner(os, indent);
}

void Statement::printInner(std::ostream& os, int indent) const {
  switch (getType()) {
    case Type::ERROR:
      os << "{{STATEMENT ERROR: " << error << "}};\n";
      break;

    case Type::EXPRESSION:
      os << expression << ";\n";
      break;
    case Type::BLOCK:
      os << "{\n";
      for (auto& statement: block) {
        statement.print(os, indent + 1);
      }
      os << "}\n";
      break;

    case Type::DECLARATION:
      declaration.print(os, indent);
      break;
    case Type::ASSIGNMENT:
      os << assignment.variable << " = " << assignment.value << ";\n";
      break;

    case Type::UNION:
      os << "union {\n";
      for (auto& decl: union_) {
        decl.print(os, indent + 1);
      }
      os << "}\n";
      break;

    case Type::IF:
      os << "if (" << if_.condition << ") ";
      if_.body->printInner(os, indent);
      break;
    case Type::ELSE:
      os << "else ";
      else_->printInner(os, indent);
      break;
    case Type::FOR:
      os << "for (" << for_.range << ") ";
      for_.body->printInner(os, indent);
      break;
    case Type::WHILE:
      os << "while (" << while_.condition << ") ";
      while_.body->printInner(os, indent);
      break;
    case Type::LOOP:
      os << "loop ";
      if (loop.name) {
        os << *loop.name << " ";
      }
      loop.body->printInner(os, indent);
      break;
    case Type::PARALLEL:
      os << "parallel {\n";
      for (auto& statement: parallel) {
        statement.print(os, indent + 1);
      }
      os << "}\n";
      break;

    case Type::RETURN:
      os << "return";
      if (return_.getType() != Expression::Type::TUPLE ||
          return_.tuple.size() != 0) {
        os << " " << return_;
      }
      os << ";\n";
      break;
    case Type::BREAK:
      os << "break";
      if (break_) {
        os << " " << *break_;
      }
      os << ";\n";
      break;
    case Type::CONTINUE:
      os << "continue";
      if (continue_) {
        os << " " << *continue_;
      }
      os << ";\n";
      break;

    case Type::BLANK:
      os << "\n";
      break;
    case Type::COMMENT: {
      string::size_type pos = 0;
      while (true) {
        string::size_type start = pos;
        pos = comment.find_first_of('\n');
        if (pos == string::npos) {
          os << "#" << comment.substr(start);
          break;
        } else {
          os << "#" << comment.substr(start, pos - start);
          ++pos;
        }
      }
      os << "\n";
      break;
    }
  }
}

}  // namespace ast
}  // namespace modc
