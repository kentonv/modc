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
#include <map>

#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"
#include "CodePrinter.h"
#if 0
namespace modc {
namespace ast {

using std::move;

template <typename T>
void destroy(T& obj) {
  obj.~T();
}

// =======================================================================================
// Expression

#define FOR_ALL_EXPRESSIONS(HANDLE) \
  HANDLE(ERROR, error, vector<errors::Error>) \
  HANDLE(VARIABLE, variable, string) \
  HANDLE(TUPLE, tuple, vector<ListElement>) \
  HANDLE(LITERAL_INT, literalInt, int) \
  HANDLE(LITERAL_DOUBLE, literalDouble, double) \
  HANDLE(LITERAL_STRING, literalString, string) \
  HANDLE(LITERAL_ARRAY, literalArray, vector<ListElement>) \
  HANDLE(BINARY_OPERATOR, binaryOperator, BinaryOperator) \
  HANDLE(PREFIX_OPERATOR, prefixOperator, PrefixOperator) \
  HANDLE(POSTFIX_OPERATOR, postfixOperator, PostfixOperator) \
  HANDLE(TERNARY_OPERATOR, ternaryOperator, TernaryOperator) \
  HANDLE(FUNCTION_CALL, functionCall, FunctionCall) \
  HANDLE(SUBSCRIPT, subscript, Subscript) \
  HANDLE(MEMBER_ACCESS, memberAccess, MemberAccess) \
  HANDLE(IMPORT, import, string) \
  HANDLE(LAMBDA, lambda, Lambda)

Expression::Expression(Expression&& other): location(other.location), type(other.type) {
  switch (type) {
#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
    FOR_ALL_EXPRESSIONS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT
  }
}

Expression::Expression(const Expression& other): location(other.location), type(other.type) {
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
      destroy(NAME); \
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

Expression Expression::fromTuple(Location location, vector<ListElement>&& elements) {
  Expression result(location, Type::TUPLE);
  new (&result.tuple) vector<ListElement>(move(elements));
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
Expression Expression::fromLiteralArray(Location location, vector<ListElement>&& elements) {
  Expression result(location, Type::LITERAL_ARRAY);
  new (&result.literalArray) vector<ListElement>(move(elements));
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
Expression Expression::fromMemberAccess(Location location, Expression&& object, string&& member,
                                        StyleAllowance thisStyleAllowance) {
  Expression result(location, Type::MEMBER_ACCESS);
  new (&result.memberAccess) MemberAccess(move(object), move(member), thisStyleAllowance);
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

ListElement ListElement::fromError(Location location, vector<errors::Error>&& errors) {
  return ListElement(vector<Declaration>(), nullptr, nullptr,
                     Expression::fromError(location, move(errors)));
}

// =======================================================================================
// Declaration

Declaration::Declaration(Location location, Kind kind)
    : kind(kind), thisStyle(Style::VALUE), style(Style::VALUE), location(location) {}
Declaration::~Declaration() {}

bool Declaration::operator==(const Declaration& other) const {
  static_assert(sizeof(Declaration) == 376, "Please update Declaration::operator==.");
  return visibility == other.visibility &&
         kind == other.kind &&
         thisStyle == other.thisStyle &&
         style == other.style &&
         name == other.name &&
         parameters == other.parameters &&
         type == other.type &&
         annotations == other.annotations &&
         documentation == other.documentation &&
         definition == other.definition &&
         comment == other.comment;
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
      destroy(expression);
      break;
    case Type::BLOCK:
      destroy(block);
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
      destroy(constant);
      break;
    case Type::VARIABLE:
      destroy(variable);
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
  HANDLE(CONTINUE, continue_, Maybe<string>)

Statement::Statement(Statement&& other)
    : comment(other.comment), location(other.location), type(other.type) {
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

Statement::Statement(const Statement& other)
    : comment(other.comment), location(other.location), type(other.type) {
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
      destroy(NAME); \
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
  if (comment != other.comment) {
    return false;
  }

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
Statement Statement::fromAssignment(Location location, Expression&& variable,
                                    Maybe<string>&& compoundOp, Expression&& value) {
  Statement result(location, Type::ASSIGNMENT);
  new (&result.assignment) Assignment(move(variable), move(compoundOp), move(value));
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
Statement Statement::fromElse(Location location, Statement&& body) {
  Statement result(location, Type::ELSE);
  new (&result.else_) Indirect<Statement>(move(body));
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

// =======================================================================================
// iostreams

namespace {

// TODO:  Put this somewhere more general?
template <typename T>
CodePrinter& operator<<(CodePrinter& printer, const Indirect<T>& value) {
  return printer << *value;
}

template <typename T>
CodePrinter& operator<<(CodePrinter& printer, const vector<T>& vec) {
  bool first = true;
  for (auto& element: vec) {
    if (first) {
      first = false;
    } else {
      printer << glue << ", ";
    }
    printer << element;
  }
  return printer;
}

CodePrinter& operator<<(CodePrinter& printer, const errors::Error& error) {
  return printer << "(" << glue << error.message << glue << ")";
}

vector<vector<string>> operators = {
  vector<string> {"*", "/", "%"},
  vector<string> {"+", "-"},
  vector<string> {"<<", ">>"},
  vector<string> {"<", ">", "<=", ">="},
  vector<string> {"==", "!="},
  vector<string> {"&"},
  vector<string> {"^"},
  vector<string> {"|"},
  vector<string> {"&&"},
  vector<string> {"||"}
};

std::map<string, int> operatorPriorities;

struct Initializer {
  Initializer() {
    for (size_t i = 0; i < operators.size(); i++) {
      for (const string& op: operators[i]) {
        operatorPriorities[op] = 50 - i;
      }
    }
  }
};
Initializer initializer;

int priority(const Expression& exp) {
  switch (exp.getType()) {
    case Expression::Type::ERROR:
    case Expression::Type::VARIABLE:
    case Expression::Type::TUPLE:
    case Expression::Type::LITERAL_INT:
    case Expression::Type::LITERAL_DOUBLE:
    case Expression::Type::LITERAL_STRING:
    case Expression::Type::LITERAL_ARRAY:
    case Expression::Type::IMPORT:
      return 100;

    case Expression::Type::FUNCTION_CALL:
    case Expression::Type::SUBSCRIPT:
    case Expression::Type::MEMBER_ACCESS:
    case Expression::Type::POSTFIX_OPERATOR:
      return 90;

    case Expression::Type::PREFIX_OPERATOR:
      return 80;

    case Expression::Type::BINARY_OPERATOR: {
      auto iter = operatorPriorities.find(exp.binaryOperator.op);
      if (iter == operatorPriorities.end()) {
        DEBUG_ERROR << "Missing operator priority: " << exp.binaryOperator.op;
        return 50;
      } else {
        return iter->second;
      }
    }

    case Expression::Type::TERNARY_OPERATOR:
      return 10;

    case Expression::Type::LAMBDA:
      // Well, it's really only a zero if stuff appear on the right side of the lambda...
      return 0;
  }

  throw "can't get here";
}

}  // namespace

CodePrinter& operator<<(CodePrinter& printer, Style style) {
  switch (style) {
    case Style::VALUE:               printer << ""; break;
    case Style::IMMUTABLE_REFERENCE: printer << "@"; break;
    case Style::MUTABLE_REFERENCE:   printer << "&"; break;
    case Style::ENTANGLED_REFERENCE: printer << "@&"; break;
    case Style::HEAP_VALUE:          printer << "*"; break;
    case Style::CONSTANT:            printer << "^"; break;
  }
  return printer;
}

CodePrinter& operator<<(CodePrinter& printer, StyleAllowance allowance) {
  switch (allowance) {
    case StyleAllowance::VALUE:               printer << ""; break;
    case StyleAllowance::IMMUTABLE_REFERENCE: printer << "@"; break;
    case StyleAllowance::MUTABLE_REFERENCE:   printer << "&"; break;
    case StyleAllowance::MOVE:                printer << "<-"; break;
  }
  return printer;
}

CodePrinter& operator<<(CodePrinter& printer,
                        const Expression::FunctionCall::Parameter& parameter) {
  return printer << parameter.styleAllowance << parameter.expression;
}

void Expression::print(CodePrinter& printer, int minPriority) const {
  int pri = priority(*this);
  if (pri < minPriority) {
    printer << "(" << glue;
  }

  switch (getType()) {
    case Expression::Type::ERROR:
      printer << "{{EXPRESSION ERROR: " << error << glue << "}}";
      break;

    case Expression::Type::VARIABLE:
      printer << variable;
      break;
    case Expression::Type::TUPLE:
      // Expressions in the array never need parenthesization.
      printer << "(" << glue << tuple << glue << ")";
      break;

    case Expression::Type::LITERAL_INT:
      printer << google::protobuf::SimpleItoa(literalInt);
      break;
    case Expression::Type::LITERAL_DOUBLE: {
      string result = google::protobuf::SimpleDtoa(literalDouble);
      if (result.find_first_of(".eE") == string::npos) {
        result += ".0";
      } else {
        // SimpleDtoa() likes to put plus signs on positive exponents.  Remove it.
        string::size_type pos = result.find_first_of('+');
        if (pos != string::npos) {
          result.erase(pos, 1);
        }
      }
      printer << result;
      break;
    }
    case Expression::Type::LITERAL_STRING:
      // TODO:  Split on line breaks.
      printer << "\"" << glue << google::protobuf::CEscape(literalString) << glue << "\"";
      break;
    case Expression::Type::LITERAL_ARRAY:
      // Expressions in the list never need parenthesization.
      printer << "[" << glue << literalArray << glue << "]";
      break;

    case Expression::Type::BINARY_OPERATOR: {
      // Left side can have equal priority to expression.
      binaryOperator.left->print(printer, pri);

      printer << space << glue << binaryOperator.op << space;

      // Right side must have higher priority.
      binaryOperator.right->print(printer, pri + 1);

      break;
    }
    case Expression::Type::PREFIX_OPERATOR: {
      printer << prefixOperator.op << glue;
      prefixOperator.operand->print(printer, pri);
      break;
    }
    case Expression::Type::POSTFIX_OPERATOR:
      postfixOperator.operand->print(printer, pri);
      printer << glue << postfixOperator.op;
      break;
    case Expression::Type::TERNARY_OPERATOR:
      // The middle part doesn't need parenthesization, but the ends might.  The ternary operator
      // is right-to-left associative, so the condition needs to have pri + 1 but the false clause
      // can have just pri.
      ternaryOperator.condition->print(printer, pri + 1);
      printer << glue << " ? " << ternaryOperator.trueClause << glue << " : ";
      ternaryOperator.falseClause->print(printer, pri);
      break;

    case Expression::Type::FUNCTION_CALL: {
      functionCall.function->print(printer, pri);
      // Parameters don't need parenthesization.
      printer << glue << "(" << functionCall.parameters << glue << ")";
      break;
    }
    case Expression::Type::SUBSCRIPT:
      subscript.container->print(printer, pri);
      // Subscript doesn't need parenthesization.
      printer << glue << "[" << subscript.subscript << glue << "]";
      break;
    case Expression::Type::MEMBER_ACCESS:
      memberAccess.object->print(printer, pri);
      switch (memberAccess.thisStyleAllowance) {
        case StyleAllowance::VALUE:               printer << "."; break;
        case StyleAllowance::IMMUTABLE_REFERENCE: printer << "."; break;
        case StyleAllowance::MUTABLE_REFERENCE:   printer << ".&"; break;
        case StyleAllowance::MOVE:                printer << "->"; break;
      }
      printer << glue << memberAccess.member;
      break;

    case Expression::Type::IMPORT:
      printer << "import \"" << glue << google::protobuf::CEscape(import) << glue << "\"";
      break;

    case Expression::Type::LAMBDA:
      printer << "(" << glue << lambda.parameters << glue << ")" << glue
              << lambda.style << glue << " => ";
      lambda.body->print(printer, pri);
      break;
  }

  if (pri < minPriority) {
    printer << glue << ")";
  }
}

CodePrinter& operator<<(CodePrinter& printer, const ListElement& element) {
  if (!element.ranges.empty()) {
    printer << "for (" << glue << element.ranges << glue << ") ";
  }
  if (element.condition) {
    printer << "if (" << glue << *element.condition << glue << ") ";
  }
  if (element.name) {
    printer << *element.name << glue << " = ";
  }

  printer << element.value;

  return printer;
}

CodePrinter& operator<<(CodePrinter& printer, const Visibility& visibility) {
  switch (visibility.type) {
    case Visibility::Type::PUBLIC:
      printer << "public";
      break;
    case Visibility::Type::PRIVATE:
      printer << "private";
      break;
  }

  if (!visibility.friends.empty()) {
    printer << glue << "(" << glue << visibility.friends << glue << ")";
  }

  printer << space;
  return printer;
}

void Declaration::print(CodePrinter& printer, bool asStatement) const {
  if (visibility) {
    printer << *visibility;
  }

  switch (kind) {
    case Kind::ERROR:
      printer << "{{DECLARATION ERROR: " << definition->expression.error << glue << "}}";
      if (asStatement) {
        printer << glue << ";" << endStatement;
      }
      return;

    case Kind::VARIABLE: if (asStatement) { printer << "var "; } break;
    case Kind::ENVIRONMENT: printer << "env "; break;

    case Kind::FUNCTION:    printer << "func "; break;
    case Kind::CONSTRUCTOR: printer << "ctor "; break;
    case Kind::DESTRUCTOR:  printer << "dtor "; break;
    case Kind::CONVERSION:  printer << "conv "; break;
    case Kind::DEFAULT_CONVERSION: printer << "default conv "; break;
    case Kind::OPERATOR:    printer << "operator "; break;

    case Kind::CLASS:       printer << "class "; break;
    case Kind::INTERFACE:   printer << "interface "; break;
    case Kind::ENUM:        printer << "enum "; break;
  }

  printer << thisStyle << glue;
  if (name) {
    printer << name->value;
  } else {
    printer << noSpace;
  }
  if (parameters) {
    printer << glue << "(" << *parameters << glue << ")";
  }
  printer << glue << style;

  if (kind == Kind::FUNCTION) {
    if (!type) {
      printer << glue << ": ";
    } else if (type->getType() == Expression::Type::TUPLE && type->tuple.empty()) {
      // empty tuple return -- nothing to print
    } else {
      printer << glue << ": " << *type;
    }
  } else {
    if (type) {
      printer << glue << ": " << glue << *type;
    }
  }

  for (auto& ann: annotations) {
    switch (ann.relationship) {
      case Annotation::Relationship::LESS_THAN: printer << space << "< "; break;
      case Annotation::Relationship::SUBCLASS_OF: printer << space << "<: "; break;
      case Annotation::Relationship::SUPERCLASS_OF: printer << space << ":> "; break;
      case Annotation::Relationship::ANNOTATION: printer << space << ":: "; break;
    }

    if (ann.param) {
      printer << glue << *ann.param;
    }
  }

  if (definition) {
    switch (definition->getType()) {
      case Declaration::Definition::Type::EXPRESSION:
        printer << space << glue << "= " << definition->expression;
        if (asStatement) {
          printer << ";" << endStatement;
        }
        break;
      case Declaration::Definition::Type::BLOCK:
        if (asStatement) {
          printer << " {" << endStatement << startBlock;
          for (auto& statement: definition->block) {
            printer << statement;
          }
          printer << endBlock << "}" << endStatement;
        } else {
          DEBUG_ERROR << "Printing block declaration as non-statement.";
          printer << "{ ... }";
        }
        break;
    }
  } else {
    if (asStatement) {
      printer << ";" << endStatement;
    }
  }
}

CodePrinter& operator<<(CodePrinter& printer, const ParameterDeclaration& param) {
  switch (param.getType()) {
    case ParameterDeclaration::Type::CONSTANT:
      printer << param.constant;
      break;
    case ParameterDeclaration::Type::VARIABLE:
      param.variable.print(printer, false);
      break;
  }

  return printer;
}

CodePrinter& operator<<(CodePrinter& printer, const Statement& stmt) {
  switch (stmt.getType()) {
    case Statement::Type::ERROR:
      printer << "{{STATEMENT ERROR: " << stmt.error << glue << "}};";
      break;

    case Statement::Type::EXPRESSION:
      printer << stmt.expression << glue << ";";
      break;
    case Statement::Type::BLOCK:
      printer << "{" << endStatement << startBlock;
      for (auto& statement: stmt.block) {
        printer << statement;
      }
      printer << endBlock << "}";
      break;

    case Statement::Type::DECLARATION:
      stmt.declaration.print(printer, true);
      return printer;
    case Statement::Type::ASSIGNMENT:
      printer << stmt.assignment.variable << glue << space;
      if (stmt.assignment.compoundOp) {
        printer << *stmt.assignment.compoundOp << glue;
      }
      printer << "= " << stmt.assignment.value << glue << ";";
      break;

    case Statement::Type::UNION:
      printer << "union {" << endStatement << startBlock;
      for (auto& decl: stmt.union_) {
        decl.print(printer, true);
      }
      printer << endBlock << "}";
      break;

    case Statement::Type::IF:
      printer << "if (" << glue << stmt.if_.condition << glue << ") " << stmt.if_.body;
      return printer;
    case Statement::Type::ELSE:
      printer.extendLineIfEquals("}");
      printer << space << "else " << stmt.else_;
      return printer;
    case Statement::Type::FOR:
      printer << "for (" << glue << stmt.for_.range << glue << ") " << stmt.for_.body;
      return printer;
    case Statement::Type::WHILE:
      printer << "while (" << glue << stmt.while_.condition << glue << ") " << stmt.while_.body;
      return printer;
    case Statement::Type::LOOP:
      printer << "loop ";
      if (stmt.loop.name) {
        printer << glue << *stmt.loop.name << space;
      }
      printer << stmt.loop.body;
      return printer;
    case Statement::Type::PARALLEL:
      printer << "parallel {" << endStatement << startBlock;
      for (auto& statement: stmt.parallel) {
        printer << statement;
      }
      printer << endBlock << "}";
      break;

    case Statement::Type::RETURN:
      printer << "return";
      if (stmt.return_.getType() != Expression::Type::TUPLE ||
          stmt.return_.tuple.size() != 0) {
        printer << space << glue << stmt.return_;
      }
      printer << glue << ";";
      break;
    case Statement::Type::BREAK:
      printer << "break";
      if (stmt.break_) {
        printer << space << glue << *stmt.break_;
      }
      printer << glue << ";";
      break;
    case Statement::Type::CONTINUE:
      printer << "continue";
      if (stmt.continue_) {
        printer << space << glue << *stmt.continue_;
      }
      printer << glue << ";";
      break;

    case Statement::Type::BLANK:
      // Nothing.
      break;
  }

  if (stmt.comment) {
    printer.advanceToColumn(stmt.comment->location.start.column);
    printer << space << "# " << glue << stmt.comment->value;
  }

  printer << endStatement;

  return printer;
}

}  // namespace ast
}  // namespace modc
