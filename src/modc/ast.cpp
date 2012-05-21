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

namespace modc {
namespace ast {

using std::move;

// =======================================================================================

std::map<BinaryOperator, BinaryOperatorInfo> makeBinaryOperatorInfoByOpMap() {
  std::map<BinaryOperator, BinaryOperatorInfo> result;

#define OP(ID, NAME, PRIORITY) \
  result.insert(std::make_pair(BinaryOperator::ID, \
      BinaryOperatorInfo(BinaryOperator::ID, NAME, PRIORITY)));

  OP(MULTIPLY, "*", 10);
  OP(DIVIDE, "/", 10);
  OP(DIVIDE_AND_FLOOR, "//", 10);
  OP(MODULUS, "%", 10);

  OP(ADD, "+", 9);
  OP(SUBTRACT, "-", 9);

  OP(LEFT_SHIFT, "<<", 8);
  OP(RIGHT_SHIFT, ">>", 8);

  OP(LESS_THAN, "<", 7);
  OP(GREATER_THAN, ">", 7);
  OP(LESS_THAN_OR_EQUAL, "<=", 7);
  OP(GREATER_THAN_OR_EQUAL, ">=", 7);

  OP(EQUALS, "==", 6);
  OP(NOT_EQUALS, "!=", 6);

  OP(BITWISE_AND, "&", 5);
  OP(BITWISE_XOR, "^", 4);
  OP(BITWISE_OR, "|", 3);

  OP(LOGICAL_AND, "&&", 2);
  OP(LOGICAL_OR, "||", 1);

  return result;
}

std::map<PrefixOperator, string> makePrefixOperatorNameMap() {
  std::map<PrefixOperator, string> result;

  result[PrefixOperator::INCREMENT] = "++";
  result[PrefixOperator::DECREMENT] = "--";
  result[PrefixOperator::POSITIVE] = "+";
  result[PrefixOperator::NEGATIVE] = "-";
  result[PrefixOperator::LOGICAL_NOT] = "!";
  result[PrefixOperator::BITWISE_NOT] = "~";

  return result;
}

std::map<PostfixOperator, string> makePostfixOperatorNameMap() {
  std::map<PostfixOperator, string> result;

  result[PostfixOperator::INCREMENT] = "++";
  result[PostfixOperator::DECREMENT] = "--";

  return result;
}

const std::map<BinaryOperator, BinaryOperatorInfo>& getBinaryOperatorInfoByOpMap() {
  static const std::map<BinaryOperator, BinaryOperatorInfo> result =
      makeBinaryOperatorInfoByOpMap();
  return result;
}

const std::map<PrefixOperator, string>& getPrefixOperatorNameMap() {
  static const std::map<PrefixOperator, string> result = makePrefixOperatorNameMap();
  return result;
}

const std::map<PostfixOperator, string>& getPostfixOperatorNameMap() {
  static const std::map<PostfixOperator, string> result = makePostfixOperatorNameMap();
  return result;
}

std::map<string, BinaryOperatorInfo> makeBinaryOperatorInfoByNameMap() {
  std::map<string, BinaryOperatorInfo> result;

  for (auto& entry: getBinaryOperatorInfoByOpMap()) {
    result.insert(std::make_pair(entry.second.name, entry.second));
  }

  return result;
}

const std::map<string, BinaryOperatorInfo>& getBinaryOperatorInfoByNameMap() {
  static const std::map<string, BinaryOperatorInfo> result = makeBinaryOperatorInfoByNameMap();
  return result;
}

template <typename T, typename U>
std::map<U, T> reverse(const std::map<T, U>& orig) {
  std::map<U, T> result;

  for (auto& entry: orig) {
    result.insert(std::make_pair(entry.second, entry.first));
  }

  return result;
}

const std::map<string, PrefixOperator>& getPrefixOperatorByNameMap() {
  static const std::map<string, PrefixOperator> result = reverse(getPrefixOperatorNameMap());
  return result;
}

const std::map<string, PostfixOperator>& getPostfixOperatorByNameMap() {
  static const std::map<string, PostfixOperator> result = reverse(getPostfixOperatorNameMap());
  return result;
}

const BinaryOperatorInfo& getBinaryOperatorInfo(BinaryOperator op) {
  return getBinaryOperatorInfoByOpMap().find(op)->second;
}

Maybe<const BinaryOperatorInfo&> findBinaryOperatorInfo(const string& name) {
  auto& map = getBinaryOperatorInfoByNameMap();
  auto iter = map.find(name);
  if (iter == map.end()) {
    return nullptr;
  } else {
    return iter->second;
  }
}

const string& getPrefixOperatorName(PrefixOperator op) {
  return getPrefixOperatorNameMap().find(op)->second;
}

Maybe<PrefixOperator> findPrefixOperator(const string& name) {
  auto& map = getPrefixOperatorByNameMap();
  auto iter = map.find(name);
  if (iter == map.end()) {
    return nullptr;
  } else {
    return iter->second;
  }
}

const string& getPostfixOperatorName(PostfixOperator op) {
  return getPostfixOperatorNameMap().find(op)->second;
}

Maybe<PostfixOperator> findPostfixOperator(const string& name) {
  auto& map = getPostfixOperatorByNameMap();
  auto iter = map.find(name);
  if (iter == map.end()) {
    return nullptr;
  } else {
    return iter->second;
  }
}

// =======================================================================================
// Expression

#define EXPRESSION_UNION(HANDLE) \
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

#define EXPRESSION_EXTRAS(HANDLE) \
  HANDLE(location, false)

UNION_IMPLEMENT(, Expression, EXPRESSION_UNION, EXPRESSION_EXTRAS)

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

Expression Expression::fromBinaryOperator(Location location, ast::BinaryOperator op,
                                          Expression&& left, Expression&& right) {
  Expression result(location, Type::BINARY_OPERATOR);
  new (&result.binaryOperator) BinaryOperator(op, move(left), move(right));
  return result;
}
Expression Expression::fromPrefixOperator(Location location, ast::PrefixOperator op,
                                          Expression&& exp) {
  Expression result(location, Type::PREFIX_OPERATOR);
  new (&result.prefixOperator) PrefixOperator(op, move(exp));
  return result;
}
Expression Expression::fromPostfixOperator(Location location, Expression&& exp,
                                           ast::PostfixOperator op) {
  Expression result(location, Type::POSTFIX_OPERATOR);
  new (&result.postfixOperator) PostfixOperator(move(exp), op);
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
    : isDefault(false), kind(kind), thisStyle(Style::VALUE), style(Style::VALUE),
      location(location) {}
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

#define DEFINITION_UNION(HANDLE) \
  HANDLE(EXPRESSION, expression, Expression) \
  HANDLE(BLOCK, block, vector<Statement>)

#define DEFINITION_EXTRAS(HANDLE)

UNION_IMPLEMENT(Declaration::, Definition, DEFINITION_UNION, DEFINITION_EXTRAS)

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

#define PARAMETER_DECLARATION_UNION(HANDLE) \
  HANDLE(CONSTANT, constant, Expression) \
  HANDLE(VARIABLE, variable, Declaration)

#define PARAMETER_DECLARATION_EXTRAS(HANDLE)

UNION_IMPLEMENT(, ParameterDeclaration, PARAMETER_DECLARATION_UNION, PARAMETER_DECLARATION_EXTRAS)

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

#define STATEMENT_UNION(HANDLE) \
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
  HANDLE(ASSERT, assert_, Assert) \
  HANDLE(DEBUG, debug, vector<Expression>) \
  HANDLE(BLANK, blank, Blank)

#define STATEMENT_EXTRAS(HANDLE) \
  HANDLE(comment, true) \
  HANDLE(location, false)

UNION_IMPLEMENT(, Statement, STATEMENT_UNION, STATEMENT_EXTRAS);

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
                                    Maybe<BinaryOperator>&& compoundOp, Expression&& value) {
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

Statement Statement::fromAssert(Location location, Expression&& condition,
                                vector<Expression>&& debugInfo) {
  Statement result(location, Type::ASSERT);
  new (&result.assert_) Assert(move(condition), move(debugInfo), true);
  return result;
}

Statement Statement::fromExpect(Location location, Expression&& condition,
                                vector<Expression>&& debugInfo) {
  Statement result(location, Type::ASSERT);
  new (&result.assert_) Assert(move(condition), move(debugInfo), false);
  return result;
}

Statement Statement::fromDebug(Location location, vector<Expression>&& debugInfo) {
  Statement result(location, Type::DEBUG);
  new (&result.debug) vector<Expression>(move(debugInfo));
  return result;
}

Statement Statement::fromBlank(Location location) {
  Statement result(location, Type::BLANK);
  new (&result.blank) Blank;
  return result;
}

// =======================================================================================
// iostreams

namespace {

// TODO:  Put this somewhere more general?
template <typename T>
CodePrinter& operator<<(CodePrinter& printer, const Indirect<T>& value) {
  return printer << *value;
}

template <typename T, typename BreakMarker>
void writeList(CodePrinter& printer, const vector<T>& vec, BreakMarker breakMarker) {
  bool first = true;
  for (auto& element: vec) {
    if (first) {
      first = false;
    } else {
      printer << ", " << breakMarker;
    }
    printer << element;
  }
}

template <typename T>
CodePrinter& operator<<(CodePrinter& printer, const vector<T>& vec) {
  printer << startSubExpression;
  writeList(printer, vec, breakable(0));
  printer << endSubExpression;
  return printer;
}

CodePrinter& operator<<(CodePrinter& printer, const errors::Error& error) {
  return printer << string("(") << error.message << string(")");
}

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

    case Expression::Type::BINARY_OPERATOR:
      return 40 + getBinaryOperatorInfo(exp.binaryOperator.op).priority;

    case Expression::Type::TERNARY_OPERATOR:
      return 20;

    case Expression::Type::LAMBDA:
      // TODO:  In theory only needs parentheses if stuff appears on the right.
      return 10;
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
    printer << "(" << startSubExpression;
  }

  switch (getType()) {
    case Expression::Type::ERROR: {
      auto formatScope = printer.addFormat(format::ERROR);
      printer << string("{{EXPRESSION ERROR: ") << error << string("}}");
      break;
    }

    case Expression::Type::VARIABLE:
      printer << variable;
      break;
    case Expression::Type::TUPLE:
      // Expressions in the tuple never need individual parenthesization.
      printer << "(" << tuple << ")";
      break;

    case Expression::Type::LITERAL_INT:
      printer << formatted(format::LITERAL, google::protobuf::SimpleItoa(literalInt));
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
      printer << formatted(format::LITERAL, result);
      break;
    }
    case Expression::Type::LITERAL_STRING: {
      // TODO:  Split on line breaks.
      auto formatScope = printer.addFormat(format::LITERAL);
      printer << string("\"") << google::protobuf::CEscape(literalString) << string("\"");
      break;
    }
    case Expression::Type::LITERAL_ARRAY:
      // Expressions in the list never need parenthesization.
      printer << "[" << literalArray << "]";
      break;

    case Expression::Type::BINARY_OPERATOR: {
      // Left side can have equal priority to expression.
      binaryOperator.left->print(printer, pri);

      const string& name = getBinaryOperatorInfo(binaryOperator.op).name;
      printer << space << formatted(format::OPERATOR, name) << breakable(pri) << space;

      // Right side must have higher priority.
      binaryOperator.right->print(printer, pri + 1);

      break;
    }
    case Expression::Type::PREFIX_OPERATOR: {
      printer << formatted(format::OPERATOR, getPrefixOperatorName(prefixOperator.op));
      prefixOperator.operand->print(printer, pri);
      break;
    }
    case Expression::Type::POSTFIX_OPERATOR:
      postfixOperator.operand->print(printer, pri);
      printer << formatted(format::OPERATOR, getPostfixOperatorName(postfixOperator.op));
      break;
    case Expression::Type::TERNARY_OPERATOR:
      // The middle part doesn't need parenthesization, but the ends might.  The ternary operator
      // is right-to-left associative, so the condition needs to have pri + 1 but the false clause
      // can have just pri.
      ternaryOperator.condition->print(printer, pri + 1);
      printer << " ? " << breakable(pri) << ternaryOperator.trueClause << " : " << breakable(pri);
      ternaryOperator.falseClause->print(printer, pri);
      break;

    case Expression::Type::FUNCTION_CALL: {
      functionCall.function->print(printer, pri);
      // Parameters don't need parenthesization.
      printer << "(" << startParameters(pri);
      writeList(printer, functionCall.parameters, nextParameter);
      printer << endParameters << ")";
      break;
    }
    case Expression::Type::SUBSCRIPT:
      subscript.container->print(printer, pri);
      // Subscript doesn't need parenthesization.
      printer << "[" << startParameters(pri) << subscript.subscript << endParameters << "]";
      break;
    case Expression::Type::MEMBER_ACCESS:
      memberAccess.object->print(printer, pri);
      printer << breakable(pri);
      switch (memberAccess.thisStyleAllowance) {
        case StyleAllowance::VALUE:               printer << "."; break;
        case StyleAllowance::IMMUTABLE_REFERENCE: printer << "."; break;
        case StyleAllowance::MUTABLE_REFERENCE:   printer << ".&"; break;
        case StyleAllowance::MOVE:                printer << "->"; break;
      }
      printer << memberAccess.member;
      break;

    case Expression::Type::IMPORT: {
      printer << "import ";
      auto formatScope = printer.addFormat(format::LITERAL);
      printer << string("\"") << google::protobuf::CEscape(import) << string("\"");
      break;
    }

    case Expression::Type::LAMBDA:
      printer << "(" << lambda.parameters << ")" << lambda.style << " => " << breakable(pri);
      lambda.body->print(printer, pri);
      break;
  }

  if (pri < minPriority) {
    printer << endSubExpression << ")";
  }
}

CodePrinter& operator<<(CodePrinter& printer, const ListElement& element) {
  if (!element.ranges.empty()) {
    printer << "for (" << element.ranges << ") " << breakable(1);
  }
  if (element.condition) {
    printer << "if (" << *element.condition << ") " << breakable(1);
  }
  if (element.name) {
    printer << *element.name << " = ";
  }

  printer << startSubExpression << element.value;

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
    printer << "(" << visibility.friends << ")";
  }

  printer << space;
  return printer;
}

void Declaration::print(CodePrinter& printer, bool asStatement) const {
  if (visibility) {
    printer << *visibility;
  }

  if (isDefault) {
    printer << "default ";
  }

  switch (kind) {
    case Kind::ERROR: {
      auto formatScope = printer.addFormat(format::ERROR);
      printer << string("{{DECLARATION ERROR: ") << definition->expression.error << string("}}");
      if (asStatement) {
        printer << ";" << endStatement;
      }
      return;
    }

    case Kind::VARIABLE: if (asStatement) { printer << "var "; } break;
    case Kind::ENVIRONMENT: printer << "env "; break;

    case Kind::FUNCTION:    printer << "func "; break;
    case Kind::CONSTRUCTOR: printer << "ctor "; break;
    case Kind::DESTRUCTOR:  printer << "dtor "; break;
    case Kind::CONVERSION:  printer << "conv "; break;
    case Kind::OPERATOR:    printer << "operator "; break;

    case Kind::CLASS:       printer << "class "; break;
    case Kind::INTERFACE:   printer << "interface "; break;
    case Kind::ENUM:        printer << "enum "; break;
  }

  printer << thisStyle;
  if (name) {
    printer << formatted(format::DECLARATION, name->value);
  } else {
    printer << noSpace;
  }
  if (parameters) {
    printer << "(" << startParameters(0);
    writeList(printer, *parameters, nextParameter);
    printer << ")" << endParameters;
  }
  printer << style;

  if (kind == Kind::FUNCTION) {
    if (!type) {
      printer << breakable(5) << ": ";
    } else if (type->getType() == Expression::Type::TUPLE && type->tuple.empty()) {
      // empty tuple return -- nothing to print
    } else {
      printer << breakable(5) << ": " << *type;
    }
  } else {
    if (type) {
      printer << breakable(5) << ": " << *type;
    }
  }

  for (auto& ann: annotations) {
    printer << breakable(5);
    switch (ann.relationship) {
      case Annotation::Relationship::LESS_THAN: printer << space << "< "; break;
      case Annotation::Relationship::SUBCLASS_OF: printer << space << "<: "; break;
      case Annotation::Relationship::SUPERCLASS_OF: printer << space << ":> "; break;
      case Annotation::Relationship::ANNOTATION: printer << space << ":: "; break;
    }

    if (ann.param) {
      printer << *ann.param;
    }
  }

  if (definition) {
    switch (definition->getType()) {
      case Declaration::Definition::Type::EXPRESSION:
        printer << space << "= " << breakable(5) << definition->expression;
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
      {
        auto formatScope = printer.addFormat(format::ERROR);
        printer << string("{{STATEMENT ERROR: ") << stmt.error << string("}}");
      }
      printer << ";";
      break;

    case Statement::Type::EXPRESSION:
      printer << stmt.expression << ";";
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
      printer << stmt.assignment.variable << space;
      if (stmt.assignment.compoundOp) {
        printer << formatted(format::OPERATOR,
                             getBinaryOperatorInfo(*stmt.assignment.compoundOp).name);
      }
      printer << "= " << breakable(0) << stmt.assignment.value << ";";
      break;

    case Statement::Type::UNION:
      printer << "union {" << endStatement << startBlock;
      for (auto& decl: stmt.union_) {
        decl.print(printer, true);
      }
      printer << endBlock << "}";
      break;

    case Statement::Type::IF:
      printer << "if (" << stmt.if_.condition << ") ";
      if (stmt.if_.body->getType() != Statement::Type::BLOCK) {
        printer << breakable(0);
      }
      printer << startSubExpression << stmt.if_.body;
      return printer;
    case Statement::Type::ELSE:
      printer.extendLineIfEquals("}");
      printer << space << "else ";
      if (stmt.else_->getType() != Statement::Type::BLOCK) {
        printer << breakable(0);
      }
      printer << startSubExpression << stmt.else_;
      return printer;
    case Statement::Type::FOR:
      printer << "for (" << stmt.for_.range << ") ";
      if (stmt.for_.body->getType() != Statement::Type::BLOCK) {
        printer << breakable(0);
      }
      printer << startSubExpression << stmt.for_.body;
      return printer;
    case Statement::Type::WHILE:
      printer << "while (" << stmt.while_.condition << ") ";
      if (stmt.while_.body->getType() != Statement::Type::BLOCK) {
        printer << breakable(0);
      }
      printer << startSubExpression << stmt.while_.body;
      return printer;
    case Statement::Type::LOOP:
      printer << "loop ";
      if (stmt.loop.name) {
        printer << *stmt.loop.name << space;
      }
      if (stmt.loop.body->getType() != Statement::Type::BLOCK) {
        printer << breakable(0);
      }
      printer << startSubExpression << stmt.loop.body;
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
        printer << space << stmt.return_;
      }
      printer << ";";
      break;
    case Statement::Type::BREAK:
      printer << "break";
      if (stmt.break_) {
        printer << space << *stmt.break_;
      }
      printer << ";";
      break;
    case Statement::Type::CONTINUE:
      printer << "continue";
      if (stmt.continue_) {
        printer << space << *stmt.continue_;
      }
      printer << ";";
      break;

    case Statement::Type::ASSERT:
      if (stmt.assert_.isFatal) {
        printer << "assert(";
      } else {
        printer << "expect(";
      }
      printer << stmt.assert_.condition;
      if (!stmt.assert_.debugInfo.empty()) {
        printer << ", " << stmt.assert_.debugInfo;
      }
      printer << ");";
      break;

    case Statement::Type::DEBUG:
      printer << "debug(" << stmt.debug << ");";
      break;

    case Statement::Type::BLANK:
      // Nothing.
      break;
  }

  if (stmt.comment) {
    auto formatScope = printer.addFormat(format::COMMENT);
    printer << space << "# " << stmt.comment->value;
  }

  printer << endStatement;

  return printer;
}

}  // namespace ast
}  // namespace modc
