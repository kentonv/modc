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

#ifndef KENTONSCODE_MODC_AST_H_
#define KENTONSCODE_MODC_AST_H_

#include <string>
#include <vector>

#include "base/OwnedPtr.h"
#include "Maybe.h"

namespace modc {
  namespace errors {
    class Error;
  }
}

namespace modc {
namespace ast {

using std::string;
using std::vector;
using ekam::OwnedPtr;
using ekam::Indirect;

enum class Style {
  VALUE,
  IMMUTABLE_REFERENCE,
  MUTABLE_REFERENCE,
  ENTANGLED_REFERENCE
};

enum class StyleAllowance {
  VALUE,
  IMMUTABLE_REFERENCE,
  MUTABLE_REFERENCE,
  MOVE
};

class Statement;
struct ParameterDeclaration;

#define VALUE_TYPE_PROTOTYPES(TYPENAME) \
  TYPENAME(TYPENAME&& other); \
  TYPENAME(const TYPENAME& other); \
  ~TYPENAME() noexcept; \
  TYPENAME& operator=(TYPENAME&& other); \
  TYPENAME& operator=(const TYPENAME& other); \
  bool operator==(const TYPENAME& other) const; \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define VALUE_TYPE1(TYPENAME, TYPE1, VAR1) \
  TYPENAME(TYPE1 VAR1): VAR1(move(VAR1)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define VALUE_TYPE2(TYPENAME, TYPE1, VAR1, TYPE2, VAR2) \
  TYPENAME(TYPE1 VAR1, TYPE2 VAR2) \
      : VAR1(move(VAR1)), VAR2(move(VAR2)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1 && VAR2 == other.VAR2; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define VALUE_TYPE3(TYPENAME, TYPE1, VAR1, TYPE2, VAR2, TYPE3, VAR3) \
  TYPENAME(TYPE1 VAR1, TYPE2 VAR2, TYPE3 VAR3) \
      : VAR1(move(VAR1)), VAR2(move(VAR2)), VAR3(move(VAR3)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1 && VAR2 == other.VAR2 && VAR3 == other.VAR3; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

class Expression {
public:
  VALUE_TYPE_PROTOTYPES(Expression);

  enum class Type {
    ERROR,

    VARIABLE,
    TUPLE,

    LITERAL_INT,
    LITERAL_DOUBLE,
    LITERAL_STRING,
    LITERAL_ARRAY,

    BINARY_OPERATOR,
    PREFIX_OPERATOR,
    POSTFIX_OPERATOR,
    TERNARY_OPERATOR,

    FUNCTION_CALL,
    SUBSCRIPT,
    MEMBER_ACCESS,

    IMPORT,

    LAMBDA
  };

  Type getType();

  struct BinaryOperator {
    string op;
    Indirect<Expression> left;
    Indirect<Expression> right;

    VALUE_TYPE3(BinaryOperator, string&&, op, Expression&&, left, Expression&&, right);
  };

  struct PrefixOperator {
    string op;
    Indirect<Expression> operand;

    VALUE_TYPE2(PrefixOperator, string&&, op, Expression&&, operand);
  };

  struct PostfixOperator {
    Indirect<Expression> operand;
    string op;

    VALUE_TYPE2(PostfixOperator, Expression&&, operand, string&&, op);
  };

  struct TernaryOperator {
    Indirect<Expression> condition;
    Indirect<Expression> trueClause;
    Indirect<Expression> falseClause;

    VALUE_TYPE3(TernaryOperator, Expression&&, condition, Expression&&, trueClause,
                Expression&&, falseClause);
  };

  struct FunctionCall {
    Indirect<Expression> function;
    struct Parameter {
      StyleAllowance styleAllowance;
      Indirect<Expression> expression;

      VALUE_TYPE2(Parameter, StyleAllowance, styleAllowance, Expression&&, expression);
    };
    vector<Parameter> parameters;

    VALUE_TYPE2(FunctionCall, Expression&&, function, vector<Parameter>&&, parameters);
  };

  struct Subscript {
    Indirect<Expression> container;
    Indirect<Expression> subscript;

    VALUE_TYPE2(Subscript, Expression&&, container, Expression&&, subscript);
  };

  struct MemberAccess {
    Indirect<Expression> object;
    string member;

    VALUE_TYPE2(MemberAccess, Expression&&, object, string&&, member);
  };

  struct Lambda {
    Style style;
    vector<ParameterDeclaration> parameters;
    Indirect<Expression> body;

    VALUE_TYPE3(Lambda, Style, style, vector<ParameterDeclaration>&&, parameters,
                Expression&&, body);
  };

  union {
    vector<errors::Error> error;

    string variable;
    vector<Expression> tuple;

    int literalInt;
    double literalDouble;
    string literalString;
    vector<Expression> literalArray;

    BinaryOperator binaryOperator;
    PrefixOperator prefixOperator;
    PostfixOperator postfixOperator;
    TernaryOperator ternaryOperator;

    FunctionCall functionCall;
    Subscript subscript;
    MemberAccess memberAccess;

    string import;

    Lambda lambda;
  };

  static Expression fromError(errors::Error&& error);
  static Expression fromError(vector<errors::Error>&& errors);

  static Expression fromVariable(string&& name);

  static Expression fromTuple(vector<Expression>&& elements);

  static Expression fromLiteralInt(int value);
  static Expression fromLiteralDouble(double value);
  static Expression fromLiteralString(string&& value);
  static Expression fromLiteralArray(vector<Expression>&& elements);

  static Expression fromBinaryOperator(string&& op, Expression&& left, Expression&& right);
  static Expression fromPrefixOperator(string&& op, Expression&& exp);
  static Expression fromPostfixOperator(Expression&& exp, string&& op);
  static Expression fromTernaryOperator(Expression&& condition, Expression&& trueClause,
                                        Expression&& falseClause);

  static Expression fromFunctionCall(Expression&& function,
                                     vector<FunctionCall::Parameter>&& parameters);
  static Expression fromSubscript(Expression&& container, Expression&& key);
  static Expression fromMemberAccess(Expression&& object, string&& member);

  static Expression fromImport(string&& moduleName);

  static Expression fromLambda(Style style, vector<ParameterDeclaration>&& parameters,
                               Expression&& body);

private:
  Type type;

  Expression(Type type): type(type) {}
};

// =======================================================================================

struct Declaration {
  Declaration();
  ~Declaration();

  enum class Kind {
    UNDECLARED,

    VARIABLE,
    ENVIRONMENT,

    FUNCTION,
    CONSTRUCTOR,
    DESTRUCTOR,
    CONVERSION,
    DEFAULT_CONVERSION,  // TODO:  Combine with CONVERSION somehow?

    CLASS,
    INTERFACE,
    ENUM
  };

  Kind kind;

  string name;  // empty for implicit constructor, destructor, conversion
  Style style;
  vector<ParameterDeclaration> parameters;

  Maybe<Expression> type;
  vector<Expression> supertypes;
  vector<Expression> subtypes;
  vector<Expression> annotations;
  string documentation;

  class Definition {
  public:
    VALUE_TYPE_PROTOTYPES(Definition);

    enum class Type {
      VALUE,
      BLOCK
    };

    Type getType() { return type; }

    union {
      Expression value;
      vector<Statement> block;
    };

  private:
    Type type;
  };

  Maybe<Definition> definition;

  bool operator==(const Declaration& other) const;
  bool operator!=(const Declaration& other) const { return !(*this == other); }
};

struct ParameterDeclaration {
  VALUE_TYPE_PROTOTYPES(ParameterDeclaration);

  enum class Type {
    CONSTANT,
    VARIABLE
  };

  union {
    Declaration variable;
    Expression constant;
  };
};

// =======================================================================================

class Statement {
public:
  VALUE_TYPE_PROTOTYPES(Statement);

  enum class Type {
    ERROR,

    EXPRESSION,
    BLOCK,

    DECLARATION,
    ASSIGNMENT,

    UNION,

    IF,
    ELSE,
    FOR,
    WHILE,
    LOOP,
    PARALLEL,

    RETURN,
    BREAK,
    CONTINUE,

    BLANK,
    COMMENT
  };

  Type getType();

  struct Assignment {
    Expression variable;
    Expression value;

    VALUE_TYPE2(Assignment, Expression&&, variable, Expression&&, value);
  };

  struct If {
    Expression condition;
    Indirect<Statement> body;

    VALUE_TYPE2(If, Expression&&, condition, Statement&&, body);
  };

  struct For {
    vector<Declaration> range;
    Indirect<Statement> body;

    VALUE_TYPE2(For, vector<Declaration>&&, range, Statement&&, body);
  };

  struct While {
    Expression condition;
    Indirect<Statement> body;

    VALUE_TYPE2(While, Expression&&, condition, Statement&&, body);
  };

  struct Loop {
    Maybe<string> name;
    Indirect<Statement> body;

    VALUE_TYPE2(Loop, Maybe<string>&&, name, Statement&&, body);
  };

  union {
    std::vector<errors::Error> error;

    Expression expression;
    vector<Statement> block;

    Declaration declaration;
    Assignment assignment;

    vector<Declaration> union_;

    If if_;
    Indirect<Statement> else_;
    For for_;
    While while_;
    Loop loop;
    vector<Statement> parallel;

    Expression return_;  // if not returning a value, expression will be empty tuple.
    Maybe<string> break_;
    Maybe<string> continue_;

    string comment;
  };

  static Statement fromError(errors::Error&& error);
  static Statement fromError(vector<errors::Error>&& errors);

  static Statement fromExpression(Expression&& expression);
  static Statement fromBlock(vector<Statement>&& block);

  static Statement fromDeclaration(Declaration&& declaration);
  static Statement fromAssignment(Expression&& variable, Expression&& value);

  static Statement fromUnion(vector<Declaration>&& declarations);

  static Statement fromIf(Expression&& condition, Statement&& body);
  static Statement fromFor(vector<Declaration>&& range, Statement&& body);
  static Statement fromWhile(Expression&& condition, Statement&& body);
  static Statement fromLoop(string&& name, Statement&& body);
  static Statement fromParallel(vector<Statement>&& statements);

  static Statement fromReturn(Expression&& value);
  static Statement fromBreak(Maybe<string>&& loopName);
  static Statement fromContinue(Maybe<string>&& loopName);

  static Statement fromBlank();
  static Statement fromComment(string&& text);

private:
  Type type;

  Statement(Type type): type(type) {}
};

}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_AST_H_ */
