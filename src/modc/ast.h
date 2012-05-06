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
#include "errors.h"

namespace modc {
  class CodePrinter;
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
using errors::Location;
using errors::Located;

enum class Style {
  VALUE,
  IMMUTABLE_REFERENCE,
  MUTABLE_REFERENCE,
  ENTANGLED_REFERENCE,
  HEAP_VALUE,
  CONSTANT
};

CodePrinter& operator<<(CodePrinter& os, Style style);

enum class StyleAllowance {
  VALUE,
  IMMUTABLE_REFERENCE,
  MUTABLE_REFERENCE,
  MOVE
};

CodePrinter& operator<<(CodePrinter& os, StyleAllowance style);

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

  Type getType() const { return type; }

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

      static Parameter fromError(Location location, vector<errors::Error> errors) {
        return Parameter(StyleAllowance::VALUE, Expression::fromError(location, move(errors)));
      }
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

  Location location;

  static Expression fromError(Location location, errors::Error&& error);
  static Expression fromError(Location location, vector<errors::Error>&& errors);

  static Expression fromVariable(Location location, string&& name);

  static Expression fromTuple(Location location, vector<Expression>&& elements);

  static Expression fromLiteralInt(Location location, int value);
  static Expression fromLiteralDouble(Location location, double value);
  static Expression fromLiteralString(Location location, string&& value);
  static Expression fromLiteralArray(Location location, vector<Expression>&& elements);

  static Expression fromBinaryOperator(Location location, string&& op,
                                       Expression&& left, Expression&& right);
  static Expression fromPrefixOperator(Location location, string&& op, Expression&& exp);
  static Expression fromPostfixOperator(Location location, Expression&& exp, string&& op);
  static Expression fromTernaryOperator(Location location, Expression&& condition,
                                        Expression&& trueClause, Expression&& falseClause);

  static Expression fromFunctionCall(Location location, Expression&& function,
                                     vector<FunctionCall::Parameter>&& parameters);
  static Expression fromSubscript(Location location, Expression&& container, Expression&& key);
  static Expression fromMemberAccess(Location location, Expression&& object, string&& member);

  static Expression fromImport(Location location, string&& moduleName);

  static Expression fromLambda(Location location, Style style,
                               vector<ParameterDeclaration>&& parameters, Expression&& body);

  // minPriority = Minimum priority level at which parenthesization is not needed.
  void print(CodePrinter& printer, int minPriority = -1) const;

private:
  Type type;

  Expression(Location location, Type type): location(location), type(type) {}
};

inline CodePrinter& operator<<(CodePrinter& printer, const Expression& expression) {
  expression.print(printer);
  return printer;
}

// =======================================================================================

struct Annotation {
  enum Relationship {
    SUBCLASS_OF,
    SUPERCLASS_OF,
    ANNOTATION
  };

  Relationship relationship;
  Maybe<Expression> param;

  VALUE_TYPE2(Annotation, Relationship, relationship, Maybe<Expression>&&, param);
};

struct Declaration {
  enum class Kind {
    ERROR,  // definition->expression contains error, rest of structure is undefined.

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

  Declaration(Location location, Kind kind);
  ~Declaration();

  Kind kind;
  Style thisStyle;
  Style style;

  Maybe<Located<string>> name;  // omitted for implicit constructor, destructor, conversion.
  Maybe<vector<ParameterDeclaration>> parameters;
  Maybe<Expression> type;
  vector<Annotation> annotations;
  Maybe<Located<string>> documentation;

  class Definition {
  public:
    VALUE_TYPE_PROTOTYPES(Definition);

    enum class Type {
      EXPRESSION,
      BLOCK
    };

    Type getType() const { return type; }

    union {
      Expression expression;
      vector<Statement> block;
    };

    static Definition fromExpression(Expression&& expression);
    static Definition fromBlock(vector<Statement>&& statements);

  private:
    Type type;

    Definition(Type type): type(type) {}
  };

  Maybe<Definition> definition;

  // Comment appearing after the end of the statement, on the same line.
  Maybe<Located<string>> comment;

  Location location;

  bool operator==(const Declaration& other) const;
  bool operator!=(const Declaration& other) const { return !(*this == other); }

  static Declaration fromError(Location location, vector<errors::Error>&& errors);

  void print(CodePrinter& printer, bool asStatement) const;
};

inline CodePrinter& operator<<(CodePrinter& os, const Declaration& declaration) {
  declaration.print(os, false);
  return os;
}

class ParameterDeclaration {
public:
  VALUE_TYPE_PROTOTYPES(ParameterDeclaration);

  enum class Type {
    CONSTANT,
    VARIABLE
  };

  Type getType() const { return type; }

  union {
    Expression constant;
    Declaration variable;
  };

  static ParameterDeclaration fromError(Location location, vector<errors::Error>&& errors);
  static ParameterDeclaration fromConstant(Location location, Expression&& expression);
  static ParameterDeclaration fromVariable(Location location, Declaration&& declaration);

private:
  Type type;

  ParameterDeclaration(Type type): type(type) {}
};

inline CodePrinter& operator<<(CodePrinter& os, const ParameterDeclaration& parameter);

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

    BLANK
  };

  Type getType() const { return type; }

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
  };

  // Comment appearing after the end of the statement, on the same line.
  // For declarations, the comment should go in declaration.comment instead of here.
  Maybe<Located<string>> comment;

  Location location;

  static Statement fromError(Location location, errors::Error&& error);
  static Statement fromError(Location location, vector<errors::Error>&& errors);

  static Statement fromExpression(Location location, Expression&& expression);
  static Statement fromBlock(Location location, vector<Statement>&& block);

  static Statement fromDeclaration(Location location, Declaration&& declaration);
  static Statement fromAssignment(Location location, Expression&& variable, Expression&& value);

  static Statement fromUnion(Location location, vector<Declaration>&& declarations);

  static Statement fromIf(Location location, Expression&& condition, Statement&& body);
  static Statement fromElse(Location location, Statement&& body);
  static Statement fromFor(Location location, vector<Declaration>&& range, Statement&& body);
  static Statement fromWhile(Location location, Expression&& condition, Statement&& body);
  static Statement fromLoop(Location location, Maybe<string>&& name, Statement&& body);
  static Statement fromParallel(Location location, vector<Statement>&& statements);

  static Statement fromReturn(Location location, Expression&& value);
  static Statement fromBreak(Location location, Maybe<string>&& loopName);
  static Statement fromContinue(Location location, Maybe<string>&& loopName);

  static Statement fromBlank(Location location);

private:
  Type type;

  Statement(Location location, Type type): location(location), type(type) {}
};

CodePrinter& operator<<(CodePrinter& printer, const Statement& statement);

}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_AST_H_ */
