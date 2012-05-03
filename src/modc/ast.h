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

class Expression {
public:
  Expression();
  Expression(Expression&& other);
  Expression(const Expression& other);
  ~Expression() noexcept;

  Expression& operator=(Expression&& other);
  Expression& operator=(const Expression& other);

  bool operator==(const Expression& other) const;
  bool operator!=(const Expression& other) const { return !(*this == other); }

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

    IMPORT
  };

  Type getType();

  struct BinaryOperator {
    string op;
    Indirect<Expression> left;
    Indirect<Expression> right;

    BinaryOperator(string&& op, Expression&& left, Expression&& right)
        : op(move(op)), left(move(left)), right(move(right)) {}

    bool operator==(const BinaryOperator& other) const {
      return op == other.op && left == other.left && right == other.right;
    }
  };

  struct PrefixOperator {
    string op;
    Indirect<Expression> operand;

    PrefixOperator(string&& op, Expression&& operand)
        : op(move(op)), operand(move(operand)) {}

    bool operator==(const PrefixOperator& other) const {
      return op == other.op && operand == other.operand;
    }
  };

  struct PostfixOperator {
    Indirect<Expression> operand;
    string op;

    PostfixOperator(Expression&& operand, string&& op)
        : operand(move(operand)), op(move(op)) {}

    bool operator==(const PostfixOperator& other) const {
      return operand == other.operand && op == other.op;
    }
  };

  struct TernaryOperator {
    Indirect<Expression> condition;
    Indirect<Expression> trueClause;
    Indirect<Expression> falseClause;

    TernaryOperator(Expression&& condition, Expression&& trueClause, Expression&& falseClause)
        : condition(move(condition)), trueClause(move(trueClause)),
          falseClause(move(falseClause)) {}

    bool operator==(const TernaryOperator& other) const {
      return condition == other.condition && trueClause == other.trueClause &&
          falseClause == other.falseClause;
    }
  };

  struct FunctionCall {
    Indirect<Expression> function;
    struct Parameter {
      StyleAllowance styleAllowance;
      Indirect<Expression> expression;

      Parameter(StyleAllowance styleAllowance, Expression&& expression)
          : styleAllowance(styleAllowance), expression(move(expression)) {}

      bool operator==(const Parameter& other) const {
        return expression == other.expression && styleAllowance == other.styleAllowance;
      }
    };
    vector<Parameter> parameters;

    FunctionCall(Expression&& function, vector<Parameter>&& parameters)
        : function(function), parameters(parameters) {}

    bool operator==(const FunctionCall& other) const {
      return function == other.function && parameters == other.parameters;
    }
  };

  struct Subscript {
    Indirect<Expression> container;
    Indirect<Expression> subscript;

    Subscript(Expression&& container, Expression&& subscript)
        : container(move(container)), subscript(move(subscript)) {}

    bool operator==(const Subscript& other) const {
      return container == other.container && subscript == other.subscript;
    }
  };

  struct MemberAccess {
    Indirect<Expression> object;
    string member;

    MemberAccess(Expression&& object, string&& member)
        : object(move(object)), member(move(member)) {}

    bool operator==(const MemberAccess& other) const {
      return object == other.object && member == other.member;
    }
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
  };

  static Expression fromError(errors::Error&& error);
  static Expression fromError(vector<errors::Error>&& errors);

  static Expression fromVariable(string&& name);

  static Expression fromTuple(vector<Expression>&& elements);

  static Expression fromLiteralInt(int value);
  static Expression fromLiteralDouble(double value);
  static Expression fromLiteralString(string&& value);
  static Expression fromLiteralArray(vector<Expression>&& elements);

  static Expression fromImport(string&& moduleName);

  static Expression fromSubscript(Expression&& container, Expression&& key);
  static Expression fromMemberAccess(Expression&& object, string&& member);
  static Expression fromFunctionCall(Expression&& function,
                                     vector<FunctionCall::Parameter>&& parameters);

  static Expression fromBinaryOperator(string&& op, Expression&& left, Expression&& right);
  static Expression fromPrefixOperator(string&& op, Expression&& exp);
  static Expression fromPostfixOperator(Expression&& exp, string&& op);
  static Expression fromTernaryOperator(Expression&& condition, Expression&& trueClause,
                                        Expression&& falseClause);

private:
  Type type;

  Expression(Type type): type(type) {}
};

class Statement;
struct ParameterDeclaration;

struct Declaration {
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
    Definition();
    Definition(const Definition& other);
    Definition(Definition&& other);
    ~Definition() noexcept;

    enum class Type {
      NONE,
      VALUE,
      BLOCK
    };

    Type getType() { return type; }

    union {
      Expression value;
      vector<Statement> body;
    };

  private:
    Type type;
  };
};

struct ParameterDeclaration {
  enum class Type {
    CONSTANT,
    VARIABLE
  };

  union {
    Declaration variable;
    Expression constant;
  };
};

class Statement {
public:
  Statement(Statement&& other);
  Statement(const Statement& other);
  ~Statement() noexcept;

  Statement& operator=(Statement&& other);
  Statement& operator=(const Statement& other);

  bool operator==(const Statement& other) const;
  bool operator!=(const Statement& other) const { return !(*this == other); }

  enum class Type {
    ERROR,

    EXPRESSION,
    SUB_BLOCK,

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
  };

  struct If {
    Expression condition;
    Indirect<Statement> body;
  };

  struct For {
    vector<Declaration> range;
    Indirect<Statement> body;
  };

  struct While {
    Expression condition;
    Indirect<Statement> body;
  };

  struct Loop {
    Maybe<string> name;
    Indirect<Statement> body;
  };

  union {
    std::vector<errors::Error> error;

    Expression expression;
    vector<Statement> subBlock;

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
    string break_;
    string continue_;

    string comment;
  };

private:
  Type type;
};

}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_AST_H_ */
