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
using ekam::OwnedPtr;
using ekam::Indirect;

enum class Kind {
  VARIABLE,
  FUNCTION,
  TYPE,
  ERROR
};

enum class Style {
  VALUE,
  IMMUTABLE_REFERENCE,
  MUTABLE_REFERENCE,
  ENTANGLED_REFERENCE
};

enum class StyleAllowance {
  VALUE,
  MUTABLE_REFERENCE
};

class Expression;

Expression errorExpression(errors::Error&& error);
Expression errorExpression(const errors::Error& error);
Expression errorExpression(std::vector<errors::Error>&& errors);
Expression errorExpression(const std::vector<errors::Error>& errors);

Expression variable(string&& name);
Expression variable(const string& name);

Expression literalArray(std::vector<Expression>&& elements);
Expression literalArray(const std::vector<Expression>& elements);

Expression literalInt(int value);
Expression literalDouble(double value);
Expression literalString(string&& value);
Expression literalString(const string& value);

Expression import(string&& moduleName);
Expression import(const string& moduleName);

Expression subscript(Expression&& container, Expression&& key);
Expression memberAccess(Expression&& object, const string& member);

Expression binaryOperator(string&& op, Expression&& left, Expression&& right);

Expression conditional(Expression&& condition, Expression&& trueClause, Expression&& falseClause);

class Expression {
public:
  Expression();
  Expression(Expression&& other);
  Expression(const Expression& other);
  ~Expression();

  Expression& operator=(Expression&& other);
  Expression& operator=(const Expression& other);

  bool operator==(const Expression& other) const;
  bool operator!=(const Expression& other) const { return !(*this == other); }

  enum class Type {
    ERROR,

    LITERAL_INT,
    LITERAL_DOUBLE,
    LITERAL_STRING,
    LITERAL_ARRAY,

    BINARY_OPERATOR,
    UNARY_OPERATOR,

    FUNCTION_CALL,
    SUBSCRIPT,
    MEMBER_ACCESS,
    CONDITIONAL,

    LAMBDA,
    IMPORT
  };

  Type getType();

  struct BinaryOperator {
    string op;
    Indirect<Expression> left;
    Indirect<Expression> right;

    bool operator==(const BinaryOperator& other) const {
      return op == other.op && left == other.left && right == other.right;
    }
  };

  struct UnaryOperator {
    string op;
    Indirect<Expression> value;

    bool operator==(const UnaryOperator& other) const {
      return op == other.op && value == other.value;
    }
  };

  struct FunctionCall {
    Indirect<Expression> function;
    struct Parameter {
      Indirect<Expression> expression;
      StyleAllowance styleAllowance;

      bool operator==(const Parameter& other) const {
        return expression == other.expression && styleAllowance == other.styleAllowance;
      }
    };
    std::vector<Parameter> parameters;

    bool operator==(const FunctionCall& other) const {
      return function == other.function && parameters == other.parameters;
    }
  };

  struct Subscript {
    Indirect<Expression> container;
    Indirect<Expression> subscript;

    bool operator==(const Subscript& other) const {
      return container == other.container && subscript == other.subscript;
    }
  };

  struct MemberAccess {
    Indirect<Expression> object;
    string member;

    bool operator==(const MemberAccess& other) const {
      return object == other.object && member == other.member;
    }
  };

  struct Conditional {
    Indirect<Expression> condition;
    Indirect<Expression> ifTrue;
    Indirect<Expression> ifFalse;

    bool operator==(const Conditional& other) const {
      return condition == other.condition && ifTrue == other.ifTrue && ifFalse == other.ifFalse;
    }
  };

  struct Lambda {
    struct Parameter {
      string name;
      Style style;
      Maybe<Indirect<Expression>> type;

      bool operator==(const Parameter& other) const {
        return name == other.name && style == other.style && type == other.type;
      }
    };
    std::vector<Parameter> parameters;
    Indirect<Expression> body;

    bool operator==(const Lambda& other) const {
      return parameters == other.parameters && body == other.body;
    }
  };

  union {
    std::vector<errors::Error> error;

    int literalInt;
    double literalDouble;
    string literalString;
    std::vector<Expression> literalArray;

    BinaryOperator binaryOperator;
    UnaryOperator unaryOperator;

    FunctionCall functionCall;
    Subscript subscript;
    MemberAccess memberAccess;
    Conditional conditional;

    Lambda lambda;
    string import;
  };

private:
  Type type;
};

class Statement {
public:
  Statement(Statement&& other);
  Statement(const Statement& other);
  ~Statement();

  Statement& operator=(Statement&& other);
  Statement& operator=(const Statement& other);

  bool operator==(const Statement& other) const;
  bool operator!=(const Statement& other) const { return !(*this == other); }

  enum class Type {
    ERROR,

    EXPRESSION,
    RETURN,
    BREAK,

    VARIABLE_DECLARATION,
    VARIABLE_DEFINITION,
    VARIABLE_ASSIGNMENT,

    METHOD_DECLARATION,
    METHOD_DEFINITION,

    SUB_BLOCK,
    IF,
    ELSE,
    FOR,
    WHILE,
    LOOP,
    PARALLEL

    // TODO:  Type definitions.
  };

  Type getType();

  union {
    std::vector<errors::Error> error;

    Expression expression;
    Expression return_;
    string break_;

    // TODO
  };

private:
  Type type;
};

}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_AST_H_ */
