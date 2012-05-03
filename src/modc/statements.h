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

#ifndef KENTONSCODE_MODC_STATEMENTS_H_
#define KENTONSCODE_MODC_STATEMENTS_H_

#include <string>
#include <vector>

#include "base/OwnedPtr.h"
#include "Maybe.h"
#include "expressions.h"

namespace modc {
  namespace errors {
    class Error;
  }
}

namespace modc {
namespace statements {

using expressions::Expression;
using expressions::Style;

using std::string;
using std::vector;
using ekam::OwnedPtr;
using ekam::Indirect;

class Statement;
struct ParameterDeclaration;

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

struct Declaration {
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

}  // namespace statements
}  // namespace modc

#endif /* KENTONSCODE_MODC_STATEMENTS_H_ */
