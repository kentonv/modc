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
using ekam::OwnedPtr;
using ekam::Indirect;

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

}  // namespace statements
}  // namespace modc

#endif /* KENTONSCODE_MODC_STATEMENTS_H_ */
