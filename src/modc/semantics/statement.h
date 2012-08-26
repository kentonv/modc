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

#ifndef KENTONSCODE_MODC_SEMANTICS_STATEMENT_H_
#define KENTONSCODE_MODC_SEMANTICS_STATEMENT_H_

#include "../syntax/ast.h"
#include "value.h"
#include "expression.h"

namespace modc {
namespace compiler {

class DataVariable;
class PointerVariable;

class Statement {
public:
  UNION_TYPE_BOILERPLATE(Statement);

  enum class Kind {
    DATA_EXPRESSION,
    POINTER_EXPRESSION,
    BLOCK,

    DATA_DECLARATION,
    POINTER_DECLARATION,
    DATA_ASSIGNMENT,
    POINTER_ASSIGNMENT
  };

  struct DataDeclaration {
    DataVariable* variable;
    Maybe<DataExpression> initialValue;
  };

  struct PointerDeclaration {
    PointerVariable* variable;
    Maybe<PointerExpression> initialValue;
  };

  struct DataAssignment {
    DataVariable* variable;
    DataExpression value;
  };

  struct PointerAssignment {
    PointerVariable* variable;
    PointerExpression value;
  };

  union {
    DataExpression dataExpression;
    PointerExpression pointerExpression;
    vector<Statement> block;

    DataDeclaration dataDeclaration;
    PointerDeclaration pointerDeclaration;
    DataAssignment dataAssignment;
    PointerAssignment pointerAssignment;
  };
};


}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_STATEMENT_H_ */
