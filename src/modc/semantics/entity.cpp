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

#include "entity.h"
#include "compiler.h"

namespace modc {
namespace compiler {

class DataFieldImpl: public DataVariable {
public:
  // -------------------------------------------------------------------------------------
  // implements Entity

  const EntityName& getName() { return name; }
  Entity* getParent() { return containingType; }

  // -------------------------------------------------------------------------------------
  // implements Variable

  ConstrainedType getType(const Context& context) {
    return type.port(context);
  }

  Maybe<ConstrainedType> getType(const Context& containingTypeContext,
                                 const DataValue& containingObject) {
    return type.port(containingTypeContext, containingObject);
  }

private:
  EntityName name;
  Class* containingType;

  // Context is relative to the variable's scope.
  ConstrainedType type;
};

}  // namespace compiler
}  // namespace modc
