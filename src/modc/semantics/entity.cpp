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
#include "port.h"

namespace modc {
namespace compiler {

// =======================================================================================

class DataVariableImpl: public DataVariable {
public:
  DataVariableImpl(EntityName&& name, Entity* parent, Maybe<size_t> contextPosition,
                   ConstrainedType&& type)
      : name(move(name)), parent(parent), contextPosition(contextPosition), type(move(type)) {}

  // -------------------------------------------------------------------------------------
  // implements Entity

  const EntityName& getName() { return name; }
  Entity* getParent() { return parent; }

  // -------------------------------------------------------------------------------------
  // implements Variable

  Maybe<size_t> getContextPosition() {
    return contextPosition;
  }

  ConstrainedType getType(Port& port) {
    return port.import(type);
  }

private:
  EntityName const name;
  Entity* const parent;
  Maybe<size_t> const contextPosition;

  // Context is relative to the variable's scope.
  ConstrainedType const type;
};

class PointerVariableImpl: public PointerVariable {
public:
  PointerVariableImpl(EntityName&& name, Entity* parent, Maybe<size_t> contextPosition,
                      ConstrainedType&& type, Exclusivity exclusivity,
                      Maybe<PointerConstraints>&& pointerConstraints)
      : name(move(name)), parent(parent), contextPosition(contextPosition), type(move(type)),
        exclusivity(exclusivity), pointerConstraints(move(pointerConstraints)) {}

  // -------------------------------------------------------------------------------------
  // implements Entity

  const EntityName& getName() { return name; }
  Entity* getParent() { return parent; }

  // -------------------------------------------------------------------------------------
  // implements Variable

  Maybe<size_t> getContextPosition() {
    return contextPosition;
  }

  ConstrainedType getType(Port& port) {
    return port.import(type);
  }

  // -------------------------------------------------------------------------------------
  // implements PointerVaribale

  Exclusivity getExclusivity() {
    return exclusivity;
  }

  // Get the declared pointer constraints for the variable, if any.
  Maybe<PointerConstraints> getPointerConstraints(Port& port) {
    return pointerConstraints.transform(
        [&](const PointerConstraints& orig) { return port.import(orig); });
  }

private:
  EntityName const name;
  Entity* const parent;
  Maybe<size_t> const contextPosition;

  // Context is relative to the variable's scope.
  ConstrainedType const type;
  Exclusivity const exclusivity;
  Maybe<PointerConstraints> const pointerConstraints;
};

}  // namespace compiler
}  // namespace modc
