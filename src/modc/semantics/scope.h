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

#ifndef KENTONSCODE_MODC_SEMANTICS_SCOPE_H_
#define KENTONSCODE_MODC_SEMANTICS_SCOPE_H_

#include "../macros.h"
#include "thing.h"

namespace modc {
namespace compiler {

class Entity;

// Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
// their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
class Scope {
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  // Get the local variable's current descriptor including transient constraints.
  DataDescriptor getVariableDescriptor(DataVariable* variable);
  PointerDescriptor getVariableDescriptor(PointerVariable* variable);

  void setVariableConstraints(DataVariable* variable, const DataConstraints& constraints);
  void setVariableConstraints(PointerVariable* variable, const PointerConstraints& ptrConstraints,
                              const DataConstraints& dataConstraints);

  // Add this entity to the pool of known entities.  This simply ensures that the object is not
  // deleted until the whole module is unloaded.
  void addEntity(OwnedPtr<Entity> entity);

  class NamedBinding {
  public:
    UNION_TYPE_BOILERPLATE(NamedBinding);

    enum class Kind {
      ENTITY,
      CONSTRAINED_TYPE
    };

    Kind getKind();

    union {
      Bound<Entity> entity;
      ConstrainedType type;
    };
  };

  // Bind / look up local variables.
  void bind(const string& name, NamedBinding&& value);
  Maybe<const NamedBinding&> lookupBinding(const string& name);

  // Declare variables.
  void declareDataVariable(DataVariable* variable,
                           DataConstraints&& impliedConstraints,
                           DataValue&& staticValue);
  void declarePointerVariable(PointerVariable* variable,
                              PointerConstraints&& impliedConstraints,
                              Pointer&& staticValue);

  // Get a pointer to the variable's value.  May be a partial value.  If the caller is simulating
  // changes to the value, it should modify the value directly.
  DataValue* getVariable(Variable* variable);

  // Sub-blocks.
  OwnedPtr<Scope> startBlock();
  OwnedPtr<Scope> startLoop();

  class Branch {
  public:
    ~Branch();

    OwnedPtr<Scope> addBranch();
    OwnedPtr<Scope> addDefaultBranch();
  };
  OwnedPtr<Branch> startBranch();

  // Other control flow.
  void addBreak(Maybe<string> loopName);
  void addContinue(Maybe<string> loopName);
  void addReturn();

private:
  Maybe<const Scope&> parent;

  OwnedPtrMap<string, Entity> bindings;
  map<string, OwnedPtrVector<Function> > functions;
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_SCOPE_H_ */
