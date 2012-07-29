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

class ThingPort {
public:
  ThingPort(const Context& localContext, const Context& foreignContext);

  const Context& getContext();

  Thing exportThing(Thing&& thing);
  Thing importThing(Thing&& thing);

  Context exportContext(const Context& input);
};

class Scope {
  // Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
  // their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  const Context& getContext();

  const Bound<Entity>& getEntity();

  // If input.params is filled in, returns it.  Otherwise, finds the context corresponding to
  // input.scope and returns that.  In other words, this effectively fills in input.params if it
  // isn't filled in already.
  const Context& fillContext(const Context& input);

  // Creates a ThingPort that can be used to port Things between this Scope's context and the
  // target context.  Note that the port is typically passed in whole into an Entity which
  // itself represents the target context.  Therefore, the port's "import" methods port *to*
  // targetContext and "export" ports *from* targetContext.  (This is why it is "makePortFor"
  // instead of just "makePort".)
  //
  // The returned port references targetContext.
  ThingPort makePortFor(const Context& targetContext);

  // Get the local variable's current descriptor including transient constraints.
  DataDescriptor getVariableDescriptor(DataVariable* variable);
  PointerDescriptor getVariableDescriptor(PointerVariable* variable);

  void setVariableConstraints(DataVariable* variable, const DataConstraints& constraints);
  void setVariableConstraints(PointerVariable* variable, const PointerConstraints& ptrConstraints,
                              const DataConstraints& dataConstraints);

  // Get a pointer to the variable's value.  May be a partial value.  If the caller is simulating
  // changes to the value, it should modify the value directly.
  DataValue* getVariable(Variable* variable);




  // For EXPRESSION

  Thing lookupBuiltinType(BuiltinType::Builtin type);

  Maybe<Thing> lookupBinding(const string& name);
  // Note that this may instantiate a template.
  // Note also that this returns a proxy specific to the current scope.

  // For DECLARATION

  Variable& declareVariable(const string& name, Thing&& value);

  // Code will be compiled on-demand.
  void declareClass(const string& name, TypeDescriptor&& descirptor,
                    const vector<ast::Statement>& code);

  // Code will be compiled on-demand.
  void declareFunction(const string& name, FunctionDescriptor&& descirptor,
                       const vector<ast::Statement>& code);

  // Sub-blocks.
  OwnedPtr<Scope> startBlock();
  OwnedPtr<Scope> startLoop();
  OwnedPtr<Scope> startParallel();

  class IfElse {
  public:
    ~IfElse();

    OwnedPtr<Scope> addIf(Thing&& condition);
    OwnedPtr<Scope> addElse();
  };
  OwnedPtr<IfElse> startIfElse();

  // Other control flow.
  void addBreak(Maybe<string> loopName);
  void addContinue(Maybe<string> loopName);
  void addReturn(Thing&& value);

private:
  const Scope& parent;

  OwnedPtrMap<string, Entity> bindings;
  map<string, OwnedPtrVector<Function> > functions;
};

vector<CxxStatement> compileImperative(Scope& scope, const vector<ast::Statement>& statements);

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_SCOPE_H_ */
