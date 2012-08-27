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
class ErrorLocation;
class VariableUsageSet;
class Scope;

// Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
// their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
class Scope {
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  // -------------------------------------------------------------------------------------
  // Creating/entering the scope

  // Create a top-level (function body) scope.
  Scope();

  // Create a scope representing a block that is always executed (i.e. statements wrapped in
  // braces with no condition / loop).
  explicit Scope(Scope& parent);

  // Create a fork with some set of branches, each being a sub-scope.  Exactly one branch will be
  // taken for any particular execution.  If a "default" or "else" clause is not explicitly given
  // in the code, you must still create a(n empty) Scope for it.
  class Fork {
  public:
    RESOURCE_TYPE_BOILERPLATE(Fork);

    Fork(Scope& parent);
    ~Fork();
  };
  explicit Scope(Fork& fork);

  // Create a loop.
  class Loop {
  public:
    RESOURCE_TYPE_BOILERPLATE(Loop);

    Loop(Scope& parent, const string& name);
    ~Loop();

    Scope& innerScope();
  };

  // -------------------------------------------------------------------------------------
  // Leaving the scope

  ~Scope();

  void addBreak(Maybe<string> loopName);
  void addContinue(Maybe<string> loopName);
  void addReturn();

  bool canControlGetHere();

  // -------------------------------------------------------------------------------------
  // Bindings

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

  // -------------------------------------------------------------------------------------
  // Varibales

  // Get the local variable's current descriptor including transient constraints.
  DataDescriptor getVariableDescriptor(DataVariable* variable);
  PointerDescriptor getVariableDescriptor(PointerVariable* variable);

  // Declare variables.  This creates a slot for the variable in this scope, but the slot is not
  // yet initialized.
  void declareVariable(DataVariable* variable, ErrorLocation location);
  void declareVariable(PointerVariable* variable, ErrorLocation location);

  // Assigns a variable to a value, additionally marking it initialized if it is not already.
  // The value may, of course, be UNKNOWN if it is not known at compile time.  If constraints or
  // values are not known due to already-reported errors when trying to compute them, then the
  // Maybes should be null.  If the variable has declared constraints, the callee will verify that
  // the provided constraints match; otherwise the provided constraints become the variable's
  // constraints.
  void assignVariable(DataVariable* variable,
                      Maybe<DataConstraints>&& derivedConstraints,
                      Maybe<DataValue>&& staticValue,
                      ErrorLocation location);
  void assignVariable(PointerVariable* variable,
                      Maybe<PointerConstraints>&& derivedConstraints,
                      Maybe<Pointer>&& staticValue,
                      ErrorLocation location);

  // Assert that the variable now has the given constraints, e.g. because the code being compiled
  // dynamically checked this.
  void assertConstraints(LocalVariablePath&& path, Maybe<DataConstraints>&& constraints);
  void assertConstraints(LocalVariablePath&& path, Maybe<PointerConstraints>&& constraints);

  // Read the variable's current value.
  Maybe<const DataValue&> readDataVariable(LocalVariablePath&& path,
                                           VariableUsageSet& variablesUsed,
                                           ErrorLocation location);
  Maybe<const Pointer&> readPointerVariable(LocalVariablePath&& path,
                                            VariableUsageSet& variablesUsed,
                                            ErrorLocation location);

  // Obtain a pointer to the given variable.  Returns null if there was an error.  If
  // DataConstraints are provided, the variable's constraints are updated to match -- use this
  // e.g. when obtaining an exclusive pointer that is expected to be modified under some broader
  // constraints than what the variable already has.
  Maybe<Pointer> getPointerTo(LocalVariablePath&& path, ErrorLocation location);
  Maybe<Pointer> getPointerTo(LocalVariablePath&& path, Maybe<DataConstraints>&& newConstraints,
                              ErrorLocation location);

private:
  enum Kind {
    TOP,
    BLOCK,
    FORK,
    LOOP
  };

  union {
    Scope* parent;  // for BLOCK
    Loop* loop;
    Fork* fork;
  };

  enum VariableState {
    UNINITIALIZED,
    INITIALIZED,
    DESTROYED    // e.g. moved away
  };

  struct State {
    // Map variable paths to changes that have been made vs. parent scope.  Note that a map must
    // not contain two keys where one is a prefix of the other -- the prefix should have superseded
    // the more-specific entry.
    map<LocalVariablePath, VariableState> variableStates;
    map<LocalVariablePath, Maybe<DataValue>> dataValues;
    map<LocalVariablePath, Maybe<Pointer>> pointerValues;
    map<LocalVariablePath, Maybe<DataConstraints>> dataConstraints;
    map<LocalVariablePath, Maybe<PointerConstraints>> pointerConstraints;

    // Given a state representing a diff from this one, integrate the changes from the diff into
    // this state.
    void integrate(State&& diff);

    // Given a state derived from the same base state as this one -- i.e. representing a different
    // branch of the code -- combine it into this state to form a merged state representing the
    // state after one of the two branches has completed.
    void merge(State&& other);
  };

  State current;
  // break states
  // continue states
  // return states

  // Bindings
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_SCOPE_H_ */
