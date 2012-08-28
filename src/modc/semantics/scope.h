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

#include <string>
#include <map>
#include "../macros.h"
#include "thing.h"

namespace modc {
namespace compiler {

class Entity;
class Scope;

using std::string;
using std::map;

class ErrorLocation {
public:
  explicit ErrorLocation(ast::Expression& expression);

  template <typename... Parts>
  void error(Parts&&... parts);
  void error(errors::Error&& error);
};

class VariableUsageSet {
public:
  enum class Style {
    IDENTITY,
    MUTABLE,
    IMMUTABLE,
    MEMBER_MUTABLE,
    MEMBER_IMMUTABLE,
    ASSIGNMENT
  };

  void addUsage(const PointerConstraints& pointer, Exclusivity exclusivity, ErrorLocation location);

  void addUsage(const LocalVariablePath& variable, Exclusivity exclusivity, ErrorLocation location);

  // TODO:  Does this need context?  We do need to distinguish between the same member of
  //   different parents.  Maybe it just needs a path?
  void addSequential(Variable* variable, Style style, ErrorLocation location);

  // Given a list of VariableUsageSets representing variable usage in a series of parallel
  // operations, check that the usages do not conflict, and then merge them all into this set.
  void merge(vector<VariableUsageSet>&& parallelUsages);

private:
  struct Usage {
    Style style;
    ErrorLocation location;

    Usage(Style style, ErrorLocation location): style(style), location(location) {}
  };

  map<Variable*, Usage> variablesUsed;
};

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

    Loop(Scope& parent);
    ~Loop();

    Scope& innerScope();
  };

  // -------------------------------------------------------------------------------------
  // Leaving the scope

  ~Scope();

  void addBreak(Loop& loop);
  void addContinue(Loop& loop);
  void addReturn();

  // Is the current line reachable?  This becomes false immediately after break/continue/return.
  bool isReachable();

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
  // Variables

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

  // -------------------------------------------------------------------------------------
  // Bindings

  map<string, NamedBinding> bindings;

  // -------------------------------------------------------------------------------------
  // Variable states

  template <typename Value, typename Constraints>
  struct VariableState {
    // This variable's state in the parent scope.  Null if declared in the current scope.
    Maybe<const VariableState&> parentState;

    // Bits indicating which parts of the state have changed from parentState.  If parentState is
    // null then these should all be true.
    bool isInitializedChanged: 1;
    bool lastAccessChanged: 1;
    bool lastWriteChanged: 1;
    bool lastVerifiedChanged: 1;
    bool staticValueChanged: 1;
    bool constraintsChanged: 1;

    // The changed parts.  Each of these is only valid if the corresponding bit is true.
    bool isInitialized: 1;
    int lastAccess;
    int lastWrite;
    int lastVerified;
    Maybe<Value> staticValue;
    Maybe<Constraints> constraints;

    VariableState():
        parentState(nullptr),
        isInitializedChanged(true),
        lastAccessChanged(true),
        lastWriteChanged(true),
        lastVerifiedChanged(true),
        staticValueChanged(true),
        constraintsChanged(true),
        isInitialized(false),
        lastAccess(0),
        lastWrite(0),
        lastVerified(0),
        staticValue(nullptr),
        constraints(nullptr) {}
    explicit VariableState(const VariableState& parentState):
        parentState(move(parentState)),
        isInitializedChanged(false),
        lastAccessChanged(false),
        lastWriteChanged(false),
        lastVerifiedChanged(false),
        staticValueChanged(false),
        constraintsChanged(false),
        isInitialized(false),
        lastAccess(0),
        lastWrite(0),
        lastVerified(0),
        staticValue(nullptr),
        constraints(nullptr) {}
  };

  typedef VariableState<DataValue, DataConstraints> DataVariableState;
  typedef VariableState<Pointer, PointerConstraints> PointerVariableState;

  struct State {
    // Increments whenever something changes.
    int timestamp;

    // Map variable paths to changes that have been made vs. parent scope.  Note that a map must
    // not contain two keys where one is a prefix of the other -- the prefix should have superseded
    // the more-specific entry.
    map<LocalVariablePath, DataVariableState> dataVariables;
    map<LocalVariablePath, PointerVariableState> pointerVariables;

    // Given a state representing a diff from this one, integrate the changes from the diff into
    // this state.
    void integrate(State&& diff);

    // Given a state derived from the same base state as this one, representing a different
    // branch of the code, combine it into this state to form a merged state representing the
    // state after one of the two branches has completed.
    void mergeBranch(State&& other);
  };

  // If null, current location is unreachable.
  Maybe<State> currentState;

  // States reached at "break" statements within this scope which exit the scope.
  std::multimap<Loop*, State> breakStates;

  // States reached at "continue" statements within this scope which exit the scope.
  std::multimap<Loop*, State> continueStates;

  // States reached at "return" statements within this scope.
  std::vector<State> returnStates;
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_SCOPE_H_ */
