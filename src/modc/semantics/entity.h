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

#ifndef KENTONSCODE_MODC_SEMANTICS_ENTITY_H_
#define KENTONSCODE_MODC_SEMANTICS_ENTITY_H_

#include "thing.h"
#include "compiler.h"
#include "../errors.h"

namespace modc {
namespace compiler {

using errors::Location;

class Entity {
public:
  virtual ~Entity();

  virtual const EntityName& getName() = 0;
  virtual Entity& getParent() = 0;

  virtual const vector<Thing>& getAnnotations(Context&& context) = 0;

  // TODO:  style + style constraints
  // TODO:  visibility

  // Read the entity.  If not known at compile time, returns a Thing of kind
  // DYNAMIC_{REFERENCE,VALUE} with code that simply reads the variable.
  // Recursively dereferences outer objects in the context.
  virtual Thing dereference(Context&& context, VariableUsageSet& variablesUsed,
                            Location location, VariableUsageSet::Style style) = 0;
};

// =======================================================================================

class Variable: public Entity {
public:
  virtual ~Variable();

  // Get the declared type for the variable.  Note that this only contains constraints that
  // were explicitly declared in the code along with the variable's declaration.  Additional
  // knowledge may be available within the context where the variable is accessed, e.g. even if
  // the variable does not declare what it might point at, if its containing object does, then
  // these constraints can be derived.
  //
  // For pointer variables, getType() returns the type of the pointed-to value.
  Thing::ConstrainedType getType(const Context& context);

  // Like getType(Context), but accepts a Context which is lacking the object instance of which
  // this variable is a member.  So, the context is for the variable's containing type, not the
  // variable itself.  Also provided is a Value which represents what is known about the containing
  // object.  This may be a partial Value, i.e. it can contain UNKNOWNs.  If the Value is sufficient
  // to construct the fully-described type of the variable, then it is returned.  Otherwise, null
  // is returned.
  //
  // In most cases, the descriptor for a member does not depend on the instance, so this will
  // succeed even if thisValue is entirely UNKNOWN.  The instance only matters if the member's
  // type is dependent on the instance or other members, e.g. because it is a parameterized type
  // or an inner type.
  //
  // When this returns null, the caller will need to bind the parent object to a local variable
  // so that it can construct a Context that contains it.
  Maybe<Thing::ConstrainedType> getType(const Context& containingTypeContext,
                                        const DataValue& containingObject);
};

class DataVariable: public Variable {
public:
  virtual ~DataVariable();
};

class PointerVariable: public Variable {
public:
  virtual ~PointerVariable();

  Exclusivity getExclusivity();

  // Get constraints independently of any binding for the object of which this variable is a member.
  // Instead, the object's type's context is provided.
  Maybe<UnboundPointerConstraints> getUnboundConstraints(const Context& containingTypeContext);
};

// =======================================================================================

class Function: public Entity {
public:
  const vector<Variable*>& getParameters();

  // TODO:  Implicit parameters, environment.
  //   Both may depend on compiling the function body.

  Maybe<DescribedRvalue> call(
      Compiler& compiler, DescribedRvalue&& this_, vector<DescribedRvalue>&& parameters,
      VariableUsageSet& variablesUsed, ErrorLocation location);

  Rvalue call(vector<Rvalue>&& typeContext, Rvalue&& this_, vector<Rvalue>&& parameters);
};

class Overload: public Entity {
public:
  // Resolves the overload for the given parameters and returns a Thing representing the result.
  // In the case where the overload is resolved to a function, the returned Thing represents the
  // result of calling that function.
  //
  // resolve() may have the effect of lazily instantiating templates.
  Maybe<Thing> resolve(Compiler& compiler, Context&& context, Tuple&& parameters,
                       ErrorLocation location);
  Maybe<Thing> resolve(Compiler& compiler, DescribedRvalue&& object,
                       Tuple&& parameters, ErrorLocation location);
};

class BinaryOperator: public Entity {
public:
  enum MatchSpecificity {
    // Order matters.
    NONE,
    GENERALIZED,
    EXACT
  };

  MatchSpecificity match(Compiler& compiler, const DescribedRvalue& this_, const Thing& other);

  Maybe<DescribedRvalue> call(Compiler& compiler, DescribedRvalue&& this_, Thing&& other,
                              VariableUsageSet& variablesUsed, ErrorLocation location);

  Rvalue call(vector<Rvalue>&& typeContext, Rvalue&& this_, Rvalue&& other);
};

class UnaryOperator: public Entity {
public:
  Maybe<DescribedRvalue> call(Compiler& compiler, DescribedRvalue&& this_,
                              VariableUsageSet& variablesUsed, ErrorLocation location);

  Rvalue call(vector<Rvalue>&& typeContext, Rvalue&& this_);
};

// =======================================================================================

class Type: public Entity {
public:
  Thing getMemberOfType(Compiler& compiler, Thing::ConstrainedType&& self,
                        const string& memberName);

  Thing getMemberOfInstance(Compiler& compiler, DescribedRvalue&& parent,
                            const string& memberName, ErrorLocation location);

  // Note that constructors, unlike all other members, have context matching the type's context.
  // All other members have a context containing one element:  the instance.
  Overload* getImplicitConstructor();
  Maybe<Overload&> lookupConstructor(const string& name);

  // The port is for the type.
  // TODO: This is wrong.  The port should include the instance.
  Maybe<UnaryOperator&> lookupConversion(ThingPort& port, const Bound<Type>& to);
  Maybe<UnaryOperator&> getDefaultConversion(ThingPort& port);

  Maybe<UnaryOperator&> lookupPrefixOperator(ast::PrefixOperator op);
  Maybe<UnaryOperator&> lookupPostfixOperator(ast::PostfixOperator op);
  Maybe<BinaryOperator&> lookupLeftBinaryOperator(ast::BinaryOperator op);
  Maybe<BinaryOperator&> lookupRightBinaryOperator(ast::BinaryOperator op);

  // NOTE:  interfaceType could be Bound<Interface>...  but this probably forces a copy that
  //    wouldn't otherwise be needed.
  Maybe<ImplementedInterface&> findImplementedInterface(
      ThingPort& port, const Bound<Type>& interfaceType);

  // Returns whether this type is a subclass of the given type.  This is to be used for the purpose
  // of choosing an overload.  I.e. say for a one-argument function call there are two matching
  // overloads, one taking type Foo and one taking type Bar.  If Foo is more specific than Bar, then
  // the overload taking Foo should be chosen.  If Bar is more specific than Foo, that overload
  // should be chosen.  If neither type is more specific than the other then the situation is
  // ambiguous and therefore an error.
  bool isMoreSpecificThan(ThingPort& port, const Bound<Type>& otherType);

  // Does this type contain any aliases which are not annotated with their potential targets?  This
  // transitively includes members of by-value and heap members, but NOT members of alias members.
  // TODO:  In theory we could be returning the set of types of these unannotated aliases, which
  //   could perhaps allow us to rule out alias annotations on the type itself which can't possibly
  //   correspond to any of these.  But that seems like it could make code too dependent on the
  //   implementation details of the classes it uses.
  bool hasUnannotatedAliases();
};

class BuiltinType: public Type {
public:
  enum class Builtin {
    BOOLEAN,
    INTEGER,
    DOUBLE
  };

  Builtin getType();

  static BuiltinType* get(Builtin builtin);
};

class Class: public Type {
public:
  // TODO:  Should some members of Type really be defined here instead?
};

class Enum: public Type {
public:
};

class Interface: public Type {
public:
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_ENTITY_H_ */
