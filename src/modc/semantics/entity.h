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
class Port;

class Entity {
public:
  virtual ~Entity();

  virtual const EntityName& getName() = 0;
  virtual Entity* getParent() = 0;

  // TODO:  style + style constraints
  // TODO:  visibility
};

// =======================================================================================

class Variable: public Entity {
public:
  virtual ~Variable();

  // If this is a context variable, get its index in the context.  Otherwise, return null.
  virtual Maybe<size_t> getContextPosition() = 0;

  // Get the declared type for the variable.  Note that this only contains constraints that
  // were explicitly declared in the code along with the variable's declaration.  Additional
  // knowledge may be available within the context where the variable is accessed, e.g. even if
  // the variable does not declare what it might point at, if its containing object does, then
  // these constraints can be derived.
  //
  // For pointer variables, getType() returns the type of the pointed-to value.
  virtual ConstrainedType getType(Port& port) = 0;
};

class DataVariable: public Variable {
public:
  virtual ~DataVariable();
};

class PointerVariable: public Variable {
public:
  virtual ~PointerVariable();

  virtual Exclusivity getExclusivity() = 0;

  // Get the declared pointer constraints for the variable, if any.
  virtual Maybe<PointerConstraints> getPointerConstraints(Port& port) = 0;
};

// =======================================================================================

class Function: public Entity {
public:
  virtual ~Function();

  virtual const vector<Variable*>& getParameters() = 0;

  // TODO:  Implicit parameters, environment.
  //   Both may depend on compiling the function body.

  virtual Maybe<DescribedRvalue> call(
      Compiler& compiler, DescribedRvalue&& this_, vector<DescribedRvalue>&& parameters,
      VariableUsageSet& variablesUsed, ErrorLocation location) = 0;

  virtual Rvalue call(vector<Rvalue>&& typeContext, Rvalue&& this_,
                      vector<Rvalue>&& parameters) = 0;
};

class Overload: public Entity {
public:
  virtual ~Overload();

  // Resolves the overload for the given parameters and returns a Thing representing the result.
  // In the case where the overload is resolved to a function, the returned Thing represents the
  // result of calling that function.
  //
  // resolve() may have the effect of lazily instantiating templates.
  virtual Maybe<Thing> resolve(Compiler& compiler, Context&& context, Tuple&& parameters,
                               ErrorLocation location) = 0;
  virtual Maybe<Thing> resolve(Compiler& compiler, DescribedRvalue&& object,
                               Tuple&& parameters, ErrorLocation location) = 0;
};

class BinaryOperator: public Entity {
public:
  virtual ~BinaryOperator();

  enum MatchSpecificity {
    // Order matters.
    NONE,
    GENERALIZED,
    EXACT
  };

  virtual MatchSpecificity match(Compiler& compiler, const DescribedRvalue& this_,
                                 const Thing& other) = 0;

  virtual Maybe<DescribedRvalue> call(Compiler& compiler, DescribedRvalue&& this_, Thing&& other,
                                      VariableUsageSet& variablesUsed, ErrorLocation location) = 0;

  virtual Rvalue call(vector<Rvalue>&& typeContext, Rvalue&& this_, Rvalue&& other) = 0;
};

class UnaryOperator: public Entity {
public:
  virtual ~UnaryOperator();

  virtual Maybe<DescribedRvalue> call(Compiler& compiler, DescribedRvalue&& this_,
                                      VariableUsageSet& variablesUsed, ErrorLocation location) = 0;

  virtual Rvalue call(vector<Rvalue>&& typeContext, Rvalue&& this_) = 0;
};

// =======================================================================================

class Type: public Entity {
public:
  virtual ~Type();

  virtual Thing getMemberOfType(Compiler& compiler, ConstrainedType&& self,
                                const string& memberName) = 0;

  virtual Thing getMemberOfInstance(Compiler& compiler, DescribedRvalue&& parent,
                                    const string& memberName, ErrorLocation location) = 0;

  // Note that constructors, unlike all other members, have context matching the type's context.
  // All other members have a context containing one element:  the instance.
  virtual Overload* getImplicitConstructor() = 0;
  virtual Maybe<Overload&> lookupConstructor(const string& name) = 0;

  // Find a conversion to the given type.
  virtual Maybe<UnaryOperator&> lookupConversion(Compiler& compiler, const DescribedRvalue& object,
                                                 const Bound<Type>& to) = 0;
  virtual Maybe<UnaryOperator&> getDefaultConversion() = 0;

  virtual Maybe<UnaryOperator&> lookupPrefixOperator(ast::PrefixOperator op) = 0;
  virtual Maybe<UnaryOperator&> lookupPostfixOperator(ast::PostfixOperator op) = 0;
  virtual Maybe<BinaryOperator&> lookupLeftBinaryOperator(ast::BinaryOperator op) = 0;
  virtual Maybe<BinaryOperator&> lookupRightBinaryOperator(ast::BinaryOperator op) = 0;

  // NOTE:  interfaceType could be Bound<Interface>...  but this probably forces a copy that
  //    wouldn't otherwise be needed.
  virtual Maybe<ImplementedInterface&> findImplementedInterface(
      Compiler& compiler, const DescribedPointer& object, const Bound<Type>& interfaceType) = 0;

  // Returns whether this type is a subclass of the given type.  This is to be used for the purpose
  // of choosing an overload.  I.e. say for a one-argument function call there are two matching
  // overloads, one taking type Foo and one taking type Bar.  If Foo is more specific than Bar, then
  // the overload taking Foo should be chosen.  If Bar is more specific than Foo, that overload
  // should be chosen.  If neither type is more specific than the other then the situation is
  // ambiguous and therefore an error.
  virtual bool isMoreSpecificThan(Compiler& compiler, const DescribedRvalue& object,
                                  const Bound<Type>& otherType) = 0;

  // Does this type contain any aliases which are not annotated with their potential targets?  This
  // transitively includes members of by-value and heap members, but NOT members of alias members.
  // TODO:  In theory we could be returning the set of types of these unannotated aliases, which
  //   could perhaps allow us to rule out alias annotations on the type itself which can't possibly
  //   correspond to any of these.  But that seems like it could make code too dependent on the
  //   implementation details of the classes it uses.
  virtual bool hasUnannotatedAliases() = 0;
};

class BuiltinType: public Type {
public:
  virtual ~BuiltinType();

  enum class Builtin {
    BOOLEAN,
    INTEGER,
    DOUBLE
  };

  virtual Builtin getType() = 0;

  static BuiltinType* get(Builtin builtin);
};

class Class: public Type {
public:
  virtual ~Class();

  // TODO:  Should some members of Type really be defined here instead?
};

class Enum: public Type {
public:
  virtual ~Enum();
};

class Interface: public Type {
public:
  virtual ~Interface();
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_ENTITY_H_ */
