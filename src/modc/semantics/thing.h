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

#ifndef KENTONSCODE_MODC_SEMANTICS_THING_H_
#define KENTONSCODE_MODC_SEMANTICS_THING_H_

#include "value.h"
#include "expression.h"
#include "context.h"

namespace modc {
namespace compiler {

class Entity;
class Variable;
class Type;
class Overload;

class Thing;

struct EntityName {
  string name;
  Maybe<vector<Thing>> parameters;

  VALUE_TYPE2(EntityName, string&&, name, vector<Thing>&&, parameters);
  explicit EntityName(string&& name): name(move(name)), parameters(nullptr) {}
};

enum class Exclusivity: char {
  // Order is important:  Latter pointers can be coerced to former pointers.
  IDENTITY,
  SHARED,
  EXCLUSIVE,
  OWNED,
};

enum class TargetSpecificity: char {
  EXACT_TARGET,
  TARGET_OR_MEMBER
};

enum class AdditionalTargets: char {
  NONE,
  FROM_CALLER
};

struct DataConstraints {
  struct PossiblePointer {
    // The member which may contain the pointer.  The identified member itself may be the pointer,
    // or it may contain it somewhere within an aggregate structure.  The MemberPath can be empty,
    // indicating that any member could potentially contain the pointer.
    //
    // No element of this path is allowed to have static constraints declared in the code, since
    // in that case it makes no sense for the container object to specify additional constraints.
    // Also, only the last element of the path may itself be a pointer, in which case we're
    // declaring what that exact pointer may point at -- not what the pointed-to value's members
    // may point at.  The pointed-to value's possible pointers are either explicitly annotated on
    // its type, or entirely unknown and presumed to be unchangable.
    MemberPath member;

    // The target of the pointer.  The pointer either points at exactly this target, or some member
    // within the target, transitively.
    //
    // TODO:  Should we distinguish between exact pointers vs. pointers that point somewhere
    //   within?  Could be useful.
    LocalVariablePath target;

    // The maximum exclusivity that the pointer might have.  The pointer could actually end up
    // having a lesser exclusivity, but not greater.
    Exclusivity exclusivity;

    // Whether or not the pointer could point at one of the target's members, in addition to the
    // target itself.
    TargetSpecificity targetSpecificity;

    VALUE_TYPE4(PossiblePointer, MemberPath&&, member, LocalVariablePath&&, target,
                Exclusivity, exclusivity, TargetSpecificity, targetSpecificity);
  };

  // Things which this value's unannotated alias members may point at.
  vector<PossiblePointer> possiblePointers;
  AdditionalTargets additionalPointers;

  // TODO:  Track things which might have a pointer *to* this?  Unclear if/when this is necessary.
  //   Seems like it would only be needed in cases where we can't declare the opposite relationship,
  //   but when would that be?  I guess in cases where the pointer (or object containing the
  //   pointer) has explicit constraints which do not cover this new pointer...  then it's up to
  //   the pointee to satisfy the constraints.  But, practically speaking, when does that happen?

  // If the value (or pointed-at value) is an integer, it is known to be in one of these ranges.
  // If this vector is empty then the integer is assumed to have maximum (32-bit) range.
  //
  // TODO:  These shouldn't be arbitrary Things.  They should be arithmetic expressions on constants
  //   and lvalues, probably allowing only +, -, *, //.
  // TODO:  Union this with pointer-related stuff above that doesn't apply to integers?
  struct Range {
    Indirect<Thing> start;
    Indirect<Thing> end;
  };
  vector<Range> intRanges;

  VALUE_TYPE3(DataConstraints, vector<PossiblePointer>&&, possiblePointers,
              AdditionalTargets, additionalPointers, vector<Range>&&, intRanges);

  DataConstraints(vector<PossiblePointer>&& possiblePointers, AdditionalTargets additionalPointers)
      : possiblePointers(move(possiblePointers)), additionalPointers(additionalPointers) {}
  DataConstraints(vector<Range>&& intRanges)
      : additionalPointers(AdditionalTargets::NONE), intRanges(move(intRanges)) {}

  static DataConstraints fromEmpty() {
    return DataConstraints({}, AdditionalTargets::NONE);
  }
};

struct PointerConstraints {
  struct PossibleTarget {
    // The elements of the path are strictly exclusive pointers, owned pointers, or values.  Shared
    // or identity pointers are replaced with their targets at the time the contraints are built,
    // because there is no need to track exactly what pointer they were derived from.  On the other
    // hand, when a pointer is derived from an existing exclusive pointer, that existing exclusive
    // pointer is no longer exclusive, so this relationship must be tracked.
    //
    // When an exclusive pointer in the path goes out-of-scope, the portion of the path up to and
    // including that pointer must be replaced with the pointer's own possible targets.
    LocalVariablePath path;

    // Whether or not the pointer could point at one of the target's members, in addition to the
    // target itself.
    TargetSpecificity specificity;

    VALUE_TYPE2(PossibleTarget, LocalVariablePath&&, path, TargetSpecificity, specificity);
  };

  // Things that this pointer may point at.
  vector<PossibleTarget> possibleTargets;
  AdditionalTargets additionalTargets;

  VALUE_TYPE2(PointerConstraints, vector<PossibleTarget>&&, possibleTargets,
              AdditionalTargets, additionalTargets)

  PointerConstraints(PossibleTarget&& target): additionalTargets(AdditionalTargets::NONE) {
    possibleTargets.push_back(move(target));
  }
};

struct DataDescriptor {
  Bound<Type> type;
  DataConstraints constraints;

  VALUE_TYPE2(DataDescriptor, Bound<Type>&&, type, DataConstraints&&, constraints);
};

struct PointerDescriptor {
  DataDescriptor targetDescriptor;
  Exclusivity exclusivity;
  PointerConstraints constraints;
  bool canAddConstraints;

  VALUE_TYPE3(PointerDescriptor, DataDescriptor&&, targetDescriptor,
              Exclusivity, exclusivity, PointerConstraints&&, constraints);
};

// ALWAYS represents an unnamed temporary!
struct DescribedData {
  DataDescriptor descriptor;
  DataExpression expression;
  DataValue staticValue;

  VALUE_TYPE3(DescribedData, DataDescriptor&&, descriptor, DataExpression&&, expression,
              DataValue&&, staticValue);
};

struct DescribedPointer {
  PointerDescriptor descriptor;
  PointerExpression expression;
  Maybe<DataValue&> staticPointer;

  VALUE_TYPE3(DescribedPointer, PointerDescriptor&&, descriptor, PointerExpression&&, expression,
              Maybe<DataValue&>, staticPointer);
};

class DescribedRvalue {
public:
  UNION_TYPE_BOILERPLATE(DescribedRvalue);

  enum Kind {
    DATA,
    POINTER
  };

  Kind getKind() { return kind; }

  union {
    DescribedData data;
    DescribedPointer pointer;
  };

  DataDescriptor& dataDescriptor();

  static DescribedRvalue from(DescribedData&& pointer);
  static DescribedRvalue from(DescribedPointer&& pointer);

private:
  Kind kind;
};

struct Tuple {
  struct Element;
  struct NamedElement;

  vector<Element> positionalElements;
  vector<NamedElement> namedElements;

  static Tuple fromSingleValue(Thing&& value);

  VALUE_TYPE2(Tuple, vector<Element>&&, positionalElements,
              vector<NamedElement>&&, namedElements);
};

struct Lvalue {
  // If present, the variable is a member of the given pointer.  Otherwise, the variable is a
  // local variable.
  Maybe<DescribedPointer> parent;

  bool isPointer;
  Variable* variable;

  // Lots of reinterpret_cast here because these types are incomplete and we can't #include them
  // without an include cycle.  Pretty lame, I know.  Things will be better when this code is
  // rewritten in %C.
  DataVariable* dataVariable() {
    return reinterpret_cast<DataVariable*>(variable);
  }
  PointerVariable* pointerVariable() {
    return reinterpret_cast<PointerVariable*>(variable);
  }

  Lvalue(DescribedPointer&& parent, DataVariable* member)
      : parent(move(parent)), isPointer(false), variable(reinterpret_cast<Variable*>(member)) {}
  Lvalue(DescribedPointer&& parent, PointerVariable* member)
      : parent(move(parent)), isPointer(true), variable(reinterpret_cast<Variable*>(member)) {}
  explicit Lvalue(DataVariable* localVariable)
      : parent(nullptr), isPointer(false), variable(reinterpret_cast<Variable*>(localVariable)) {}
  explicit Lvalue(PointerVariable* localVariable)
      : parent(nullptr), isPointer(true), variable(reinterpret_cast<Variable*>(localVariable)) {}

  bool operator==(const Lvalue& other) const {
    return parent == other.parent && variable == other.variable;
  }
  bool operator!=(const Lvalue& other) const {
    return !(*this == other);
  }
};

struct ConstrainedType {
  Bound<Type> type;
  Maybe<DataConstraints> constraints;

  VALUE_TYPE2(ConstrainedType, Bound<Type>&&, type, DataConstraints&&, constraints);
  explicit ConstrainedType(Bound<Type>&& type)
      : type(move(type)), constraints(nullptr) {}
  explicit ConstrainedType(Bound<Type>&& type, Maybe<DataConstraints>&& constraints)
      : type(move(type)), constraints(move(constraints)) {}
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Kind {
    RVALUE,
    LVALUE,

    FUNCTION,
    METHOD,
    TYPE,
    TUPLE
  };

  Kind getKind() const { return kind; }

  struct Method {
    DescribedRvalue object;
    Overload* method;
  };

  union {
    DescribedRvalue rvalue;
    Lvalue lvalue;

    Bound<Overload> function;
    Method method;
    ConstrainedType type;
    Tuple tuple;
  };

  static Thing fromEntity(Bound<Entity>&& entity);

  static Thing fromFunction(Bound<Overload>&& overload);
  static Thing fromMethod(DescribedData&& object, Overload* method);

  static Thing fromType(Bound<Type>&& type);
  static Thing fromType(Bound<Type>&& type, DataConstraints&& constraints);

  static Thing from(DescribedData&& data);
  static Thing from(DescribedPointer&& pointer);
  static Thing from(DescribedRvalue&& rvalue);
  static Thing from(Lvalue&& lvalue);

  // Convenient.
  static Maybe<Thing> from(Maybe<DescribedData>&& data);
  static Maybe<Thing> from(Maybe<DescribedPointer>&& pointer);
  static Maybe<Thing> from(Maybe<DescribedRvalue>&& rvalue);

private:
  Kind kind;
};

struct Tuple::Element {
  ast::StyleAllowance styleAllowance;
  Indirect<Thing> value;

  VALUE_TYPE2(Element, ast::StyleAllowance, styleAllowance, Thing&&, value);
};

struct Tuple::NamedElement {
  EntityName name;
  Element element;

  VALUE_TYPE2(NamedElement, EntityName&&, name, Element&&, element);
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_THING_H_ */
