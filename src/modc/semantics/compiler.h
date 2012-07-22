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

#ifndef KENTONSCODE_MODC_COMPILER_H_
#define KENTONSCODE_MODC_COMPILER_H_

#include <map>
#include <set>
#include <string>
#include <vector>

#include "base/OwnedPtr.h"
#include "../Maybe.h"
#include "../macros.h"
#include "../syntax/ast.h"

namespace modc {
namespace compiler {

using namespace ekam;
using std::string;
using std::vector;
using std::map;
using std::set;
using errors::Location;

class CodePrinter;

// Resource types.
class Scope;
class Entity;
class Variable;
class ValueVariable;
class PointerVariable;
class Type;
class Class;
class Interface;
class ImplementedInterface;
class Enum;
class Function;
class Overload;
class ThingPort;
class Compiler;

// Value types.
class Value;
class Reference;
class Thing;
class CxxExpression;

// =======================================================================================

class ErrorLocation {
public:
  template <typename... Parts>
  void error(Parts&&... parts);
  void error(errors::Error&& error);
};

// =======================================================================================

struct CxxExpression {
  class Piece;

  enum class Priority {
    UNKNOWN,
    COMMA,
    THROW,
    ASSIGNMENT,
    TERNARY,
    LOGICAL_OR,
    LOGICAL_AND,
    BITWISE_OR,
    BITWISE_XOR,
    BITWISE_AND,
    EQUALITY,
    COMPARISON,
    SHIFT,
    ADDITION,
    MULTIPLICATION,
    POINTER_TO_MEMBER,
    PREFIX,
    SUFFIX,
    SCOPE
  };

  vector<Piece> pieces;
  Priority priority;
  bool isPointer;

  CxxExpression& append(const char* text);
  CxxExpression& append(string&& text);
  CxxExpression& append(const string& text);
  CxxExpression& append(CxxExpression&& subExpression);
  CxxExpression& appendAsPointer(CxxExpression&& subExpression);
  CxxExpression& appendMemberAccess(CxxExpression&& subExpression, const string& memberName);
  CxxExpression& appendBreak();

  CxxExpression(Priority priority, bool isPointer): priority(priority), isPointer(isPointer) {}
  CxxExpression(string&& raw): priority(Priority::UNKNOWN), isPointer(false) { append(raw); }
  VALUE_TYPE3(CxxExpression, vector<Piece>&&, pieces, Priority, priority, bool, isPointer);
};

struct CxxStatement {
  CxxExpression code;

  struct Block;
  vector<Block> blocks;

  CxxStatement(CxxExpression&& code): code(move(code)) {}
  VALUE_TYPE2(CxxStatement, CxxExpression&&, code, vector<Block>&&, blocks);

  static CxxStatement addSemicolon(CxxExpression&& code);
};

struct CxxStatement::Block {
  vector<CxxStatement> statements;
  CxxExpression trailingLine;

  Block();
  VALUE_TYPE2(Block, vector<CxxStatement>&&, statements, CxxExpression&&, trailingLine);
};

class CxxExpression::Piece {
public:
  UNION_TYPE_BOILERPLATE(Piece);

  enum class Type {
    TEXT,
    SUB_EXPRESSION
  };

  union {
    string text;
    CxxExpression subExpression;
  };
};

// =======================================================================================

struct Context {
  class Binding;

  // How many Bindings are visible in this Context?
  vector<Binding>::size_type depth;

  // The set of bindings which are not shared with the underlay.  These are the last bindings in
  // the context.
  vector<Binding> suffix;

  // Bindings not covered by the suffix can be found in the underlay.  If suffix.size() == depth,
  // then the underlay may be null.
  const Context* underlay;

  const Binding& operator[](vector<Binding>::size_type index) const {
    assert(index < depth);
    vector<Binding>::size_type overlayStart = depth - suffix.size();
    if (index < overlayStart) {
      return (*underlay)[index];
    } else {
      return suffix[index - overlayStart];
    }
  }

  Context(vector<Binding>::size_type depth, vector<Binding>&& suffix, const Context& underlay)
      : depth(depth), suffix(move(suffix)), underlay(&underlay) {
    assert(suffix.size() <= depth);
  }
  Context(vector<Binding>&& suffix)
      : depth(suffix.size()), suffix(move(suffix)), underlay(nullptr) {}
  Context(const Context& underlay, Binding&& suffix)
      : depth(underlay.depth + 1), underlay(&underlay) {
    this->suffix.push_back(move(suffix));
  }

  bool operator==(const Context& other) const;
};

template <typename EntityType>
struct Bound {
  EntityType* entity;
  Context context;

  VALUE_TYPE2(Bound, EntityType*, entity, Context&&, context);

  template <typename OtherEntity>
  Bound(Bound<OtherEntity>&& other)
      : entity(other.entity), context(move(other.context)) {}
};

struct MemberPath {
  // TODO:  What about array subscripts?
  vector<Variable*> path;

  VALUE_TYPE1(MemberPath, vector<Variable*>&&, path);
  MemberPath() {}

  bool isPrefix(const MemberPath& other) const;
};

struct LocalVariablePath {
  Variable* root;
  MemberPath member;

  VALUE_TYPE2(LocalVariablePath, Variable*, root, MemberPath&&, member);

  LocalVariablePath(Variable* root): root(root) {}

  bool isPrefix(const LocalVariablePath& other) const;
};

class ContextBinding;

class Value {
public:
  UNION_TYPE_BOILERPLATE(Value);

  enum class Kind {
    // A placeholder value that is not yet known, e.g. because it is a metaprogramming constant,
    // or because there was an error when trying to evaluate the value.
    UNKNOWN,

    // Pure data.
    BOOLEAN,
    INTEGER,
    DOUBLE,

    OBJECT,
    ARRAY,
  };

  Kind getKind() { return kind; }

  struct Object {
    Bound<Type> type;

    map<ValueVariable*, Value> fields;

    // null pointers are unknown.
    map<PointerVariable*, Value*> pointerFields;
  };

  struct Array {
    Bound<Type> elementType;
    vector<Value> elements;
  };

  union {
    bool boolean;
    int integer;
    double double_;

    Object object;
    Array array;
  };

  static Value fromUnknown();

  static Value fromBoolean(bool value);
  static Value fromInteger(int value);
  static Value fromDouble(double value);

private:
  Kind kind;
};

class Context::Binding {
public:
  UNION_TYPE_BOILERPLATE(Binding);

  enum class Kind {
    CONSTANT,
    POINTER,
    INTEGER_EXPRESSION
  };

  Kind getKind();

  union {
    Value constant;
    LocalVariablePath pointer;
    // TODO:  Integer expressions.
  };

  static Binding fromPointer(LocalVariablePath&& pointer);
};

// =======================================================================================

class DynamicPointer;
class DynamicValueOrPointer;

class DynamicValue {
public:
  UNION_TYPE_BOILERPLATE(DynamicValue);

  enum class Kind {
    CONSTANT,
    LOCAL_VARIABLE,

    // Note that the arithmetic operators here are strictly built-in.  Overloaded operators would
    // have been converted to function calls.
    BINARY_OPERATOR,
    PREFIX_OPERATOR,
    POSTFIX_OPERATOR,
    TERNARY_OPERATOR,

    METHOD_CALL,

    // Given a pure ("temporary") object, produce a member value.
    READ_MEMBER,

    // Given a pointer input, read its value.
    READ_POINTER,
  };

  Kind getKind() const { return kind; }

  struct BinaryOperator {
    ast::BinaryOperator op;
    Indirect<DynamicValue> left;
    Indirect<DynamicValue> right;

    VALUE_TYPE3(BinaryOperator, ast::BinaryOperator, op, DynamicValue&&, left,
                DynamicValue&&, right);
  };

  struct PrefixOperator {
    ast::PrefixOperator op;
    Indirect<DynamicValue> operand;

    VALUE_TYPE2(PrefixOperator, ast::PrefixOperator, op, DynamicValue&&, operand);
  };

  struct PostfixOperator {
    Indirect<DynamicValue> operand;
    ast::PostfixOperator op;

    VALUE_TYPE2(PostfixOperator, DynamicValue&&, operand, ast::PostfixOperator, op);
  };

  struct TernaryOperator {
    Indirect<DynamicValue> condition;
    Indirect<DynamicValue> trueClause;
    Indirect<DynamicValue> falseClause;

    VALUE_TYPE3(TernaryOperator, DynamicValue&&, condition, DynamicValue&&, trueClause,
                DynamicValue&&, falseClause);
  };

  struct MethodCall {
    Function* method;

    Indirect<DynamicPointer> object;

    // Just the variable parameters, since we've already matched them up against the function
    // signature.
    vector<DynamicValueOrPointer> parameters;

    VALUE_TYPE3(MethodCall, Function*, method, DynamicPointer&&, object,
                vector<DynamicValueOrPointer>&&, parameters);
  };

  struct ReadMember {
    Indirect<DynamicValue> object;
    ValueVariable* member;

    VALUE_TYPE2(ReadMember, DynamicValue&&, object, ValueVariable*, member);
  };

  union {
    Value constant;
    ValueVariable* localVariable;

    BinaryOperator binaryOperator;
    PrefixOperator prefixOperator;
    PostfixOperator postfixOperator;
    TernaryOperator ternaryOperator;

    MethodCall methodCall;
    ReadMember readMember;

    Indirect<DynamicValue> readPointer;
  };

  static DynamicValue fromConstant(Value&& value);
  static DynamicValue fromLocalVariable(ValueVariable* variable);

  static DynamicValue fromBinaryOperator(ast::BinaryOperator op, DynamicValue&& left,
                                         DynamicValue&& right);
  static DynamicValue fromPrefixOperator(ast::PrefixOperator op, DynamicValue&& exp);
  static DynamicValue fromPostfixOperator(DynamicValue&& exp, ast::PostfixOperator op);
  static DynamicValue fromTernaryOperator(DynamicValue&& condition,
                                          DynamicValue&& trueClause,
                                          DynamicValue&& falseClause);

  static DynamicValue fromMethodCall(Function* method, DynamicPointer&& object,
                                     vector<DynamicValueOrPointer>&& parameters);
  static DynamicValue fromReadMember(DynamicValue&& object, ValueVariable* member);

  static DynamicValue fromReadPointer(DynamicPointer&& pointer);

private:
  Kind kind;

  DynamicValue(Kind kind): kind(kind) {}
};

class DynamicPointer {
public:
  UNION_TYPE_BOILERPLATE(DynamicPointer);

  enum class Kind {
    LOCAL_VARIABLE,

    TERNARY_OPERATOR,

    METHOD_CALL,
    SUBSCRIPT,

    // Given a pure ("temporary") object, read a member pointer.
    READ_MEMBER,

    // Given a pointer, read a member pointer.
    READ_POINTER_MEMBER,

    // Given a pointer, produce a pointer to a value member.
    POINTER_TO_MEMBER,

    // Upcast to an interface.
    UPCAST,
  };

  Kind getKind() const { return kind; }

  struct TernaryOperator {
    Indirect<DynamicValue> condition;
    Indirect<DynamicPointer> trueClause;
    Indirect<DynamicPointer> falseClause;

    VALUE_TYPE3(TernaryOperator, DynamicValue&&, condition, DynamicPointer&&, trueClause,
                DynamicPointer&&, falseClause);
  };

  struct MethodCall {
    Function* method;

    Indirect<DynamicPointer> object;

    // Just the variable parameters, since we've already matched them up against the function
    // signature.
    vector<DynamicValueOrPointer> parameters;

    VALUE_TYPE3(MethodCall, Function*, method, DynamicPointer&&, object,
                vector<DynamicValueOrPointer>&&, parameters);
  };

  struct Subscript {
    // Container must strictly be a built-in array.  Calls to an overloaded operator[] on a class
    // would have been converted to MethodCall.
    Indirect<DynamicPointer> container;
    Indirect<DynamicValue> subscript;

    VALUE_TYPE2(Subscript, DynamicPointer&&, container, DynamicValue&&, subscript);
  };

  struct ReadMember {
    Indirect<DynamicValue> object;
    PointerVariable* member;

    VALUE_TYPE2(ReadMember, DynamicValue&&, object, PointerVariable*, member);
  };

  struct ReadPointerMember {
    Indirect<DynamicPointer> object;
    PointerVariable* member;

    VALUE_TYPE2(ReadPointerMember, DynamicPointer&&, object, PointerVariable*, member);
  };

  struct PointerToMember {
    Indirect<DynamicPointer> object;
    Variable* member;

    VALUE_TYPE2(PointerToMember, DynamicPointer&&, object, Variable*, member);
  };

  struct Upcast {
    Indirect<DynamicPointer> object;
    ImplementedInterface* interface;

    VALUE_TYPE2(Upcast, DynamicPointer&&, object, ImplementedInterface*, interface);
  };

  union {
    PointerVariable* localVariable;

    TernaryOperator ternaryOperator;

    MethodCall methodCall;
    Subscript subscript;
    ReadMember readMember;
    ReadPointerMember readPointerMember;
    PointerToMember pointerToMember;
    Upcast upcast;
  };

  static DynamicPointer fromLocalVariable(PointerVariable* variable);

  static DynamicPointer fromTernaryOperator(DynamicValue&& condition,
                                            DynamicPointer&& trueClause,
                                            DynamicPointer&& falseClause);

  static DynamicPointer fromMethodCall(Function* method, DynamicPointer&& object,
                                        vector<DynamicValueOrPointer>&& parameters);
  static DynamicPointer fromSubscript(DynamicPointer&& container, DynamicValue&& key);
  static DynamicPointer fromPointerToMember(DynamicPointer&& object, Variable* member);
  static DynamicPointer fromUpcast(DynamicPointer&& object, ImplementedInterface* interface);

private:
  Kind kind;

  DynamicPointer(Kind kind): kind(kind) {}
};

class DynamicValueOrPointer {
public:
  UNION_TYPE_BOILERPLATE(DynamicValueOrPointer);

  enum Kind {
    VALUE,
    POINTER,
  };

  Kind getKind();

  union {
    DynamicValue value;
    DynamicPointer pointer;
  };
};

// =======================================================================================

struct EntityName {
  const string& name;
  Maybe<vector<Thing>> parameters;
};

enum Exclusivity: char {
  // Order is important:  Latter pointers can be coerced to former pointers.
  IDENTITY,
  SHARED,
  EXCLUSIVE,
  OWNED,
};

enum TargetSpecificity: char {
  EXACT_TARGET,
  TARGET_OR_MEMBER
};

enum AdditionalTargets: char {
  NONE,
  FROM_CALLER
};

struct ValueConstraints {
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

  VALUE_TYPE3(ValueConstraints, vector<PossiblePointer>&&, possiblePointers,
              AdditionalTargets, additionalPointers, vector<Range>&&, intRanges);

  ValueConstraints(vector<PossiblePointer>&& possiblePointers,
                   AdditionalTargets additionalPointers)
      : possiblePointers(move(possiblePointers)), additionalPointers(additionalPointers) {}
  ValueConstraints(vector<Range>&& intRanges)
      : additionalPointers(AdditionalTargets::NONE), intRanges(move(intRanges)) {}
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

// Description of pointer constraints that doesn't require a specific binding for the object
// of which this variable is a member.
struct UnboundPointerConstraints {
  // Constraints that don't reference the object at all, but rather something in the pointer's
  // context, or the calling scope.
  PointerConstraints parentIndependentConstraints;

  // Additional possible targets of this pointer which point at sibling variables -- i.e. other
  // members of the same object.  Each path is rooted at such a sibling variable, which means
  // they must be grafted to some path rooted in a local variable before they can be used in
  // any other context.
  vector<PointerConstraints::PossibleTarget> innerPointers;
};

struct ValueDescriptor {
  Bound<Type> type;
  ValueConstraints constraints;

  VALUE_TYPE2(ValueDescriptor, Bound<Type>&&, type, ValueConstraints&&, constraints);
};

struct PointerDescriptor {
  ValueDescriptor targetDescriptor;
  Exclusivity exclusivity;
  PointerConstraints constraints;
  bool canAddConstraints;

  VALUE_TYPE3(PointerDescriptor, ValueDescriptor&&, targetDescriptor,
              Exclusivity, exclusivity, PointerConstraints&&, constraints);
};

struct DescribedValue {
  ValueDescriptor descriptor;
  DynamicValue value;
  Value staticValue;

  VALUE_TYPE3(DescribedValue, ValueDescriptor&&, descriptor, DynamicValue&&, value,
              Value&&, staticValue);
};

struct DescribedPointer {
  PointerDescriptor descriptor;
  DynamicPointer pointer;
  Maybe<Value&> staticPointer;

  VALUE_TYPE3(DescribedPointer, PointerDescriptor&&, descriptor, DynamicPointer&&, pointer,
              Maybe<Value&>, staticPointer);
};

class DescribedPointerOrValue {
public:
  UNION_TYPE_BOILERPLATE(DescribedPointerOrValue);

  enum Kind {
    POINTER,
    VALUE
  };

  Kind getKind() { return kind; }

  union {
    DescribedPointer pointer;
    DescribedValue value;
  };

  DescribedPointerOrValue from(DescribedPointer&& pointer);
  DescribedPointerOrValue from(DescribedValue&& pointer);

private:
  Kind kind;
};

struct Tuple {
  struct Element;

  vector<Element> positionalElements;
  vector<std::pair<EntityName, Element>> namedElements;

  static Tuple fromSingleValue(Thing&& value);
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Kind {
    UNKNOWN,

    VALUE,
    POINTER,
    LVALUE,
    POINTER_LVALUE,

    FUNCTION,
    METHOD,
    TYPE,
    TUPLE
  };

  Kind getKind() const { return kind; }

  struct Unknown {
    VALUE_TYPE0(Unknown);
  };

  struct Lvalue {
    // If present, the variable is a member of the given pointer.  Otherwise, the variable is a
    // local variable.
    Maybe<DescribedPointer> parent;
    ValueVariable* variable;

    VALUE_TYPE2(Lvalue, DescribedPointer&&, parent, ValueVariable*, variable);
    Lvalue(ValueVariable* variable): parent(nullptr), variable(variable) {}
  };

  struct PointerLvalue {
    // If present, the variable is a member of the given pointer.  Otherwise, the variable is a
    // local variable.
    Maybe<DescribedPointer> parent;
    PointerVariable* variable;

    VALUE_TYPE2(PointerLvalue, DescribedPointer&&, parent, PointerVariable*, variable);
    PointerLvalue(PointerVariable* variable): parent(nullptr), variable(variable) {}
  };

  struct Method {
    DescribedPointerOrValue object;
    Overload* method;
  };

  struct ConstrainedType {
    Bound<Type> type;
    Maybe<ValueConstraints> constraints;
  };

  union {
    Unknown unknown;

    DescribedValue value;
    DescribedPointer pointer;
    Lvalue lvalue;
    PointerLvalue pointerLvalue;

    Bound<Overload> function;
    Method method;
    ConstrainedType type;
    Tuple tuple;
  };

  static Thing fromUnknown();

  static Thing fromEntity(Bound<Entity>&& entity);

  static Thing fromValue(ValueDescriptor&& descriptor, DynamicValue&& value, Value staticValue);
  static Thing fromValue(DescribedValue&& value);
  static Thing fromPointer(PointerDescriptor&& descriptor, DynamicPointer&& pointer,
                           Maybe<Value&> staticPointer);
  static Thing fromPointer(DescribedPointer&& pointer);
  static Thing fromLvalue(DescribedPointer&& parent, ValueVariable* variable);
  static Thing fromLvalue(DescribedPointer&& parent, PointerVariable* variable);
  static Thing fromLvalue(ValueVariable* variable);
  static Thing fromLvalue(PointerVariable* variable);

  static Thing fromFunction(Bound<Overload>&& overload);
  static Thing fromMethod(DescribedValue&& object, Overload* method);

  static Thing fromType(Bound<Type>&& type);
  static Thing fromType(Bound<Type>&& type, ValueConstraints&& constraints);

private:
  Kind kind;
};

struct Tuple::Element {
  ast::StyleAllowance styleAllowance;
  Indirect<Thing> value;

  VALUE_TYPE2(Element, ast::StyleAllowance, styleAllowance, Thing&&, value);
};

// TODO:  EntityUsageSet?  Could be useful in determining dependencies and forward declarations?
//   Or is that a fundamentally different thing?
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

  bool operator==(const VariableUsageSet& other) const {
    return variablesUsed == other.variablesUsed;
  }
  bool operator!=(const VariableUsageSet& other) const {
    return variablesUsed != other.variablesUsed;
  }

private:
  struct Usage {
    Style style;
    ErrorLocation location;

    Usage(Style style, ErrorLocation location): style(style), location(location) {}
  };

  map<Variable*, Usage> variablesUsed;
};

// =======================================================================================

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

class Constant: public Entity {
public:
  const Thing& getValue();
};

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
                                        const Value& containingObject);
};

class ValueVariable: public Variable {
public:
  virtual ~ValueVariable();
};

class PointerVariable: public Variable {
public:
  virtual ~PointerVariable();

  Exclusivity getExclusivity();

  bool hasConstraints();

  // Get declared constraints for this pointer.
  Maybe<PointerConstraints> getConstraints(const Context& context);

  // Get constraints independently of any binding for the object of which this variable is a member.
  // Instead, the object's type's context is provided.
  Maybe<UnboundPointerConstraints> getUnboundConstraints(const Context& containingTypeContext);
};

class Alias: public Entity {
public:
  virtual ~Alias();

  // Dereferencing returns one of:
  // - ERROR if the alias is identity-only.
  // - A constant if the exact target value is known at compile time and the alias is immutable.
  // - REFERENCE if the target entity is known at compile time.  Note that the target entity is
  //   never itself an alias!
  // - DYNAMIC_REFERENCE of DEREFERENCE of REFERENCE of this, entangled with this.
};

class Function: public Entity {
public:
  const vector<Variable*>& getParameters();

  // TODO:  Implicit parameters, environment.
  //   Both may depend on compiling the function body.
};

class ValueFunction: public Function {
public:
  // TODO: Get return descriptor.  This is similar to getting the descriptor for a member variable
  //   except more complicated because any of the parameters may factor into the output type so we
  //   may need to bind any of them to local vars if they aren't already, etc.

  // TODO: invoke()
};

class PointerFunction: public Function {
public:
  // TODO: Get return descriptor.  This is similar to getting the descriptor for a member variable
  //   except more complicated because any of the parameters may factor into the output type so we
  //   may need to bind any of them to local vars if they aren't already, etc.

  // TODO: invoke()
};

class Overload: public Entity {
public:
  // Resolves the overload for the given parameters and returns a Thing representing the result.
  // In the case where the overload is resolved to a function, the returned Thing represents the
  // result of calling that function.
  //
  // resolve() may have the effect of lazily instantiating templates.
  Thing resolve(Compiler& compiler, Context&& context, const Tuple& parameters,
                ErrorLocation location);
  Thing resolve(Compiler& compiler, DescribedPointerOrValue&& object,
                const Tuple& parameters, ErrorLocation location);
};

class Type: public Entity {
public:
  Thing getMemberOfType(Compiler& compiler, Thing::ConstrainedType&& self,
                        const string& memberName);

  Thing getMemberOfInstance(Compiler& compiler, DescribedValue&& parent,
                            const string& memberName, ErrorLocation location);
  Thing getMemberOfInstance(Compiler& compiler, DescribedPointer&& parent,
                            const string& memberName, ErrorLocation location);

  // Note that constructors, unlike all other members, have context matching the type's context.
  // All other members have a context containing one element:  the instance.
  Overload* getImplicitConstructor();
  Maybe<Overload&> lookupConstructor(const string& name);

  Maybe<Function&> lookupConversion(ThingPort& port, const Bound<Type>& to);

  Maybe<Function&> lookupPrefixOperator(ast::PrefixOperator op);
  Maybe<Function&> lookupPostfixOperator(ast::PostfixOperator op);
  Maybe<Overload&> lookupLeftBinaryOperator(ast::BinaryOperator op);
  Maybe<Overload&> lookupRightBinaryOperator(ast::BinaryOperator op);

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
  ValueDescriptor getVariableDescriptor(ValueVariable* variable);
  PointerDescriptor getVariableDescriptor(PointerVariable* variable);

  void setVariableConstraints(ValueVariable* variable, const ValueConstraints& constraints);
  void setVariableConstraints(PointerVariable* variable, const PointerConstraints& ptrConstraints,
                              const ValueConstraints& valConstraints);

  // Get a pointer to the variable's value.  May be a partial value.  If the caller is simulating
  // changes to the value, it should modify the value directly.
  Value* getVariable(Variable* variable);




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

// =======================================================================================

vector<CxxStatement> compileImperative(Scope& scope, const vector<ast::Statement>& statements);

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_COMPILER_H_ */
