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
#include "Maybe.h"
#include "macros.h"
#include "ast.h"

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
class Type;
class Class;
class Interface;
class ImplementedInterface;
class Enum;
class Function;
class Overload;
class ThingPort;

// Value types.
class Value;
class DynamicValue;
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

template <typename T>
struct Context {
  // Top-level scope containing all entities that share this context.
  //
  // Note that it does NOT make sense to bind the scope entity to the context itself, as the scope
  // entity actually lives in its parent's context.  The scope entity merely defines a new context
  // for all its children.
  //
  // For example, take this code:
  //   class Foo {
  //     class Bar {
  //       var baz: Integer;
  //     }
  //   }
  // Bar's context contains an instance of Foo.  baz's context contains an instance of Bar.  baz's
  // context's "scope" is Bar.  Bar's context's "scope" is Foo.
  Entity* scope;

  // If |scope| is not the current execution scope or one of its parents then |params| must be
  // filled in with the scope's parameters.  If the scope is a type, then the first param is always
  // "this" (an instance of the scope type).
  Maybe<vector<T>> params;

  VALUE_TYPE2(Context, Entity*, scope, Maybe<vector<T>>&&, params);

  Context(Entity* scope, T&& param): scope(scope), params(vector<T>()) {
    params.push_back(move(param));
  }
};

template <typename EntityType, typename ContextElementType>
struct Bound {
  EntityType* entity;
  Context<ContextElementType> context;

  VALUE_TYPE2(Bound, EntityType*, entity, Context<ContextElementType>&&, context);

  template <typename OtherEntity>
  Bound(Bound<OtherEntity, ContextElementType>&& other)
      : entity(other.entity), context(move(other.context)) {}
};

class Value {
public:
  UNION_TYPE_BOILERPLATE(Value);

  enum class Kind {
    // A placeholder value that is not yet known, e.g. because it is a metaprogramming constant,
    // or because there was an error when trying to evaluate the value.
    UNKNOWN,

    BOOLEAN,
    INTEGER,
    DOUBLE,

    OBJECT,
    ARRAY,

    POINTER,
    INTERFACE_POINTER
  };

  Kind getKind() { return kind; }

  struct Object {
    Bound<Type, Value> type;
    map<Variable*, Value> fields;
  };

  struct Array {
    Bound<Type, Value> elementType;
    vector<Value> elements;
  };

  struct InterfacePointer {
    Bound<Variable, Value> target;
    ImplementedInterface* interface;
  };

  union {
    bool boolean;
    int integer;
    double double_;

    Object object;
    Array array;

    Bound<Variable, Value> pointer;
    InterfacePointer interfacePointer;
  };

  static Value fromUnknown();

  static Value fromBoolean(bool value);
  static Value fromInteger(int value);
  static Value fromDouble(double value);

  static Value fromPointer(Bound<Variable, Value>&& pointer);

private:
  Kind kind;
};

class BoundExpression {
public:
  UNION_TYPE_BOILERPLATE(BoundExpression);

  enum class Kind {
    CONSTANT,

    // Note that the arithmetic operators here are strictly built-in.  Overloaded operators would
    // have been converted to function calls.
    BINARY_OPERATOR,
    PREFIX_OPERATOR,
    POSTFIX_OPERATOR,
    TERNARY_OPERATOR,

    METHOD_CALL,
    SUBSCRIPT,
    MEMBER_ACCESS,

    READ_POINTER,

    // Given a non-pointer input, produce a const pointer to a "temporary" value.
    ALIAS_TEMPORARY,

    // Simple upcast.
    UPCAST
  };

  Kind getKind() const { return kind; }

  struct BinaryOperator {
    ast::BinaryOperator op;
    Indirect<BoundExpression> left;
    Indirect<BoundExpression> right;

    VALUE_TYPE3(BinaryOperator, ast::BinaryOperator, op, BoundExpression&&, left,
                BoundExpression&&, right);
  };

  struct PrefixOperator {
    ast::PrefixOperator op;
    Indirect<BoundExpression> operand;

    VALUE_TYPE2(PrefixOperator, ast::PrefixOperator, op, BoundExpression&&, operand);
  };

  struct PostfixOperator {
    Indirect<BoundExpression> operand;
    ast::PostfixOperator op;

    VALUE_TYPE2(PostfixOperator, BoundExpression&&, operand, ast::PostfixOperator, op);
  };

  struct TernaryOperator {
    Indirect<BoundExpression> condition;
    Indirect<BoundExpression> trueClause;
    Indirect<BoundExpression> falseClause;

    VALUE_TYPE3(TernaryOperator, BoundExpression&&, condition, BoundExpression&&, trueClause,
                BoundExpression&&, falseClause);
  };

  struct MethodCall {
    Function* method;
    Indirect<BoundExpression> object;  // i.e., this

    // Just the variable parameters, since we've already matched them up against the function
    // signature.
    vector<BoundExpression> parameters;

    VALUE_TYPE3(MethodCall, Function*, method, BoundExpression&&, object,
                vector<BoundExpression>&&, parameters);
  };

  struct Subscript {
    // Container must strictly be a built-in array.  Calls to an overloaded operator[] on a class
    // would have been converted to MethodCall.
    Indirect<BoundExpression> container;
    Indirect<BoundExpression> subscript;

    VALUE_TYPE2(Subscript, BoundExpression&&, container, BoundExpression&&, subscript);
  };

  struct MemberAccess {
    Indirect<BoundExpression> object;
    Variable* member;

    VALUE_TYPE2(MemberAccess, BoundExpression&&, object, Variable*, member);
  };

  struct Upcast {
    Indirect<BoundExpression> value;
    ImplementedInterface* interface;
  };

  union {
    Value constant;

    BinaryOperator binaryOperator;
    PrefixOperator prefixOperator;
    PostfixOperator postfixOperator;
    TernaryOperator ternaryOperator;

    MethodCall methodCall;
    Subscript subscript;
    MemberAccess memberAccess;

    Indirect<BoundExpression> readPointer;
    Indirect<BoundExpression> aliasTemporary;

    Upcast upcast;
  };

  static BoundExpression fromUnknown();  // = fromConstant(Value::fromUnknown())

  static BoundExpression fromConstant(Value&& value);

  static BoundExpression fromBinaryOperator(ast::BinaryOperator op, BoundExpression&& left,
                                            BoundExpression&& right);
  static BoundExpression fromPrefixOperator(ast::PrefixOperator op, BoundExpression&& exp);
  static BoundExpression fromPostfixOperator(BoundExpression&& exp, ast::PostfixOperator op);
  static BoundExpression fromTernaryOperator(BoundExpression&& condition,
                                             BoundExpression&& trueClause,
                                             BoundExpression&& falseClause);

  static BoundExpression fromMethodCall(Function* method, BoundExpression&& object,
                                        vector<BoundExpression>&& parameters);
  static BoundExpression fromSubscript(BoundExpression&& container, BoundExpression&& key);
  static BoundExpression fromMemberAccess(BoundExpression&& object, Variable* member);

  static BoundExpression fromReadPointer(BoundExpression&& pointer);

  static BoundExpression fromUpcast(BoundExpression&& value, ImplementedInterface* interface);

private:
  Kind kind;

  BoundExpression(Kind kind): kind(kind) {}
};

struct DynamicValue {
  BoundExpression expression;

  // If the expression is NOT a constant, but is some kind of aggregate where some of the content
  // is actually known at compile time, then partialValue will be filled in with a value that has
  // some UNKNOWN contents.  Otherwise, the partialValue itself is simply UNKNOWN.
  Value partialValue;

  VALUE_TYPE2(DynamicValue, BoundExpression&&, expression, Value&&, partialValue);
  DynamicValue(BoundExpression&& expression)
      : expression(move(expression)), partialValue(Value::fromUnknown()) {}
};

// =======================================================================================

struct EntityName {
  const string& name;
  Maybe<vector<Thing>> parameters;
};

class ValueConstraints {
public:
  // If the value is an alias, these are the things it could possibly point at.  If not present,
  // then the set of things it could point at is undeclared.
  Maybe<vector<Bound<Variable, DynamicValue>>> possibleTargets;

  // Things which this value's unannotated alias members may point at.  If not present, then no
  // declaration has been made -- but if the type contains no unannotated aliases, then this is
  // equivalent to the list being empty.
  enum class AliasType {
    IDENTITY,
    IMMUTABLE,
    MUTABLE,
    ENTANGLED
  };
  Maybe<vector<std::pair<Bound<Variable, DynamicValue>, AliasType>>> transitiveAliases;

  // If the value is an integer, it is known to be in one of these ranges.  If this vector is
  // empty then the integer is assumed to have maximum range.
  struct Range {
    Indirect<Thing> start;
    Indirect<Thing> end;
  };
  vector<Range> intRanges;
};

class ValueDescriptor {
public:
  Bound<Type, DynamicValue> type;
  ast::Style style;
  ValueConstraints constraints;

  bool isPointer() const {
    switch (style) {
      case ast::Style::VALUE:
      case ast::Style::CONSTANT:
        return false;

      case ast::Style::IMMUTABLE_REFERENCE:
      case ast::Style::MUTABLE_REFERENCE:
      case ast::Style::ENTANGLED_REFERENCE:
      case ast::Style::HEAP_VALUE:
        return true;
    }
    // Can't happen, make compiler happy.
    return false;
  }

  bool isAlias() const {
    switch (style) {
      case ast::Style::VALUE:
      case ast::Style::CONSTANT:
      case ast::Style::HEAP_VALUE:
        return false;

      case ast::Style::IMMUTABLE_REFERENCE:
      case ast::Style::MUTABLE_REFERENCE:
      case ast::Style::ENTANGLED_REFERENCE:
        return true;
    }
    // Can't happen, make compiler happy.
    return false;
  }
};

class TypeDescriptor {
public:
  vector<Thing> supertypes;
  vector<Thing> subtypes;
};

class FunctionDescriptor {
public:
  // We don't know anything about functions unless we know the specific function.  So there
  // is nothing to describe.
  VALUE_TYPE0(FunctionDescriptor);
};

// Contains information known at compile time about a Thing, where the Thing itself is not known
// at compile time.
// TODO:  I don't think we actually need this.  Delete?
class ThingDescriptor {
public:
  UNION_TYPE_BOILERPLATE(ThingDescriptor);

  enum class Kind {
    VALUE,
    TYPE,
    FUNCTION  // (possibly overloaded)
  };

  Kind getKind() const;

  union {
    ValueDescriptor value;
    TypeDescriptor type;
    FunctionDescriptor function;
  };

  static ThingDescriptor fromValue(ValueDescriptor&& descriptor);
};

struct Tuple {
public:
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
    UNKNOWN_TYPE,
    VALUE,
    ENTITY,  // i.e. type, function, or overload -- never a variable
    TUPLE
  };

  Kind getKind() const { return kind; }

  struct Unknown {
    VALUE_TYPE0(Unknown);
  };

  struct DescribedValue {
    ValueDescriptor descriptor;
    DynamicValue value;
  };

  union {
    Unknown unknown;
    TypeDescriptor unknownType;
    DescribedValue value;
    Bound<Entity, DynamicValue> entity;
    Tuple tuple;
  };

  static Thing fromUnknown();
  static Thing fromUnknownType(TypeDescriptor&& descriptor);

  static Thing fromEntity(Bound<Entity, DynamicValue>&& entity);

  static Thing fromValue(ValueDescriptor&& descriptor, DynamicValue&& value);
  static Thing fromValue(DescribedValue&& value);

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

  void addUsage(const ValueDescriptor& valueDescriptor, Style style);

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

    VALUE_TYPE2(Usage, Style, style, ErrorLocation, location);
  };

  map<Variable*, Usage> variablesUsed;
};

// =======================================================================================

class Entity {
public:
  virtual ~Entity();

  virtual const EntityName& getName() = 0;
  virtual Entity& getParent() = 0;

  virtual ThingDescriptor getDescriptor(Context<DynamicValue>&& context) = 0;
  virtual const vector<Thing>& getAnnotations(Context<DynamicValue>&& context) = 0;

  // TODO:  style + style constraints
  // TODO:  visibility

  // Read the entity.  If not known at compile time, returns a Thing of kind
  // DYNAMIC_{REFERENCE,VALUE} with code that simply reads the variable.
  // Recursively dereferences outer objects in the context.
  virtual Thing dereference(Context<DynamicValue>&& context, VariableUsageSet& variablesUsed,
                            Location location, VariableUsageSet::Style style) = 0;
};

class Constant: public Entity {
public:
  const Thing& getValue();
};

class Variable: public Entity {
  // Variable keeps track of knowledge about a named value in the current scope.
public:
  virtual ~Variable();

  Bound<Type, DynamicValue> getType(ThingPort& port);

  // This is the *original* descriptor for this variable, but during the course of a function the
  // variable can be modified, adding other annotations.
  // TODO(kenton):  I wonder if, instead of allowing modifications over time, we should require
  //   that the declared constraints be the maximum needed over the value's lifetime?  One problem
  //   is that means they can't be inferred.
  ValueDescriptor getDeclaredDescriptor(ThingPort& port);

  bool assign(Thing&& value, VariableUsageSet& variablesUsed, ErrorLocation location);

  // TODO:  May need context if VariableUsageSet does.
  void entangle(VariableUsageSet::Style style, VariableUsageSet& variablesUsed,
                ErrorLocation location);
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
  struct ParameterSpec {
    Entity* entity;
    Maybe<Thing> defaultValue;
    bool positional;
  };

  const vector<ParameterSpec>& getParameters();

  // The return type doesn't depend on the inputs, but the return descriptor does.
  Type* getReturnType();
  ValueDescriptor getReturnDescriptor(ValueDescriptor&& object, vector<ValueDescriptor>&& params);

  // context.params must be filled in to match the Variables and Aliases in getParameters() --
  // but not the Constants.
  // Returns a FUNCTION_CALL BoundExpression if the call cannot be computed due to inputs being
  // dynamic or if the function implementation is unavailable.
  // TODO:  If a dynamic input is ignored but the computation had side effects we need to make sure
  //   those side effects still take place...
  Value call(Context<DynamicValue>&& context, vector<Value>&& parameters);

  Thing call(Thing::DynamicValue&& object, vector<Thing::DynamicValue>&& params);

  // TODO:  Implicit parameters, environment.
  //   Both may depend on compiling the function body.
};

class Overload: public Entity {
public:
  // Choose an overload to use.  The parameter may be a singular value or a tuple.  If it's a
  // singular value, then it is assumed to be unnamed and have a StyleAllowance of VALUE.  The
  // reason this is allowed is because constructing a single-element tuple in some cases would
  // require making an unnecessary copy of the Thing.
  //
  // If there is no exact match, this may return something that is *close* to matching, if the
  // Overload is confident that it knows which variant the caller intended.  In particular if
  // there is only one overload then it will just be returned.  If the mismatch was in constant
  // parameters, the error will be reported by the Overload itself, but if the mismatch was in
  // dynamic parameters, then the caller is expected to report the problem when it attempts to
  // place those parameters.
  //
  // This may generate meta-functions on-demand.
  //
  // TODO:  Return ordering of TupleElements for convenience?  Or is that essentially a constant
  //   for each Function anyway?  Maybe not for keyword args?
  Maybe<Entity&> resolve(ThingPort& port, const Thing& parameter);

  // TODO:  Resolve given ThingDescriptors?  Or just built a tuple of UNKNOWNs for that?  Actually
  //   ThingDescriptors are somewhat more info than we need...  we really just want to know kind
  //   and type.
};

class Type: public Entity {
public:
  Maybe<Entity&> lookupMemberOfInstance(const string& name);

  // Note that constructors, unlike all other members, have context matching the type's context.
  // All other members have a context containing one element:  the instance.
  Maybe<Overload&> getImplicitConstructor();
  Maybe<Overload&> lookupConstructor(const string& name);

  Maybe<Function&> lookupConversion(ThingPort& port, const Bound<Type, DynamicValue>& to);

  Maybe<Function&> lookupPrefixOperator(ast::PrefixOperator op);
  Maybe<Function&> lookupPostfixOperator(ast::PostfixOperator op);
  Maybe<Overload&> lookupLeftBinaryOperator(ast::BinaryOperator op);
  Maybe<Overload&> lookupRightBinaryOperator(ast::BinaryOperator op);

  // NOTE:  interfaceType could be Bound<Interface, DynamicValue>...  but this probably forces
  //   a copy that wouldn't otherwise be needed.
  Maybe<ImplementedInterface&> findImplementedInterface(
      ThingPort& port, const Bound<Type, DynamicValue>& interfaceType);

  // Returns whether this type is a subclass of the given type.  This is to be used for the purpose
  // of choosing an overload.  I.e. say for a one-argument function call there are two matching
  // overloads, one taking type Foo and one taking type Bar.  If Foo is more specific than Bar, then
  // the overload taking Foo should be chosen.  If Bar is more specific than Foo, that overload
  // should be chosen.  If neither type is more specific than the other then the situation is
  // ambiguous and therefore an error.
  bool isMoreSpecificThan(ThingPort& port, const Bound<Type, DynamicValue>& otherType);

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
  ThingPort(const Context<DynamicValue>& localContext, const Context<DynamicValue>& foreignContext);

  const Context<DynamicValue>& getContext();

  Thing exportThing(Thing&& thing);
  Thing importThing(Thing&& thing);

  Context<DynamicValue> exportContext(const Context<DynamicValue>& input);
};

class Scope {
  // Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
  // their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  const Bound<Entity, DynamicValue>& getEntity();

  // If input.params is filled in, returns it.  Otherwise, finds the context corresponding to
  // input.scope and returns that.  In other words, this effectively fills in input.params if it
  // isn't filled in already.
  const Context<DynamicValue>& fillContext(const Context<DynamicValue>& input);

  // Creates a ThingPort that can be used to port Things between this Scope's context and the
  // target context.  Note that the port is typically passed in whole into an Entity which
  // itself represents the target context.  Therefore, the port's "import" methods port *to*
  // targetContext and "export" ports *from* targetContext.  (This is why it is "makePortFor"
  // instead of just "makePort".)
  //
  // The returned port references targetContext.
  ThingPort makePortFor(const Context<DynamicValue>& targetContext);

  // A variable may have additional constraints (beyond those explicitly declared) that change
  // over the course of a function body.  Only variables with local scope (including members of
  // locals) can have additional constraints, since at any time an exception could cause non-local
  // variables to be left in whatever state they are currently in, so that state must be consistent.
  Maybe<const ValueConstraints&> getTransientConstraints(
      const Bound<Variable, DynamicValue>& variable);

  // Get the variable's current descriptor including transient constraints.
  ValueDescriptor getVariableDescriptor(const Bound<Variable, DynamicValue>& variable);




  // For EXPRESSION

  Thing lookupBuiltinType(BuiltinType::Builtin type);

  Maybe<Entity&> lookupBinding(const string& name);
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
