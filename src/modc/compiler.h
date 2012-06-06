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
class Context;
class Scope;
class Entity;
class Variable;
class Type;
class Class;
class Interface;
class Enum;
class Function;
class Overload;

// Value types.
class Value;
class Reference;
class Thing;
class CxxExpression;

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

    POINTER
  };

  Kind getKind() { return kind; }

  struct Object {
    Type* type;
    vector<Value> typeContext;
    map<Variable*, Value> fields;
  };

  struct Array {
    Type* elementType;
    vector<Value> elementTypeContext;
    vector<Value> elements;
  };

  struct Pointer {
    Variable* variable;
    vector<Value> context;
  };

  union {
    bool boolean;
    int integer;
    double double_;

    Object object;
    Array array;

    Pointer pointer;
  };

  static Value fromUnknown();

  static Value fromBoolean(bool value);
  static Value fromInteger(int value);
  static Value fromDouble(double value);

  static Value fromPointer(Variable* variable, vector<Value>&& context);

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

    READ_POINTER

    // cast?
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
    Entity* member;
    ast::StyleAllowance thisStyleAllowance;

    VALUE_TYPE3(MemberAccess, BoundExpression&&, object, Entity*, member,
                ast::StyleAllowance, thisStyleAllowance);
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
  };

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
  static BoundExpression fromMemberAccess(BoundExpression&& object, string&& member,
                                          ast::StyleAllowance thisStyleAllowance);

  static BoundExpression fromReadPointer(BoundExpression&& pointer);

private:
  Kind kind;

  BoundExpression(Kind kind): kind(kind) {}
};

// =======================================================================================

struct EntityName {
  const string& name;
  Maybe<vector<Thing>> parameters;
};

class ValueDescriptor {
public:
  Type* type;
  vector<Thing> typeContext;
  ast::Style style;

  // Things which are accessible through this value, e.g. if this value is a pointer or contains
  // pointers.
  enum class AliasType {
    IDENTITY,
    IMMUTABLE,
    MUTABLE,
    ENTANGLED
  };
  vector<std::pair<Value::Pointer, AliasType>> aliases;

  // TODO:  Constraints, e.g. integer range
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
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Kind {
    UNKNOWN,
    VALUE,
    ENTITY,  // i.e. type or overload
    TUPLE
  };

  Kind getKind() const { return kind; }

  struct DynamicValue {
    ValueDescriptor descriptor;
    BoundExpression expression;

    // If expression is NOT a constant, but is partially known, this is filled in.
    Maybe<Value> partialValue;
  };

  struct TupleElement {
    Maybe<EntityName> name;
    ast::StyleAllowance styleAllowance;
    Indirect<Thing> value;

    VALUE_TYPE3(TupleElement, Maybe<EntityName>&&, name, ast::StyleAllowance, styleAllowance,
                Thing&&, value);
  };

  struct BoundEntity {
    Entity* entity;
    vector<Thing> context;
  };

  union {
    Maybe<ThingDescriptor> unknown;
    DynamicValue value;
    BoundEntity entity;
    vector<TupleElement> tuple;
  };

  static Thing fromUnknown();
  static Thing fromUnknown(ThingDescriptor&& descriptor);
  static Thing fromUnknown(Maybe<ThingDescriptor>&& descriptor);

  static Thing fromEntity(Entity* entity, vector<Thing>&& context);

  static Thing fromValue(ValueDescriptor&& descriptor, BoundExpression&& expression);
  static Thing fromValue(ValueDescriptor&& descriptor, BoundExpression&& expression,
                         Maybe<Value>&& partialValue);

private:
  Kind kind;
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
  void addSequential(Variable* variable, Style style, errors::Location location);

  // Given a list of VariableUsageSets representing variable usage in a series of parallel
  // operations, check that the usages do not conflict, and then merge them all into this set.
  void merge(vector<VariableUsageSet>&& parallelUsages, Context& context);

  bool operator==(const VariableUsageSet& other) const {
    return variablesUsed == other.variablesUsed;
  }
  bool operator!=(const VariableUsageSet& other) const {
    return variablesUsed != other.variablesUsed;
  }

private:
  struct Usage {
    Style style;
    errors::Location location;

    VALUE_TYPE2(Usage, Style, style, errors::Location, location);
  };

  map<Variable*, Usage> variablesUsed;
};

// =======================================================================================

class Entity {
public:
  virtual ~Entity();

  virtual const EntityName& getName() = 0;
  virtual Entity& getParent() = 0;

  virtual ThingDescriptor getDescriptor(vector<Thing>&& context) = 0;
  virtual const vector<Thing>& getAnnotations(vector<Thing>&& context) = 0;

  // TODO:  style + style constraints
  // TODO:  visibility

  // Read the entity.  If not known at compile time, returns a Thing of kind
  // DYNAMIC_{REFERENCE,VALUE} with code that simply reads the variable.
  // Recursively dereferences outer objects in the context.
  virtual Thing dereference(vector<Thing>&& context, VariableUsageSet& variablesUsed,
                            Location location, VariableUsageSet::Style style) = 0;

  virtual Maybe<Entity&> lookupMember(const string& name) = 0;
};

class Constant: public Entity {
public:
  const Thing& getValue();
};

class Variable: public Entity {
  // Variable keeps track of knowledge about a named value in the current scope.
public:
  virtual ~Variable();

  bool assign(Thing&& value, VariableUsageSet& variablesUsed, errors::Location location);

  // TODO:  May need context if VariableUsageSet does.
  void entangle(VariableUsageSet::Style style, VariableUsageSet& variablesUsed,
                errors::Location location);
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
  Value call(vector<Value&&> context, vector<Value>&& parameters);

  Thing call(Thing::DynamicValue&& object, vector<Thing::DynamicValue>&& params);

  // TODO:  Implicit parameters, environment.
  //   Both may depend on compiling the function body.
};

class Overload: public Entity {
public:
  // This may generate meta-functions on-demand.
  // TODO:  Do we need a context to resolve?  I don't think so since no execution occurs here.
  // TODO:  Return ordering of TupleElements for convenience?
  Maybe<Entity&> resolve(const vector<Thing::TupleElement>& parameters);

  // TODO:  Resolve given ThingDescriptors?  Or just built a tuple of UNKNOWNs for that?  Actually
  //   ThingDescriptors are somewhat more info than we need...  we really just want to know kind
  //   and type.
};

class Type: public Entity {
public:
  Maybe<Entity&> lookupMemberOfInstance(const string& name);

  Maybe<Function&> lookupPrefixOperator(ast::PrefixOperator op);
  Maybe<Function&> lookupPostfixOperator(ast::PostfixOperator op);
  Maybe<Overload&> lookupLeftBinaryOperator(ast::BinaryOperator op);
  Maybe<Overload&> lookupRightBinaryOperator(ast::BinaryOperator op);
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
  Maybe<Entity&> lookupMember(const string& name);
  Maybe<Overload&> getConstructor(const string& name);
  Overload& getImplicitConstructor();
};

class Enum: public Type {
public:
};

class Interface: public Type {
public:
};

class Scope {
  // Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
  // their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

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

class Context {
public:
  Thing import(const string& name);

  void error(errors::Error&& err);
  void error(const errors::Error& err);
};

vector<CxxStatement> compileImperative(Context& context, Scope& scope,
                                       const vector<ast::Statement>& statements);

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_COMPILER_H_ */
