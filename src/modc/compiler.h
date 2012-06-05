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
struct TupleElement;

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

class EntityContext {
public:
  UNION_TYPE_BOILERPLATE(EntityContext);

  enum class Kind {
    MEMBER,
    LOCAL
  };

  Kind getKind();

  union {
    Indirect<Value> outerObject;
    Scope* scope;
  };

  // Parameters, including:
  // - For a parameterized class, the dynamic type parameters.
  // - For a local class or function, the local parameters.
  //   TODO:  Should local parameters really be passed here, as opposed to being readable directly
  //   in the inner scope?
  vector<Value> params;

  static EntityContext forMember(Value&& outerObject);
  static EntityContext forLocal(Scope& scope);
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

    POINTER
  };

  Kind getKind() { return kind; }

  struct Object {
    Entity* type;
    EntityContext typeContext;
    map<Variable*, Value> fields;
  };

  struct Array {
    Entity* elementType;
    EntityContext elementTypeContext;
    vector<Value> elements;
  };

  struct Pointer {
    Entity* entity;
    EntityContext context;
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

  static Value fromReference(Entity* entity, EntityContext&& context);

private:
  Kind kind;
};

class BoundExpression {
public:
  UNION_TYPE_BOILERPLATE(BoundExpression);

  enum class Kind {
    CONSTANT,

    BINARY_OPERATOR,
    PREFIX_OPERATOR,
    POSTFIX_OPERATOR,
    TERNARY_OPERATOR,

    FUNCTION_CALL,
    SUBSCRIPT,
    MEMBER_ACCESS,

    READ_POINTER

    // cast?
  };

  Kind getKind() const { return kind; }

  struct BinaryOperator {
    ast::BinaryOperator op;
    Indirect<Value> left;
    Indirect<Value> right;

    VALUE_TYPE3(BinaryOperator, ast::BinaryOperator, op, Value&&, left, Value&&, right);
  };

  struct PrefixOperator {
    ast::PrefixOperator op;
    Indirect<Value> operand;

    VALUE_TYPE2(PrefixOperator, ast::PrefixOperator, op, Value&&, operand);
  };

  struct PostfixOperator {
    Indirect<Value> operand;
    ast::PostfixOperator op;

    VALUE_TYPE2(PostfixOperator, Value&&, operand, ast::PostfixOperator, op);
  };

  struct TernaryOperator {
    Indirect<Value> condition;
    Indirect<Value> trueClause;
    Indirect<Value> falseClause;

    VALUE_TYPE3(TernaryOperator, Value&&, condition, Value&&, trueClause, Value&&, falseClause);
  };

  struct FunctionCall {
    Indirect<Value> function;
    vector<BoundExpression> parameters;

    VALUE_TYPE2(FunctionCall, Value&&, function, vector<BoundExpression>&&, parameters);
  };

  struct Subscript {
    Indirect<Value> container;
    Indirect<Value> subscript;

    VALUE_TYPE2(Subscript, Value&&, container, Value&&, subscript);
  };

  struct MemberAccess {
    Indirect<Value> object;
    Entity* member;
    ast::StyleAllowance thisStyleAllowance;

    VALUE_TYPE3(MemberAccess, Value&&, object, Entity*, member,
                ast::StyleAllowance, thisStyleAllowance);
  };

  union {
    Value constant;

    BinaryOperator binaryOperator;
    PrefixOperator prefixOperator;
    PostfixOperator postfixOperator;
    TernaryOperator ternaryOperator;

    FunctionCall functionCall;
    Subscript subscript;
    MemberAccess memberAccess;

    Indirect<BoundExpression> readPointer;
  };

  static BoundExpression fromBinaryOperator(ast::BinaryOperator op, Value&& left, Value&& right);
  static BoundExpression fromPrefixOperator(ast::PrefixOperator op, Value&& exp);
  static BoundExpression fromPostfixOperator(Value&& exp, ast::PostfixOperator op);
  static BoundExpression fromTernaryOperator(Value&& condition, Value&& trueClause,
                                             Value&& falseClause);

  static BoundExpression fromFunctionCall(Value&& function, vector<BoundExpression>&& parameters);
  static BoundExpression fromSubscript(Value&& container, Value&& key);
  static BoundExpression fromMemberAccess(Value&& object, string&& member,
                                          ast::StyleAllowance thisStyleAllowance);

private:
  Kind kind;

  BoundExpression(Kind kind): kind(kind) {}
};

// =======================================================================================

struct EntityName {
  const string& name;
  Maybe<vector<Thing>> parameters;
};

class TypeDescriptor {
public:
  vector<Thing> supertypes;
  vector<Thing> subtypes;
};

class ValueDescriptor {
public:
  Indirect<Thing> type;

  // Variables which the value aliases.
  enum class AliasType {
    IDENTITY,
    IMMUTABLE,
    MUTABLE
  };
  map<Variable*, AliasType> aliases;

  // TODO:  Constraints, e.g. integer range
};

class ThingDescriptor {
public:
  enum class Kind {
    VALUE,
    TYPE,
    FUNCTION
  };

  union {
    ValueDescriptor valueDescriptor;
    TypeDescriptor typeDescriptor;
  };
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Kind {
    CONSTANT,
    TUPLE,
    DYNAMIC,
    ENTITY
  };

  Kind getKind() { return kind; }

  struct Dynamic {
    ValueDescriptor descriptor;
    BoundExpression expression;

    // TODO:  Aliases
    // TODO:  Partial knowledge, e.g. some members of an object or elements of an array.

    // Only if the expression is a pointer.
    // Variables which must be marked mutated if/when the references is used mutably.  These
    // are accumulated when calling functions that return entangled references.
    set<Variable*> entangledVariables;
  };

  struct BoundEntity {
    Entity* entity;
    EntityContext context;
  };

  struct MetaConstant {
    // Type may or may not be known.
    Maybe<ValueDescriptor> descriptor;

    VALUE_TYPE1(MetaConstant, Maybe<ValueDescriptor>&&, descriptor);
  };

  union {
    Value constant;
    vector<TupleElement> tuple;
    Dynamic dynamic;
    BoundEntity entity;
  };

  static Thing fromConstant(Value&& value);
  static Thing fromDynamic(ValueDescriptor&& descriptor, BoundExpression&& expression);

private:
  Kind kind;
};

struct TupleElement {
  Maybe<EntityName> name;
  ast::StyleAllowance styleAllowance;
  Thing value;

  VALUE_TYPE3(TupleElement, Maybe<EntityName>&&, name, ast::StyleAllowance, styleAllowance,
              Thing&&, value);
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

  // TODO:  Does this need EntityContext?  We do need to distinguish between the same member of
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
  virtual Scope& getScope() = 0;
  virtual Entity& getParent() = 0;

  virtual const ThingDescriptor& getDescriptor() = 0;
  virtual const vector<Thing>& getAnnotations() = 0;

  // TODO:  style + style constraints
  // TODO:  visibility

  // Read the entity.  If not known at compile time, returns a Thing of kind
  // DYNAMIC_{REFERENCE,VALUE} with code that simply reads the variable.
  // Recursively dereferences outer objects in the context.
  virtual Thing dereference(EntityContext&& context, VariableUsageSet& variablesUsed,
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

  // TODO:  May need EntityContext if VariableUsageSet does.
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

  // context.params must be filled in to match the Variables and Aliases in getParameters() --
  // but not the Constants.
  // Returns a FUNCTION_CALL BoundExpression if the call cannot be computed due to inputs being
  // dynamic or if the function implementation is unavailable.
  // TODO:  If a dynamic input is ignored but the computation had side effects we need to make sure
  //   those side effects still take place...
  Thing call(EntityContext&& context);

  // TODO:  Implicit parameters, environment.
  //   Both may depend on compiling the function body.
};

class Overload: public Entity {
public:
  // TODO:  Do we need a context to resolve?  I don't think so since no execution occurs here.
  Maybe<Entity&> resolve(const vector<TupleElement>& parameters);
};

class Type: public Entity {
public:
  Maybe<Entity&> lookupMemberOfInstance(const string& name);
};

class BuiltinType: public Entity {
public:
  enum class Type {
    BOOLEAN,
    INTEGER,
    DOUBLE
  };

  Type getType();
};

class Class: public Entity {
public:
  Maybe<Entity&> lookupMember(const string& name);
  Maybe<Overload&> getConstructor(const string& name);
  Overload& getImplicitConstructor();
};

class Enum: public Entity {
public:
};

class Interface: public Entity {
public:
};

class Scope {
  // Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
  // their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  // For EXPRESSION

  Thing lookupBuiltinType(BuiltinType::Type type);

  Maybe<Entity&> lookupBinding(const string& name);
  // Note that this may instantiate a template.
  // Note also that this returns a proxy specific to the current scope.

  // For DECLARATION

  Variable& declareVariable(const string& name, Thing&& value);

  // Code will be compiled on-demand.
  void declareClass(const string& name, ClassDescriptor&& descirptor,
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
