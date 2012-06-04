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

enum class BuiltinType {
  BOOLEAN,
  INTEGER,
  DOUBLE

  // TODO:  fixed array?
};

class Type {
public:
  enum class Kind {
    BUILTIN,
    CLASS,
    ENUM,
    INTERFACE
  };

  Kind getKind();

  union {
    BuiltinType builtin;
    Class* class_;
    Enum* enum_;
    Interface* interface;
  };

  Maybe<Indirect<Thing>> outerObject;
  vector<Thing> paramValues;

  static Type fromBuiltinType(BuiltinType type);
};

class EntityContext {
public:
  // TODO:  Do these Things need to be Evaluations, or something that supports references?
  //   What about the case where a type parameter is an expression -- should that even be allowed?
  //   BoundExpression = Expression + Scope, maybe?
  Indirect<Thing> outerObject;
  vector<Thing> paramValues;

  // TODO:  Implicit references drawn from environment.
};

class Reference {
public:
  UNION_TYPE_BOILERPLATE(Reference);

  enum class Kind {
    NAMED,
    ANONYMOUS
  };

  Kind getKind();

  struct Anonymous {
    ValueDescriptor descriptor;
    CxxExpression cxxExpression;

    // Variables which must be locked if and when the reference is used.
    set<Variable*> entangledVariables;
  };

  union {
    Variable* named;
    Anonymous anonymous;
  };

  static Reference fromNamed(Variable* variable);
  static Reference fromAnonymous(ValueDescriptor&& descriptor, CxxExpression&& code,
                                 set<Variable*>&& entangledVariables);
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Kind {
    ERROR,

    // Compile-time known

    // Compile-time known values.
    BOOLEAN,
    INTEGER,
    DOUBLE,
    OBJECT,
    TUPLE,

    // Compile-time known types.
    TYPE,

    // Not compile-time known.
    // TODO:  Rename DYNAMIC
    RUNTIME_VALUE,

    META_CONSTANT  // A metaprogramming parameter that has not yet been bound, but will be a
                   // compile-time constant.
  };

  Kind getKind() { return kind; }

  struct Object {
    Class* type;
    Maybe<Indirect<Thing>> outerObject;
    vector<Thing> typeParams;
    map<Variable*, Thing> fields;
  };

  struct Tuple {
    vector<Thing> positionalElements;
    map<string, Thing> keywordElements;

    typedef map<string, Thing> KeywordElementMap;  // Macro can't handle comma.  Yay C++.
    VALUE_TYPE2(Tuple, vector<Thing>&&, positionalElements, KeywordElementMap&&, keywordElements);
  };

  struct RuntimeValue {
    ValueDescriptor descriptor;
    CxxExpression cxxExpression;
  };

  struct MetaConstant {
    // Type may or may not be known.
    Maybe<Indirect<Thing>> type;

    VALUE_TYPE1(MetaConstant, Maybe<Indirect<Thing>>&&, type);
  };

  union {
    bool boolean;
    int integer;
    double double_;
    Object object;
    Tuple tuple;

    Type type_;

    RuntimeValue runtimeValue;
    MetaConstant metaConstant;
  };

  static Thing fromError();

  static Thing fromBoolean(bool value);
  static Thing fromInteger(int value);
  static Thing fromDouble(double value);

  static Thing fromType(Type&& type);

  static Thing fromRuntimeValue(ValueDescriptor&& descriptor, CxxExpression&& cxxExpression);

private:
  Kind kind;
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

  void addSequential(Variable* variable, Style style, errors::Location location);
  void mergeSequential(const VariableUsageSet& other);
  void mergeParallel(const VariableUsageSet& other, Context* context);

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

class Evaluation {
public:
  UNION_TYPE_BOILERPLATE(Evaluation);

  enum Kind {
    REFERENCE,
    THING
  };

  Kind getKind();

  union {
    Reference reference;
    Thing thing;
  };

  VariableUsageSet variablesUsed;

  static Evaluation fromReference(Reference&& reference, VariableUsageSet&& variablesUsed);
  static Evaluation fromThing(Thing&& thing, VariableUsageSet&& variablesUsed);

  static Evaluation fromError();
};

// =======================================================================================

class Entity {
public:
  virtual ~Entity();
};

class Variable {
  // Variable keeps track of knowledge about a named value in the current scope.
public:
  virtual ~Variable();

  // interface
  virtual const ThingDescriptor& getDescriptor() = 0;
  virtual const vector<Thing>& getAnnotations() = 0;

  // style + style constraints
  // visibility

  // implementation

  bool assign(Thing&& value, VariableUsageSet& variablesUsed, errors::Location location);

  Thing read(VariableUsageSet& variablesUsed, errors::Location location);
  void entangle(VariableUsageSet::Style style, VariableUsageSet& variablesUsed,
                errors::Location location);

  Maybe<Entity&> getMember(const string& name);
};

class Function: public Entity {
public:
};

class Overload: public Entity {
public:
  Entity* resolve(const Thing::Tuple& parameters);
};

// TODO:  Rename BuiltinType
class BuiltinClass: public Entity {
public:
};

class Class: public Entity {
public:
  Maybe<Entity&> lookupMember(const string& name);
  Thing convertFrom(Thing::Tuple&& input);
  Maybe<Overload&> getConstructor(const string& name);
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
