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
class Scope;
class Binding;
class Class;
class Interface;
class Field;
class Enum;
class Function;

// Value types.
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

enum class BuiltinType {
  BOOLEAN,
  INTEGER,
  DOUBLE

  // TODO:  fixed array?
};

class ValueDescriptor {
public:
  // TODO:  type, supertypes, subtypes, annotations, constraints, outstanding binding locks

  Indirect<Thing> type;

  // Aliases which the value contains.
  enum class AliasType {
    IDENTITY,
    IMMUTABLE,
    MUTABLE
  };
  map<Binding*, AliasType> identityAliases;

  // TODO:  Constraints.
};

class TypeDescriptor {
public:
  vector<Thing> supertypes;
  vector<Thing> subtypes;
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Kind {
    ERROR,

    // Compile-time known

    // Reference to known binding, which itself may have constant value.
    BINDING,

    // Compile-time known values.
    BOOLEAN,
    INTEGER,
    DOUBLE,
    OBJECT,
    TUPLE,

    // Compile-time known types.
    BUILTIN_TYPE,
    CLASS,
    INTERFACE,
    ENUM,

    // Not compile-time known.
    RUNTIME_LVALUE,
    RUNTIME_RVALUE,

    META_CONSTANT  // A metaprogramming parameter that has not yet been bound, but will be a
                   // compile-time constant.
  };

  Kind getKind() { return type; }

  struct Object {
    Class* type;
    map<Field*, Thing> fields;
  };

  struct Tuple {
    vector<Thing> positionalElements;
    map<string, Thing> keywordElements;

    typedef map<string, Thing> KeywordElementMap;  // Macro can't handle comma.  Yay C++.
    VALUE_TYPE2(Tuple, vector<Thing>&&, positionalElements, KeywordElementMap&&, keywordElements);
  };

  struct RuntimeLvalue {
    ValueDescriptor descriptor;
    CxxExpression cxxExpression;

    // Bindings which must be locked if and when the lvalue is used.
    set<Binding*> entangledBindings;
  };

  struct RuntimeRvalue {
    ValueDescriptor descriptor;
    CxxExpression cxxExpression;
  };

  struct MetaConstant {
    // Type may or may not be known.
    Maybe<Indirect<Thing>> type;

    VALUE_TYPE1(MetaConstant, Maybe<Indirect<Thing>>&&, type);
  };

  union {
    Binding* binding;

    bool boolean;
    int integer;
    double double_;
    Object object;
    Tuple tuple;

    // TODO:  Types and functions may need to be bound to type parameters.
    Function* function;

    BuiltinType builtinType;
    Class* class_;
    Interface* interface;
    Enum* enum_;

    RuntimeLvalue runtimeLvalue;
    RuntimeRvalue runtimeRvalue;
    MetaConstant metaConstant;
  };

  static Thing fromError();

  static Thing fromBoolean(bool value);
  static Thing fromInteger(int value);
  static Thing fromDouble(double value);

  static Thing fromBuiltinType(BuiltinType type);

  static Thing fromRuntimeRvalue(ValueDescriptor&& descriptor, CxxExpression&& cxxExpression);

private:
  // For the sake of the union macros, the discriminating enum must be called Type, but that is
  // confusing in this context, so we make this a private detail and use "kind" publicly.
  typedef Kind Type;
  Type type;
};

class Evaluation {
public:
  Thing result;

  // Bitfield.
  enum BindingUsage {
    MUTABLY = 1,
    IMMUTABLY = 2,
    MEMBER_MUTABLY = 4,
    MEMBER_IMMUTABLY = 8
  };
  map<Binding*, int> bindingsUsed;

  explicit Evaluation(Thing&& result);
  Evaluation(Thing&& result, Binding* binding, BindingUsage usage);
  Evaluation(Thing&& result, std::initializer_list<const Evaluation*> inputs);

  void mergeBindingsUsedFrom(const Evaluation& other);
};

// =======================================================================================

class Function {
public:
};

class Field {
public:
};

class Class {
public:
};

class Enum {
public:
};

class Interface {
public:
};

class BindingDescriptor {
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
  vector<Thing> annotations;
};

class Binding {
  // Binding keeps track of knowledge about a named thing in the current scope.
public:
  virtual ~Binding();

  // interface
  // TODO:  Lazy init?
  const ThingDescriptor& getDescriptor();
  // style + style constraints
  // visibility

  // implementation
  // VARIABLE:  value if known
  // FUNCTION, CLASS, INTERFACE, ENUM:  lazy ref to body


  // For VARIABLE

  virtual bool assign(Thing&& value) = 0;

  class ImmutableHandle {
  public:
    virtual ~ImmutableHandle();
    virtual Thing get() = 0;
  };
  virtual OwnedPtr<ImmutableHandle> useImmutably(errors::Location location) = 0;

  class MutableHandle {
  public:
    virtual ~MutableHandle();
  };
  virtual OwnedPtr<MutableHandle> useMutably(errors::Location location) = 0;

  // For VARIABLE or TYPE

  class MemberHandle {
  public:
    RESOURCE_TYPE_BOILERPLATE(MemberHandle);

    MemberHandle(Binding& bindind): binding(binding) {}
    ~MemberHandle();

    Binding& binding;
  };
  virtual OwnedPtr<MemberHandle> useMember(errors::Location location) = 0;

  // For TYPE

  virtual Thing convertFrom(Thing::Tuple&& input) = 0;

  // For FUNCTION

  class OverloadHandle {
  public:
    RESOURCE_TYPE_BOILERPLATE(OverloadHandle);

    OverloadHandle(Binding& bindind): binding(binding) {}
    ~OverloadHandle();

    Binding& binding;
  };
  virtual OwnedPtr<OverloadHandle> useOverload(const Thing::Tuple& parameters) = 0;
};

class Scope {
  // Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
  // their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  // For EXPRESSION

  Maybe<Binding&> lookupBinding(const string& name);
  // Note that this may instantiate a template.
  // Note also that this returns a proxy specific to the current scope.

  // For DECLARATION

  Binding& declareValueBinding(const string& name, Thing&& value);
  void declareBinding(const string& name, Type&& type);

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

  OwnedPtrMap<string, Binding> bindings;
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
