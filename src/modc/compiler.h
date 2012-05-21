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

class CodePrinter;

class Scope;
class Thing;
class ThingDescriptor;
class Binding;

class CxxCode {
public:
  virtual ~CxxCode();

  virtual void write(CodePrinter& printer) = 0;

  // TODO:  Collect forward declarations.
};

class Type {
public:
  Binding& binding;
  vector<Thing> params;
};

class Function {
public:
};

class FunctionDescriptor {
public:
};

class Value {
public:
  UNION_TYPE_BOILERPLATE(Value);

  enum class Type {
    BOOLEAN,
    INTEGER,
    DOUBLE,
    OBJECT,
    TUPLE
  };

  struct Object {
    Type type;
    map<string, Thing> members;
  };

  struct Tuple {
    vector<Thing> positionalElements;
    map<string, Thing> keywordElements;

    typedef map<string, Thing> KeywordElementMap;  // Macro can't handle comma.  Yay C++.
    VALUE_TYPE2(Tuple, vector<Thing>&&, positionalElements, KeywordElementMap&&, keywordElements);
  };

  union {
    bool boolean;
    int integer;
    double double_;
    Object object;
    Tuple tuple;
  };

  static Value fromBoolean(bool b);
  static Value fromInteger(int b);
  static Value fromDouble(double b);

private:
  Value(Type type): type(type) {}

  Type type;
};

class Class {
public:
  map<string, ThingDescriptor> members;
};

class ClassDescriptor {
public:
  // TODO:  supertypes, subtypes
};

class Enum {
public:
};

class Interface {
public:
};

class Thing {
public:
  UNION_TYPE_BOILERPLATE(Thing);

  enum class Type {
    ERROR,

    VALUE,
    FUNCTION,  // i.e., overloaded name

    CLASS,
    INTERFACE,
    ENUM,

    CXX_EXPRESSION,
    META_CONSTANT  // A metaprogramming parameter that has not yet been bound, but will be a
                   // compile-time constant.
  };

  Type getType();

  struct MetaConstant {
    VALUE_TYPE0(MetaConstant);
  };

  union {
    Value value;
    Function function;

    Class class_;
    Interface interface;
    Enum enum_;

    OwnedPtr<CxxCode> code;
    MetaConstant metaConstant;
  };

  static Thing fromError();

  static Thing fromValue(Value&& value);

  static Thing fromCxxExpression(OwnedPtr<CxxCode> code);
};

class ThingDescriptor {
public:
  Thing::Type type;

  // TODO:  type, supertypes, subtypes, annotations, constraints
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



  virtual bool assign(Thing&& value) = 0;

  virtual Maybe<const Thing&> getValue() = 0;
  // Returns the binding's current assigned value, if it is known at compile time.

  virtual OwnedPtr<CxxCode> getReferenceCode() = 0;

  class MutableHandle;
  virtual OwnedPtr<MutableHandle> useMutably(errors::Location location) = 0;

  class ImmutableHandle;
  virtual OwnedPtr<ImmutableHandle> useImmutably(errors::Location location) = 0;

  class MemberHandle {
  public:
    RESOURCE_TYPE_BOILERPLATE(MemberHandle);

    MemberHandle(Binding& bindind): binding(binding) {}
    ~MemberHandle();

    Binding& binding;
  };
  virtual OwnedPtr<MemberHandle> useMember(errors::Location location) = 0;

  class OverloadHandle {
  public:
    RESOURCE_TYPE_BOILERPLATE(OverloadHandle);

    OverloadHandle(Binding& bindind): binding(binding) {}
    ~OverloadHandle();

    Binding& binding;
  };
  virtual OwnedPtr<OverloadHandle> useOverload(const vector<Thing>& parameters) = 0;
};

class Scope {
  // Scope keeps track of knowledge about the current scope, e.g. what variables are defined, what
  // their properties are, etc.  Scope does NOT keep track of code, only auxiliary knowledge.
public:
  RESOURCE_TYPE_BOILERPLATE(Scope);

  // For EXPRESSION
  // Note that this may instantiate a template.
  Maybe<Binding&> lookupBinding(const string& name);

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
  std::map<string, OwnedPtrVector<Function> > functions;
};

// =======================================================================================

class Context {
public:
  Thing import(const string& name);

  void error(errors::Error&& err);
  void error(const errors::Error& err);
};

OwnedPtr<CxxCode> compileImperative(Context& context, Scope& scope,
                                    const vector<ast::Statement>& statements);

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_COMPILER_H_ */
