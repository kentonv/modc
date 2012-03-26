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

#ifndef KENTONSCODE_MODC_AST_H_
#define KENTONSCODE_MODC_AST_H_

#include <map>
#include <string>
#include <vector>

#include "base/OwnedPtr.h"

namespace modc {

using namespace ekam;
using std::string;

template <typename T> class Array;
template <typename T> class Maybe;

namespace tokens {
  class Tree;
}

class CodePrinter;

class Type;
class Value;
class Function;

class Descriptor;
class Visibility;
class UnboundName;

class Block;
class Statement;
class Expression;

class Scope;

class Value {
public:
};

class Type {
public:

};

class Function {
public:

};

enum class Kind {
  VARIABLE,
  FUNCTION,
  TYPE,
  ERROR
};

class UnboundName {
public:
  bool relative;
  std::vector<string> parts;
};

class Visibility {
public:
};

class ValueDescriptor {
public:
  OwnedPtr<Type> type;

  enum class Style {
    VALUE,
    IMMUTABLE_REFERENCE,
    MUTABLE_REFERENCE,
    ENTANGLED_REFERENCE
  };

  Style style;

  // TODO:  lifecycle
  // TODO:  range
  // TODO:  stage

  // TODO:  constant value?  Do we put that here or in Binding?
};

class TypeDescriptor {
public:
  OwnedPtr<Type> supertype;

  OwnedPtrMap<string, Descriptor> members;
};

class FunctionDescriptor {
public:
  class Parameter {
  public:
    string name;
    bool isType;
    union {
      ValueDescriptor value;
      TypeDescriptor type;
    };
  };
  OwnedPtrVector<Parameter> parameters;

  bool returnsType;
  union {
    ValueDescriptor returnValue;
    TypeDescriptor returnType;
  };
};

class Descriptor {
public:
  Descriptor() = delete;
  Descriptor(Kind kind);
  Descriptor(ValueDescriptor&& valueDescriptor);
  Descriptor(TypeDescriptor&& typeDescriptor);
  Descriptor(FunctionDescriptor&& functionDescriptor);
  Descriptor(Descriptor&& other);
  ~Descriptor();

  const Kind kind;

  union {
    ValueDescriptor value;
    TypeDescriptor type;
    FunctionDescriptor funciton;
  };
};

extern const Descriptor errorDescriptor;

enum class ScopeType {
  CLASS,
  FUNCTION
};

class Binding {
public:
  virtual const Descriptor& descriptor() const = 0;

  virtual const Scope& scope() const = 0;

  inline bool operator==(const Binding& other) const { return this == &other; }
  inline bool operator!=(const Binding& other) const { return this != &other; }
};

class Scope {
public:
  const Binding& lookupBinding(const string& name) const;

  Binding& declareValueBinding(const string& name, ValueDescriptor&& descriptor);

  void declareBinding(const string& name, Descriptor&& descriptor);

  // Return false if already initialized, in which case the statement should probably be considered
  // an assignment instead of an initialization.
  bool initializeBinding(const string& name);

  // Code will be compiled on-demand.
  void declareClass(const string& name, const Block& code);

  // Code will be compiled on-demand.
  void declareFunction(const string& name, Function&& function, const Block& code);

private:
  const Scope& parent;

  OwnedPtrMap<string, Binding> bindings;
  std::map<string, OwnedPtrVector<Function> > functions;
};

class CompiledStatement {
public:
  virtual void toCpp(CodePrinter& printer) const = 0;
};

class CompiledExpression {
public:
  virtual const Descriptor& descriptor() const = 0;
  virtual void toCpp(CodePrinter& printer) const = 0;
};

class Statement {
public:
  virtual ~Statement();

  virtual void toCode(CodePrinter& printer) const = 0;
  virtual OwnedPtr<CompiledStatement> bind(Scope& scope) const = 0;
};

class Expression {
public:
  virtual ~Expression();

  virtual void toCode(CodePrinter& printer) const = 0;
  virtual OwnedPtr<CompiledExpression> bind(Scope& scope) const = 0;
};

}  // namespace modc

#endif /* KENTONSCODE_MODC_AST_H_ */
