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

#include <stdexcept>

#include "compiler.h"
#include "tokens.h"
#include "CodePrinter.h"

namespace modc {
namespace expressions {

using std::move;

class Variable : public Expression {
public:
  Variable(string name): name(move(name)) {}
  ~Variable() {}

  class Compiled : public CompiledExpression {
  public:
    Compiled(string name, const Binding& binding)
        : name(move(name)), binding(binding) {}

    const Descriptor& descriptor() const {
      return binding.descriptor();
    }

    void toCpp(CodePrinter& printer) const {
      // TODO:
      // - Module members must be accessed via global module pointer.
      // - Outer class members must be accessed via outer class pointer.
      // - Need to avoid accidental superclass references.

      printer.write(name);
    }

  private:
    string name;
    const Binding& binding;
  };

  void toCode(CodePrinter& printer) const {
    printer.write(name);
  }

  OwnedPtr<CompiledExpression> bind(Scope& scope) const {
    return newOwned<Compiled>(name, scope.lookupBinding(name));
  }

private:
  string name;
};

extern const Descriptor integerDescriptor;

class IntegerLiteral : public Expression {
public:
  IntegerLiteral(int value, const string& original): value(value), original(original) {}
  ~IntegerLiteral() {}

  class Compiled : public CompiledExpression {
  public:
    Compiled(int value, const string& original): value(value), original(original) {}

    const Descriptor& descriptor() const {
      return integerDescriptor;
    }

    void toCpp(CodePrinter& printer) const {
      printer.write(original);
    }

  private:
    int value;
    string original;
  };

  void toCode(CodePrinter& printer) const {
    printer.write(original);
  }

  OwnedPtr<CompiledExpression> bind(Scope& scope) const {
    return newOwned<Compiled>(value, original);
  }

private:
  int value;
  string original;
};

class MemberAccess : public Expression {
public:
  MemberAccess(OwnedPtr<Expression> object, string memberName)
      : object(object.release()), memberName(memberName) {}
  ~MemberAccess() {}

  class Compiled : public CompiledExpression {
  public:
    Compiled(OwnedPtr<CompiledExpression> object, string memberName)
        : object(object.release()), memberName(memberName) {}

    const Descriptor& descriptor() const {
      return integerDescriptor;
    }

    void toCpp(CodePrinter& printer) const {
      // TODO:  Detect when we'd generate (*foo).bar and generate foo->bar instead.
      object->toCpp(printer);
      printer.writePrefix(".");
      printer.write(memberName);
    }

  private:
    OwnedPtr<CompiledExpression> object;
    string memberName;
  };

  void toCode(CodePrinter& printer) const {
    object->toCode(printer);
    printer.writePrefix(".");
    printer.write(memberName);
  }

  OwnedPtr<CompiledExpression> bind(Scope& scope) const {
    return newOwned<Compiled>(object->bind(scope), memberName);
  }

private:
  OwnedPtr<Expression> object;
  string memberName;
};

class FunctionCall : public Expression {
public:
  FunctionCall(OwnedPtr<Expression> function, std::vector<OwnedPtr<Expression>> parameters)
      : function(function.release()), parameters(move(parameters)) {}
  ~FunctionCall() {}

  class Compiled : public CompiledExpression {
  public:
    Compiled(OwnedPtr<CompiledExpression> function,
             std::vector<OwnedPtr<CompiledExpression>> parameters)
        : function(function.release()), parameters(move(parameters)) {}

    const Descriptor& descriptor() const {
      return integerDescriptor;
    }

    void toCpp(CodePrinter& printer) const {
      function->toCpp(printer);

      printer.writeSuffix("(");

      bool isFirst = true;
      for (const auto& parameter : parameters) {
        if (isFirst) {
          isFirst = false;
        } else {
          printer.writeSuffix(", ");
        }

        parameter->toCpp(printer);
      }

      printer.writeSuffix(")");
    }

  private:
    OwnedPtr<CompiledExpression> function;
    std::vector<OwnedPtr<CompiledExpression>> parameters;
  };

  void toCode(CodePrinter& printer) const {
    function->toCode(printer);

    printer.write("(");

    bool isFirst = true;
    for (const auto& parameter : parameters) {
      if (isFirst) {
        isFirst = false;
      } else {
        printer.write(", ");
      }

      parameter->toCode(printer);
    }

    printer.write(")");
  }

  OwnedPtr<CompiledExpression> bind(Scope& scope) const {
    std::vector<OwnedPtr<CompiledExpression>> compiledParameters;
    for (const auto& parameter : parameters) {
      compiledParameters.push_back(parameter->bind(scope));
    }

    return newOwned<Compiled>(move(function->bind(scope)), move(compiledParameters));
  }

private:
  OwnedPtr<Expression> function;
  std::vector<OwnedPtr<Expression>> parameters;
};

}  // namespace expressions
}  // namespace modc
