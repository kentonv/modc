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

#ifndef KENTONSCODE_MODC_SEMANTICS_PORT_H_
#define KENTONSCODE_MODC_SEMANTICS_PORT_H_

#include "context.h"
#include "thing.h"

namespace modc {
namespace compiler {

class Compiler;

class Port {
public:
  Port(Compiler& compiler, const Context& foreignContext);

  // additionalValue is one more value that should be considered appended to the context, but which
  // as yet is not bound to a local variable.  If it turns out that the value needs to be converted
  // to an lvalue in order to complete some porting operation, compiler.bindTemporary() will be used
  // to bind it to a local variable, modifying additionalValue in-place.  This is useful when
  // porting descriptions of members of a temporary -- usually, said descriptions do not reference
  // the object instance, so usually it's not necessary to bind to a local, but occasionally it is.
  Port(Compiler& compiler, const Context& foreignContext, DescribedRvalue& additionalValue);
  Port(Compiler& compiler, const Context& foreignContext, DescribedData& additionalValue);
  Port(Compiler& compiler, const Context& foreignContext, DescribedPointer& additionalValue);

  Context import(const Context& input);
  template <typename T>
  Bound<T> import(const Bound<T>& input);
  DataConstraints import(const DataConstraints& input);
  PointerConstraints import(const PointerConstraints& input);
  ConstrainedType import(const ConstrainedType& input);

private:
  Compiler& compiler;

  const Context& foreignContext;
  Maybe<Context::Binding> additionalBinding;

  enum class AdditionalValueType {
    NONE,
    RVALUE,
    DATA,
    POINTER
  };

  AdditionalValueType additionalValueType;
  union {
    DescribedRvalue* additionalRvalue;
    DescribedData* additionalData;
    DescribedPointer* additionalPointer;
  };

  Maybe<const Context::Binding&> getBinding(size_t index);
  LocalVariablePath importPointerTarget(const LocalVariablePath& input);
};

template <typename T>
Bound<T> Port::import(const Bound<T>& input) {
  return Bound<T>(input.entity, import(input.context));
}

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_PORT_H_ */
