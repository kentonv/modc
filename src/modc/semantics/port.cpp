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

#include "port.h"
#include "entity.h"

namespace modc {
namespace compiler {

Port::Port(Compiler& compiler, const Context& foreignContext)
    : compiler(compiler), foreignContext(foreignContext),
      additionalValueType(AdditionalValueType::NONE) {}

Port::Port(Compiler& compiler, const Context& foreignContext, DescribedRvalue& additionalValue)
    : compiler(compiler), foreignContext(foreignContext),
      additionalValueType(AdditionalValueType::RVALUE) {
  additionalRvalue = &additionalValue;
}
Port::Port(Compiler& compiler, const Context& foreignContext, DescribedData& additionalValue)
    : compiler(compiler), foreignContext(foreignContext),
      additionalValueType(AdditionalValueType::DATA) {
  additionalData = &additionalValue;
}
Port::Port(Compiler& compiler, const Context& foreignContext, DescribedPointer& additionalValue)
    : compiler(compiler), foreignContext(foreignContext),
      additionalValueType(AdditionalValueType::POINTER) {
  additionalPointer = &additionalValue;
}

Maybe<const Context::Binding&> Port::getBinding(size_t index) {
  if (index < foreignContext.unboundCount) {
    return nullptr;
  }

  index -= foreignContext.unboundCount;

  if (index == foreignContext.bindings.size()) {
    if (additionalBinding != nullptr) {
      return *additionalBinding;
    }

    switch (additionalValueType) {
      case AdditionalValueType::NONE:
        // assert below
        break;
      case AdditionalValueType::RVALUE:
        additionalBinding = Context::Binding::fromPointer(
            compiler.bindTemporary(*additionalRvalue));
        return *additionalBinding;
      case AdditionalValueType::DATA:
        additionalBinding = Context::Binding::fromPointer(
            compiler.bindTemporary(*additionalData));
        return *additionalBinding;
      case AdditionalValueType::POINTER:
        additionalBinding = Context::Binding::fromPointer(
            compiler.bindTemporary(*additionalPointer));
        return *additionalBinding;
    }
  }

  assert(index < foreignContext.bindings.size());
  return foreignContext.bindings[index];
}

Context Port::import(const Context& input) {
  vector<Context::Binding> newBindings;
  newBindings.reserve(input.bindings.size());

  for (auto& binding: input.bindings) {
    switch (binding.getKind()) {
      case Context::Binding::Kind::DATA:
        // Nothing to port.
        newBindings.push_back(binding);
        break;
      case Context::Binding::Kind::POINTER: {
        Maybe<size_t> pos = binding.pointer.root->getContextPosition();
        if (pos == nullptr) {
          // TODO:  We obviously should never encounter local variables when importing, but what
          //   about member variables?  Will they already have the parent object set as the root or
          //   do we need to do that?
          throw "Can't port variable reference not rooted in a context variable.";
        }

        Maybe<const Context::Binding&> substitution = getBinding(*pos);
        if (substitution == nullptr) {
          // This variable is not bound so the pointer remains unchanged.
          newBindings.push_back(binding);
        } else {
          switch (substitution->getKind()) {
            case Context::Binding::Kind::DATA: {
              Maybe<const DataValue&> newValue =
                  binding.pointer.member.readFrom(substitution->data);
              if (newValue == nullptr) {
                throw "Encountered incomplete value in context binding.";
              }
              newBindings.push_back(Context::Binding::fromData(DataValue(*newValue)));
              break;
            }
            case Context::Binding::Kind::POINTER: {
              LocalVariablePath newPath = substitution->pointer;
              newPath.member.append(binding.pointer.member);
              newBindings.push_back(Context::Binding::fromPointer(move(newPath)));
              break;
            }
          }
        }
        break;
      }
    }
  }

  return Context(input.unboundCount, move(newBindings));
}

DataConstraints Port::import(const DataConstraints& input) {
  vector<DataConstraints::PossiblePointer> newPossiblePointers;
  newPossiblePointers.reserve(input.possiblePointers.size());
  for (auto& ptr: input.possiblePointers) {
    Maybe<size_t> pos = ptr.target.root->getContextPosition();
    if (pos == nullptr) {
      // TODO:  We obviously should never encounter local variables when importing, but what about
      //   member variables?  Will they already have the parent object set as the root or do we
      //   need to do that?
      throw "Can't port variable reference not rooted in a context variable.";
    }

    Maybe<const Context::Binding&> substitution = getBinding(*pos);
    if (substitution == nullptr) {
      // This variable is not bound so the pointer remains unchanged.
      newPossiblePointers.push_back(ptr);
    } else {
      switch (substitution->getKind()) {
        case Context::Binding::Kind::DATA:
          throw "Pointers cannot be declared to point at non-pointer context variables.";

        case Context::Binding::Kind::POINTER: {
          LocalVariablePath newTarget = substitution->pointer;
          newTarget.member.append(ptr.target.member);
          newPossiblePointers.emplace_back(
              MemberPath(ptr.member),
              move(newTarget),
              ptr.exclusivity,
              ptr.targetSpecificity);
          break;
        }
      }
    }
  }

  // TODO:  intRanges
  assert(input.intRanges.empty());

  return DataConstraints(move(newPossiblePointers), input.additionalPointers);
}

ConstrainedType Port::import(const ConstrainedType& input) {
  return ConstrainedType(
      import(input.type),
      input.constraints.transform(
          [&](const DataConstraints& constraints) { return import(constraints); }));
}

}  // namespace compiler
}  // namespace modc
