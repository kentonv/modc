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

#include "context.h"
#include "entity.h"

namespace modc {
namespace compiler {

Context Context::port(const Context& base) const {
  return port(base, depth);
}

Maybe<Context> Context::port(const Context& base, const DataValue& additionalValue) const {
  // TODO:  I think we need to separate unknown-due-to-error and unknown-because-its-dynamic.
  //   The former can appear anywhere whereas the latter can only appear in the "partial" values
  //   in Described{Data,Pointer}.
#error "TODO"
}

Context Context::port(const Context& base, vector<Binding>::size_type upToDepth) const {
  if (upToDepth == 0) {
    return Context(vector<Binding>());
  }

  if (overlayStart() >= upToDepth) {
    // Requested content is entirely in underlay.
    return underlay->port(base, upToDepth);
  }

  for (const Context* baseUnderlay = &base; ; baseUnderlay = baseUnderlay->underlay) {
    if (baseUnderlay->overlayStart() < upToDepth) {
      if (baseUnderlay == this) {
        // Remainder of context is shared.  Presume that it is self-referential.
        return Context(upToDepth, vector<Binding>(), *this);
      }
    }
  }

  return port2(base, upToDepth - overlayStart());
}

Context Context::port2(const Context& base, vector<Binding>::size_type upToSuffixIndex) const {
  Context result = underlay->port(base, overlayStart());

  for (vector<Binding>::size_type i = 0; i < upToSuffixIndex; i++) {
    const Binding& binding = suffix[i];
    switch (binding.getKind()) {
      case Binding::Kind::DATA:
        // Nothing to port.
        result.suffix.push_back(binding);
        break;
      case Binding::Kind::POINTER: {
        Maybe<int> pos = binding.pointer.root->getContextPosition();
        if (pos == nullptr) {
          // Must be a local variable.
          throw "Can't port local variable.";
        }

        const Binding& substitution = base[*pos];
        switch (substitution.getKind()) {
          case Binding::Kind::DATA: {
            Maybe<const DataValue&> newValue = binding.pointer.member.readFrom(substitution.data);
            if (newValue == nullptr) {
              result.suffix.push_back(Binding::fromData(DataValue::fromUnknown()));
            } else {
              result.suffix.push_back(Binding::fromData(DataValue(*newValue)));
            }
            break;
          }
          case Binding::Kind::POINTER: {
            LocalVariablePath newPath = substitution.pointer;
            newPath.member.append(binding.pointer.member);
            result.suffix.push_back(Binding::fromPointer(move(newPath)));
            break;
          }
        }
        break;
      }
    }
  }

  return result;
}

}  // namespace compiler
}  // namespace modc
