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

#include "thing.h"
#include "entity.h"

namespace modc {
namespace compiler {

DataConstraints DataConstraints::port(const Context& from) const {
  AdditionalTargets newAdditionalPointers = additionalPointers;

  vector<PossiblePointer> newPossiblePointers;
  newPossiblePointers.reserve(possiblePointers.size());
  for (auto& ptr: possiblePointers) {
    Maybe<Context::Size> pos = ptr.target.root->getContextPosition();
    if (pos == nullptr) {
      newAdditionalPointers = AdditionalTargets::FROM_CALLER;
      continue;
    }

    const Context::Binding& substitution = from[*pos];
    switch (substitution.getKind()) {
      case Context::Binding::Kind::DATA:
        // The context variable is a constant.  A pointer pointing to a constant cannot be
        // exclusive.
        assert(ptr.exclusivity <= Exclusivity::SHARED);

        // The pointer actually points to a temporary created by the caller.
        newAdditionalPointers = AdditionalTargets::FROM_CALLER;
        continue;

      case Context::Binding::Kind::POINTER: {
        LocalVariablePath newTarget(substitution.pointer);  // intentional copy
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

  // TODO:  intRanges
  assert(intRanges.empty());

  return DataConstraints(move(newPossiblePointers), newAdditionalPointers);
}

Maybe<DataConstraints> DataConstraints::port(
    const Context& from, const DataValue& additionalValue) const {
#error "TODO"
}

ConstrainedType ConstrainedType::port(const Context& from) const {
  return ConstrainedType(
      type.port(from),
      constraints.transform([&](const DataConstraints& input) { return input.port(from); }));
}

Maybe<ConstrainedType> ConstrainedType::port(
    const Context& from, const DataValue& additionalValue) const {
  Maybe<Bound<Type>> newType = type.port(from, additionalValue);
  Maybe<Maybe<DataConstraints>> newConstraints =
      constraints.transform([&](const DataConstraints& input) {
        return input.port(from, additionalValue);
      });

  if (newType == nullptr || newConstraints == nullptr) {
    return nullptr;
  } else {
    return ConstrainedType(move(*newType), move(*newConstraints));
  }
}

}  // namespace compiler
}  // namespace modc
