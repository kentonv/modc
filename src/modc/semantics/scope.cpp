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

#include "scope.h"

namespace modc {
namespace compiler {

class Entity;

// Get the local variable's current descriptor including transient constraints.
DataDescriptor Scope::getVariableDescriptor(DataVariable* variable) {

}

PointerDescriptor Scope::getVariableDescriptor(PointerVariable* variable) {

}

void Scope::setVariableConstraints(DataVariable* variable, const DataConstraints& constraints) {

}
void Scope::setVariableConstraints(
    PointerVariable* variable, const PointerConstraints& ptrConstraints,
    const DataConstraints& dataConstraints) {

}


// =======================================================================================
// Sub-blocks.

OwnedPtr<Scope> Scope::startBlock() {

}
OwnedPtr<Scope> Scope::startLoop() {

}

OwnedPtr<Scope::Branch> Scope::startBranch() {

}

// =======================================================================================
// Other control flow.

void Scope::addBreak(Maybe<string> loopName) {

}
void Scope::addContinue(Maybe<string> loopName) {

}
void Scope::addReturn() {

}

}  // namespace compiler
}  // namespace modc
