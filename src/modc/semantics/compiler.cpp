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
#include "entity.h"

namespace modc {
namespace compiler {

using std::move;
using ast::Expression;
using ast::Statement;

// ---------------------------------------------------------------------------------------
// getInheritedConstraints

DataConstraints Compiler::getInheritedConstraints(DataConstraints&& parentConstraints,
                                                  DataVariable* member) {
  vector<DataConstraints::PossiblePointer> possiblePointers;

  // The member does not declare what pointers it may contain, so inherit from the parent.
  for (auto& possiblePointer: parentConstraints.possiblePointers) {
    if (possiblePointer.member.path.empty()) {
      // It's unspecified which member of the parent could contain this pointer, so
      // assume this one could.
      possiblePointers.push_back(move(possiblePointer));
    } else if (possiblePointer.member.path.front() == member) {
      // The parent declared that this possible pointer specifically applies to this
      // member variable, or a further member thereof.  Chop off the first element in the
      // path to make the path relative to this member.
      possiblePointer.member.path.erase(possiblePointer.member.path.begin(),
                                        possiblePointer.member.path.begin() + 1);
      possiblePointers.push_back(move(possiblePointer));
    }
  }

  return DataConstraints(move(possiblePointers), parentConstraints.additionalPointers);
}

PointerConstraints Compiler::getInheritedConstraints(DataConstraints&& parentConstraints,
                                                     PointerVariable* member) {
  vector<PointerConstraints::PossibleTarget> possibleTargets;
  Exclusivity memberExclusivity = member->getExclusivity();

  for (auto& pointer: parentConstraints.possiblePointers) {
    // Does the possiblePointer apply to this member, and does this member match its
    // exclusivity limit?
    if ((pointer.member.path.empty() ||
         pointer.member.path.front() == member) &&
        memberExclusivity <= pointer.exclusivity) {
      assert(pointer.member.path.size() <= 1);
      possibleTargets.push_back(PointerConstraints::PossibleTarget(
          move(pointer.target), pointer.targetSpecificity));
    }
  }

  return PointerConstraints(move(possibleTargets), parentConstraints.additionalPointers);
}

DataConstraints Compiler::getDefaultConstraints(Type* type) {
  if (type->hasUnannotatedAliases()) {
    // The target type has aliases, but it was never declared what they might point at.  This
    // can only be the case for parameters since local variables would have had the
    // constraints inferred.  So, assume the pointers point at stuff in the caller's scope.
    return DataConstraints(
        vector<DataConstraints::PossiblePointer>(),
        AdditionalTargets::FROM_CALLER);
  } else {
    // The target contains no unannotated aliases.
    return DataConstraints(
        vector<DataConstraints::PossiblePointer>(),
        AdditionalTargets::NONE);
  }
}

// ---------------------------------------------------------------------------------------
// checkConstraints

bool Compiler::checkPointerPath(
    const LocalVariablePath& allowedPath, TargetSpecificity allowedSpecificity,
    const LocalVariablePath& actualPath, TargetSpecificity actualSpecificity) {
  switch (allowedSpecificity) {
    case TargetSpecificity::EXACT_TARGET:
      return actualSpecificity == TargetSpecificity::EXACT_TARGET && actualPath == allowedPath;

    case TargetSpecificity::TARGET_OR_MEMBER:
      if (!allowedPath.isPrefix(actualPath)) {
        // TODO:  If the pointer is non-exclusive, we can try denormalizing each of the
        //   allowed targets to see if they transitively cover it.
        return false;
      } else {
        return true;
      }
  }

  throw "can't get here";
}

void Compiler::checkConstraints(AdditionalTargets allowed, AdditionalTargets actual,
                                ErrorLocation location) {
  switch (allowed) {
    case AdditionalTargets::NONE:
      if (actual != AdditionalTargets::NONE) {
        location.error("Input may point at calling scope, but target type does not allow this.");
      }
      break;

    case AdditionalTargets::FROM_CALLER:
      // Nothing to check.
      break;
  }
}

void Compiler::checkConstraints(const DataConstraints& allowed, const DataConstraints& actual,
                                ErrorLocation location) {
  for (auto& actualPp: actual.possiblePointers) {
    bool foundMatch = false;
    for (auto& allowedPp: allowed.possiblePointers) {
      if (actualPp.exclusivity > allowedPp.exclusivity) {
        continue;
      }

      if (!allowedPp.member.isPrefix(actualPp.member)) {
        continue;
      }

      if (!checkPointerPath(allowedPp.target, allowedPp.targetSpecificity,
                            actualPp.target, actualPp.targetSpecificity)) {
        continue;
      }

      // Found a match!
      foundMatch = true;
      break;
    }

    if (!foundMatch) {
      location.error("Input may contain disallowed pointers.");
    }
  }

  checkConstraints(allowed.additionalPointers, actual.additionalPointers, location);

  // TODO:  Enforce integer ranges.
}

void Compiler::checkConstraints(const PointerConstraints& allowed, const PointerConstraints& actual,
                                ErrorLocation location) {
  for (auto& actualTarget: actual.possibleTargets) {
    bool foundMatch = false;
    for (auto& allowedTarget: allowed.possibleTargets) {
      if (!checkPointerPath(allowedTarget.path, allowedTarget.specificity,
                            actualTarget.path, actualTarget.specificity)) {
        continue;
      }

      foundMatch = true;
      break;
    }

    if (!foundMatch) {
      location.error("Input may point at disallowed targets.");
    }
  }

  checkConstraints(allowed.additionalTargets, actual.additionalTargets, location);
}

// ---------------------------------------------------------------------------------------
// applyDefaultConvertion

Maybe<DescribedRvalue> Compiler::applyDefaultConversion(
    DescribedRvalue&& input, VariableUsageSet& variablesUsed, ErrorLocation location) {
  Maybe<UnaryOperator&> conversion = input.dataDescriptor().type.entity->getDefaultConversion();
  if (conversion == nullptr) {
    return move(input);
  }

  return conversion->call(*this, move(input), variablesUsed, location);
}

Maybe<Thing> Compiler::applyDefaultConversion(
    Thing&& input, VariableUsageSet& variablesUsed, ErrorLocation location) {
  switch (input.getKind()) {
    case Thing::Kind::TYPE:
    case Thing::Kind::FUNCTION:
    case Thing::Kind::METHOD:
    case Thing::Kind::TUPLE:
      return move(input);

    case Thing::Kind::RVALUE:
      return Thing::from(applyDefaultConversion(move(input.rvalue), variablesUsed, location));

    case Thing::Kind::LVALUE: {
      Maybe<DescribedPointer> ptr = toPointer(move(input.lvalue), location);
      if (ptr == nullptr) {
        return nullptr;
      }
      return Thing::from(applyDefaultConversion(DescribedRvalue::from(move(*ptr)),
                                                variablesUsed, location));
    }
  }

  throw "can't get here";
}

// ---------------------------------------------------------------------------------------
// castTo

Maybe<DescribedRvalue> Compiler::castTo(
    DescribedRvalue&& input, Bound<Type>&& targetType,
    VariableUsageSet& variablesUsed, ErrorLocation location) {
  if (areEqual(input.dataDescriptor().type, targetType)) {
    return move(input);
  }

  // Look for conversions on the source type.
  Maybe<UnaryOperator&> conversion =
      input.dataDescriptor().type.entity->lookupConversion(*this, input, targetType);
  if (conversion != nullptr) {
    Maybe<DescribedRvalue> result = conversion->call(*this, move(input), variablesUsed, location);
    if (result == nullptr) {
      return nullptr;
    }
    switch (result->getKind()) {
      case DescribedRvalue::Kind::DATA:
        assert(areEqual(result->data.descriptor.type, targetType));
        break;
      case DescribedRvalue::Kind::POINTER:
        assert(areEqual(result->pointer.descriptor.targetDescriptor.type, targetType));
        break;
    }
    return move(result);
  }

  // Look for a constructor on the destination type.
  Overload* constructor = targetType.entity->getImplicitConstructor();

  Maybe<Thing> result = constructor->resolve(*this, move(targetType.context),
                                             Tuple::fromSingleValue(Thing::from(move(input))),
                                             location);

  if (result == nullptr) {
    return nullptr;
  } else {
    switch (result->getKind()) {
      case Thing::Kind::RVALUE:
        return move(result->rvalue);

      case Thing::Kind::TYPE:
      case Thing::Kind::FUNCTION:
      case Thing::Kind::METHOD:
      case Thing::Kind::LVALUE:
      case Thing::Kind::TUPLE:
        throw "Constructor did not return value.";
    }

    throw "can't get here";
  }
}

Maybe<DescribedRvalue> Compiler::castTo(
    Thing&& input, Bound<Type>&& targetType,
    VariableUsageSet& variablesUsed, ErrorLocation location) {
  switch (input.getKind()) {
    case Thing::Kind::TYPE:
    case Thing::Kind::FUNCTION:
    case Thing::Kind::METHOD:
      location.error("Not a value.");
      return nullptr;

    case Thing::Kind::RVALUE:
      return castTo(move(input.rvalue), move(targetType), variablesUsed, location);

    case Thing::Kind::LVALUE: {
      Maybe<DescribedPointer> ptr = toPointer(move(input.lvalue), location);
      if (ptr == nullptr) {
        return nullptr;
      } else {
        return castTo(DescribedRvalue::from(move(*ptr)),
                      move(targetType), variablesUsed, location);
      }
    }

    case Thing::Kind::TUPLE: {
      Overload* constructor = targetType.entity->getImplicitConstructor();
      Maybe<Thing> result = constructor->resolve(*this, move(targetType.context),
                                                 move(input.tuple), location);
      if (result == nullptr) {
        return nullptr;
      } else {
        switch (result->getKind()) {
          case Thing::Kind::RVALUE:
            return move(result->rvalue);

          case Thing::Kind::TYPE:
          case Thing::Kind::FUNCTION:
          case Thing::Kind::METHOD:
          case Thing::Kind::LVALUE:
          case Thing::Kind::TUPLE:
            throw "Constructor did not return value.";
        }
      }
    }
  }

  throw "can't get here";
}

Maybe<DescribedRvalue> Compiler::castTo(
    Thing&& input, DataDescriptor&& targetDescriptor,
    VariableUsageSet& variablesUsed, ErrorLocation location) {
  Maybe<DescribedRvalue> result =
      castTo(move(input), move(targetDescriptor.type), variablesUsed, location);

  if (result != nullptr) {
    switch (result->getKind()) {
      case DescribedRvalue::Kind::DATA:
        checkConstraints(targetDescriptor.constraints,
                         result->data.descriptor.constraints, location);
        break;
      case DescribedRvalue::Kind::POINTER:
        checkConstraints(targetDescriptor.constraints,
                         result->pointer.descriptor.targetDescriptor.constraints, location);
        break;
    }
  }

  return result;
}

// ---------------------------------------------------------------------------------------
// castToPointer

Maybe<DescribedPointer> Compiler::castToPointer(DescribedPointer&& input,
                                                Bound<Type>&& targetType,
                                                Exclusivity targetExclusivity,
                                                VariableUsageSet& variablesUsed,
                                                ErrorLocation location) {
  if (!areEqual(input.descriptor.targetDescriptor.type, targetType)) {
    if (dynamic_cast<Interface*>(targetType.entity) == nullptr) {
      location.error("Type mismatch.");
    } else {
      Maybe<ImplementedInterface&> interface =
          input.descriptor.targetDescriptor.type.entity->findImplementedInterface(
              *this, input, targetType);
      if (interface == nullptr) {
        location.error("Object does not implement desired interface.");
      } else {
        input.descriptor.targetDescriptor.type = targetType;
        input.expression = expressionBuilder.upcast(move(input.expression), &*interface);
        input.staticPointer = evaluator.upcast(move(input.staticPointer), &*interface);
      }
    }
  }

  if (input.descriptor.exclusivity >= targetExclusivity) {
    // We now know that the pointer is going to be used with this exclusivity, so mark it used.
    variablesUsed.addUsage(input.descriptor.constraints, targetExclusivity, location);
  } else {
    location.error("Pointer has insufficient exclusivity to be used here.");
  }

  return move(input);
}

Maybe<DescribedPointer> Compiler::castToPointer(Thing&& input, Bound<Type>&& targetType,
                                                Exclusivity targetExclusivity,
                                                VariableUsageSet& variablesUsed,
                                                ErrorLocation location) {
  switch (input.getKind()) {
    case Thing::Kind::TYPE:
    case Thing::Kind::FUNCTION:
    case Thing::Kind::METHOD:
    case Thing::Kind::TUPLE:
      // TODO:  If non-exclusive, try castTo() then take pointer-to-temporary?
      location.error("Not a pointer.");
      return nullptr;

    case Thing::Kind::RVALUE:
      switch (input.rvalue.getKind()) {
        case DescribedRvalue::Kind::DATA:
          // TODO:  If non-exclusive, try castTo() then take pointer-to-temporary?
          location.error("Not a pointer.");
          return nullptr;
        case DescribedRvalue::Kind::POINTER:
          return castToPointer(move(input.rvalue.pointer), move(targetType), targetExclusivity,
                               variablesUsed, location);
      }

    case Thing::Kind::LVALUE: {
      if (!input.lvalue.isPointer) {
        // TODO:  If non-exclusive, try castTo() then take pointer-to-temporary?
        location.error("Not a pointer.");
        return nullptr;
      }

      Maybe<DescribedPointer> ptr = toPointer(move(input.lvalue), location);
      if (ptr == nullptr) {
        return nullptr;
      } else {
        return castToPointer(move(*ptr), move(targetType), targetExclusivity,
                             variablesUsed, location);
      }
    }
  }

  throw "can't get here";
}

Maybe<DescribedPointer> Compiler::castToPointer(
    Thing&& input, PointerDescriptor&& targetDescriptor,
    VariableUsageSet& variablesUsed, ErrorLocation location) {
  Maybe<DescribedPointer> result =
      castToPointer(move(input), move(targetDescriptor.targetDescriptor.type),
                    targetDescriptor.exclusivity, variablesUsed, location);

  if (result != nullptr) {
    // Check pointer constraints.
    checkConstraints(targetDescriptor.constraints, result->descriptor.constraints, location);

    // Check target value constraints.
    checkConstraints(targetDescriptor.targetDescriptor.constraints,
                     result->descriptor.targetDescriptor.constraints, location);

    if (result->descriptor.exclusivity >= Exclusivity::EXCLUSIVE) {
      // We need to ensure that any modifications made through the new pointer are compatible
      // with the original constraints, or we must adjust the original constraints to allow for
      // any possible modifications.  We can only adjust the original constraints if this is
      // a local variable with undeclared constraints.

      // TODO:  Implement widening of constraints.

      // For now, just check that the constraints apply in reverse.
      checkConstraints(result->descriptor.targetDescriptor.constraints,
                       targetDescriptor.targetDescriptor.constraints, location);
    }

    result->descriptor.constraints = move(targetDescriptor.constraints);
  }

  return result;
}

// ---------------------------------------------------------------------------------------
// toPointer

Maybe<DescribedPointer> Compiler::toPointer(
    DataVariable* localVariable, ErrorLocation location) {
  // Read the variable.
  DataDescriptor descriptor = scope.getVariableDescriptor(localVariable);

  // Form a pointer to the local variable.
  PointerConstraints constraints(
      PointerConstraints::PossibleTarget(
          LocalVariablePath(localVariable),
          TargetSpecificity::EXACT_TARGET));
  return DescribedPointer(
      PointerDescriptor(move(descriptor), Exclusivity::OWNED, move(constraints)),
      expressionBuilder.makePointerToLocalVariable(localVariable),
      evaluator.makePointerToLocalVariable(localVariable));
}

Maybe<DescribedPointer> Compiler::toPointer(
    PointerVariable* localVariable, ErrorLocation location) {
  PointerDescriptor descriptor = scope.getVariableDescriptor(localVariable);

  return DescribedPointer(move(descriptor),
      expressionBuilder.readLocalVariable(localVariable),
      evaluator.readLocalVariable(localVariable));
}

Maybe<DescribedPointer> Compiler::toPointer(
    DescribedPointer&& parent, DataVariable* member, ErrorLocation location) {
  Maybe<Thing::ConstrainedType> type = getMemberType(parent, member, location);
  if (type == nullptr) {
    return nullptr;
  }

  PointerDescriptor& parentDesc = parent.descriptor;

  if (parentDesc.exclusivity <= Exclusivity::IDENTITY) {
    // TODO:  Allow reading of constant members of identity pointers.
    location.error("Cannot access member of identity alias.");
    // We can keep going here rather than return null, since we know what the code meant.
  }

  if (type->constraints == nullptr) {
    // The member does not declare what pointers it may contain, so inherit from the parent.
    type->constraints = getInheritedConstraints(
        move(parentDesc.targetDescriptor.constraints), member);
  }

  assert(type->constraints != nullptr);

  // Inherit pointer constraints.
  for (auto& possibleTarget: parentDesc.constraints.possibleTargets) {
    switch (possibleTarget.specificity) {
      case TargetSpecificity::EXACT_TARGET:
        // Parent possibly pointed at this exact path, so child points at this exact path plus
        // the variable added to the end.
        possibleTarget.path.member.path.push_back(member);
        break;

      case TargetSpecificity::TARGET_OR_MEMBER:
        // Parent possibly pointed at this path or some member thereof.  The child also points
        // to some member thereof, but we can't add any new useful info.
        break;
    }
  }

  return DescribedPointer(
      PointerDescriptor(
          DataDescriptor(move(type->type), move(*type->constraints)),
          parentDesc.exclusivity,
          move(parentDesc.constraints)),
      expressionBuilder.getPointerToMember(move(parent.expression), member),
      evaluator.getPointerToMember(move(parent.staticPointer), member));
}

Maybe<DescribedPointer> Compiler::toPointer(
    DescribedPointer&& parent, PointerVariable* member, ErrorLocation location) {
  Maybe<Thing::ConstrainedType> type = getMemberType(parent, member, location);
  if (type == nullptr) {
    return nullptr;
  }

  PointerDescriptor& parentDesc = parent.descriptor;

  if (parentDesc.exclusivity <= Exclusivity::IDENTITY) {
    // TODO:  Allow reading of constant members of identity pointers.
    location.error("Cannot access member of identity alias.");
    // We can keep going here rather than return null, since we know what the code meant.
  }

  Exclusivity memberExclusivity = member->getExclusivity();

  if (memberExclusivity >= Exclusivity::EXCLUSIVE) {
    // This is an exclusive pointer.  While the copy that we're making exists, the original
    // pointer can no longer be treated as exclusive.  Therefore, copy's constraints must
    // explicitly list the source pointer in its possibleTargets, so that we can keep track of
    // the fact that the source is no longer exclusive.  If the source pointer is destroyed
    // before the copy, then the copy's possibleTargets will need to be replaced at that time
    // by substituting in the source's possibleTargets.
    //
    // TODO:  Do we want to compute the eventual replacement targets now?  Unclear if
    //   it will be hard to do later, when the substitution actually happens.

    for (auto& target: parentDesc.constraints.possibleTargets) {
      switch (target.specificity) {
        case TargetSpecificity::EXACT_TARGET:
          // Since this is an exact path, we can append the new variable to the path.
          target.path.member.path.push_back(member);
          break;

        case TargetSpecificity::TARGET_OR_MEMBER:
          // We only know that the member being read is nested somewhere inside a larger
          // object.  Since we don't know exactly which member, this pointer will have to be
          // invalidated when the outer object goes out-of-scope, even though the pointer itself
          // may point to a longer-lived object.
          break;
      }
    }
  } else {
    // Since this is a shared or identity pointer, making a copy of it does not affect its
    // exclusivity level.  Therefore, we can derive the copied pointer's possible targets
    // directly from the pointer member's possible targets, with no later substitution.

    Maybe<UnboundPointerConstraints> unboundConstraints =
        member->getUnboundConstraints(parentDesc.targetDescriptor.type.context);

    if (unboundConstraints == nullptr) {
      // No explicit constraints, so infer them from the parent's possiblePointers.
      parentDesc.constraints = getInheritedConstraints(
          move(parentDesc.targetDescriptor.constraints), member);
    } else {
      // We need to fill out the possibleTargets missing from
      // unboundConstraints->parentIndependentConstraints using
      // unboundConstraints->innerPointers.
      for (auto& parent: parentDesc.constraints.possibleTargets) {
        switch (parent.specificity) {
          case TargetSpecificity::EXACT_TARGET:
            // Append each inner pointer to this exact target to form a new exact target.
            for (const auto& target: unboundConstraints->innerPointers) {
              LocalVariablePath newPath = parent.path;  // intentional copy
              newPath.member.path.push_back(target.path.root);
              newPath.member.path.insert(newPath.member.path.end(),
                  target.path.member.path.begin(), target.path.member.path.end());

              unboundConstraints->parentIndependentConstraints.possibleTargets.push_back(
                  PointerConstraints::PossibleTarget(move(newPath), target.specificity));
            }
            break;

          case TargetSpecificity::TARGET_OR_MEMBER:
            // All we know is that if there are any inner pointers, they also point somewhere
            // inside this target.
            if (!unboundConstraints->innerPointers.empty()) {
              unboundConstraints->parentIndependentConstraints.possibleTargets.push_back(
                  move(parent));
            }
            break;
        }
      }

      // Now that it's filled in, just use it.
      parentDesc.constraints = move(unboundConstraints->parentIndependentConstraints);
    }
  }

  // OK, parentDesc.constraints now actually describe the member's constraints, not the
  // parent's.  God, in-place modification creates so much confusion.

  if (type->constraints == nullptr) {
    // No constraints were declared on the pointer's target, so we must invent some.
    type->constraints = getDefaultConstraints(type->type.entity);
  }

  assert(type->constraints != nullptr);

  return DescribedPointer(
      PointerDescriptor(
          DataDescriptor(move(type->type), move(*type->constraints)),
          std::min(parentDesc.exclusivity, memberExclusivity),
          move(parentDesc.constraints)),
      expressionBuilder.readMember(move(parent.expression), member),
      evaluator.readMember(move(parent.staticPointer), member));
}

Maybe<DescribedPointer> Compiler::toPointer(Lvalue&& lvalue, ErrorLocation location) {
  if (lvalue.isPointer) {
    if (lvalue.parent == nullptr) {
      return toPointer(lvalue.pointerVariable(), location);
    } else {
      return toPointer(move(*lvalue.parent), lvalue.pointerVariable(), location);
    }
  } else {
    if (lvalue.parent == nullptr) {
      return toPointer(lvalue.dataVariable(), location);
    } else {
      return toPointer(move(*lvalue.parent), lvalue.dataVariable(), location);
    }
  }
}

// ---------------------------------------------------------------------------------------
// bindTemporary

LocalVariablePath Compiler::bindTemporary(DescribedData& value) {
  // TODO:  Implement.
  throw "Unimplemented:  Binding a temporary to a local variable.";
}

LocalVariablePath Compiler::bindTemporary(DescribedPointer& pointer) {
  const vector<PointerConstraints::PossibleTarget>& possibleTargets =
      pointer.descriptor.constraints.possibleTargets;
  if (pointer.descriptor.constraints.additionalTargets == AdditionalTargets::NONE &&
      possibleTargets.size() == 1 &&
      possibleTargets[0].specificity == TargetSpecificity::EXACT_TARGET) {
    // We can just return this path.
    return possibleTargets[0].path;   // intentional copy
  } else {
    // TODO:  Bind the pointer to a local variable scoped to just this statement.
    throw "Unimplemented:  Binding a temporary to a local variable.";
  }
}

// ---------------------------------------------------------------------------------------
// getMemberType

Maybe<Thing::ConstrainedType> Compiler::getMemberType(DescribedData& parent, Variable* member,
                                                      ErrorLocation location) {
  Maybe<Thing::ConstrainedType> result =
      member->getType(parent.descriptor.type.context, parent.staticValue);

  if (result == nullptr) {
    // Member type is dependent on dynamic instance details.  We need to bind the value to a
    // temporary local variable.
    Context subContext(parent.descriptor.type.context,
                       Context::Binding::fromPointer(bindTemporary(parent)));
    result = member->getType(subContext);
  }

  return result;
}

Maybe<Thing::ConstrainedType> Compiler::getMemberType(DescribedPointer& parent,
                                                      Variable* variable, ErrorLocation location) {
  Maybe<Thing::ConstrainedType> result;

  if (parent.staticPointer) {
    result = variable->getType(
        parent.descriptor.targetDescriptor.type.context, *parent.staticPointer);
  }

  if (result == nullptr) {
    // Member type is dependent on dynamic instance details.  We need to bind the pointer to a
    // temporary local variable.
    Context subContext(parent.descriptor.targetDescriptor.type.context,
                       Context::Binding::fromPointer(bindTemporary(parent)));
    result = variable->getType(subContext);
  }

  return result;
}

// ---------------------------------------------------------------------------------------
// getMember

Maybe<DescribedData> Compiler::getMember(DescribedData&& object, DataVariable* member,
                                         ErrorLocation location) {
  Maybe<Thing::ConstrainedType> type = getMemberType(object, member, location);
  if (type == nullptr) {
    return nullptr;
  }

  if (type->constraints == nullptr) {
    // Derive constraints from parent constraints.
    type->constraints = getInheritedConstraints(move(object.descriptor.constraints), member);
  }

  return DescribedData(
      DataDescriptor(move(type->type), move(*type->constraints)),
      expressionBuilder.readMember(move(object.expression), member),
      evaluator.readMember(move(object.staticValue), member));
}

Maybe<DescribedPointer> Compiler::getMember(DescribedData&& object, PointerVariable* member,
                                            ErrorLocation location) {
  Maybe<Thing::ConstrainedType> type = getMemberType(object, member, location);
  if (type == nullptr) {
    return nullptr;
  }

  Maybe<UnboundPointerConstraints> unboundConstraints =
      member->getUnboundConstraints(object.descriptor.type.context);

  Maybe<PointerConstraints> constraints;

  if (unboundConstraints == nullptr) {
    // No explicit constraints, so infer them from the parent's possiblePointers.
    constraints = getInheritedConstraints(move(object.descriptor.constraints), member);
  } else {
    constraints = move(unboundConstraints->parentIndependentConstraints);

    if (!unboundConstraints->innerPointers.empty()) {
      // Must bind "this" to a local variable.
      LocalVariablePath binding = bindTemporary(object);

      for (auto& innerPointer: unboundConstraints->innerPointers) {
        LocalVariablePath newPath = binding;  // intentional copy
        newPath.member.path.pop_back();
        newPath.member.path.push_back(innerPointer.path.root);
        newPath.member.path.insert(newPath.member.path.end(),
            innerPointer.path.member.path.begin(), innerPointer.path.member.path.end());
        constraints->possibleTargets.push_back(
            PointerConstraints::PossibleTarget(move(newPath), innerPointer.specificity));
      }
    }
  }

  assert(constraints != nullptr);

  if (type->constraints == nullptr) {
    // No constraints were declared on the pointer's target, so we must invent some.
    type->constraints = getDefaultConstraints(type->type.entity);
  }

  return DescribedPointer(
      PointerDescriptor(
          DataDescriptor(move(type->type), move(*type->constraints)),
          member->getExclusivity(),
          move(*constraints)),
      expressionBuilder.readMember(move(object.expression), member),
      evaluator.readMember(move(object.staticValue), member));
}

Maybe<Thing> Compiler::getMember(Thing&& object, const string& memberName, ErrorLocation location) {
  switch (object.getKind()) {
    case Thing::Kind::FUNCTION:
    case Thing::Kind::METHOD:
    case Thing::Kind::TUPLE:
      location.error("This thing doesn't have members.");
      return nullptr;

    case Thing::Kind::TYPE:
      return object.type.type.entity->getMemberOfType(*this, move(object.type), memberName);

    case Thing::Kind::RVALUE:
      return object.rvalue.dataDescriptor().type.entity->getMemberOfInstance(
          *this, move(object.rvalue), memberName, location);

    case Thing::Kind::LVALUE: {
      Maybe<DescribedPointer> ptr = toPointer(move(object.lvalue), location);
      if (ptr == nullptr) {
        return nullptr;
      }

      return ptr->descriptor.targetDescriptor.type.entity->getMemberOfInstance(
          *this, DescribedRvalue::from(move(*ptr)), memberName, location);
    }
  }

  throw "can't get here";
}

// ---------------------------------------------------------------------------------------
// evaluate

Maybe<Thing> Compiler::evaluate(ast::Expression& expression, VariableUsageSet& variablesUsed) {
  switch (expression.getType()) {
    case Expression::Type::ERROR: {
      for (auto& error: expression.error) {
        ErrorLocation(expression).error(error);
      }
      return nullptr;
    }

    case Expression::Type::VARIABLE: {
      Maybe<Thing> thing = scope.lookupBinding(expression.variable);
      if (thing == nullptr) {
        ErrorLocation(expression).error("\"", expression.variable, "\" is not declared.");
        return nullptr;
      } else {
        return move(*thing);
      }
    }

    case Expression::Type::TUPLE: {
      // TODO
      throw "unimplemented";
    }

    case Expression::Type::LITERAL_INT:
      return Thing::from(
          DescribedRvalue::from(
              DescribedData(
                  DataDescriptor(
                      Bound<Type>(BuiltinType::get(BuiltinType::Builtin::INTEGER), Context({})),
                      DataConstraints::fromEmpty()),
                  DataExpression::fromConstant(DataValue::fromInteger(expression.literalInt)),
                  DataValue::fromInteger(expression.literalInt))));

    case Expression::Type::LITERAL_DOUBLE:
      return Thing::from(
          DescribedRvalue::from(
              DescribedData(
                  DataDescriptor(
                      Bound<Type>(BuiltinType::get(BuiltinType::Builtin::DOUBLE), Context({})),
                      DataConstraints::fromEmpty()),
                  DataExpression::fromConstant(DataValue::fromDouble(expression.literalDouble)),
                  DataValue::fromDouble(expression.literalDouble))));

    case Expression::Type::LITERAL_STRING:
      // TODO:  Generate code to construct the string class.
      throw "unimplemented";

    case Expression::Type::LITERAL_ARRAY:
      // TODO:  Generate code to construct the FixedArray class.
      throw "unimplemented";

    case Expression::Type::BINARY_OPERATOR: {
      vector<VariableUsageSet> subVariablesUsed(2);
      Maybe<Thing> left = evaluate(*expression.binaryOperator.left, subVariablesUsed[0]);
      Maybe<Thing> right = evaluate(*expression.binaryOperator.right, subVariablesUsed[1]);
      variablesUsed.merge(move(subVariablesUsed));

      if (left == nullptr || right == nullptr) {
        return nullptr;
      }

      Maybe<DescribedRvalue&> leftRvalue =
          asRvalue(*left, ErrorLocation(*expression.binaryOperator.left));
      Maybe<DescribedRvalue&> rightRvalue =
          asRvalue(*right, ErrorLocation(*expression.binaryOperator.right));

      Maybe<BinaryOperator&> leftOp = nullptr;
      if (leftRvalue != nullptr) {
        leftOp = leftRvalue->dataDescriptor().type.entity->lookupLeftBinaryOperator(
            expression.binaryOperator.op);
      }

      Maybe<BinaryOperator&> rightOp = nullptr;
      if (rightRvalue != nullptr) {
        rightOp = rightRvalue->dataDescriptor().type.entity->lookupRightBinaryOperator(
            expression.binaryOperator.op);
      }

      BinaryOperator::MatchSpecificity leftMatch =
          leftOp == nullptr ? BinaryOperator::MatchSpecificity::NONE
                            : leftOp->match(*this, *leftRvalue, *right);
      BinaryOperator::MatchSpecificity rightMatch =
          rightOp == nullptr ? BinaryOperator::MatchSpecificity::NONE
                             : rightOp->match(*this, *rightRvalue, *left);

      if (leftMatch > rightMatch) {
        return Thing::from(
            leftOp->call(*this, move(*leftRvalue), move(*right), variablesUsed,
                         ErrorLocation(expression)));
      } else if (rightMatch > leftMatch) {
        return Thing::from(
            rightOp->call(*this, move(*rightRvalue), move(*left), variablesUsed,
                          ErrorLocation(expression)));
      } else {
        if (leftMatch == BinaryOperator::MatchSpecificity::NONE) {
          ErrorLocation(expression).error("No such operator.");
        } else {
          ErrorLocation(expression).error(
              "Ambiguous operator:  Each operand's type defines an operator that matches a "
              "superclass of the other's type.");
        }
        return nullptr;
      }
    }

    case Expression::Type::PREFIX_OPERATOR: {
      Maybe<Thing> operand = evaluate(*expression.prefixOperator.operand, variablesUsed);
      if (operand == nullptr) {
        return nullptr;
      }
      Maybe<DescribedRvalue&> rvalue =
          asRvalue(*operand, ErrorLocation(*expression.binaryOperator.left));
      if (rvalue == nullptr) {
        ErrorLocation(expression).error("Can't apply unary operator to non-value.");
        return nullptr;
      }

      Maybe<UnaryOperator&> op =
          rvalue->dataDescriptor().type.entity->lookupPrefixOperator(expression.prefixOperator.op);
      if (op == nullptr) {
        ErrorLocation(expression).error("No such operator.");
        return nullptr;
      }

      return Thing::from(op->call(*this, move(*rvalue), variablesUsed, ErrorLocation(expression)));
    }

    case Expression::Type::POSTFIX_OPERATOR: {
      Maybe<Thing> operand = evaluate(*expression.postfixOperator.operand, variablesUsed);
      if (operand == nullptr) {
        return nullptr;
      }
      Maybe<DescribedRvalue&> rvalue =
          asRvalue(*operand, ErrorLocation(*expression.binaryOperator.left));
      if (rvalue == nullptr) {
        ErrorLocation(expression).error("Can't apply unary operator to non-value.");
        return nullptr;
      }

      Maybe<UnaryOperator&> op =
          rvalue->dataDescriptor().type.entity->lookupPostfixOperator(
              expression.postfixOperator.op);
      if (op == nullptr) {
        ErrorLocation(expression).error("No such operator.");
        return nullptr;
      }

      return Thing::from(op->call(*this, move(*rvalue), variablesUsed, ErrorLocation(expression)));
    }

    case Expression::Type::TERNARY_OPERATOR:
      // TODO:  Ternary operator is kind of painful, actually.
      throw "unimplemented";

    case Expression::Type::FUNCTION_CALL: {
      vector<VariableUsageSet> subVariablesUsed(1);
      subVariablesUsed.reserve(expression.functionCall.parameters.size() + 1);

      Maybe<Thing> function = evaluate(*expression.functionCall.function, subVariablesUsed[0]);

      vector<Tuple::Element> positionalElements;
      vector<Tuple::NamedElement> namedElements;

      bool hadError = function == nullptr;
      for (auto& param: expression.functionCall.parameters) {
        // TODO:  Support named parameters once they are supported in the AST.
        subVariablesUsed.emplace_back();
        Maybe<Thing> parameter = evaluate(*param.expression, subVariablesUsed.back());
        if (parameter == nullptr) {
          hadError = true;
        } else {
          positionalElements.emplace_back(
              param.styleAllowance,
              evaluate(*param.expression, subVariablesUsed.back()));
        }
      }

      variablesUsed.merge(move(subVariablesUsed));

      Tuple parameters(move(positionalElements), move(namedElements));

      if (hadError) {
        return nullptr;
      }

      switch (function->getKind()) {
        case Thing::Kind::TYPE:
        case Thing::Kind::RVALUE:
        case Thing::Kind::LVALUE:
        case Thing::Kind::TUPLE:
          ErrorLocation(*expression.functionCall.function).error("Not a function");
          return nullptr;

        case Thing::Kind::FUNCTION:
          return function->function.entity->resolve(
              *this, move(function->function.context), move(parameters), ErrorLocation(expression));

        case Thing::Kind::METHOD:
          return function->method.method->resolve(
              *this, move(function->method.object), move(parameters), ErrorLocation(expression));
      }
    }

    case Expression::Type::SUBSCRIPT:
      // TODO
      throw "unimplemented";

    case Expression::Type::MEMBER_ACCESS: {
      // TODO:  Handle style allowance.
      Maybe<Thing> object = evaluate(*expression.memberAccess.object, variablesUsed);
      if (object == nullptr) {
        return nullptr;
      }
      return getMember(move(*object), expression.memberAccess.member, ErrorLocation(expression));
    }

    case Expression::Type::IMPORT:
    case Expression::Type::LAMBDA:
      // TODO
      throw "unimplemented";
  }

  throw "Can't get here.";
}

Maybe<DescribedRvalue&> Compiler::asRvalue(Thing& thing, ErrorLocation location) {
  switch (thing.getKind()) {
    case Thing::Kind::TYPE:
    case Thing::Kind::TUPLE:
    case Thing::Kind::FUNCTION:
    case Thing::Kind::METHOD:
      return nullptr;

    case Thing::Kind::RVALUE:
      return thing.rvalue;

    case Thing::Kind::LVALUE:
      Maybe<DescribedPointer> pointer = toPointer(move(thing.lvalue), location);
      if (pointer == nullptr) {
        return nullptr;
      } else {
        thing = Thing::from(move(*pointer));
        return thing.rvalue;
      }
  }

  throw "can't get here";
}

}  // namespace compiler
}  // namespace modc
