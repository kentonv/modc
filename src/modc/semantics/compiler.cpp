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

namespace modc {
namespace compiler {

using std::move;
using ast::Expression;
using ast::Statement;

template <typename ValueType, typename PointerType>
class Evaluator {
public:
  ValueType readLocalVariable(ValueVariable* variable);
  PointerType readLocalVariable(PointerVariable* variable);
  PointerType makePointerToLocalVariable(ValueVariable* variable);

  ValueType readPointer(PointerType&& pointer);

  ValueType readMember(ValueType&& object, ValueVariable* member);
  PointerType readMember(ValueType&& object, PointerVariable* member);
  PointerType readMember(PointerType&& object, PointerVariable* member);
  PointerType getPointerToMember(PointerType&& object, ValueVariable* member);
  PointerType upcast(PointerType&& object, ImplementedInterface* interface);
};

class Compiler {
public:
  Compiler(Scope& scope): scope(scope) {}

  // Returns Thing::fromUnknown() for convenience.
  template <typename... Parts>
  Thing error(const ast::Expression& location, Parts&&... parts);
  template <typename... Parts>
  Thing error(const ast::Declaration& location, Parts&&... parts);
  template <typename... Parts>
  Thing error(const ast::Statement& location, Parts&&... parts);

  ErrorLocation errorLocation(const Expression& expression);

  CxxExpression asCxx(Thing&& value);

  bool areEqual(const Thing& a, const Thing& b);
  bool areEqual(const EntityName& a, const EntityName& b);
  bool areEqual(const Context& a, const Context& b);
  template <typename T, typename U>
  bool areEqual(const Bound<T>& a, const Bound<U>& b);

  Thing call(Entity& entity, ThingPort& port, Tuple&& parameters);

  bool isComplete(const Value& value);

  // -------------------------------------------------------------------------------------
  // getInheritedConstraints

  ValueConstraints getInheritedConstraints(ValueConstraints&& parentConstraints,
                                           ValueVariable* member) {
    vector<ValueConstraints::PossiblePointer> possiblePointers;

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

    return ValueConstraints(move(possiblePointers), parentConstraints.additionalPointers);
  }

  PointerConstraints getInheritedConstraints(ValueConstraints&& parentConstraints,
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

  ValueConstraints getDefaultConstraints(Type* type) {
    if (type->hasUnannotatedAliases()) {
      // The target type has aliases, but it was never declared what they might point at.  This
      // can only be the case for parameters since local variables would have had the
      // constraints inferred.  So, assume the pointers point at stuff in the caller's scope.
      return ValueConstraints(
          vector<ValueConstraints::PossiblePointer>(),
          AdditionalTargets::FROM_CALLER);
    } else {
      // The target contains no unannotated aliases.
      return ValueConstraints(
          vector<ValueConstraints::PossiblePointer>(),
          AdditionalTargets::NONE);
    }
  }

  // -------------------------------------------------------------------------------------
  // getMemberType

  Maybe<Thing::ConstrainedType> getMemberType(const DescribedValue& parent, Variable* member,
                                              ErrorLocation location) {
    Maybe<Thing::ConstrainedType> result =
        member->getType(parent.descriptor.type.context, parent.staticValue);

    if (result == nullptr) {
      // Member type is dependent on dynamic instance details.  We need to bind the value to a
      // temporary local variable.

      // TODO:  Bind the value to a local variable scoped to just this statement, then use that to
      //   build a context to pass to member->getType(const Context&).
      location.error("Unimplemented:  Accessing member whose type is dependent on the object "
                     "instance, where the object is a temporary.");
    }

    return result;
  }

  // Get the type of a member variable given its parent pointer.
  Maybe<Thing::ConstrainedType> getMemberType(const DescribedPointer& parent,
                                              Variable* variable, ErrorLocation location) {
    Maybe<Thing::ConstrainedType> result;

    if (parent.staticPointer) {
      result = variable->getType(
          parent.descriptor.targetDescriptor.type.context, *parent.staticPointer);
    }

    if (result == nullptr) {
      // Member type is dependent on dynamic instance details.  We need to find a local path to
      // the instance.
      const vector<PointerConstraints::PossibleTarget>& possibleTargets =
          parent.descriptor.constraints.possibleTargets;
      if (possibleTargets.size() == 1 &&
          possibleTargets[0].specificity == TargetSpecificity::EXACT_TARGET) {
        Context subContext(parent.descriptor.targetDescriptor.type.context,
            Context::Binding::fromPointer(LocalVariablePath(possibleTargets[0].path)));
        result = variable->getType(subContext);
      } else {
        // TODO:  Bind the pointer to a local variable scoped to just this statement.
        location.error("Unimplemented:  Accessing member whose type is dependent on the object "
                       "instance, where the object is a temporary.");
        result = nullptr;
      }
    }

    return result;
  }

  // -------------------------------------------------------------------------------------
  // toPointer

  // Construct a pointer from an lvalue.
  //
  // Returns null on error or incomplete information.
  Maybe<DescribedPointer> toPointer(Thing::Lvalue&& lvalue, ErrorLocation location) {
    if (lvalue.parent == nullptr) {
      // The lvalue names a local variable.
      // Read the variable.
      ValueDescriptor descriptor = scope.getVariableDescriptor(lvalue.variable);

      // Form a pointer to the local variable.
      PointerConstraints constraints(
          PointerConstraints::PossibleTarget(
              LocalVariablePath(lvalue.variable),
              TargetSpecificity::EXACT_TARGET));
      return DescribedPointer(
          PointerDescriptor(move(descriptor), Exclusivity::OWNED, move(constraints)),
          expressionBuilder.makePointerToLocalVariable(lvalue.variable),
          evaluator.makePointerToLocalVariable(lvalue.variable));

    } else {
      Maybe<Thing::ConstrainedType> type = getMemberType(*lvalue.parent, lvalue.variable, location);
      if (type == nullptr) {
        return nullptr;
      }

      PointerDescriptor& parentDesc = lvalue.parent->descriptor;

      if (parentDesc.exclusivity <= Exclusivity::IDENTITY) {
        // TODO:  Allow reading of constant members of identity pointers.
        location.error("Cannot access member of identity alias.");
        // We can keep going here rather than return null, since we know what the code meant.
      }

      if (type->constraints == nullptr) {
        // The member does not declare what pointers it may contain, so inherit from the parent.
        type->constraints = getInheritedConstraints(
            move(parentDesc.targetDescriptor.constraints), lvalue.variable);
      }

      assert(type->constraints != nullptr);

      // Inherit pointer constraints.
      for (auto& possibleTarget: parentDesc.constraints.possibleTargets) {
        switch (possibleTarget.specificity) {
          case TargetSpecificity::EXACT_TARGET:
            // Parent possibly pointed at this exact path, so child points at this exact path plus
            // the variable added to the end.
            possibleTarget.path.member.path.push_back(lvalue.variable);
            break;

          case TargetSpecificity::TARGET_OR_MEMBER:
            // Parent possibly pointed at this path or some member thereof.  The child also points
            // to some member thereof, but we can't add any new useful info.
            break;
        }
      }

      return DescribedPointer(
          PointerDescriptor(
              ValueDescriptor(move(type->type), move(*type->constraints)),
              parentDesc.exclusivity,
              move(parentDesc.constraints)),
          expressionBuilder.getPointerToMember(move(lvalue.parent->pointer), lvalue.variable),
          evaluator.getPointerToMember(move(lvalue.parent->staticPointer), lvalue.variable));
    }
  }

  // Construct a pointer from a pointer lvalue.  This actually reads the lvalue and returns it,
  // since you can't have a pointer to a pointer.
  //
  // Returns null on error or incomplete information.
  Maybe<DescribedPointer> toPointer(Thing::PointerLvalue&& lvalue, ErrorLocation location) {
    if (lvalue.parent == nullptr) {
      // The lvalue names a local variable.
      PointerDescriptor descriptor = scope.getVariableDescriptor(lvalue.variable);

      return DescribedPointer(move(descriptor),
          expressionBuilder.readLocalVariable(lvalue.variable),
          evaluator.readLocalVariable(lvalue.variable));

    } else {
      Maybe<Thing::ConstrainedType> type = getMemberType(*lvalue.parent, lvalue.variable, location);
      if (type == nullptr) {
        return nullptr;
      }

      PointerDescriptor& parentDesc = lvalue.parent->descriptor;

      if (parentDesc.exclusivity <= Exclusivity::IDENTITY) {
        // TODO:  Allow reading of constant members of identity pointers.
        location.error("Cannot access member of identity alias.");
        // We can keep going here rather than return null, since we know what the code meant.
      }

      Exclusivity memberExclusivity = lvalue.variable->getExclusivity();

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
              target.path.member.path.push_back(lvalue.variable);
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
            lvalue.variable->getUnboundConstraints(parentDesc.targetDescriptor.type.context);

        if (unboundConstraints == nullptr) {
          // No explicit constraints, so infer them from the parent's possiblePointers.
          parentDesc.constraints = getInheritedConstraints(
              move(parentDesc.targetDescriptor.constraints), lvalue.variable);
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
              ValueDescriptor(move(type->type), move(*type->constraints)),
              std::min(parentDesc.exclusivity, memberExclusivity),
              move(parentDesc.constraints)),
          expressionBuilder.readMember(move(lvalue.parent->pointer), lvalue.variable),
          evaluator.readMember(move(lvalue.parent->staticPointer), lvalue.variable));
    }
  }

  // -------------------------------------------------------------------------------------
  // dereference

  DescribedValue dereference(DescribedPointer&& input,
                             VariableUsageSet& variablesUsed,
                             ErrorLocation location) {
    variablesUsed.addUsage(input.descriptor.constraints, input.descriptor.exclusivity, location);

    return DescribedValue(
        move(input.descriptor.targetDescriptor),
        expressionBuilder.readPointer(move(input.pointer)),
        evaluator.readPointer(move(input.staticPointer)));
  }

  // -------------------------------------------------------------------------------------
  // checkConstraints

  bool checkPointerPath(const LocalVariablePath& allowedPath, TargetSpecificity allowedSpecificity,
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
  }

  void checkConstraints(AdditionalTargets allowed, AdditionalTargets actual,
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

  void checkConstraints(const ValueConstraints& allowed, const ValueConstraints& actual,
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

  void checkConstraints(const PointerConstraints& allowed, const PointerConstraints& actual,
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

  // -------------------------------------------------------------------------------------
  // convertToValue

  Maybe<DescribedValue> convertToValue(
      DescribedValue&& input, Bound<Type>&& targetType, ErrorLocation location) {
    if (areEqual(input.descriptor.type, targetType)) {
      return move(input);
    }

    // Look for conversions on the source type.
    ThingPort inputTypePort = scope.makePortFor(input.descriptor.type.context);
    Maybe<Function&> conversion =
        input.descriptor.type.entity->lookupConversion(inputTypePort, targetType);
    if (conversion != nullptr) {
      DescribedPointerOrValue result = callMethod(move(input), &*conversion, {}, location);
      assert(result.getKind() == DescribedPointerOrValue::Kind::VALUE);
      assert(areEqual(result.value.descriptor.type, targetType));
      return move(result.value);
    }

    // Look for a constructor on the destination type.
    Overload* constructor = targetType.entity->getImplicitConstructor();

    Thing result = constructor->resolve(*this, move(targetType.context),
                                        Tuple::fromSingleValue(Thing::fromValue(move(input))),
                                        location);

    if (result.getKind() == Thing::Kind::UNKNOWN) {
      return nullptr;
    } else {
      assert(result.getKind() == Thing::Kind::VALUE);
      return move(result.value);
    }
  }

  Maybe<DescribedValue> convertToValue(
      DescribedPointer&& input, Bound<Type>&& targetType,
      VariableUsageSet& variablesUsed, ErrorLocation location) {
    // TODO: Do we want to try to find a constructor on the target type that accepts a pointer?
    //   Probably -- it's useful for pointer wrappers.
    return convertToValue(dereference(move(input), variablesUsed, location),
                          move(targetType), location);
  }

  Maybe<DescribedValue> convertToValue(
      Thing&& input, Bound<Type>&& targetType,
      VariableUsageSet& variablesUsed, ErrorLocation location) {
    switch (input.getKind()) {
      case Thing::Kind::UNKNOWN:
        return nullptr;

      case Thing::Kind::TYPE:
      case Thing::Kind::FUNCTION:
      case Thing::Kind::METHOD:
        location.error("Not a value.");
        return nullptr;

      case Thing::Kind::VALUE:
        return convertToValue(move(input.value), move(targetType), location);

      case Thing::Kind::POINTER:
        return convertToValue(move(input.pointer), move(targetType), variablesUsed, location);

      case Thing::Kind::LVALUE: {
        Maybe<DescribedPointer> ptr = toPointer(move(input.lvalue), location);
        if (ptr == nullptr) {
          return nullptr;
        } else {
          return convertToValue(move(*ptr), move(targetType), variablesUsed, location);
        }
      }

      case Thing::Kind::POINTER_LVALUE: {
        Maybe<DescribedPointer> ptr = toPointer(move(input.pointerLvalue), location);
        if (ptr == nullptr) {
          return nullptr;
        } else {
          return convertToValue(move(*ptr), move(targetType), variablesUsed, location);
        }
      }

      case Thing::Kind::TUPLE: {
        Overload* constructor = targetType.entity->getImplicitConstructor();
        Thing result = constructor->resolve(*this, move(targetType.context),
                                            move(input.tuple), location);
        if (result.getKind() == Thing::Kind::UNKNOWN) {
          return nullptr;
        } else {
          assert(result.getKind() == Thing::Kind::VALUE);
          return move(result.value);
        }
      }
    }
  }

  Maybe<DescribedValue> convertToValue(
      Thing&& input, ValueDescriptor&& targetDescriptor,
      VariableUsageSet& variablesUsed, ErrorLocation location) {
    Maybe<DescribedValue> result =
        convertToValue(move(input), move(targetDescriptor.type), variablesUsed, location);

    if (result != nullptr) {
      checkConstraints(targetDescriptor.constraints, result->descriptor.constraints, location);
      result->descriptor.constraints = move(targetDescriptor.constraints);
    }

    return result;
  }

  // -------------------------------------------------------------------------------------
  // convertToPointer

  Maybe<DescribedPointer> convertToPointer(DescribedPointer&& input,
                                           Bound<Type>&& targetType,
                                           Exclusivity targetExclusivity,
                                           VariableUsageSet& variablesUsed,
                                           ErrorLocation location) {
    if (!areEqual(input.descriptor.targetDescriptor.type, targetType)) {
      if (dynamic_cast<Interface*>(targetType.entity) == nullptr) {
        location.error("Type mismatch.");
      } else {
        ThingPort inputPort = scope.makePortFor(input.descriptor.targetDescriptor.type.context);
        Maybe<ImplementedInterface&> interface =
            input.descriptor.targetDescriptor.type.entity->findImplementedInterface(
                inputPort, targetType);
        if (interface == nullptr) {
          location.error("Object does not implement desired interface.");
        } else {
          input.descriptor.targetDescriptor.type = targetType;
          input.pointer = expressionBuilder.upcast(move(input.pointer), &*interface);
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

  Maybe<DescribedPointer> convertToPointer(Thing&& input, Bound<Type>&& targetType,
                                           Exclusivity targetExclusivity,
                                           VariableUsageSet& variablesUsed,
                                           ErrorLocation location) {
    switch (input.getKind()) {
      case Thing::Kind::UNKNOWN:
        return nullptr;

      case Thing::Kind::TYPE:
      case Thing::Kind::FUNCTION:
      case Thing::Kind::METHOD:
      case Thing::Kind::VALUE:
      case Thing::Kind::LVALUE:
      case Thing::Kind::TUPLE:
        // TODO:  If non-exclusive, try convertToValue() then take pointer-to-temporary?
        location.error("Not a pointer.");
        return nullptr;

      case Thing::Kind::POINTER:
        return convertToPointer(move(input.pointer), move(targetType), targetExclusivity,
                                variablesUsed, location);

      case Thing::Kind::POINTER_LVALUE: {
        Maybe<DescribedPointer> ptr = toPointer(move(input.pointerLvalue), location);
        if (ptr == nullptr) {
          return nullptr;
        } else {
          return convertToPointer(move(*ptr), move(targetType), targetExclusivity,
                                  variablesUsed, location);
        }
      }
    }
  }

  Maybe<DescribedPointer> convertToPointer(
      Thing&& input, PointerDescriptor&& targetDescriptor,
      VariableUsageSet& variablesUsed, ErrorLocation location) {
    Maybe<DescribedPointer> result =
        convertToPointer(move(input), move(targetDescriptor.targetDescriptor.type),
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

  // -------------------------------------------------------------------------------------
  // lookupBinding

  Thing lookupBinding(string&& name, ErrorLocation location) {
    Maybe<Thing> thing = scope.lookupBinding(name);
    if (thing == nullptr) {
      location.error("\"", name, "\" is not declared.");
      return Thing::fromUnknown();
    } else {
      // TODO:  Apply default conversion.
      return move(*thing);
    }
  }

  // -------------------------------------------------------------------------------------
  // getMember

  Maybe<DescribedValue> getMember(DescribedValue&& object, ValueVariable* member,
                                  ErrorLocation location) {
    Maybe<Thing::ConstrainedType> type = getMemberType(object, member, location);
    if (type == nullptr) {
      return nullptr;
    }

    if (type->constraints == nullptr) {
      // Derive constraints from parent constraints.
      type->constraints = getInheritedConstraints(move(object.descriptor.constraints), member);
    }

    return DescribedValue(
        ValueDescriptor(move(type->type), move(*type->constraints)),
        expressionBuilder.readMember(move(object.value), member),
        evaluator.readMember(move(object.staticValue), member));
  }

  Maybe<DescribedPointer> getMember(DescribedValue&& object, PointerVariable* member,
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
    } else if (unboundConstraints->innerPointers.empty()) {
      constraints = move(unboundConstraints->parentIndependentConstraints);
    } else {
      // TODO:  Bind the value to a local variable scoped to just this statement.
      location.error("Unimplemented:  Trying to access pointer member of temporary where the "
                     "pointer may point at another member of the same temporary.");
      return nullptr;
    }

    assert(constraints != nullptr);

    if (type->constraints == nullptr) {
      // No constraints were declared on the pointer's target, so we must invent some.
      type->constraints = getDefaultConstraints(type->type.entity);
    }

    return DescribedPointer(
        PointerDescriptor(
            ValueDescriptor(move(type->type), move(*type->constraints)),
            member->getExclusivity(),
            move(*constraints)),
        expressionBuilder.readMember(move(object.value), member),
        evaluator.readMember(move(object.staticValue), member));
  }

  Thing::Lvalue getMember(DescribedPointer&& object, ValueVariable* member,
                          ErrorLocation location) {
    return Thing::Lvalue(move(object), member);
  }

  Thing::PointerLvalue getMember(DescribedPointer&& object, PointerVariable* member,
                                 ErrorLocation location) {
    return Thing::PointerLvalue(move(object), member);
  }

  Thing getMember(Thing&& object, const string& memberName, ErrorLocation location) {
    switch (object.getKind()) {
      case Thing::Kind::UNKNOWN:
        return Thing::fromUnknown();

      case Thing::Kind::FUNCTION:
      case Thing::Kind::METHOD:
      case Thing::Kind::TUPLE:
        location.error("This thing doesn't have members.");
        return Thing::fromUnknown();

      case Thing::Kind::TYPE:
        return object.type.type.entity->getMemberOfType(*this, move(object.type), memberName);

      case Thing::Kind::VALUE:
        return object.value.descriptor.type.entity->getMemberOfInstance(
            *this, move(object.value), memberName, location);

      case Thing::Kind::POINTER:
        return object.pointer.descriptor.targetDescriptor.type.entity->getMemberOfInstance(
            *this, move(object.pointer), memberName, location);

      case Thing::Kind::LVALUE: {
        Maybe<DescribedPointer> ptr = toPointer(move(object.lvalue), location);
        if (ptr == nullptr) {
          return Thing::fromUnknown();
        }

        return ptr->descriptor.targetDescriptor.type.entity->getMemberOfInstance(
            *this, move(*ptr), memberName, location);
      }

      case Thing::Kind::POINTER_LVALUE: {
        Maybe<DescribedPointer> ptr = toPointer(move(object.pointerLvalue), location);
        if (ptr == nullptr) {
          return Thing::fromUnknown();
        }

        return ptr->descriptor.targetDescriptor.type.entity->getMemberOfInstance(
            *this, move(*ptr), memberName, location);
      }
    }
  }

  // -------------------------------------------------------------------------------------
  // callFunction / callMethod

  Thing callFunction(Bound<Function> function, vector<DescribedPointerOrValue>&& parameters,
                     ErrorLocation location);
  DescribedPointerOrValue callMethod(
      DescribedValue&& object, Function* method,
      vector<DescribedPointerOrValue>&& parameters, ErrorLocation location);












#if 0

  ValueDescriptor findCommonSupertype(const ValueDescriptor& type1, const ValueDescriptor& type2);


  Thing applyBinaryOperator(const Expression& leftExpression, ast::BinaryOperator op,
                            const Expression& rightExpression,
                            VariableUsageSet& variablesUsed, ErrorLocation location) {
    vector<VariableUsageSet> subVariablesUsed(2);

    Thing left = dereference(
        evaluate(leftExpression, variablesUsed), subVariablesUsed[0],
                 errorLocation(leftExpression));
    Thing right = dereference(
        evaluate(rightExpression, variablesUsed), subVariablesUsed[1],
                 errorLocation(rightExpression));

    variablesUsed.merge(move(subVariablesUsed));

    const ValueDescriptor* leftDesc = getValueDescriptor(left);
    const ValueDescriptor* rightDesc = getValueDescriptor(right);

    if (leftDesc == nullptr || rightDesc == nullptr) {
      // Can't do anything if one input is fully unknown.
      return Thing::fromUnknown();
    }

    Maybe<Overload&> leftOverload = leftDesc->type->lookupLeftBinaryOperator(op);
    Maybe<Overload&> righOverload = rightDesc->type->lookupLeftBinaryOperator(op);

    // TODO:  Binary operators are more complicated since parameters arguably shouldn't be
    //   dereferenced...  hmm...
    throw "TODO";

    if (!leftOverload && !leftOverload) {
      location.error("Operands do not support this operator.");
      return Thing::fromUnknown();
    }

    if (left.getKind() == Thing::Kind::UNKNOWN || right.getKind() == Thing::Kind::UNKNOWN) {
      // One operand is unknown, so result must be unknown.
    }
  }

  Thing applyPrefixOperator(ast::PrefixOperator op, const Expression& operandExpression,
                            VariableUsageSet& variablesUsed, ErrorLocation location) {
    Thing operand = evaluate(operandExpression, variablesUsed);

    switch (operand.getKind()) {
      case Thing::Kind::UNKNOWN:
        // Update the descriptor, if there is one and it is a value.
        if (operand.unknown) {
          if (operand.unknown->getKind() != ThingDescriptor::Kind::VALUE) {
            return errorCantUseHere(move(operand), errorLocation(operandExpression));
          }

          Maybe<Function&> method = operand.unknown->value.type->lookupPrefixOperator(op);
          if (!method) {
            location.error("Operand does not support this operator.");
            return Thing::fromUnknown();
          }

          // Unary operator must always have empty parameters.
          assert(method->getParameters().empty());

          operand.unknown->value = method->getReturnDescriptor(
              move(operand.unknown->value), vector<ValueDescriptor>());
        }
        return move(operand);

      case Thing::Kind::VALUE: {
        Maybe<Function&> method = operand.value.descriptor.type->lookupPrefixOperator(op);
        if (!method) {
          location.error("Operand does not support this operator.");
          return Thing::fromUnknown();
        }

        assert(method->getParameters().empty());

        return method->call(move(operand.value), vector<Thing::DynamicValue>());
      }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(operand), errorLocation(operandExpression));
    }
  }

  Thing applyPostfixOperator(const Expression& operandExpression, ast::PostfixOperator op,
                             VariableUsageSet& variablesUsed, ErrorLocation location) {
    Thing operand = evaluate(operandExpression, variablesUsed);

    switch (operand.getKind()) {
      case Thing::Kind::UNKNOWN:
        // Update the descriptor, if there is one and it is a value.
        if (operand.unknown) {
          if (operand.unknown->getKind() != ThingDescriptor::Kind::VALUE) {
            return errorCantUseHere(move(operand), errorLocation(operandExpression));
          }

          Maybe<Function&> method = operand.unknown->value.type->lookupPostfixOperator(op);
          if (!method) {
            location.error("Operand does not support this operator.");
            return Thing::fromUnknown();
          }

          assert(method->getParameters().empty());

          operand.unknown->value = method->getReturnDescriptor(
              move(operand.unknown->value), vector<ValueDescriptor>());
        }
        return move(operand);

      case Thing::Kind::VALUE: {
        Maybe<Function&> method = operand.value.descriptor.type->lookupPostfixOperator(op);
        if (!method) {
          location.error("Operand does not support this operator.");
          return Thing::fromUnknown();
        }

        assert(method->getParameters().empty());

        return method->call(move(operand.value), vector<Thing::DynamicValue>());
      }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(operand), errorLocation(operandExpression));
    }
  }

  Thing applyTernaryOperator(const Expression& conditionExpression,
                             const Expression& trueClause,
                             const Expression& falseClause,
                             VariableUsageSet& variablesUsed, Location location) {
    Thing condition =
        castTo(evaluate(conditionExpression, variablesUsed),
               scope.lookupBuiltinType(BuiltinType::Builtin::BOOLEAN),
               variablesUsed, conditionExpression.location);

    switch (condition.getKind()) {
      case Thing::Kind::UNKNOWN: {
        // The condition is a meta variable, meaning it will be known at compile time, meaning
        // that we only expect to compile one of the two branches.  You might think this means we
        // can't compile either branch, because it's perfectly valid for the branch that ends up
        // compiled out to have type errors.  However, the fact that the condition is a meta
        // variable implies that the function has not yet been specialized and we're just giving
        // it the once-over for errors.  We should expect that if we don't have enough knowledge
        // yet to know which branch will be taken, then we don't have enough knowledge yet
        // to know why the other branch might be broken.  If a branch fails to compile even in the
        // meta phase then it should just be deleted.
        //
        // TL;DR:  Compile both branches even though the condition is constant, just to check for
        // errors.

        // TODO:  I wonder if we should discard variablesUsed for the branches?

        Thing trueThing = evaluate(trueClause, variablesUsed);
        Thing falseThing = evaluate(falseClause, variablesUsed);

        // If the two branches have exactly the same type, then we can assume that's the type that
        // will be returned.  Otherwise, we can't assume anything.  We can't look for the common
        // super-type because at specialization time we will end up taking one branch or the other
        // and will not do this generalization.
        Maybe<ThingDescriptor> trueDescriptor = getThingDescriptor(trueThing);
        Maybe<ThingDescriptor> falseDescriptor = getThingDescriptor(falseThing);

        if (trueDescriptor == falseDescriptor) {
          return Thing::fromUnknown(move(trueDescriptor));
        } else {
          return Thing::fromUnknown();
        }
      }

      case Thing::Kind::VALUE: {
        if (condition.value.expression.getKind() == BoundExpression::Kind::CONSTANT) {
          // The condition is known at compile time.  We don't even compile the dead branch because
          // the branch may assume things that the condition disproves.
          Value& conditionValue = condition.value.expression.constant;

          // We cast to Boolean, so the constant must be a boolean.
          assert(conditionValue.getKind() == Value::Kind::BOOLEAN);

          if (conditionValue.boolean) {
            return evaluate(trueClause, variablesUsed);
          } else {
            return evaluate(falseClause, variablesUsed);
          }
        } else {
          // TODO:  Need to create sub-scopes or something to track the fact that only one
          //   of the two branches' side effects will occur.
          Thing trueThing = evaluate(trueClause, variablesUsed);
          Thing falseThing = evaluate(falseClause, variablesUsed);

          const ValueDescriptor* trueDescriptor = getValueDescriptor(trueThing);
          const ValueDescriptor* falseDescriptor = getValueDescriptor(falseThing);

          if (trueDescriptor == nullptr || falseDescriptor == nullptr) {
            if (trueDescriptor == nullptr && trueThing.getKind() != Thing::Kind::UNKNOWN) {
              error(trueClause, "Branches of dynamic conditions must be values.");
            }
            if (falseDescriptor == nullptr && falseThing.getKind() != Thing::Kind::UNKNOWN) {
              error(falseClause, "Branches of dynamic conditions must be values.");
            }
            return Thing::fromUnknown();
          }

          // TODO:  Coerce trueThing and falseThing to a common type and isPointer status.
          ValueDescriptor commonDescriptor = findCommonSupertype(*trueDescriptor, *falseDescriptor);

          Thing commonType = Thing::fromEntity(commonDescriptor.type)

          trueThing = castTo(move(trueThing), commonDescriptor.,
                             variablesUsed, errorLocation(trueClause));

          // TODO:  If both branches are references, we actually want to return a reference...
          return Thing::fromValue(
              move(commonDescriptor),
              BoundExpression::fromTernaryOperator(
                  move(condition.value.expression), move(trueExpression), move(falseExpression)));
        }
      }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(condition), conditionExpression);
    }

    throw "can't get here";
  }

  Thing evaluate(const Expression& expression, VariableUsageSet& variablesUsed) {
    switch (expression.getType()) {
      case Expression::Type::ERROR: {
        for (auto& error: expression.error) {
          errorLocation(expression).error(error);
        }
        return Thing::fromUnknown();
      }

      case Expression::Type::VARIABLE: {
        Maybe<Entity&> binding = scope.lookupBinding(expression.variable);
        if (binding) {
          // For now only the binding's identity is used.
          variablesUsed.addSequential(&*binding, VariableUsageSet::Style::IDENTITY,
                                      errorLocation(expression));
          return Thing::fromReference(&*binding, EntityContext::forLocal(scope));
        } else {
          return error(expression, "Unknown identifier: ", expression.variable);
        }
      }

      case Expression::Type::TUPLE: {
        // TODO
        throw "unimplemented";
      }

      case Expression::Type::LITERAL_INT:
        return Thing::fromInteger(expression.literalInt);

      case Expression::Type::LITERAL_DOUBLE:
        return Thing::fromDouble(expression.literalDouble);

      case Expression::Type::LITERAL_STRING:
        // TODO:  Generate code to construct the string class.
        throw "unimplemented";

      case Expression::Type::LITERAL_ARRAY:
        // TODO:  Generate code to construct the FixedArray class.
        throw "unimplemented";

      case Expression::Type::BINARY_OPERATOR: {
        vector<VariableUsageSet> subVariablesUsed(2);
        Thing left = evaluate(*expression.binaryOperator.left, subVariablesUsed[0]);
        Thing right = evaluate(*expression.binaryOperator.right, subVariablesUsed[1]);
        variablesUsed.merge(move(subVariablesUsed));

        return applyBinaryOperator(move(left), expression.binaryOperator.op, move(right));
      }

      case Expression::Type::PREFIX_OPERATOR:
        return applyPrefixOperator(
            expression.prefixOperator.op,
            evaluate(*expression.prefixOperator.operand, variablesUsed),
            variablesUsed, expression.location);

      case Expression::Type::POSTFIX_OPERATOR:
        return applyPostfixOperator(
            evaluate(*expression.postfixOperator.operand, variablesUsed),
            expression.postfixOperator.op);

      case Expression::Type::TERNARY_OPERATOR: {
        return applyTernaryOperator(
            evaluate(*expression.ternaryOperator.condition, variablesUsed),
            *expression.ternaryOperator.trueClause,
            *expression.ternaryOperator.falseClause,
            variablesUsed, expression.location);
      }

      case Expression::Type::FUNCTION_CALL:
        return callFunction(expression.functionCall, variablesUsed, expression.location);

      case Expression::Type::SUBSCRIPT:
        // TODO
        throw "unimplemented";

      case Expression::Type::MEMBER_ACCESS: {
        // TODO:  Handle style allowance.
        Thing object = evaluate(*expression.memberAccess.object, variablesUsed);
        return getMember(move(object), expression.memberAccess.member,
                         variablesUsed, expression.location);
      }

      case Expression::Type::IMPORT:
      case Expression::Type::LAMBDA:
        // TODO
        throw "unimplemented";
    }

    throw "Can't get here.";
  }

  vector<CxxStatement> compileImperative(const vector<ast::Statement>& statements) {
    vector<CxxStatement> code;

    for (auto& statement: statements) {
      switch (statement.getType()) {
        case ast::Statement::Type::ERROR:
          for (auto& error: statement.error) {
            errorLocation(statement).error(error);
          }
          break;

        case ast::Statement::Type::EXPRESSION: {
          Evaluation eval = evaluate(statement.expression);

          switch (eval.getKind()) {
            case Evaluation::Kind::REFERENCE:
              if (eval.reference.getKind() != Reference::Kind::ANONYMOUS) {
                error(statement, "Statement has no effect.");
              }
              code.push_back(CxxStatement::addSemicolon(
                  move(eval.reference.anonymous.cxxExpression)));
              break;
            case Evaluation::Kind::THING:
              if (eval.thing.getKind() != Thing::Kind::RUNTIME_VALUE) {
                error(statement, "Statement has no effect.");
              }
              code.push_back(CxxStatement::addSemicolon(
                  move(eval.thing.runtimeValue.cxxExpression)));
              break;
          }

          // TODO:  Verify no conflicting variable usage.
          break;
        }

        case ast::Statement::Type::BLOCK: {
          OwnedPtr<Scope> subscope = scope.startBlock();
          CxxStatement cxxStatement(CxxExpression("{"));
          cxxStatement.blocks.emplace_back(
              move(Compiler(*subscope).compileImperative(statement.block)),
              CxxExpression("}"));
          code.push_back(move(cxxStatement));
          break;
        }

        case ast::Statement::Type::DECLARATION: {
          //??
        }
      }
    }

    return move(code);
  }

#endif

private:
  Scope& scope;
  Evaluator<Value, Maybe<Value&>>& evaluator;
  Evaluator<DynamicValue, DynamicPointer>& expressionBuilder;
};

vector<CxxStatement> compileImperative(Scope& scope, const vector<ast::Statement>& statements) {
  return Compiler(scope).compileImperative(statements);
}

}  // namespace compiler
}  // namespace modc
