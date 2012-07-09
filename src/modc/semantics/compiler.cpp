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

  ValueType aliasTemporary(ValueType&& pureData);
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

  Thing errorCantUseHere(Thing&& value, ErrorLocation location) {
    // TODO:  Improve error message.
    location.error("Can't use this thing here.");
    return Thing::fromUnknown();
  }

  ErrorLocation errorLocation(const Expression& expression);

  CxxExpression asCxx(Thing&& value);

  bool compare(const Thing& a, const Thing& b);
  bool compare(const EntityName& a, const EntityName& b);
  bool compare(const Context& a, const Context& b);
  template <typename T, typename U>
  bool compare(const Bound<T>& a, const Bound<U>& b);

  Thing call(Entity& entity, ThingPort& port, Tuple&& parameters);

  bool isComplete(const Value& value);

  // -------------------------------------------------------------------------------------

  // Get the type of a member variable given its parent pointer.
  Maybe<Thing::ConstrainedType> getMemberType(const Thing::DescribedPointer& parent,
                                              Variable* variable, ErrorLocation location) {
    Maybe<Thing::ConstrainedType> result;

    if (parent.staticPointer) {
      result = variable->getType(
          parent.descriptor.targetDescriptor.type.context, *parent.staticPointer);
    }

    if (!result) {
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

  // Construct a pointer from an lvalue.
  //
  // Returns null on error or incomplete information.
  Maybe<Thing::DescribedPointer> toPointer(Thing::Lvalue&& lvalue, ErrorLocation location) {
    if (lvalue.parent) {
      Maybe<Thing::ConstrainedType> type = getMemberType(*lvalue.parent, lvalue.variable, location);
      if (!type) {
        return nullptr;
      }

      PointerDescriptor& parentDesc = lvalue.parent->descriptor;

      if (!type->constraints) {
        vector<ValueConstraints::PossiblePointer> possiblePointers;

        // The member does not declare what pointers it may contain, so inherit from the parent.
        for (auto& possiblePointer: parentDesc.targetDescriptor.constraints.possiblePointers) {
          if (possiblePointer.member.path.empty()) {
            // It's unspecified which member of the parent could contain this pointer, so
            // assume this one could.
            possiblePointers.push_back(move(possiblePointer));
          } else if (possiblePointer.member.path.front() == lvalue.variable) {
            // The parent declared that this possible pointer specifically applies to this
            // member variable, or a further member thereof.  Chop off the first element in the
            // path to make the path relative to this member.
            possiblePointer.member.path.erase(possiblePointer.member.path.begin(),
                                              possiblePointer.member.path.begin() + 1);
            possiblePointers.push_back(move(possiblePointer));
          }
        }

        type->constraints = ValueConstraints(move(possiblePointers),
            parentDesc.targetDescriptor.constraints.additionalPointers);
      }

      assert(type->constraints);

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

      return Thing::DescribedPointer(
          PointerDescriptor(
              ValueDescriptor(move(type->type), move(*type->constraints)),
              parentDesc.exclusivity,
              move(parentDesc.constraints)),
          expressionBuilder.getPointerToMember(move(lvalue.parent->pointer), lvalue.variable),
          evaluator.getPointerToMember(move(lvalue.parent->staticPointer), lvalue.variable));

    } else {
      // The lvalue names a local variable.
      // Read the variable.
      ValueDescriptor descriptor = scope.getVariableDescriptor(lvalue.variable);

      // Form a pointer to the local variable.
      PointerConstraints constraints(
          PointerConstraints::PossibleTarget(
              LocalVariablePath(lvalue.variable),
              TargetSpecificity::EXACT_TARGET));
      return Thing::DescribedPointer(
          PointerDescriptor(move(descriptor), Exclusivity::OWNED, move(constraints)),
          expressionBuilder.makePointerToLocalVariable(lvalue.variable),
          evaluator.makePointerToLocalVariable(lvalue.variable));
    }
  }

  // Construct a pointer from a pointer lvalue.  This actually reads the lvalue and returns it,
  // since you can't have a pointer to a pointer.
  //
  // Returns null on error or incomplete information.
  Maybe<Thing::DescribedPointer> toPointer(Thing::PointerLvalue&& lvalue, ErrorLocation location) {
    if (lvalue.parent) {
      Maybe<Thing::ConstrainedType> type = getMemberType(*lvalue.parent, lvalue.variable, location);
      if (!type) {
        return nullptr;
      }

      PointerDescriptor& parentDesc = lvalue.parent->descriptor;

      // Have to get PointerConstraints...  If the member has no declared constraints we just
      // derive them from the parent pointer.  Otherwise:
      // - For each possible target of the parent pointer
      //   - If it's an exact target, use it as the object instance to build a context and
      //     call variable.getConstraints().
      //   - Otherwise, if the pointer has constraints, they are either inner pointers, in
      //     which case we keep the same fuzzy constraint, or they are pointers to something in
      //     the type's context (without the instance), so we can figure those out...  hmm...
      //
      // What it seems like we need to do here is record, for each PossibleTarget, the
      // PointerConstraints to replace it with when that target goes out of scope, or at least
      // enough info to deduce that later.  Remember that the first pointer in the path always goes
      // out of scope first.  Problem with deducing constraints later is that if the parent targets
      // are fuzzy then we'll just inherit them without recording which particular pointers within
      // were accessed, so we won't know where to substitute later.


      for (auto& possibleParent: parentDesc.constraints.possibleTargets) {
        possibleParent.specificity
      }



      if (parentDesc.targetDescriptor.constraints.possiblePointers) {
        for (auto& possiblePointer: *parentDesc.targetDescriptor.constraints.possiblePointers) {
          // Does this possible pointer apply to us?
          if (possiblePointer.member.path.empty() ||
              possiblePointer.member.path.front() == lvalue.variable) {
            assert(possiblePointer.member.path.size() <= 1);
            if (memberDesc->constraints.exclusivity <= possiblePointer.exclusivity) {
              memberDesc->constraints.possibleTargets.push_back(possiblePointer.target);
            }
          }
        }
      }

      // Restrict pointer constraints based on parent pointer.
      memberDesc->constraints.exclusivity = std::min(memberDesc->constraints.exclusivity,
                                                     parentDesc.constraints.exclusivity);
      for (auto& varToLock: parentDesc.constraints.lockOnUse) {
        if (parentDesc.constraints.exclusivity == Exclusivity::EXCLUSIVE) {
          memberDesc->constraints.derivedFrom.push_back(varToLock);
        }
        varToLock.member.path.push_back(lvalue.variable);
        memberDesc->constraints.lockOnUse.push_back(move(varToLock));
      }

      return Thing::DescribedPointer(move(*memberDesc),
          expressionBuilder.readMember(move(lvalue.parent->pointer), lvalue.variable),
          evaluator.readMember(move(lvalue.parent->staticPointer), lvalue.variable));

    } else {
      // The lvalue names a local variable.
      PointerDescriptor descriptor = scope.getVariableDescriptor(lvalue.variable);

      return Thing::DescribedPointer(move(descriptor),
          expressionBuilder.readLocalVariable(lvalue.variable),
          evaluator.readLocalVariable(lvalue.variable));
    }
  }













#if 0

  // Given a pointer to an object, get a pointer to one of its members.  If the member is itself
  // a pointer, that pointer is returned (not a pointer to a pointer, which is anyway not
  // supported).
  Thing::Rvalue pointerToMemberPointer(
      ValueDescriptor::Pointer&& descriptor, DynamicValue&& value,
      Variable* member) {
    // To decide possibleTargets:
    // - If there are no non-owned pointers in the chain, possibleTargets = the lvalue.
    // - Otherwise, find the last non-owned pointer in the chain, and copy its possibleTargets.
    //   - For exact targets, append the rest of the chain.  (As of this writing, exact targets
    //     aren't supported.)
    //   - For fuzzy targets, don't append anything; they remain fuzzy.
    //   - Also copy mayTargetCaller.

    // To decide derivedFrom:
    // - If the desired exclusivity is identity, it's empty.
    // - Otherwise, find the last non-owned pointer in the chain.
    //   - If it is an exclusive pointer, then it goes in derivedFrom.
    //   - Otherwise, derivedFrom is empty.
  }

  Maybe<Thing::Rvalue> coerceToPointer(
      Thing&& input, VariableUsageSet& variablesUsed,
      Exclusivity exclusivity, ErrorLocation location) {
    Maybe<Thing::Rvalue> value = coerceToValue(move(input), location);
    if (!value) {
      return nullptr;
    }

    switch (value->descriptor.getKind()) {
      case ValueDescriptor::Kind::PURE_DATA:
        if (exclusivity != Exclusivity::SHARED) {
          location.error("Cannot form pointer to temporary value.");
        }

        if (value->value.expression.getKind() == BoundExpression::Kind::CONSTANT) {
          value->value.expression.constant =
              evaluator.aliasTemporary(move(value->value.expression.constant));
          assert(value->value.partialValue.getKind() == Value::Kind::UNKNOWN);
        } else {
          value->value.expression =
              BoundExpression::fromAliasTemporary(move(value->value.expression));
          value->value.partialValue =
              evaluator.aliasTemporary(move(value->value.partialValue));
        }
        // TODO:  Fix descriptor.
        return move(value);

      case ValueDescriptor::Kind::POINTER:
        if (exclusivity > value->descriptor.pointer.constraints.exclusivity) {
          location.error("Cannot convert pointer to more-exclusive kind.");
        }

        value->descriptor.pointer.constraints.exclusivity = exclusivity;
        return move(value);

      case ValueDescriptor::Kind::LVALUE: {
        Thing::Rvalue pointer;

        if (value->descriptor.lvalue.isDynamic) {
          accumPointerConstraints = move(value->descriptor.lvalue.dynamicRoot.constraints);
          accumValueConstraints =
              move(value->descriptor.lvalue.dynamicRoot.targetDescriptor.constraints);
        } else {
          accumPointerConstraints.exclusivity = OWNED;
          accumPointerConstraints.mayTargetCaller = false;
          accumPointerConstraints.possibleTargets.emplace_back(
              value->descriptor.lvalue.staticRoot, MemberPath());

          ValueDescriptor baseDescriptor = scope.getVariableDescriptor(
              Value::Lvalue(value->descriptor.lvalue.staticRoot, MemberPath()));
          switch (baseDescriptor.getKind()) {
            case ValueDescriptor::Kind::PURE_DATA:
              accumValueConstraints = baseDescriptor.pureData.constraints;
              break;
            case ValueDescriptor::Kind::POINTER:
              // todo
              break;
            case ValueDescriptor::Kind::LVALUE:
              throw "Variable's value can't be an lvalue.";
          }
        }

        // If the lvalue is static, add it to variablesUsed.  (If it is dynamic, the original
        // source should already have been added to variablesUsed.)
        //
        // Exception:  If the lvalue contains any shared pointers, there's no need.
        if (!value->descriptor.lvalue.isDynamic &&
            accumPointerConstraints.exclusivity > Exclusivity::SHARED) {
          Value::Lvalue temp(value->descriptor.lvalue.staticRoot,
                             move(value->descriptor.lvalue.memberPath));
          variablesUsed.addUsage(temp, exclusivity);
          value->descriptor.lvalue.memberPath = move(temp.path);
        }

        // TODO:  Check that the final exclusivity is compatible.

        break;
      }
    }
  }

  // Dereferences the input if it is a pointer, or just returns it unchanged otherwise.  Reports an
  // error if the input is not a value.
  Thing dereference(Thing&& input, VariableUsageSet& variablesUsed, ErrorLocation location,
                    VariableUsageSet::Style style = VariableUsageSet::Style::IMMUTABLE) {
    switch (input.getKind()) {
      case Thing::Kind::UNKNOWN:
        return Thing::fromUnknown();

      case Thing::Kind::VALUE:
        variablesUsed.addUsage(input.value.descriptor, style);
        if (input.value.descriptor.isPointer()) {
          input.value.descriptor.style = ast::Style::VALUE;
          if (input.value.value.expression.getKind() == BoundExpression::Kind::CONSTANT) {
            input.value.value.expression.constant =
                evaluator.readPointer(move(input.value.value.expression.constant));
            assert(input.value.value.partialValue.getKind() == Value::Kind::UNKNOWN);
          } else {
            input.value.value.expression =
                BoundExpression::fromReadPointer(move(input.value.value.expression));
            input.value.value.partialValue =
                evaluator.readPointer(move(input.value.value.partialValue));
          }
          #error "need to apply possibleTargets"
          return move(input);
        } else {
          // Already dereferenced.
          return move(input);
        }

      case Thing::Kind::UNKNOWN_TYPE:
      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(input), location);
    }

    throw "can't get here";
  }



















  // -------------------------------------------------------------------------------------

  enum class CastMode {
    CHECK_CONSTRAINTS_ONLY,
    UPCAST_ONLY,
    FULL_CONVERSION
  };

  Thing checkConstraints(Thing::DescribedValue&& input,
                         const ValueDescriptor& desiredDescirptor,
                         VariableUsageSet& variablesUsed, ErrorLocation location);
  Thing checkConstraints(Thing&& input,
                         const ValueDescriptor& desiredDescirptor,
                         VariableUsageSet& variablesUsed, ErrorLocation location);

  Thing castTo(Thing::DescribedValue&& input, const ValueDescriptor& desiredDescriptor,
               VariableUsageSet& variablesUsed, ErrorLocation location, CastMode mode) {
    ThingPort inputTypePort = scope.makePortFor(input.descriptor.type.context);

    if (compare(input.descriptor.type, desiredDescriptor.type)) {
      return checkConstraints(move(input), desiredDescriptor, variablesUsed, location);
    }

    if (mode != CastMode::CHECK_CONSTRAINTS_ONLY) {
      // Can we upcast?
      if (dynamic_cast<const Interface*>(desiredDescriptor.type.entity) != nullptr) {
        // Casting to an interface.  See if upcast is available.
        Maybe<ImplementedInterface&> impl = input.descriptor.type.entity->findImplementedInterface(
            inputTypePort, desiredDescriptor.type);
        if (impl) {
          input.descriptor.type = desiredDescriptor.type;
          if (input.value.expression.getKind() == BoundExpression::Kind::CONSTANT) {
            input.value.expression.constant =
                evaluator.upcast(move(input.value.expression.constant), &*impl);
            assert(input.value.partialValue.getKind() == Value::Kind::UNKNOWN);
          } else {
            input.value.expression = BoundExpression::fromUpcast(
                move(input.value.expression), &*impl);
            input.value.partialValue =
                evaluator.upcast(move(input.value.partialValue), &*impl);
          }

          return castTo(move(input), desiredDescriptor, variablesUsed, location,
                        CastMode::CHECK_CONSTRAINTS_ONLY);
        }
      }

      if (mode == CastMode::FULL_CONVERSION) {
        // We prefer conversions declared by the source type over constructors on the destination
        // type because conversions are necessarily more specific.
        Maybe<Function&> conversion = input.descriptor.type.entity->lookupConversion(
            inputTypePort, desiredDescriptor.type);
        if (conversion) {
          return castTo(
              call(*conversion, inputTypePort,
                   Tuple::fromSingleValue(Thing::fromValue(move(input)))),
              desiredDescriptor, variablesUsed, location, CastMode::UPCAST_ONLY);
        }

        // Try calling implicit constructor.
        Maybe<Overload&> ctor = desiredDescriptor.type.entity->getImplicitConstructor();
        if (ctor) {
          ThingPort desiredTypePort = scope.makePortFor(desiredDescriptor.type.context);
          return castTo(
              call(*ctor, desiredTypePort,
                   Tuple::fromSingleValue(Thing::fromValue(move(input)))),
              desiredDescriptor, variablesUsed, location, CastMode::UPCAST_ONLY);
        }
      }
    }

    location.error("No conversion.");
    return Thing::fromValue(ValueDescriptor(desiredDescriptor), BoundExpression::fromUnknown());
  }

  // Casts the thing to match the given ValueDescriptor.  This includes dereferencing if the
  // ValueDescriptor has style = VALUE.  Reports an error if the input is not a value.
  //
  // Note that if the requested output style is MUTABLE_ALIAS, the input must be a mutable alias
  // and only "upcasting" is allowed.
  Thing castTo(Thing&& input, const ValueDescriptor& desiredDescriptor,
               VariableUsageSet& variablesUsed, ErrorLocation location, CastMode mode) {
    assert(!desiredDescriptor.isPointer());

    switch (input.getKind()) {
      case Thing::Kind::UNKNOWN:
        return Thing::fromValue(ValueDescriptor(desiredDescriptor), BoundExpression::fromUnknown());

      case Thing::Kind::VALUE:
        return castTo(move(input.value), desiredDescriptor, variablesUsed, location, mode);

      case Thing::Kind::UNKNOWN_TYPE:
      case Thing::Kind::ENTITY:
        errorCantUseHere(move(input), location);

        // Assume that the cast is *supposed* to succeed.
        return Thing::fromValue(ValueDescriptor(desiredDescriptor), BoundExpression::fromUnknown());

      case Thing::Kind::TUPLE: {
        Maybe<Overload&> ctor = desiredDescriptor.type.entity->getImplicitConstructor();
        if (ctor) {
          ThingPort desiredTypePort = scope.makePortFor(desiredDescriptor.type.context);
          return castTo(
              call(*ctor, desiredTypePort, move(input.tuple)),
              desiredDescriptor, variablesUsed, location, CastMode::UPCAST_ONLY);
        } else {
          location.error("No conversion from tuple to target type.");
          return Thing::fromValue(ValueDescriptor(desiredDescriptor),
                                  BoundExpression::fromUnknown());
        }
      }
    }
  }

  // -------------------------------------------------------------------------------------
  // Looking up a member.

  // Called when a member is *dereferenced*, to decide its constraints based on its declared
  // constraints and the parent object's constraints.
  void inheritValueConstraints(ValueDescriptor& member, const ValueConstraints& parent) {
    if (member.isAlias()) {
      if (!member.constraints.possibleTargets) {
        // Inherit possibleTargets from the parent object's transitiveAliases.
        if (parent.transitiveAliases) {
          // We can decide this member pointer's possible targets based on the parent's transitive
          // aliases.
          member.constraints.possibleTargets.init();

          int possibleAliasTypes = 0;  // a bitfield
          switch (member.style) {
            case ast::Style::VALUE:
            case ast::Style::CONSTANT:
            case ast::Style::HEAP_VALUE:
              throw "can't get here; it's an alias";

            case ast::Style::IMMUTABLE_REFERENCE:
              // An immutable alias member could be pointing at almost anything.
              possibleAliasTypes |= 1 << (int) ValueConstraints::AliasType::IMMUTABLE;
              possibleAliasTypes |= 1 << (int) ValueConstraints::AliasType::MUTABLE;
              possibleAliasTypes |= 1 << (int) ValueConstraints::AliasType::ENTANGLED;
              break;
            case ast::Style::MUTABLE_REFERENCE:
              // A mutable alias member can only be pointing at mutable targets.
              possibleAliasTypes |= 1 << (int) ValueConstraints::AliasType::MUTABLE;
              break;
            case ast::Style::ENTANGLED_REFERENCE:
              // An entangled alias member can be pointing at mutable or entangled targets?
              possibleAliasTypes |= 1 << (int) ValueConstraints::AliasType::MUTABLE;
              possibleAliasTypes |= 1 << (int) ValueConstraints::AliasType::ENTANGLED;
              break;
          }

          for (auto& alias: *parent.transitiveAliases) {
            if (possibleAliasTypes & (1 << (int) alias.second)) {
              member.constraints.possibleTargets->push_back(alias.first);
            }

            // Dummy switch to make sure that if another AliasType is added we update this code.
            switch (alias.second) {
              case ValueConstraints::AliasType::IDENTITY:
              case ValueConstraints::AliasType::IMMUTABLE:
              case ValueConstraints::AliasType::MUTABLE:
              case ValueConstraints::AliasType::ENTANGLED:
                break;
            }
          }
        }
      }
      // TODO:  We could theoretically fill in member.constraints.transitiveAliases if all of the
      //   possible targets have well-defined transitiveAliases...  but it's unclear if this is
      //   useful or desirable.
    } else {
      // We are dealing with an owned object (normal member or heap pointer), so just propagate
      // the transitiveAliases from the parent (unless explict aliases were declared).
      // possibleTargets does not make sense here.
      if (!member.constraints.transitiveAliases) {
        member.constraints.transitiveAliases = parent.transitiveAliases;
      }
    }
  }

  void inheritPointerConstraints(ValueDescriptor& member, const ValueConstraints& parent) {
    // We're forming a pointer to a member from a pointer to the object.  So, the object
    // pointer's possible targets should simply be inherited.  Transitive aliases should
    // also just be inherited *unless* either:
    // - the new target member is a non-alias and declares its own transitive aliases,
    //   which should simply be kept
    // - the new target member is an alias and declares possible targets, in which case
    //   those should be turned into our transitiveAliases.
    // TODO:  Do we really want to inherit anything from the target value?  Will its
    // constraints be applied in due course anyway, once the pointer is dereferenced?

    member.constraints.possibleTargets = parent.possibleTargets;
  }

  Thing getMember(Thing&& object, const string& memberName,
                  VariableUsageSet& variablesUsed, ErrorLocation location) {
    switch (object.getKind()) {
      case Thing::Kind::UNKNOWN:
        return Thing::fromUnknown();

      case Thing::Kind::UNKNOWN_TYPE:
        // We have no idea what constructors (or enum values) the type has, even if we know its
        // superclasses.
        return Thing::fromUnknown();

      case Thing::Kind::VALUE: {
        // Determine what member we're looking up.
        Maybe<Entity&> member =
            object.value.descriptor.type.entity->lookupMemberOfInstance(memberName);
        if (!member) {
          location.error("No such member.");
          return Thing::fromUnknown();
        }

        if (Variable* variable = dynamic_cast<Variable*>(&*member)) {
          Bound<Variable, DynamicValue> boundMember(
              variable, Context<DynamicValue>(
                  object.value.descriptor.type.entity, move(object.value.value)));
          DynamicValue& objval = boundMember.context.params->at(0);

          if (objval.expression.getKind() == BoundExpression::Kind::CONSTANT) {
            assert(objval.partialValue.getKind() == Value::Kind::UNKNOWN);
            DynamicValue memberValue(BoundExpression::fromConstant(
                evaluator.getMember(move(objval.expression.constant), variable)));
            if (object.value.descriptor.isPointer()) {
              // Parent is a pointer to a known variable which may have in-scope constraints.
              return Thing::fromValue(move(scope.getVariableDescriptor(boundMember)),
                                      move(memberValue));
            } else {
              // Object is a temporary.  Merge the variable's declared constraints with the
              // parent's constraints.
              // TODO:  Since the object is a constant, we technically know all of its
              //   contents.  Should we just use that knowledge instead of tracking constraints
              //   as if the object were unknown?
              ThingPort port = scope.makePortFor(boundMember.context);
              ValueDescriptor memberDescriptor = variable->getDeclaredDescriptor(port);
              inheritValueConstraints(memberDescriptor, object.value.descriptor.constraints);
              return Thing::fromValue(move(memberDescriptor), move(memberValue));
            }
            throw "can't get here";
          } else {
            DynamicValue memberValue(
                BoundExpression::fromMemberAccess(move(objval.expression), variable),
                evaluator.getMember(move(objval.partialValue), variable));
            // TODO:  Check if new partialValue is fully-known and convert to a constant?  Watch
            //   out for missed side effects.

            if (object.value.descriptor.isPointer()) {
              // We're forming a pointer to a member from a pointer to the object.  So, the object
              // pointer's possible targets should simply be inherited.  Transitive aliases should
              // also just be inherited *unless* either:
              // - the new target member is a non-alias and declares its own transitive aliases,
              //   which should simply be kept
              // - the new target member is an alias and declares possible targets, in which case
              //   those should be turned into our transitiveAliases.
              // TODO:  Do we really want to inherit anything from the target value?  Will its
              // constraints be applied in due course anyway, once the pointer is dereferenced?

              object.value.value = move(memberValue);
              #error "shit, boundMember was clobbered to build memberValue"
              ThingPort port = scope.makePortFor(boundMember.context);
              object.value.descriptor.type = variable->getType(port);
              #error "above isn't done at all"


            } else {
              // The object is a dynamic temporary.
              ThingPort port = scope.makePortFor(boundMember.context);
              ValueDescriptor memberDescriptor = variable->getDeclaredDescriptor(port);
              inheritValueConstraints(memberDescriptor, object.value.descriptor.constraints);
              return Thing::fromValue(move(memberDescriptor), move(memberValue));
            }
          }
        } else {
          // TODO:  Handle functions and types.
          #error "implement"
        }
      }

      case Thing::Kind::ENTITY:
        // If a type, look for a constructor / enum constant.
        #error "TODO"

      case Thing::Kind::TUPLE:
        // Tuples do not have members.
        errorCantUseHere(move(object), location);
        return Thing::fromUnknown();
    }
  }

  // -------------------------------------------------------------------------------------
  // Calling a function.

  Thing callFunction(const Expression::FunctionCall& functionCall,
                     VariableUsageSet& variablesUsed, ErrorLocation location) {
    vector<VariableUsageSet> subVariablesUsed;
    subVariablesUsed.reserve(functionCall.parameters.size());

    subVariablesUsed.emplace_back();
    Thing function = dereference(
        evaluate(*functionCall.function, subVariablesUsed.back()),
        subVariablesUsed.back(), location);

    vector<TupleElement> boundParameters;
    for (auto& parameter: functionCall.parameters) {
      subVariablesUsed.emplace_back();
      // TODO:  Keyword args in AST.
      boundParameters.emplace_back(nullptr, parameter.styleAllowance,
                                   evaluate(*parameter.expression, subVariablesUsed.back()));
    }

    // TODO:  In theory we should wait until after casts before doing this merge, since casts could
    //   have side effects (but shouldn't).  Note OTOH that binding alias parameters counts as a
    //   *sequential* use because the use actually happens during the function call, which is
    //   obviously after all parameters have been computed.
    variablesUsed.merge(move(subVariablesUsed));

    switch (function.getKind()) {
      case Thing::Kind::ERROR:
        return move(function);

      case Thing::Kind::BOOLEAN:
      case Thing::Kind::INTEGER:
      case Thing::Kind::DOUBLE:
      case Thing::Kind::TUPLE:
      case Thing::Kind::ARRAY:
        return error(*functionCall.function, "Not a function");

      case Thing::Kind::ENTITY: {
        Entity* entity = function.entity.entity;
        if (Overload* overload = dynamic_cast<Overload*>(entity)) {
          Maybe<Entity&> resolved = overload->resolve(boundParameters);
          if (!resolved) {
            location.error("No such overload.");
            return Thing::fromUnknown();
          }
          entity = &*resolved;
        }

        if (Function* functionEntity = dynamic_cast<Function*>(entity)) {
          // It's a function!
          const vector<Function::ParameterSpec>& specs = functionEntity->getParameters();
          map<const Function::ParameterSpec*, TupleElement*> matchedSpecs;
          int pos = 0;
          for (TupleElement& input: boundParameters) {
            if (input.name) {
              const Function::ParameterSpec* match = nullptr;
              for (auto& spec: specs) {
                if (compare(*input.name, spec.entity->getName())) {
                  match = &spec;
                  break;
                }
              }

              if (match == nullptr) {
                // TODO: Better error.
                location.error("No parameter matching name.");
                return Thing::fromUnknown();
              } else if (!matchedSpecs.insert(std::make_pair(match, &input)).second) {
                // TODO: Better error.
                location.error("Named parameter already satisfied.");
                return Thing::fromUnknown();
              }
            } else {
              int index = pos++;
              if (index >= specs.size()) {
                // TODO: Better error.
                location.error("Too many parameters.");
                return Thing::fromUnknown();
              } else if (!matchedSpecs.insert(std::make_pair(&specs[index], &input)).second) {
                // TODO: Better error.
                location.error("Positional parameter already satisfied by name.");
                return Thing::fromUnknown();
              }
            }
          }

          for (auto& spec: specs) {
            if (dynamic_cast<Variable*>(spec.entity) != nullptr ||
                dynamic_cast<Alias*>(spec.entity) != nullptr) {
              auto iter = matchedSpecs.find(&spec);
              if (iter != matchedSpecs.end()) {
                // TODO:  Cast types and match references.  See also TODO above about variablesUsed.
                #error "TODO"
                function.entity.context.params.push_back(iter->second->value);
              } else if (spec.defaultValue) {
                function.entity.context.params.push_back(*spec.defaultValue);
              } else {
                // TODO: Better error.
                location.error("Missing parameter.");
                return Thing::fromUnknown();
              }
            }
          }

          functionEntity->call(move(function.entity.context));
        }
      }

      case Thing::Kind::DYNAMIC_VALUE:
        // TODO:  Since functions must always resolve at compile time, the only possibility here
        //   is that the value is a class with operator().  Deal with that.
        throw "unimplemented";

      case Thing::Kind::REFERENCE:
      case Thing::Kind::DYNAMIC_REFERENCE:
        throw "can't happen; dereferenced";
    }

    throw "can't get here";
  }

  // -------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------

























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
          // TODO(kenton):  Need to create sub-scopes or something to track the fact that only one
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

          // TODO(kenton):  If both branches are references, we actually want to return a reference...
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

          // TODO(kenton):  Verify no conflicting variable usage.
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