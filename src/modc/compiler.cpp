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
#include "CodePrinter.h"

namespace modc {
namespace compiler {

using std::move;
using ast::Expression;
using ast::Statement;

class Evaluator {
public:
  Value readPointer(Value&& pointer);
};

class Compiler {
public:
  Compiler(Context& context, Scope& scope): context(context), scope(scope) {}

  CxxExpression asCxx(Thing&& value);

  bool compare(const Thing& a, const Thing& b);
  bool compare(const EntityName& a, const EntityName& b);

  Value evaluate(const BoundExpression& expression);

  Thing errorCantUseHere(Thing&& value, errors::Location location) {
    // TODO:  Improve error message.
    context.error(errors::error(location, "Can't use this thing here."));
    return Thing::fromUnknown();
  }


  Thing dereference(Thing&& input, VariableUsageSet& variablesUsed, errors::Location location,
                    VariableUsageSet::Style style = VariableUsageSet::Style::IMMUTABLE) {
    switch (input.getKind()) {
      case Thing::Kind::UNKNOWN:
        // Update the descriptor, if there is one and it is a value.
        if (input.unknown) {
          if (input.unknown->getKind() != ThingDescriptor::Kind::VALUE) {
            return errorCantUseHere(move(input), location);
          }
          variablesUsed.addUsage(input.unknown->value, style);
          switch (input.unknown->value.style) {
            case ast::Style::VALUE:
            case ast::Style::CONSTANT:
              // Already dereferenced.
              break;

            case ast::Style::IMMUTABLE_REFERENCE:
            case ast::Style::MUTABLE_REFERENCE:
            case ast::Style::ENTANGLED_REFERENCE:
            case ast::Style::HEAP_VALUE:
              input.unknown->value.style = ast::Style::VALUE;
              break;
          }
        }
        return move(input);

      case Thing::Kind::VALUE:
        variablesUsed.addUsage(input.value.descriptor, style);
        switch (input.value.descriptor.style) {
          case ast::Style::VALUE:
          case ast::Style::CONSTANT:
            // Already dereferenced.
            return move(input);

          case ast::Style::IMMUTABLE_REFERENCE:
          case ast::Style::MUTABLE_REFERENCE:
          case ast::Style::ENTANGLED_REFERENCE:
          case ast::Style::HEAP_VALUE:
            input.value.descriptor.style = ast::Style::VALUE;
            if (input.value.expression.getKind() == BoundExpression::Kind::CONSTANT) {
              input.value.expression.constant =
                  evaluator.readPointer(move(input.value.expression.constant));
              assert(!input.value.partialValue);
            } else {
              input.value.expression =
                  BoundExpression::fromReadPointer(move(input.value.expression));
              if (input.value.partialValue) {
                input.value.partialValue = evaluator.readPointer(move(*input.value.partialValue));
              }
            }
            return move(input);
        }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(input), location);
    }

    throw "can't get here";
  }

  Thing castTo(Thing&& input, const Thing& type,
               VariableUsageSet& variablesUsed, errors::Location location);


  ValueDescriptor findCommonSupertype(const ValueDescriptor& type1, const ValueDescriptor& type2);

  const ValueDescriptor* getValueDescriptor(const Thing& value) {
    switch (value.getKind()) {
      case Thing::Kind::UNKNOWN:
        // Update the descriptor, if there is one and it is a value.
        if (value.unknown) {
          assert(value.unknown->getKind() == ThingDescriptor::Kind::VALUE);
          return &value.unknown->value;
        } else {
          return nullptr;
        }

      case Thing::Kind::VALUE:
        return &value.value.descriptor;

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return nullptr;
    }

    throw "can't get here";
  }

  Maybe<ThingDescriptor> getThingDescriptor(const Thing& value);

  Thing applyBinaryOperator(const Expression& leftExpression, ast::BinaryOperator op,
                            const Expression& rightExpression,
                            VariableUsageSet& variablesUsed, errors::Location location) {
    vector<VariableUsageSet> subVariablesUsed(2);

    Thing left = dereference(
        evaluate(leftExpression, variablesUsed), subVariablesUsed[0], leftExpression.location);
    Thing right = dereference(
        evaluate(rightExpression, variablesUsed), subVariablesUsed[1], rightExpression.location);

    variablesUsed.merge(move(subVariablesUsed), context);

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
      context.error(errors::error(location, "Operands do not support this operator."));
      return Thing::fromUnknown();
    }

    if (left.getKind() == Thing::Kind::UNKNOWN || right.getKind() == Thing::Kind::UNKNOWN) {
      // One operand is unknown, so result must be unknown.
    }
  }

  Thing applyPrefixOperator(ast::PrefixOperator op, const Expression& operandExpression,
                            VariableUsageSet& variablesUsed, errors::Location location) {
    Thing operand = evaluate(operandExpression, variablesUsed);

    switch (operand.getKind()) {
      case Thing::Kind::UNKNOWN:
        // Update the descriptor, if there is one and it is a value.
        if (operand.unknown) {
          if (operand.unknown->getKind() != ThingDescriptor::Kind::VALUE) {
            return errorCantUseHere(move(operand), operandExpression.location);
          }

          Maybe<Function&> method = operand.unknown->value.type->lookupPrefixOperator(op);
          if (!method) {
            context.error(errors::error(location, "Operand does not support this operator."));
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
          context.error(errors::error(location, "Operand does not support this operator."));
          return Thing::fromUnknown();
        }

        assert(method->getParameters().empty());

        return method->call(move(operand.value), vector<Thing::DynamicValue>());
      }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(operand), operandExpression.location);
    }
  }

  Thing applyPostfixOperator(const Expression& operandExpression, ast::PostfixOperator op,
                             VariableUsageSet& variablesUsed, errors::Location location) {
    Thing operand = evaluate(operandExpression, variablesUsed);

    switch (operand.getKind()) {
      case Thing::Kind::UNKNOWN:
        // Update the descriptor, if there is one and it is a value.
        if (operand.unknown) {
          if (operand.unknown->getKind() != ThingDescriptor::Kind::VALUE) {
            return errorCantUseHere(move(operand), operandExpression.location);
          }

          Maybe<Function&> method = operand.unknown->value.type->lookupPostfixOperator(op);
          if (!method) {
            context.error(errors::error(location, "Operand does not support this operator."));
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
          context.error(errors::error(location, "Operand does not support this operator."));
          return Thing::fromUnknown();
        }

        assert(method->getParameters().empty());

        return method->call(move(operand.value), vector<Thing::DynamicValue>());
      }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(operand), operandExpression.location);
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
              context.error(errors::error(trueClause.location,
                  "Branches of dynamic conditions must be values."));
            }
            if (falseDescriptor == nullptr && falseThing.getKind() != Thing::Kind::UNKNOWN) {
              context.error(errors::error(falseClause.location,
                  "Branches of dynamic conditions must be values."));
            }
            return Thing::fromUnknown();
          }

          // TODO:  Coerce trueThing and falseThing to a common type and isPointer status.
          ValueDescriptor commonDescriptor = findCommonSupertype(*trueDescriptor, *falseDescriptor);

          Thing commonType = Thing::fromEntity(commonDescriptor.type)

          trueThing = castTo(move(trueThing), commonDescriptor.,
                             variablesUsed, trueClause.location);

          // TODO(kenton):  If both branches are references, we actually want to return a reference...
          return Thing::fromValue(
              move(commonDescriptor),
              BoundExpression::fromTernaryOperator(
                  move(condition.value.expression), move(trueExpression), move(falseExpression)));
        }
      }

      case Thing::Kind::ENTITY:
      case Thing::Kind::TUPLE:
        return errorCantUseHere(move(condition), conditionExpression.location);
    }

    throw "can't get here";
  }

  Maybe<Entity&> lookupMemberEntity(const ValueDescriptor& descriptor);
  Maybe<Entity&> lookupMemberEntity(const ThingDescriptor& descriptor);
  Maybe<Entity&> lookupMemberEntity(const Thing& thing);

  Thing getMember(Thing&& object, const string& memberName,
                  VariableUsageSet& variablesUsed, errors::Location location) {
    Maybe<Entity&> member = lookupMemberEntity(object);

    if (member) {
      return Thing::fromReference(&*member, EntityContext::forMember(move(object)));
    } else {
      context.error(errors::error(location, "No such member: ", memberName));
      return Thing::fromError();
    }
  }

  Thing callFunction(const Expression::FunctionCall& functionCall,
                     VariableUsageSet& variablesUsed, errors::Location location) {
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
    variablesUsed.merge(move(subVariablesUsed), context);

    switch (function.getKind()) {
      case Thing::Kind::ERROR:
        return move(function);

      case Thing::Kind::BOOLEAN:
      case Thing::Kind::INTEGER:
      case Thing::Kind::DOUBLE:
      case Thing::Kind::TUPLE:
      case Thing::Kind::ARRAY:
        context.error(errors::error(functionCall.function->location, "Not a function"));
        return Thing::fromError();

      case Thing::Kind::ENTITY: {
        Entity* entity = function.entity.entity;
        if (Overload* overload = dynamic_cast<Overload*>(entity)) {
          Maybe<Entity&> resolved = overload->resolve(boundParameters);
          if (!resolved) {
            context.error(errors::error(location, "No such overload."));
            return Thing::fromError();
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
                context.error(errors::error(location, "No parameter matching name."));
                return Thing::fromError();
              } else if (!matchedSpecs.insert(std::make_pair(match, &input)).second) {
                // TODO: Better error.
                context.error(errors::error(location, "Named parameter already satisfied."));
                return Thing::fromError();
              }
            } else {
              int index = pos++;
              if (index >= specs.size()) {
                // TODO: Better error.
                context.error(errors::error(location, "Too many parameters."));
                return Thing::fromError();
              } else if (!matchedSpecs.insert(std::make_pair(&specs[index], &input)).second) {
                // TODO: Better error.
                context.error(errors::error(location,
                    "Positional parameter already satisfied by name."));
                return Thing::fromError();
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
                context.error(errors::error(location, "Missing parameter."));
                return Thing::fromError();
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

  Thing evaluate(const Expression& expression, VariableUsageSet& variablesUsed) {
    switch (expression.getType()) {
      case Expression::Type::ERROR: {
        for (auto& error: expression.error) {
          context.error(error);
        }
        return Thing::fromError();
      }

      case Expression::Type::VARIABLE: {
        Maybe<Entity&> binding = scope.lookupBinding(expression.variable);
        if (binding) {
          // For now only the binding's identity is used.
          variablesUsed.addSequential(&*binding, VariableUsageSet::Style::IDENTITY,
                                      expression.location);
          return Thing::fromReference(&*binding, EntityContext::forLocal(scope));
        } else {
          context.error(errors::error(expression.location, "Unknown identifier: ",
                                      expression.variable));
          return Thing::fromError();
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
        variablesUsed.merge(move(subVariablesUsed), context);

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
            context.error(error);
          }
          break;

        case ast::Statement::Type::EXPRESSION: {
          Evaluation eval = evaluate(statement.expression);

          switch (eval.getKind()) {
            case Evaluation::Kind::REFERENCE:
              if (eval.reference.getKind() != Reference::Kind::ANONYMOUS) {
                context.error(errors::error(statement.location, "Statement has no effect."));
              }
              code.push_back(CxxStatement::addSemicolon(
                  move(eval.reference.anonymous.cxxExpression)));
              break;
            case Evaluation::Kind::THING:
              if (eval.thing.getKind() != Thing::Kind::RUNTIME_VALUE) {
                context.error(errors::error(statement.location, "Statement has no effect."));
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
              move(Compiler(context, *subscope).compileImperative(statement.block)),
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

private:
  Context& context;
  Scope& scope;
  Evaluator& evaluator;
};

vector<CxxStatement> compileImperative(Context& context, Scope& scope,
                                       const vector<ast::Statement>& statements) {
  return Compiler(context, scope).compileImperative(statements);
}

}  // namespace compiler
}  // namespace modc
