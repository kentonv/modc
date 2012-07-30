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

#ifndef KENTONSCODE_MODC_SEMANTICS_COMPILER_H_
#define KENTONSCODE_MODC_SEMANTICS_COMPILER_H_

#include <map>
#include <set>
#include <string>
#include <vector>

#include "base/OwnedPtr.h"
#include "../Maybe.h"
#include "../macros.h"
#include "../syntax/ast.h"

#include "value.h"
#include "expression.h"
#include "context.h"
#include "thing.h"
#include "scope.h"

namespace modc {
namespace compiler {

class ErrorLocation {
public:
  explicit ErrorLocation(ast::Expression& expression);

  template <typename... Parts>
  void error(Parts&&... parts);
  void error(errors::Error&& error);
};

class VariableUsageSet {
public:
  enum class Style {
    IDENTITY,
    MUTABLE,
    IMMUTABLE,
    MEMBER_MUTABLE,
    MEMBER_IMMUTABLE,
    ASSIGNMENT
  };

  void addUsage(const PointerConstraints& pointer, Exclusivity exclusivity, ErrorLocation location);

  void addUsage(const LocalVariablePath& variable, Exclusivity exclusivity, ErrorLocation location);

  // TODO:  Does this need context?  We do need to distinguish between the same member of
  //   different parents.  Maybe it just needs a path?
  void addSequential(Variable* variable, Style style, ErrorLocation location);

  // Given a list of VariableUsageSets representing variable usage in a series of parallel
  // operations, check that the usages do not conflict, and then merge them all into this set.
  void merge(vector<VariableUsageSet>&& parallelUsages);

  bool operator==(const VariableUsageSet& other) const {
    return variablesUsed == other.variablesUsed;
  }
  bool operator!=(const VariableUsageSet& other) const {
    return variablesUsed != other.variablesUsed;
  }

private:
  struct Usage {
    Style style;
    ErrorLocation location;

    Usage(Style style, ErrorLocation location): style(style), location(location) {}
  };

  map<Variable*, Usage> variablesUsed;
};

class Compiler {
public:
  Compiler(Scope& scope,
           Evaluator<DataValue, Maybe<DataValue&>>& evaluator,
           Evaluator<DataExpression, PointerExpression>& expressionBuilder)
      : scope(scope), evaluator(evaluator), expressionBuilder(expressionBuilder) {}

  Scope& scope;
  Evaluator<DataValue, Maybe<DataValue&>>& evaluator;
  Evaluator<DataExpression, PointerExpression>& expressionBuilder;

  bool areEqual(const Thing& a, const Thing& b);
  bool areEqual(const EntityName& a, const EntityName& b);
  bool areEqual(const Context& a, const Context& b);
  template <typename T, typename U>
  bool areEqual(const Bound<T>& a, const Bound<U>& b);

  DataConstraints getDefaultConstraints(Type* type);

  DataConstraints getInheritedConstraints(DataConstraints&& parentConstraints,
                                          DataVariable* member);
  PointerConstraints getInheritedConstraints(DataConstraints&& parentConstraints,
                                             PointerVariable* member);

  bool checkPointerPath(const LocalVariablePath& allowedPath, TargetSpecificity allowedSpecificity,
                        const LocalVariablePath& actualPath, TargetSpecificity actualSpecificity);
  void checkConstraints(AdditionalTargets allowed, AdditionalTargets actual,
                        ErrorLocation location);
  void checkConstraints(const DataConstraints& allowed, const DataConstraints& actual,
                        ErrorLocation location);
  void checkConstraints(const PointerConstraints& allowed, const PointerConstraints& actual,
                        ErrorLocation location);

  Maybe<DescribedRvalue> applyDefaultConversion(
      DescribedRvalue&& input, VariableUsageSet& variablesUsed, ErrorLocation location);
  Maybe<Thing> applyDefaultConversion(
      Thing&& input, VariableUsageSet& variablesUsed, ErrorLocation location);

  Maybe<DescribedRvalue> castTo(
      DescribedRvalue&& input, Bound<Type>&& targetType,
      VariableUsageSet& variablesUsed, ErrorLocation location);
  Maybe<DescribedRvalue> castTo(
      Thing&& input, Bound<Type>&& targetType,
      VariableUsageSet& variablesUsed, ErrorLocation location);
  Maybe<DescribedRvalue> castTo(
      Thing&& input, DataDescriptor&& targetDescriptor,
      VariableUsageSet& variablesUsed, ErrorLocation location);

  Maybe<DescribedPointer> castToPointer(DescribedPointer&& input,
                                        Bound<Type>&& targetType,
                                        Exclusivity targetExclusivity,
                                        VariableUsageSet& variablesUsed,
                                        ErrorLocation location);
  Maybe<DescribedPointer> castToPointer(Thing&& input, Bound<Type>&& targetType,
                                        Exclusivity targetExclusivity,
                                        VariableUsageSet& variablesUsed,
                                        ErrorLocation location);
  Maybe<DescribedPointer> castToPointer(
      Thing&& input, PointerDescriptor&& targetDescriptor,
      VariableUsageSet& variablesUsed, ErrorLocation location);

  // Construct a pointer from an lvalue.  In the case of a pointer lvalue, this actually reads
  // the lvalue and returns it, since you can't have a pointer to a pointer.
  //
  // Returns null on error or incomplete information.
  Maybe<DescribedPointer> toPointer(DataVariable* localVariable, ErrorLocation location);
  Maybe<DescribedPointer> toPointer(PointerVariable* localVariable, ErrorLocation location);
  Maybe<DescribedPointer> toPointer(DescribedPointer&& parent, DataVariable* member,
                                    ErrorLocation location);
  Maybe<DescribedPointer> toPointer(DescribedPointer&& parent, PointerVariable* member,
                                    ErrorLocation location);
  Maybe<DescribedPointer> toPointer(Lvalue&& lvalue, ErrorLocation location);

  // Creates a local variable, scoped only to the current statement, and initializes it with the
  // given value.  The input is modified in-place to become a reference to the local instead of
  // an expression.  If the input is already a simple reference to a specific variable then its path
  // is simply returned without creating a new local.
  LocalVariablePath bindTemporary(DescribedData& value);
  LocalVariablePath bindTemporary(DescribedPointer& pointer);

  // Get the type of a member variable given its parent pointer.
  //
  // "parent" may be modified in-place in order to bind it to a local variable -- see
  // bindTemporary().
  Maybe<Thing::ConstrainedType> getMemberType(DescribedData& parent, Variable* member,
                                              ErrorLocation location);
  Maybe<Thing::ConstrainedType> getMemberType(DescribedPointer& parent,
                                              Variable* variable, ErrorLocation location);

  Maybe<DescribedData> getMember(DescribedData&& object, DataVariable* member,
                                 ErrorLocation location);
  Maybe<DescribedPointer> getMember(DescribedData&& object, PointerVariable* member,
                                    ErrorLocation location);
  Maybe<Thing> getMember(Thing&& object, const string& memberName, ErrorLocation location);

  // Input is non-const because it will be annotated.
  Maybe<Thing> evaluate(ast::Expression& expression, VariableUsageSet& variablesUsed);

private:
  // If the thing is an rvalue, return a reference to it.  If the thing is an lvalue, convert it
  // to an rvalue and return a reference to it.  Otherwise, return null.
  Maybe<DescribedRvalue&> asRvalue(Thing& thing, ErrorLocation location);
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_COMPILER_H_ */
