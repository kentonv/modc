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

#ifndef KENTONSCODE_MODC_SEMANTICS_EXPRESSION_H_
#define KENTONSCODE_MODC_SEMANTICS_EXPRESSION_H_

#include "value.h"
#include "../syntax/ast.h"

namespace modc {
namespace compiler {

class Variable;
class Function;
class ImplementedInterface;

class PointerExpression;
class RvalueExpression;

class DataExpression {
public:
  UNION_TYPE_BOILERPLATE(DataExpression);

  enum class Kind {
    CONSTANT,
    LOCAL_VARIABLE,

    // Note that the arithmetic operators here are strictly built-in.  Overloaded operators would
    // have been converted to function calls.
    BINARY_OPERATOR,
    PREFIX_OPERATOR,
    POSTFIX_OPERATOR,
    TERNARY_OPERATOR,

    METHOD_CALL,

    // Given a pure ("temporary") object, produce a member value.
    READ_MEMBER,

    // Given a pointer input, read its value.
    READ_POINTER,
  };

  Kind getKind() const { return kind; }

  struct BinaryOperator {
    ast::BinaryOperator op;
    Indirect<DataExpression> left;
    Indirect<DataExpression> right;

    VALUE_TYPE3(BinaryOperator, ast::BinaryOperator, op, DataExpression&&, left,
                DataExpression&&, right);
  };

  struct PrefixOperator {
    ast::PrefixOperator op;
    Indirect<DataExpression> operand;

    VALUE_TYPE2(PrefixOperator, ast::PrefixOperator, op, DataExpression&&, operand);
  };

  struct PostfixOperator {
    Indirect<DataExpression> operand;
    ast::PostfixOperator op;

    VALUE_TYPE2(PostfixOperator, DataExpression&&, operand, ast::PostfixOperator, op);
  };

  struct TernaryOperator {
    Indirect<DataExpression> condition;
    Indirect<DataExpression> trueClause;
    Indirect<DataExpression> falseClause;

    VALUE_TYPE3(TernaryOperator, DataExpression&&, condition, DataExpression&&, trueClause,
                DataExpression&&, falseClause);
  };

  struct MethodCall {
    Function* method;

    Indirect<PointerExpression> object;

    // Just the variable parameters, since we've already matched them up against the function
    // signature.
    vector<RvalueExpression> parameters;

    VALUE_TYPE3(MethodCall, Function*, method, PointerExpression&&, object,
                vector<RvalueExpression>&&, parameters);
  };

  struct ReadMember {
    Indirect<DataExpression> object;
    DataVariable* member;

    VALUE_TYPE2(ReadMember, DataExpression&&, object, DataVariable*, member);
  };

  union {
    DataValue constant;  // Cannot contain any RUNTIME_VALUE, transitively.
    DataVariable* localVariable;

    BinaryOperator binaryOperator;
    PrefixOperator prefixOperator;
    PostfixOperator postfixOperator;
    TernaryOperator ternaryOperator;

    MethodCall methodCall;
    ReadMember readMember;

    Indirect<DataExpression> readPointer;
  };

  static DataExpression fromConstant(DataValue&& value);
  static DataExpression fromLocalVariable(DataVariable* variable);

  static DataExpression fromBinaryOperator(ast::BinaryOperator op, DataExpression&& left,
                                           DataExpression&& right);
  static DataExpression fromPrefixOperator(ast::PrefixOperator op, DataExpression&& exp);
  static DataExpression fromPostfixOperator(DataExpression&& exp, ast::PostfixOperator op);
  static DataExpression fromTernaryOperator(DataExpression&& condition,
                                            DataExpression&& trueClause,
                                            DataExpression&& falseClause);

  static DataExpression fromMethodCall(Function* method, PointerExpression&& object,
                                       vector<RvalueExpression>&& parameters);
  static DataExpression fromReadMember(DataExpression&& object, DataVariable* member);

  static DataExpression fromReadPointer(PointerExpression&& pointer);

private:
  Kind kind;

  DataExpression(Kind kind): kind(kind) {}
};

class PointerExpression {
public:
  UNION_TYPE_BOILERPLATE(PointerExpression);

  enum class Kind {
    LOCAL_VARIABLE,

    TERNARY_OPERATOR,

    METHOD_CALL,
    SUBSCRIPT,

    // Given a pure ("temporary") object, read a member pointer.
    READ_MEMBER,

    // Given a pointer, read a member pointer.
    READ_POINTER_MEMBER,

    // Given a pointer, produce a pointer to a value member.
    POINTER_TO_MEMBER,

    // Upcast to an interface.
    UPCAST,
  };

  Kind getKind() const { return kind; }

  struct TernaryOperator {
    Indirect<DataExpression> condition;
    Indirect<PointerExpression> trueClause;
    Indirect<PointerExpression> falseClause;

    VALUE_TYPE3(TernaryOperator, DataExpression&&, condition, PointerExpression&&, trueClause,
                PointerExpression&&, falseClause);
  };

  struct MethodCall {
    Function* method;

    Indirect<PointerExpression> object;

    // Just the variable parameters, since we've already matched them up against the function
    // signature.
    vector<RvalueExpression> parameters;

    VALUE_TYPE3(MethodCall, Function*, method, PointerExpression&&, object,
                vector<RvalueExpression>&&, parameters);
  };

  struct Subscript {
    // Container must strictly be a built-in array.  Calls to an overloaded operator[] on a class
    // would have been converted to MethodCall.
    Indirect<PointerExpression> container;
    Indirect<DataExpression> subscript;

    VALUE_TYPE2(Subscript, PointerExpression&&, container, DataExpression&&, subscript);
  };

  struct ReadMember {
    Indirect<DataExpression> object;
    PointerVariable* member;

    VALUE_TYPE2(ReadMember, DataExpression&&, object, PointerVariable*, member);
  };

  struct ReadPointerMember {
    Indirect<PointerExpression> object;
    PointerVariable* member;

    VALUE_TYPE2(ReadPointerMember, PointerExpression&&, object, PointerVariable*, member);
  };

  struct PointerToMember {
    Indirect<PointerExpression> object;
    Variable* member;

    VALUE_TYPE2(PointerToMember, PointerExpression&&, object, Variable*, member);
  };

  struct Upcast {
    Indirect<PointerExpression> object;
    ImplementedInterface* interface;

    VALUE_TYPE2(Upcast, PointerExpression&&, object, ImplementedInterface*, interface);
  };

  union {
    PointerVariable* localVariable;

    TernaryOperator ternaryOperator;

    MethodCall methodCall;
    Subscript subscript;
    ReadMember readMember;
    ReadPointerMember readPointerMember;
    PointerToMember pointerToMember;
    Upcast upcast;
  };

  static PointerExpression fromLocalVariable(PointerVariable* variable);

  static PointerExpression fromTernaryOperator(DataExpression&& condition,
                                               PointerExpression&& trueClause,
                                               PointerExpression&& falseClause);

  static PointerExpression fromMethodCall(Function* method, PointerExpression&& object,
                                          vector<RvalueExpression>&& parameters);
  static PointerExpression fromSubscript(PointerExpression&& container, DataExpression&& key);
  static PointerExpression fromPointerToMember(PointerExpression&& object, Variable* member);
  static PointerExpression fromUpcast(PointerExpression&& object, ImplementedInterface* interface);

private:
  Kind kind;

  PointerExpression(Kind kind): kind(kind) {}
};

class RvalueExpression {
public:
  UNION_TYPE_BOILERPLATE(RvalueExpression);

  enum Kind {
    DATA,
    POINTER,
  };

  Kind getKind();

  union {
    DataExpression data;
    PointerExpression pointer;
  };
};

template <typename DataType, typename PointerType>
class Evaluator {
public:
  virtual ~Evaluator() {}

  virtual DataType readLocalVariable(DataVariable* variable) = 0;
  virtual PointerType readLocalVariable(PointerVariable* variable) = 0;
  virtual PointerType makePointerToLocalVariable(DataVariable* variable) = 0;

  virtual DataType readPointer(PointerType&& pointer) = 0;

  virtual DataType readMember(DataType&& object, DataVariable* member) = 0;
  virtual PointerType readMember(DataType&& object, PointerVariable* member) = 0;
  virtual PointerType readMember(PointerType&& object, PointerVariable* member) = 0;
  virtual PointerType getPointerToMember(PointerType&& object, DataVariable* member) = 0;
  virtual PointerType upcast(PointerType&& object, ImplementedInterface* interface) = 0;
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_EXPRESSION_H_ */
