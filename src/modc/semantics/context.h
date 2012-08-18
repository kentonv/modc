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

#ifndef KENTONSCODE_MODC_SEMANTICS_CONTEXT_H_
#define KENTONSCODE_MODC_SEMANTICS_CONTEXT_H_

#include <map>
#include <string>
#include <vector>
#include <assert.h>

#include "../Maybe.h"
#include "../macros.h"
#include "value.h"

namespace modc {
namespace compiler {

using namespace ekam;
using std::string;
using std::vector;
using std::map;

class ConstrainedType;
class Context;
class Variable;
class Port;

struct MemberPath {
  // TODO:  What about array subscripts?
  vector<Variable*> path;

  VALUE_TYPE1(MemberPath, vector<Variable*>&&, path);
  MemberPath() {}

  bool isPrefix(const MemberPath& other) const;

  // These return nullptr if the path contained an UNKNOWN before the last component.  They
  // assert-fail if the path contained an unset optional member, since you were supposed to verify
  // that that wasn't the case separately.
  Maybe<const DataValue&> readFrom(const DataValue& value) const;
  Maybe<DataValue&> readFrom(DataValue& value) const;

  void append(const MemberPath& other);
};

struct LocalVariablePath {
  Variable* root;
  MemberPath member;

  VALUE_TYPE2(LocalVariablePath, Variable*, root, MemberPath&&, member);

  LocalVariablePath(Variable* root): root(root) {}

  bool isPrefix(const LocalVariablePath& other) const;
};

// A list of bindings for context variables, e.g. "this" and type parameters.  A particular Context
// object is specific to some scope, as it defines all of the context variables visible in that
// scope.  Context bindings are ordered from outermost to innermost -- e.g. for an inner class, the
// outer class's "this" binding comes before the inner class's "this" binding.
//
// A Context may be only partially filled in -- in particular, some prefix of the context may be
// omitted.  This means that the compiler doesn't have bindings for that prefix at the moment.  E.g.
// if the compiler is compiling a particular method of a class, "this" is not bound because it is
// only known at runtime.  In fact, a Context object typically represents a delta from the scope
// currently being compiled to some other scope.
class Context {
public:
  class Binding {
  public:
    UNION_TYPE_BOILERPLATE(Binding);

    enum class Kind {
      DATA,
      POINTER
    };

    Kind getKind() const;

    union {
      DataValue data;  // Must be complete!  No UNKNOWNs allowed.
      LocalVariablePath pointer;
      // TODO:  Integer expressions.
    };

    static Binding fromData(DataValue&& value);
    static Binding fromPointer(LocalVariablePath&& pointer);
  };

  // Number of bindings at the beginning of the context which are not bound.
  size_t unboundCount;

  // Bindings following the unbound.
  vector<Binding> bindings;

  VALUE_TYPE2(Context, size_t, unboundCount, vector<Binding>&&, bindings);

  static Context fromEmpty() { return Context(0, vector<Binding>()); }
};

template <typename EntityType>
struct Bound {
  EntityType* entity;
  Context context;

  VALUE_TYPE2(Bound, EntityType*, entity, Context&&, context);

  template <typename OtherEntity>
  Bound(Bound<OtherEntity>&& other)
      : entity(other.entity), context(move(other.context)) {}
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_CONTEXT_H_ */
