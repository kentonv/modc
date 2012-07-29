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

namespace modc {
namespace compiler {

using namespace ekam;
using std::string;
using std::vector;
using std::map;

struct MemberPath {
  // TODO:  What about array subscripts?
  vector<Variable*> path;

  VALUE_TYPE1(MemberPath, vector<Variable*>&&, path);
  MemberPath() {}

  bool isPrefix(const MemberPath& other) const;
};

struct LocalVariablePath {
  Variable* root;
  MemberPath member;

  VALUE_TYPE2(LocalVariablePath, Variable*, root, MemberPath&&, member);

  LocalVariablePath(Variable* root): root(root) {}

  bool isPrefix(const LocalVariablePath& other) const;
};

struct Context {
  class Binding {
  public:
    UNION_TYPE_BOILERPLATE(Binding);

    enum class Kind {
      DATA,
      POINTER,
      INTEGER_EXPRESSION
    };

    Kind getKind();

    union {
      DataValue data;
      LocalVariablePath pointer;
      // TODO:  Integer expressions.
    };

    static Binding fromPointer(LocalVariablePath&& pointer);
  };

  // How many Bindings are visible in this Context?
  vector<Binding>::size_type depth;

  // The set of bindings which are not shared with the underlay.  These are the last bindings in
  // the context.
  vector<Binding> suffix;

  // Bindings not covered by the suffix can be found in the underlay.  If suffix.size() == depth,
  // then the underlay may be null.
  const Context* underlay;

  const Binding& operator[](vector<Binding>::size_type index) const {
    assert(index < depth);
    vector<Binding>::size_type overlayStart = depth - suffix.size();
    if (index < overlayStart) {
      return (*underlay)[index];
    } else {
      return suffix[index - overlayStart];
    }
  }

  Context(vector<Binding>::size_type depth, vector<Binding>&& suffix, const Context& underlay)
      : depth(depth), suffix(move(suffix)), underlay(&underlay) {
    assert(suffix.size() <= depth);
  }
  Context(vector<Binding>&& suffix)
      : depth(suffix.size()), suffix(move(suffix)), underlay(nullptr) {}
  Context(const Context& underlay, Binding&& suffix)
      : depth(underlay.depth + 1), underlay(&underlay) {
    this->suffix.push_back(move(suffix));
  }

  bool operator==(const Context& other) const;
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
