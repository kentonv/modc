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

#ifndef KENTONSCODE_MODC_SEMANTICS_VALUE_H_
#define KENTONSCODE_MODC_SEMANTICS_VALUE_H_

#include <map>
#include <string>
#include <vector>

#include "base/OwnedPtr.h"
#include "../Maybe.h"
#include "../macros.h"

namespace modc {
namespace compiler {

using namespace ekam;
using std::string;
using std::vector;
using std::map;

class DataVariable;
class PointerVariable;
class Pointer;
class Variable;

class DataValue {
public:
  UNION_TYPE_BOILERPLATE(DataValue);

  enum class Kind {
    // A placeholder value that is not yet known, because it is computed at runtime.
    // Note that this should NOT be used in any other situation -- e.g. do NOT use this for values
    // that aren't known due to already-reported compile errors, or for meta parameters that are
    // not yet known when sanity-checking an uninstantiated meta function.
    UNKNOWN,

    // Literals.
    BOOLEAN,
    INTEGER,
    DOUBLE,

    // Aggregates.
    OBJECT,
    ARRAY,
  };

  Kind getKind() { return kind; }

  struct Object {
    map<DataVariable*, DataValue> fields;
    map<PointerVariable*, Pointer> pointerFields;
  };

  union {
    bool boolean;
    int integer;
    double double_;

    Object object;
    vector<DataValue> array;
  };

  static DataValue fromUnknown();

  static DataValue fromBoolean(bool value);
  static DataValue fromInteger(int value);
  static DataValue fromDouble(double value);

private:
  Kind kind;
};

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

class PointerManager {
public:
  RESOURCE_TYPE_BOILERPLATE(PointerManager);
  virtual ~PointerManager();

  virtual const DataValue& get(const LocalVariablePath& path) = 0;
  virtual void set(const LocalVariablePath& path, DataValue&& value) = 0;
};

class Pointer {
public:
  UNION_TYPE_BOILERPLATE(Pointer);

  enum class Kind {
    UNKNOWN,
    RAW,
    MANAGED
  };

  Kind getKind() { return kind; }

  struct ManagedPointer {
    PointerManager* manager;
    LocalVariablePath path;

    VALUE_TYPE2(ManagedPointer, PointerManager*, manager, LocalVariablePath&&, path);
  };

  union {
    DataValue* raw;
    ManagedPointer managed;
  };

  static Pointer fromUnknown();
  static Pointer fromRaw(DataValue* pointer);
  static Pointer fromManaged(PointerManager* manager, LocalVariablePath&& path);

private:
  Kind kind;
};

class Rvalue {
public:
  UNION_TYPE_BOILERPLATE(Rvalue);

  enum class Kind {
    DATA,
    POINTER
  };

  Kind getKind();

  union {
    DataValue data;
    Pointer pointer;
  };

  static Rvalue fromData(DataValue&& data);
  static Rvalue fromPointer(Pointer&& pointer);
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_VALUE_H_ */
