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
    map<PointerVariable*, Maybe<DataValue&>> pointerFields;
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
    DataValue* pointer;
  };

  static Rvalue fromData(DataValue&& data);
  static Rvalue fromPointer(DataValue* pointer);
};

}  // namespace compiler
}  // namespace modc

#endif /* KENTONSCODE_MODC_SEMANTICS_VALUE_H_ */
