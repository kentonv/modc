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

#include "errors.h"

#include <ostream>

namespace modc {
namespace errors {

std::ostream& operator<<(std::ostream& os, const LineColumn& lineColumn) {
  if (lineColumn.line == -1) {
    os << "?";
  } else {
    os << lineColumn.line;
    if (lineColumn.column != -1) {
      os << lineColumn.column;
    }
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const Location& location) {
  os << '[' << location.start;
  if (location.end != location.start) {
    os << '-' << location.end;
  }
  os << "]";
  return os;
}

std::ostream& operator<<(std::ostream& os, const Error& error) {
  os << error.location << "\"" << error.message << "\"";
  return os;
}

}  // namespace errors
}  // namespace modc
