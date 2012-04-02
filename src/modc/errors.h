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

#ifndef KENTONSCODE_MODC_ERRORS_H_
#define KENTONSCODE_MODC_ERRORS_H_

#include <sstream>
#include <string>
#include <utility>

namespace modc {
namespace errors {

using std::string;
using std::move;

inline void writeAll(std::ostream& os) {}

template <typename Part1, typename... Parts>
void writeAll(std::ostream& os, Part1&& part1, Parts&&... parts) {
  os << part1;
  writeAll(os, std::forward<Parts>(parts)...);
}

template <typename... Parts>
string concat(Parts&&... parts) {
  std::ostringstream os;
  writeAll(os, std::forward<Parts>(parts)...);
  return os.str();
}

class Error {
public:
  string message;

  int startOffset;
  int endOffset;

  // TODO:  Suggested fix.

  inline bool operator==(const Error& other) const {
    return message == other.message && startOffset == other.startOffset &&
           endOffset == other.endOffset;
  }
  inline bool operator!=(const Error& other) const { return !(*this == other); }
};

template <typename... Parts>
Error error(int startOffset, int endOffset, Parts... message) {
  Error result;
  result.startOffset = startOffset;
  result.endOffset = endOffset;
  result.message = concat(std::forward<Parts>(message)...);
  return result;
}

std::ostream& operator<<(std::ostream& os, const Error& error);

}  // namespace errors
}  // namespace modc

#endif /* KENTONSCODE_MODC_ERRORS_H_ */
