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

#include "statements.h"

#include <utility>
#include <functional>

#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"

namespace modc {
namespace statements {

// =============================================================================

template <typename T>
void Destroy(T& obj) {
  obj.~T();
}

#define FOR_ALL_STATEMENTS(HANDLE) \
  HANDLE(ERROR, error, std::vector<errors::Error>) \
  HANDLE(EXPRESSION, expression, Expression) \
  HANDLE(RETURN, return_, Expression) \
  HANDLE(BREAK, break_, string)

Statement::Statement(Statement&& other): type(other.type) {
  switch (type) {
#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
      FOR_ALL_STATEMENTS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT

    default:  // temporary
      break;
  }
}

Statement::Statement(const Statement& other): type(other.type) {
  switch (type) {
#define COPY_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(other.NAME); \
      break;
      FOR_ALL_STATEMENTS(COPY_CONSTRUCT)
#undef COPY_CONSTRUCT

    default:  // temporary
      break;
  }
}

Statement::~Statement() {
  switch (type) {
#define DESTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      Destroy(NAME); \
      break;
      FOR_ALL_STATEMENTS(DESTRUCT)
#undef DESTRUCT

    default:  // temporary
      break;
  }
}

Statement& Statement::operator=(Statement&& other) {
  // Lazy.
  this->~Statement();
  new(this) Statement(move(other));
  return *this;
}

Statement& Statement::operator=(const Statement& other) {
  // Lazy.
  this->~Statement();
  new(this) Statement(other);
  return *this;
}

bool Statement::operator==(const Statement& other) const {
  if (type == other.type) {
    switch (type) {
#define COMPARE(ID, NAME, TYPE) \
      case Type::ID: \
        return NAME == other.NAME;
        FOR_ALL_STATEMENTS(COMPARE)
#undef MOVE_CONSTRUCT

      default:  // temporary
        break;
    }
  }

  return false;
}

}  // namespace statements
}  // namespace modc
