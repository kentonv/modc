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

#ifndef KENTONSCODE_MODC_UNION_H_
#define KENTONSCODE_MODC_UNION_H_

#include <new>
#include <utility>
#include <type_traits>

namespace modc {

template <typename T>
void NewAt(T& dst, const T& src) {
  new (&dst) T(src);
}

template <typename T>
void NewAt(T& dst, T&& src) {
  new (&dst) T(std::move(src));
}

// typename std::enable_if<std::is_destructible<T>, void>::type

template <typename T>
void Destroy(T& obj) {
  obj.~T();
}

#define UNION_CONSTRUCT_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    case _Union_##UNION_NAME::MEMBER_NAME: \
      new (&this->MEMBER_NAME) MEMBER_TYPE; \
      break;
#define UNION_DESTROY_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    case _Union_##UNION_NAME::MEMBER_NAME: \
      ::modc::Destroy(this->MEMBER_NAME); \
      break;
#define UNION_COPY_CONSTRUCT_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    case _Union_##UNION_NAME::MEMBER_NAME: \
      new (&this->MEMBER_NAME) MEMBER_TYPE(other.MEMBER_NAME); \
      break;
#define UNION_MOVE_CONSTRUCT_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    case _Union_##UNION_NAME::MEMBER_NAME: \
      new (&this->MEMBER_NAME) MEMBER_TYPE(::std::move(other.MEMBER_NAME)); \
      break;
#define UNION_COPY_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    case _Union_##UNION_NAME::MEMBER_NAME: \
      this->MEMBER_NAME = other.MEMBER_NAME; \
      break;
#define UNION_MOVE_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    case _Union_##UNION_NAME::MEMBER_NAME: \
      this->MEMBER_NAME = ::std::move(other.MEMBER_NAME); \
      break;

#define UNION_DECLARE_ENUM(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    MEMBER_NAME,
#define UNION_DECLARE_MEMBER(UNION_NAME, MEMBER_NAME, MEMBER_TYPE) \
    MEMBER_TYPE MEMBER_NAME;

#define UNION_DECLARE(UNION_NAME, EXPAND_MEMBERS) \
    enum class _Union_##UNION_NAME { \
      EXPAND_MEMBERS(UNION_DECLARE_ENUM) \
    } \
    \
    _Union_##UNION_NAME UNION_NAME##_case;\
    \
    union { \
      EXPAND_MEMBERS(UNION_DECLARE_MEMBER) \
    };

#define UNION_COPY_CONSTRUCT()

// Fuck, this isn't working.  Never mind.

}  // namespace modc

#endif /* KENTONSCODE_MODC_UNION_H_ */
