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

#ifndef KENTONSCODE_MODC_MACROS_H_
#define KENTONSCODE_MODC_MACROS_H_

namespace modc {

template <typename T>
void destroy(T& obj) {
  obj.~T();
}

#define VALUE_TYPE0(TYPENAME) \
  bool operator==(const TYPENAME& other) const { return true; } \
  bool operator!=(const TYPENAME& other) const { return false; }

#define VALUE_TYPE1(TYPENAME, TYPE1, VAR1) \
  TYPENAME(TYPE1 VAR1): VAR1(move(VAR1)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define VALUE_TYPE2(TYPENAME, TYPE1, VAR1, TYPE2, VAR2) \
  TYPENAME(TYPE1 VAR1, TYPE2 VAR2) \
      : VAR1(move(VAR1)), VAR2(move(VAR2)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1 && VAR2 == other.VAR2; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define VALUE_TYPE3(TYPENAME, TYPE1, VAR1, TYPE2, VAR2, TYPE3, VAR3) \
  TYPENAME(TYPE1 VAR1, TYPE2 VAR2, TYPE3 VAR3) \
      : VAR1(move(VAR1)), VAR2(move(VAR2)), VAR3(move(VAR3)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1 && VAR2 == other.VAR2 && VAR3 == other.VAR3; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define VALUE_TYPE4(TYPENAME, TYPE1, VAR1, TYPE2, VAR2, TYPE3, VAR3, TYPE4, VAR4) \
  TYPENAME(TYPE1 VAR1, TYPE2 VAR2, TYPE3 VAR3, TYPE4 VAR4) \
      : VAR1(move(VAR1)), VAR2(move(VAR2)), VAR3(move(VAR3)), VAR4(move(VAR4)) {} \
  bool operator==(const TYPENAME& other) const { \
    return VAR1 == other.VAR1 && VAR2 == other.VAR2 && VAR3 == other.VAR3 && VAR4 == other.VAR4; \
  } \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define RESOURCE_TYPE_BOILERPLATE(TYPENAME) \
  TYPENAME(const TYPENAME& other) = delete; \
  TYPENAME(TYPENAME&& other) = delete; \
  TYPENAME(const TYPENAME&& other) = delete; \
  TYPENAME& operator=(TYPENAME& other) = delete; \
  TYPENAME& operator=(const TYPENAME& other) = delete; \
  TYPENAME& operator=(TYPENAME&& other) = delete; \
  TYPENAME& operator=(const TYPENAME&& other) = delete; \
  inline bool operator==(const TYPENAME& other) const { return this == &other; } \
  inline bool operator!=(const TYPENAME& other) const { return this != &other; }

#define UNION_TYPE_BOILERPLATE(TYPENAME) \
  TYPENAME(TYPENAME&& other); \
  TYPENAME(const TYPENAME& other); \
  ~TYPENAME() noexcept; \
  TYPENAME& operator=(TYPENAME&& other); \
  TYPENAME& operator=(const TYPENAME& other); \
  bool operator==(const TYPENAME& other) const; \
  bool operator!=(const TYPENAME& other) const { return !(*this == other); }

#define UNION_MEMBER_MOVE_CONSTRUCT(ID, NAME, TYPE) \
  case Type::ID: \
    new (&NAME) TYPE(move(other.NAME)); \
    break;
#define UNION_EXTRA_MOVE_CONSTRUCT(NAME, SHOULD_COMPARE) NAME(move(other.NAME)),
#define UNION_MEMBER_COPY_CONSTRUCT(ID, NAME, TYPE) \
  case Type::ID: \
    new (&NAME) TYPE(other.NAME); \
    break;
#define UNION_EXTRA_COPY_CONSTRUCT(NAME, SHOULD_COMPARE) NAME(other.NAME),
#define UNION_MEMBER_DESTRUCT(ID, NAME, TYPE) \
  case Type::ID: \
    destroy(NAME); \
    break;
#define UNION_EXTRA_DESTRUCT(NAME, SHOULD_COMPARE)
#define UNION_MEMBER_COMPARE(ID, NAME, TYPE) \
  case Type::ID: \
    return NAME == other.NAME;
#define UNION_EXTRA_COMPARE(NAME, SHOULD_COMPARE) \
  if ((SHOULD_COMPARE) && NAME != other.NAME) return false;

#define UNION_IMPLEMENT(SCOPE, TYPENAME, MEMBERS, EXTRAS) \
  SCOPE TYPENAME::TYPENAME(TYPENAME&& other) \
      : EXTRAS(UNION_EXTRA_MOVE_CONSTRUCT) type(other.type) { \
    switch (type) { \
      MEMBERS(UNION_MEMBER_MOVE_CONSTRUCT) \
    } \
  } \
  \
  SCOPE TYPENAME::TYPENAME(const TYPENAME& other) \
      : EXTRAS(UNION_EXTRA_COPY_CONSTRUCT) type(other.type) { \
    switch (type) { \
      MEMBERS(UNION_MEMBER_COPY_CONSTRUCT) \
    } \
  } \
  \
  SCOPE TYPENAME::~TYPENAME() noexcept { \
    EXTRAS(UNION_EXTRA_DESTRUCT) \
    switch (type) { \
      MEMBERS(UNION_MEMBER_DESTRUCT) \
    } \
  } \
  \
  SCOPE TYPENAME& SCOPE TYPENAME::operator=(TYPENAME&& other) { \
    this->~TYPENAME(); \
    new(this) TYPENAME(move(other)); \
    return *this; \
  } \
  \
  SCOPE TYPENAME& SCOPE TYPENAME::operator=(const TYPENAME& other) { \
    this->~TYPENAME(); \
    new(this) TYPENAME(other); \
    return *this; \
  } \
  \
  bool SCOPE TYPENAME::operator==(const TYPENAME& other) const { \
    EXTRAS(UNION_EXTRA_COMPARE) \
    if (type == other.type) { \
      switch (type) { \
        MEMBERS(UNION_MEMBER_COMPARE) \
      } \
    } \
    \
    return false; \
  }






#define KINDUNION_MEMBER_MOVE_CONSTRUCT(ID, NAME, TYPE) \
  case Kind::ID: \
    new (&NAME) TYPE(move(other.NAME)); \
    break;
#define KINDUNION_EXTRA_MOVE_CONSTRUCT(NAME, SHOULD_COMPARE) NAME(move(other.NAME)),
#define KINDUNION_MEMBER_COPY_CONSTRUCT(ID, NAME, TYPE) \
  case Kind::ID: \
    new (&NAME) TYPE(other.NAME); \
    break;
#define KINDUNION_EXTRA_COPY_CONSTRUCT(NAME, SHOULD_COMPARE) NAME(other.NAME),
#define KINDUNION_MEMBER_DESTRUCT(ID, NAME, TYPE) \
  case Kind::ID: \
    destroy(NAME); \
    break;
#define KINDUNION_EXTRA_DESTRUCT(NAME, SHOULD_COMPARE)
#define KINDUNION_MEMBER_COMPARE(ID, NAME, TYPE) \
  case Kind::ID: \
    return NAME == other.NAME;
#define KINDUNION_EXTRA_COMPARE(NAME, SHOULD_COMPARE) \
  if ((SHOULD_COMPARE) && NAME != other.NAME) return false;

#define KINDUNION_IMPLEMENT(SCOPE, TYPENAME, MEMBERS, EXTRAS) \
  SCOPE TYPENAME::TYPENAME(TYPENAME&& other) \
      : EXTRAS(UNION_EXTRA_MOVE_CONSTRUCT) kind(other.kind) { \
    switch (kind) { \
      MEMBERS(UNION_MEMBER_MOVE_CONSTRUCT) \
    } \
  } \
  \
  SCOPE TYPENAME::TYPENAME(const TYPENAME& other) \
      : EXTRAS(UNION_EXTRA_COPY_CONSTRUCT) kind(other.kind) { \
    switch (kind) { \
      MEMBERS(UNION_MEMBER_COPY_CONSTRUCT) \
    } \
  } \
  \
  SCOPE TYPENAME::~TYPENAME() noexcept { \
    EXTRAS(UNION_EXTRA_DESTRUCT) \
    switch (kind) { \
      MEMBERS(UNION_MEMBER_DESTRUCT) \
    } \
  } \
  \
  SCOPE TYPENAME& SCOPE TYPENAME::operator=(TYPENAME&& other) { \
    this->~TYPENAME(); \
    new(this) TYPENAME(move(other)); \
    return *this; \
  } \
  \
  SCOPE TYPENAME& SCOPE TYPENAME::operator=(const TYPENAME& other) { \
    this->~TYPENAME(); \
    new(this) TYPENAME(other); \
    return *this; \
  } \
  \
  bool SCOPE TYPENAME::operator==(const TYPENAME& other) const { \
    EXTRAS(UNION_EXTRA_COMPARE) \
    if (kind == other.kind) { \
      switch (kind) { \
        MEMBERS(UNION_MEMBER_COMPARE) \
      } \
    } \
    \
    return false; \
  }

}  // namespace modc

#endif /* KENTONSCODE_MODC_MACROS_H_ */
