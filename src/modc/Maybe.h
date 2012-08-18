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

#ifndef KENTONSCODE_MODC_MAYBE_H_
#define KENTONSCODE_MODC_MAYBE_H_

#include <utility>

namespace modc {

using std::move;

template <typename T>
T any();

template <typename T>
class Maybe {
public:
  Maybe(): isSet(false) {}
  Maybe(T&& t)
      : isSet(true) {
    new (&value) T(move(t));
  }
  Maybe(const T& t)
      : isSet(true) {
    new (&value) T(t);
  }
  Maybe(Maybe&& other)
      : isSet(other.isSet) {
    if (isSet) {
      new (&value) T(move(other.value));
    }
  }
  Maybe(const Maybe& other)
      : isSet(other.isSet) {
    if (isSet) {
      new (&value) T(other.value);
    }
  }
  Maybe(std::nullptr_t): isSet(false) {}

  // TODO:  Should be noexcept(noexcept(T::~T)), but GCC 4.7 doesn't seem to understand that.
  ~Maybe() noexcept {
    if (isSet) {
      value.~T();
    }
  }

  template <typename... Params>
  inline void init(Params&&... params) {
    if (isSet) {
      value.~T();
    }
    isSet = true;
    new (&value) T(std::forward(params)...);
  }

  inline T& operator*() { return value; }
  inline const T& operator*() const { return value; }
  inline T* operator->() { return &value; }
  inline const T* operator->() const { return &value; }

  inline Maybe& operator=(Maybe&& other) {
    if (&other != this) {
      if (isSet) {
        value.~T();
      }
      isSet = other.isSet;
      if (isSet) {
        new (&value) T(move(other.value));
      }
    }
    return *this;
  }

  inline Maybe& operator=(const Maybe& other) {
    if (&other != this) {
      if (isSet) {
        value.~T();
      }
      isSet = other.isSet;
      if (isSet) {
        new (&value) T(other.value);
      }
    }
    return *this;
  }

  bool operator==(const Maybe& other) const {
    if (isSet == other.isSet) {
      if (isSet) {
        return value == other.value;
      } else {
        return true;
      }
    }
    return false;
  }
  inline bool operator!=(const Maybe& other) const { return !(*this == other); }

  inline bool operator==(std::nullptr_t) const { return !isSet; }
  inline bool operator!=(std::nullptr_t) const { return isSet; }

  template <typename Func>
  auto morph(const Func& func) -> Maybe<decltype(func(any<T&&>()))> {
    if (isSet) {
      return func(move(value));
    } else {
      return nullptr;
    }
  }

  template <typename Func>
  auto transform(const Func& func) const -> Maybe<decltype(func(any<const T&>()))> {
    if (isSet) {
      return func(value);
    } else {
      return nullptr;
    }
  }

private:
  bool isSet;
  union {
    T value;
  };
};

template <typename T>
class Maybe<T&> {
public:
  Maybe(): ptr(nullptr) {}
  Maybe(T& t): ptr(&t) {}
  Maybe(std::nullptr_t): ptr(nullptr) {}

  ~Maybe() noexcept {}

  inline operator bool() const { return ptr != nullptr; }
  inline T& operator*() { return *ptr; }
  inline const T& operator*() const { return *ptr; }
  inline T* operator->() { return ptr; }
  inline const T* operator->() const { return ptr; }

  inline bool operator==(const Maybe& other) const { return ptr == other.ptr; }
  inline bool operator!=(const Maybe& other) const { return ptr != other.ptr; }

private:
  T* ptr;
};

}  // namespace modc

#endif /* KENTONSCODE_MODC_MAYBE_H_ */
