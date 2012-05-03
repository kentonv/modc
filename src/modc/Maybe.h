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

  inline operator bool() const { return isSet; }
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
        value = move(other.value);
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
        value = other.value;
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

private:
  bool isSet;
  union {
    T value;
  };
};

}  // namespace modc

#endif /* KENTONSCODE_MODC_MAYBE_H_ */
