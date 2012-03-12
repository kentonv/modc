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

#ifndef KENTONSCODE_MODC_TOKENS_H_
#define KENTONSCODE_MODC_TOKENS_H_

#include <string>
#include <vector>
#include <stdint.h>

#include "base/OwnedPtr.h"

namespace modc {
namespace tokens {

using ekam::OwnedPtr;
using ekam::newOwned;
using std::string;

enum class Keyword {

};

class Tree;

Tree parse(const string& text);

Tree keyword(Keyword keyword);
Tree identifier(string&& name);
Tree identifier(const string& name);
Tree sequence(std::vector<Tree>&& elements);
Tree sequence(const std::vector<Tree>& elements);

Tree block(std::vector<Tree>&& statements);
Tree block(const std::vector<Tree>& statements);
Tree bracketed(Tree&& content);
Tree bracketed(const Tree& content);
Tree parenthesized(Tree&& content);
Tree parenthesized(const Tree& content);
Tree list(std::vector<Tree>&& elements);
Tree list(const std::vector<Tree>& elements);

Tree literal(int value);
Tree literal(double value);
Tree literal(string&& value);
Tree literal(const string& value);

class Tree {
public:
  enum class Type {
    KEYWORD,
    IDENTIFIER,
    SEQUENCE,

    BLOCK,
    BRACKETED,
    PARENTHESIZED,
    LIST,

    LITERAL_INT,
    LITERAL_DOUBLE,
    LITERAL_STRING

    // TODO:  Empty lines, comments -- should these be allowed only between statements?
  };

  Tree(Tree&& other);
  Tree(const Tree& other);
  ~Tree();

  Tree& operator=(Tree&& other);
  Tree& operator=(const Tree& other);

  const Type type;

  union {
    // A keyword or symbol.
    Keyword keyword;

    // An identifier, e.g. a variable name.
    string identifier;

    // A sequence of tokens with no particular separator.
    std::vector<Tree> sequence;

    // A block in curly-braces consisting of statements terminated by either a semicolon or a
    // sub-block.
    std::vector<Tree> block;

    // Expression in square brackets.
    OwnedPtr<Tree> bracketed;

    // Expression in parentheses.
    OwnedPtr<Tree> parenthesized;

    // A comma-separated list.
    std::vector<Tree> list;

    // A literal value.
    int literalInt;
    double literalDouble;
    string literalString;
  };

  int startLine;
  int startColumn;
  int length;

private:
  Tree(Type type);

  friend Tree keyword(Keyword keyword);
  friend Tree identifier(string&& name);
  friend Tree identifier(const string& name);
  friend Tree sequence(std::vector<Tree>&& elements);
  friend Tree sequence(const std::vector<Tree>& elements);

  friend Tree block(std::vector<Tree>&& statements);
  friend Tree block(const std::vector<Tree>& statements);
  friend Tree bracketed(Tree&& content);
  friend Tree bracketed(const Tree& content);
  friend Tree parenthesized(Tree&& content);
  friend Tree parenthesized(const Tree& content);
  friend Tree list(std::vector<Tree>&& elements);
  friend Tree list(const std::vector<Tree>& elements);

  friend Tree literal(int value);
  friend Tree literal(double value);
  friend Tree literal(string&& value);
  friend Tree literal(const string& value);
};

template <typename T>
void appendAll(std::vector<T>& vec) {}

template <typename T, typename First, typename... Rest>
void appendAll(std::vector<T>& vec, First&& first, Rest&&... rest) {
  vec.push_back(std::forward<First>(first));
  appendAll(vec, std::forward<Rest>(rest)...);
}

template <typename... Params>
Tree sequence(Params&&... params) {
  std::vector<Tree> vec;
  appendAll(vec, params...);
  return sequence(std::move(vec));
}

template <typename... Params>
Tree block(Params&&... params) {
  std::vector<Tree> vec;
  appendAll(vec, params...);
  return block(std::move(vec));
}

template <typename... Params>
Tree list(Params&&... params) {
  std::vector<Tree> vec;
  appendAll(vec, params...);
  return list(std::move(vec));
}

}  // namespace tokens
}  // namespace modc

#endif /* KENTONSCODE_MODC_TOKENS_H_ */
