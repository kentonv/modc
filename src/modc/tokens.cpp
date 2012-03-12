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

#include "tokens.h"

#include <new>

namespace modc {
namespace tokens {

using std::move;

enum class CharType: unsigned char {
  CONTROL,
  SPACE,
  OPERATOR,
  QUOTE,
  DELIMITER,
  GROUPER,
  ALPHANUMERIC,

  UTF8_START,
  UTF8_INNER
};

class CharTypeMap {
public:
  CharTypeMap() {
    setRange(0, 0x7F, CharType::CONTROL);
    setRange(0x80, 0xBF, CharType::UTF8_START);
    setRange(0xC0, 0xFF, CharType::UTF8_INNER);

    setAll("\n\r\t ", CharType::SPACE);
    setAll("!#$%&*+-./:<=>?@\\^|~", CharType::OPERATOR);
    setAll("\"'`", CharType::QUOTE);
    setAll(",;", CharType::DELIMITER);
    setAll("()[]{}", CharType::GROUPER);

    setRange('a', 'z', CharType::ALPHANUMERIC);
    setRange('A', 'Z', CharType::ALPHANUMERIC);
    setRange('0', '9', CharType::ALPHANUMERIC);
    charTypes['_'] = CharType::ALPHANUMERIC;
  }

  inline CharType operator[](char c) const {
    return charTypes[(unsigned char) c];
  }

private:
  CharType charTypes[256];

  void setRange(unsigned char begin, unsigned char end, CharType type) {
    for (unsigned char c = begin; c <= end; ++c) {
      charTypes[c] = type;
    }
  }

  void setAll(const char* chars, CharType type) {
    while (*chars != '\0') {
      charTypes[(unsigned char) *chars] = type;
      ++chars;
    }
  }
};

static const CharTypeMap charTypes;

class Reader {
public:
  bool lookingAt(CharType charType);
  bool tryConsume(CharType charType);
};


Tree parse(const string& text);

// =======================================================================================

Tree::Tree(Type type): type(type) {}

Tree::Tree(Tree&& other): type(other.type) {
  switch (type) {
    case Type::KEYWORD:
      new (&keyword) Keyword(move(other.keyword));
      break;
    case Type::IDENTIFIER:
      new (&identifier) string(move(other.identifier));
      break;

    case Type::SEQUENCE:
      new (&sequence) std::vector<Tree>(move(other.sequence));
      break;

    case Type::BLOCK:
      new (&block) std::vector<Tree>(move(other.block));
      break;
    case Type::BRACKETED:
      new (&bracketed) OwnedPtr<Tree>(move(other.bracketed));
      break;
    case Type::PARENTHESIZED:
      new (&parenthesized) OwnedPtr<Tree>(move(other.parenthesized));
      break;
    case Type::LIST:
      new (&list) std::vector<Tree>(move(other.list));
      break;

    case Type::LITERAL_INT:
      new (&literalInt) int(move(other.literalInt));
      break;
    case Type::LITERAL_DOUBLE:
      new (&literalDouble) double(move(other.literalDouble));
      break;
    case Type::LITERAL_STRING:
      new (&literalString) string(move(other.literalString));
      break;
  }
}

Tree::Tree(const Tree& other): type(other.type) {
  switch (type) {
    case Type::KEYWORD:
      new (&keyword) Keyword(other.keyword);
      break;
    case Type::IDENTIFIER:
      new (&identifier) string(other.identifier);
      break;

    case Type::SEQUENCE:
      new (&sequence) std::vector<Tree>(other.sequence);
      break;

    case Type::BLOCK:
      new (&block) std::vector<Tree>(other.block);
      break;
    case Type::BRACKETED:
      new (&bracketed) OwnedPtr<Tree>(newOwned<Tree>(*other.bracketed));
      break;
    case Type::PARENTHESIZED:
      new (&parenthesized) OwnedPtr<Tree>(newOwned<Tree>(*other.parenthesized));
      break;
    case Type::LIST:
      new (&list) std::vector<Tree>(other.list);
      break;

    case Type::LITERAL_INT:
      new (&literalInt) int(other.literalInt);
      break;
    case Type::LITERAL_DOUBLE:
      new (&literalDouble) double(other.literalDouble);
      break;
    case Type::LITERAL_STRING:
      new (&literalString) string(other.literalString);
      break;
  }
}

Tree::~Tree() {
  switch (type) {
    case Type::KEYWORD:
      break;
    case Type::IDENTIFIER:
      identifier.~string();
      break;

    case Type::SEQUENCE:
      sequence.~vector<Tree>();
      break;

    case Type::BLOCK:
      block.~vector<Tree>();
      break;
    case Type::BRACKETED:
      bracketed.~OwnedPtr<Tree>();
      break;
    case Type::PARENTHESIZED:
      parenthesized.~OwnedPtr<Tree>();
      break;
    case Type::LIST:
      list.~vector<Tree>();
      break;

    case Type::LITERAL_INT:
      break;
    case Type::LITERAL_DOUBLE:
      break;
    case Type::LITERAL_STRING:
      literalString.~basic_string();
      break;
  }
}

// =======================================================================================

Tree keyword(Keyword keyword) {
  Tree result(Tree::Type::KEYWORD);
  new (&result.keyword) Keyword(keyword);
  return result;
}

Tree identifier(string&& name) {
  Tree result(Tree::Type::IDENTIFIER);
  new (&result.identifier) string(move(name));
  return result;
}

Tree identifier(const string& name) {
  Tree result(Tree::Type::IDENTIFIER);
  new (&result.identifier) string(name);
  return result;
}

Tree sequence(std::vector<Tree>&& elements) {
  Tree result(Tree::Type::SEQUENCE);
  new (&result.sequence) std::vector<Tree>(move(elements));
  return result;
}
Tree sequence(const std::vector<Tree>& elements) {
  Tree result(Tree::Type::SEQUENCE);
  new (&result.sequence) std::vector<Tree>(elements);
  return result;
}

Tree block(std::vector<Tree>&& statements) {
  Tree result(Tree::Type::BLOCK);
  new (&result.block) std::vector<Tree>(move(statements));
  return result;
}
Tree block(const std::vector<Tree>& statements) {
  Tree result(Tree::Type::BLOCK);
  new (&result.block) std::vector<Tree>(statements);
  return result;
}
Tree bracketed(Tree&& content) {
  Tree result(Tree::Type::BRACKETED);
  new (&result.bracketed) Tree(move(content));
  return result;
}
Tree bracketed(const Tree& content) {
  Tree result(Tree::Type::BRACKETED);
  new (&result.bracketed) Tree(move(content));
  return result;
}
Tree parenthesized(Tree&& content) {
  Tree result(Tree::Type::PARENTHESIZED);
  new (&result.parenthesized) Tree(move(content));
  return result;
}
Tree parenthesized(const Tree& content) {
  Tree result(Tree::Type::PARENTHESIZED);
  new (&result.parenthesized) Tree(content);
  return result;
}
Tree list(std::vector<Tree>&& elements) {
  Tree result(Tree::Type::LIST);
  new (&result.list) std::vector<Tree>(move(elements));
  return result;
}
Tree list(const std::vector<Tree>& elements) {
  Tree result(Tree::Type::LIST);
  new (&result.list) std::vector<Tree>(elements);
  return result;
}

Tree literal(int value) {
  Tree result(Tree::Type::LITERAL_INT);
  new (&result.literalInt) int(value);
  return result;
}
Tree literal(double value) {
  Tree result(Tree::Type::LITERAL_DOUBLE);
  new (&result.literalDouble) double(value);
  return result;
}
Tree literal(string&& value) {
  Tree result(Tree::Type::LITERAL_STRING);
  new (&result.literalString) string(move(value));
  return result;
}
Tree literal(const string& value) {
  Tree result(Tree::Type::LITERAL_STRING);
  new (&result.literalString) string(value);
  return result;
}

}  // namespace tokens
}  // namespace modc
