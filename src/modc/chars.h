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

#ifndef KENTONSCODE_MODC_CHARS_H_
#define KENTONSCODE_MODC_CHARS_H_

#include <cstdint>

namespace modc {
namespace chars {

#ifdef __CDT_PARSER__
#define constexpr
#endif

static constexpr std::uint64_t oneBits(int count) {
  return count <= 0 ? 0ll : count >= 64 ? -1ll : ((1ll << count) - 1);
}

static constexpr std::uint64_t bit(int index) {
  return index <= 0 ? 0 : index >= 64 ? 0 : (1ll << index);
}

class CharClass {
public:
  constexpr CharClass(): bits{0, 0, 0, 0} {}
  constexpr CharClass(unsigned char c)
      : bits{bit(c), bit(c - 64), bit(c - 128), bit(c - 192)} {}
  constexpr CharClass(unsigned char start, unsigned char end): bits{
      oneBits(end +   1) & ~oneBits(start      ),
      oneBits(end -  63) & ~oneBits(start -  64),
      oneBits(end - 127) & ~oneBits(start - 128),
      oneBits(end - 191) & ~oneBits(start - 192)} {}

  constexpr CharClass operator+(const CharClass& other) const {
    return CharClass(bits[0] | other.bits[0],
                     bits[1] | other.bits[1],
                     bits[2] | other.bits[2],
                     bits[3] | other.bits[3]);
  }

  constexpr CharClass operator-(const CharClass& other) const {
    return CharClass(bits[0] & ~other.bits[0],
                     bits[1] & ~other.bits[1],
                     bits[2] & ~other.bits[2],
                     bits[3] & ~other.bits[3]);
  }

  constexpr bool contains(unsigned char c) const {
    return (bits[c / 64] & (1ll << (c % 64))) != 0;
  }

private:
  constexpr CharClass(uint64_t a, uint64_t b, uint64_t c, uint64_t d): bits{a, b, c, d} {}
  std::uint64_t bits[4];
};

enum class CharType: unsigned char {
  UNKNOWN,
  UNPRINTABLE,
  WHITESPACE,
  OPERATOR,
  LETTER,
  DIGIT
};

template <int... I>
struct TemplateInts {};

template <int N, int... I>
struct TemplateIntRange : public TemplateIntRange<N - 1, N - 1, I...> {};

template <int... I>
struct TemplateIntRange<0, I...> {
  static constexpr TemplateInts<I...> value() { return TemplateInts<I...>(); }
};

struct CharTypeMapInit;

class CharTypeMap {
public:
  constexpr CharTypeMap(): charTypes{CharType::UNKNOWN} {}

  constexpr CharType operator[](char c) const {
    return charTypes[(unsigned char) c];
  }

private:
  CharType charTypes[256];

  template <int... I>
  constexpr CharTypeMap(const CharTypeMap& orig, const CharClass& charClass,
                        CharType type, TemplateInts<I...> hack)
      : charTypes{charClass.contains(I) ? type : orig.charTypes[I]...} {}

  friend constexpr CharTypeMap operator+(const CharTypeMap& orig, const CharTypeMapInit& init);
};

class CharTypeMapInit {
public:
  constexpr CharTypeMapInit(const CharClass& charClass, CharType type)
      : charClass(charClass), type(type) {}

  CharClass charClass;
  CharType type;
};

constexpr CharTypeMap operator+(const CharTypeMap& orig, const CharTypeMapInit& init) {
  return CharTypeMap(orig, init.charClass, init.type, TemplateIntRange<256>::value());
}


constexpr CharClass range(unsigned char start, unsigned char end) {
  return CharClass(start, end);
}

constexpr CharClass allOf(const char* str) {
  return *str ? (CharClass(*str) + allOf(str + 1)) : CharClass();
}

constexpr CharClass ANY = range(0, 0xFF);

constexpr CharClass WHITESPACE = allOf("\n\r\t ");

constexpr CharClass QUOTE = allOf("\"'`");
constexpr CharClass DELIMITER = allOf(",;");
constexpr CharClass OPENER = allOf("{[(");
constexpr CharClass CLOSER = allOf("}])");

constexpr CharClass SKIPPABLE = ANY - allOf("{};");

constexpr CharClass LETTER = range('a', 'z') + range('A', 'Z');
constexpr CharClass DIGIT = range('0', '9');
constexpr CharClass OCTAL_DIGIT = range('0', '7');
constexpr CharClass HEX_DIGIT = DIGIT + range('a', 'f') + range('A', 'F');
constexpr CharClass ALPHANUMERIC = LETTER + DIGIT + '_';

constexpr CharClass OPERATOR = allOf("!#$%&*+-./:<=>?@\\^|~");

constexpr CharClass PRINTABLE = range(0x20, 0xFF) - '\x7f';
constexpr CharClass UNPRINTABLE = ANY - PRINTABLE;

constexpr CharClass ASCII = range(0, 0x7F);
constexpr CharClass UTF8_START = range(0x80, 0xBF);
constexpr CharClass UTF8_INNER = range(0xC0, 0xFF);
constexpr CharClass UTF8 = UTF8_START + UTF8_INNER;

constexpr CharTypeMap TYPES = CharTypeMap() +
    CharTypeMapInit(UNPRINTABLE, CharType::UNPRINTABLE) +
    CharTypeMapInit(WHITESPACE, CharType::WHITESPACE) +
    CharTypeMapInit(OPERATOR, CharType::OPERATOR) +
    CharTypeMapInit(LETTER, CharType::LETTER) +
    CharTypeMapInit(DIGIT, CharType::DIGIT);

constexpr CharClass ESCAPE = '\\';
constexpr CharClass SINGLE_QUOTABLE = PRINTABLE - ESCAPE - allOf("'");
constexpr CharClass DOUBLE_QUOTABLE = PRINTABLE - ESCAPE - allOf("\"");
constexpr CharClass BACK_QUOTABLE = PRINTABLE - allOf("`");

constexpr CharClass ESCAPABLE = allOf("abfnrtv'\"\\\?");

}  // namespace chars
}  // namespace modc

#endif /* KENTONSCODE_MODC_CHARS_H_ */
