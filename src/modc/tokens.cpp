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

#include <cstdlib>
#include <new>
#include <sstream>

#include "base/Debug.h"
#include "chars.h"

namespace modc {
namespace tokens {

using std::move;

using chars::CharClass;
using chars::CharType;
Token::Token(Type type): type(type) {}

Token::Token(Token&& other): type(other.type) {
  switch (type) {
    case Type::ERROR:
      break;

    case Type::KEYWORD:
      new (&keyword) string(move(other.keyword));
      break;
    case Type::IDENTIFIER:
      new (&identifier) string(move(other.identifier));
      break;

    case Type::BRACKETED:
      new (&bracketed) std::vector<TokenSequence>(move(other.bracketed));
      break;
    case Type::PARENTHESIZED:
      new (&parenthesized) std::vector<TokenSequence>(move(other.parenthesized));
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

Token::Token(const Token& other): type(other.type) {
  switch (type) {
    case Type::ERROR:
      break;

    case Type::KEYWORD:
      new (&keyword) string(other.keyword);
      break;
    case Type::IDENTIFIER:
      new (&identifier) string(other.identifier);
      break;

    case Type::BRACKETED:
      new (&bracketed) std::vector<TokenSequence>(other.bracketed);
      break;
    case Type::PARENTHESIZED:
      new (&parenthesized) std::vector<TokenSequence>(other.parenthesized);
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

Token::~Token() {
  switch (type) {
    case Type::ERROR:
      break;

    case Type::KEYWORD:
      break;
    case Type::IDENTIFIER:
      identifier.~string();
      break;

    case Type::BRACKETED:
      bracketed.~vector<TokenSequence>();
      break;
    case Type::PARENTHESIZED:
      parenthesized.~vector<TokenSequence>();
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

Token& Token::operator=(Token&& other) {
  // Too lazy.
  this->~Token();
  new (this) Token(move(other));
  return *this;
}

Token& Token::operator=(const Token& other) {
  // Too lazy.
  this->~Token();
  new (this) Token(other);
  return *this;
}

bool Token::operator==(const Token& other) const {
  if (type == other.type) {
    switch (type) {
      case Type::ERROR:
        return true;

      case Type::KEYWORD:
        return keyword == other.keyword;
      case Type::IDENTIFIER:
        return identifier == other.identifier;

      case Type::BRACKETED:
        return bracketed == other.bracketed;
      case Type::PARENTHESIZED:
        return parenthesized == other.parenthesized;

      case Type::LITERAL_INT:
        return literalInt == other.literalInt;
      case Type::LITERAL_DOUBLE:
        return literalDouble == other.literalDouble;
      case Type::LITERAL_STRING:
        return literalString == other.literalString;
    }
  }

  return false;
}

// =======================================================================================

Token keyword(string&& keyword) {
  Token result(Token::Type::KEYWORD);
  new (&result.keyword) string(move(keyword));
  return result;
}

Token keyword(const string& keyword) {
  Token result(Token::Type::KEYWORD);
  new (&result.keyword) string(keyword);
  return result;
}

Token identifier(string&& name) {
  Token result(Token::Type::IDENTIFIER);
  new (&result.identifier) string(move(name));
  return result;
}

Token identifier(const string& name) {
  Token result(Token::Type::IDENTIFIER);
  new (&result.identifier) string(name);
  return result;
}

Token bracketed(std::vector<TokenSequence>&& content) {
  Token result(Token::Type::BRACKETED);
  new (&result.bracketed) std::vector<TokenSequence>(move(content));
  return result;
}
Token bracketed(const std::vector<TokenSequence>& content) {
  Token result(Token::Type::BRACKETED);
  new (&result.bracketed) std::vector<TokenSequence>(content);
  return result;
}
Token parenthesized(std::vector<TokenSequence>&& content) {
  Token result(Token::Type::PARENTHESIZED);
  new (&result.parenthesized) std::vector<TokenSequence>(move(content));
  return result;
}
Token parenthesized(const std::vector<TokenSequence>& content) {
  Token result(Token::Type::PARENTHESIZED);
  new (&result.parenthesized) std::vector<TokenSequence>(content);
  return result;
}

Token literal(int value) {
  Token result(Token::Type::LITERAL_INT);
  new (&result.literalInt) int(value);
  return result;
}
Token literal(double value) {
  Token result(Token::Type::LITERAL_DOUBLE);
  new (&result.literalDouble) double(value);
  return result;
}
Token literal(string&& value) {
  Token result(Token::Type::LITERAL_STRING);
  new (&result.literalString) string(move(value));
  return result;
}
Token literal(const string& value) {
  Token result(Token::Type::LITERAL_STRING);
  new (&result.literalString) string(value);
  return result;
}

std::ostream& operator<<(std::ostream& os, const Token& token) {
  switch (token.getType()) {
    case Token::Type::ERROR:
      os << "errorToken()";
      break;

    case Token::Type::KEYWORD:
      os << "keyword(" << token.keyword << ")";
      break;
    case Token::Type::IDENTIFIER:
      os << "identifier(" << token.identifier << ")";
      break;

    case Token::Type::BRACKETED: {
      bool first = true;
      os << "bracketed(";
      for (const TokenSequence& term : token.bracketed) {
        if (first) {
          first = false;
        } else {
          os << ", ";
        }
        os << term;
      }
      os << ")";
      break;
    }

    case Token::Type::PARENTHESIZED: {
      bool first = true;
      os << "parenthesized(";
      for (const TokenSequence& term : token.parenthesized) {
        if (first) {
          first = false;
        } else {
          os << ", ";
        }
        os << term;
      }
      os << ")";
      break;
    }

    case Token::Type::LITERAL_INT:
      os << "literalInt(" << token.literalInt << ")";
      break;

    case Token::Type::LITERAL_DOUBLE:
      os << "literalDouble(" << token.literalDouble << ")";
      break;

    case Token::Type::LITERAL_STRING: {
      os << "literalString(\"";
      for (unsigned char c : token.literalString) {
        switch (c) {
          case '\a' : os << "\\a"; break;
          case '\b' : os << "\\b"; break;
          case '\f' : os << "\\f"; break;
          case '\n' : os << "\\n"; break;
          case '\r' : os << "\\r"; break;
          case '\t' : os << "\\t"; break;
          case '\v' : os << "\\v"; break;
          case '\"' : os << "\\\""; break;
          case '\\' : os << "\\\\"; break;
          default:
            // TODO: Interpret UTF-8?  Eww.
            if (c < 0x20 || c >= 0x7F) {
              static const char HEX_DIGITS[] = "0123456789abcdef";
              os << "\\x" << HEX_DIGITS[c / 16] << HEX_DIGITS[c % 16];
            } else {
              os << c;
            }
            break;
        }
      }
      os << "\")";
      break;
    }
  }
  return os;
}

bool Statement::operator==(const Statement& other) const {
  return tokens == other.tokens && block == other.block;
}

std::ostream& operator<<(std::ostream& os, const TokenSequence& sequence) {
  bool first = true;
  for (const Token& token : sequence.tokens) {
    if (first) {
      first = false;
    } else {
      os << ' ';
    }
    os << token;
  }
  return os;
}

static void write(std::ostream& os, const Statement& statement, int indent) {
  if (statement.tokens.tokens.empty()) {
    return;
  }

  for (int i = 0; i < indent; i++) {
    os << "  ";
  }
  os << statement.tokens;
  if (statement.block) {
    os << " {\n";
    for (const Statement& subStatement : *statement.block) {
      write(os, subStatement, indent + 1);
    }
    os << "}\n";
  } else {
    os << ";\n";
  }
}

std::ostream& operator<<(std::ostream& os, const Statement& statement) {
  write(os, statement, 0);
  return os;
}

// =======================================================================================

Parser::Parser(std::set<string> keywords): keywords(move(keywords)) {}

class Parser::Reader {
public:
  Reader(const string& text, std::vector<string>& errors)
      : parent(NULL), errors(errors), start(&text.front()), end(start), limit(&text.back() + 1) {}
  Reader(Reader& parent)
      : parent(&parent), errors(parent.errors), start(parent.end), end(start), limit(parent.limit) {
    parent.end = NULL;
  }

  ~Reader() {
    if (parent != NULL) {
      parent->end = end;
    }
  }

  bool lookingAt(const CharClass& charClass) {
    return end < limit && charClass.contains(*end);
  }
  bool tryConsume(const CharClass& charClass) {
    if (lookingAt(charClass)) {
      ++end;
      return true;
    } else {
      return false;
    }
  }
  void consumeAll(const CharClass& charClass) {
    while (tryConsume(charClass));
  }

  CharType nextCharType() {
    return end < limit ? chars::TYPES[*end] : CharType::UNKNOWN;
  }
  char nextChar() {
    return end < limit ? *end : '\0';
  }

  string content() {
    return string(start, end);
  }

  template <typename... Parts>
  void error(Parts&&... message) {
    std::ostringstream os;
    // TODO:  line/column number
    os << "\"" << string(start, end) << "\": ";
    writeError(os, std::forward<Parts>(message)...);
    errors.push_back(os.str());
  }

private:
  Reader* const parent;
  std::vector<string>& errors;
  const char* const start;
  const char* end;
  const char* const limit;

  void writeError(std::ostream& os) {}

  template <typename Part1, typename... Parts>
  void writeError(std::ostream& os, Part1&& part1, Parts&&... message) {
    os << std::forward<Part1>(part1);
    writeError(os, std::forward<Parts>(message)...);
  }
};

int Parser::parseInt(const string& str, int base) {
  char* end = NULL;
  int result = std::strtol(str.c_str(), &end, base);
  if (*end != '\0') {
    DEBUG_ERROR << "parseInt() received invalid string: " << str;
  }
  return result;
}

double Parser::parseDouble(const string& str) {
  char* end = NULL;
  double result = std::strtod(str.c_str(), &end);
  if (*end != '\0') {
    DEBUG_ERROR << "parseDouble() received invalid string: " << str;
  }
  return result;
}

string Parser::toUtf8(char16_t c) {
  string result;

  if (c < 0x80) {
    result += (char)c;
  } else if (c < 0x800) {
    result += (char)((c >> 6) | 0xC0);
    result += (char)((c & 0x3F) | 0x80);
  } else {
    result += (char)((c >> 12) | 0xE0);
    result += (char)(((c >> 6) & 0x3F) | 0x80);
    result += (char)((c & 0x3F) | 0x80);
  }

  return result;
}

string Parser::toUtf8(char32_t c) {
  string result;

  if (c < 0x80) {
    result += (char)c;
  } else if (c < 0x800) {
    result += (char)((c >> 6) | 0xC0);
    result += (char)((c & 0x3F) | 0x80);
  } else if (c < 0x10000){
    result += (char)((c >> 12) | 0xE0);
    result += (char)(((c >> 6) & 0x3F) | 0x80);
    result += (char)((c & 0x3F) | 0x80);
  } else if (c < 0x200000){
    result += (char)((c >> 18) | 0xF0);
    result += (char)(((c >> 12) & 0x3F) | 0x80);
    result += (char)(((c >> 6) & 0x3F) | 0x80);
    result += (char)((c & 0x3F) | 0x80);
  } else if (c < 0x4000000){
    result += (char)((c >> 24) | 0xF8);
    result += (char)(((c >> 18) & 0x3F) | 0x80);
    result += (char)(((c >> 12) & 0x3F) | 0x80);
    result += (char)(((c >> 6) & 0x3F) | 0x80);
    result += (char)((c & 0x3F) | 0x80);
  } else if (c < 0x80000000){
    result += (char)((c >> 30) | 0xFC);
    result += (char)(((c >> 24) & 0x3F) | 0x80);
    result += (char)(((c >> 18) & 0x3F) | 0x80);
    result += (char)(((c >> 12) & 0x3F) | 0x80);
    result += (char)(((c >> 6) & 0x3F) | 0x80);
    result += (char)((c & 0x3F) | 0x80);
  } else {
    DEBUG_ERROR << "Parameter to toUtf8() is too large: " << (uint32_t)c;
  }

  return result;
}

char Parser::interpretEscape(char c) {
  switch (c) {
    case 'a' : return '\a';
    case 'b' : return '\b';
    case 'f' : return '\f';
    case 'n' : return '\n';
    case 'r' : return '\r';
    case 't' : return '\t';
    case 'v' : return '\v';
    default:
      return c;
      break;
  }
}

Token Parser::parseQuote(Reader& reader, const CharClass& quotable, const CharClass& quote) {
  Reader span(reader);
  string text;

  span.tryConsume(quote);

  while (true) {
    // Consume a span of quotable characters.
    {
      Reader quotableSpan(span);
      quotableSpan.consumeAll(quotable);

      if (quotableSpan.tryConsume('\n')) {
        // String with no end quote ends at new line.
        text += quotableSpan.content();
        break;
      }

      text += quotableSpan.content();
    }

    // What kind of unquotable character did we encounter?
    Reader unquotableSpan(span);
    if (unquotableSpan.tryConsume(chars::ESCAPE)) {
      // An escape sequence.
      Reader escapeSpan(unquotableSpan);
      if (escapeSpan.tryConsume(chars::ESCAPABLE)) {
        text += interpretEscape(escapeSpan.content().front());
      } else if (escapeSpan.tryConsume(chars::OCTAL_DIGIT)) {
        // Try to consume two more.
        escapeSpan.tryConsume(chars::OCTAL_DIGIT) &&
            escapeSpan.tryConsume(chars::OCTAL_DIGIT);

        text += (char) parseInt(escapeSpan.content(), 8);
      } else if (escapeSpan.tryConsume('x')) {
        Reader charCodeSpan(escapeSpan);
        if (charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT)) {
          text += (char) parseInt(charCodeSpan.content(), 16);
        } else {
          charCodeSpan.error("Expected two hex digits for \\x.");
        }
      } else if (escapeSpan.tryConsume('u')) {
        Reader charCodeSpan(escapeSpan);
        if (charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT)) {
          text += toUtf8((char16_t) parseInt(charCodeSpan.content(), 16));
        } else {
          charCodeSpan.error("Expected four hex digits for \\u.");
        }
      } else if (escapeSpan.tryConsume('U')) {
        Reader charCodeSpan(escapeSpan);
        if (charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT) &&
            charCodeSpan.tryConsume(chars::HEX_DIGIT)) {
          char32_t rune = (char32_t) parseInt(charCodeSpan.content(), 16);
          if (rune >= 0x80000000) {
            charCodeSpan.error("Code point too large; maximum size is 31 bits.");
          } else {
            text += toUtf8(rune);
          }
        } else {
          escapeSpan.error("Expected eight hex digits for \\U.");
        }
      } else {
        escapeSpan.error("Invalid escape sequence.");
      }
    } else if (unquotableSpan.tryConsume(quote)) {
      break;
    } else if (!unquotableSpan.lookingAt(chars::ANY)) {
      unquotableSpan.error("Unexpected EOF in string literal.");
      break;
    } else if (unquotableSpan.lookingAt(chars::UNPRINTABLE)) {
      unquotableSpan.consumeAll(chars::UNPRINTABLE);
      unquotableSpan.error("Unprintable characters in string literal must be escaped.");
      text += unquotableSpan.content();
    } else {
      // Shouldn't get here.
      unquotableSpan.tryConsume(chars::ANY);
      unquotableSpan.error("Internal error:  Character not handled.");
    }
  }

  return literal(text);
}

Token Parser::parseNumber(Reader& reader) {
  Reader span(reader);
  if (span.tryConsume('0')) {
    if (span.tryConsume('x')) {
      span.consumeAll(chars::HEX_DIGIT);
      return literal(parseInt(span.content(), 0));
    } else {
      span.consumeAll(chars::OCTAL_DIGIT);
      return literal(parseInt(span.content(), 0));
    }
  } else {
    bool isFloat = false;
    span.consumeAll(chars::DIGIT);
    if (span.tryConsume('.')) {
      isFloat = true;
      span.consumeAll(chars::DIGIT);
    }
    if (span.tryConsume('e')) {
      isFloat = true;
      span.tryConsume('-');
      span.consumeAll(chars::DIGIT);
    }

    if (isFloat) {
      return literal(parseDouble(span.content().c_str()));
    } else {
      return literal(parseInt(span.content().c_str(), 10));
    }
  }
}

TokenSequence Parser::parseSequence(Reader& reader) {
  TokenSequence result;

  while (true) {
    switch (reader.nextCharType()) {
      case CharType::UNPRINTABLE: {
        Reader span(reader);
        span.consumeAll(chars::UNPRINTABLE);
        span.error("Encountered unprintable characters.");
        break;
      }

      case CharType::WHITESPACE: {
        Reader span(reader);
        span.consumeAll(chars::WHITESPACE);
        break;
      }

      case CharType::OPERATOR: {
        Reader span(reader);
        span.consumeAll(chars::OPERATOR);
        string op = span.content();

        while (!op.empty()) {
          for (string::size_type i = op.size(); ; i--) {
            if (i == 0) {
              span.error("Invalid operator: ", op);
              op.clear();
              break;
            }

            string sub = op.substr(0, i);
            if (keywords.count(sub)) {
              op.erase(0, i);
              result.tokens.push_back(keyword(sub));
              break;
            }
          }
        }
        break;
      }

      case CharType::LETTER: {
        Reader span(reader);
        span.consumeAll(chars::ALPHANUMERIC);
        string word = span.content();
        if (keywords.count(word) > 0) {
          result.tokens.push_back(keyword(move(word)));
        } else {
          result.tokens.push_back(identifier(move(word)));
        }
        break;
      }

      case CharType::DIGIT: {
        result.tokens.push_back(parseNumber(reader));
        if (reader.lookingAt(chars::ALPHANUMERIC)) {
          Reader span(reader);
          span.consumeAll(chars::ALPHANUMERIC);
          span.error("Need space before additional letters/digits after end of number.");
        }
        break;
      }

      default:
        switch (reader.nextChar()) {
          case '\'':
            result.tokens.push_back(parseQuote(reader, chars::SINGLE_QUOTABLE, '\''));
            break;
          case '\"':
            result.tokens.push_back(parseQuote(reader, chars::DOUBLE_QUOTABLE, '\"'));
            break;
          case '`':
            result.tokens.push_back(parseQuote(reader, chars::BACK_QUOTABLE, '`'));
            break;

          case ',':
          case ';':
          case ')':
          case ']':
          case '{':
          case '}':
            return result;

          case '(': {
            Reader span(reader);
            span.tryConsume('(');

            std::vector<TokenSequence> terms;
            do {
              terms.push_back(parseSequence(span));
            } while (span.tryConsume(','));

            if (!span.tryConsume(')')) {
              span.error("Expected ')'.");
            }

            result.tokens.push_back(parenthesized(move(terms)));
            break;
          }

          case '[': {
            Reader span(reader);
            span.tryConsume('[');

            std::vector<TokenSequence> terms;
            do {
              terms.push_back(parseSequence(span));
            } while (span.tryConsume(','));

            if (!span.tryConsume(']')) {
              span.error("Expected ']'.");
            }

            result.tokens.push_back(bracketed(move(terms)));
            break;
          }

          case '_': {
            Reader span(reader);
            span.tryConsume('_');
            span.error("Names beginning with an underscore are reserved for the implementation.");
            break;
          }

          default: {
            Reader span(reader);
            if (span.tryConsume(chars::ANY)) {
              span.error("Unknown character.");
            } else {
              // EOF
              return result;
            }
            break;
          }
        }
        break;
    }
  }
}

void Parser::skipStatement(Reader& reader) {
  reader.consumeAll(chars::SKIPPABLE);
  if (reader.tryConsume('{')) {
    while (reader.lookingAt(chars::ANY) && !reader.tryConsume('}')) {
      skipStatement(reader);
    }
  } else {
    reader.tryConsume(';');
  }
}

Statement Parser::parseStatement(Reader& reader) {
  Reader span(reader);
  Statement result;
  result.tokens = parseSequence(span);
  if (span.lookingAt('{')) {
    Reader blockSpan(span);
    blockSpan.tryConsume('{');
    result.block.init();
    while (!blockSpan.tryConsume('}')) {
      if (blockSpan.lookingAt(chars::ANY)) {
        result.block->push_back(parseStatement(blockSpan));
      } else {
        blockSpan.error("Block missing closing brace.");
        break;
      }
    }
  } else if (!span.tryConsume(';')) {
    if (!result.tokens.tokens.empty()) {
      span.error("Expected ';'.");
      skipStatement(span);
    }
  }
  return result;
}

std::vector<Statement> Parser::parse(const string& text) {
  Reader reader(text, errors);
  std::vector<Statement> result;
  while (reader.lookingAt(chars::ANY)) {
    result.push_back(parseStatement(reader));
  }
  return result;
}

}  // namespace tokens
}  // namespace modc
