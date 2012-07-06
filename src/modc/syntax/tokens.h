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
#include <set>
#include <stdint.h>

#include "base/OwnedPtr.h"
#include "../Maybe.h"
#include "../errors.h"

namespace modc {
  namespace chars {
    class CharClass;
  }
}

namespace modc {
namespace tokens {

using ekam::OwnedPtr;
using ekam::newOwned;
using std::string;
using errors::Location;
using errors::Located;

class Token;
class TokenSequence;

class Token {
public:
  enum class Type {
    ERROR,

    KEYWORD,
    IDENTIFIER,

    BRACKETED,
    PARENTHESIZED,

    LITERAL_INT,
    LITERAL_DOUBLE,
    LITERAL_STRING

    // TODO:  Empty lines, comments -- should these be allowed only between statements?
  };

  Token(Token&& other);
  Token(const Token& other);
  ~Token() noexcept;

  Token& operator=(Token&& other);
  Token& operator=(const Token& other);

  bool operator==(const Token& other) const;
  bool operator!=(const Token& other) const { return !(*this == other); }

  Type getType() const { return type; }

  union {
    // Placeholder indicating that token parsing failed.
    std::vector<errors::Error> error;

    // A keyword or symbol.
    string keyword;

    // An identifier, e.g. a variable name.
    string identifier;

    // Comma-delimited token sequences in square brackets.
    std::vector<TokenSequence> bracketed;

    // Comma-delimited token sequences in parentheses.
    std::vector<TokenSequence> parenthesized;

    // A literal value.
    int literalInt;
    double literalDouble;
    string literalString;
  };

  Location location;

  void getErrors(std::vector<errors::Error>& errors) const;
  std::vector<errors::Error> getErrors() const;

private:
  Type type;

  Token(Location location, Type type);

  friend Token errorToken(Location location, errors::Error&& error);
  friend Token errorToken(Location location, const errors::Error& error);
  friend Token errorToken(Location location, std::vector<errors::Error>&& errors);
  friend Token errorToken(Location location, const std::vector<errors::Error>& errors);

  friend Token keyword(Location location, string&& keyword);
  friend Token keyword(Location location, const string& keyword);
  friend Token identifier(Location location, string&& name);
  friend Token identifier(Location location, const string& name);

  friend Token bracketed(Location location, std::vector<TokenSequence>&& content);
  friend Token bracketed(Location location, const std::vector<TokenSequence>& content);
  friend Token parenthesized(Location location, std::vector<TokenSequence>&& content);
  friend Token parenthesized(Location location, const std::vector<TokenSequence>& content);

  friend Token literal(Location location, int value);
  friend Token literal(Location location, double value);
  friend Token literal(Location location, string&& value);
  friend Token literal(Location location, const string& value);
};

Token errorToken(Location location, errors::Error&& error);
Token errorToken(Location location, const errors::Error& error);
Token errorToken(Location location, std::vector<errors::Error>&& errors);
Token errorToken(Location location, const std::vector<errors::Error>& errors);

Token keyword(Location location, string&& keyword);
Token keyword(Location location, const string& keyword);
Token identifier(Location location, string&& name);
Token identifier(Location location, const string& name);

Token bracketed(Location location, std::vector<TokenSequence>&& content);
Token bracketed(Location location, const std::vector<TokenSequence>& content);
Token parenthesized(Location location, std::vector<TokenSequence>&& content);
Token parenthesized(Location location, const std::vector<TokenSequence>& content);

Token literal(Location location, int value);
Token literal(Location location, double value);
Token literal(Location location, string&& value);
Token literal(Location location, const string& value);

std::ostream& operator<<(std::ostream& os, const Token& token);

class TokenSequence {
public:
  std::vector<Token> tokens;

  Location location;

  bool operator==(const TokenSequence& other) const { return tokens == other.tokens; }
  bool operator!=(const TokenSequence& other) const { return tokens != other.tokens; }

  void getErrors(std::vector<errors::Error>& errors) const;
  std::vector<errors::Error> getErrors() const;
};

std::ostream& operator<<(std::ostream& os, const TokenSequence& sequence);

class TokenStatement {
public:
  TokenSequence tokens;
  Maybe<std::vector<TokenStatement>> block;

  // Comment appearing after the end of the statement.
  Maybe<Located<string>> comment;

  bool operator==(const TokenStatement& other) const;
  bool operator!=(const TokenStatement& other) const { return !(*this == other); }

  void getErrors(std::vector<errors::Error>& errors) const;
  std::vector<errors::Error> getErrors() const;
};

std::ostream& operator<<(std::ostream& os, const TokenStatement& statement);

class Parser {
public:
  Parser(std::set<string> keywords);

  std::vector<TokenStatement> parse(const string& text);

private:
  std::set<string> keywords;

  class Reader;

  int parseInt(const string& str, int base);
  double parseDouble(const string& str);
  string toUtf8(char16_t c);
  string toUtf8(char32_t c);
  char interpretEscape(char c);

  Token parseQuote(Reader& reader, const chars::CharClass& quotable, const chars::CharClass& quote);
  Token parseNumber(Reader& reader);
  TokenSequence parseSequence(Reader& reader);
  TokenSequence parseSequenceInternal(Reader& reader);
  void skipStatement(Reader& reader);
  TokenStatement parseStatement(Reader& reader);
};

}  // namespace tokens
}  // namespace modc

#endif /* KENTONSCODE_MODC_TOKENS_H_ */
