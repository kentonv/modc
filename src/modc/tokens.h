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
#include "Maybe.h"

namespace modc {
  namespace chars {
    class CharClass;
  }
  namespace errors {
    class Error;
  }
}

namespace modc {
namespace tokens {

using ekam::OwnedPtr;
using ekam::newOwned;
using std::string;

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

  // TODO:  Actually initialize these!
  int startOffset;
  int endOffset;

  void getErrors(std::vector<errors::Error>& errors) const;
  std::vector<errors::Error> getErrors() const;

private:
  Type type;

  Token(Type type);

  friend Token errorToken(errors::Error&& error);
  friend Token errorToken(const errors::Error& error);
  friend Token errorToken(std::vector<errors::Error>&& errors);
  friend Token errorToken(const std::vector<errors::Error>& errors);

  friend Token keyword(string&& keyword);
  friend Token keyword(const string& keyword);
  friend Token identifier(string&& name);
  friend Token identifier(const string& name);

  friend Token bracketed(std::vector<TokenSequence>&& content);
  friend Token bracketed(const std::vector<TokenSequence>& content);
  friend Token parenthesized(std::vector<TokenSequence>&& content);
  friend Token parenthesized(const std::vector<TokenSequence>& content);

  friend Token literal(int value);
  friend Token literal(double value);
  friend Token literal(string&& value);
  friend Token literal(const string& value);
};

Token errorToken(errors::Error&& error);
Token errorToken(const errors::Error& error);
Token errorToken(std::vector<errors::Error>&& errors);
Token errorToken(const std::vector<errors::Error>& errors);

Token keyword(string&& keyword);
Token keyword(const string& keyword);
Token identifier(string&& name);
Token identifier(const string& name);

Token bracketed(std::vector<TokenSequence>&& content);
Token bracketed(const std::vector<TokenSequence>& content);
Token parenthesized(std::vector<TokenSequence>&& content);
Token parenthesized(const std::vector<TokenSequence>& content);

Token literal(int value);
Token literal(double value);
Token literal(string&& value);
Token literal(const string& value);

std::ostream& operator<<(std::ostream& os, const Token& token);

class TokenSequence {
public:
  std::vector<Token> tokens;

  // TODO:  Fill these in.
  int startOffset;
  int endOffset;

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
  void skipStatement(Reader& reader);
  TokenStatement parseStatement(Reader& reader);
};

}  // namespace tokens
}  // namespace modc

#endif /* KENTONSCODE_MODC_TOKENS_H_ */
