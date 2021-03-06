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

#include "../errors.h"
#include "gtest.h"

namespace modc {
namespace tokens {
namespace {

void expectNoErrors(const std::vector<TokenStatement>& statements) {
  for (auto& statement: statements) {
    for (auto& error: statement.getErrors()) {
      ADD_FAILURE() << "Unexpected parse error: " << error;
    }
  }
}

auto LOC = Location();

TEST(Tokens, ParseIdentifiers) {
  Parser parser({});
  std::vector<TokenStatement> statements = parser.parse("foo bar baz;");

  expectNoErrors(statements);

  ASSERT_EQ(1u, statements.size());

  const TokenStatement& statement = statements[0];
  const std::vector<Token>& tokens = statement.tokens.tokens;
  ASSERT_EQ(3u, tokens.size());

  EXPECT_EQ(identifier(LOC, "foo"), tokens[0]);
  EXPECT_EQ(identifier(LOC, "bar"), tokens[1]);
  EXPECT_EQ(identifier(LOC, "baz"), tokens[2]);
}

TEST(Tokens, ParseKeywords) {
  Parser parser({"foo", "bar", "+", "-", "=", "+="});
  std::vector<TokenStatement> statements = parser.parse("foo bar baz + - += -=;");

  expectNoErrors(statements);

  ASSERT_EQ(1u, statements.size());

  const TokenStatement& statement = statements[0];
  const std::vector<Token>& tokens = statement.tokens.tokens;
  ASSERT_EQ(8u, tokens.size());

  EXPECT_EQ(keyword(LOC, "foo"), tokens[0]);
  EXPECT_EQ(keyword(LOC, "bar"), tokens[1]);
  EXPECT_EQ(identifier(LOC, "baz"), tokens[2]);
  EXPECT_EQ(keyword(LOC, "+"), tokens[3]);
  EXPECT_EQ(keyword(LOC, "-"), tokens[4]);
  EXPECT_EQ(keyword(LOC, "+="), tokens[5]);
  EXPECT_EQ(keyword(LOC, "-"), tokens[6]);
  EXPECT_EQ(keyword(LOC, "="), tokens[7]);
}

TEST(Tokens, ParseIntegers) {
  Parser parser({});
  std::vector<TokenStatement> statements = parser.parse("1234567890 01234567 0x1234abCD;");

  expectNoErrors(statements);

  ASSERT_EQ(1u, statements.size());

  const TokenStatement& statement = statements[0];
  const std::vector<Token>& tokens = statement.tokens.tokens;
  ASSERT_EQ(3u, tokens.size());

  EXPECT_EQ(literal(LOC, 1234567890), tokens[0]);
  EXPECT_EQ(literal(LOC, 01234567), tokens[1]);
  EXPECT_EQ(literal(LOC, 0x1234abcd), tokens[2]);
}

TEST(Tokens, ParseFloats) {
  Parser parser({});
  std::vector<TokenStatement> statements = parser.parse("1.0 12.25 12e34 12.25e34;");

  expectNoErrors(statements);

  ASSERT_EQ(1u, statements.size());

  const TokenStatement& statement = statements[0];
  const std::vector<Token>& tokens = statement.tokens.tokens;
  ASSERT_EQ(4u, tokens.size());

  EXPECT_EQ(literal(LOC, 1.0), tokens[0]);
  EXPECT_EQ(literal(LOC, 12.25), tokens[1]);
  EXPECT_EQ(literal(LOC, 12e34), tokens[2]);
  EXPECT_EQ(literal(LOC, 12.25e34), tokens[3]);
}

const char HEX_DIGITS[] = "0123456789abcdef";

string toHex(const string& data) {
  string result;
  for (unsigned char c : data) {
    if (c == ' ') {
      result += ' ';
    } else {
      result += HEX_DIGITS[c / 16];
      result += HEX_DIGITS[c % 16];
    }
  }
  return result;
}

TEST(Tokens, ParseStrings) {
  Parser parser({});
  std::vector<TokenStatement> statements = parser.parse(
      "\"foo bar \\b\\n\\t\\\"\\\\ baz\" "
      "`foo \\bar \"' baz`\n"
      "\"foo \n"
      "'test\" test' "
      "\"\\1 \\12 \\123\" "
      "\"\\x12 \\xab\" "
      "\"\\u000a \\u00ab \\u0abc \\uabcd\" "
      "\"\\U0000000a \\U000000a1 \\U00000a12 \\U0000a123 \\U000a1234 \\U00a12345 \\U0a123456 "
        "\\U1a123456\";");

  expectNoErrors(statements);

  ASSERT_EQ(1u, statements.size());

  const TokenStatement& statement = statements[0];
  const std::vector<Token>& tokens = statement.tokens.tokens;
  ASSERT_EQ(8u, tokens.size());

  EXPECT_EQ(literal(LOC, "foo bar \b\n\t\"\\ baz"), tokens[0]);
  EXPECT_EQ(literal(LOC, "foo \\bar \"' baz"), tokens[1]);
  EXPECT_EQ(literal(LOC, "foo \n"), tokens[2]);
  EXPECT_EQ(literal(LOC, "test\" test"), tokens[3]);
  EXPECT_EQ(literal(LOC, "\1 \12 \123"), tokens[4]);
  EXPECT_EQ(literal(LOC, "\x12 \xab"), tokens[5]);
  EXPECT_EQ(literal(LOC, u8"\u000a \u00ab \u0abc \uabcd"), tokens[6]);
  EXPECT_EQ(literal(LOC,
      u8"\U0000000a \U000000a1 \U00000a12 \U0000a123 \U000a1234 \U00a12345 \U0a123456 \U1a123456"),
      tokens[7]);
}

}  // namespace
}  // namespace tokens
}  // namespace modc
