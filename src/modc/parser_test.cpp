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

#include "parser.h"

#include "gtest.h"

namespace modc {
namespace parser {
namespace {

TEST(Tuple, Flatten) {
  int output = 0;

  Tuple<Tuple<Tuple<>, Tuple<int>>, Tuple<>, int, Tuple<Tuple<Tuple<>, int>>> t =
  tuple(tuple(tuple(), tuple( 1 )), tuple(),  20, tuple(tuple(tuple(), 300)));

  t.apply([&](int i, int j, int k) { output = i + j + k; });
  EXPECT_EQ(321, output);

  EXPECT_EQ(321, t.apply<int>([](int i, int j, int k) { return i + j + k; }));
}

TEST(Parsers, ParseResult) {
  ParseResult<int> result1(123);
  EXPECT_EQ(123, result1.value);

  ParseResult<int> result2(result1);
  EXPECT_EQ(123, result2.value);

  ParseResult<int> result3(move(result1));
  EXPECT_EQ(123, result3.value);
}

typedef IteratorInput<char, string::iterator> Input;
ExactElementParser<Input> exactChar(char c) {
  return exactElement<Input>(move(c));
}

TEST(Parsers, ExactElementParser) {
  string text = "foo";
  Input input(text.begin(), text.end());

  ParseResult<Void> result = exactChar('f')(input);
  EXPECT_FALSE(result.isError());
  EXPECT_FALSE(input.isBroken());
  EXPECT_FALSE(input.atEnd());

  result = exactChar('o')(input);
  EXPECT_FALSE(result.isError());
  EXPECT_FALSE(input.isBroken());
  EXPECT_FALSE(input.atEnd());

  result = exactChar('x')(input);
  EXPECT_TRUE(result.isError());
  EXPECT_TRUE(input.isBroken());
  EXPECT_FALSE(input.atEnd());

  input.setBroken(false);

  result = exactChar('o')(input);
  EXPECT_FALSE(result.isError());
  EXPECT_FALSE(input.isBroken());
  EXPECT_TRUE(input.atEnd());
}

TEST(Parsers, SequenceParser) {
  string text = "foo";

  {
    Input input(text.begin(), text.end());
    ParseResult<Tuple<Void, Void, Void> > result =
        sequence(exactChar('f'), exactChar('o'), exactChar('o'))(input);
    EXPECT_FALSE(result.isError());
    EXPECT_FALSE(input.isBroken());
    EXPECT_TRUE(input.atEnd());
  }

  {
    Input input(text.begin(), text.end());
    ParseResult<Tuple<Void, Void> > result =
        sequence(exactChar('f'), exactChar('o'))(input);
    EXPECT_FALSE(result.isError());
    EXPECT_FALSE(input.isBroken());
    EXPECT_FALSE(input.atEnd());
  }

  {
    Input input(text.begin(), text.end());
    ParseResult<Tuple<Void, Void, Void> > result =
        sequence(exactChar('x'), exactChar('o'), exactChar('o'))(input);
    EXPECT_TRUE(result.isError());
    EXPECT_TRUE(input.isBroken());
    EXPECT_FALSE(input.atEnd());
  }
}

TEST(Parsers, TransformParser) {
  string text = "foo";

  auto parser = transform<int>(
      sequence(exactChar('f'), exactChar('o'), exactChar('o')),
      []() -> int { return 123; });

  {
    Input input(text.begin(), text.end());
    ParseResult<int> result = parser(input);
    ASSERT_FALSE(result.isError());
    EXPECT_EQ(123, result.value);
    EXPECT_FALSE(input.isBroken());
    EXPECT_TRUE(input.atEnd());
  }
}

TEST(Parsers, RepeatedParser) {
  string text = "foooob";

  auto parser = transform<int>(
      sequence(exactChar('f'), repeated(exactChar('o'))),
      [](std::vector<Void>&& values) -> int { return values.size(); });

  {
    Input input(text.begin(), text.begin() + 3);
    ParseResult<int> result = parser(input);
    ASSERT_FALSE(result.isError());
    EXPECT_EQ(2, result.value);
    EXPECT_FALSE(input.isBroken());
    EXPECT_TRUE(input.atEnd());
  }

  {
    Input input(text.begin(), text.begin() + 5);
    ParseResult<int> result = parser(input);
    ASSERT_FALSE(result.isError());
    EXPECT_EQ(4, result.value);
    EXPECT_FALSE(input.isBroken());
    EXPECT_TRUE(input.atEnd());
  }

  {
    Input input(text.begin(), text.end());
    ParseResult<int> result = parser(input);
    EXPECT_FALSE(result.isError());
    EXPECT_EQ(4, result.value);
    EXPECT_FALSE(input.isBroken());
    EXPECT_FALSE(input.atEnd());
  }
}

TEST(Parsers, AlternativeParser) {
  auto parser = alternative(
      transform<string>(sequence(exactChar('f'), commit(), exactChar('o'), exactChar('o')),
                        []() -> string { return "foo"; }),
      transform<string>(sequence(exactChar('b'), exactChar('a'), commit(), exactChar('r')),
                        []() -> string { return "bar"; }));

  {
    string text = "foo";
    Input input(text.begin(), text.end());
    ParseResult<string> result = parser(input);
    ASSERT_FALSE(result.isError());
    EXPECT_EQ("foo", result.value);
    EXPECT_FALSE(input.isBroken());
    EXPECT_TRUE(input.atEnd());
  }

  {
    string text = "bar";
    Input input(text.begin(), text.end());
    ParseResult<string> result = parser(input);
    ASSERT_FALSE(result.isError());
    EXPECT_EQ("bar", result.value);
    EXPECT_FALSE(input.isBroken());
    EXPECT_TRUE(input.atEnd());
  }
}

}  // namespace
}  // namespace parser
}  // namespace modc
