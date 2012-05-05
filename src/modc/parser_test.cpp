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

  Tuple<int, int, int> t =
  tuple(tuple(tuple(), tuple(1)), tuple(), 20, tuple(tuple(tuple(), 300)));

  applyTuple([&](int i, int j, int k) { output = i + j + k; }, t);
  EXPECT_EQ(321, output);

  EXPECT_EQ(321, applyTuple([](int i, int j, int k) { return i + j + k; }, t));

  Tuple<Maybe<int>, string> t2 = tuple(Maybe<int>(123), string("foo"));
  string t3 = tuple(string("foo"));
}

typedef IteratorInput<char, string::iterator> Input;
ExactElementParser<Input> exactChar(char c) {
  return exactElement<Input>(move(c));
}

TEST(Parsers, ExactElementParser) {
  string text = "foo";
  Input input(text.begin(), text.end());

  Maybe<Void> result = exactChar('f')(input);
  EXPECT_TRUE(result);
  EXPECT_FALSE(input.atEnd());

  result = exactChar('o')(input);
  EXPECT_TRUE(result);
  EXPECT_FALSE(input.atEnd());

  result = exactChar('x')(input);
  EXPECT_FALSE(result);
  EXPECT_FALSE(input.atEnd());

  result = exactChar('o')(input);
  EXPECT_TRUE(result);
  EXPECT_TRUE(input.atEnd());
}

TEST(Parsers, SequenceParser) {
  string text = "foo";

  {
    Input input(text.begin(), text.end());
    Maybe<Void> result = sequence(exactChar('f'), exactChar('o'), exactChar('o'))(input);
    EXPECT_TRUE(result);
    EXPECT_TRUE(input.atEnd());
  }

  {
    Input input(text.begin(), text.end());
    Maybe<Void> result = sequence(exactChar('f'), exactChar('o'))(input);
    EXPECT_TRUE(result);
    EXPECT_FALSE(input.atEnd());
  }

  {
    Input input(text.begin(), text.end());
    Maybe<Void> result = sequence(exactChar('x'), exactChar('o'), exactChar('o'))(input);
    EXPECT_FALSE(result);
    EXPECT_FALSE(input.atEnd());
  }
}

TEST(Parsers, TransformParser) {
  string text = "foo";

  auto parser = transform(
      sequence(exactChar('f'), exactChar('o'), exactChar('o')),
      []() -> int { return 123; });

  {
    Input input(text.begin(), text.end());
    Maybe<int> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ(123, *result);
    EXPECT_TRUE(input.atEnd());
  }
}

TEST(Parsers, TransformParser_MaybeRef) {
  struct Transform {
    int value;

    Transform(int value): value(value) {}

    int operator()() const { return value; }
  };

  // Don't use auto for the TransformParsers here because we're trying to make sure that MaybeRef
  // is working correctly.  When transform() is given an lvalue, it should wrap the type in
  // ParserRef.

  TransformParser<ExactElementParser<Input>, Transform> parser1 =
      transform(exactChar('f'), Transform(12));

  auto otherParser = exactChar('o');
  TransformParser<ParserRef<ExactElementParser<Input>>, Transform> parser2 =
      transform(otherParser, Transform(34));

  auto otherParser2 = exactChar('b');
  TransformParser<ExactElementParser<Input>, Transform> parser3 =
      transform(move(otherParser2), Transform(56));

  string text = "foob";
  auto parser = transform(
      sequence(parser1, parser2, exactChar('o'), parser3),
      [](int i, int j, int k) { return i + j + k; });

  {
    Input input(text.begin(), text.end());
    Maybe<int> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ(12 + 34 + 56, *result);
    EXPECT_TRUE(input.atEnd());
  }
}

TEST(Parsers, RepeatedParser) {
  string text = "foooob";

  auto parser = transform(
      sequence(exactChar('f'), repeated(exactChar('o'))),
      [](std::vector<Void>&& values) -> int { return values.size(); });

  {
    Input input(text.begin(), text.begin() + 3);
    Maybe<int> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ(2, *result);
    EXPECT_TRUE(input.atEnd());
  }

  {
    Input input(text.begin(), text.begin() + 5);
    Maybe<int> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ(4, *result);
    EXPECT_TRUE(input.atEnd());
  }

  {
    Input input(text.begin(), text.end());
    Maybe<int> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ(4, *result);
    EXPECT_FALSE(input.atEnd());
  }
}

TEST(Parsers, OneOfParser) {
  auto parser = oneOf(
      transform(sequence(exactChar('f'), exactChar('o'), exactChar('o')),
                []() -> string { return "foo"; }),
      transform(sequence(exactChar('b'), exactChar('a'), exactChar('r')),
                []() -> string { return "bar"; }));

  {
    string text = "foo";
    Input input(text.begin(), text.end());
    Maybe<string> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ("foo", *result);
    EXPECT_TRUE(input.atEnd());
  }

  {
    string text = "bar";
    Input input(text.begin(), text.end());
    Maybe<string> result = parser(input);
    ASSERT_TRUE(result);
    EXPECT_EQ("bar", *result);
    EXPECT_TRUE(input.atEnd());
  }
}

}  // namespace
}  // namespace parser
}  // namespace modc
