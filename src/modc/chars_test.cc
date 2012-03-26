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

#include "chars.h"

#include "gtest.h"

namespace modc {
namespace chars {
namespace {

TEST(Chars, Simple) {
  EXPECT_TRUE(LETTER.contains('a'));
  EXPECT_FALSE(LETTER.contains('a'-1));
  EXPECT_TRUE(LETTER.contains('z'));
  EXPECT_FALSE(LETTER.contains('z'+1));
  EXPECT_FALSE(LETTER.contains('a'-64));
  EXPECT_FALSE(LETTER.contains('a'+64));
  EXPECT_FALSE(LETTER.contains('a'+128));

  EXPECT_TRUE(TYPES['a'] == CharType::LETTER);
  EXPECT_TRUE(TYPES['z'] == CharType::LETTER);
  EXPECT_TRUE(TYPES['a'-1] != CharType::LETTER);
  EXPECT_TRUE(TYPES['z'+1] != CharType::LETTER);

  EXPECT_TRUE(WHITESPACE.contains('\n'));
  EXPECT_TRUE(WHITESPACE.contains('\r'));
  EXPECT_TRUE(WHITESPACE.contains('\t'));
  EXPECT_TRUE(WHITESPACE.contains(' '));

  {
    CharClass test('c');
    EXPECT_TRUE(test.contains('c'));
    EXPECT_FALSE(test.contains('b'));
    EXPECT_FALSE(test.contains('d'));
    EXPECT_FALSE(test.contains('c'-64));
    EXPECT_FALSE(test.contains('c'+64));
    EXPECT_FALSE(test.contains('c'+128));
  }

  EXPECT_FALSE(CharClass('{').contains(';'));
}

}  // namespace
}  // namespace chars
}  // namespace modc
