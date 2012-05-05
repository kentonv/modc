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

#include "CodePrinter.h"

namespace modc {

static string::size_type countChars(const string& text) {
  string::size_type result = 0;
  for (char c : text) {
    // Don't count UTF-8 inner bytes.
    if ((c & 0xC0) != 0x80) {
      ++result;
    }
  }
  return result;
}

static const int INDENT_WIDTH = 2;
static const int WRAP_INDENT_WIDTH = 4;

CodePrinter::CodePrinter(string::size_type lineWidth)
    : lineWidth(lineWidth), indentLevel(0), column(0), lineStart(0), nextWriteCanBreak(true) {}
CodePrinter::~CodePrinter() {}

void CodePrinter::startNewLine(int leadingSpaceCount) {
  flushUnbreakable();
  buffer.erase(buffer.find_last_not_of(' ') + 1);
  buffer += '\n';
  buffer.append(leadingSpaceCount, ' ');
  column = leadingSpaceCount;
  lineStart = buffer.size();
}

void CodePrinter::nextStatement() {
  startNewLine(indentLevel * INDENT_WIDTH);
}

void CodePrinter::wrapLine() {
  startNewLine(indentLevel * INDENT_WIDTH + WRAP_INDENT_WIDTH);
}

void CodePrinter::enterBlock() {
  ++indentLevel;
  nextStatement();
}

void CodePrinter::leaveBlock() {
  --indentLevel;
  if (buffer.size() == lineStart) {
    if (indentLevel >= 0) {
      buffer.erase(buffer.size() - INDENT_WIDTH);
    }
  } else {
    nextStatement();
  }
}

void CodePrinter::flushUnbreakable() {
  if (unbreakableText.empty()) {
    return;
  }

  string::size_type length = countChars(unbreakableText);
  string::size_type last_nonspace = unbreakableText.find_last_not_of(' ') + 1;

  if (buffer.size() == lineStart) {
    // We're at the beginning of a line, so don't wrap even if there is not enough space!
    if (last_nonspace == 0) {
      // Don't add spaces at the beginning of a line.
      return;
    }
  } else if (column + last_nonspace > lineWidth) {
    // Not enough space on current line.  Wrap.
    wrapLine();
  }

  buffer += unbreakableText;
  column += length;
  unbreakableText.clear();
}

CodePrinter& CodePrinter::operator<<(const string& text) {
  if (nextWriteCanBreak) flushUnbreakable();

  unbreakableText += text;

  nextWriteCanBreak = true;

  return *this;
}

namespace {

inline void ensureTrailingSpace(string& str) {
  if (str.back() != ' ') {
    str.push_back(' ');
  }
}

inline void ensureNoTrailingSpace(string& str) {
  if (str.back() == ' ') {
    str.pop_back();
  }
}

}

CodePrinter& CodePrinter::operator<<(Space) {
  if (!unbreakableText.empty()) {
    ensureTrailingSpace(unbreakableText);
  } else if (buffer.size() > lineStart) {
    ensureTrailingSpace(buffer);
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(NoSpace) {
  if (!unbreakableText.empty()) {
    ensureNoTrailingSpace(unbreakableText);
  } else if (buffer.size() > lineStart) {
    ensureNoTrailingSpace(buffer);
  }
  return *this;
}

}  // namespace modc
