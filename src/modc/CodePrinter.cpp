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
    : lineWidth(lineWidth), indentLevel(0), lineStart(0), indentedLineStart(0),
      nextWriteStartsNewStatement(false), nextWriteCanBreak(true) {}
CodePrinter::~CodePrinter() {}

void CodePrinter::startNewLine(int leadingSpaceCount) {
  buffer.erase(buffer.find_last_not_of(' ') + 1);
  buffer += '\n';
  lineStart = buffer.size();
  buffer.append(leadingSpaceCount, ' ');
  indentedLineStart = buffer.size();
}

void CodePrinter::nextStatement() {
  if (nextWriteStartsNewStatement) {
    flushUnbreakable();
  }
  nextWriteStartsNewStatement = true;
}

void CodePrinter::wrapLine() {
  startNewLine(indentLevel * INDENT_WIDTH + WRAP_INDENT_WIDTH);
}

void CodePrinter::enterBlock() {
  ++indentLevel;
}

void CodePrinter::leaveBlock() {
  bool newStatement = nextWriteStartsNewStatement;
  nextWriteStartsNewStatement = false;
  flushUnbreakable();
  nextWriteStartsNewStatement = newStatement;
  --indentLevel;
}

void CodePrinter::extendLineIfEquals(const char* text) {
  if (!nextWriteStartsNewStatement) {
    return;
  }

  nextWriteStartsNewStatement = false;
  flushUnbreakable();

  if (buffer.substr(indentedLineStart) != text) {
    startNewLine(indentLevel * INDENT_WIDTH);
  }
}

void CodePrinter::advanceToColumn(string::size_type column) {
  flushUnbreakable();
  if (buffer.size() - lineStart < column) {
    buffer.append(column - (buffer.size() - lineStart), ' ');
  }
}

void CodePrinter::flushUnbreakable() {
  if (!unbreakableText.empty()) {
    string::size_type length = countChars(unbreakableText);
    string::size_type last_nonspace = unbreakableText.find_last_not_of(' ') + 1;

    if (buffer.size() == indentedLineStart) {
      // We're at the beginning of a line, so don't wrap even if there is not enough space!
      if (last_nonspace == 0) {
        // Don't add spaces at the beginning of a line.
        return;
      }
    } else if (buffer.size() + last_nonspace > lineStart + lineWidth) {
      // Not enough space on current line.  Wrap.
      wrapLine();
    }

    buffer += unbreakableText;
    unbreakableText.clear();
  }

  if (nextWriteStartsNewStatement) {
    nextWriteStartsNewStatement = false;
    startNewLine(indentLevel * INDENT_WIDTH);
  }
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
  } else if (buffer.size() > indentedLineStart) {
    ensureTrailingSpace(buffer);
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(NoSpace) {
  if (!unbreakableText.empty()) {
    ensureNoTrailingSpace(unbreakableText);
  } else if (buffer.size() > indentedLineStart) {
    ensureNoTrailingSpace(buffer);
  }
  return *this;
}

}  // namespace modc
