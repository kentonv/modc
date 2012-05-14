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
#include "base/Debug.h"
#include <assert.h>

namespace modc {

using std::move;

static string::size_type countChars(string::const_iterator begin, string::const_iterator end) {
  while (begin < end && *begin == ' ') {
    ++begin;
  }
  while (begin < end && *end == ' ') {
    --end;
  }

  string::size_type result = 0;
  while (begin < end) {
    // Don't count UTF-8 inner bytes.
    if ((*begin++ & 0xC0) != 0x80) {
      ++result;
    }
  }

  return result;
}

static const int INDENT_WIDTH = 2;
static const int WRAP_INDENT_WIDTH = 4;

CodePrinter::CodePrinter(std::ostream& out, string::size_type lineWidth)
    : out(out), lineWidth(lineWidth), indentLevel(0),
      nextWriteStartsNewStatement(false), nextWriteCanBreak(true) {}
CodePrinter::~CodePrinter() {}

CodePrinter& CodePrinter::operator<<(const string& text) {
  if (nextWriteCanBreak) {
    bool trailingSpace = !lineBuffer.empty() && lineBuffer.back() == ' ';
    breakablePoints.push_back(lineBuffer.size() - trailingSpace);
  }
  nextWriteCanBreak = true;

  lineBuffer += text;
  return *this;
}

CodePrinter& CodePrinter::operator<<(Space) {
  if (!lineBuffer.empty() && lineBuffer.back() != ' ') {
    lineBuffer += ' ';
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(NoSpace) {
  if (!lineBuffer.empty() && lineBuffer.back() == ' ') {
    lineBuffer.erase(lineBuffer.size() - 1);
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndStatement) {
  while (!currentGroups.empty()) {
    DEBUG_ERROR << "Mismatched start-group.";
    *this << endGroup;
  }

  if (breakablePoints.empty() || breakablePoints.back() < lineBuffer.size()) {
    breakablePoints.push_back(lineBuffer.size());
  }

  string::size_type availableSpace = lineWidth - indentLevel * INDENT_WIDTH;
  layoutLine(availableSpace, availableSpace, lineBuffer.begin(), lineBuffer.end(),
             groups.begin(), groups.end());

  nextWriteCanBreak = true;
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartBlock) {
  ++indentLevel;
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndBlock) {
  --indentLevel;
  return *this;
}

CodePrinter& CodePrinter::operator<<(Breakable marker) {
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartSubExpression) {
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndSubExpression) {
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartParameters marker) {
  return *this;
}

CodePrinter& CodePrinter::operator<<(NextParameter) {
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndParameters) {
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartGroup) {
  currentGroups.push_back(groups.size());
  groups.emplace_back();
  groups.back().startBreak = breakablePoints.size();
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndGroup) {
  if (currentGroups.empty()) {
    DEBUG_ERROR << "Mismatched end-group.";
    return *this;
  }

  bool isPastLastBreak = lineBuffer.size() > (breakablePoints.empty() ? 0 : breakablePoints.back());

  Group& group = groups[currentGroups.back()];
  group.endBreak = breakablePoints.size() + isPastLastBreak;

  currentGroups.pop_back();

  return *this;
}

class CodePrinter::GroupIterator {
public:
  GroupIterator(vector<Group>::const_iterator pos, vector<Group>::const_iterator end)
      : pos(pos), end(end) {}

  const Group& operator*() { return *pos; }
  const Group* operator->() { return pos.operator->(); }

  bool nextIsBefore(int chunk) {
    return pos < end && pos->startBreak < chunk;
  }

  void advanceTo(int chunk) {
    while (nextIsBefore(chunk)) {
      ++pos;
    }
  }

private:
  vector<Group>::const_iterator pos;
  vector<Group>::const_iterator end;
};

class CodePrinter::LayoutEngine {
public:

  void layout(int beginChunk, int endChunk);

private:
  string buffer;

  int lineWidth;
  int indentAmount;
  int usedOnLine;

  vector<Group>::const_iterator groupPos;
  vector<Group>::const_iterator groupEnd;

  void addToLine(int chunk);
  bool tryAddToLine(int beginChunk, int endChunk);
  bool haveSpaceFor(int chunk);
  bool haveSpaceOnNewLineFor(int chunk);

  bool startNewLineWithIndent(int newIndentAmount) {
    if (indentAmount == newIndentAmount && usedOnLine == 0) {
      // Already on a fresh line.
      return false;
    } else {
      indentAmount = newIndentAmount;
      buffer += '\n';
      buffer.append(indentAmount, ' ');
      return true;
    }
  }

  void startNewIndentedLine() {
    startNewLineWithIndent(indentAmount + WRAP_INDENT_WIDTH);
  }

  bool nextGroupIsBefore(int chunk) {
    return groupPos < groupEnd && groupPos->endBreak <= chunk;
  }

  void advanceGroupTo(int chunk) {
    while (nextGroupIsBefore(chunk)) {
      ++groupPos;
    }
  }

  const Group& nextGroup() { return *groupPos; }
};

void CodePrinter::LayoutEngine::layout(int beginChunk, int endChunk) {
  while (true) {
    if (tryAddToLine(beginChunk, endChunk)) {
      return;
    }

    // Write the prefix (stuff before any groups), wrapping as needed.
    int prefixEndChunk;
    if (nextGroupIsBefore(endChunk)) {
      prefixEndChunk = nextGroup().startBreak;
    } else {
      prefixEndChunk = endChunk;
    }

    while (beginChunk < prefixEndChunk) {
      if (!haveSpaceFor(beginChunk)) {
        startNewIndentedLine();
        if (tryAddToLine(beginChunk, endChunk)) {
          return;
        }
      }
      addToLine(beginChunk);
      ++beginChunk;
    }

    startNewIndentedLine();
    int oldIndentAmount = indentAmount;

    // Write consecutive groups.
    while (nextGroupIsBefore(endChunk) && beginChunk == nextGroup().startBreak) {
      if (!tryAddToLine(nextGroup().startBreak, nextGroup().endBreak)) {
        if (!startNewLineWithIndent(oldIndentAmount) ||
            !tryAddToLine(nextGroup().startBreak, nextGroup().endBreak)) {
          layout(nextGroup().startBreak, nextGroup().endBreak);
          startNewLineWithIndent(oldIndentAmount);
        }
      }

      beginChunk = groupPos->endBreak;
    }

    if (beginChunk == endChunk) {
      return;
    }

    startNewLineWithIndent(oldIndentAmount);
  }


  // if (enough space for all text) {
  //   write all text
  // } else {
  //   // write prefix
  //   while (begin break < first group start) {
  //     if (not at line start and not enough space for first chunk) {
  //       write line break
  //       lineWidth -= WRAP_INDENT_WIDTH
  //       availableSpace = lineWidth
  //     }
  //     write first chunk
  //     increment begin break
  //   }
  //   if (everything else would fit on next line) {
  //     put it there
  //   } else if (only one group) {
  //     recurse into that group, but with endBreak = our endBreak, so that it includes our
  //         suffix as if part of its own.
  //   } else {
  //     for earch group {
  //       if the group doesn't fit on the current line, start a new one
  //       if the group now fits on the current line, write it
  //       otherwise {
  //         recurse to write the group, with endBreak = start of next group or our own end break
  //         start a new line
  //       }
  //     }
  //   }
  // }
}




















}  // namespace modc
