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
#include "base/OwnedPtr.h"
#include <assert.h>
#include "Maybe.h"
#include <limits>
#include <ostream>

namespace modc {

using std::move;

static string::size_type countChars(string::const_iterator begin, string::const_iterator end) {
  string::size_type result = 0;
  while (begin < end) {
    // Don't count UTF-8 inner bytes.
    if ((*begin++ & 0xC0) != 0x80) {
      ++result;
    }
  }

  return result;
}

static const unsigned int INDENT_WIDTH = 2;
static const unsigned int WRAP_INDENT_WIDTH = 4;
static const unsigned int FORCE_WRAP_FIRST_PARAMETER_THRESHOLD = 16;

CodePrinter::CodePrinter(std::ostream& out, string::size_type lineWidth)
    : out(out), lineWidth(lineWidth), indentLevel(0),
      nextWriteStartsNewStatement(false), expressionDepth(1) {}
CodePrinter::~CodePrinter() {
  applyPendingEndStatement();
}

void CodePrinter::extendLineIfEquals(const char* text) {
  if (lineBuffer == text) {
    nextWriteStartsNewStatement = false;
  }
}

CodePrinter& CodePrinter::operator<<(const char* literalText) {
  applyPendingEndStatement();
  if (*literalText == ' ') {
    *this << space;
    ++literalText;
  }
  lineBuffer += literalText;
  return *this;
}

CodePrinter& CodePrinter::operator<<(const string& text) {
  applyPendingEndStatement();
  lineBuffer += text;
  return *this;
}

CodePrinter& CodePrinter::operator<<(Space) {
  applyPendingEndStatement();
  if (!lineBuffer.empty() && lineBuffer.back() != ' ') {
    lineBuffer += ' ';
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(NoSpace) {
  applyPendingEndStatement();
  if (!lineBuffer.empty() && lineBuffer.back() == ' ') {
    lineBuffer.erase(lineBuffer.size() - 1);
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndStatement) {
  applyPendingEndStatement();
  nextWriteStartsNewStatement = true;
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartBlock) {
  applyPendingEndStatement();
  ++indentLevel;
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndBlock) {
  applyPendingEndStatement();
  --indentLevel;
  return *this;
}

void CodePrinter::addBreakPoint(bool isCall, unsigned int priority) {
  bool trailingSpace = !lineBuffer.empty() && lineBuffer.back() == ' ';
  int pos = lineBuffer.size() - trailingSpace;
  priority += expressionDepth * 256;

  if (!breakPoints.empty() && breakPoints.back().pos == pos) {
    breakPoints.back().isCall = breakPoints.back().isCall || isCall;
    breakPoints.back().priority = std::min(breakPoints.back().priority, priority);
  } else {
    breakPoints.emplace_back(pos, isCall, priority);
  }
}

CodePrinter& CodePrinter::operator<<(Breakable marker) {
  applyPendingEndStatement();
  addBreakPoint(false, marker.priority);
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartSubExpression) {
  applyPendingEndStatement();
  ++expressionDepth;
  return *this;
}

CodePrinter& CodePrinter::operator<<(EndSubExpression) {
  applyPendingEndStatement();
  --expressionDepth;
  return *this;
}

CodePrinter& CodePrinter::operator<<(StartParameters marker) {
  applyPendingEndStatement();
  addBreakPoint(true, marker.chainPriority);
  return *this << startSubExpression;
}

CodePrinter& CodePrinter::operator<<(NextParameter) {
  return *this << breakable(0);
}

CodePrinter& CodePrinter::operator<<(EndParameters) {
  return *this << endSubExpression;
}

class CodePrinter::LayoutEngine {
public:
  LayoutEngine(unsigned int lineWidth): lineWidth(lineWidth) {
    lines.emplace_back(0);
  }

  void layout(const Group& group, int indentLevel);

  void write(std::ostream& out, int outerIndentAmount) {
    if (lines.back().text.empty()) {
      lines.pop_back();
    }
    for (auto& line: lines) {
      out << string(outerIndentAmount + line.indentLevel * WRAP_INDENT_WIDTH, ' ')
          << line.text << '\n';
    }
  }

private:
  const unsigned int lineWidth;

  struct Line {
    int indentLevel;
    int width;
    string text;

    explicit Line(int indentLevel): indentLevel(indentLevel), width(0) {}
  };
  vector<Line> lines;

  void addToLine(const Group& group) {
    assert(group.begin < group.end);
    bool removedLeadingSpace = lines.back().text.empty() && *group.begin == ' ';
    lines.back().text.append(group.begin + removedLeadingSpace, group.end);
    lines.back().width += group.width - removedLeadingSpace;
  }

  bool tryAddToLine(const Group& group) {
    if (group.children.empty() && lines.back().text.empty()) {
      // We're on a fresh line and there's not enough space.  Write it anyway.
      addToLine(group);
      return true;
    }

    assert(group.begin < group.end);
    bool removedLeadingSpace = lines.back().text.empty() && *group.begin == ' ';
    unsigned int width = group.width - removedLeadingSpace;
    if (lines.back().indentLevel * WRAP_INDENT_WIDTH + lines.back().width + width <= lineWidth) {
      lines.back().text.append(group.begin + removedLeadingSpace, group.end);
      lines.back().width += width;
      return true;
    } else {
      return false;
    }
  }

  bool haveSpaceFor(string::const_iterator begin, string::const_iterator end, unsigned int width) {
    bool removedLeadingSpace = lines.back().text.empty() && *begin == ' ';
    width -= removedLeadingSpace;
    return lines.back().indentLevel * WRAP_INDENT_WIDTH + lines.back().width + width <= lineWidth;
  }

  inline bool haveSpaceFor(const Group& group) {
    return haveSpaceFor(group.begin, group.end, group.width);
  }

  bool haveSpaceOnNewLineFor(string::const_iterator begin, string::const_iterator end,
                             unsigned int width, int indentLevel) {
    bool removedLeadingSpace = *begin == ' ';
    width -= removedLeadingSpace;
    return indentLevel * WRAP_INDENT_WIDTH + width <= lineWidth;
  }

  bool onFreshLine() {
    return lines.back().text.empty();
  }

  int lineWidthSoFar() {
    return lines.back().width;
  }

  void wrap(int newIndentLevel) {
    if (lines.back().text.empty()) {
      lines.back().indentLevel = newIndentLevel;
    } else {
      lines.emplace_back(newIndentLevel);
    }
  }
};

void CodePrinter::LayoutEngine::layout(const Group& group, int indentLevel) {
  if (tryAddToLine(group)) {
    return;
  }

  if (group.children.empty()) {
    wrap(indentLevel);
    addToLine(group);
    return;
  }

  bool haveWrapped = false;
  for (auto& child: group.children) {
    if (tryAddToLine(child)) {
      // If this is a call and all the parameters would fit on the next line, prefer to wrap now
      // so that all parameters get put together.  Otherwise we'll wrap between parameters.
      if (group.isCall && &child == &group.children.front()) {
        if (haveSpaceOnNewLineFor(child.end, group.end, group.width - child.width, indentLevel) ||
            lineWidthSoFar() >
                (indentLevel * WRAP_INDENT_WIDTH + FORCE_WRAP_FIRST_PARAMETER_THRESHOLD)) {
          wrap(indentLevel);
          haveWrapped = true;
        }
      }
    } else {
      // If all of:
      // - We've already wrapped previously OR this is the last child.
      // - The child ends with a parameter list.
      // - We can fit everything before said parameter list on this line.
      // Then:  Go ahead and put everything except the parameter list on this line, and wrap
      //   the parameter list.
      if (haveWrapped || &child == &group.children.back()) {
        const Group* trailingCall = child.trailingCall();
        if (trailingCall != nullptr) {
          int funcWidth = child.width - trailingCall->width + trailingCall->children.front().width;
          if (haveSpaceFor(child.begin, trailingCall->children.front().end, funcWidth)) {
            layout(child, indentLevel + haveWrapped);
            wrap(indentLevel);
            haveWrapped = true;
            continue;
          }
        }
      }

      // OK, just put the child on the next line.
      if (!onFreshLine()) {
        wrap(indentLevel);
      }
      haveWrapped = true;
      if (!tryAddToLine(child)) {
        // Child too big for one line.  Make sure to wrap after the end of the child because we
        // don't want
        layout(child, indentLevel + 1);
        wrap(indentLevel);
      }
    }
  }
}

CodePrinter::Group CodePrinter::collectGroup(
    vector<BreakPoint>::const_iterator& iter, int priority,
    string::const_iterator begin) {
  int groupPriority = std::numeric_limits<int>::max();
  bool isCall = false;
  unsigned int width;
  vector<Group> children;

  if (iter->realPriority() > priority) {
    int priorityOfChildren = iter->realPriority();
    groupPriority = iter->priority;
    isCall = iter->isCall;

    children.push_back(collectGroup(iter, priorityOfChildren, begin));
    width = children.back().width;

    while (true) {
      assert(!children.empty());
      ++iter;

      // Read next child.
      children.push_back(collectGroup(iter, priorityOfChildren, children.back().end));
      width += children.back().width;

      // Where did the child end?
      if (iter->realPriority() <= priority) {
        // At a break point that ends this group!
        break;
      } else if (iter->realPriority() < priorityOfChildren) {
        // Break point was lower-priority than our children so far, meaning those children are
        // actually grand children -- children of our first child.
        Group child(begin, lineBuffer.begin() + iter->pos, width, isCall, groupPriority,
                    move(children));
        children.clear();
        children.push_back(move(child));
        priorityOfChildren = iter->realPriority();
        groupPriority = iter->priority;
        isCall = iter->isCall;
      } else {
        // Break point priority priority was our child priority.  More coming.
        assert(iter->realPriority() == priorityOfChildren);
      }
    }
  } else {
    width = countChars(begin, lineBuffer.begin() + iter->pos);
  }

  return Group(begin, lineBuffer.begin() + iter->pos, width, isCall, groupPriority,
               move(children));
}

void CodePrinter::applyPendingEndStatement() {
  if (!nextWriteStartsNewStatement) {
    return;
  }

  nextWriteStartsNewStatement = false;

  if (expressionDepth < 1) {
    DEBUG_ERROR << "Unmatched endSubExpression.";
  }
  expressionDepth = 0;

  *this << breakable(0);
  lineBuffer.resize(breakPoints.back().pos);

  vector<BreakPoint>::const_iterator iter = breakPoints.begin();
  Group root = collectGroup(iter, 0, lineBuffer.begin());

  assert(root.begin == lineBuffer.begin());
  assert(root.end == lineBuffer.end());
  assert(root.width == countChars(lineBuffer.begin(), lineBuffer.end()));

  if (root.begin == root.end) {
    out << '\n';
  } else {
    LayoutEngine layoutEngine(lineWidth - indentLevel * INDENT_WIDTH);
    layoutEngine.layout(root, 1);
    layoutEngine.write(out, indentLevel * INDENT_WIDTH);
  }

  breakPoints.clear();
  lineBuffer.clear();

  expressionDepth = 1;
}

}  // namespace modc
