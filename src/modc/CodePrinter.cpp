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
#include "Maybe.h"
#include "chars.h"
#include <limits>
#include <ostream>

namespace modc {

using std::move;

// =======================================================================================

FormattedCode::FormattedCode() {
  pieces.emplace_back(currentFormat, string());
}
FormattedCode::~FormattedCode() {}

void FormattedCode::append(string&& text) {
  pieces.back() = Piece(currentFormat, move(text));
  pieces.emplace_back(currentFormat, string());
}
void FormattedCode::append(string&& text, Format format) {
  pieces.back() = Piece(currentFormat + format, move(text));
  pieces.emplace_back(currentFormat, string());
}
void FormattedCode::ensureTrailingSpace() {
  if (!empty() && back().text != " ") {
    append(" ");
  }
}
void FormattedCode::stripTrailingSpace() {
  if (!empty() && back().text == " ") {
    pieces.pop_back();
  }
}
FormattedCode::iterator FormattedCode::beforeTrailingSpace() {
  if (!empty() && back().text == " ") {
    FormattedCode::iterator result = end();
    --result;
    return result;
  } else {
    return end();
  }
}
FormattedCode::iterator FormattedCode::afterLeadingSpace() {
  if (!empty() && front().text == " ") {
    FormattedCode::iterator result = begin();
    ++result;
    return result;
  } else {
    return begin();
  }
}

string FormattedCode::toString() const {
  string result;
  for (auto& piece: pieces) {
    result += piece.text;
  }
  return result;
}

// =======================================================================================

FormattedCodeWriter::~FormattedCodeWriter() {}

TextCodeWriter::TextCodeWriter(std::ostream& out, FormattedCodeWriter::Metrics metrics)
    : out(out), metrics(metrics) {}
TextCodeWriter::~TextCodeWriter() {}

void TextCodeWriter::writeStatement(int blockLevel, vector<FormattedLine>&& lines) {
  for (auto& line: lines) {
    TextWidth indent = blockLevel * metrics.blockIndentWidth +
        line.indentLevel * metrics.indentWidth;
    for (TextWidth i = 0; i < indent; i++) {
      out.put(' ');
    }

    for (auto& piece: line.code) {
      out << piece.text;
    }

    out << '\n';
  }
}

void TextCodeWriter::writeBlankLine() {
  out << '\n';
}

TextWidth TextCodeWriter::getWidth(FormattedCode::const_iterator begin,
                                   FormattedCode::const_iterator end) {
  TextWidth result = 0;
  for (auto iter = begin; iter != end; ++iter) {
    for (unsigned char c: iter->text) {
      // Don't count UTF-8 inner bytes.
      if ((c & 0xC0) != 0x80) {
        ++result;
      }
    }
  }
  return result;
}

FormattedCodeWriter::Metrics TextCodeWriter::getMetrics(int blockLevel) {
  Metrics result = metrics;
  result.availableWidth -= blockLevel * metrics.blockIndentWidth;
  return result;
}

// =======================================================================================

CodePrinter::CodePrinter(FormattedCodeWriter& out)
    : out(out), indentLevel(0), nextWriteStartsNewStatement(false), expressionDepth(1) {}
CodePrinter::~CodePrinter() {
  applyPendingEndStatement();
}

void CodePrinter::extendLineIfEquals(const char* text) {
  if (lineBuffer.toString() == text) {
    nextWriteStartsNewStatement = false;
  }
}

CodePrinter& CodePrinter::operator<<(const char* literalText) {
  applyPendingEndStatement();

  if (*literalText == ' ') {
    *this << space;
    ++literalText;
  }

  while (*literalText != '\0') {
    if (chars::LETTER.contains(*literalText)) {
      const char* start = literalText;
      while (chars::ALPHANUMERIC.contains(*literalText)) {
        ++literalText;
      }
      lineBuffer.append(string(start, literalText), format::KEYWORD);
    } else if (chars::DIGIT.contains(*literalText)) {
      const char* start = literalText;
      while (chars::ALPHANUMERIC.contains(*literalText)) {
        ++literalText;
      }
      lineBuffer.append(string(start, literalText), format::LITERAL);
    } else if (chars::OPERATOR.contains(*literalText)) {
      const char* start = literalText;
      while (chars::OPERATOR.contains(*literalText)) {
        ++literalText;
      }
      lineBuffer.append(string(start, literalText), format::OPERATOR);
    } else if (chars::BRACKET.contains(*literalText)) {
      const char* start = literalText;
      while (chars::BRACKET.contains(*literalText)) {
        ++literalText;
      }
      lineBuffer.append(string(start, literalText), format::BRACKET);
    } else {
      lineBuffer.append(string(1, *literalText));
      ++literalText;
    }
  }

  return *this;
}

CodePrinter& CodePrinter::operator<<(string&& text) {
  applyPendingEndStatement();

  if (!text.empty()) {
    lineBuffer.append(move(text));
  }
  return *this;
}

CodePrinter& CodePrinter::operator<<(Space) {
  applyPendingEndStatement();
  lineBuffer.ensureTrailingSpace();
  return *this;
}

CodePrinter& CodePrinter::operator<<(NoSpace) {
  applyPendingEndStatement();
  lineBuffer.stripTrailingSpace();
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
  auto pos = lineBuffer.beforeTrailingSpace();
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
  LayoutEngine(FormattedCodeWriter::Metrics metrics, FormattedCode& lineBuffer)
      : metrics(metrics), lineBuffer(lineBuffer) {}

  vector<FormattedLine> layout(const Group& root) {
    lines.clear();
    currentLine.indentLevel = 0;
    currentLine.code.clear();
    currentLineWidth = 0;

    layout(root, 1);

    if (!currentLine.code.empty()) {
      lines.push_back(move(currentLine));
    }

    return move(lines);
  }

private:
  const FormattedCodeWriter::Metrics metrics;
  FormattedCode& lineBuffer;

  vector<FormattedLine> lines;

  FormattedLine currentLine;
  int currentLineWidth;

  void layout(const Group& group, int indentLevel);

  void addToLine(const Group& group) {
    tryAddToLine(group, true);
  }

  bool tryAddToLine(const Group& group, bool force = false) {
    if (group.children.empty() && currentLine.code.empty()) {
      // We're on a fresh line and there's not enough space.  Write it anyway.
      force = true;
    }

    auto begin = group.begin;
    auto end = group.end;
    assert(begin != end);

    int width = group.width;
    if (currentLine.code.empty() && begin->text == " ") {
      ++begin;
      width -= metrics.spaceWidth;
    }

    if (force || currentLine.indentLevel * metrics.indentWidth + currentLineWidth + width <=
          metrics.availableWidth) {
      currentLine.code.splice(lineBuffer, begin, end);
      currentLineWidth += width;
      return true;
    } else {
      return false;
    }
  }

  bool haveSpaceFor(FormattedCode::iterator begin, FormattedCode::iterator end,
                    unsigned int width) {
    assert(begin != end);
    if (currentLine.code.empty() && begin->text == " ") {
      ++begin;
      width -= metrics.spaceWidth;
    }

    return currentLine.indentLevel * metrics.indentWidth + currentLineWidth + width
        <= metrics.availableWidth;
  }

  bool haveSpaceOnNewLineFor(FormattedCode::iterator begin, FormattedCode::iterator end,
                             unsigned int width, int indentLevel) {
    assert(begin != end);
    if (currentLine.code.empty() && begin->text == " ") {
      width -= metrics.spaceWidth;
    }

    return currentLine.indentLevel * metrics.indentWidth + width <= metrics.availableWidth;
  }

  bool onFreshLine() {
    return currentLine.code.empty();
  }

  unsigned int lineWidthSoFar() {
    return currentLineWidth;
  }

  void wrap(int newIndentLevel) {
    if (onFreshLine()) {
      currentLine.indentLevel = newIndentLevel;
    } else {
      lines.emplace_back(move(currentLine));
      currentLine.code.clear();
      currentLine.indentLevel = newIndentLevel;
      currentLineWidth = 0;
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
                (indentLevel * metrics.indentWidth + metrics.forceWrapFirstParameterThreshold)) {
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
    FormattedCode::iterator begin) {
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
        Group child(begin, iter->pos, width, isCall, groupPriority, move(children));
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
    width = out.getWidth(begin, iter->pos);
  }

  return Group(begin, iter->pos, width, isCall, groupPriority, move(children));
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

  lineBuffer.stripTrailingSpace();

  vector<BreakPoint>::const_iterator iter = breakPoints.begin();
  Group root = collectGroup(iter, 0, lineBuffer.begin());

  assert(root.begin == lineBuffer.begin());
  assert(root.end == breakPoints.back().pos);
  assert(root.width == out.getWidth(lineBuffer.begin(), lineBuffer.end()));

  if (root.begin == root.end) {
    out.writeBlankLine();
  } else {
    LayoutEngine layoutEngine(out.getMetrics(indentLevel), lineBuffer);
    out.writeStatement(indentLevel, layoutEngine.layout(root));
  }

  breakPoints.clear();
  lineBuffer.clear();

  expressionDepth = 1;
}

}  // namespace modc
