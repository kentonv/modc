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

#ifndef KENTONSCODE_MODC_CODEPRINTER_H_
#define KENTONSCODE_MODC_CODEPRINTER_H_

#include <string>
#include <vector>
#include <utility>
#include <iosfwd>
#include <stdint.h>

namespace modc {

using std::string;
using std::vector;
using std::move;

struct Space {};
static Space space __attribute__((unused));
struct NoSpace {};
static NoSpace noSpace __attribute__((unused));

struct EndStatement {};
static EndStatement endStatement __attribute__((unused));
struct StartBlock {};
static StartBlock startBlock __attribute__((unused));
struct EndBlock {};
static EndBlock endBlock __attribute__((unused));

struct Breakable {
  uint8_t priority;

  explicit Breakable(uint8_t priority): priority(priority) {}
};
inline Breakable breakable(int priority) { return Breakable(priority); }
struct StartSubExpression {};
static StartSubExpression startSubExpression __attribute__((unused));
struct EndSubExpression {};
static EndSubExpression endSubExpression __attribute__((unused));

struct StartParameters {
  uint8_t chainPriority;

  explicit StartParameters(uint8_t chainPriority): chainPriority(chainPriority) {}
};
inline StartParameters startParameters(int chainPriority) { return StartParameters(chainPriority); }
struct NextParameter {};
static NextParameter nextParameter __attribute__((unused));
struct EndParameters {};
static EndParameters endParameters __attribute__((unused));

// breakableStartParams(priority)
// breakableNextParam
// end parameters

// breakable(priority)
// startSubExpression
// endSubExpression

class CodePrinter {
public:
  CodePrinter(std::ostream& out, string::size_type lineWidth);
  ~CodePrinter();

  // If the previous line matches exactly the given text (not including indent), and nothing has
  // been printed yet on the current line, go back to the previous line.  Useful for making "else"
  // appear after a closing brace.
  void extendLineIfEquals(const char* text);

  CodePrinter& operator<<(const char* literalText);
  CodePrinter& operator<<(const string& text);

  CodePrinter& operator<<(Space);
  CodePrinter& operator<<(NoSpace);

  CodePrinter& operator<<(EndStatement);
  CodePrinter& operator<<(StartBlock);
  CodePrinter& operator<<(EndBlock);

  CodePrinter& operator<<(Breakable marker);
  CodePrinter& operator<<(StartSubExpression);
  CodePrinter& operator<<(EndSubExpression);
  CodePrinter& operator<<(StartParameters marker);
  CodePrinter& operator<<(NextParameter);
  CodePrinter& operator<<(EndParameters);

private:
  std::ostream& out;
  const string::size_type lineWidth;

  int indentLevel;
  bool nextWriteStartsNewStatement;

  string lineBuffer;

  struct BreakPoint {
    int pos;
    bool isCall:1;
    unsigned int priority:31;

    int realPriority() const {
      return isCall ? ((priority | 255) + 1) : priority;
    }

    BreakPoint(int pos, bool isCall, unsigned int priority)
        : pos(pos), isCall(isCall), priority(priority) {}
  };

  vector<BreakPoint> breakPoints;

  struct Group {
    string::const_iterator begin;
    string::const_iterator end;
    unsigned int width;
    bool isCall:1;
    unsigned int priority:31;
    vector<Group> children;

    Group(string::const_iterator begin, string::const_iterator end, string::size_type width,
          bool isCall, unsigned int priority, vector<Group>&& children)
        : begin(begin), end(end), width(width), isCall(isCall), priority(priority),
          children(move(children)) {}

    const Group* trailingCall() const {
      if (isCall) {
        return this;
      } else if (children.empty()) {
        return nullptr;
      } else {
        return children.back().trailingCall();
      }
    }
  };

  int expressionDepth;

  void addBreakPoint(bool isCall, unsigned int priority);
  Group collectGroup(vector<BreakPoint>::const_iterator& iter, int priority,
                     string::const_iterator begin);
  void applyPendingEndStatement();

  class LayoutEngine;
};

}  // namespace modc

#endif /* KENTONSCODE_MODC_CODEPRINTER_H_ */
