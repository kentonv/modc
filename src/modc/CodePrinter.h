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
#include <list>
#include <utility>
#include <iosfwd>
#include <stdint.h>
#include "base/OwnedPtr.h"

namespace modc {

using std::string;
using std::vector;
using std::list;
using std::move;

class Format {
public:
  constexpr Format(): bits(0) {}
  static constexpr Format fromBit(int bit) {
    return Format(1 << bit);
  }

  Format operator|(const Format& other) const { return Format(bits |  other.bits); }
  Format operator&(const Format& other) const { return Format(bits &  other.bits); }
  Format operator+(const Format& other) const { return Format(bits |  other.bits); }
  Format operator-(const Format& other) const { return Format(bits & ~other.bits); }

  Format& operator|=(const Format& other) { bits |=  other.bits; return *this; }
  Format& operator&=(const Format& other) { bits &=  other.bits; return *this; }
  Format& operator+=(const Format& other) { bits |=  other.bits; return *this; }
  Format& operator-=(const Format& other) { bits &= ~other.bits; return *this; }

  bool isSupersetOf(const Format& other) const { return (bits & other.bits) == other.bits; }

  bool operator==(const Format& other) const { return bits == other.bits; }
  bool operator!=(const Format& other) const { return bits != other.bits; }
  bool operator<=(const Format& other) const { return bits <= other.bits; }
  bool operator>=(const Format& other) const { return bits >= other.bits; }
  bool operator< (const Format& other) const { return bits <  other.bits; }
  bool operator> (const Format& other) const { return bits >  other.bits; }

  class iterator {
  public:
    iterator& operator++() {
      remainingBits &= remainingBits - 1;
      return *this;
    }
    Format operator*() {
      return Format(((remainingBits ^ (remainingBits - 1)) + 1) >> 1);
    }

    bool operator==(const iterator& other) const { return remainingBits == other.remainingBits; }
    bool operator!=(const iterator& other) const { return remainingBits != other.remainingBits; }

  private:
    friend class Format;
    iterator(uint32_t remainingBits): remainingBits(remainingBits) {}

    uint32_t remainingBits;
  };

  iterator begin() { return iterator(bits); }
  iterator end() { return iterator(0); }

private:
  uint32_t bits;

  explicit constexpr Format(uint32_t bits): bits(bits) {}
};

namespace format {
  constexpr Format ERROR = Format::fromBit(0);
  constexpr Format COMMENT = Format::fromBit(1);

  constexpr Format KEYWORD = Format::fromBit(2);
  constexpr Format KEYSYMBOL = Format::fromBit(3);
  constexpr Format OPERATOR = Format::fromBit(4);
  constexpr Format BRACKET = Format::fromBit(5);
  constexpr Format DELIMITER = Format::fromBit(6);
  constexpr Format LITERAL = Format::fromBit(7);

  constexpr Format LOCAL = Format::fromBit(8);
  constexpr Format MEMBER = Format::fromBit(9);
  constexpr Format GLOBAL = Format::fromBit(10);

  // If this identifier is defining a name, rather than referencing one.
  constexpr Format DECLARATION = Format::fromBit(11);
}

class FormattedCode {
public:
  struct Piece {
    Format format;
    string text;

    Piece(Format format, string&& text): format(format), text(move(text)) {}
  };

  typedef list<Piece>::iterator iterator;
  typedef list<Piece>::const_iterator const_iterator;

  FormattedCode();
  ~FormattedCode();

  void append(string&& text, Format format);
  void ensureTrailingSpace(Format format);
  void stripTrailingSpace();
  iterator beforeTrailingSpace();
  iterator afterLeadingSpace();

  iterator begin() { return pieces.begin(); }
  iterator end() { auto end = pieces.end(); --end; return end; }
  const_iterator begin() const { return pieces.begin(); }
  const_iterator end() const { auto end = pieces.end(); --end; return end; }
  bool empty() const { return begin() == end(); }
  void clear() { pieces.erase(begin(), end()); }

  Piece& back() { auto end = pieces.end(); --end; --end; return *end; }
  Piece& front() { return pieces.front(); }

  void splice(FormattedCode& other, iterator begin, iterator end) {
    pieces.splice(this->end(), other.pieces, begin, end);
  }

  string toString() const;

private:
  list<Piece> pieces;
};

struct FormattedLine {
  int indentLevel;
  FormattedCode code;
};

typedef unsigned int TextWidth;

class FormattedCodeWriter {
public:
  virtual ~FormattedCodeWriter();

  struct Metrics {
    TextWidth availableWidth;
    TextWidth spaceWidth;
    TextWidth indentWidth;
    TextWidth blockIndentWidth;
    TextWidth forceWrapFirstParameterThreshold;
  };

  virtual void writeStatement(int blockLevel, vector<FormattedLine>&& lines) = 0;
  virtual void writeBlankLine() = 0;
  virtual TextWidth getWidth(FormattedCode::const_iterator begin,
                             FormattedCode::const_iterator end) = 0;
  virtual Metrics getMetrics(int blockLevel) = 0;
};

class TextCodeWriter: public FormattedCodeWriter {
public:
  TextCodeWriter(std::ostream& out, FormattedCodeWriter::Metrics metrics);
  ~TextCodeWriter();

  // implements FormattedCodeWriter ------------------------------------------------------

  void writeStatement(int blockLevel, vector<FormattedLine>&& lines);
  void writeBlankLine();
  TextWidth getWidth(FormattedCode::const_iterator begin, FormattedCode::const_iterator end);
  Metrics getMetrics(int blockLevel);

private:
  std::ostream& out;
  FormattedCodeWriter::Metrics metrics;
};

class HtmlCodeWriter: public FormattedCodeWriter {
public:
  HtmlCodeWriter(std::ostream& out, FormattedCodeWriter::Metrics metrics);
  ~HtmlCodeWriter();

  // implements FormattedCodeWriter ------------------------------------------------------

  void writeStatement(int blockLevel, vector<FormattedLine>&& lines);
  void writeBlankLine();
  TextWidth getWidth(FormattedCode::const_iterator begin, FormattedCode::const_iterator end);
  Metrics getMetrics(int blockLevel);

private:
  std::ostream& out;
  FormattedCodeWriter::Metrics metrics;

  int currentBlockLevel;
};

// =======================================================================================

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

struct Formatted {
  Format format;
  string text;

  Formatted(Format format, string&& text): format(format), text(move(text)) {}
};
inline Formatted formatted(Format format, const string& text) {
  return Formatted(format, string(text));
}
inline Formatted formatted(Format format, string&& text) {
  return Formatted(format, move(text));
}

class CodePrinter {
public:
  CodePrinter(FormattedCodeWriter& out);
  ~CodePrinter();

  class FormatScope {
  public:
    FormatScope(FormatScope& other) = delete;
    FormatScope(const FormatScope& other) = delete;
    FormatScope(FormatScope&& other): printer(other.printer), old(other.old), disabled(false) {
      other.disabled = true;
    }
    ~FormatScope() {
      printer.currentFormat = old;
    }
  private:
    friend class CodePrinter;
    FormatScope(CodePrinter& printer, Format old): printer(printer), old(old), disabled(false) {}

    CodePrinter& printer;
    Format old;
    bool disabled;
  };

  FormatScope addFormat(Format format) {
    Format old = currentFormat;
    currentFormat += format;
    return FormatScope(*this, old);
  }
  FormatScope removeFormat(Format format) {
    Format old = currentFormat;
    currentFormat -= format;
    return FormatScope(*this, old);
  }

  // If the previous line matches exactly the given text (not including indent), and nothing has
  // been printed yet on the current line, go back to the previous line.  Useful for making "else"
  // appear after a closing brace.
  void extendLineIfEquals(const char* text);

  CodePrinter& operator<<(const char* literalText);
  CodePrinter& operator<<(const string& text) { return *this << string(text); }
  CodePrinter& operator<<(string&& text);
  CodePrinter& operator<<(Formatted&& formatted);

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
  FormattedCodeWriter& out;

  int indentLevel;
  bool nextWriteStartsNewStatement;

  FormattedCode lineBuffer;

  Format currentFormat;

  struct BreakPoint {
    FormattedCode::iterator pos;
    bool isCall;
    unsigned int priority;

    int realPriority() const {
      return isCall ? ((priority | 255) + 1) : priority;
    }

    BreakPoint(FormattedCode::iterator pos, bool isCall, unsigned int priority)
        : pos(pos), isCall(isCall), priority(priority) {}
  };

  vector<BreakPoint> breakPoints;

  struct Group {
    FormattedCode::iterator begin;
    FormattedCode::iterator end;
    unsigned int width;
    bool isCall:1;
    unsigned int priority:31;
    vector<Group> children;

    Group(FormattedCode::iterator begin, FormattedCode::iterator end,
          unsigned int width, bool isCall, unsigned int priority, vector<Group>&& children)
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
                     FormattedCode::iterator begin);
  void applyPendingEndStatement();

  class LayoutEngine;
};

}  // namespace modc

#endif /* KENTONSCODE_MODC_CODEPRINTER_H_ */
