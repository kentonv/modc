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

namespace modc {

using std::string;

class Glue {};
static Glue glue __attribute__((unused));
class EndStatement {};
static EndStatement endStatement __attribute__((unused));
class StartBlock {};
static StartBlock startBlock __attribute__((unused));
class EndBlock {};
static EndBlock endBlock __attribute__((unused));
class Space {};
static Space space __attribute__((unused));
class NoSpace {};
static NoSpace noSpace __attribute__((unused));

class CodePrinter {
public:
  CodePrinter(string::size_type lineWidth);
  ~CodePrinter();

  const string& getText() { flushUnbreakable(); return buffer; }

  // Begin new statement.
  void nextStatement();

  // Begin new line of current statement (usually automatic).
  void wrapLine();

  // Modify indent level and call nextStatement().
  void enterBlock();
  void leaveBlock();

  CodePrinter& operator<<(const string& text);
  CodePrinter& operator<<(Glue) { nextWriteCanBreak = false; return *this; }
  CodePrinter& operator<<(Space);
  CodePrinter& operator<<(NoSpace);

  CodePrinter& operator<<(EndStatement) { nextStatement(); return *this; }
  CodePrinter& operator<<(StartBlock) { enterBlock(); return *this; }
  CodePrinter& operator<<(EndBlock) { leaveBlock(); return *this; }

private:
  const string::size_type lineWidth;
  string buffer;
  string unbreakableText;

  int indentLevel;
  string::size_type column;
  string::size_type lineStart;

  bool nextWriteCanBreak;

  void startNewLine(int leadingSpaceCount);

  void flushUnbreakable();
};

}  // namespace modc

#endif /* KENTONSCODE_MODC_CODEPRINTER_H_ */
