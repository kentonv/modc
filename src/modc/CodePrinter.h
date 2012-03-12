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

class CodePrinter {
public:
  CodePrinter(string::size_type lineWidth);
  ~CodePrinter();

  const string& getText() { return buffer; }

  void nextStatement();
  void wrapLine();
  void enterBlock();
  void leaveBlock();

  void requireSpace();

  void write(const string& text);
  void writePrefix(const string& text);
  void writeSuffix(const string& text);
  void writeInfix(const string& text);

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
