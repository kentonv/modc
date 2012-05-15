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

#include <iostream>
#include <string>
#include "tokens.h"
#include "astParser.h"
#include "ast.h"
#include "errors.h"
#include "CodePrinter.h"
#include "base/Debug.h"

using std::cout;
using std::cerr;
using std::endl;
using std::vector;

int main(int argc, char* argv[]) {
  std::string text, line;

  bool executable = false;

  if (std::getline(std::cin, line)) {
    if (line.substr(0, 3) == "#!/" || line.substr(0, 4) == "#! /") {
      executable = true;
      cout << line << '\n';
    } else {
      text += line;
      text += '\n';
    }
  }

  while (std::getline(std::cin, line)) {
    text += line;
    text += '\n';
  }

  modc::tokens::Parser parser(modc::astParser::keywordSet());
  auto tokenStatements = parser.parse(text);

  // TODO:  Make line width configurable.
  modc::CodePrinter printer(std::cout, 100);

  for (auto& tokenStatement: tokenStatements) {
    if (executable) {
      printer << modc::astParser::parseImperative(tokenStatement);
    } else {
      printer << modc::astParser::parseDeclarative(tokenStatement);
    }
  }

  return 0;
}
