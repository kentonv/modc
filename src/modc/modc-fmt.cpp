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
#include <string.h>
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

const char* htmlHeader =
    "<html>\n"
    "<head>\n"
    "  <style type='text/css'>\n"
    "    .error { color: #c00; }\n"
    "    .code { font-family: monospace; }\n"
    "    .comment { color: #444; font-style: italic; }\n"
    "    .keyword { font-weight: bold; color: #008; }\n"
    "    .keysymbol { font-weight: bold; }\n"
    "    .bracket { font-weight: bold; }\n"
    "    .delimiter { font-weight: bold; }\n"
    "    .operator { font-weight: bold; }\n"
    "    .literal { color: #00f; }\n"
    "  </style>\n"
    "</head>\n"
    "\n"
    "<body>\n"
    "<div class='code'>\n";

const char* htmlFooter =
    "</div>\n"
    "</body>\n"
    "</html>\n";

int main(int argc, char* argv[]) {
  bool html = false;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];

    if (arg == "--html") {
      html = true;
    } else {
      cerr << "Unknown arg: " << arg;
      return 1;
    }
  }

  if (html) {
    cout << htmlHeader;
  }

  std::string text, line;

  bool executable = false;

  if (std::getline(std::cin, line)) {
    if (line.substr(0, 3) == "#!/" || line.substr(0, 4) == "#! /") {
      executable = true;
      if (html) {
        cout << "<div class='statement'><span class='comment'>" << line << "</span></div>\n";
      } else {
        cout << line << '\n';
      }
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
  modc::FormattedCodeWriter::Metrics metrics;
  memset(&metrics, 0, sizeof(metrics));
  metrics.availableWidth = 100;
  metrics.spaceWidth = 1;
  metrics.indentWidth = 4;
  metrics.blockIndentWidth = 2;
  metrics.forceWrapFirstParameterThreshold = 16;

  ekam::OwnedPtr<modc::FormattedCodeWriter> writer;
  if (html) {
    writer = ekam::newOwned<modc::HtmlCodeWriter>(std::cout, metrics);
  } else {
    writer = ekam::newOwned<modc::TextCodeWriter>(std::cout, metrics);
  }

  modc::CodePrinter printer(*writer);

  for (auto& tokenStatement: tokenStatements) {
    if (executable) {
      printer << modc::astParser::parseImperative(tokenStatement);
    } else {
      printer << modc::astParser::parseDeclarative(tokenStatement);
    }
  }

  if (html) {
    cout << htmlFooter;
  }

  return 0;
}
