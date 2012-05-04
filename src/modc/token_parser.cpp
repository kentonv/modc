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
#include "errors.h"

int main(int argc, char* argv[]) {
  std::string text, line;

  while (std::getline(std::cin, line)) {
    text += line;
    text += '\n';
  }

  modc::tokens::Parser parser({
    "if", "else", "+", "+=", "=", "<", "<<", "<<<", "<<=", "<<<=", "=="});
  auto statements = parser.parse(text);

  for (auto& statement: statements) {
    for (auto& error: statement.getErrors()) {
      std::cerr << "ERROR: " << error << std::endl;
    }
  }

  for (auto& statement : statements) {
    std::cout << statement;
  }

  return 0;
}
