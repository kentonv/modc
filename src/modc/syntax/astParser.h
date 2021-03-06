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

#ifndef KENTONSCODE_MODC_ASTPARSER_H_
#define KENTONSCODE_MODC_ASTPARSER_H_

#include <vector>
#include <set>
#include <string>

namespace modc {
  namespace tokens { class TokenStatement; }
  namespace ast {
    class Expression;
    class Statement;
  }
}

namespace modc {
namespace astParser {

ast::Statement parseImperative(const tokens::TokenStatement& input);
ast::Statement parseDeclarative(const tokens::TokenStatement& input);

// All keywords used by the base parser.
const std::set<std::string>& keywordSet();

}  // namespace astParser
}  // namespace modc

#endif /* KENTONSCODE_MODC_ASTPARSER_H_ */
