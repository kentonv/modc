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

#include "astParser.h"

#include <utility>
#include <functional>

#include "ast.h"
#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"

namespace modc {
namespace astParser {

using std::move;
using std::vector;
using std::string;
using ast::StyleAllowance;
using ast::Expression;
using ast::Statement;

using tokens::Token;
using tokens::TokenSequence;
using tokens::TokenStatement;

using parser::oneOf;
using parser::sequence;
using parser::transform;
using parser::repeated;
using parser::optional;

using namespace std::placeholders;

// =======================================================================================
// Parsers for simple tokens

struct TokenParserInput : public parser::IteratorInput<Token, vector<Token>::const_iterator> {
  explicit TokenParserInput(const TokenSequence& sequence)
      : parser::IteratorInput<Token, vector<Token>::const_iterator>(
          sequence.tokens.begin(), sequence.tokens.end()) {}
};

template <typename Parser>
typename parser::ExtractParserType<Parser>::OutputType
ParseTokenSequence(const TokenSequence& sequence, const Parser& subParser) {
  TokenParserInput input(sequence);
  typedef typename parser::ExtractParserType<Parser>::OutputType Result;
  Maybe<Result> parseResult = subParser(input);

  if (parseResult && input.atEnd()) {
    return *parseResult;
  } else {
    return Result::fromError(sequence.getErrors());
  }
}

parser::ExactElementParser<TokenParserInput> keyword(string&& name) {
  return parser::exactElement<TokenParserInput>(tokens::keyword(move(name)));
}

class OneOfKeywordsParser {
public:
  OneOfKeywordsParser(std::initializer_list<string> names)
      : keywords(names) {}

  Maybe<string> operator()(TokenParserInput& input) const {
    if (input.atEnd() || input.current().getType() != Token::Type::KEYWORD ||
        keywords.count(input.current().keyword) == 0) {
      return nullptr;
    } else {
      string result = input.current().keyword;
      input.next();
      return move(result);
    }
  }

private:
  std::set<string> keywords;
};

OneOfKeywordsParser oneOfKeywords(std::initializer_list<string> names) {
  return OneOfKeywordsParser(names);
}

auto identifier = [](TokenParserInput& input) -> Maybe<string> {
  if (input.atEnd() || input.current().getType() != Token::Type::IDENTIFIER) {
    return nullptr;
  } else {
    string result = input.current().identifier;
    input.next();
    return move(result);
  }
};

auto literalInt = [](TokenParserInput& input) -> Maybe<int> {
  if (input.atEnd() || input.current().getType() != Token::Type::LITERAL_INT) {
    return nullptr;
  } else {
    int result = input.current().literalInt;
    input.next();
    return result;
  }
};

auto literalDouble = [](TokenParserInput& input) -> Maybe<double> {
  if (input.atEnd() || input.current().getType() != Token::Type::LITERAL_DOUBLE) {
    return nullptr;
  } else {
    double result = input.current().literalDouble;
    input.next();
    return result;
  }
};

auto literalString = [](TokenParserInput& input) -> Maybe<string> {
  if (input.atEnd() || input.current().getType() != Token::Type::LITERAL_STRING) {
    return nullptr;
  } else {
    string result = input.current().literalString;
    input.next();
    return move(result);
  }
};

template <Token::Type type, vector<TokenSequence> Token::*member>
struct BracketedParserTemplate {
  template <typename SubParser>
  struct Parser {
    Parser(SubParser&& subParser): subParser(move(subParser)) {}

    typedef typename parser::ExtractParserType<SubParser>::OutputType SubResult;

    Maybe<SubResult> operator()(TokenParserInput& input) const {
      if (!input.atEnd() && input.current().getType() == type) {
        if ((input.current().*member).size() == 1) {
          return ParseTokenSequence((input.consume().*member)[0], subParser);
        } else {
          vector<errors::Error> errors;
          errors.push_back(errors::error(input.current().startOffset, input.current().endOffset,
                           "Must have exactly one element."));
          input.consume().getErrors(errors);
          return SubResult::fromError(move(errors));
        }
      } else {
        return nullptr;
      }
    }

    SubParser subParser;
  };
};

template <Token::Type type, vector<TokenSequence> Token::*member>
struct ListParserTemplate {
  template <typename SubParser>
  struct Parser {
    Parser(SubParser&& subParser): subParser(move(subParser)) {}

    typedef typename parser::ExtractParserType<SubParser>::OutputType SubResult;

    Maybe<vector<SubResult>> operator()(TokenParserInput& input) const {
      if (!input.atEnd() && input.current().getType() == type) {
        vector<SubResult> subResults;

        for (const TokenSequence& element: input.current().*member) {
          subResults.push_back(ParseTokenSequence(element, subParser));
        }

        input.next();

        return move(subResults);
      } else {
        return nullptr;
      }
    }

    SubParser subParser;
  };
};

// These are actually declaring functions, where the parameter is another parser.
parser::WrapperParserConstructor<BracketedParserTemplate<
    Token::Type::BRACKETED, &Token::bracketed>::Parser> bracketed;
parser::WrapperParserConstructor<ListParserTemplate<
    Token::Type::BRACKETED, &Token::bracketed>::Parser> bracketedList;
parser::WrapperParserConstructor<BracketedParserTemplate<
    Token::Type::PARENTHESIZED, &Token::bracketed>::Parser> parenthesized;
parser::WrapperParserConstructor<ListParserTemplate<
    Token::Type::PARENTHESIZED, &Token::bracketed>::Parser> parenthesizedList;

// =======================================================================================
// Expression parser!

typedef parser::Parser<TokenParserInput, Expression> ExpressionParser;

extern ExpressionParser outerExpression;

ExpressionParser atomicExpression = oneOf(
    // Identifier.
    transform(identifier,
        [](string&& name) { return Expression::fromVariable(move(name)); }),

    // Parenthesized expression or tuple.
    transform(parenthesizedList(outerExpression),
        [](vector<Expression>&& expressions) {
          if (expressions.size() == 1) {
            return expressions[0];
          } else {
            return Expression::fromTuple(move(expressions));
          }
        }),

    // Array literal
    transform(bracketedList(outerExpression),
        [](vector<Expression>&& expressions) {
          return Expression::fromLiteralArray(move(expressions));
        }),

    // Import
    transform(sequence(keyword("import"), literalString),
        [](string&& moduleName) { return Expression::fromImport(move(moduleName)); }),

    // Primitive literals
    transform(literalInt, [](int value) { return Expression::fromLiteralInt(value); }),
    transform(literalDouble, [](double value) { return Expression::fromLiteralDouble(value); }),
    transform(literalString,
        [](string&& value) { return Expression::fromLiteralString(move(value)); }));

parser::Parser<TokenParserInput, Expression::FunctionCall::Parameter> functionCallParameter =
    transform(sequence(optional(oneOfKeywords({"@", "&", "<-"})), outerExpression),
        [](Maybe<string>&& prefix, Expression&& exp) {
          StyleAllowance style = StyleAllowance::VALUE;
          if (prefix) {
            if (*prefix == "@") {
              style = StyleAllowance::IMMUTABLE_REFERENCE;
            } else if (*prefix == "&") {
              style = StyleAllowance::MUTABLE_REFERENCE;
            } else if (*prefix == "<-") {
              style = StyleAllowance::MOVE;
            }
          }

          return Expression::FunctionCall::Parameter(style, move(exp));
        });

parser::Parser<TokenParserInput, std::function<Expression(Expression&&)>> suffix = oneOf(
    // Member access
    transform(sequence(keyword("."), identifier),
        [](string&& name) -> std::function<Expression(Expression&&)> {
          return std::bind([](string& name, Expression&& seed) {
            return Expression::fromMemberAccess(move(seed), move(name));
          }, move(name), _1);
        }),

    // Function call
    transform(parenthesizedList(functionCallParameter),
        [](vector<Expression::FunctionCall::Parameter>&& params) ->
            std::function<Expression(Expression&&)> {
          return std::bind(
            [](vector<Expression::FunctionCall::Parameter>& params, Expression&& seed) {
              return Expression::fromFunctionCall(move(seed), move(params));
            }, move(params), _1);
        }),

    // Subscript
    transform(bracketed(outerExpression),
        [](Expression&& key) -> std::function<Expression(Expression&&)> {
          return std::bind([](Expression& key, Expression&& seed) {
            return Expression::fromSubscript(move(seed), move(key));
          }, move(key), _1);
        }),

    // postincrement / postdecrement
    transform(oneOfKeywords({"++", "--"}),
        [](string&& op) -> std::function<Expression(Expression&&)> {
          return std::bind([](string& op, Expression&& seed) {
            return Expression::fromPostfixOperator(move(seed), move(op));
          }, move(op), _1);
        }));

ExpressionParser suffixedExpression = transform(
    sequence(atomicExpression, repeated(suffix)),
    [] (Expression&& seed, vector<std::function<Expression(Expression&&)>>&& suffixes) {
      for (auto& suffix : suffixes) {
        seed = move(suffix(move(seed)));
      }
      return move(seed);
    });

ExpressionParser prefixedExpression =
    transform(sequence(repeated(oneOfKeywords({"++", "--", "+", "-", "!", "~"})),
                       suffixedExpression),
        [](vector<string>&& ops, Expression&& seed) {
          for (string& op : ops) {
            seed = Expression::fromPrefixOperator(move(op), move(seed));
          }
          return move(seed);
        });

ExpressionParser binaryOpParser(const ExpressionParser& next,
                                std::initializer_list<string> ops) {
  auto multiplicativeSuffix = transform(
      sequence(oneOfKeywords(ops), next),
      [](string&& op, Expression&& operand) {
        return std::make_pair(move(op), move(operand));
      });

  return transform(sequence(next, repeated(move(multiplicativeSuffix))),
      [](Expression&& seed, vector<std::pair<string, Expression>>&& ops) {
        for (auto& op : ops) {
          seed = Expression::fromBinaryOperator(move(op.first), move(seed), move(op.second));
        }
        return move(seed);
      });
}

ExpressionParser multiplyExpression = binaryOpParser(prefixedExpression, {"*", "/", "%"});
ExpressionParser addExpression      = binaryOpParser(multiplyExpression, {"+", "-"});
ExpressionParser shiftExpression    = binaryOpParser(addExpression     , {"<<", ">>"});
ExpressionParser bitandExpression   = binaryOpParser(shiftExpression   , {"&"});
ExpressionParser bitxorExpression   = binaryOpParser(bitandExpression  , {"^"});
ExpressionParser bitorExpression    = binaryOpParser(bitxorExpression  , {"|"});
ExpressionParser andExpression      = binaryOpParser(bitorExpression   , {"&&"});
ExpressionParser orExpression       = binaryOpParser(andExpression     , {"||"});

auto ternarySuffix = transform(
    sequence(keyword("?"), outerExpression, keyword(":"), outerExpression),
    [](Expression&& trueClause, Expression&& falseClause) {
      return std::make_pair(move(trueClause), move(falseClause));
    });

ExpressionParser outerExpression = transform(
    sequence(orExpression, optional(move(ternarySuffix))),
    [](Expression&& condition, Maybe<std::pair<Expression, Expression>>&& clauses) {
      if (clauses) {
        return Expression::fromTernaryOperator(
            move(condition), move(clauses->first), move(clauses->second));
      } else {
        return move(condition);
      }
    });

// =======================================================================================
// Statement parser!

// =======================================================================================

vector<Statement> parse(const vector<TokenStatement>& statements) {
  return vector<Statement>();
}

}  // namespace astParser
}  // namespace modc
