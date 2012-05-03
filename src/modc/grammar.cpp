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

#include "grammar.h"

#include <utility>
#include <functional>

#include "expressions.h"
#include "statements.h"
#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"

namespace modc {
namespace grammar {

using std::move;
using std::vector;
using std::string;
using expressions::StyleAllowance;
using expressions::Expression;
using statements::Statement;

using tokens::Token;
using tokens::TokenSequence;
using tokens::TokenStatement;

using parser::oneOf;
using parser::sequence;
using parser::transform;
using parser::repeated;
using parser::optional;
using parser::commit;

// =======================================================================================
// Parsers for simple tokens

struct TokenParserInput : public parser::IteratorInput<Token, vector<Token>::const_iterator> {
  explicit TokenParserInput(const TokenSequence& sequence)
      : parser::IteratorInput<Token, vector<Token>::const_iterator>(
          sequence.tokens.begin(), sequence.tokens.end()) {}
};

parser::ExactElementParser<TokenParserInput> keyword(string&& name) {
  return parser::exactElement<TokenParserInput>(tokens::keyword(move(name)));
}

class OneOfKeywordsParser {
public:
  OneOfKeywordsParser(std::initializer_list<string> names)
      : keywords(names) {}

  parser::ParseResult<string> operator()(TokenParserInput& input) const {
    if (input.atEnd() || input.current().getType() != Token::Type::KEYWORD ||
        keywords.count(input.current().keyword) == 0) {
      input.setBroken(true);
      return parser::ParseResult<string>(input.error("TODO"));
    } else {
      parser::ParseResult<string> result(input.current().keyword);
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

auto errorTokens = [](TokenParserInput& input) -> parser::ParseResult<parser::Void> {
  if (input.atEnd() || input.current().getType() != Token::Type::ERROR) {
    input.setBroken(true);
    return parser::ParseResult<parser::Void>(vector<errors::Error>());
  } else {
    vector<errors::Error> result = input.current().error;
    input.next();
    while (!input.atEnd() && input.current().getType() == Token::Type::ERROR) {
      result.insert(result.end(), input.current().error.begin(), input.current().error.end());
      input.next();
    }
    return parser::ParseResult<parser::Void>(move(result));
  }
};

auto identifier = [](TokenParserInput& input) -> parser::ParseResult<string> {
  if (input.atEnd() || input.current().getType() != Token::Type::IDENTIFIER) {
    input.setBroken(true);
    return parser::ParseResult<string>(input.error("Expected identifier."));
  } else {
    string result = input.current().identifier;
    input.next();
    return parser::ParseResult<string>(move(result));
  }
};

auto literalInt = [](TokenParserInput& input) -> parser::ParseResult<int> {
  if (input.atEnd() || input.current().getType() != Token::Type::LITERAL_INT) {
    input.setBroken(true);
    return parser::ParseResult<int>(input.error("Expected integer."));
  } else {
    int result = input.current().literalInt;
    input.next();
    return parser::ParseResult<int>(result);
  }
};

auto literalDouble = [](TokenParserInput& input) -> parser::ParseResult<double> {
  if (input.atEnd() || input.current().getType() != Token::Type::LITERAL_DOUBLE) {
    input.setBroken(true);
    return parser::ParseResult<double>(input.error("Expected double."));
  } else {
    double result = input.current().literalDouble;
    input.next();
    return parser::ParseResult<double>(result);
  }
};

auto literalString = [](TokenParserInput& input) -> parser::ParseResult<string> {
  if (input.atEnd() || input.current().getType() != Token::Type::LITERAL_STRING) {
    input.setBroken(true);
    return parser::ParseResult<string>(input.error("Expected string."));
  } else {
    string result = input.current().literalString;
    input.next();
    return parser::ParseResult<string>(move(result));
  }
};

template <Token::Type type, vector<TokenSequence> Token::*member>
struct BracketedParserTemplate {
  template <typename SubParser>
  struct Parser {
    Parser(SubParser&& subParser): subParser(move(subParser)) {}

    typedef typename parser::ExtractParserType<SubParser>::OutputType SubResult;

    parser::ParseResult<SubResult>
    operator()(TokenParserInput& input) const {
      if (!input.atEnd() && input.current().getType() == type) {
        if ((input.current().*member).size() == 1) {
          TokenParserInput subInput((input.current().*member)[0]);
          input.next();
          return subParser(subInput);
        } else {
          auto result = parser::ParseResult<SubResult>(
              input.error("Must have exactly one element."));
          input.next();
          return move(result);
        }
      } else {
        input.setBroken(true);
        return parser::ParseResult<SubResult>(input.error("Expected list."));
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

    typedef vector<typename parser::ExtractParserType<SubParser>::OutputType> SubResults;

    parser::ParseResult<SubResults>
    operator()(TokenParserInput& input) const {
      if (!input.atEnd() && input.current().getType() == type) {
        SubResults subResults;
        vector<errors::Error> errors;

        for (const TokenSequence& element: input.current().*member) {
          TokenParserInput subInput(element);
          auto subResult = subParser(subInput);
          if (subResult.isError()) {
            for (auto& error : subResult.errors) {
              errors.push_back(move(error));
            }
          } else {
            subResults.push_back(move(subResult.value));
          }
        }

        input.next();

        if (errors.empty()) {
          return parser::ParseResult<SubResults>(move(subResults));
        } else {
          return parser::ParseResult<SubResults>(move(errors));
        }
      } else {
        input.setBroken(true);
        return parser::ParseResult<SubResults>(input.error("Expected list."));
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
// A generic helper.  TODO:  Move elsewhere?

template <typename Func, typename Param>
struct Binding {
  Func func;
  Param param;

  Binding(Func&& func, Param&& param): func(move(func)), param(move(param)) {}

  template <typename... MoreParams>
  auto operator()(MoreParams&&... moreParams) ->
      decltype(func(move(param), std::forward<MoreParams>(moreParams)...)) {
    return func(move(param), std::forward<MoreParams>(moreParams)...);
  }
};

template <typename Func, typename Param>
Binding<decay(Func), decay(Param)>
bind(Param&& param, Func&& func) {
  return Binding<decay(Func), decay(Param)>(move(func), move(param));
}

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
          return bind(move(name), [](string&& name, Expression&& seed) {
            return Expression::fromMemberAccess(move(seed), move(name));
          });
        }),

    // Function call
    transform(parenthesizedList(functionCallParameter),
        [](vector<Expression::FunctionCall::Parameter>&& params) ->
            std::function<Expression(Expression&&)> {
          return bind(move(params),
            [](vector<Expression::FunctionCall::Parameter>&& params, Expression&& seed) {
              return Expression::fromFunctionCall(move(seed), move(params));
            });
        }),

    // Subscript
    transform(bracketed(outerExpression),
        [](Expression&& key) -> std::function<Expression(Expression&&)> {
          return bind(move(key), [](Expression&& key, Expression&& seed) {
            return Expression::fromSubscript(move(seed), move(key));
          });
        }),

    // postincrement / postdecrement
    transform(oneOfKeywords({"++", "--"}),
        [](string&& op) -> std::function<Expression(Expression&&)> {
          return bind(move(op), [](string&& op, Expression&& seed) {
            return Expression::fromPostfixOperator(move(seed), move(op));
          });
        }));

ExpressionParser suffixedExpression = transform(
    sequence(atomicExpression, commit(), repeated(suffix)),
    [] (Expression&& seed, vector<std::function<Expression(Expression&&)>>&& suffixes) {
      for (auto& suffix : suffixes) {
        seed = move(suffix(move(seed)));
      }
      return move(seed);
    });

ExpressionParser prefixedExpression =
    transform(sequence(repeated(oneOfKeywords({"++", "--", "+", "-", "!", "~"})),
                       commit(), suffixedExpression),
        [](vector<string>&& ops, Expression&& seed) {
          for (string& op : ops) {
            seed = Expression::fromPrefixOperator(move(op), move(seed));
          }
          return move(seed);
        });

ExpressionParser binaryOpParser(const ExpressionParser& next,
                                std::initializer_list<string> ops) {
  auto multiplicativeSuffix = transform(
      sequence(oneOfKeywords(ops), commit(), next),
      [](string&& op, Expression&& operand) {
        return std::make_pair(move(op), move(operand));
      });

  return transform(sequence(next, commit(), repeated(move(multiplicativeSuffix))),
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
    sequence(keyword("?"), commit(), outerExpression, keyword(":"), outerExpression),
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

}  // namespace grammar
}  // namespace modc
