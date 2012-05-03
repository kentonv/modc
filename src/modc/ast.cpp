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

#include "ast.h"

#include <utility>
#include <functional>

#include "tokens.h"
#include "errors.h"
#include "parser.h"
#include "base/Debug.h"

namespace modc {
namespace ast {

using std::move;
using tokens::Token;
using tokens::TokenSequence;



struct TokenParserInput : public parser::IteratorInput<Token, std::vector<Token>::const_iterator> {
  explicit TokenParserInput(const TokenSequence& sequence)
      : parser::IteratorInput<Token, std::vector<Token>::const_iterator>(
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
    return parser::ParseResult<parser::Void>(std::vector<errors::Error>());
  } else {
    std::vector<errors::Error> result = input.current().error;
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

template <Token::Type type, std::vector<TokenSequence> Token::*member>
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

template <Token::Type type, std::vector<TokenSequence> Token::*member>
struct ListParserTemplate {
  template <typename SubParser>
  struct Parser {
    Parser(SubParser&& subParser): subParser(move(subParser)) {}

    typedef std::vector<typename parser::ExtractParserType<SubParser>::OutputType> SubResults;

    parser::ParseResult<SubResults>
    operator()(TokenParserInput& input) const {
      if (!input.atEnd() && input.current().getType() == type) {
        SubResults subResults;
        std::vector<errors::Error> errors;

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

parser::WrapperParserConstructor<BracketedParserTemplate<
    Token::Type::BRACKETED, &Token::bracketed>::Parser> bracketed;
parser::WrapperParserConstructor<ListParserTemplate<
    Token::Type::BRACKETED, &Token::bracketed>::Parser> bracketedList;
parser::WrapperParserConstructor<BracketedParserTemplate<
    Token::Type::PARENTHESIZED, &Token::bracketed>::Parser> parenthesized;
parser::WrapperParserConstructor<ListParserTemplate<
    Token::Type::PARENTHESIZED, &Token::bracketed>::Parser> parenthesizedList;

// =======================================================================================

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
Binding<typename std::remove_reference<Func>::type,
        typename std::remove_reference<Param>::type>
bind(Param&& param, Func&& func) {
  return Binding<typename std::remove_reference<Func>::type,
                 typename std::remove_reference<Param>::type>(move(func), move(param));
}

// =======================================================================================

using parser::oneOf;
using parser::sequence;
using parser::transform;
using parser::repeated;
using parser::optional;
using parser::commit;

typedef expression ex;

typedef parser::Parser<TokenParserInput, Expression> ExpressionParser;

extern ExpressionParser outerExpression;

ExpressionParser atomicExpression = oneOf(
    // Identifier.
    transform(identifier,
        [](string&& name) { return ex::variable(move(name)); }),

    // Parenthesized expression or tuple.
    transform(parenthesizedList(outerExpression),
        [](std::vector<Expression>&& expressions) {
          if (expressions.size() == 1) {
            return expressions[0];
          } else {
            return ex::tuple(move(expressions));
          }
        }),

    // Array literal
    transform(bracketedList(outerExpression),
        [](std::vector<Expression>&& expressions) { return ex::literalArray(move(expressions)); }),

    // Import
    transform(sequence(keyword("import"), literalString),
        [](string&& moduleName) { return ex::import(move(moduleName)); }),

    // Primitive literals
    transform(literalInt, [](int value) { return ex::literalInt(value); }),
    transform(literalDouble, [](double value) { return ex::literalDouble(value); }),
    transform(literalString, [](string&& value) { return ex::literalString(value); }));

auto functionCallParameter = [](TokenParserInput& input) {
  return parser::ParseResult<Expression::FunctionCall::Parameter>(errors::error(-1, -1, "TODO"));
};

parser::Parser<TokenParserInput, std::function<Expression(Expression&&)>> suffix = oneOf(
    // Member access
    transform(sequence(keyword("."), identifier),
        [](string&& name) -> std::function<Expression(Expression&&)> {
          return bind(move(name), [](string&& name, Expression&& seed) {
            return ex::memberAccess(move(seed), move(name));
          });
        }),

    // Function call
    transform(parenthesizedList(functionCallParameter),
        [](std::vector<Expression::FunctionCall::Parameter>&& params) ->
            std::function<Expression(Expression&&)> {
          return bind(move(params),
            [](std::vector<Expression::FunctionCall::Parameter>&& params, Expression&& seed) {
              return ex::functionCall(move(seed), move(params));
            });
        }),

    // Subscript
    transform(bracketed(outerExpression),
        [](Expression&& key) -> std::function<Expression(Expression&&)> {
          return bind(move(key), [](Expression&& key, Expression&& seed) {
            return ex::subscript(move(seed), move(key));
          });
        }),

    // postincrement / postdecrement
    transform(oneOfKeywords({"++", "--"}),
        [](string&& op) -> std::function<Expression(Expression&&)> {
          return bind(move(op), [](string&& op, Expression&& seed) {
            return ex::postfixOperator(move(seed), move(op));
          });
        }));

ExpressionParser suffixedExpression = transform(
    sequence(atomicExpression, commit(), repeated(suffix)),
    [] (Expression&& seed, std::vector<std::function<Expression(Expression&&)>>&& suffixes) {
      for (auto& suffix : suffixes) {
        seed = move(suffix(move(seed)));
      }
      return move(seed);
    });

ExpressionParser prefixedExpression = oneOf(
    transform(sequence(oneOfKeywords({"++", "--", "+", "-", "!", "~"}),
                       commit(), suffixedExpression),
        [](string&& op, Expression&& seed) { return ex::prefixOperator(move(op), move(seed)); }),

    suffixedExpression);

ExpressionParser binaryOpParser(const ExpressionParser& next,
                                std::initializer_list<string> ops) {
  auto multiplicativeSuffix = transform(
      sequence(oneOfKeywords(ops), commit(), next),
      [](string&& op, Expression&& operand) {
        return std::make_pair(move(op), move(operand));
      });

  return transform(sequence(next, commit(), repeated(move(multiplicativeSuffix))),
      [](Expression&& seed, std::vector<std::pair<string, Expression>>&& ops) {
        for (auto& op : ops) {
          seed = ex::binaryOperator(move(op.first), move(seed), move(op.second));
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
        return ex::ternaryOperator(move(condition), move(clauses->first), move(clauses->second));
      } else {
        return move(condition);
      }
    });

void foo(TokenParserInput& input) {
  outerExpression(input);
}

// =============================================================================

template <typename T>
void Destroy(T& obj) {
  obj.~T();
}

#define FOR_ALL_EXPRESSIONS(HANDLE) \
  HANDLE(ERROR, error, std::vector<errors::Error>) \
  HANDLE(LITERAL_INT, literalInt, int) \
  HANDLE(LITERAL_DOUBLE, literalDouble, double) \
  HANDLE(LITERAL_STRING, literalString, string) \
  HANDLE(LITERAL_ARRAY, literalArray, std::vector<Expression>) \
  HANDLE(BINARY_OPERATOR, binaryOperator, BinaryOperator) \
  HANDLE(PREFIX_OPERATOR, prefixOperator, PrefixOperator) \
  HANDLE(POSTFIX_OPERATOR, postfixOperator, PostfixOperator) \
  HANDLE(TERNARY_OPERATOR, ternaryOperator, TernaryOperator) \
  HANDLE(FUNCTION_CALL, functionCall, FunctionCall) \
  HANDLE(SUBSCRIPT, subscript, Subscript) \
  HANDLE(MEMBER_ACCESS, memberAccess, MemberAccess) \
  HANDLE(CONDITIONAL, conditional, Conditional) \
  HANDLE(LAMBDA, lambda, Lambda) \
  HANDLE(IMPORT, import, string)

Expression::Expression(Expression&& other): type(other.type) {
  switch (type) {
    case Type::PLACEHOLDER:
      break;

#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
    FOR_ALL_EXPRESSIONS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT
  }
}

Expression::Expression(const Expression& other): type(other.type) {
  switch (type) {
    case Type::PLACEHOLDER:
      break;

#define COPY_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(other.NAME); \
      break;
    FOR_ALL_EXPRESSIONS(COPY_CONSTRUCT)
#undef COPY_CONSTRUCT
  }
}

Expression::~Expression() {
  switch (type) {
    case Type::PLACEHOLDER:
      break;

#define DESTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      Destroy(NAME); \
      break;
    FOR_ALL_EXPRESSIONS(DESTRUCT)
#undef DESTRUCT
  }
}

Expression& Expression::operator=(Expression&& other) {
  // Lazy.
  this->~Expression();
  new(this) Expression(move(other));
  return *this;
}

Expression& Expression::operator=(const Expression& other) {
  // Lazy.
  this->~Expression();
  new(this) Expression(other);
  return *this;
}

bool Expression::operator==(const Expression& other) const {
  if (type == other.type) {
    switch (type) {
      case Type::PLACEHOLDER:
        return true;

#define COMPARE(ID, NAME, TYPE) \
      case Type::ID: \
        return NAME == other.NAME;
      FOR_ALL_EXPRESSIONS(COMPARE)
#undef MOVE_CONSTRUCT
    }
  }

  return false;
}

// ---------------------------------------------------------------------------------------

#define FOR_ALL_STATEMENTS(HANDLE) \
  HANDLE(ERROR, error, std::vector<errors::Error>) \
  HANDLE(EXPRESSION, expression, Expression) \
  HANDLE(RETURN, return_, Expression) \
  HANDLE(BREAK, break_, string)

Statement::Statement(Statement&& other): type(other.type) {
  switch (type) {
#define MOVE_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(move(other.NAME)); \
      break;
      FOR_ALL_STATEMENTS(MOVE_CONSTRUCT)
#undef MOVE_CONSTRUCT

    default:  // temporary
      break;
  }
}

Statement::Statement(const Statement& other): type(other.type) {
  switch (type) {
#define COPY_CONSTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      new (&NAME) TYPE(other.NAME); \
      break;
      FOR_ALL_STATEMENTS(COPY_CONSTRUCT)
#undef COPY_CONSTRUCT

    default:  // temporary
      break;
  }
}

Statement::~Statement() {
  switch (type) {
#define DESTRUCT(ID, NAME, TYPE) \
    case Type::ID: \
      Destroy(NAME); \
      break;
      FOR_ALL_STATEMENTS(DESTRUCT)
#undef DESTRUCT

    default:  // temporary
      break;
  }
}

Statement& Statement::operator=(Statement&& other) {
  // Lazy.
  this->~Statement();
  new(this) Statement(move(other));
  return *this;
}

Statement& Statement::operator=(const Statement& other) {
  // Lazy.
  this->~Statement();
  new(this) Statement(other);
  return *this;
}

bool Statement::operator==(const Statement& other) const {
  if (type == other.type) {
    switch (type) {
#define COMPARE(ID, NAME, TYPE) \
      case Type::ID: \
        return NAME == other.NAME;
        FOR_ALL_STATEMENTS(COMPARE)
#undef MOVE_CONSTRUCT

      default:  // temporary
        break;
    }
  }

  return false;
}

}  // namespace ast
}  // namespace modc
