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

template <typename SubParser, Token::Type type, std::vector<TokenSequence> Token::*member,
          bool multiElement>
struct ListParser {
  ListParser(SubParser&& subParser): subParser(move(subParser)) {}

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
          errors.push_back(move(subResult.error));
        } else {
          subResults.push_back(move(subResult.value));
        }
      }

      input.next();

      if (errors.empty()) {
        if (!multiElement && subResults.size() != 1) {
          return parser::ParseResult<SubResults>(input.error("Must have exactly one element."));
        } else {
          return parser::ParseResult<SubResults>(move(subResults));
        }
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

template <typename SubParser>
ListParser<typename std::remove_reference<SubParser>::type, Token::Type::BRACKETED,
           &Token::bracketed, false>
bracketed(SubParser&& subParser) {
  return ListParser<typename std::remove_reference<SubParser>::type, Token::Type::BRACKETED,
      &Token::bracketed, false>(std::forward<SubParser>(subParser));
}

template <typename SubParser>
ListParser<typename std::remove_reference<SubParser>::type, Token::Type::BRACKETED,
           &Token::bracketed, true>
bracketedList(SubParser&& subParser) {
  return ListParser<typename std::remove_reference<SubParser>::type, Token::Type::BRACKETED,
      &Token::bracketed, true>(std::forward<SubParser>(subParser));
}

template <typename SubParser>
ListParser<typename std::remove_reference<SubParser>::type, Token::Type::PARENTHESIZED,
           &Token::parenthesized, false>
parenthesized(SubParser&& subParser) {
  return ListParser<typename std::remove_reference<SubParser>::type, Token::Type::PARENTHESIZED,
      &Token::parenthesized, false>(std::forward<SubParser>(subParser));
}

template <typename SubParser>
ListParser<typename std::remove_reference<SubParser>::type, Token::Type::PARENTHESIZED,
           &Token::parenthesized, true>
parenthesizedList(SubParser&& subParser) {
  return ListParser<typename std::remove_reference<SubParser>::type, Token::Type::PARENTHESIZED,
      &Token::parenthesized, true>(std::forward<SubParser>(subParser));
}

parser::ExactElementParser<TokenParserInput> keyword(string&& name) {
  return parser::exactElement<TokenParserInput>(tokens::keyword(move(name)));
}

auto identifier = [](TokenParserInput& input) -> parser::ParseResult<string> {
  if (input.atEnd() || input.current().getType() != Token::Type::IDENTIFIER) {
    input.setBroken(true);
    return parser::ParseResult<string>(input.error("Expected identifier."));
  } else {
    const string& result = input.current().identifier;
    input.next();
    return parser::ParseResult<string>(string(result));
  }
};


auto outerExpression = [](TokenParserInput& input) -> parser::ParseResult<Expression> {
  return parser::ParseResult<Expression>(errors::error(-1, -1, "TODO"));
};

using parser::alternative;
using parser::sequence;
using parser::transform;
using parser::repeated;
using parser::ref;

auto atomicExpression = alternative(
    transform<Expression>(ref(identifier),
        [](string&& name) { return variable(move(name)); }),
    parenthesized(ref(outerExpression)),
    transform<Expression>(bracketedList(ref(outerExpression)),
        [](std::vector<Expression>&& expressions) { return literalArray(move(expressions)); })
    // TODO: literals, import
    );






class TokenReader {
public:
  TokenReader(const TokenSequence& sequnece) : handledAtEnd(false) {}
  ~TokenReader() {
    if (!handledAtEnd) {
      DEBUG_ERROR << "Failed to check if all tokens were consumed on TokenReader.";
    }
  }

  Token::Type nextTokenType();
  bool atEnd();

  bool lookingAt(Token::Type type);
  const string& nextKeyword();

  bool tryReadKeyword(const string& keyword);
  const string& readIdentifier();
  int readLiteralInt();
  double readLiteralDouble();
  string readLiteralString();

  const std::vector<TokenSequence>& readParenthesized();
  const std::vector<TokenSequence>& readBraketed();

  const std::vector<errors::Error>& readError();

  template <typename... Parts>
  errors::Error error(Parts&&... message) {
    // Location?
    return errors::error(-1, -1, std::forward<Parts>(message)...);
  }

  template <typename... Parts>
  Expression errorExpression(Parts&&... message) {
    return ast::errorExpression(error(message...));
  }

  Expression errorIfNotConsumed(Expression&& parsed) {
    handledAtEnd = true;
    if (atEnd() || parsed.getType() == Expression::Type::ERROR) {
      return move(parsed);
    } else {
      // TODO:  Span all remaining tokens.
      return errorExpression("I do not understand these tokens.");
    }
  }

private:
  bool handledAtEnd;
};

Expression parseExpression(TokenReader& reader);

Expression parseAtomicExpression(TokenReader& reader) {
  switch (reader.nextTokenType()) {
    case Token::Type::ERROR:
      return errorExpression(reader.readError());
    case Token::Type::KEYWORD:
      if (reader.tryReadKeyword("import")) {
        return import(reader.readLiteralString());
      } else {
        return errorExpression(reader.error("Expected expression."));
      }
    case Token::Type::IDENTIFIER:
      return variable(reader.readIdentifier());
    case Token::Type::BRACKETED: {
      std::vector<Expression> elementExpressions;
      for (const TokenSequence& sequence: reader.readParenthesized()) {
        TokenReader subReader(sequence);
        elementExpressions.push_back(subReader.errorIfNotConsumed(parseExpression(subReader)));
      }
      return literalArray(move(elementExpressions));
    }
    case Token::Type::PARENTHESIZED: {
      const std::vector<TokenSequence>& parts = reader.readParenthesized();
      if (parts.size() != 1) {
        return reader.errorExpression(
            "Parenthesized sub-expression must have exactly one element (no commas).");
      } else {
        TokenReader subReader(parts[0]);
        return subReader.errorIfNotConsumed(parseExpression(subReader));
      }
    }
    case Token::Type::LITERAL_INT:
      return literalInt(reader.readLiteralInt());
    case Token::Type::LITERAL_DOUBLE:
      return literalDouble(reader.readLiteralDouble());
    case Token::Type::LITERAL_STRING:
      return literalString(reader.readLiteralString());
    default:
      return reader.errorExpression("Expected expression.");
  }
}

Expression parseSuffixedExpression(TokenReader& reader) {
  Expression seed = parseAtomicExpression(reader);

  while (!reader.atEnd()) {
    switch (reader.nextTokenType()) {
      case Token::Type::KEYWORD:
        if (reader.tryReadKeyword(".")) {
          seed = memberAccess(move(seed), reader.readIdentifier());
        } else {
          return seed;
        }
        break;
      case Token::Type::BRACKETED: {
        Expression key;
        const std::vector<TokenSequence>& parts = reader.readBraketed();
        if (parts.size() != 1) {
          key = reader.errorExpression("Subscript must have exactly one element (no commas).");
        } else {
          TokenReader subReader(parts[0]);
          key = subReader.errorIfNotConsumed(parseExpression(subReader));
        }

        seed = subscript(move(seed), move(key));
        break;
      }
      case Token::Type::PARENTHESIZED: {
        std::vector<Expression::FunctionCall::Parameter> parameters;
        for (const TokenSequence& sequence: reader.readParenthesized()) {
          Expression::FunctionCall::Parameter parameter;
          TokenReader subReader(sequence);
          Expression expression = parseExpression(subReader);

          if (subReader.tryReadKeyword("&")) {
            parameter.styleAllowance = StyleAllowance::MUTABLE_REFERENCE;
          } else {
            parameter.styleAllowance = StyleAllowance::VALUE;
          }

          parameter.expression = subReader.errorIfNotConsumed(move(expression));

          parameters.push_back(parameter);
        }

        break;
      }
      default:
        return seed;
    }
  }

  return seed;
}

Maybe<int> getOperatorPriority(const string& keyword);

Expression parseArithmeticExpression(TokenReader& reader, int priority) {
  Expression left = parseSuffixedExpression(reader);

  while (reader.lookingAt(Token::Type::KEYWORD)) {
    Maybe<int> opPriority = getOperatorPriority(reader.nextKeyword());
    if (opPriority && *opPriority > priority) {
      string keyword = reader.nextKeyword();
      reader.tryReadKeyword(keyword);
      Expression right = parseArithmeticExpression(reader, opPriority);
      left = binaryOperator(move(keyword), move(left), move(right));
    } else {
      break;
    }
  }

  return left;
}

Expression parseExpression(TokenReader& reader) {
  Expression seed = parseArithmeticExpression(reader, 0);

  if (reader.tryReadKeyword("?")) {
    Expression trueClause = parseExpression(reader);
    if (!reader.tryReadKeyword(":")) {
      return reader.errorExpression("Expected ':'.");
    }
    Expression falseClause = parseExpression(reader);
    seed = conditional(move(seed), move(trueClause), move(falseClause));
  }

  return seed;
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
  HANDLE(UNARY_OPERATOR, unaryOperator, UnaryOperator) \
  HANDLE(FUNCTION_CALL, functionCall, FunctionCall) \
  HANDLE(SUBSCRIPT, subscript, Subscript) \
  HANDLE(MEMBER_ACCESS, memberAccess, MemberAccess) \
  HANDLE(CONDITIONAL, conditional, Conditional) \
  HANDLE(LAMBDA, lambda, Lambda) \
  HANDLE(IMPORT, import, string)

Expression::Expression(Expression&& other): type(other.type) {
  switch (type) {
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
