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
#include <set>
#include <map>

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

using ast::Style;
using ast::StyleAllowance;
using ast::Expression;
using ast::Declaration;
using ast::ParameterDeclaration;
using ast::Statement;
using ast::Annotation;

using tokens::Token;
using tokens::TokenSequence;
using tokens::TokenStatement;

using parser::oneOf;
using parser::sequence;
using parser::transform;
using parser::acceptIf;
using parser::repeated;
using parser::oneOrMore;
using parser::optional;

using namespace std::placeholders;

struct TokenParserInput : public parser::IteratorInput<Token, vector<Token>::const_iterator> {
  explicit TokenParserInput(const TokenSequence& sequence)
      : parser::IteratorInput<Token, vector<Token>::const_iterator>(
          sequence.tokens.begin(), sequence.tokens.end()) {}
};

typedef parser::Parser<TokenParserInput, Expression> ExpressionParser;
extern const ExpressionParser generalExpression;
typedef parser::Parser<TokenParserInput, Declaration> DeclarationParser;
extern const DeclarationParser generalDeclaration;
typedef parser::Parser<TokenParserInput, ParameterDeclaration> ParameterDeclarationParser;
extern const ParameterDeclarationParser parameterDeclaration;
typedef parser::Parser<TokenParserInput, Statement> StatementParser;
extern const StatementParser generalStatement;

// =======================================================================================
// Parsers for simple tokens

template <typename Parser>
typename parser::ExtractParserType<Parser>::OutputType
parseTokenSequence(const TokenSequence& sequence, const Parser& subParser) {
  TokenParserInput input(sequence);
  typedef typename parser::ExtractParserType<Parser>::OutputType Result;
  Maybe<Result> parseResult = subParser(input);

  if (parseResult && input.atEnd()) {
    return *parseResult;
  } else {
    vector<errors::Error> errors;

    vector<Token>::const_iterator best = input.getBest();
    if (best < sequence.tokens.end()) {
      errors.push_back(errors::error(best->startOffset, sequence.endOffset, "Syntax error."));
    } else {
      errors.push_back(errors::error(sequence.endOffset, sequence.endOffset, "Syntax error."));
    }
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
      return input.consume().keyword;
    }
  }

private:
  std::set<string> keywords;
};

OneOfKeywordsParser oneOfKeywords(std::initializer_list<string> names) {
  return OneOfKeywordsParser(names);
}

template <typename T>
class OneOfKeywordsToTParser {
public:
  OneOfKeywordsToTParser(std::initializer_list<typename std::map<string, T>::value_type> entries)
      : keywords(entries) {}

  Maybe<T> operator()(TokenParserInput& input) const {
    if (input.atEnd() || input.current().getType() != Token::Type::KEYWORD) {
      return nullptr;
    } else {
      auto iter = keywords.find(input.current().keyword);
      if (iter == keywords.end()) {
        return nullptr;
      } else {
        input.consume();
        return iter->second;
      }
    }
  }

private:
  std::map<string, T> keywords;
};

template <typename T>
OneOfKeywordsToTParser<T> oneOfKeywordsTo(
    std::initializer_list<typename std::map<string, T>::value_type> entries) {
  return OneOfKeywordsToTParser<T>(entries);
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
          return parseTokenSequence((input.consume().*member)[0], subParser);
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
          subResults.push_back(parseTokenSequence(element, subParser));
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

const ExpressionParser atomicExpression = oneOf(
    // Identifier.
    transform(identifier,
        [](string&& name) { return Expression::fromVariable(move(name)); }),

    // Parenthesized expression or tuple.
    transform(parenthesizedList(generalExpression),
        [](vector<Expression>&& expressions) {
          if (expressions.size() == 1) {
            return expressions[0];
          } else {
            return Expression::fromTuple(move(expressions));
          }
        }),

    // Array literal
    transform(bracketedList(generalExpression),
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

auto styleAllowance = oneOfKeywordsTo<StyleAllowance>({
    std::make_pair("@", StyleAllowance::IMMUTABLE_REFERENCE),
    std::make_pair("&", StyleAllowance::MUTABLE_REFERENCE),
    std::make_pair("<-", StyleAllowance::MOVE)});

parser::Parser<TokenParserInput, Expression::FunctionCall::Parameter> functionCallParameter =
    transform(sequence(optional(styleAllowance), generalExpression),
        [](Maybe<StyleAllowance>&& prefix, Expression&& exp) {
          StyleAllowance style = prefix ? *prefix : StyleAllowance::VALUE;
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
    transform(bracketed(generalExpression),
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

const ExpressionParser suffixedExpression = transform(
    sequence(atomicExpression, repeated(suffix)),
    [] (Expression&& seed, vector<std::function<Expression(Expression&&)>>&& suffixes) {
      for (auto& suffix : suffixes) {
        seed = move(suffix(move(seed)));
      }
      return move(seed);
    });

const ExpressionParser prefixedExpression =
    transform(sequence(repeated(oneOfKeywords({"++", "--", "+", "-", "!", "~"})),
                       suffixedExpression),
        [](vector<string>&& ops, Expression&& seed) {
          for (string& op : ops) {
            seed = Expression::fromPrefixOperator(move(op), move(seed));
          }
          return move(seed);
        });

const ExpressionParser binaryOpParser(const ExpressionParser& next,
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

const ExpressionParser multiplyExpression = binaryOpParser(prefixedExpression, {"*", "/", "%"});
const ExpressionParser addExpression      = binaryOpParser(multiplyExpression, {"+", "-"});
const ExpressionParser shiftExpression    = binaryOpParser(addExpression     , {"<<", ">>"});
const ExpressionParser bitandExpression   = binaryOpParser(shiftExpression   , {"&"});
const ExpressionParser bitxorExpression   = binaryOpParser(bitandExpression  , {"^"});
const ExpressionParser bitorExpression    = binaryOpParser(bitxorExpression  , {"|"});
const ExpressionParser andExpression      = binaryOpParser(bitorExpression   , {"&&"});
const ExpressionParser orExpression       = binaryOpParser(andExpression     , {"||"});

auto ternarySuffix = transform(
    sequence(keyword("?"), generalExpression, keyword(":"), generalExpression),
    [](Expression&& trueClause, Expression&& falseClause) {
      return std::make_pair(move(trueClause), move(falseClause));
    });

const ExpressionParser generalExpression = transform(
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
// Declaration parser!

auto kind = oneOfKeywordsTo<Declaration::Kind>({
    std::make_pair("var", Declaration::Kind::VARIABLE),
    std::make_pair("env", Declaration::Kind::ENVIRONMENT),
    std::make_pair("func", Declaration::Kind::FUNCTION),
    std::make_pair("constructor", Declaration::Kind::CONSTRUCTOR),
    std::make_pair("destructor", Declaration::Kind::DESTRUCTOR),
    std::make_pair("conversion", Declaration::Kind::CONVERSION),
    std::make_pair("class", Declaration::Kind::CLASS),
    std::make_pair("interface", Declaration::Kind::INTERFACE),
    std::make_pair("enum", Declaration::Kind::ENUM)});

auto aliasQualifier = oneOfKeywordsTo<Style>({
    std::make_pair("@", Style::IMMUTABLE_REFERENCE),
    std::make_pair("&", Style::MUTABLE_REFERENCE),
    std::make_pair("@&", Style::ENTANGLED_REFERENCE),
    std::make_pair("*", Style::HEAP_VALUE),
    std::make_pair("^", Style::CONSTANT)});

auto relationship = oneOfKeywordsTo<Annotation::Relationship>({
    std::make_pair(":", Annotation::Relationship::IS_A),
    std::make_pair("<:", Annotation::Relationship::SUBCLASS_OF),
    std::make_pair(":>", Annotation::Relationship::SUPERCLASS_OF),
    std::make_pair("::", Annotation::Relationship::ANNOTATION)});

auto annotation = transform(sequence(relationship, optional(generalExpression)),
    [](Annotation::Relationship relationship, Maybe<Expression>&& param) {
      return Annotation(relationship, move(param));
    });

const DeclarationParser variableDeclaration = transform(
    sequence(identifier, optional(aliasQualifier), repeated(annotation)),
    [](string&& name, Maybe<Style>&& style, vector<Annotation>&& annotations) {
      Declaration result(Declaration::Kind::VARIABLE);
      result.name = move(name);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser annotatedVariableDeclaration = transform(
    sequence(identifier, optional(aliasQualifier), oneOrMore(annotation)),
    [](string&& name, Maybe<Style>&& style, vector<Annotation>&& annotations) {
      Declaration result(Declaration::Kind::VARIABLE);
      result.name = move(name);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser complexlyNamedVariableDeclaration = transform(
    sequence(identifier,
             parenthesizedList(generalExpression),
             optional(aliasQualifier),
             oneOrMore(annotation)),
    [] (string&& name,
        vector<Expression>&& parameters,
        Maybe<Style>&& style,
        vector<Annotation>&& annotations) {
      Declaration result(Declaration::Kind::VARIABLE);
      result.name = move(name);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser functionDeclaration = transform(
    sequence(optional(aliasQualifier),
             identifier,
             parenthesizedList(parameterDeclaration),
             optional(aliasQualifier),
             repeated(annotation)),
    [] (Maybe<Style> thisStyle,
        string&& name,
        vector<ParameterDeclaration> parameters,
        Maybe<Style> style, vector<Annotation>&& annotations) {
      Declaration result(Declaration::Kind::FUNCTION);
      result.thisStyle = thisStyle ? *thisStyle : Style::VALUE;
      result.name = move(name);
      result.parameters = move(parameters);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser explicitDeclaration = transform(
    sequence(kind,
             optional(aliasQualifier),
             optional(identifier),
             optional(parenthesizedList(parameterDeclaration)),
             optional(aliasQualifier),
             repeated(annotation)),
    [] (Declaration::Kind kind,
        Maybe<Style>&& thisStyle,
        Maybe<string>&& name,
        Maybe<vector<ParameterDeclaration>>&& parameters,
        Maybe<Style>&& style,
        vector<Annotation>&& annotations) {
      Declaration result(kind);
      result.thisStyle = thisStyle ? *thisStyle : Style::VALUE;
      result.style = style ? *style : Style::VALUE;
      if (name) {
        result.name = move(*name);
      }
      result.parameters = move(parameters);
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser declarationForBlock = oneOf(
    variableDeclaration,
    functionDeclaration,
    explicitDeclaration);

// TODO:  Allow assignment syntax for defining functions, without "func" keyword?  It could be a
//   nice way to write a quick one-liner function without using a block.  The trouble is:
//   1) We need to not match the left-hand side when it is a function call.  We could accomplish
//      this by requiring the declaration to have a return type annotation, but this is
//      inconsistent with other function call declarations that can omit the return type to return
//      void.
//   2) We may want to distinguish from complexly-named variables as well, in which case we need to
//      ensure that at least one parameter is a variable.  If we want to support complexly-named
//      variables here, it may be better to avoid supporting functions.
// TODO:  Speaking of which, *do* we want to allow complexly-named variables here, without the
//   "var" keyword?
const DeclarationParser declarationForAssignment = oneOf(
    annotatedVariableDeclaration,
    explicitDeclaration);

const DeclarationParser bareDeclarationInDeclarativeScope = oneOf(
    annotatedVariableDeclaration,
    functionDeclaration,
    explicitDeclaration);

const DeclarationParser bareDeclarationInImperativeScope = oneOf(
    annotatedVariableDeclaration,
    explicitDeclaration);

// Since it makes no sense for a parameter to be a function, we can allow complexly-named
// variables here without the "var" keyword.
const DeclarationParser declarationForParameter = oneOf(
    annotatedVariableDeclaration,
    complexlyNamedVariableDeclaration,
    explicitDeclaration);

const ParameterDeclarationParser parameterDeclaration = oneOf(
    transform(declarationForParameter, [](Declaration&& declaration){
      return ParameterDeclaration::fromVariable(move(declaration));
    }),
    transform(generalExpression, [](Expression&& expression){
      return ParameterDeclaration::fromConstant(move(expression));
    }));

const DeclarationParser definitionByAssignment = transform(
    sequence(declarationForAssignment, keyword("="), generalExpression),
    [](Declaration&& declaration, Expression&& expression) {
      declaration.definition = Declaration::Definition::fromExpression(move(expression));
      return move(declaration);
    });

StatementParser declarationStatement(const DeclarationParser& parser) {
  return transform(parser, [](Declaration&& declaration) {
    return Statement::fromDeclaration(move(declaration));
  });
}

// =======================================================================================
// Statement parser!

const StatementParser declarativeBlockStatement = oneOf(
    declarationStatement(declarationForBlock));

const StatementParser imperativeBlockStatement = oneOf(
    declarationStatement(declarationForBlock));

const StatementParser declarativeLineStatement = oneOf(
    declarationStatement(bareDeclarationInDeclarativeScope),
    declarationStatement(definitionByAssignment));

const StatementParser imperativeLineStatement = oneOf(
    declarationStatement(bareDeclarationInImperativeScope),
    declarationStatement(definitionByAssignment));

// =======================================================================================

enum class BlockType {
  DECLARATIVE,
  IMPERATIVE,
  ENUM,
  UNION,
  NONE
};

BlockType getBlockType(Declaration::Kind kind) {
  switch (kind) {
    case Declaration::Kind::VARIABLE:
    case Declaration::Kind::CLASS:
    case Declaration::Kind::INTERFACE:
      return BlockType::DECLARATIVE;

    case Declaration::Kind::FUNCTION:
    case Declaration::Kind::CONSTRUCTOR:
    case Declaration::Kind::DESTRUCTOR:
    case Declaration::Kind::CONVERSION:
    case Declaration::Kind::DEFAULT_CONVERSION:
      return BlockType::IMPERATIVE;

    case Declaration::Kind::ENUM:
      return BlockType::ENUM;

    case Declaration::Kind::ENVIRONMENT:
      return BlockType::NONE;
  }

  return BlockType::NONE;
}

std::pair<BlockType, vector<Statement>*> whereDoesTheBlockGo(Statement& outer) {
  switch (outer.getType()) {
    case Statement::Type::ERROR:
    case Statement::Type::EXPRESSION:
    case Statement::Type::ASSIGNMENT:
    case Statement::Type::RETURN:
    case Statement::Type::BREAK:
    case Statement::Type::CONTINUE:
    case Statement::Type::BLANK:
    case Statement::Type::COMMENT:
      return std::make_pair(BlockType::NONE, nullptr);

    case Statement::Type::DECLARATION:
      outer.declaration.definition = Declaration::Definition::fromBlock(vector<Statement>());
      return std::make_pair(getBlockType(outer.declaration.kind),
                            &outer.declaration.definition->block);

    case Statement::Type::BLOCK:
      return std::make_pair(BlockType::IMPERATIVE, &outer.block);

    case Statement::Type::IF:
      return whereDoesTheBlockGo(*outer.if_.body);
    case Statement::Type::ELSE:
      return whereDoesTheBlockGo(*outer.else_);
    case Statement::Type::FOR:
      return whereDoesTheBlockGo(*outer.for_.body);
    case Statement::Type::WHILE:
      return whereDoesTheBlockGo(*outer.while_.body);
    case Statement::Type::LOOP:
      return whereDoesTheBlockGo(*outer.loop.body);
    case Statement::Type::PARALLEL:
      return std::make_pair(BlockType::IMPERATIVE, &outer.parallel);

    case Statement::Type::UNION:
      return std::make_pair(BlockType::UNION, nullptr);
  }

  return std::make_pair(BlockType::NONE, nullptr);
}

Statement parseImperative(const TokenStatement& input);
Statement parseDeclarative(const TokenStatement& input);

void parseBlock(const vector<TokenStatement>& block, Statement& target) {
  auto where = whereDoesTheBlockGo(target);
  BlockType blockType = where.first;
  vector<Statement>* targetVec = where.second;

  if (targetVec != nullptr) {
    targetVec->reserve(block.size());
  }

  switch (blockType) {
    case BlockType::DECLARATIVE:
      for (auto& tokenStatement: block) {
        targetVec->push_back(parseDeclarative(tokenStatement));
      }
      break;
    case BlockType::IMPERATIVE:
      for (auto& tokenStatement: block) {
        targetVec->push_back(parseImperative(tokenStatement));
      }
      break;
    case BlockType::ENUM:
      throw "unimplemented";
    case BlockType::UNION:
      throw "unimplemented";
    case BlockType::NONE:
      // TODO:  Some kind of error.
      break;
  }
}

Statement parseImperative(const TokenStatement& input) {
  if (input.block) {
    Statement result = parseTokenSequence(input.tokens, imperativeBlockStatement);
    parseBlock(*input.block, result);
    return result;
  } else {
    return parseTokenSequence(input.tokens, imperativeBlockStatement);
  }
}

Statement parseDeclarative(const TokenStatement& input) {
  if (input.block) {
    Statement result = parseTokenSequence(input.tokens, declarativeBlockStatement);
    parseBlock(*input.block, result);
    return result;
  } else {
    return parseTokenSequence(input.tokens, declarativeBlockStatement);
  }
}

vector<Statement> parse(const vector<TokenStatement>& statements) {
  // Temporary:  Force compilation.
  generalExpression(*((TokenParserInput*)nullptr));

  return vector<Statement>();
}

}  // namespace astParser
}  // namespace modc
