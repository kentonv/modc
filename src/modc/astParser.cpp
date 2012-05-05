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

using errors::Location;
using errors::Located;

using namespace std::placeholders;

struct TokenParserInput : public parser::IteratorInput<Token, vector<Token>::const_iterator> {
  explicit TokenParserInput(const TokenSequence& sequence)
      : parser::IteratorInput<Token, vector<Token>::const_iterator>(
          sequence.tokens.begin(), sequence.tokens.end()) {}
};

// Proxy type that converts token iterator range to location.
struct Loc: public Location {
  Loc(const std::pair<vector<Token>::const_iterator, vector<Token>::const_iterator>& range)
      : Location(range.first->location.to(range.second->location)) {}
};

auto endOfInput = parser::endOfInput<TokenParserInput>();

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

template <typename SubParser>
class LocatedParser {
public:
  explicit LocatedParser(SubParser&& subParser)
      : subParser(move(subParser)) {}

  typedef typename parser::ExtractParserType<SubParser>::OutputType SubOutput;

  Maybe<Located<SubOutput>> operator()(TokenParserInput& input) const {
    auto start = input.getPosition();
    Maybe<SubOutput> subResult = subParser(input);
    if (subResult) {
      return errors::Located<SubOutput>(start->location.to(input.getPosition()->location),
                                        *subResult);
    } else {
      return nullptr;
    }
  }

private:
  SubParser subParser;
};

parser::WrapperParserConstructor<LocatedParser> located;

// Hack:  This set is automatically initialized as the parsers are constructed.
std::set<string> keywords;

const std::set<string>& keywordSet() {
  return keywords;
}

template <typename Parser>
typename parser::ExtractParserType<Parser>::OutputType
parseTokenSequence(const TokenSequence& sequence, const Parser& subParser) {
  TokenParserInput input(sequence);
  typedef typename parser::ExtractParserType<Parser>::OutputType Result;
  Maybe<Result> parseResult = subParser(input);

  if (parseResult && input.atEnd()) {
    return *parseResult;
  } else {
    vector<errors::Error> errors = sequence.getErrors();

    vector<Token>::const_iterator best = input.getBest();
    if (best < sequence.tokens.end()) {
      errors.push_back(errors::error(best->location.to(sequence.location), "Syntax error."));
    } else {
      errors.push_back(errors::error(sequence.location.last(0), "Syntax error."));
    }
    return Result::fromError(sequence.location, move(errors));
  }
}

parser::ExactElementParser<TokenParserInput> keyword(string&& name) {
  keywords.insert(name);
  return parser::exactElement<TokenParserInput>(tokens::keyword(Location(), move(name)));
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
  keywords.insert(names.begin(), names.end());
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
  for (auto& pair: entries) {
    keywords.insert(pair.first);
  }
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
          errors.push_back(errors::error(input.current().location,
                           "Must have exactly one element."));
          input.consume().getErrors(errors);
          return SubResult::fromError(input.current().location, move(errors));
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
    Token::Type::PARENTHESIZED, &Token::parenthesized>::Parser> parenthesized;
parser::WrapperParserConstructor<ListParserTemplate<
    Token::Type::PARENTHESIZED, &Token::parenthesized>::Parser> parenthesizedList;

// =======================================================================================
// Expression parser!

const ExpressionParser atomicExpression = oneOf(
    // Identifier.
    transform(identifier,
        [](Loc l, string&& name) { return Expression::fromVariable(l, move(name)); }),

    // Parenthesized expression or tuple.
    transform(parenthesizedList(generalExpression),
        [](Loc l, vector<Expression>&& expressions) {
          if (expressions.size() == 1) {
            return expressions[0];
          } else {
            return Expression::fromTuple(l, move(expressions));
          }
        }),

    // Array literal
    transform(bracketedList(generalExpression),
        [](Loc l, vector<Expression>&& expressions) {
          return Expression::fromLiteralArray(l, move(expressions));
        }),

    // Import
    transform(sequence(keyword("import"), literalString),
        [](Loc l, string&& moduleName) {
          return Expression::fromImport(l, move(moduleName));
        }),

    // Primitive literals
    transform(literalInt,
        [](Loc l, int value) { return Expression::fromLiteralInt(l, value); }),
    transform(literalDouble,
        [](Loc l, double value) { return Expression::fromLiteralDouble(l, value); }),
    transform(literalString,
        [](Loc l, string&& value) {
          return Expression::fromLiteralString(l, move(value));
        }));

auto styleAllowance = oneOfKeywordsTo<StyleAllowance>({
    std::make_pair("@", StyleAllowance::IMMUTABLE_REFERENCE),
    std::make_pair("&", StyleAllowance::MUTABLE_REFERENCE),
    std::make_pair("<-", StyleAllowance::MOVE)});

parser::Parser<TokenParserInput, Expression::FunctionCall::Parameter> functionCallParameter =
    transform(sequence(optional(styleAllowance), generalExpression),
        [](Loc l, Maybe<StyleAllowance>&& prefix, Expression&& exp) {
          StyleAllowance style = prefix ? *prefix : StyleAllowance::VALUE;
          return Expression::FunctionCall::Parameter(style, move(exp));
        });

parser::Parser<TokenParserInput, std::function<Expression(Expression&&)>> suffix = oneOf(
    // Member access
    transform(sequence(keyword("."), identifier),
        [](Loc l, string&& name) -> std::function<Expression(Expression&&)> {
          return std::bind([l](string& name, Expression&& seed) {
            return Expression::fromMemberAccess(l, move(seed), move(name));
          }, move(name), _1);
        }),

    // Function call
    transform(parenthesizedList(functionCallParameter),
        [](Loc l, vector<Expression::FunctionCall::Parameter>&& params) ->
            std::function<Expression(Expression&&)> {
          return std::bind(
            [l](vector<Expression::FunctionCall::Parameter>& params, Expression&& seed) {
              return Expression::fromFunctionCall(l, move(seed), move(params));
            }, move(params), _1);
        }),

    // Subscript
    transform(bracketed(generalExpression),
        [](Loc l, Expression&& key) -> std::function<Expression(Expression&&)> {
          return std::bind([l](Expression& key, Expression&& seed) {
            return Expression::fromSubscript(l, move(seed), move(key));
          }, move(key), _1);
        }),

    // postincrement / postdecrement
    transform(oneOfKeywords({"++", "--"}),
        [](Loc l, string&& op) -> std::function<Expression(Expression&&)> {
          return std::bind([l](string& op, Expression&& seed) {
            return Expression::fromPostfixOperator(l, move(seed), move(op));
          }, move(op), _1);
        }));

const ExpressionParser suffixedExpression = transform(
    sequence(atomicExpression, repeated(suffix)),
    [] (Loc l, Expression&& seed, vector<std::function<Expression(Expression&&)>>&& suffixes) {
      for (auto& suffix : suffixes) {
        seed = suffix(move(seed));
        // Need to fix location to cover the whole expression and not just the suffix.
        seed.location = l.to(seed.location);
      }
      return move(seed);
    });

const ExpressionParser prefixedExpression =
    transform(sequence(repeated(located(oneOfKeywords({"++", "--", "+", "-", "!", "~"}))),
                       suffixedExpression),
        [](Loc l, vector<Located<string>>&& ops, Expression&& seed) {
          for (auto& op : ops) {
            seed = Expression::fromPrefixOperator(op.location.to(l), move(op.value), move(seed));
          }
          return move(seed);
        });

const ExpressionParser binaryOpParser(const ExpressionParser& next,
                                      std::initializer_list<string> ops) {
  auto suffix = transform(
      sequence(oneOfKeywords(ops), next),
      [](Loc l, string&& op, Expression&& operand) {
        return std::make_pair(move(op), move(operand));
      });

  return transform(sequence(next, repeated(move(suffix))),
      [](Loc l, Expression&& seed, vector<std::pair<string, Expression>>&& ops) {
        for (auto& op : ops) {
          seed = Expression::fromBinaryOperator(l.to(op.second.location),
                                                move(op.first), move(seed), move(op.second));
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
    [](Loc l, Expression&& trueClause, Expression&& falseClause) {
      return std::make_pair(move(trueClause), move(falseClause));
    });

const ExpressionParser generalExpression = transform(
    sequence(orExpression, optional(move(ternarySuffix))),
    [](Loc l, Expression&& condition, Maybe<std::pair<Expression, Expression>>&& clauses) {
      if (clauses) {
        return Expression::fromTernaryOperator(
            l, move(condition), move(clauses->first), move(clauses->second));
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
    [](Loc l, Annotation::Relationship relationship, Maybe<Expression>&& param) {
      return Annotation(relationship, move(param));
    });

const DeclarationParser variableDeclaration = transform(
    sequence(located(identifier), optional(aliasQualifier), repeated(annotation)),
    [](Loc l, Located<string>&& name, Maybe<Style>&& style, vector<Annotation>&& annotations) {
      Declaration result(l, Declaration::Kind::VARIABLE);
      result.name = move(name);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser annotatedVariableDeclaration = transform(
    sequence(located(identifier), optional(aliasQualifier), oneOrMore(annotation)),
    [](Loc l, Located<string>&& name, Maybe<Style>&& style, vector<Annotation>&& annotations) {
      Declaration result(l, Declaration::Kind::VARIABLE);
      result.name = move(name);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser complexlyNamedVariableDeclaration = transform(
    sequence(located(identifier),
             parenthesizedList(generalExpression),
             optional(aliasQualifier),
             oneOrMore(annotation)),
    [] (Loc l,
        Located<string>&& name,
        vector<Expression>&& parameters,
        Maybe<Style>&& style,
        vector<Annotation>&& annotations) {
      Declaration result(l, Declaration::Kind::VARIABLE);
      result.name = move(name);
      result.style = style ? *style : Style::VALUE;
      result.annotations = move(annotations);
      return move(result);
    });

const DeclarationParser functionDeclaration = transform(
    sequence(optional(aliasQualifier),
             located(identifier),
             parenthesizedList(parameterDeclaration),
             optional(aliasQualifier),
             repeated(annotation)),
    [] (Loc l,
        Maybe<Style> thisStyle,
        Located<string>&& name,
        vector<ParameterDeclaration> parameters,
        Maybe<Style> style, vector<Annotation>&& annotations) {
      Declaration result(l, Declaration::Kind::FUNCTION);
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
             optional(located(identifier)),
             optional(parenthesizedList(parameterDeclaration)),
             optional(aliasQualifier),
             repeated(annotation)),
    [] (Loc l,
        Declaration::Kind kind,
        Maybe<Style>&& thisStyle,
        Maybe<Located<string>>&& name,
        Maybe<vector<ParameterDeclaration>>&& parameters,
        Maybe<Style>&& style,
        vector<Annotation>&& annotations) {
      Declaration result(l, kind);
      result.thisStyle = thisStyle ? *thisStyle : Style::VALUE;
      result.style = style ? *style : Style::VALUE;
      result.name = move(name);
      result.parameters = move(parameters);
      result.annotations = move(annotations);
      return move(result);
    });

// -------------------------------------------------------------------

// A declaration that will be followed by a block.
const DeclarationParser declarationForBlock = oneOf(
    variableDeclaration,
    functionDeclaration,
    explicitDeclaration);

// A declaration that will be followed by an assignment.
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
// TODO:  Maybe use "=>" for short functions?
const DeclarationParser declarationForAssignment = oneOf(
    annotatedVariableDeclaration,
    explicitDeclaration);

// A declaration that is followed by semicolon, in declarative scope.
const DeclarationParser bareDeclarationInDeclarativeScope = oneOf(
    annotatedVariableDeclaration,
    functionDeclaration,
    explicitDeclaration);

// A declaration that is followed by semicolon, in imperative scope.  In fact, only variable
// declarations can occur this way in imperative scope, but we parse the others in order to produce
// nicer errors.
const DeclarationParser bareDeclarationInImperativeScope = oneOf(
    annotatedVariableDeclaration,
    explicitDeclaration);

// The only declaration kinds that remotely make sense as parameters are "var" and "class".
// As a result, we can allow complexly-named vars without the explit "var" keyword, because they
// cannot be mistaken for functions.
// TODO:  Do we actually want to allow "class" here, or just use vars with type "Type"?
//
// Note that this parser is also used to parse the stuff in parentheses after the "for" keyword.
const DeclarationParser declarationForParameter = oneOf(
    annotatedVariableDeclaration,
    complexlyNamedVariableDeclaration,
    explicitDeclaration);

// A parameter can be either a declaration or a constant.
const ParameterDeclarationParser parameterDeclaration = oneOf(
    transform(sequence(declarationForParameter,
          optional(sequence(keyword("="), generalExpression))),
      [](Loc l, Declaration&& declaration, Maybe<Expression>&& definition){
      if (definition) {
        declaration.definition = Declaration::Definition::fromExpression(move(*definition));
      }
      return ParameterDeclaration::fromVariable(l, move(declaration));
    }),
    transform(generalExpression, [](Loc l, Expression&& expression){
      return ParameterDeclaration::fromConstant(l, move(expression));
    }));

// A complete assignment statement.
const DeclarationParser definitionByAssignment = transform(
    sequence(declarationForAssignment, keyword("="), generalExpression),
    [](Loc l, Declaration&& declaration, Expression&& expression) {
      declaration.definition = Declaration::Definition::fromExpression(move(expression));
      return move(declaration);
    });

// =======================================================================================
// Statement parser!

StatementParser expressionStatement = transform(generalExpression,
    [](Loc l, Expression&& expression) { return Statement::fromExpression(l, move(expression)); });

StatementParser blockStatement = transform(endOfInput,
    // Block will be filled in later.
    [](Loc l) { return Statement::fromBlock(l, vector<Statement>()); });

// Convert a declaration parser into a statement parser.
StatementParser declarationStatement(const DeclarationParser& parser) {
  return transform(parser, [](Loc l, Declaration&& declaration) {
    return Statement::fromDeclaration(l, move(declaration));
  });
}

StatementParser assignmentStatement = transform(
    sequence(generalExpression, keyword("="), generalExpression),
    [](Loc l, Expression&& variable, Expression&& value) {
      return Statement::fromAssignment(l, move(variable), move(value));
    });

StatementParser unionStatement = transform(keyword("union"),
    // Block will be filled in later.
    [](Loc l) { return Statement::fromUnion(l, vector<Declaration>()); });

StatementParser ifStatement(const StatementParser& generalStatement) {
  return transform(sequence(keyword("if"), parenthesized(generalExpression), generalStatement),
      [](Loc l, Expression&& condition, Statement&& body) {
        return Statement::fromIf(l, move(condition), move(body));
      });
}

StatementParser forStatement(const StatementParser& generalStatement) {
  return transform(sequence(keyword("for"), parenthesizedList(declarationForParameter),
                            generalStatement),
      [](Loc l, vector<Declaration>&& declarations, Statement&& body) {
        return Statement::fromFor(l, move(declarations), move(body));
      });
}

StatementParser whileStatement(const StatementParser& generalStatement) {
  return transform(sequence(keyword("while"), parenthesized(generalExpression),
                            generalStatement),
      [](Loc l, Expression&& condition, Statement&& body) {
        return Statement::fromWhile(l, move(condition), move(body));
      });
}

StatementParser loopStatement(const StatementParser& generalStatement) {
  return transform(sequence(keyword("loop"), optional(identifier), generalStatement),
      [](Loc l, Maybe<string>&& name, Statement&& body) {
        return Statement::fromLoop(l, move(name), move(body));
      });
}

StatementParser parallelStatement = transform(keyword("parallel"),
    // Block will be filled in later.
    [](Loc l) { return Statement::fromParallel(l, vector<Statement>()); });

StatementParser returnStatement = transform(
    sequence(keyword("return"), optional(generalExpression)),
    [](Loc l, Maybe<Expression>&& expression) {
      if (expression) {
        return Statement::fromReturn(l, move(*expression));
      } else {
        return Statement::fromReturn(l, Expression::fromTuple(l.last(0), vector<Expression>()));
      }
    });


auto breakOrContinue = oneOfKeywordsTo<bool>({
    std::make_pair("break", true),
    std::make_pair("continue", false)});

StatementParser breakContinueStatement = transform(
    sequence(breakOrContinue, optional(identifier),
             optional(sequence(keyword("if"), parenthesized(generalExpression)))),
    [](Loc l, bool isBreak, Maybe<string>&& loopName, Maybe<Expression>&& condition) {
      // TODO:  How should location work when an "if" is present?  Should the if statement contain
      //   the break, or the break contain the if?  Currently they just end up with equal spans.
      Statement result = isBreak ?
          Statement::fromBreak(l, move(loopName)) :
          Statement::fromContinue(l, move(loopName));
      if (condition) {
        result = Statement::fromIf(l, move(*condition), move(result));
      }
      return result;
    });

const StatementParser imperativeLineStatement = oneOf(
    assignmentStatement,
    declarationStatement(definitionByAssignment),
    declarationStatement(bareDeclarationInImperativeScope),
    expressionStatement,
    ifStatement(imperativeLineStatement),
    forStatement(imperativeLineStatement),
    whileStatement(imperativeLineStatement),
    loopStatement(imperativeLineStatement),
    returnStatement,
    breakContinueStatement);

const StatementParser imperativeBlockStatement = oneOf(
    blockStatement,
    declarationStatement(declarationForBlock),
    ifStatement(imperativeBlockStatement),
    forStatement(imperativeBlockStatement),
    whileStatement(imperativeBlockStatement),
    loopStatement(imperativeBlockStatement),
    parallelStatement);

const StatementParser declarativeLineStatement = oneOf(
    declarationStatement(definitionByAssignment),
    declarationStatement(bareDeclarationInDeclarativeScope),
    ifStatement(declarativeLineStatement),
    forStatement(declarativeLineStatement),
    whileStatement(declarativeLineStatement),
    loopStatement(declarativeLineStatement));

const StatementParser declarativeBlockStatement = oneOf(
    declarationStatement(declarationForBlock),
    unionStatement,
    ifStatement(declarativeBlockStatement),
    forStatement(declarativeBlockStatement),
    whileStatement(declarativeBlockStatement),
    loopStatement(declarativeBlockStatement));

// TODO: enumStatement

const DeclarationParser unionLineMember = oneOf(
    definitionByAssignment,
    bareDeclarationInDeclarativeScope);
auto& unionBlockMember = declarationForBlock;

// =======================================================================================

Declaration parseUnionMember(const TokenStatement& input);

enum class StatementContext {
  IMPERATIVE,
  DECLARATIVE,
  ENUM
};

bool attachBlock(vector<Statement>& target, const vector<TokenStatement>& block,
                 StatementContext context) {
  target.reserve(block.size());
  switch (context) {
    case StatementContext::IMPERATIVE:
      for (auto& tokenStatement: block) {
        target.push_back(parseImperative(tokenStatement));
      }
      return true;
    case StatementContext::DECLARATIVE:
      for (auto& tokenStatement: block) {
        target.push_back(parseDeclarative(tokenStatement));
      }
      return true;
    case StatementContext::ENUM:
      throw "unimplemented";
      return true;
  }

  return false;
}

bool attachBlock(vector<Declaration>& target, const vector<TokenStatement>& block) {
  target.reserve(block.size());
  for (auto& tokenStatement: block) {
    target.push_back(parseUnionMember(tokenStatement));
  }

  return true;
}

bool attachBlock(Declaration& declaration, const vector<TokenStatement>& block) {
  declaration.definition = Declaration::Definition::fromBlock(vector<Statement>());

  switch (declaration.kind) {
    case Declaration::Kind::VARIABLE:
    case Declaration::Kind::CLASS:
    case Declaration::Kind::INTERFACE:
      attachBlock(declaration.definition->block, block, StatementContext::DECLARATIVE);
      return true;

    case Declaration::Kind::FUNCTION:
    case Declaration::Kind::CONSTRUCTOR:
    case Declaration::Kind::DESTRUCTOR:
    case Declaration::Kind::CONVERSION:
    case Declaration::Kind::DEFAULT_CONVERSION:
      attachBlock(declaration.definition->block, block, StatementContext::IMPERATIVE);
      return true;

    case Declaration::Kind::ENUM:
      attachBlock(declaration.definition->block, block, StatementContext::ENUM);
      return true;

    case Declaration::Kind::ERROR:
      return true;

    case Declaration::Kind::ENVIRONMENT:
      return false;
  }

  return false;
}

bool attachBlock(Statement& statement, const vector<TokenStatement>& block) {
  switch (statement.getType()) {
    case Statement::Type::ERROR:
      return true;

    case Statement::Type::EXPRESSION:
    case Statement::Type::ASSIGNMENT:
    case Statement::Type::RETURN:
    case Statement::Type::BREAK:
    case Statement::Type::CONTINUE:
    case Statement::Type::BLANK:
    case Statement::Type::COMMENT:
      return false;

    case Statement::Type::DECLARATION:
      return attachBlock(statement.declaration, block);

    case Statement::Type::BLOCK:
      return attachBlock(statement.block, block, StatementContext::IMPERATIVE);

    case Statement::Type::IF:
      return attachBlock(*statement.if_.body, block);
    case Statement::Type::ELSE:
      return attachBlock(*statement.else_, block);
    case Statement::Type::FOR:
      return attachBlock(*statement.for_.body, block);
    case Statement::Type::WHILE:
      return attachBlock(*statement.while_.body, block);
    case Statement::Type::LOOP:
      return attachBlock(*statement.loop.body, block);
    case Statement::Type::PARALLEL:
      return attachBlock(statement.parallel, block, StatementContext::IMPERATIVE);

    case Statement::Type::UNION:
      return attachBlock(statement.union_, block);
  }

  return false;
}

Statement parseImperative(const TokenStatement& input) {
  if (input.block) {
    Statement result = parseTokenSequence(input.tokens, imperativeBlockStatement);
    if (!attachBlock(result, *input.block)) {
      throw "bad block";
    }
    return result;
  } else {
    return parseTokenSequence(input.tokens, imperativeLineStatement);
  }
}

Statement parseDeclarative(const TokenStatement& input) {
  if (input.block) {
    Statement result = parseTokenSequence(input.tokens, declarativeBlockStatement);
    if (!attachBlock(result, *input.block)) {
      throw "bad block";
    }
    return result;
  } else {
    return parseTokenSequence(input.tokens, declarativeLineStatement);
  }
}

Declaration parseUnionMember(const TokenStatement& input) {
  if (input.block) {
    Declaration result = parseTokenSequence(input.tokens, unionBlockMember);
    if (!attachBlock(result, *input.block)) {
      throw "bad block";
    }
    return result;
  } else {
    return parseTokenSequence(input.tokens, unionLineMember);
  }
}

}  // namespace astParser
}  // namespace modc
