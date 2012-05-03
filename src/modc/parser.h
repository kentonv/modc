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

#ifndef KENTONSCODE_MODC_PARSER_H_
#define KENTONSCODE_MODC_PARSER_H_

#include <utility>
#include <type_traits>
#include <vector>
#include <string>

#include "base/OwnedPtr.h"
#include "modc/errors.h"
#include "Maybe.h"

namespace modc {
namespace parser {

using ekam::OwnedPtr;
using std::move;
using std::remove_reference;
using std::string;

// I don't understand std::tuple.
template <typename... T>
struct Tuple;

template <>
struct Tuple<> {
  Tuple() {}

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType apply(Func&& f, InitialParams&&... initialParams) const {
    return f(std::forward<InitialParams>(initialParams)...);
  }

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    return f(std::forward<InitialParams>(initialParams)...);
  }
};

template <typename First, typename... Rest>
struct Tuple<First, Rest...> {
  Tuple() {}

  Tuple(Tuple&& other): first(move(other.first)), rest(move(other.rest)) {}
  Tuple(const Tuple& other): first(other.first), rest(other.rest) {}
  Tuple(Tuple& other): first(other.first), rest(other.rest) {}

  template <typename First2, typename... Rest2>
  explicit Tuple(First2&& first2, Rest2&&... rest2)
      : first(std::forward<First2>(first2)),
        rest(std::forward<Rest2>(rest2)...) {}

  First first;
  Tuple<Rest...> rest;

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType apply(Func&& f, InitialParams&&... initialParams) const {
    return rest.apply<ReturnType>(
        std::forward<Func>(f), std::forward<InitialParams>(initialParams)..., first);
  }

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    return rest.applyAsRvalue<ReturnType>(
        std::forward<Func>(f), std::forward<InitialParams>(initialParams)..., std::move(first));
  }
};

template <typename... First, typename... Rest>
struct Tuple<Tuple<First...>, Rest...> {
  Tuple() {}

  Tuple(Tuple&& other): first(move(other.first)), rest(move(other.rest)) {}
  Tuple(const Tuple& other): first(other.first), rest(other.rest) {}
  Tuple(Tuple& other): first(other.first), rest(other.rest) {}

  template <typename First2, typename... Rest2>
  Tuple(First2&& first2, Rest2&&... rest2)
      : first(std::forward<First2>(first2)),
        rest(std::forward<Rest2>(rest2)...) {}

  Tuple<First...> first;
  Tuple<Rest...> rest;

  template <typename ReturnType, typename Func>
  struct Continue {
    Continue(const Tuple* self, Func&& f): self(self), f(f) {}
    const Tuple* self;
    Func& f;

    template <typename... T>
    ReturnType operator()(T&&... t) const {
      return self->rest.apply<ReturnType>(std::forward<Func>(f), std::forward<T>(t)...);
    }
  };

  template <typename ReturnType, typename Func>
  struct ContinueAsRvalue {
    ContinueAsRvalue(Tuple* self, Func&& f): self(self), f(f) {}
    Tuple* self;
    Func& f;

    template <typename... T>
    ReturnType operator()(T&&... t) const {
      return self->rest.applyAsRvalue<ReturnType>(std::forward<Func>(f), std::forward<T>(t)...);
    }
  };

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType apply(Func&& f, InitialParams&&... initialParams) const {
    return first.apply<ReturnType>(
        Continue<ReturnType, Func>(this, std::forward<Func>(f)),
        std::forward<InitialParams>(initialParams)...);
  }

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    return first.applyAsRvalue<ReturnType>(
        ContinueAsRvalue<ReturnType, Func>(this, std::forward<Func>(f)),
        std::forward<InitialParams>(initialParams)...);
  }
};

template <typename... Rest>
struct Tuple<Tuple<>, Rest...> {
  Tuple() {}

  Tuple(Tuple&& other): first(move(other.first)), rest(move(other.rest)) {}
  Tuple(const Tuple& other): first(other.first), rest(other.rest) {}
  Tuple(Tuple& other): first(other.first), rest(other.rest) {}

  template <typename First2, typename... Rest2>
  Tuple(First2&& first2, Rest2&&... rest2)
      : first(std::forward<First2>(first2)),
        rest(std::forward<Rest2>(rest2)...) {}

  Tuple<> first;
  Tuple<Rest...> rest;

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType apply(Func&& f, InitialParams&&... initialParams) const {
    return rest.apply<ReturnType>(
        std::forward<Func>(f), std::forward<InitialParams>(initialParams)...);
  }

  template <typename ReturnType = void, typename Func, typename... InitialParams>
  ReturnType applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    return rest.applyAsRvalue<ReturnType>(
        std::forward<Func>(f), std::forward<InitialParams>(initialParams)...);
  }
};

template <typename... T>
Tuple<T...> tuple(T&&... elements) {
  return Tuple<T...>(elements...);
}

typedef Tuple<> Void;



template <typename T>
T any();

template <typename ReturnType = void, typename Transform, typename T>
ReturnType applyMaybeTuple(Transform& transform, T&& t) {
  return transform(move(t));
}

template <typename ReturnType = void, typename Transform, typename... T>
ReturnType applyMaybeTuple(Transform& transform, Tuple<T...>&& t) {
  return t.template applyAsRvalue<ReturnType>(transform);
}

// =======================================================================================

template <typename T, typename Return, typename... Params>
Return extractReturnType(Return (T::*func)(Params...) const);
template <typename T, typename Return, typename... Params>
Return extractReturnType(Return (T::*func)(Params...));

// =======================================================================================

template <typename T>
class ParseResult {
public:
  ParseResult(ParseResult&& other): isError_(other.isError_) {
    if (isError_) {
      new (&errors) std::vector<errors::Error>(move(other.errors));
    } else {
      new (&value) T(move(other.value));
    }
  }
  ParseResult(const ParseResult& other): isError_(other.isError_) {
    if (isError_) {
      new (&errors) std::vector<errors::Error>(other.errors);
    } else {
      new (&value) T(other.value);
    }
  }
  ParseResult(errors::Error&& error): isError_(true) {
    new (&errors) std::vector<errors::Error>();
    errors.push_back(move(error));
  }
  ParseResult(std::vector<errors::Error>&& errors): isError_(true) {
    new (&this->errors) std::vector<errors::Error>(move(errors));
  }
  ParseResult(T&& value): isError_(false) {
    new (&this->value) T(move(value));
  }
  ParseResult(const T& value): isError_(false) {
    new (&this->value) T(value);
  }
  ~ParseResult() {
    if (isError_) {
      errors.~vector();
    } else {
      value.~T();
    }
  }

  ParseResult& operator=(ParseResult&& other) {
    this->~ParseResult();
    new (this) ParseResult(move(other));
    return *this;
  }
  ParseResult& operator=(const ParseResult& other) {
    this->~ParseResult();
    new (this) ParseResult(other);
    return *this;
  }

  bool isError() { return isError_; }

  union {
    std::vector<errors::Error> errors;
    T value;
  };

private:
  bool isError_;
};

template <typename Element, typename Iterator>
class IteratorInput {
public:
  typedef Element ElementType;

  IteratorInput(Iterator begin, Iterator end)
      : pos(begin), end(end), broken(false), committed(false) {}

  bool atEnd() { return pos == end; }
  const Element& current() { return *pos; }
  void next() { ++pos; }

  errors::Error error(const string& message) {
    // TODO: Location.
    return errors::error(-1, -1, message);
  }

  // Indicates that we have lost context due to a syntax error, and therefore subsequent tokens
  // are meaningless unless we can skip forward to some sort of marker that lets us figure out
  // where we are.  (E.g., if a syntax error occurs in a parenthesized sub-expression, we cannot
  // parse the rest of the inner expression, but we can continue parsing the outer expression after
  // the closing parenthesis.)
  void setBroken(bool broken) { this->broken = broken; }

  // Returns true after error() has been called.
  bool isBroken() { return broken; }

  // Indicates that we've parsed enough input to be sure that we have chosen the correct branch of
  // the enclosing "oneOf".  If a parse error occurs before committing, then OneOfParser
  // will discard the error and choose the next branch instead.
  void setCommitted(bool committed) { this->committed = committed; }
  bool isCommitted() { return committed; }

private:
  Iterator pos;
  Iterator end;
  bool broken;
  bool committed;
};

template <typename T>
struct ExtractParseFuncType;

template <typename I, typename O, typename Object>
struct ExtractParseFuncType<ParseResult<O> (Object::*)(I&) const> {
  typedef I InputType;
  typedef typename I::ElementType ElementType;
  typedef O OutputType;
};

template <typename I, typename O, typename Object>
struct ExtractParseFuncType<ParseResult<O> (Object::*)(I&)> {
  typedef I InputType;
  typedef typename I::ElementType ElementType;
  typedef O OutputType;
};

template <typename T>
struct ExtractParserType: public ExtractParseFuncType<decltype(&T::operator())> {};
template <typename T>
struct ExtractParserType<T&>: public ExtractParseFuncType<decltype(&T::operator())> {};
template <typename T>
struct ExtractParserType<T&&>: public ExtractParseFuncType<decltype(&T::operator())> {};
template <typename T>
struct ExtractParserType<const T>: public ExtractParseFuncType<decltype(&T::operator())> {};
template <typename T>
struct ExtractParserType<const T&>: public ExtractParseFuncType<decltype(&T::operator())> {};
template <typename T>
struct ExtractParserType<const T&&>: public ExtractParseFuncType<decltype(&T::operator())> {};

// =======================================================================================

template <typename Input, typename Output>
class ParserWrapper {
public:
  virtual ~ParserWrapper() {}

  typedef Input InputType;
  typedef typename Input::ElementType ElementType;
  typedef Output OutputType;

  virtual ParseResult<Output> operator()(Input& input) const = 0;
  virtual OwnedPtr<ParserWrapper> clone() = 0;
};

template <typename Input, typename Output>
class Parser {
public:
  Parser(const Parser& other): wrapper(other.wrapper->clone()) {}
  Parser(Parser&& other): wrapper(move(other.wrapper)) {}
  Parser(OwnedPtr<ParserWrapper<Input, Output>> wrapper): wrapper(move(wrapper)) {}

  template <typename Other>
  Parser(Other&& other) {
    struct WrapperImpl: public ParserWrapper<Input, Output> {
      WrapperImpl(Other&& impl): impl(move(impl)) {};

      ParseResult<Output> operator()(Input& input) const {
        return impl(input);
      }

      OwnedPtr<ParserWrapper<Input, Output>> clone() {
        return ekam::newOwned<WrapperImpl>(*this);
      }

      Other impl;
    };

    wrapper = ekam::newOwned<WrapperImpl>(move(other));
  }

  Parser& operator=(const Parser& other) { wrapper = other.wrapper->clone(); }
  Parser& operator=(Parser&& other) { wrapper = move(other.wrapper); }

  ParseResult<Output> operator()(Input& input) const {
    return (*wrapper)(input);
  }

private:
  OwnedPtr<ParserWrapper<Input, Output>> wrapper;
};

template <typename ParserImpl>
Parser<typename ExtractParserType<ParserImpl>::InputType,
       typename ExtractParserType<ParserImpl>::OutputType>
wrap(ParserImpl&& impl) {
  typedef typename ExtractParserType<ParserImpl>::InputType Input;
  typedef typename ExtractParserType<ParserImpl>::OutputType Output;

  return Parser<Input, Output>(move(impl));
}

template <typename SubParser>
class ParserRef {
public:
  explicit ParserRef(const SubParser& parser): parser(&parser) {}

  ParseResult<typename ExtractParserType<SubParser>::OutputType> operator()(
      typename ExtractParserType<SubParser>::InputType& input) const {
    return (*parser)(input);
  }

private:
  const SubParser* parser;
};

template <typename SubParser>
ParserRef<typename remove_reference<SubParser>::type> ref(const SubParser& impl) {
  return ParserRef<SubParser>(impl);
}

template <typename T>
struct MaybeRef {
  typedef typename std::remove_reference<T>::type Type;

  template <typename U>
  static Type from(U&& parser) {
    return static_cast<Type&&>(parser);
  }
};

template <typename T>
struct MaybeRef<T&> {
  typedef ParserRef<typename std::decay<T>::type> Type;

  template <typename U>
  static Type from(U& parser) {
    // Apparent GCC 4.6 bug:  If "ref" is not qualified with "parser::" here, GCC will sometimes
    // call std::ref instead.  WTF.
    return parser::ref(parser);
  }
};

template <template <typename SubParser> class WrapperParser>
struct WrapperParserConstructor {
  template <typename SubParser, typename... Args>
  WrapperParser<typename MaybeRef<SubParser>::Type> operator()(
      SubParser&& subParser, Args&&... args) {
    return WrapperParser<typename MaybeRef<SubParser>::Type>(
        MaybeRef<SubParser>::from(subParser),
        std::forward(args)...);
  }
};

// -------------------------------------------------------------------
// ExactElementParser
// Output = Void

template <typename Input>
class ExactElementParser {
public:
  explicit ExactElementParser(typename Input::ElementType&& expected): expected(expected) {}

  virtual ParseResult<Void> operator()(Input& input) const {
    if (input.atEnd() || input.current() != expected) {
      input.setBroken(true);
      return ParseResult<Void>(input.error("Expected [TODO: complete error message]"));
    } else {
      input.next();
      return ParseResult<Void>(Void());
    }
  }

private:
  typename Input::ElementType expected;
};

template <typename Input>
ExactElementParser<Input> exactElement(typename Input::ElementType&& expected) {
  return ExactElementParser<typename remove_reference<Input>::type>(move(expected));
}

// -------------------------------------------------------------------
// SequenceParser
// Output = Tuple of outputs of sub-parsers, eliding voids.

template <typename Input, typename... SubParsers> class SequenceParser;

template <typename Input, typename FirstSubParser, typename... SubParsers>
class SequenceParser<Input, FirstSubParser, SubParsers...> {
public:
  template <typename T, typename... U>
  explicit SequenceParser(T&& firstSubParser, U&&... rest)
      : first(std::forward<T>(firstSubParser)), rest(std::forward<U>(rest)...) {}

  ParseResult<Tuple<typename ExtractParserType<FirstSubParser>::OutputType,
                    typename ExtractParserType<SubParsers>::OutputType...>>
  operator()(Input& input) const {
    return parseNext(input);
  }

  template <typename... InitialParams>
  ParseResult<Tuple<InitialParams...,
      typename ExtractParserType<FirstSubParser>::OutputType,
      typename ExtractParserType<SubParsers>::OutputType...>>
  parseNext(Input& input, InitialParams&&... initialParams) const {
    auto firstResult = first(input);
    if (firstResult.isError()) {
      if (!input.isBroken()) {
        rest.parseForErrors(input, firstResult.errors);
      }
      return move(firstResult.errors);
    } else {
      return rest.parseNext(input, std::forward<InitialParams>(initialParams)...,
                            move(firstResult.value));
    }
  }

  void parseForErrors(Input& input, std::vector<errors::Error>& errors) const {
    {
      auto firstResult = first(input);
      if (firstResult.isError()) {
        for (auto& error : firstResult.errors) {
          errors.push_back(move(error));
        }
        if (input.isBroken()) {
          return;
        }
      }
    }

    rest.parseForErrors(input, errors);
  }

private:
  FirstSubParser first;
  SequenceParser<Input, SubParsers...> rest;
};

template <typename Input>
class SequenceParser<Input> {
public:
  ParseResult<Tuple<>> operator()(Input& input) const {
    return parseNext(input);
  }

  template <typename... Params>
  ParseResult<Tuple<Params...>>
  parseNext(Input& input, Params&&... params) const {
    return ParseResult<Tuple<Params...>>(Tuple<Params...>(params...));
  }

  void parseForErrors(Input& input, std::vector<errors::Error>& errors) const {
    // Nothing.
  }
};

template <typename FirstSubParser, typename... MoreSubParsers>
SequenceParser<typename ExtractParserType<FirstSubParser>::InputType,
               typename MaybeRef<FirstSubParser>::Type,
               typename MaybeRef<MoreSubParsers>::Type...>
sequence(FirstSubParser&& first, MoreSubParsers&&... rest) {
  return SequenceParser<typename ExtractParserType<FirstSubParser>::InputType,
                        typename MaybeRef<FirstSubParser>::Type,
                        typename MaybeRef<MoreSubParsers>::Type...>(
      MaybeRef<FirstSubParser>::from(first), MaybeRef<MoreSubParsers>::from(rest)...);
}


// -------------------------------------------------------------------
// RepeatedParser
// Output = Vector of output of sub-parser.

template <typename SubParser>
class RepeatedParser {
public:
  explicit RepeatedParser(SubParser&& subParser)
      : subParser(move(subParser)) {}

  ParseResult<std::vector<typename ExtractParserType<SubParser>::OutputType>> operator()(
      typename ExtractParserType<SubParser>::InputType& input) const {
    typedef std::vector<typename ExtractParserType<SubParser>::OutputType> Results;
    Results results;

    while (!input.atEnd()) {
      typename ExtractParserType<SubParser>::InputType subInput(input);
      subInput.setCommitted(false);
      auto subResult = subParser(subInput);

      if (subResult.isError()) {
        if (subInput.isCommitted()) {
          // Note that we intentionally don't swallow the committed bit.
          input = subInput;
          return ParseResult<Results>(move(subResult.errors));
        } else {
          break;
        }
      } else {
        // Note that we intentionally don't swallow the committed bit.
        input = subInput;
        results.push_back(move(subResult.value));
      }
    }

    return ParseResult<Results>(move(results));
  }

private:
  SubParser subParser;
};

template <typename SubParser>
RepeatedParser<typename MaybeRef<SubParser>::Type>
repeated(SubParser&& subParser) {
  return RepeatedParser<typename MaybeRef<SubParser>::Type>(
      MaybeRef<SubParser>::from(subParser));
}

// -------------------------------------------------------------------
// OptionalParser
// Output = Maybe<output of sub-parser>

template <typename SubParser>
class OptionalParser {
public:
  explicit OptionalParser(SubParser&& subParser)
      : subParser(move(subParser)) {}

  ParseResult<Maybe<typename ExtractParserType<SubParser>::OutputType>> operator()(
      typename ExtractParserType<SubParser>::InputType& input) const {
    typedef Maybe<typename ExtractParserType<SubParser>::OutputType> Result;

    typename ExtractParserType<SubParser>::InputType subInput(input);
    subInput.setCommitted(false);
    auto subResult = subParser(subInput);

    if (subResult.isError()) {
      if (subInput.isCommitted()) {
        // Note that we intentionally don't swallow the committed bit.
        input = subInput;
        return ParseResult<Result>(move(subResult.errors));
      } else {
        return ParseResult<Result>(Result());
      }
    } else {
      // Note that we intentionally don't swallow the committed bit.
      input = subInput;
      return ParseResult<Result>(Result(move(subResult.value)));
    }
  }

private:
  SubParser subParser;
};

template <typename SubParser>
OptionalParser<typename MaybeRef<SubParser>::Type>
optional(SubParser&& subParser) {
  return OptionalParser<typename MaybeRef<SubParser>::Type>(
      MaybeRef<SubParser>::from(subParser));
}

// -------------------------------------------------------------------
// OneOfParser
// All SubParsers must have same output type, which becomes the output type of the
// OneOfParser.

template <typename Input, typename Output, typename... SubParsers>
class OneOfParser;

template <typename Input, typename Output, typename FirstSubParser, typename... SubParsers>
class OneOfParser<Input, Output, FirstSubParser, SubParsers...> {
public:
  template <typename T, typename... U>
  explicit OneOfParser(T&& firstSubParser, U&&... rest)
      : first(std::forward<T>(firstSubParser)), rest(std::forward<U>(rest)...) {}

  ParseResult<Output> operator()(Input& input) const {
    {
      Input subInput(input);
      subInput.setCommitted(false);
      ParseResult<Output> firstResult = first(subInput);

      if (!firstResult.isError() || subInput.isCommitted()) {
        // MAYBE: Should we try parsing with "rest" in order to check for ambiguities?
        subInput.setCommitted(input.isCommitted());
        input = subInput;
        return move(firstResult);
      }
    }

    // Hoping for some tail recursion here...
    return rest(input);
  }

private:
  FirstSubParser first;
  OneOfParser<Input, Output, SubParsers...> rest;
};

template <typename Input, typename Output>
class OneOfParser<Input, Output> {
public:
  ParseResult<Output> operator()(Input& input) const {
    input.setBroken(true);
    return ParseResult<Output>(input.error("Syntax error."));
  }
};

template <typename FirstSubParser, typename... MoreSubParsers>
OneOfParser<typename ExtractParserType<FirstSubParser>::InputType,
            typename ExtractParserType<FirstSubParser>::OutputType,
            typename MaybeRef<FirstSubParser>::Type,
            typename MaybeRef<MoreSubParsers>::Type...>
oneOf(FirstSubParser&& first, MoreSubParsers&&... rest) {
  return OneOfParser<typename ExtractParserType<FirstSubParser>::InputType,
                     typename ExtractParserType<FirstSubParser>::OutputType,
                     typename MaybeRef<FirstSubParser>::Type,
                     typename MaybeRef<MoreSubParsers>::Type...>(
      MaybeRef<FirstSubParser>::from(first), MaybeRef<MoreSubParsers>::from(rest)...);
}

class CommitParser {
public:
  template <typename Input>
  ParseResult<Void> operator()(Input& input) const {
    input.setCommitted(true);
    return ParseResult<Void>(Void());
  }
};

template <>
struct ExtractParserType<CommitParser> {
  typedef Void OutputType;
};

CommitParser commit() {
  return CommitParser();
}

// -------------------------------------------------------------------
// TransformParser
// Output = Result of applying transform functor to input value.  If input is a tuple, it is
// unpacked to form the transformation parameters.

template <typename Output, typename SubParser, typename Transform>
class TransformParser {
public:
  explicit TransformParser(SubParser&& subParser, Transform&& transform)
      : subParser(move(subParser)), transform(move(transform)) {}

  ParseResult<Output> operator()(typename ExtractParserType<SubParser>::InputType& input) const {
    ParseResult<typename ExtractParserType<SubParser>::OutputType> subResult =
        subParser(input);
    if (subResult.isError()) {
      return ParseResult<Output>(move(subResult.errors));
    } else {
      return ParseResult<Output>(applyMaybeTuple<Output>(transform, move(subResult.value)));
    }
  }

private:
  SubParser subParser;
  Transform transform;
};

template <typename SubParser, typename Transform>
TransformParser<decltype(extractReturnType(&Transform::operator())),
                typename MaybeRef<SubParser>::Type,
                typename remove_reference<Transform>::type>
transform(SubParser&& subParser, Transform&& transform) {
  return TransformParser<decltype(extractReturnType(&Transform::operator())),
                         typename MaybeRef<SubParser>::Type,
                         typename remove_reference<Transform>::type>(
      MaybeRef<SubParser>::from(subParser), std::forward<Transform>(transform));
}

}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_PARSER_H_ */
