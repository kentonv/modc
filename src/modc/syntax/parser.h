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
#include <assert.h>

#include "base/OwnedPtr.h"
#include "modc/errors.h"
#include "../Maybe.h"

namespace modc {
namespace parser {

using ekam::OwnedPtr;
using std::move;
using std::string;
using std::vector;

#define decay(TYPE) typename std::decay<TYPE>::type

// I don't understand std::tuple.
template <typename... T>
struct Tuple;

template <>
struct Tuple<> {
  Tuple() {}
};

template <typename First, typename... Rest>
struct Tuple<First, Rest...> {
  Tuple() {}

  Tuple(Tuple&& other): first(move(other.first)), rest(move(other.rest)) {}
  Tuple(const Tuple& other): first(other.first), rest(other.rest) {}
  Tuple(Tuple& other): first(other.first), rest(other.rest) {}

  template <typename First2, typename Rest2>
  explicit Tuple(First2&& first2, Rest2&& rest2)
      : first(std::forward<First2>(first2)),
        rest(std::forward<Rest2>(rest2)) {}

  First first;
  Tuple<Rest...> rest;
};

typedef Tuple<> Void;



template <typename T, typename U>
struct ConsTuple {
  typedef Tuple<decay(T), decay(U)> Type;
};

template <typename T, typename... U>
struct ConsTuple<T, Tuple<U...>> {
  typedef Tuple<decay(T), U...> Type;
};

template <typename T>
struct ConsTuple<T, Tuple<>> {
  typedef decay(T) Type;
};

template <typename... T>
struct MakeTuple;

template <>
struct MakeTuple<> {
  typedef Tuple<> Type;
};

template <typename T>
struct MakeTuple<T> {
  typedef decay(T) Type;
};

template <typename T, typename U, typename... V>
struct MakeTuple<T, U, V...> {
  typedef typename ConsTuple<T, typename MakeTuple<U, V...>::Type>::Type Type;
};

template <typename T, typename... U>
struct MakeTuple<Tuple<>, T, U...> {
  typedef typename MakeTuple<T, U...>::Type Type;
};

template <typename T, typename... U, typename V, typename... W>
struct MakeTuple<Tuple<T, U...>, V, W...> {
  typedef typename ConsTuple<T, typename MakeTuple<Tuple<U...>, V, W...>::Type>::Type Type;
};

template <typename T, typename U>
typename ConsTuple<T, U>::Type
inline consTuple(T&& t, U&& u) {
  return typename ConsTuple<T, U>::Type(
      std::forward<T>(t),
      Tuple<decay(U)>(std::forward<U>(u), Void()));
}

template <typename T, typename... U>
typename ConsTuple<T, Tuple<U...>>::Type
inline consTuple(T&& t, Tuple<U...>&& u) {
  return typename ConsTuple<T, Tuple<U...>>::Type(move(t), move(u));
}

template <typename T>
typename ConsTuple<T, Tuple<>>::Type
inline consTuple(T&& t, Tuple<>&& u) {
  return std::forward<T>(t);
}

typename MakeTuple<>::Type
inline tuple() {
  return Tuple<>();
}

template <typename T>
typename MakeTuple<T>::Type
inline tuple(T&& first) {
  return std::forward<T>(first);
}

template <typename T, typename U, typename... V>
typename MakeTuple<T, U, V...>::Type
inline tuple(T&& first, U&& second, V&&... rest) {
  return consTuple(std::forward<T>(first),
      tuple(std::forward<U>(second), std::forward<V>(rest)...));
}

template <typename T, typename... U>
typename MakeTuple<Tuple<>, T, U...>::Type
inline tuple(Tuple<>&&, T&& first, U&&... rest) {
  return tuple(std::forward<T>(first), std::forward<U>(rest)...);
}

template <typename T, typename... U>
typename MakeTuple<Tuple<>, T, U...>::Type
inline tuple(const Tuple<>&, T&& first, U&&... rest) {
  return tuple(std::forward<T>(first), std::forward<U>(rest)...);
}

template <typename T, typename... U, typename V, typename... W>
typename MakeTuple<Tuple<T, U...>, V, W...>::Type
inline tuple(Tuple<T, U...>&& first, V&& second, W&&... rest) {
  return consTuple(move(first.first),
      tuple(move(first.rest), std::forward<V>(second), std::forward<W>(rest)...));
}

template <typename T, typename... U, typename V, typename... W>
typename MakeTuple<Tuple<T, U...>, V, W...>::Type
inline tuple(const Tuple<T, U...>& first, V&& second, W&&... rest) {
  return consTuple(first.first,
      tuple(first.rest, std::forward<V>(second), std::forward<W>(rest)...));
}

template <typename T>
T any();

template <typename Func, typename T, typename... Params>
inline auto applyTuple(Func&& func, T&& t, Params&&... params) ->
decltype(func(std::forward<Params>(params)..., std::forward<T>(t))) {
  return func(std::forward<Params>(params)..., std::forward<T>(t));
}

template <typename Func, typename... Params>
inline auto applyTuple(Func&& func, Tuple<> t, Params&&... params) ->
decltype(func(std::forward<Params>(params)...)) {
  return func(std::forward<Params>(params)...);
}

template <typename Func, typename T, typename... U, typename... Params>
inline auto applyTuple(Func&& func, Tuple<T, U...>&& t, Params&&... params) ->
decltype(func(std::forward<Params>(params)..., any<T&&>(), any<U&&>()...)) {
  return applyTuple(std::forward<Func>(func), move(t.rest),
                    std::forward<Params>(params)..., move(t.first));
}

template <typename Func, typename T, typename... U, typename... Params>
inline auto applyTuple(Func&& func, const Tuple<T, U...>& t, Params&&... params) ->
decltype(func(std::forward<Params>(params)..., any<const T&>(), any<const U&>()...)) {
  return applyTuple(std::forward<Func>(func), t.rest,
                    std::forward<Params>(params)..., t.first);
}

// =======================================================================================

template <typename Element, typename Iterator>
class IteratorInput {
public:
  typedef Element ElementType;

  IteratorInput(Iterator begin, Iterator end)
      : parent(nullptr), pos(begin), end(end), best(begin) {}
  IteratorInput(IteratorInput& parent)
      : parent(&parent), pos(parent.pos), end(parent.end), best(parent.pos) {}
  ~IteratorInput() {
    if (parent != nullptr) {
      parent->best = std::max(std::max(pos, best), parent->best);
    }
  }

  void advanceParent() {
    parent->pos = pos;
  }

  bool atEnd() { return pos == end; }
  const Element& current() {
    assert(!atEnd());
    return *pos;
  }
  const Element& consume() {
    assert(!atEnd());
    return *pos++;
  }
  void next() {
    assert(!atEnd());
    ++pos;
  }

  Iterator getBest() { return std::max(pos, best); }

  Iterator getPosition() { return pos; }

private:
  IteratorInput* parent;
  Iterator pos;
  Iterator end;
  Iterator best;  // furthest we got with any sub-input

  IteratorInput(IteratorInput&&) = delete;
  IteratorInput& operator=(const IteratorInput&) = delete;
  IteratorInput& operator=(IteratorInput&&) = delete;
};


template <typename T>
struct ExtractParseFuncType;

template <typename I, typename O, typename Object>
struct ExtractParseFuncType<Maybe<O> (Object::*)(I&) const> {
  typedef I InputType;
  typedef typename I::ElementType ElementType;
  typedef O OutputType;
};

template <typename I, typename O, typename Object>
struct ExtractParseFuncType<Maybe<O> (Object::*)(I&)> {
  typedef I InputType;
  typedef typename I::ElementType ElementType;
  typedef O OutputType;
};

template <typename T>
struct ExtractParserType: public ExtractParseFuncType<decltype(&T::operator())> {};
template <typename T>
struct ExtractParserType<T&>: public ExtractParserType<T> {};
template <typename T>
struct ExtractParserType<T&&>: public ExtractParserType<T> {};
template <typename T>
struct ExtractParserType<const T>: public ExtractParserType<T> {};
template <typename T>
struct ExtractParserType<const T&>: public ExtractParserType<T> {};
template <typename T>
struct ExtractParserType<const T&&>: public ExtractParserType<T> {};

// =======================================================================================

template <typename Input, typename Output>
class ParserWrapper {
public:
  virtual ~ParserWrapper() {}

  typedef Input InputType;
  typedef typename Input::ElementType ElementType;
  typedef Output OutputType;

  virtual Maybe<Output> operator()(Input& input) const = 0;
  virtual OwnedPtr<ParserWrapper> clone() = 0;
};

template <typename Input, typename Output>
class Parser {
public:
  Parser(const Parser& other): wrapper(other.wrapper->clone()) {}
  Parser(Parser& other): wrapper(other.wrapper->clone()) {}
  Parser(const Parser&& other): wrapper(other.wrapper->clone()) {}
  Parser(Parser&& other): wrapper(move(other.wrapper)) {}
  Parser(OwnedPtr<ParserWrapper<Input, Output>> wrapper): wrapper(move(wrapper)) {}

  template <typename Other>
  Parser(Other&& other) {
    wrapper = ekam::newOwned<WrapperImpl<Other>>(move(other));
  }

  Parser& operator=(const Parser& other) { wrapper = other.wrapper->clone(); }
  Parser& operator=(Parser&& other) { wrapper = move(other.wrapper); }

  // Always inline in the hopes that this allows branch prediction to kick in so the virtual call
  // doesn't hurt so much.
  inline Maybe<Output> operator()(Input& input) const __attribute__((always_inline)) {
    return (*wrapper)(input);
  }

private:
  OwnedPtr<ParserWrapper<Input, Output>> wrapper;

  template <typename Other>
  struct WrapperImpl: public ParserWrapper<Input, Output> {
    WrapperImpl(Other&& impl): impl(move(impl)) {};
    ~WrapperImpl() {}

    Maybe<Output> operator()(Input& input) const {
      return impl(input);
    }

    OwnedPtr<ParserWrapper<Input, Output>> clone() {
      return ekam::newOwned<WrapperImpl>(*this);
    }

    Other impl;
  };
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

  Maybe<typename ExtractParserType<SubParser>::OutputType> operator()(
      typename ExtractParserType<SubParser>::InputType& input) const {
    return (*parser)(input);
  }

private:
  const SubParser* parser;
};

template <typename SubParser>
ParserRef<decay(SubParser)> ref(const SubParser& impl) {
  return ParserRef<decay(SubParser)>(impl);
}

template <typename T>
struct MaybeRef {
  typedef decay(T) Type;

  template <typename U>
  static Type from(U&& parser) {
    return static_cast<Type&&>(parser);
  }
};

template <typename T>
struct MaybeRef<T&> {
  typedef ParserRef<decay(T)> Type;

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

  virtual Maybe<Void> operator()(Input& input) const {
    if (input.atEnd() || input.current() != expected) {
      return nullptr;
    } else {
      input.next();
      return Void();
    }
  }

private:
  typename Input::ElementType expected;
};

template <typename Input>
ExactElementParser<Input> exactElement(typename Input::ElementType&& expected) {
  return ExactElementParser<decay(Input)>(move(expected));
}

// -------------------------------------------------------------------
// SequenceParser
// Output = Flattened Tuple of outputs of sub-parsers.

template <typename Input, typename... SubParsers> class SequenceParser;

template <typename Input, typename FirstSubParser, typename... SubParsers>
class SequenceParser<Input, FirstSubParser, SubParsers...> {
public:
  template <typename T, typename... U>
  explicit SequenceParser(T&& firstSubParser, U&&... rest)
      : first(std::forward<T>(firstSubParser)), rest(std::forward<U>(rest)...) {}

  auto operator()(Input& input) const ->
      Maybe<decltype(tuple(
          any<typename ExtractParserType<FirstSubParser>::OutputType>(),
          any<typename ExtractParserType<SubParsers>::OutputType>()...))> {
    return parseNext(input);
  }

  template <typename... InitialParams>
  auto parseNext(Input& input, InitialParams&&... initialParams) const ->
      Maybe<decltype(tuple(
          std::forward<InitialParams>(initialParams)...,
          any<typename ExtractParserType<FirstSubParser>::OutputType>(),
          any<typename ExtractParserType<SubParsers>::OutputType>()...))> {
    auto firstResult = first(input);
    if (firstResult) {
      return rest.parseNext(input, std::forward<InitialParams>(initialParams)...,
                            move(*firstResult));
    } else {
      return nullptr;
    }
  }

private:
  FirstSubParser first;
  SequenceParser<Input, SubParsers...> rest;
};

template <typename Input>
class SequenceParser<Input> {
public:
  Maybe<Void> operator()(Input& input) const {
    return parseNext(input);
  }

  template <typename... Params>
  auto parseNext(Input& input, Params&&... params) const ->
      Maybe<decltype(tuple(std::forward<Params>(params)...))> {
    return tuple(std::forward<Params>(params)...);
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

template <typename SubParser, bool atLeastOne>
class RepeatedParser {
public:
  explicit RepeatedParser(SubParser&& subParser)
      : subParser(move(subParser)) {}

  Maybe<vector<typename ExtractParserType<SubParser>::OutputType>> operator()(
      typename ExtractParserType<SubParser>::InputType& input) const {
    typedef vector<typename ExtractParserType<SubParser>::OutputType> Results;
    Results results;

    while (!input.atEnd()) {
      typename ExtractParserType<SubParser>::InputType subInput(input);
      auto subResult = subParser(subInput);

      if (subResult) {
        subInput.advanceParent();
        results.push_back(move(*subResult));
      } else {
        break;
      }
    }

    if (atLeastOne && results.empty()) {
      return nullptr;
    }

    return move(results);
  }

private:
  SubParser subParser;
};

template <typename SubParser>
RepeatedParser<typename MaybeRef<SubParser>::Type, false>
repeated(SubParser&& subParser) {
  return RepeatedParser<typename MaybeRef<SubParser>::Type, false>(
      MaybeRef<SubParser>::from(subParser));
}

template <typename SubParser>
RepeatedParser<typename MaybeRef<SubParser>::Type, true>
oneOrMore(SubParser&& subParser) {
  return RepeatedParser<typename MaybeRef<SubParser>::Type, true>(
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

  Maybe<Maybe<typename ExtractParserType<SubParser>::OutputType>> operator()(
      typename ExtractParserType<SubParser>::InputType& input) const {
    typedef Maybe<typename ExtractParserType<SubParser>::OutputType> Result;

    typename ExtractParserType<SubParser>::InputType subInput(input);
    auto subResult = subParser(subInput);

    if (subResult) {
      subInput.advanceParent();
      return Result(move(*subResult));
    } else {
      return Result(nullptr);
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

  Maybe<Output> operator()(Input& input) const {
    {
      Input subInput(input);
      Maybe<Output> firstResult = first(subInput);

      if (firstResult) {
        // MAYBE: Should we try parsing with "rest" in order to check for ambiguities?
        subInput.advanceParent();
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
  Maybe<Output> operator()(Input& input) const {
    return nullptr;
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

// -------------------------------------------------------------------
// TransformParser
// Output = Result of applying transform functor to input value.  If input is a tuple, it is
// unpacked to form the transformation parameters.

template <typename SubParser, typename Transform>
class TransformParser {
public:
  explicit TransformParser(SubParser&& subParser, Transform&& transform)
      : subParser(move(subParser)), transform(move(transform)) {}

  typedef typename ExtractParserType<SubParser>::InputType InputType;
  typedef decltype(any<InputType>().getPosition()) Position;
  typedef typename ExtractParserType<SubParser>::OutputType SubOutput;
  typedef decltype(applyTuple(any<Transform&>(), any<SubOutput&&>(),
                              any<std::pair<Position, Position>>())) Output;

  Maybe<Output> operator()(InputType& input) const {
    auto start = input.getPosition();
    Maybe<SubOutput> subResult = subParser(input);
    if (subResult) {
      return applyTuple(transform, move(*subResult), std::make_pair(start, input.getPosition()));
    } else {
      return nullptr;
    }
  }

private:
  SubParser subParser;
  Transform transform;
};

template <typename SubParser, typename Transform>
TransformParser<typename MaybeRef<SubParser>::Type, decay(Transform)>
transform(SubParser&& subParser, Transform&& transform) {
  return TransformParser<typename MaybeRef<SubParser>::Type, decay(Transform)>(
      MaybeRef<SubParser>::from(subParser), std::forward<Transform>(transform));
}

// -------------------------------------------------------------------
// AcceptIfParser
// Output = Same as SubParser

template <typename SubParser, typename Condition>
class AcceptIfParser {
public:
  explicit AcceptIfParser(SubParser&& subParser, Condition&& condition)
      : subParser(move(subParser)), condition(move(condition)) {}

  Maybe<typename ExtractParserType<SubParser>::OutputType>
  operator()(typename ExtractParserType<SubParser>::InputType& input) const {
    Maybe<typename ExtractParserType<SubParser>::OutputType> subResult = subParser(input);
    if (subResult && !condition(*subResult)) {
      subResult = nullptr;
    }
    return subResult;
  }

private:
  SubParser subParser;
  Condition condition;
};

template <typename SubParser, typename Condition>
AcceptIfParser<typename MaybeRef<SubParser>::Type, decay(Condition)>
acceptIf(SubParser&& subParser, Condition&& condition) {
  return AcceptIfParser<typename MaybeRef<SubParser>::Type, decay(Condition)>(
      MaybeRef<SubParser>::from(subParser), std::forward<Condition>(condition));
}

// -------------------------------------------------------------------
// EndOfInputParser
// Output = Void, only succeeds if at end-of-input

template <typename Input>
class EndOfInputParser {
public:
  Maybe<Void> operator()(Input& input) const {
    if (input.atEnd()) {
      return Void();
    } else {
      return nullptr;
    }
  }
};

template <typename T>
EndOfInputParser<T> endOfInput() {
  return EndOfInputParser<T>();
}

}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_PARSER_H_ */
