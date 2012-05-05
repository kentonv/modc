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
using std::string;
using std::vector;

#define decay(TYPE) typename std::decay<TYPE>::type

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
  const Element& current() { return *pos; }
  const Element& consume() { return *pos++; }
  void next() { ++pos; }

  Iterator getBest() { return std::max(pos, best); }

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
  Parser(Parser&& other): wrapper(move(other.wrapper)) {}
  Parser(OwnedPtr<ParserWrapper<Input, Output>> wrapper): wrapper(move(wrapper)) {}

  template <typename Other>
  Parser(Other&& other) {
    struct WrapperImpl: public ParserWrapper<Input, Output> {
      WrapperImpl(Other&& impl): impl(move(impl)) {};

      Maybe<Output> operator()(Input& input) const {
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

  // Always inline in the hopes that this allows branch prediction to kick in so the virtual call
  // doesn't hurt so much.
  inline Maybe<Output> operator()(Input& input) const __attribute__((always_inline)) {
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
// Output = Tuple of outputs of sub-parsers, eliding voids.

template <typename Input, typename... SubParsers> class SequenceParser;

template <typename Input, typename FirstSubParser, typename... SubParsers>
class SequenceParser<Input, FirstSubParser, SubParsers...> {
public:
  template <typename T, typename... U>
  explicit SequenceParser(T&& firstSubParser, U&&... rest)
      : first(std::forward<T>(firstSubParser)), rest(std::forward<U>(rest)...) {}

  Maybe<Tuple<typename ExtractParserType<FirstSubParser>::OutputType,
              typename ExtractParserType<SubParsers>::OutputType...>>
  operator()(Input& input) const {
    return parseNext(input);
  }

  template <typename... InitialParams>
  Maybe<Tuple<InitialParams...,
      typename ExtractParserType<FirstSubParser>::OutputType,
      typename ExtractParserType<SubParsers>::OutputType...>>
  parseNext(Input& input, InitialParams&&... initialParams) const {
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
  Maybe<Tuple<Params...>>
  parseNext(Input& input, Params&&... params) const {
    return Tuple<Params...>(std::forward<Params>(params)...);
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
      return nullptr;
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

template <typename Output, typename SubParser, typename Transform>
class TransformParser {
public:
  explicit TransformParser(SubParser&& subParser, Transform&& transform)
      : subParser(move(subParser)), transform(move(transform)) {}

  Maybe<Output> operator()(typename ExtractParserType<SubParser>::InputType& input) const {
    Maybe<typename ExtractParserType<SubParser>::OutputType> subResult =
        subParser(input);
    if (subResult) {
      return applyMaybeTuple<Output>(transform, move(*subResult));
    } else {
      return nullptr;
    }
  }

private:
  SubParser subParser;
  Transform transform;
};

template <typename SubParser, typename Transform>
TransformParser<decltype(extractReturnType(&Transform::operator())),
                typename MaybeRef<SubParser>::Type,
                decay(Transform)>
transform(SubParser&& subParser, Transform&& transform) {
  return TransformParser<decltype(extractReturnType(&Transform::operator())),
                         typename MaybeRef<SubParser>::Type,
                         decay(Transform)>(
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
