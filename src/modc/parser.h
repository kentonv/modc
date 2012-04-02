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
#include <vector>

#include "base/OwnedPtr.h"
#include "modc/chars.h"
#include "modc/errors.h"

namespace modc {
namespace ast {

using ekam::OwnedPtr;
using std::move;

template <typename T>
class ErrorOr {
public:
  ErrorOr(ErrorOr&& other);
  ErrorOr(const ErrorOr& other);
  ErrorOr(errors::Error&& error);
  ErrorOr(std::vector<errors::Error>&& errors);
  ErrorOr(T&& value);

  bool isError();

  union {
    std::vector<errors::Error> errors;
    T value;
  };

private:
  bool isError_;
};

struct Void {};

// I don't understand std::tuple.
template <typename... T>
struct Tuple;

template <typename First, typename... Rest>
struct Tuple<First, Rest...> {
  template <typename First2, typename... Rest2>
  Tuple(First2&& first2, Rest2&&... rest2)
      : first(std::forward(first2)),
        rest(std::forward(rest2)...) {}

  First first;
  Tuple<Rest...> rest;

  template <typename Func, typename... InitialParams>
  void apply(Func&& f, InitialParams&&... initialParams) {
    rest.apply(f, std::forward(initialParams)..., first);
  }

  template <typename Func, typename... InitialParams>
  void applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    rest.applyAsRvalue(f, std::forward(initialParams)..., std::move(first));
  }
};

template <typename... Rest>
struct Tuple<Void, Rest...> {
  template <typename... Rest2>
  Tuple(Void first, Rest2&&... rest2)
      : rest(std::forward(rest2)...) {}

  Tuple<Rest...> rest;

  template <typename Func, typename... InitialParams>
  void apply(Func&& f, InitialParams&&... initialParams) {
    rest.apply(f, std::forward(initialParams)...);
  }

  template <typename Func, typename... InitialParams>
  void applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    rest.applyAsRvalue(f, std::forward(initialParams)...);
  }
};

template <>
struct Tuple<> {
  template <typename Func, typename... InitialParams>
  void apply(Func&& f, InitialParams&&... initialParams) {
    f(std::forward(initialParams)...);
  }

  template <typename Func, typename... InitialParams>
  void applyAsRvalue(Func&& f, InitialParams&&... initialParams) {
    f(std::forward(initialParams)...);
  }
};

template <typename... T>
Tuple<T...> tuple(T&&... elements) {
  return Tuple<T...>(elements...);
}





template <typename Input>
class TokenIterator {
public:
  bool atEnd();
  const Input& current();
  void next();

  void setBroken();
  bool isBroken();

  void commit();
  bool isCommitted();
};

template <typename InputType, typename OutputType>
class ParserWrapper {
public:
  typedef InputType Input;
  typedef OutputType Output;

  virtual ErrorOr<Output> parse(TokenIterator<Input>& input) = 0;
  virtual OwnedPtr<ParserWrapper> clone() = 0;
};

template <typename InputType, typename OutputType>
class Parser {
public:
  typedef InputType Input;
  typedef OutputType Output;

  Parser(const Parser& other): wrapper(other.wrapper->clone()) {}
  Parser(Parser&& other): wrapper(move(other.wrapper)) {}
  Parser(OwnedPtr<ParserWrapper<Input, Output>> wrapper): wrapper(move(wrapper)) {}

  Parser& operator=(const Parser& other) { wrapper = other.wrapper->clone(); }
  Parser& operator=(Parser&& other) { wrapper = move(other.wrapper); }

  ErrorOr<Output> parse(TokenIterator<Input>& input) {
    return wrapper.parse(input);
  }

private:
  OwnedPtr<ParserWrapper<Input, Output>> wrapper;
};

template <typename ParserImpl>
Parser<typename ParserImpl::InputType, typename ParserImpl::OutputType> wrap(ParserImpl&& impl) {
  typedef typename ParserImpl::InputType Input;
  typedef typename ParserImpl::OutputType Output;

  struct WrapperImpl: public ParserWrapper<Input, Output> {
    WrapperImpl(ParserImpl&& impl): impl(move(impl)) {};

    ErrorOr<Output> parse(TokenIterator<Input>& input) {
      return impl.parse(input);
    }

    OwnedPtr<ParserWrapper<Input, Output>> clone() {
      return ekam::newOwned<WrapperImpl>(*this);
    }

    ParserImpl impl;
  };

  return Parser<Input, Output>(ekam::newOwned<WrapperImpl>(move(impl)));
}



// Output = Void.
template <typename Input> class ExactTokenParser;
template <typename Input>
ExactTokenParser<Input> token(Input&& expected);

// Output = Input.
template <typename Input> class OneOfParser;
template <typename Input>
OneOfParser<Input> oneOf(std::vector<Input>&& options);

// Output = Tuple of outputs of sub-parsers, eliding voids.
template <typename Input, typename... SubParsers> class SequenceParser;
template <typename FirstSubParser, typename... MoreSubParsers>
SequenceParser<typename FirstSubParser::Input, FirstSubParser, MoreSubParsers...>
sequence(FirstSubParser&& first, MoreSubParsers&&... rest);

// All SubParsers must have same output type, which becomes the output type of the
// AlternativeParser.
template <typename Input, typename Output, typename... SubParsers> class AlternativeParser;
template <typename FirstSubParser, typename... MoreSubParsers>
AlternativeParser<typename FirstSubParser::Input, typename FirstSubParser::Output,
                  FirstSubParser, MoreSubParsers...>
alternative(FirstSubParser&& first, MoreSubParsers&&... rest);

// Output = Result of applying transform functor to input value.  If input is a tuple, it is
// unpacked to form the transformation parameters.
template <typename SubParser, typename Transform> class TransformParser;
template <typename SubParser, typename Transform>
TransformParser<SubParser, Transform> transform(SubParser&& subParser, Transform&& transform);




template <typename InputType>
class ExactTokenParser {
public:
  typedef InputType Input;
  typedef Void Output;

  ExactTokenParser(Input&& expected): expected(expected) {}

  virtual ErrorOr<Void> parse(TokenIterator<Input>& input) {
    if (input.atEnd() || input.current() != expected) {
      return ErrorOr<Void>(errors::error(-1, -1, "Expected: ", expected));
    } else {
      input.next();
      return ErrorOr<Void>(Void());
    }
  }

private:
  Input expected;
};

template <typename Input>
ExactTokenParser<typename std::remove_reference<Input>::type> token(Input&& expected) {
  return ExactTokenParser<typename std::remove_reference<Input>::type>(
      std::forward<Input>(expected));
}







template <typename InputType, typename FirstSubParser, typename... SubParsers>
class SequenceParser<InputType, FirstSubParser, SubParsers...> {
public:
  typedef InputType Input;
  typedef Tuple<typename FirstSubParser::Output, typename SubParsers::Output...> Output;

  template <typename T, typename... U>
  SequenceParser(T&& firstSubParser, U&&... rest)
      : first(std::forward(firstSubParser)), rest(std::forward(rest)...) {}

  template <typename... InitialParams>
  ErrorOr<Tuple<InitialParams..., typename FirstSubParser::Output, typename SubParsers::Output...>>
  parse(TokenIterator<Input>& input, InitialParams&&... initialParams) {
    typedef ErrorOr<Tuple<InitialParams..., typename FirstSubParser::Output,
        typename SubParsers::Output...>> Result;

    ErrorOr<typename FirstSubParser::Output> firstResult = first.parse(input);
    if (firstResult.isError()) {
      if (!input.isBroken()) {
        rest.parseForErrors(input, firstResult.errors);
      }
      return Result(move(firstResult.errors));
    } else {
      return rest.parse(input, move(firstResult.value), std::forward(initialParams)...);
    }
  }

  void parseForErrors(TokenIterator<Input>& input, std::vector<errors::Error>& errors) {
    ErrorOr<typename FirstSubParser::Output> firstResult = first.parse(input);
    if (firstResult.isError()) {
      errors.push_back(move(firstResult.errors));
      if (input.isBroken()) {
        return;
      }
    }

    rest.parseForErrors(input, errors);
  }

private:
  FirstSubParser first;
  SequenceParser<Input, SubParsers...> rest;
};

template <typename InputType>
class SequenceParser<InputType> {
public:
  typedef InputType Input;
  typedef Tuple<> Output;

  template <typename... Params>
  ErrorOr<Tuple<Params...>>
  parse(TokenIterator<Input>& input, Params&&... params) {
    return ErrorOr<Tuple<Params...>>(Tuple<Params...>(params...));
  }

  void parseForErrors(TokenIterator<Input>& input, std::vector<errors::Error>& errors) {
    // Nothing.
  }
};

template <typename FirstSubParser, typename... MoreSubParsers>
SequenceParser<typename FirstSubParser::Input, FirstSubParser, MoreSubParsers...>
sequence(FirstSubParser&& first, MoreSubParsers&&... rest) {
  return SequenceParser<typename FirstSubParser::Input, FirstSubParser, MoreSubParsers...>(
      std::forward(first), std::forward(rest)...);
}







template <typename InputType, typename OutputType, typename FirstSubParser, typename... SubParsers>
class AlternativeParser<InputType, OutputType, FirstSubParser, SubParsers...> {
public:
  typedef InputType Input;
  typedef OutputType Output;

  template <typename T, typename... U>
  AlternativeParser(T&& firstSubParser, U&&... rest)
      : first(std::forward(firstSubParser)), rest(std::forward(rest)...) {}

  ErrorOr<Output> parse(TokenIterator<Input>& input) {
    TokenIterator<Input> subInput(input);
    ErrorOr<Output> firstResult = first.parse(subInput);

    if (subInput.isCommitted()) {
      if (rest.tryParse(input)) {
        return ErrorOr<Output>(errors::error(-1, -1, "Ambiguous."));
      } else {
        input = subInput;
        return move(firstResult);
      }
    } else {
      return rest.parse(input);
    }
  }

  bool tryParse(TokenIterator<Input>& input) {
    TokenIterator<Input> subInput(input);
    first.parse(subInput);

    if (subInput.isCommitted()) {
      return true;
    } else {
      return rest.tryParse(input);
    }
  }

private:
  FirstSubParser first;
  AlternativeParser<Input, Output, SubParsers...> rest;
};

template <typename InputType, typename OutputType>
class AlternativeParser<InputType, OutputType> {
public:
  typedef InputType Input;
  typedef OutputType Output;

  ErrorOr<Output> parse(TokenIterator<Input>& input) {
    return ErrorOr<Output>(-1, -1, "Syntax error.");
  }

  bool tryParse(TokenIterator<Input>& input) {
    return false;
  }
};

template <typename FirstSubParser, typename... MoreSubParsers>
AlternativeParser<typename FirstSubParser::Input, typename FirstSubParser::Output,
                  FirstSubParser, MoreSubParsers...>
alternative(FirstSubParser&& first, MoreSubParsers&&... rest) {
  return AlternativeParser<typename FirstSubParser::Input, FirstSubParser, MoreSubParsers...>(
      std::forward(first), std::forward(rest)...);
}


template <typename T>
T any();

template <typename SubParser, typename Transform>
class TransformParser {
public:
  typedef typename SubParser::Input Input;
  typedef decltype(any<Transform>()(any<typename SubParser::Output>()).value) Output;

  TransformParser(SubParser&& subParser, Transform&& transform)
      : subParser(move(subParser)), transform(move(transform)) {}

  ErrorOr<Output> parse(TokenIterator<Input>& input) {
    ErrorOr<Input> subResult = subParser.parse(input);
    if (subResult.isError()) {
      return ErrorOr<Output>(subResult.error);
    } else {
      return transform(subResult.value);
    }
  }

private:
  SubParser subParser;
  Transform transform;
};

template <typename SubParser, typename Transform>
TransformParser<typename std::remove_reference<SubParser>::type,
                typename std::remove_reference<Transform>::type>
transform(SubParser&& subParser, Transform&& transform) {
  return TransformParser<typename std::remove_reference<SubParser>::type,
                         typename std::remove_reference<Transform>::type>(
      std::forward(subParser), std::forward(transform));
}




}  // namespace ast
}  // namespace modc

#endif /* KENTONSCODE_MODC_PARSER_H_ */
