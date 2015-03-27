# C++%C

_aka %C, pronounced "mod-see"_

In 2012, while employed at Google, I spent many of my weekends and some 20% time designing a new programming language called "C++%C". The goal was to create a modern object-oriented systems language without garbage collection or other high-overhead runtime requirements. Or, in other words, to make something "like C++ but doesn't suck".

What I designed was actually a lot like [Rust](http://www.rust-lang.org/).

A very long design document can be found here:

https://docs.google.com/document/d/1WpUhy_d9RBtFICM4ioScRRFLGdzAs6wOckd4inv3AW8/edit

Much of the thought that went into this document later inspired the [KJ coding style](https://github.com/sandstorm-io/capnproto/blob/master/style-guide.md), used in C++ code developed at [Sandstorm.io](https://sandstorm.io) including [Cap'n Proto](https://capnproto.org).

This repository contains the beginning of an implementation, but it never reached any kind of working state (or anything near a working state, really).

This code is intended to be built with [ekam](https://github.com/sandstorm-io/ekam). It also reuses utility code from the Ekam code base, so you'll need to merge the `src` directories.
