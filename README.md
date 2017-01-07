[![Build Status,](https://img.shields.io/travis/jsmaniac/tr-immutable/master.svg)](https://travis-ci.org/jsmaniac/tr-immutable)
[![Coverage Status,](https://img.shields.io/codecov/c/github/jsmaniac/tr-immutable/master.svg)](https://codecov.io/gh/jsmaniac/tr-immutable/branch/master)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/tr-immutable)
[![Online Documentation.](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/tr-immutable/)

tr-immutable
============

Immutable alternatives to Vector, Box, Sexp, Syntax-E and Syntax for
Typed/Racket.

This library wraps vectors and boxes so that Typed/Racket recognises them as
immutable. This means that `(make-predicate (IVectorof Integer))` works,
whereas `(make-predicate (Vectorof Integer))` is rejected by current versions
of Typed/Racket (due to the fact that vectors are mutable).

This should make it possible to write code operating on syntax objects
(containing these immutable vectors), so that typed macros can be written.

There seem to be plans for support for immutable vectors in Typed/Racket at
some point in the future. When this happens, this library will be changed to
rely on the official immutable vectors.
