#lang scribble/manual
@require[@for-label[tr-immutable
                    typed/racket/base]]

@title{tr-immutable}
@author{georges}

@defmodule[tr-immutable]

This library implements immutable wrappers for @racket[vector] and
@racket[box], in a way that @racketmodname[typed/racket] is able to recognise.
This makes it possible to write @racket[(make-predicate (IVectorof Integer))]
in current versions of Typed/Racket.

@defform[#:kind "type"
         (IVectorof A)]{
 The type for immutable vectors containing elements of type @racket[A].
}

@defproc[(ivector [v : A] ...) (IVectorof A)]{
 The type for immutable vectors containing elements of type @racket[A].
}