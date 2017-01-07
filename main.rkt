#lang typed/racket/base

(module unsafe racket/base
  ;; TODO: make this a vector in the implementation, but make TR think it's a
  ;; list (via a contract?)
  (provide (struct-out ivector))
  (struct ivector (v) #:mutable)
  )

(require typed/racket/unsafe)
(unsafe-require/typed 'unsafe
                      [#:struct (A) ivector ([v : (Listof A)])
                       #:type-name IVector])

(make-predicate (IVector Integer))