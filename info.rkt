#lang info
(define collection "tr-immutable")
(define deps '("base"
               "rackunit-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "typed-map-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-doc"))
(define scribblings '(("scribblings/tr-immutable.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(georges))
