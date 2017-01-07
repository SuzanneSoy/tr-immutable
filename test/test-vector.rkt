#lang typed/racket

(require tr-immutable
         typed/rackunit)
(check-pred (make-predicate (IVectorof Positive-Byte)) (ivector 1 2 3))
