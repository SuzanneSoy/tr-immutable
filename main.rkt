#lang typed/racket/base

(require typed/racket/unsafe)

(provide IVectorof
         (rename-out [new-ivector ivector]))

(unsafe-require/typed tr-immutable/private/unsafe
                      [#:struct (A) ivector ([v : (Listof A)])
                       #:type-name IVectorof])

(: new-ivector (∀ (A) (→ A * (IVectorof A))))
(define (new-ivector . vs)
  (ivector vs))