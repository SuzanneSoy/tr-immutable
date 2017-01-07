#lang typed/racket/base

(require typed/racket/unsafe)

(provide IVectorof
         IVectorof2
         (rename-out [new-ivector ivector])
         (rename-out [new-ivector2 ivector2])
         ivector2-v)

(unsafe-require/typed tr-immutable/private/unsafe
                      [#:struct (A) ivector ([v : (Listof A)])
                       #:type-name IVectorof]
                      [#:struct (A) ivector2 ([v : (Listof A)])
                       #:constructor-name make-ivector2
                       #:type-name IVectorof2]
                      [new-ivector2 (∀ (A) (→ A * (IVectorof2 A)))])

(: new-ivector (∀ (A) (→ A * (IVectorof A))))
(define (new-ivector . vs)
  (ivector vs))