#lang typed/racket/base

(require typed/racket/unsafe)

(provide IVectorof
         (rename-out [new-ivector ivector]))

(unsafe-require/typed tr-immutable/private/unsafe
                      [#:struct (A) ivector ([v : (Listof A)])
                       #:constructor-name make-ivector
                       #:type-name IVectorof])

(: new-ivector (∀ (A) (→ A * (IVectorof A))))
(define (new-ivector . vs)
  (make-ivector vs))

;TODO: do a (with-sexp (var) body) which transforms to isexp on input, and back
; to sexp on output, to prevent any obvious leaks?