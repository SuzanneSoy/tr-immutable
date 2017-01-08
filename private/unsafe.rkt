#lang racket/base

(require (for-syntax racket/base
                     racket/contract))
;; TODO: make this a vector in the implementation, but make TR think it's a
;; list (via a contract?)
(provide (except-out (struct-out ivector) make-ivector*)
         make-ivector)
(struct ivector (v) #:mutable
  #:constructor-name make-ivector*
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc iv port mode)
     (case mode
       [(#t) (write (raw-ivector-v iv) port)]
       [(#f) (display (raw-ivector-v iv) port)]
       [else (print (raw-ivector-v iv) port mode)]))])

(define raw-ivector-v? (make-parameter #f))
(define (raw-ivector-v iv)
  (parameterize ([raw-ivector-v? #t])
    (ivector-v iv)))

(define (make-ivector v)
  (impersonate-struct (make-ivector* (apply vector-immutable v))
                      ivector-v
                      (λ (self val)
                        (if (raw-ivector-v?)
                            val
                            (vector->list val)))
                      set-ivector-v!
                      (λ (self val)
                        (error "vector is immutable!"))))