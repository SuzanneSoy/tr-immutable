#lang racket/base

;; TODO: make this a vector in the implementation, but make TR think it's a
;; list (via a contract?)
(provide (struct-out ivector)
         ;(struct-out ivector2)
         ivector2
         ivector2?
         struct:ivector2
         (rename-out [vector->list ivector2-v])
         (rename-out [list->vector make-ivector2])
         (rename-out [vector new-ivector2]))
(define insp (make-inspector))
(struct ivector (v) #:mutable
  #:inspector insp)

;;;;;;;;;;;;;
(require (for-syntax racket/base
                     racket/struct-info))

(define (ivector2? v) (and (vector? v) (immutable? v)))

(define struct:ivector2 #f)
(define-syntax ivector2
  (make-struct-info
   (λ ()
     (list #f
           #'list->vector
           #'ivector2?
           (list #'vector->list)
           (list #f)
           #t))))