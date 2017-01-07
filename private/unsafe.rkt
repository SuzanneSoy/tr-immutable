#lang racket/base

;; TODO: make this a vector in the implementation, but make TR think it's a
;; list (via a contract?)
(provide (struct-out ivector))
(struct ivector (v) #:mutable)
