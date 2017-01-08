#lang typed/racket

(require tr-immutable
         typed/rackunit)
(check-pred (make-predicate (IVectorof Positive-Byte)) (ivector 1 2 3))

(check-equal? (with-output-to-string
               (λ ()
                 (display (ivector 1 2 3))))
              (with-output-to-string
               (λ ()
                 (display #(1 2 3)))))

(check-equal? (with-output-to-string
               (λ ()
                 (write (ivector 1 2 3))))
              (with-output-to-string
               (λ ()
                 (write #(1 2 3)))))

(check-equal? (with-output-to-string
               (λ ()
                 (print (ivector 1 2 3) (current-output-port) 0)))
              (with-output-to-string
               (λ ()
                 (print #(1 2 3) (current-output-port) 0))))

(check-equal? (with-output-to-string
               (λ ()
                 (print (ivector 1 2 3) (current-output-port) 1)))
              (with-output-to-string
               (λ ()
                 (print #(1 2 3) (current-output-port) 1))))