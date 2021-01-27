#lang racket

(begin-for-syntax
  (require syntax/parse)

  (struct Value (val typ) #:transparent)

  (define (unify exp act)
    (unless (equal? exp act)
      (error 'semantic "type mismatched, expected: ~a, but got: ~a" exp act)))

  (define (infer x)
    (Value-typ x)))


(define-for-syntax Bool 'Bool)
(define-for-syntax true (Value 'true Bool))
(define-for-syntax false (Value 'false Bool))

(define-for-syntax Nat 'Nat)
(define-for-syntax zero (Value 'zero Nat))
(define-for-syntax suc
  (lambda (n)
    (unify Nat (infer n))
    (Value `(suc ,n) Nat)))

(begin-for-syntax
  (displayln (suc zero))

  ;(displayln (suc false))
  )
