#lang racket

(provide (all-defined-out)
         freevar
         make-subst
         subst-resolve)

(require racket/syntax
         "subst.rkt")

(define (full-expand exp occurs)
  (match exp
    [`(,e* ...)
     (map (λ (e) (full-expand e occurs)) e*)]
    [v (let ([new-v (hash-ref occurs v #f)])
         (if new-v (full-expand new-v occurs) v))]))
(define (unify stx exp act #:subst [subst (make-subst)])
  (match* {exp act}
    [{(? freevar?) _} (subst-set! stx subst exp act)]
    [{_ (? freevar?)} (unify stx act exp #:subst subst)]
    [{`(,t1* ...) `(,t2* ...)}
     (map (λ (t1 t2) (unify stx t1 t2 #:subst subst))
          t1* t2*)]
    [{_ _} (unless (equal? exp act)
             (wrong-syntax stx (format "expected: ~a, but got: ~a" exp act)))])
  (full-expand exp (subst-resolve subst)))
(define (replace-occur target #:occur occurs)
  (match target
    [`(,e* ...)
     (map (λ (e) (replace-occur e #:occur occurs)) e*)]
    [v (let ([new-v (hash-ref occurs v #f)])
         (if new-v new-v v))]))
