#lang racket/base

(provide compose-pass)

(require "lang.rkt")

(define (compose-pass . pass*)
  (λ (in)
    (let/cc return
      (foldl (λ (pass res)
               (let ([next (pass res)])
                 (unless next
                   (return))
                 next))
             in
             pass*))))
