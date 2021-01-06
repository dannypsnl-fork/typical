#lang racket/base

(provide compose-pass)

(define (compose-pass . pass*)
  (λ (in)
    (let/cc return
      (foldl (λ (pass res)
               (let ([next (pass res)])
                 (unless next
                   (return #f))
                 next))
             in
             pass*))))
