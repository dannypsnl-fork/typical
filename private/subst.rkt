#|
* At the first step, multiple entries mapping was created, which means ?a -> ?b and ?a -> Nat existed at the same time was allowed(let ?a stands for a free variable with a tag a).
* Then in the second step, only replace the free with the bound, then leave free to free.
* In the third step, replace all entries with bound, then check any entries still is free?
  * yes, then meta(value) unsolvable
  * no, reverse key-value and bound
|#
#lang racket/base

(require racket/match
         racket/syntax
         racket/hash
         racket/string)

(struct subst (bound-map free-map) #:transparent)
(provide make-subst)
(define (make-subst)
  (subst (make-hash) (make-hash)))

(provide subst-set!)
(define (subst-set! stx subst key value)
  (match* {key value}
    [{(? freevar?) (? freevar?)}
     (let ([cur-bound? (hash-ref (subst-free-map subst) key #f)])
       (if cur-bound?
           (hash-set! (subst-free-map subst) key (cons value cur-bound?))
           (hash-set! (subst-free-map subst) key (list value))))]
    [{(? freevar?) _}
     (define cur-bound? (hash-ref (subst-bound-map subst) key #f))
     (when (and cur-bound? (not (equal? cur-bound? value)))
       (wrong-syntax stx (format "expected: `~a`, but got: `~a`" value cur-bound?)))
     (hash-set! (subst-bound-map subst) key value)]))

(provide subst-resolve)
(define (subst-resolve subst stx)
  (define resolved-map (make-hash))
  (hash-for-each (subst-free-map subst)
                 (λ (k v*)
                   (let ([bound? (hash-ref (subst-bound-map subst) k #f)])
                     (unless bound?
                       (raise-syntax-error 'substitute (format "~a unsolvable" v*)
                                           stx))
                     (for ([v v*])
                       (hash-set! resolved-map v bound?)))))
  (hash-union! resolved-map (subst-bound-map subst)
               #:combine/key (λ (k a b) a))
  resolved-map)

(provide freevar?)
(define (freevar? n)
  (and (symbol? n)
       (string-prefix? (symbol->string n) "?")))
