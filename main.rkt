#lang racket

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(require (for-syntax nanopass/base
                     "lang.rkt"
                     "type-check.rkt"))

(define-for-syntax (tc stx)
  (let ([todo* (list pass:expand-data
                     pass:ty/bind
                     pass:ty/check)]
        [r (parse stx)])
    (for ([f todo*])
      (set! r (if r
                  (f r)
                  #f)))
    (void)))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(e* ...)
     (for ([e (cdr (syntax->list #'(e* ...)))])
       (tc e))
     #'(#%module-begin
        "hello world")]))

(define-syntax (top-interaction stx)
  stx)

(module reader syntax/module-reader
  typical)
