#lang racket

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(require (for-syntax nanopass/base
                     racket/match
                     "lang.rkt"
                     "type-check.rkt"
                     "backend.rkt"))

(define-for-syntax (compose-pass* stx pass*)
  (let ([r (parse stx)]
        [todo* pass*])
    (for ([f todo*])
      (set! r (if r
                  (f r)
                  #f)))
    (if r r (void))))

(define-for-syntax (tc stx)
  (compose-pass* stx (list pass:expand-data
                           pass:ty/bind
                           pass:ty/check))
  (void))

(define-for-syntax (define-constructor b)
  (nanopass-case
   (L1 Bind) b
   [(: ,name ,typ)
    (with-syntax ([e (syntax->datum name)])
      (match (unparse-L1 typ)
        [`(,typ* ... -> ,typ)
         #`(define (#,name . arg*)
             `(e ,@arg*))]
        [else #`(define #,name 'e)]))]))
(define-for-syntax (pass:to-racket s)
  (nanopass-case
   (L1 Stmt) s
   [(data ,stx ,name (,dependency* ...) ,constructor* ...)
    #`(begin
        #,@(map define-constructor constructor*))]
   [(is-a? ,stx ,expr ,typ)
    ;; FIXME: expand expr
    expr]
   [(define ,stx ,name ,expr)
    `(define ,(syntax-e name) ,expr)
    ;; FIXME: expand expr
    (with-syntax ([e (unparse-L1 expr)])
      #`(define #,name 'e))]))
(define-for-syntax (expand-to-racket stx)
  (compose-pass* stx (list pass:remove-claim
                           pass:Typical->L1
                           pass:to-racket)))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(e* ...)
     ;;; type checking
     ; cdr to skip #%module-begin
     (for ([e (cdr (syntax->list #'(e* ...)))])
       (tc e))
     #`(#%module-begin
        #,@(map expand-to-racket (cdr (syntax->list #'(e* ...)))))]))

(module reader syntax/module-reader
  typical)
