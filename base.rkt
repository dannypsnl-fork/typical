#lang racket

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [@#%module-begin #%module-begin]))

(require (for-syntax nanopass/base
                     racket/match
                     "private/lang.rkt"
                     "private/type-check.rkt"))

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
                           pass:termination-check
                           pass:ty/bind
                           pass:ty/check))
  (void))

(define-for-syntax (define-constructor b)
  (nanopass-case
   (Typical Bind) b
   [(: ,name ,typ)
    (with-syntax ([e (syntax->datum name)])
      (match (unparse-Typical typ)
        [`(,typ* ... -> ,typ)
         #`(define (#,name . arg*)
             `(e ,@arg*))]
        [else #`(define #,name 'e)]))]))
(define-for-syntax (expand-pattern p)
  (nanopass-case
   (Typical Pattern) p
   [,name #`,(== #,name)]
   [(intro ,name) #`,#,name]
   [(,name ,pat* ...)
    #`(#,name #,@(map expand-pattern pat*))]))
(define-for-syntax (expand-clause c)
  (nanopass-case
   (Typical Clause) c
   [(+> ,stx ,pat* ... ,expr)
    #`[{#,@(map (λ (pat) #``#,(expand-pattern pat)) pat*)}
       #,(expand-expr expr)]]))
(define-for-syntax (expand-expr e)
  (nanopass-case
   (Typical Expr) e
   [(match ,stx (,expr* ...) ,clause* ...)
    #`(match* {#,@expr*}
        #,@(map expand-clause clause*))]
   [(λ ,stx (,param* ...) ,expr)
    #`(λ (#,@param*) #,(expand-expr expr))]
   [(app ,stx ,expr ,expr* ...)
    #`(#,(expand-expr expr) #,@(map expand-expr expr*))]
   [,name name]))
(define-for-syntax (pass:to-racket s)
  (nanopass-case
   (Typical Stmt) s
   [(data ,stx ,name (,dependency* ...) ,constructor* ...)
    ;; FIXME: make type function when dependency existed
    #`(begin
        (define #,name '#,(syntax->datum name))
        #,@(map define-constructor constructor*))]
   [(is-a? ,stx ,expr ,typ)
    (expand-expr expr)]
   [(define ,stx ,name ,typ ,expr)
    #`(define #,name #,(expand-expr expr))]))
(define-for-syntax (expand-to-racket stx)
  (compose-pass* stx (list pass:to-racket)))

(define-syntax (@#%module-begin stx)
  (syntax-case stx ()
    [(e* ...)
     ;;; type checking
     ; cdr to skip #%module-begin
     (for ([e (cdr (syntax->list #'(e* ...)))])
       (tc e))
     #`(#%module-begin
        #,@(map expand-to-racket (cdr (syntax->list #'(e* ...)))))]))

(module reader syntax/module-reader
  typical/base)
