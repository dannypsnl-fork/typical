#lang racket/base

(require nanopass/base
         "lang.rkt"
         "helper.rkt")

(define base-type-env (make-hash))
(define type-env (make-hash))

(define-pass expand-data : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(data ,stx ,name (,dependency* ...)
               ,constructor* ...)
         (hash-set! base-type-env (syntax-e name) name)
         (for ([c constructor*])
           (Bind c))
         #f]
        [else t])
  (Bind : Bind (b) -> * ()
        [(: ,name ,typ)
         (hash-set! type-env (syntax-e name) typ)])
  (Stmt t))

(define-pass ty/bind : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(claim ,stx ,name ,typ)
         (hash-set! type-env (syntax-e name) typ)
         #f]
        [else t])
  (Stmt t))

(define (ty-eq? expect actual)
  (define e (unparse-Typical expect))
  (define a (unparse-Typical actual))
  (unless (equal? e a)
    (error 'semantic "~a != ~a" e a)))

(define-pass ty/infer : Typical (e) -> (Typical Type) ()
  (Expr : Expr (e) -> Type (t)
        [,name (hash-ref type-env (syntax-e name))]
        [(app ,stx ,expr ,expr* ...)
         (nanopass-case
          (Typical Type) (ty/infer expr)
          [(-> ,typ* ... ,typ)
           (for ([t typ*]
                 [e expr*])
             (ty-eq? t (ty/infer e)))
           typ]
          [else (error 'semantic "~a not appliable" stx)])])
  (Expr e))

(define-pass ty/check : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(is-a? ,stx ,expr ,typ)
         (ty-eq? typ (ty/infer expr))
         #f]
        [(define ,stx ,name ,expr)
         (ty-eq? (hash-ref type-env (syntax-e name)) (ty/infer expr))
         #f]
        [else #t])
  (Stmt t))

(module+ test
  (define final
    (compose-pass parse
                  expand-data
                  ty/bind
                  ty/check))

  (final #'(data Nat
                 [z : Nat]
                 [s : (-> Nat Nat)]))
  (final #'(data Bool
                 [false : Bool]
                 [true : Bool]))
  (final #'(data (Pair [A : U] [B : U])
                 [cons : (-> A B (Pair A B))]))
  (final #'((s (s z)) : Nat)))
