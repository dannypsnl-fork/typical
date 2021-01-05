#lang racket/base

(require racket/syntax
         racket/list
         racket/match
         nanopass/base
         "lang.rkt"
         "env.rkt"
         "core.rkt"
         "helper.rkt")

(define-pass expand-data : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(data ,stx ,name (,dependency* ...)
               ,constructor* ...)
         (env/bind name 'Type)
         (define dep* (make-immutable-hash
                       (map (λ (d)
                              (nanopass-case
                               (Typical Bind) d
                               [(: ,name ,typ)
                                (cons (syntax-e name) (freevar (syntax-e name) (convert-ty typ)))]))
                            dependency*)))
         (for ([c constructor*])
           (Bind c dep*))
         #f]
        [else t])
  (extract-ty : Bind (b) -> * ()
              [(: ,name ,typ)
               (convert-ty typ)])
  (Bind : Bind (b dep*) -> * ()
        [(: ,name ,typ)
         (define ty
           (if (hash-empty? dep*)
               (convert-ty typ)
               (replace-occur (convert-ty typ) #:occur dep*)))
         (env/bind name ty)])
  (Stmt t))

(define-pass ty/bind : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(claim ,stx ,name ,typ)
         (env/bind name (convert-ty typ))
         #f]
        [else t])
  (Stmt t))

(define-pass ty/check : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(is-a? ,stx ,expr ,typ)
         (unify stx (convert-ty typ) (ty/infer expr))
         #f]
        [(define ,stx ,name ,expr)
         (unify stx (env/lookup name) (ty/infer expr))
         #f]
        [else #t])
  (Stmt t))

(define-pass ty/infer : Typical (e) -> * ()
  (Expr : Expr (e) -> * (t)
        [,name (env/lookup name)]
        [(app ,stx ,expr ,expr* ...)
         (match (ty/infer expr)
           [`(-> ,typ* ... ,typ)
            (let ([subst (make-subst)])
              (for ([t typ*]
                    [e expr*])
                (unify (exp->stx e) t (ty/infer e) #:subst subst))
              (replace-occur typ #:occur (subst-resolve subst)))]
           [else (wrong-syntax expr "not appliable")])])
  (Expr e))

(module+ test
  (define final
    (compose-pass parse
                  expand-data
                  ty/bind
                  ty/check))

  (final #'(data Nat
                 [zero : Nat]
                 [suc : (Nat -> Nat)]))
  (final #'(data Bool
                 [false : Bool]
                 [true : Bool]))
  (final #'(a : Nat))
  (final #'(a = zero))
  (final #'(data (Pair [A : Type] [B : Type])
                 [cons : (A B -> (Pair A B))]))
  (final #'(b : (Pair Bool Nat)))
  (final #'(b = (cons true zero)))
  (final #'(data (List [A : Type])
                 [nil : (List A)]
                 [:: : (A (List A) -> (List A))]))
  (final #'(c : (List Nat)))
  (final #'(c = (:: (suc zero) (:: zero nil))))
  )
