#lang racket/base

(provide (all-from-out)
         (all-defined-out))

(require nanopass/base
         racket/match
         racket/syntax
         "lang.rkt"
         "core.rkt"
         "env.rkt")

(define-pass convert-ty : (Typical Type) (t) -> * ()
  (unparse-Typical t))

(define-pass pass:expand-data : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(data ,stx ,name (,dependency* ...)
               ,constructor* ...)
         (env/bind name 'Type)
         (define dep* (make-immutable-hash
                       (map (λ (d)
                              (nanopass-case
                               (Typical Bind) d
                               [(: ,name ,typ)
                                (cons (syntax-e name) (freevar (syntax-e name)))]))
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

(define-pass pass:ty/bind : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(claim ,stx ,name ,typ)
         (env/bind name (convert-ty typ))
         #f]
        [else t])
  (Stmt t))

(define-pass pass:ty/check : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(is-a? ,stx ,expr ,typ)
         (unify (exp->stx expr) (convert-ty typ) (ty/infer expr))]
        [(define ,stx ,name ,expr)
         (unify (exp->stx expr) (env/lookup name) (ty/infer expr))]
        [else (void)])
  (Stmt t))

(define-pass ty/infer : Typical (e) -> * ()
  (Expr : Expr (e) -> * (t)
        [,name (env/lookup name)]
        [(λ ,stx (,param* ...) ,expr)
         (parameterize ([cur-env (make-env)])
           (define param-typ* (map (λ (p) (freevar (syntax-e p))) param*))
           (for ([p param*]
                 [pt param-typ*])
             (env/bind p pt))
           `(,@param-typ*
             ->
             ; return type
             ,(ty/infer expr)))]
        [(app ,stx ,expr ,expr* ...)
         (match (ty/infer expr)
           [`(,typ* ... -> ,typ)
            (let ([subst (make-subst)]
                  [param-len (length typ*)]
                  [arg-len (length expr*)])
              (unless (= param-len arg-len)
                (raise-syntax-error 'semantic (format "arity mismatched, expected ~a, given ~a" param-len arg-len)
                                    stx
                                    (exp->stx expr)))
              (for ([t typ*]
                    [e expr*])
                (unify (exp->stx e) t (ty/infer e) #:subst subst))
              (replace-occur typ #:occur (subst-resolve subst)))]
           [else (wrong-syntax (exp->stx expr) "not appliable")])])
  (Expr e))
