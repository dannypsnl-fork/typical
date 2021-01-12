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
  (Stmt t)
  t)

(define-pass ty/infer : Typical (e) -> * ()
  (Clause : Clause (c sym* subst) -> * (t)
          [(+> ,stx ,pat* ... ,expr)
           (parameterize ([cur-env (make-env)])
             (for ([s sym*]
                   [p pat*])
               (unify stx (env/lookup s) (Pattern p subst)
                      #:subst subst
                      #:solve? #f))
             (Expr expr))])
  (Pattern : Pattern (p subst) -> * (t)
           [,name (env/lookup name)]
           [(intro ,name)
            (define cur-ty (freevar (gensym (syntax->datum name))))
            (env/bind name cur-ty)
            cur-ty]
           [(,name ,pat* ...)
            (define ret-ty (freevar (gensym 'ret)))
            (unify name
                   (env/lookup name) `(,@(map (λ (p) (Pattern p subst)) pat*) -> ,ret-ty)
                   #:subst subst
                   #:solve? #f)
            ret-ty])
  (Expr : Expr (e) -> * (t)
        [(match ,stx (,expr* ...) ,clause* ...)
         (define subst (make-subst))
         (parameterize ([cur-env (make-env)])
           (define sym* (generate-temporaries expr*))
           ; bind a generated symbols for match targets
           (for ([s sym*]
                 [e expr*])
             (env/bind s (ty/infer e)))
           (foldl (λ (c current-typ)
                    (unify stx
                           current-typ
                           (Clause c sym* subst)
                           #:subst subst
                           #:solve? #f))
                  ;; current-typ starts from a free variable
                  (freevar 'match)
                  clause*))]
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
              (replace-occur typ #:occur (subst-resolve subst stx)))]
           [else (wrong-syntax (exp->stx expr) "not appliable")])])
  (Expr e))

(define-pass pass:termination-check : Typical (t) -> * ()
  (Stmt : Stmt (t) -> * ()
        [(define ,stx ,name ,expr)
         (parameterize ([cur-env (make-env #f)])
           (nanopass-case
            (Typical Expr) expr
            [(λ ,stx (,param* ...) ,expr)
             ; 1. binding param* with random number as init level
             (for ([p param*])
               (env/bind p 0))
             ; 2. check expr
             (check-terminate expr name)]
            ; skip non-λ case since no recursion can happen
            [else (void)]))]
        ; skip non-define since no recursion can happen
        [else (void)])
  (Stmt t)
  t)

(define-pass check-terminate : Typical (e name) -> * ()
  (bind-level : Pattern (p level) -> * ()
           [,name (void)]
           [(intro ,name)
            (env/bind name level)]
           [(,name ,pat* ...)
            (for-each (λ (p) (bind-level p (- level 1))) pat*)])
  (infer : Expr (e) -> * (l)
               [(app ,stx ,expr ,expr* ...)
                (+ 1 (apply max (map infer expr*)))]
               [,name (env/lookup name)]
               [else 0])
  (check : Expr (e) -> * ()
               [(app ,stx ,expr ,expr* ...)
                (for-each check expr*)
                (when (equal? (syntax->datum name) (syntax-e expr))
                  ; a recursive call
                  (unless (and (ormap (λ (e) (< (infer e) 0)) expr*))
                    (raise-syntax-error 'termination "cannot prove termination"
                                        stx)))]
               [else (void)])
  (check-clause : Clause (c) -> * ()
          [(+> ,stx ,pat* ... ,expr)
           (parameterize ([cur-env (make-env)])
             (for-each (λ (p) (bind-level p 0)) pat*)
             (check expr))])
  (Expr : Expr (e) -> * ()
        [(match ,stx (,expr* ...) ,clause* ...)
         (for-each check-clause clause*)]
        [else (check e)])
  (Expr e))
