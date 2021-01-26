#lang racket/base

(provide (all-defined-out))

(require racket/syntax
         syntax/parse
         nanopass/base)

(define (unparse-id x)
  (syntax->datum x))

;;; Typical
(define-language Typical
  (entry Stmt)
  (terminals
   (syntax (stx))
   (symbol (base))
   ((identifier (name param)) . => . unparse-id))
  (Stmt (stmt)
        ;; (expr : typ)
        (is-a? stx expr typ) => (expr is-a? typ)
        ;; (define x : Nat
        ;;   z)
        (define stx name typ expr) => (define name typ expr)
        (data stx name (dependency* ...)
              constructor* ...) => (data name (dependency* ...) constructor* ...))
  (Bind (dependency constructor)
        (: name* ... typ) => (name* ... : typ))
  (Expr (expr)
        name
        ;; abstraction
        (λ stx (param* ...) expr) => (λ (param* ...) expr)
        (match stx (expr* ...) clause* ...) => (match (expr* ...) clause* ...)
        ;; application
        (app stx expr expr* ...) => (expr expr* ...))
  (Clause (clause)
          (+> stx pat* ... expr) => (pat* ... +> expr))
  (Pattern (pat)
           name
           (intro name)
           (name pat* ...))
  (Type (typ)
        base
        (typ* ...)
        (-> typ* ... typ) => (typ* ... -> typ)))

(define-pass parse : * (stx) -> Typical ()
  (Stmt : * (stx) -> Stmt (stmt)
        (syntax-parse stx
          #:datum-literals (data :? : define)
          [(data (name:id dep* ...) constructor* ...)
           `(data ,stx ,#'name (,(map Bind (syntax->list #'(dep* ...))) ...)
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [(data name:id constructor* ...)
           `(data ,stx ,#'name ()
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [(data name . rest)
           (parameterize ([current-syntax-context stx])
             (wrong-syntax #'name "expected an identifier"))]
          [(expr:expr :? typ)
           `(is-a? ,stx ,(Expr #'expr) ,(Type #'typ))]
          [(define name:id : typ expr:expr)
           `(define ,stx ,#'name ,(Type #'typ) ,(Expr #'expr))]
          [(define name:id . rest)
           (parameterize ([current-syntax-context stx])
             (wrong-syntax #'rest "expected `:`"))]
          [else (wrong-syntax stx "invalid statement")]))
  (Bind : * (stx) -> Bind (constructor)
        (syntax-parse stx
          #:datum-literals (:)
          [(name*:id ... : typ)
           `(: ,(syntax->list #'(name* ...)) ... ,(Type #'typ))]
          [else (wrong-syntax stx "bad binding")]))
  (Expr : * (stx) -> Expr (expr)
        (syntax-parse stx
          #:datum-literals (λ match)
          [(λ (param*:id ...) exp:expr)
           `(λ ,stx (,(map Expr (syntax->list #'(param* ...))) ...) ,(Expr #'exp))]
          [(match {exp* ...} clause* ...)
           `(match ,stx (,(map Expr (syntax->list #'(exp* ...))) ...)
              ,(map Clause (syntax->list #'(clause* ...))) ...)]
          [(match exp:expr clause* ...)
           `(match ,stx (,(Expr #'exp))
              ,(map Clause (syntax->list #'(clause* ...))) ...)]
          [(f:expr arg*:expr ...)
           `(app ,stx ,(Expr #'f) ,(map Expr (syntax->list #'(arg* ...))) ...)]
          [e:id #'e]
          [else (wrong-syntax stx "invalid expression")]))
  (Clause : * (stx) -> Clause (clause)
          (syntax-parse stx
            #:datum-literals (=>)
            [(pat* ... => exp)
             `(+> ,stx ,(map Pattern (syntax->list #'(pat* ...))) ... ,(Expr #'exp))]
            [else (wrong-syntax stx "invalid clause")]))
  (Pattern : * (stx) -> Pattern (pat)
           (syntax-parse stx
             #:datum-literals (unquote)
             [(unquote name:id)
              `(intro ,#'name)]
             ; (suc ,n)
             ; (suc (suc ,n))
             ; (suc zero)
             [(name:id pat* ...)
              `(,#'name ,(map Pattern (syntax->list #'(pat* ...))) ...)]
             [name:id #'name]
             [else (wrong-syntax stx "invalid pattern")]))
  (Type : * (stx) -> Type (typ)
        (syntax-parse stx
          #:datum-literals (->)
          [(typ* ... -> typ)
           `(-> ,(map Type (syntax->list #'(typ* ...))) ... ,(Type #'typ))]
          [(typ* ...)
           `(,(map Type (syntax->list #'(typ* ...))) ...)]
          [ty:id (syntax->datum #'ty)]
          [else (wrong-syntax stx "invalid type")]))
  (Stmt stx))

(define-pass exp->stx : Typical (e) -> * ()
  (Expr : Expr (e) -> * (t)
        [,name name]
        [(λ ,stx (,param* ...) ,expr) stx]
        [(match ,stx (,expr* ...) ,clause* ...) stx]
        [(app ,stx ,expr ,expr* ...) stx])
  (Expr e))

(module+ test
  (require rackunit)

  (let ([p (λ (e) (unparse-Typical (parse e)))])
    (check-equal? (p #'(data (List [A : Type])
                             [nil : (List A)]
                             [:: : (A (List A) -> (List A))]))
                  '(data List ([A : Type])
                         [nil : (List A)]
                         [:: : (A (List A) -> (List A))]))
    (check-equal? (p #'((s z) :? Nat)) '((s z) is-a? Nat))
    (check-equal? (p #'(define a : Nat z)) '(define a Nat z))
    (check-equal? (p #'(define id : (Nat -> Nat)
                         (λ (n) n)))
                  '(define id (Nat -> Nat) (λ (n) n)))
    (check-equal? (p #'(define is-zero? : (Nat -> Bool)
                         (λ (n)
                           (match n
                             [zero => true]
                             [(suc ,n) => false]))))
                  '(define is-zero?
                     (Nat -> Bool)
                     (λ (n)
                       (match {n}
                         [zero +> true]
                         [(suc (intro n)) +> false]))))))
