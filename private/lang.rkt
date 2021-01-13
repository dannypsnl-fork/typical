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
        ;; (claim x : Nat)
        (claim stx name typ) => (claim name typ)
        ;; (define x z)
        (define stx name expr) => (define name expr)
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
          #:datum-literals (data :? : =)
          [(data (name dep* ...) constructor* ...)
           `(data ,stx ,#'name (,(map Bind (syntax->list #'(dep* ...))) ...)
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [(data name constructor* ...)
           `(data ,stx ,#'name ()
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [(expr :? typ)
           `(is-a? ,stx ,(Expr #'expr) ,(Type #'typ))]
          [(name : typ)
           `(claim ,stx ,#'name ,(Type #'typ))]
          [(name = expr)
           `(define ,stx ,#'name ,(Expr #'expr))]
          [else (wrong-syntax stx "invalid statement")]))
  (Bind : * (stx) -> Bind (constructor)
        (syntax-case stx (:)
          [(name* ... : typ)
           `(: ,(syntax->list #'(name* ...)) ... ,(Type #'typ))]
          [else (wrong-syntax stx "bad binding")]))
  (Expr : * (stx) -> Expr (expr)
        (syntax-parse stx
          #:datum-literals (λ match)
          [(λ (param* ...) exp)
           `(λ ,stx (,(map Expr (syntax->list #'(param* ...))) ...) ,(Expr #'exp))]
          [(match {exp* ...} clause* ...)
           `(match ,stx (,(map Expr (syntax->list #'(exp* ...))) ...)
              ,(map Clause (syntax->list #'(clause* ...))) ...)]
          [(match exp clause* ...)
           `(match ,stx (,(Expr #'exp))
              ,(map Clause (syntax->list #'(clause* ...))) ...)]
          [(f arg* ...)
           `(app ,stx ,(Expr #'f) ,(map Expr (syntax->list #'(arg* ...))) ...)]
          [_
           (cond
             [(identifier? stx) stx]
             [else (wrong-syntax stx "invalid expression")])]))
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
        (syntax-case stx (->)
          [(typ* ... -> typ)
           `(-> ,(map Type (syntax->list #'(typ* ...))) ... ,(Type #'typ))]
          [(typ* ...)
           `(,(map Type (syntax->list #'(typ* ...))) ...)]
          [_
           (cond
             [(identifier? stx) (syntax-e stx)]
             [else (wrong-syntax stx "invalid type")])]))
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
    (check-equal? (p #'(a : Nat)) '(claim a Nat))
    (check-equal? (p #'(a = z)) '(define a z))

    (check-equal? (p #'(id = (λ (n) n))) '(define id (λ (n) n)))
    (check-equal? (p #'(is-zero? = (λ (n)
                                     (match n
                                       [zero => true]
                                       [(suc ,n) => false]))))
                  '(define is-zero?
                     (λ (n)
                       (match {n}
                         [zero +> true]
                         [(suc (intro n)) +> false]))))))
