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
        (: name typ) => (name : typ))
  (Expr (expr)
        name
        ;; application
        (app stx expr expr* ...) => (expr expr* ...))
  (Type (typ)
        ;; base
        base
        ;; polymorphism
        (typ* ...)
        ;; arrow
        (-> typ* ... typ) => (typ* ... -> typ)))

(define-pass parse : * (stx) -> Typical ()
  (Stmt : * (stx) -> Stmt (stmt)
        (syntax-case stx (:? data : =)
          [(expr :? typ)
           `(is-a? ,stx ,(Expr #'expr) ,(Type #'typ))]
          [(name : typ)
           `(claim ,stx ,#'name ,(Type #'typ))]
          [(name = expr)
           `(define ,stx ,#'name ,(Expr #'expr))]
          [(data (name dep* ...) constructor* ...)
           `(data ,stx ,#'name (,(map Bind (syntax->list #'(dep* ...))) ...)
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [(data name constructor* ...)
           `(data ,stx ,#'name ()
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [else (wrong-syntax stx "invalid statement")]))
  (Bind : * (stx) -> Bind (constructor)
        (syntax-case stx (:)
          [(name : typ)
           `(: ,#'name ,(Type #'typ))]
          [else (wrong-syntax stx "bad binding")]))
  (Expr : * (stx) -> Expr (expr)
        (syntax-case stx ()
          [(f arg* ...)
           `(app ,stx ,(Expr #'f) ,(map Expr (syntax->list #'(arg* ...))) ...)]
          [_
           (cond
             [(identifier? stx) stx]
             [else (wrong-syntax stx "invalid expression")])]))
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
        [(app ,stx ,expr ,expr* ...) stx])
  (Expr e))

(define-pass convert-ty : (Typical Type) (t) -> * ()
  (conv : Type (t) -> * ()
        [(-> ,typ* ... ,typ)
         `(-> ,@(map conv typ*) ,(conv typ))]
        [(,typ* ...)
         (map conv typ*)]
        [,base
         base])
  (conv t))

(module+ test
  (require rackunit)

  (define-parser _ Typical)
  (let ([p (λ (e) (unparse-Typical (parse e)))])
    (check-equal? (p #'(data (List [A : Type])
                             [nil : (List A)]
                             [:: : (A (List A) -> (List A))]))
                  '(data List ([A : Type])
                         [nil : (List A)]
                         [:: : (A (List A) -> (List A))]))
    (check-equal? (p #'((s z) : Nat)) '((s z) is-a? Nat))
    (check-equal? (p #'(claim a Nat)) '(claim a Nat))
    (check-equal? (p #'(define a z)) '(define a z))))
