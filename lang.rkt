#lang racket/base

(provide (all-defined-out))

(require syntax/parse
         nanopass/base)

(define (unparse-id x)
  (syntax->datum x))

;;; Typical
(define-language Typical
  (entry Stmt)
  (terminals
   (syntax (stx))
   ((identifier (name param)) . => . unparse-id))
  (Stmt (stmt)
        ;; (expr : typ)
        (is-a? stx expr typ) => (expr is-a? typ)
        ;; (claim x : Nat)
        (claim stx name typ) => (claim name typ)
        ;; (define x z)
        (define stx name expr) => (define name expr)
        ;; (data Nat
        ;;   [z : Nat]
        ;;   [s : (-> Nat Nat)])
        ;; (data (List [A : U])
        ;;   [nil : (List A)]
        ;;   [cons : (-> A (List A) (List A))])
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
        name
        ;; polymorphism
        (name typ* ...)
        ;; arrow
        (-> typ* ... typ) => (typ* ... -> typ)))

(define-pass parse : * (stx) -> Typical ()
  (Stmt : * (stx) -> Stmt (stmt)
        (syntax-case stx (: data define claim)
          [(expr : typ)
           `(is-a? ,stx ,(Expr #'expr) ,(Type #'typ))]
          [(claim name typ)
           `(claim ,stx ,#'name ,(Type #'typ))]
          [(define name expr)
           `(define ,stx ,#'name ,(Expr #'expr))]
          [(data (name dep* ...) constructor* ...)
           `(data ,stx ,#'name (,(map Bind (syntax->list #'(dep* ...))) ...)
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [(data name constructor* ...)
           `(data ,stx ,#'name ()
                  ,(map Bind (syntax->list #'(constructor* ...))) ...)]
          [else (error 'syntax "unknown statement: ~a" stx)]))
  (Bind : * (stx) -> Bind (constructor)
        (syntax-case stx (:)
          [(name : typ)
           `(: ,#'name ,(Type #'typ))]
          [else (error 'syntax "bad binding: ~a" stx)]))
  (Expr : * (stx) -> Expr (expr)
        (syntax-case stx ()
          [(f arg* ...)
           `(app ,stx ,(Expr #'f) ,(map Expr (syntax->list #'(arg* ...))) ...)]
          [_
           (cond
             [(identifier? stx) stx]
             [else (error 'syntax "unknown expression: ~a" stx)])]))
  (Type : * (stx) -> Type (typ)
        (syntax-case stx (->)
          [(-> typ* ... typ)
           `(-> ,(map Type (syntax->list #'(typ* ...))) ... ,(Type #'typ))]
          [(name typ* ...)
           `(,(Type #'name) ,(map Type (syntax->list #'(typ* ...))) ...)]
          [_
           (cond
             [(identifier? stx) stx]
             [else (error 'syntax "unknown type: ~a" stx)])]))
  (Stmt stx))

(define-parser _ Typical)

(module+ test
  (require rackunit)

  (let ([p (λ (e) (unparse-Typical (parse e)))])
    (check-equal? (p #'(data (List [A : U])
                   [nil : (List A)]
                   [cons : (-> A (List A) (List A))]))
                  '(data List ([A : U])
                         [nil : (List A)]
                         [cons : (A (List A) -> (List A))]))
    (check-equal? (p #'((s z) : Nat)) '((s z) is-a? Nat))
    (check-equal? (p #'(claim a Nat)) '(claim a Nat))
    (check-equal? (p #'(define a z)) '(define a z))))
