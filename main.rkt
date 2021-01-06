#lang racket/base

(require nanopass/base
         "lang.rkt"
         "helper.rkt"
         "type-check.rkt")

(define-language L1
  (extends Typical)
  (Stmt ()
        (- (claim stx name typ)
           (data stx name (dependency* ...) constructor* ...))))

(define-pass pass:remove-type-related : Typical (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
        [(claim ,stx ,name ,typ) (error 'unreachable)]
        [(data ,stx ,name (,dependency* ...) ,constructor* ...)
         (error 'unreachable)])
  (Stmt s))

(define-pass pass:to-racket : L1 (s) -> * ()
  (Stmt : Stmt (s) -> * ()
        [(is-a? ,stx ,expr ,typ)
         expr]
        [(define ,stx ,name ,expr)
         `(define ,(syntax-e name) ,expr)])
  (Stmt s))

(module+ test
  (define (final e)
    (define parsed-e (parse e))
    (define stmt?
      ((compose-pass pass:expand-data
                     pass:ty/bind
                     pass:ty/check)
       parsed-e))
    (if stmt?
        (displayln
         ((compose-pass pass:remove-type-related
                        pass:to-racket)
          parsed-e))
        (void)))

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

  (final #'(d : (Nat -> Nat)))
  (final #'(d = (λ (n) n)))
  (final #'(e : Nat))
  (final #'(e = (d (suc (suc zero)))))
  )
