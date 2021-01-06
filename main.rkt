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
  (define final (compose-pass parse
                              pass:expand-data
                              pass:ty/bind
                              pass:ty/check))

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

  (final #'(data (Vec [A : Type] [N : Nat])
                 [vecnil : (Vec A zero)]
                 [vec:: : (A (Vec A N) -> (Vec A (suc N)))]))
  (final #'(f : (Vec Bool (suc zero))))
  (final #'(f = (vec:: false vecnil)))
  (final #'(g : (Vec Bool (suc (suc zero)))))
  (final #'(g = (vec:: true f)))
  )
