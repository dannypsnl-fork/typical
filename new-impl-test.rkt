#lang s-exp "new-impl.rkt"

(data Empty)
(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc : (Nat -> Nat)])

(define empty-has-no-constructor : Empty)
(define b : Bool
  true)
(check b : Bool)

(define n : Nat
  (suc zero))
(check n : Nat)

(check (suc (suc (suc zero))) : Nat)
