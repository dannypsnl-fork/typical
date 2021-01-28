#lang s-exp "new-impl.rkt"

(data Empty)
(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc : (Nat . -> . Nat)])

false
(app suc zero)

(define b : Bool
  true)
(check b : Bool)

(define n : Nat
  (app suc (app suc zero)))
n
