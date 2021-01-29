#lang s-exp "new-impl.rkt"

(data Empty)
(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc : (Nat . -> . Nat)])
(data (List [A : Type])
      [nil : (List A)]
      [∷ : (A (List A) . -> . (List A))])
(data (Pair [L R : Type])
      [cons : (L R . -> . (Pair L R))])


(app suc zero)

(define b : Bool
  true)
(check b : Bool)

(define n : Nat
  (app suc (app suc zero)))
n

(check nil : (List Nat))
(check (app ∷ false nil) : (List Bool))

(check (app cons zero false) : (Pair Nat Bool))
