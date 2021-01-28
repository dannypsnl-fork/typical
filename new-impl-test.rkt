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
(data (Vec [A : Type] [LEN : Nat])
      [vecnil : (Vec A zero)]
      [vec∷ : (A (Vec A LEN) . -> . (Vec A (suc LEN)))])

(app suc zero)

(define b : Bool
  true)
(check b : Bool)

(define n : Nat
  (app suc (app suc zero)))
n

(check nil : (List Nat))
(check (app ∷ false nil) : (List Bool))

(check vecnil : (Vec Nat zero))
(check (app vec∷ false vecnil) : (Vec Bool (suc zero)))
