#lang typical/base

(provide
 (for-syntax Empty
             Bool true false
             Nat zero suc
             List nil ::
             × cons
             = refl
             Vec vecnil vec::)
 true false
 zero suc
 nil ::
 vecnil vec::
 cons
 refl)

(data Empty)
(data Bool
      [true : Bool]
      [false : Bool])
(data Nat
      [zero : Nat]
      [suc : (Nat . -> . Nat)])
(data (List [A : Type])
      [nil : (List A)]
      [:: : (A (List A) . -> . (List A))])
(data (Vec [A : Type] [N : Nat])
      [vecnil : (Vec A zero)]
      [vec:: : (A (Vec A N) . -> . (Vec A (suc N)))])
(data (× [L R : Type])
      [cons : (L R . -> . (× L R))])
(data (= [T : Type] [a b : T])
      [refl : (T . -> . (= T a a))])
