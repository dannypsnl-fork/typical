#lang typical/base

(provide
 (for-syntax Empty
             Bool true false
             Nat zero suc
             List nil ::
             Pair cons
             = refl)
 true false
 zero suc
 nil ::
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
(data (Pair [L R : Type])
      [cons : (L R . -> . (Pair L R))])
(data (= [T : Type] [a b : T])
      [refl : (T . -> . (= T a a))])
