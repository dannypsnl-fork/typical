#lang typical

(data Nat
      [zero : Nat]
      [suc : (Nat -> Nat)])
(data Bool
      [true : Bool]
      [false : Bool])
(data (List [A : Type])
      [nil : (List A)]
      [:: : (A (List A) -> (List A))])
(data (Vec [A : Type] [N : Nat])
      [vecnil : (Vec A zero)]
      [vec:: : (A (Vec A N) -> (Vec A (suc N)))])
(data (Pair [A : Type] [B : Type])
      [cons : (A B -> (Pair A B))])

(zero :? Nat)
((suc zero) :? Nat)
((:: zero (:: zero (:: zero nil))) :? (List Nat))
(vecnil :? (Vec Nat zero))
((vec:: (suc zero) (vec:: zero vecnil)) :? (Vec Nat (suc (suc zero))))
((cons zero true) :? (Pair Nat Bool))

(a : Nat)
(a = zero)
(a :? Nat)
