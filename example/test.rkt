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
(data (= [A : Type] [a : A] [b : A])
      [refl : (A -> (= A a a))])

((refl zero) :? (= Nat zero zero))

(zero :? Nat)
((suc zero) :? Nat)
((:: zero (:: zero (:: zero nil))) :? (List Nat))
(vecnil :? (Vec Nat zero))
((vec:: (suc zero) (vec:: zero vecnil)) :? (Vec Nat (suc (suc zero))))
((cons zero true) :? (Pair Nat Bool))

(a : Nat)
(a = zero)
(a :? Nat)

(id-Nat : (Nat -> Nat))
(id-Nat = (λ (n) n))

(+ : (Nat Nat -> Nat))
(+ =
   (λ (n m)
     (match {n m}
       [zero ,m => m]
       [(suc ,n) ,m => (suc (+ n m))])))
((+ (suc (suc zero)) (suc (suc zero))) :? Nat)
((+ (suc zero) (suc (suc zero))) :? Nat)

(fib : (Nat -> Nat))
(fib =
     (λ (n)
       (match n
         [zero => (suc zero)]
         [(suc zero) => (suc zero)]
         [(suc (suc ,n)) => (+ (fib (suc n)) (fib n))])))
; 0 -> 1
((fib zero) :? Nat)
; 1 -> 1
((fib (suc zero)) :? Nat)
; 2 -> 2
((fib (suc (suc zero))) :? Nat)
; 3 -> 3
((fib (suc (suc (suc zero)))) :? Nat)
; 4 -> 5
((fib (suc (suc (suc (suc zero))))) :? Nat)
; 5 -> 8
((fib (suc (suc (suc (suc (suc zero)))))) :? Nat)

(ack : (Nat Nat -> Nat))
(ack = (λ (n m)
  (match {n m}
    [zero ,m => (suc m)]
    [(suc ,n) zero => (ack n (suc zero))]
    [(suc ,n) (suc ,m) => (ack n (ack (suc n) m))])))
