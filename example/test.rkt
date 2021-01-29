#lang typical

(require typical/prelude)

(check (refl zero) : (= Nat zero zero))

(check zero : Nat)
(check (suc zero) : Nat)
(check (:: zero (:: zero (:: zero nil))) : (List Nat))
(check (cons zero true) : (× Nat Bool))

;(data (Vec [E : Type] [N : Nat])
;      [vecnil : (Vec E zero)]
;      [vec:: : (E (Vec E N) . -> . (Vec E (suc N)))])
;(check vecnil : (Vec Nat zero))
;(check (vec:: (suc zero) (vec:: zero vecnil)) : (Vec Nat (suc (suc zero))))

#;(define a : Nat
    zero)

#;(define id-Nat : (Nat -> Nat)
    (λ (n) n))

#;(define + : (Nat Nat -> Nat)
    (λ (n m)
      (match {n m}
        [zero ,m => m]
        [(suc ,n) ,m => (suc (+ n m))])))
#;(check (+ (suc (suc zero)) (suc (suc zero))) : Nat)
#;(check (+ (suc zero) (suc (suc zero))) : Nat)

#;(define fib : (Nat -> Nat)
    (λ (n)
      (match n
        [zero => (suc zero)]
        [(suc zero) => (suc zero)]
        [(suc (suc ,n)) => (+ (fib (suc n)) (fib n))])))
; 0 -> 1
#;(check (fib zero) : Nat)
; 1 -> 1
#;(check (fib (suc zero)) : Nat)
; 2 -> 2
#;(check (fib (suc (suc zero))) : Nat)
; 3 -> 3
#;(check (fib (suc (suc (suc zero)))) : Nat)
; 4 -> 5
#;(check (fib (suc (suc (suc (suc zero))))) : Nat)
; 5 -> 8
#;(check (fib (suc (suc (suc (suc (suc zero)))))) : Nat)

#;(define ack : (Nat Nat -> Nat)
    (λ (n m)
      (match {n m}
        [zero ,m => (suc m)]
        [(suc ,n) zero => (ack n (suc zero))]
        [(suc ,n) (suc ,m) => (ack n (ack (suc n) m))])))
