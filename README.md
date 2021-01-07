# typical

### Basic Syntax

1. data(inductive type)

    ```racket
    (data Nat
        [zero : Nat]
        [suc : (Nat -> Nat)])
    ```

2. claim/define pair to introduce user-defined variables

    ```racket
    ; claim `a` is a `Nat`
    (a : Nat)
    ; define `a` is `zero`
    (a = zero)
    ```

    claim a binding without a define is allowed, this case type checker simply believing the claim is correct no doubt.

3. is-a judgement

    ```racket
    ; read as "`zero` is a `Nat`?"
    (zero :? Nat)
    ```

### TODO

- strictly positive of data type
- dependent function in claim type
- termination checking
- expand to racket to get evaluated result
- lambda
- pattern matching
