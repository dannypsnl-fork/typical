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
    (a : Nat)
    (a = zero)
    ```

3. is-a judgement

    ```racket
    (zero :? Nat)
    ```

### TODO

- strictly positive of data type
- dependent function in claim type
- termination checking
- expand to racket to get evaluated result
- lambda
- pattern matching
