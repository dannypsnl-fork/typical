# typical

### Basic Syntax

1. data(inductive type)

    ```racket
    (data Nat
        [zero : Nat]
        [suc : (Nat -> Nat)])
    ```

2. `define` introduces user-defined variables

    ```racket
    ; define `a` is `zero`
    (define a : Nat
      zero)
    ```

3. is-a judgement

    ```racket
    ; read as "`zero` is a `Nat`?"
    (zero :? Nat)
    ```

### TODO

- strictly positive of data type
- dependent function
