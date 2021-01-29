# typical

### Basic Syntax

1. data(inductive type)

    ```racket
    (data Nat
        [zero : Nat]
        [suc : (Nat . -> . Nat)])
    ```

2. `define` introduces user-defined variables

    ```racket
    ; define `a` is `zero`
    (define a : Nat
      zero)
    ```

3. check type of term

    ```racket
    ; read as "check `zero` is a `Nat`?"
    (check zero : Nat)
    ```

### TODO

- strictly positive of data type
- dependent function
