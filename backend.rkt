#lang racket

(require nanopass/base
         "lang.rkt")

(define-language L1
  (extends Typical)
  (Stmt ()
        (- (claim stx name typ)
           (data stx name (dependency* ...) constructor* ...))))

(define-pass pass:remove-type-related : Typical (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
        [(claim ,stx ,name ,typ) (error 'unreachable)]
        [(data ,stx ,name (,dependency* ...) ,constructor* ...)
         (error 'unreachable)])
  (Stmt s))

(define-pass pass:to-racket : L1 (s) -> * ()
  (Stmt : Stmt (s) -> * ()
        [(is-a? ,stx ,expr ,typ)
         expr]
        [(define ,stx ,name ,expr)
         `(define ,(syntax-e name) ,expr)])
  (Stmt s))
