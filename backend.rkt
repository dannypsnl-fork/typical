#lang racket

(provide (all-defined-out))

(require nanopass/base
         "lang.rkt")

(define-pass pass:remove-claim : Typical (s) -> * ()
  (Stmt : Stmt (s) -> * ()
        [(claim ,stx ,name ,typ) #f]
        [else s])
  (Stmt s))

(define-language L1
  (extends Typical)
  (Stmt ()
        (- (claim stx name typ))))

(define-pass pass:Typical->L1 : Typical (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
        [(claim ,stx ,name ,typ) (raise-syntax-error 'unreachable "internal bug" stx)])
  (Stmt s))
