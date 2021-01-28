#lang racket

(provide (except-out (all-from-out racket)
                     define)
         data check
         app
         (for-syntax ->)
         (rename-out [define- define]))

(require syntax/parse/define)

(define-for-syntax Type
  (syntax-property #''Type 'type 'Type))
(define-syntax (Type stx) Type)
(define-for-syntax (-> . any)
  `(-> ,@(map syntax->datum any)))

(begin-for-syntax
  (require racket/match)

  (define-syntax-class type
    #:datum-literals (->)
    (pattern name:id)
    (pattern (-> param*:type ... ret:type)))

  (define-syntax-class data-clause
    #:datum-literals (:)
    (pattern (name:id : typ:type)
             #:attr val
             (match (syntax->datum #'typ)
               [`(-> ,param* ... ,ret)
                #'(λ (arg*)
                    `(name ,@arg*))]
               [ty #''name]))))

(define-syntax-parser data
  [(_ name:id ctor*:data-clause ...)
   #'(begin
       (define-for-syntax name (syntax-property #'name 'type Type))
       (define-syntax (name stx) name)
       (define-for-syntax ctor*.name
         (syntax-property #'ctor*.val
                          'type ctor*.typ))
       ...
       (define-syntax (ctor*.name stx) ctor*.name)
       ...)])

(define-for-syntax (same-type? exp-ty act-ty
                               this-syntax
                               e)
  (unless (equal? exp-ty act-ty)
    (raise-syntax-error 'semantic
                        (format "type mismatched, expected: ~a, but got: ~a" exp-ty act-ty)
                        this-syntax
                        e)))

(define-syntax-parser app
  [(_ f:expr arg*:expr ...)
   (match (syntax-property (eval #'f) 'type)
     [`(-> ,param-ty* ... ,ret-ty)
      ; type check
      (for ([arg (syntax->list #'(arg* ...))]
            [exp-ty param-ty*])
        (define act-ty (<-type arg))
        (same-type? exp-ty act-ty
                    this-syntax
                    arg))
      #'(apply f (list arg* ...))]
     [else (raise-syntax-error 'semantic
                               "not appliable"
                               this-syntax
                               #'f)])])

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : typ:type expr:expr)
   (define exp-ty (syntax->datum #'typ))
   (define act-ty (<-type #'expr))
   (same-type? exp-ty act-ty
               this-syntax
               #'expr)
   #'(begin
       (define-for-syntax name typ)
       (define name expr))])

(define-syntax-parser check
  #:datum-literals (:)
  [(_ expr:expr : typ:type)
   #'(begin
       (define-for-syntax name typ)
       expr)])

; type inference
(define-for-syntax (<-type stx)
  (syntax-parse stx
    [(app f:expr arg*:expr ...)
     (match (syntax-property (eval #'f) 'type)
       [`(-> ,param-ty* ... ,ret-ty) ret-ty]
       [else (raise-syntax-error 'semantic
                                 "not appliable"
                                 this-syntax
                                 #'f)])]
    [x:id
     (define r (syntax-property (eval #'x) 'type))
     (if (syntax? r)
         (syntax->datum r)
         r)]))
