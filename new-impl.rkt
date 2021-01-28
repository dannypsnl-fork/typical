#lang racket

(provide (except-out (all-from-out racket)
                     define)
         data check
         app
         (for-syntax ->)
         (rename-out [define- define]))

(require syntax/parse/define
         (for-syntax "private/core.rkt"))

(define-for-syntax Type
  (syntax-property #''Type 'type 'Type))
(define-syntax (Type stx) Type)
(define-for-syntax (-> . any)
  `(-> ,@(map (λ (ty)
                (cond
                  [(syntax? ty) (syntax->datum ty)]
                  [else ty]))
              any)))

(begin-for-syntax
  (require racket/match)

  (define-syntax-class type
    #:datum-literals (->)
    (pattern name:id)
    (pattern (name:id e*:type))
    (pattern (-> param*:type ... ret:type)))

  (define-syntax-class data-clause
    #:datum-literals (:)
    (pattern (name:id : typ:type)
             #:attr val
             (match (syntax->datum #'typ)
               [`(-> ,param* ... ,ret)
                #'(λ (arg*)
                    `(name ,@arg*))]
               [ty #''name])))

  (define-syntax-class bind
    #:datum-literals (:)
    (pattern (name:id : typ:type))))

(define-syntax-parser data
  [(_ data-def ctor*:data-clause ...)
   (with-syntax ([ty-runtime #'(define-syntax (name stx) #'name)]
                 [ctor-compiletime*
                  #'(begin (define-for-syntax ctor*.name
                             (syntax-property
                              #'ctor*.val
                              'type ctor*.typ)) ...)]
                 [ctor-runtime*
                  #'(begin (define-syntax (ctor*.name stx) ctor*.name) ...)])
     (syntax-parse #'data-def
       [name:id
        #'(begin
            (define-for-syntax name (syntax-property #'name 'type Type))
            ty-runtime
            ctor-compiletime*
            ctor-runtime*)]
       [(name:id bind*:bind ...)
        #'(begin
            (define-for-syntax bind*.name (freevar (gensym 'bind*.name))) ...
            (define-for-syntax (name . arg*)
              `(name ,@arg*))
            ty-runtime
            ctor-compiletime*
            ctor-runtime*)]))])

(define-syntax-parser app
  [(_ f:expr arg*:expr ...)
   (<-type this-syntax)
   #`(#,(eval #'f) (list arg* ...))])

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : typ:type expr:expr)
   #'(begin
       (check expr : typ)
       (define-for-syntax name
         (syntax-property #'expr
                          'type
                          typ))
       (define name expr))])

(define-syntax-parser check
  #:datum-literals (:)
  [(_ expr:expr : typ:type)
   (define exp-ty (syntax->datum #'typ))
   (define act-ty (<-type #'expr))
   (unify exp-ty act-ty
          this-syntax
          #'expr)
   #'(begin
       (begin-for-syntax
         typ)
       expr)])

; type equality
(define-for-syntax (same-type? exp-ty act-ty
                               this-syntax
                               e)
  (unless (equal? exp-ty act-ty)
    (raise-syntax-error 'semantic
                        (format "type mismatched, expected: ~a, but got: ~a" exp-ty act-ty)
                        this-syntax
                        e)))
; type inference
(define-for-syntax (<-type stx)
  (syntax-parse stx
    [(app f:expr arg*:expr ...)
     (match (syntax-property (eval #'f) 'type)
       [`(-> ,param-ty* ... ,ret-ty)
        (define subst (make-subst))
        (for ([arg (syntax->list #'(arg* ...))]
              [exp-ty param-ty*])
          (define act-ty (<-type arg))
          (unify exp-ty act-ty
                 stx
                 arg
                 #:subst subst
                 #:solve? #f))
        (replace-occur ret-ty #:occur (subst-resolve subst stx))]
       [else (raise-syntax-error 'semantic
                                 "not appliable"
                                 this-syntax
                                 #'f)])]
    [x:id
     (define r (syntax-property (eval #'x) 'type))
     (if (syntax? r)
         (syntax->datum r)
         r)]))
