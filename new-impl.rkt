#lang racket

(provide (except-out (all-from-out racket) define #%plain-app)
         data check
         (rename-out [define- define]))

(require syntax/parse/define)

(begin-for-syntax
  (require racket/match)

  (define (ty-check! stx expected actual)
    (unless (equal? expected actual)
      (raise-syntax-error 'semantic
                          (format "type mismatched, expected: ~a, but got: ~a" expected actual)
                          stx)))

  (define-syntax-class type
    #:datum-literals (->)
    (pattern name:id
             #:attr datum #''name
             #:attr val #'name)
    (pattern (param*:type ... -> ret:type)
             #:attr datum #''(-> param* ... ret)
             #:attr val
             (with-syntax ([(arg* ...) (generate-temporaries #'(param* ...))])
               #'(λ (arg* ...)
                   (for ([actual (list arg* ...)]
                         [expected (list param* ...)])
                     (unless (equal? expected actual)
                       (raise-syntax-error 'semantic
                                           (format "type mismatched, expected: ~a, but got: ~a" expected actual)
                                           #'(param* ...))))
                   ret))))

  (define-syntax-class data-clause
    #:datum-literals (:)
    (pattern (name:id : typ:type)
             #:attr def
             #`(define name
                 #,(match (cadr (syntax->datum #'typ.datum))
                     [`(-> ,param* ... ,ret)
                      #'(λ (arg*)
                          `(name ,arg*))]
                     [ty #''name]))
             #:attr typ-def
             #'(define-for-syntax name typ.val))))

(define-syntax-parser data
  [(_ name:id constructor*:data-clause ...)
   #'(begin
       (define-for-syntax name 'name)
       (define name 'name)
       constructor*.typ-def ...
       constructor*.def ...)])

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : typ:type expr:expr)
   (ty-check! this-syntax
              (eval #'typ)
              (eval #'expr))
   #'(begin
       (define-for-syntax name typ)
       (define name expr))]
  [(_ name:id : typ:type)
   #'(define-for-syntax name typ)])

(define-syntax-parser check
  #:datum-literals (:)
  [(_ expr:expr : typ:type)
   (ty-check! this-syntax
              (eval #'typ)
              (eval #'expr))
   #'expr])
