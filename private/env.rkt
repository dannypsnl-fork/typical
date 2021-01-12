#lang racket/base

(provide make-env cur-env
         env/bind env/lookup)

(require racket/syntax)

(struct env (cur-map parent) #:transparent)
(define (make-env [parent (cur-env)])
  (env (make-hash) parent))
(define cur-env (make-parameter (make-env #f)))

(define (env/bind id val)
  (let* ([name (syntax-e id)]
         [binding* (env-cur-map (cur-env))]
         [bound? (hash-ref binding* name #f)])
    (unless (not bound?)
      (raise-syntax-error 'semantic (format "redefined `~a`" id) id))
    (hash-set! binding* name val)))
(define (env/lookup id)
  (let* ([name (syntax-e id)]
         [binding* (env-cur-map (cur-env))]
         [parent (env-parent (cur-env))])
    (hash-ref binding* name
              (λ () (if parent
                        (parameterize ([cur-env parent])
                          (env/lookup id))
                        (raise-syntax-error 'semantic (format "`~a` not found" id) id))))))

(module+ test
  (require rackunit)

  (parameterize ([cur-env (make-env #f)])
    (env/bind #'a 1)
    (check-equal? (env/lookup #'a) 1)))
