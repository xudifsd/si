(load "unit-test.scm")
(load "types.scm")
(load "utils.scm") ;; for `range` and `same-list?`

;; test get-define-sym and get-define-value
(define (test-define define-list sym value)
  (let ((sym1 (get-define-sym define-list))
        (value1 (get-define-value define-list)))
    (and (eq? sym1 sym)
         (if (list? value)
           (same-list? value1 value)
           (eq? value1 value)))))

(define test-case (init-case))
(set! test-case (add-case '((define a 100) a 100) test-case))
(set! test-case (add-case '((define (a) 100) a (lambda () 100)) test-case))
(set! test-case (add-case '((define (a x y) x y) a (lambda (x y) x y)) test-case))
(unit-test test-define test-case)

;; test get-lambda-pars and get-lambda-body
(define (test-lambda lambda-list exp-pars exp-body)
  (let ((pars (get-lambda-pars lambda-list))
        (body (get-lambda-body lambda-list)))
    (and (same-list? pars exp-pars)
         (same-list? body exp-body))))

(set! test-case (init-case))
(set! test-case (add-case '((lambda (x y) (print x) 100) (x y) ((print x) 100)) test-case))
(set! test-case (add-case '((lambda () (print x) 100) () ((print x) 100)) test-case))
(set! test-case (add-case '((lambda () 100) () (100)) test-case))
(unit-test test-lambda test-case)
