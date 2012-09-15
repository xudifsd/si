(load "unit-test.scm")
(load "eval.scm")
(load "utils.scm")

(define global (extend-env '(x y true false) '((x y z) 100 #t #f) '()))

(define test-case (init-case))

(define (test-pro expression expected)
  (let ((result (si-eval expression global)))
    (if (list? expected)
      (same-list? result expected)
      (eq? result expected))))

(set! test-case (add-case '(x (x y z)) test-case))
(set! test-case (add-case '(y 100) test-case))
(set! test-case (add-case '((if true x y) (x y z)) test-case))
(set! test-case (add-case '((if false x y) 100) test-case))
(set! test-case (add-case '('x x) test-case))
(set! test-case (add-case '((begin x y) 100) test-case))
(set! test-case (add-case '(`(abc ,@x ,y) (abc x y z 100)) test-case))
(unit-test test-pro test-case)
