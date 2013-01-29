(load "unit-test.scm")
(load "setup.scm")

(define test-case (init-case))

(define (test-pro expression expected)
  (let ((result (si-eval expression global-env)))
    (if (list? expected)
      (same-list? result expected)
      (eq? result expected))))

;; test let and cond
(set! test-case (add-case '((let ((x 1)) x) 1) test-case))
(set! test-case (add-case '((let ((x 1) (y 2)) (+ x y)) 3) test-case))
(set! test-case (add-case '((let ((x 1) (y 2))
			     (cond ((= y 1) y)
				    ((= x 2) x)
				    (else
				     'done)))
			    done) test-case))

;; test delay and force
(set! test-case (add-case '((let ((x 100))
			     (define a (delay x))
			     (force a))
			    100) test-case))
(unit-test test-pro test-case)
