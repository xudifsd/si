(load "unit-test.scm")
(load "eval.scm")
(load "utils.scm")

(define global (extend-env '(x y true false) '((x y z) 100 #t #f) '()))
(si-eval '(define (bbb x) x) global)
(define-sym! '+ + global)
(define-sym! '/ / global)
(define-sym! '= = global)
(define-sym! 'null? null? global)
(define-sym! 'car car global)
(define-sym! 'cdr cdr global)

(si-eval '(define (make-adder base)
	   (lambda (x)
	    (set! base (+ x base))
	    base)) global)
(si-eval '(define base100 (make-adder 100)) global)
(si-eval '(define base5 (make-adder 5)) global)

;; primitive procedures don't recognize user-procedure, so we have to define our own
(si-eval '(define (for-each pro lst)
           (if (null? lst)
             'done
             (begin
              (pro (car lst))
              (for-each pro (cdr lst)))))
         global)

(si-eval '(define (test-call/cc lst num)
           (call/cc (lambda (return)
                      (for-each (lambda (ele)
                                  (if (= ele num)
                                    (return ele)))
                                lst)
                      #f)))
         global)

(si-eval '(defmacro unless (cond st1 alt)
           `(if ,cond
             ,alt
             ,st1))
         global)

(define test-case (init-case))

(define (test-pro expression expected)
  (let ((result (si-eval expression global)))
    (if (list? expected)
      (same-list? result expected)
      (eq? result expected))))

;; test symbol resolve
(set! test-case (add-case '(x (x y z)) test-case))
(set! test-case (add-case '(y 100) test-case))
;; test if statement
(set! test-case (add-case '((if true x y) (x y z)) test-case))
(set! test-case (add-case '((if false x y) 100) test-case))
;; test quote
(set! test-case (add-case '('x x) test-case))
(set! test-case (add-case '((begin x y) 100) test-case))
(set! test-case (add-case '(`(abc ,@x ,y) (abc x y z 100)) test-case))
;; test procedure call
(set! test-case (add-case '((bbb 10000) 10000) test-case))
(set! test-case (add-case '(((lambda (x y) (+ x y)) 5 4) 9) test-case))
(set! test-case (add-case '(((lambda () 5)) 5) test-case))
;; test lexical closure
(set! test-case (add-case '((base100 20) 120) test-case))
(set! test-case (add-case '((base5 10) 15) test-case))
;; test call/cc
(set! test-case (add-case '((test-call/cc '(1 5 9) 9) 9) test-case))
(set! test-case (add-case '((test-call/cc '(1 5 9) 2) #f) test-case))
;; test macro
(si-eval '(define zero 0) global)
(set! test-case (add-case '((unless (= zero 0) (/ 1 0) 100) 100) test-case))
(set! test-case (add-case '((unless (= zero 2) 200 100) 200) test-case))
(unit-test test-pro test-case)
