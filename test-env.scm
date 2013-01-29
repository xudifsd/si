(load "unit-test.scm")
(load "env.scm")

(define global-env (extend-env '(a b c) '(1 2 3) the-empty-env))
(define a-env (extend-env '(c d e) '(x 7 8) global-env))
(define b-env (extend-env '(c f d) '(y 9 6) global-env))

(define test-case (init-case))

(define (test-pro-for-find-sym-* env sym expect in-all?)
  (if in-all?
    (eq? expect (find-sym-in-all-env sym env))
    (eq? expect (find-sym-in-env sym env))))

(define (test-pro-for-link syms args expect)
  (same-list? (link syms args) expect))

;; test find-sym-*
(set! test-case (add-case (list global-env 'a 1 #t) test-case))
(set! test-case (add-case (list global-env 'a 1 #f) test-case))
(set! test-case (add-case (list global-env 'x '*no-sym* #f) test-case))
(set! test-case (add-case (list a-env 'a 1 #t) test-case))
(set! test-case (add-case (list a-env 'a '*no-sym* #f) test-case))
(set! test-case (add-case (list a-env 'c 'x #t) test-case))
(set! test-case (add-case (list a-env 'c 'x #f) test-case))
(set! test-case (add-case (list a-env 'f '*no-sym* #f) test-case))
(set! test-case (add-case (list a-env 'f '*no-sym* #t) test-case))
(set! test-case (add-case (list b-env 'd 6 #t) test-case))
(set! test-case (add-case (list b-env 'd 6 #f) test-case))
(set! test-case (add-case (list b-env 'e '*no-sym* #t) test-case))
(unit-test test-pro-for-find-sym-* test-case)

;; test define-sym!
(set! test-case (init-case))
(define-sym! 'z 100 global-env)
(print global-env)
(set! test-case (add-case (list global-env 'z 100 #f) test-case))
(set! test-case (add-case (list a-env 'z 100 #t) test-case))
(set! test-case (add-case (list b-env 'z 100 #t) test-case))
(set! test-case (add-case (list b-env 'z '*no-sym* #f) test-case))
(set! test-case (add-case (list global-env 'a 1 #f) test-case))
(set! test-case (add-case (list b-env 'd 6 #f) test-case))
(unit-test test-pro-for-find-sym-* test-case)

;; test set-sym!
(set! test-case (init-case))
(set-sym! 'c 1000 a-env)
(set! test-case (add-case (list a-env 'c 1000 #f) test-case))
(set! test-case (add-case (list a-env 'c 1000 #t) test-case))
(set! test-case (add-case (list b-env 'c 'y #f) test-case))
(set! test-case (add-case (list b-env 'c 'y #t) test-case))
(set! test-case (add-case (list global-env 'c 3 #f) test-case))
(set! test-case (add-case (list global-env 'c 3 #t) test-case))
(unit-test test-pro-for-find-sym-* test-case)

;; test link
(set! test-case (init-case))
(set! test-case (add-case '((x #!rest c) (1 2 3 4 5) ((x . 1) (c 2 3 4 5))) test-case))
(set! test-case (add-case '((x c) (1 2) ((x . 1) (c . 2))) test-case))
(set! test-case (add-case '((#!rest c) (1 2) ((c 1 2))) test-case))
(set! test-case (add-case '(() () ()) test-case))
(set! test-case (add-case '((#!rest c) (1 2) ((c 1 2))) test-case))
(set! test-case (add-case '((x . c) (1 2 3) ((x . 1) (c 2 3))) test-case))
(set! test-case (add-case '((x . c) (1) ((x . 1) (c))) test-case))
(unit-test test-pro-for-link test-case)
