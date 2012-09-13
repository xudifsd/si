;; Author : xudifsd, Date : 2011-12-30
;; This is the utils for unit test
;;
;; case is a list whose elements also are list which used to applied to pro
;; pro is a user specified procedure to test if case is passed the test,
;; if pro returns true, case is passed, otherwise case is failed.

(define (unit-test pro case)
  (define (empty-case? case)
    (null? case))
  (define (first-case case)
    (car case))
  (define (next-case case)
    (cdr case))
  (define (report-finished failed?)
    (display "unit-test finished: ")
    (if failed?
      (print "test failed")
      (print "No fail")))
  (define (report-failure case)
    (print "unit-test fail at: " case))
 
  (define (iter case failed?)
    (if (empty-case? case)
      (report-finished failed?)
      (begin
       (print "testing " (first-case case))
       (if (apply pro (first-case case))
         (iter (next-case case) failed?)
         (begin
          (report-failure (first-case case))
          (iter (next-case case) #t))))))

  (iter case #f))

;; add new case to acc
(define (add-case case acc)
  ;; ATTENTION: this procedure doesn't call set!
  (append acc (list case)))

(define (init-case . cases)
  (define (iter cases acc)
    (if (null? cases)
      acc
      (iter (cdr cases) (cons (car cases) acc))))
  (iter cases empty-case))

(define empty-case '())
