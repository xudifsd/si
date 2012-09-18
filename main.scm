(load "setup.scm")

(define (promote)
  (display "si-evaluator> "))

(define debug #t)

(if debug
  (set! error print))

(let loop ()
  (promote)
  (let ((input (read)))
    (if (eq? input '#!eof)
      (exit 0)
      (let ((output (si-eval input global-env)))
        (cond ((user-procedure? output) (print "#<user-defined-procedure>"))
              ((macro? output) (print "#<user-defined-macro>"))
              (else (print output)))
        (loop)))))
