(load "utils.scm")
(load "env.scm")
(load "types.scm")

(define (si-eval expression env)
  (cond ((constant? expression) expression)

        ((variable? expression)
         (let ((value (find-sym-in-all-env expression env)))
           (if (find? value)
             value
             (error "unbond variable " expression))))

        ((lambda? expression)
         (let ((pars (get-lambda-pars expression))
               (body (get-lambda-body expression)))
           (make-procedure pars body env)))

        ((if? expression)
         (if (si-eval (get-if-condition expression) env)
           (si-eval (get-if-clause expression) env)
           (si-eval (get-if-alternative expression) env)))

        ((define? expression)
         (let ((sym (get-define-sym expression))
               (value (get-define-value expression)))
           (define-sym! sym value env)))

        ((set!? expression)
         (let ((sym (get-set-sym expression))
               (value (get-set-value expression)))
           (set-sym! sym value env)))

        ((quote? expression)
         (get-quoted-text expression))

        ((sequence? expression)
         (eval-sequence expression env))

        ((backquote? expression)
         (splice-list (map (lambda (element)
                             (cond ((comma? element)
                                    (si-eval (get-comma-text element) env))
                                   ((comma-at? element) ;; preprocess comma-at
                                    (make-comma-at
                                     (si-eval (get-comma-at-text element) env)))
                                   (else
                                     element)))
                           (get-backquoted-text expression))))))

(define (splice-list expression)
  (let ((rtn '()))
    (define (push x)
      (set! rtn (append (list x) rtn)))

    (for-each (lambda (x)
                (if (comma-at? x)
                  (for-each (lambda (y)
                              (push y))
                            (get-comma-at-text x))
                  (push x)))
              expression)
    (reverse rtn)))

(define (eval-sequence expressions env)
  (last-element
   (map (lambda (statment)
          (si-eval statment env))
        (get-sequence expressions))))
