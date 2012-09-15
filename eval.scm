(load "utils.scm")
(load "env.scm")

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
           (si-eval (get-if-clause expression))
           (si-eval (get-if-alternative expression))))

        ((define? expression)
         (let ((sym (get-define-sym expression))
               (value (get-define-value expression)))
           (define-sym! sym value env)))

        ((set!? expression)
         (let ((sym (get-set-sym expression))
               (value (get-set-value expression)))
           (set-sym! sym value env)))))

(define (eval-sequence expressions env)
  (last-element
   (map (lambda (statment)
          (si-eval statment env))
        expressions)))
