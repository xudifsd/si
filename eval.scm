(load "utils.scm")
(load "env.scm")
(load "types.scm")
(load "apply.scm")

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
           (let ((alternative (get-if-alternative expression)))
             (if (null? alternative)
               'standard-does-not-specify-return-value
               (si-eval alternative env)))))

        ((define? expression)
         (let ((sym (get-define-sym expression)))
           (if (define-1-format? expression)
             (let ((value (si-eval (get-define-value expression) env)))
               (define-sym! sym value env))
             (define-sym! sym (si-eval (get-define-value expression) env) env))))

        ((set!? expression)
         (let ((sym (get-set-sym expression))
               (value (si-eval (get-set-value expression) env)))
           (set-sym! sym value env)))

        ((quote? expression)
         (get-quoted-text expression))

        ((sequence? expression)
         (eval-sequence expression env))

        ((call/cc? expression)
         (call/cc (lambda (cur)
                    (si-apply (si-eval (get-call/cc-pro expression)
                                       env)
                              (list cur)))))

        ((defmacro? expression)
         (define-sym! (get-defmacro-sym expression)
           (make-macro (get-defmacro-pars expression)
                       (get-defmacro-body expression)
                       env) ;; lexical closure for macro, I'm not sure if this is right
           env))

        ((backquote? expression)
         (define (unquote lst)
           (map (lambda (element)
                  (cond ((null? element) element)
                        ((comma? element)
                         (si-eval (get-comma-text element) env))
                        ((comma-at? element) ;; preprocess comma-at
                         (make-comma-at
                          (si-eval (get-comma-at-text element) env)))
                        ((list? element) ;;nest
                         (unquote element))
                        (else
                          element)))
                lst))
         (splice-list (unquote (get-backquoted-text expression))))

        (else
          (let ((value (si-eval (operator expression) env)))
            (cond ((application? value)
                   (si-apply value (map (lambda (ele)
                                          (si-eval ele env))
                                        (get-application-args expression))))

                  ((macro? value)
                   (si-eval (macroexpand value (get-application-args expression)) env))

                  (else
                    (error "Unknow expression " expression)))))))

(define (splice-list expression)
  (define (splice lst)
    (let ((rtn '()))
      (define (push! x)
        (set! rtn (append rtn (list x))))

      (for-each (lambda (x)
                  (cond ((null? x) (push! x))
                        ((comma-at? x)
                         (for-each (lambda (y)
                                     (push! y))
                                   (get-comma-at-text x)))
                        ((list? x)
                         (push! (splice x)))
                        (else
                          (push! x))))
                lst)
      rtn))

  (splice expression))

(define (eval-sequence expressions env)
  (last-element
   (map (lambda (statment)
          (si-eval statment env))
        (get-sequence expressions))))
