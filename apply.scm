(load "utils.scm")

(define (si-apply procedure args)
  (cond ((primitve-procedure? procedure)
         (apply procedure args))
        ((user-procedure? procedure)
         (let ((new-env
                (extend-env (get-userp-pars procedure)
                            args
                            (get-userp-env procedure))))
           (eval-sequence (get-userp-body procedure) new-env)))
        (else
          (error procedure "is neither primitve nor user defined procedure"))))

(define (macroexpand macro args)
  (let ((new-env
         (extend-env (get-macro-pars macro)
                     args
                     (get-macro-env macro))))
    (eval-sequence (get-macro-body macro) new-env)))
