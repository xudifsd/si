(load "eval.scm")

(define global-env (extend-env '(+ - * / sin cos tan log)
                               `(,+ ,- ,* ,/ ,sin ,cos ,tan ,log)
                               the-empty-env))

(define (user-space-procedure? obj)
  (or (procedure? obj)
      (user-procedure? obj)))

(define-sym! 'procedure? user-space-procedure? global-env)

(define-sym! 'eval (lambda (expression . env)
                     (if (null? env)
                       (si-eval expression global-env)
                       (si-eval expression `,@(env))))
  global-env)

(define-sym! 'apply si-apply global-env)

(define-sym! 'macroexpand
  (lambda (expression)
    (let ((value (si-eval (operator expression) global-env)))
      (if (not (macro? value))
        (error expression " is not a macro")
        (macroexpand value (get-application-args expression)))))
  global-env)

(define (indentity-with-user! sym)
  (define-sym! sym (eval sym) global-env))

;; full list of scheme primitive see
;; http://www.eecs.berkeley.edu/~bh/ssch27/appendix-funlist.html
(indentity-with-user! '=)
(indentity-with-user! '<)
(indentity-with-user! '<=)
(indentity-with-user! '>)
(indentity-with-user! '>=)
(indentity-with-user! 'abs)
(indentity-with-user! 'append)
(indentity-with-user! 'assoc)
(indentity-with-user! 'boolean?)
(indentity-with-user! 'car)
(indentity-with-user! 'caar)
(indentity-with-user! 'caaar)
(indentity-with-user! 'caadr)
(indentity-with-user! 'cadr)
(indentity-with-user! 'cadar)
(indentity-with-user! 'caddr)
(indentity-with-user! 'cdr)
(indentity-with-user! 'cdar)
(indentity-with-user! 'cdaar)
(indentity-with-user! 'cdadr)
(indentity-with-user! 'cddr)
(indentity-with-user! 'cddar)
(indentity-with-user! 'cdddr)
(indentity-with-user! 'ceiling)
(indentity-with-user! 'cons)
(indentity-with-user! 'display)
(indentity-with-user! 'error)
(indentity-with-user! 'even?)
(indentity-with-user! 'floor)
(indentity-with-user! 'integer?)
(indentity-with-user! 'length)
(indentity-with-user! 'list)
(indentity-with-user! 'list-ref)
(indentity-with-user! 'list?)
(indentity-with-user! 'max)
(indentity-with-user! 'min)
(indentity-with-user! 'member)
(indentity-with-user! 'newline)
(indentity-with-user! 'not)
(indentity-with-user! 'null?)
(indentity-with-user! 'number?)
(indentity-with-user! 'odd?)
(indentity-with-user! 'random)
(indentity-with-user! 'set-car!)
(indentity-with-user! 'set-cdr!)
(indentity-with-user! 'string<)
(indentity-with-user! 'string<=)
(indentity-with-user! 'string<=?)
(indentity-with-user! 'string<>)
(indentity-with-user! 'string<?)
(indentity-with-user! 'string=)
(indentity-with-user! 'string=?)
(indentity-with-user! 'string>)
(indentity-with-user! 'string>=)
(indentity-with-user! 'string>=?)
(indentity-with-user! 'string>?)
(indentity-with-user! 'string?)
(indentity-with-user! 'pair?)
(indentity-with-user! 'print)
(indentity-with-user! 'atom?)
(indentity-with-user! 'eq?)
(indentity-with-user! 'equal?)

;; primitive procedures don't recognize user-procedure, so we have to define our own
(si-eval '(define (for-each pro lst)
           (if (null? lst)
             'done
             (begin
              (pro (car lst))
              (for-each pro (cdr lst)))))
         global-env)

(si-eval '(define (map pro lst)
           (define (iter lst acc)
             (if (null? lst)
               acc
               (iter (cdr lst)
                     (append acc
                             (list (pro (car lst)))))))
           (iter lst '()))
         global-env)

(si-eval '(defmacro cond (#!rest conds)
           (if (null? conds)
             '()
             (if (eq? (caar conds) 'else)
               `(begin ,@(cdar conds))
               `(if ,(caar conds)
                 (begin ,@(cdar conds))
                 (cond ,@(cdr conds))))))
         global-env)

(si-eval '(defmacro let (args #!rest body)
           `((lambda ,(map car args)
               ,@body)
             ,@(map cadr args)))
         global-env)

(si-eval '(defmacro delay (exp)
           `(lambda ()
             ,exp))
         global-env)

(si-eval '(define (force exp)
           (exp))
         global-env)
