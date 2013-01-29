;; env has such structure: '(parent . frame) which parent is enclosed env
;; of current, and frame is associate list which stores key-value paires of
;; symbol and value, like '((x . 1) (y . 2))
;; we use '*no-sym* to indicate no sym found in env

(load "utils.scm")

(define the-empty-env '())

(define (empty-env? env)
  (eq? env the-empty-env))

(define (get-env-parent env)
  (car env))

(define (get-env-frame env)
  (cdr env))

(define the-empty-frame '())

(define (empty-frame? env) ;; argument is env not frame
  (eq? (get-env-frame env) the-empty-frame))

(define (find? a)
  (not (eq? a '*no-sym*)))

(define (find-sym-in-env sym env)
  (let ((value (assoc sym (get-env-frame env))))
    (if (pair? value)
      (cdr value)
      '*no-sym*)))

(define (find-sym-in-all-env sym env)
  (if (empty-env? env)
    '*no-sym*
    (let ((value (find-sym-in-env sym env)))
      (if (find? value)
        value
        (find-sym-in-all-env sym (get-env-parent env))))))

(define (define-sym! sym value env)
  (let ((rtn (find-sym-in-env sym env)))
    (if (find? rtn)
      (error sym " is already defined")
      (begin
       (set-cdr! env (cons (cons sym value) (get-env-frame env)))
       sym)))) ;; return defined sym

(define (set-sym! sym value env)
  (if (empty-env? env)
    (error sym " haven't been defined")
    (let ((rtn (find-sym-in-env sym env)))
      (if (find? rtn)
        (let ((old-pair (assoc sym (get-env-frame env))))
          (set-cdr! old-pair value)
          sym) ;; return seted sym
        (set-sym! sym value (get-env-parent env))))))

(define (link a b)
  ;; we now support `a` like (#!rest rest) and (x . rest)
  (let ((rtn '()))
    (call/cc (lambda (return)
               (for-each (lambda (i)
                           (let ((ae (list-ref-rest a i))
                                 (be (list-ref-rest b i)))
                             (if (eq? (car ae) '#!rest)
                               (let ((ae (list-ref-rest a (+ i 1))))
                                 (set! rtn (cons (cons
                                                  (car ae)
                                                  be)
                                                 rtn))
                                 (return 'dummy))
                               (begin
                                (set! rtn (cons (cons (car ae) (car be)) rtn))
                                (if (and
				     (not (pair? (cdr ae)))
				     (not (null? (cdr ae))));; like (x . rest)
                                  (begin
                                   (set! rtn (cons (cons (cdr ae) (list-ref-rest b (+ i 1))) rtn))
                                   (return 'dummy)))))))
                         (range 0 (pair-length a)))))
    (reverse rtn)))

(define (extend-env syms values base)
  (cons base (link syms values)))
