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
  ;; a and b must be same length list
  (let ((rtn '()))
    (for-each (lambda (i)
                (set! rtn (cons (cons (list-ref a i)
                                      (list-ref b i)) rtn)))
              (range 0 (length a)))
    (reverse rtn)))

(define (extend-env syms values base)
  (if (not (= (length syms) (length values)))
    (error "requires " (length syms) " args") ;; FIXME we should consider ... here
    (cons base (link syms values))))
