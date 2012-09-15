;; This file defines all the internal structure of si

(define (primitve-procedure? obj)
  (procedure? obj))

(define (constant? obj)
  (or (string? obj) (number? obj) (boolean? obj)))

(define (variable? obj)
  (symbol? obj))

(define (is-operator? expression symbol)
  (and (list? expression)
       (eq? (car expression) symbol)))

(define (quote? expression)
  (is-operator? expression 'quote))

(define (get-quoted-text expression)
  (cadr expression))

(define (backquote? expression)
  (is-operator? expression 'quasiquote))

(define (get-backquoted-text expression)
  (cadr expression))

(define (comma? expression)
  (is-operator? expression 'unquote))

(define (get-comma-text expression)
  (cadr expression))

(define (comma-at? expression)
  (is-operator? expression 'unquote-splicing))

(define (get-comma-at-text expression)
  (cadr expression))

(define (make-comma-at expression)
  (append (list 'unquote-splicing) (list expression)))

(define (sequence? expression)
  (is-operator? expression 'begin))

(define (make-sequence expression)
  (append (list 'begin)
          (if (not (list? expression))
            (list expression)
            expression)))

(define (get-sequence expression)
  (cdr expression))


;; lambda
(define (lambda? expression)
  (is-operator? expression 'lambda))

(define (get-lambda-pars expression)
  (let ((pars (cadr expression)))
    (if (not (list? pars))
      (error pars "is not a list")
      pars)))

(define (get-lambda-body expression)
  (cddr expression))

(define (make-lambda pars body)
  (append (list 'lambda) (list pars) body))


;; if
(define (if? expression)
  (is-operator? expression 'if))

(define (get-if-condition expression)
  (cadr expression))

(define (get-if-clause expression)
  (caddr expression))

(define (get-if-alternative expression)
  (cadddr expression))


;; define
(define (define? expression)
  (is-operator? expression 'define))

;; define has two format (define (pro x) x) and (define r 100)
(define (get-define-sym expression)
  (let ((rtn (cadr expression)))
    (if (list? rtn)
      (car rtn)
      rtn)))

;; (define (pro x) x)
(define (define-1-format? expression)
  (list? (cadr expression)))

;; (define r 100)
(define (define-2-format? expression)
  (not (define-1-format? expression)))

(define (get-define-value expression)
  (let ((body (cddr expression)))
    (if (define-1-format? expression)
      (let ((pars (cdadr expression)))
        (make-lambda pars body))
      (caddr expression))))


;; set!
(define (set!? expression)
  (is-operator? expression 'set!))

(define (get-set-sym expression)
  (cadr expression))

(define (get-set-value expression)
  (caddr expression))

;; user defined procedure
(define (make-procedure pars body env)
  (list 'user-procedure pars body env))

(define (user-procedure? obj)
  (is-operator? obj 'user-procedure))

(define (get-userp-pars userp)
  (cadr userp))

(define (get-userp-body userp)
  (caddr userp))

(define (get-userp-env userp)
  (cadddr userp))