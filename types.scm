;; This file defines all the internal structure of si

(define (primitve-procedure? obj)
  (procedure? obj))

(define (constant? obj)
  (or (string? obj) (number? obj)))

(define (is-operator? expression symbol)
  (and (list? expression)
       (eq? (car expression) symbol)))


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
