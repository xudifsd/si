(define (range base top)
  (define (iter cur acc)
    (if (= cur top)
      acc
      (iter (+ cur 1) (cons cur acc))))
  (reverse (iter base '())))
