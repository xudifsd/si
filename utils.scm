(define (range base top)
  (define (iter cur acc)
    (if (= cur top)
      acc
      (iter (+ cur 1) (cons cur acc))))
  (reverse (iter base '())))

(define (pair-length lst)
  (define (re lst acc)
    (if (or (null? lst)
            (not (pair? lst)))
      acc
      (re (cdr lst) (+ 1 acc))))
  (re lst 0))

(define (list-ref-rest lst num)
  ;; same with list-ref expect it return list start from num
  (if (= num 0)
      lst
    (list-ref-rest (cdr lst) (- num 1))))

(define (same-list? list1 list2)
  ;; we now support '(1 (1 2 . 3) . 3)
  (cond ((and (eq? list1 '())
              (eq? list2 '()))
         #t)

        ((or
          (not (pair? list1))
          (not (pair? list2))
          (not (= (pair-length list1) (pair-length list2))))
        #f)

        (else
          (call/cc (lambda (return)
                     (for-each (lambda (i)
                                 (let ((cur1 (list-ref list1 i))
                                       (cur2 (list-ref list2 i)))
                                   (if (pair? cur1)
                                     (if (not (same-list? cur1 cur2))
                                       (return #f))
                                     (if (not (eq? cur1 cur2))
                                       (return #f)))))
                               (range 0 (pair-length list1)))
                     (if (eq?
                          (cdr (list-ref-rest list1 (- (pair-length list1) 1)))
                          (cdr (list-ref-rest list2 (- (pair-length list2) 1))))
                       (return #t)
                       (return #f)))))))

(define (last-element list)
  (list-ref list (- (length list) 1)))
