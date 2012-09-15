(define (range base top)
  (define (iter cur acc)
    (if (= cur top)
      acc
      (iter (+ cur 1) (cons cur acc))))
  (reverse (iter base '())))

(define (same-list? list1 list2)
  (if (or
       (not (list? list1))
       (not (list? list2))
       (not (= (length list1) (length list2))))
    #f
    (call/cc (lambda (return)
               (for-each (lambda (i)
                           (let ((cur1 (list-ref list1 i))
                                 (cur2 (list-ref list2 i)))
                             (if (list? cur1)
                               (if (not (same-list? cur1 cur2))
                                 (return #f))
                               (if (not (eq? cur1 cur2))
                                 (return #f)))))
                         (range 0 (length list1)))
               (return #t)))))

(define (last-element list)
  (list-ref list (- (length list) 1)))
