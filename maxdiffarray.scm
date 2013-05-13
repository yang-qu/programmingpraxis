;;http://programmingpraxis.com/2011/04/01/maximum-difference-in-an-array/

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (last-index l)
  (- (length l) 1))

(define (get-diff data j i)
  (- (list-ref data j) (list-ref data i)))

(define (get-value result)
  (car result))

(define (get-i result)
  (cadr result))

(define (get-j result)
  (caddr result))

(define (max-diff-internal max data)
  (cond ((null? data) max)
        
        ((< (get-value max) (get-value (car data)))
         (max-diff-internal (car data) (cdr data)))
        
        ((= (get-value max) (get-value (car data)))
         (cond ((< (get-i max) (get-i (car data)))
                (max-diff-internal max (cdr data)))
               ((> (get-i max) (get-i (car data)))
                (max-diff-internal (car data) (cdr data)))
               (else
                (cond ((< (get-j max) (get-j (car data)))
                       (max-diff-internal max (cdr data)))
                      (else 
                       (max-diff-internal (car data) (cdr data)))))))
        (else
         (max-diff-internal max (cdr data)))))


(define (max-diff data)
  (let ((diff-result (accumulate append
                                 '()
                                 (map (lambda (i)
                                        (map (lambda (j) (list (get-diff data j i) i j))
                                             (enumerate-interval i (last-index data))))
                                      (enumerate-interval 0 (last-index data))))))
    (max-diff-internal (car diff-result) diff-result)))


(max-diff (list 4 3 9 1 8 2 6 7 5))

;linear-time solution
;result is a list contains (max-diff-value i j)
(define (max-diff-iter k min-i result data)
  (cond ((= k (length data)) result)
        (else
         (let* ((new-min-i (if (< (list-ref data k) (list-ref data min-i))
                               k
                               min-i))
                (diff (get-diff data k new-min-i))
                (new-result (if (> diff (get-value result))
                                (list diff new-min-i k)
                                result)))
           (max-diff-iter (+ k 1) new-min-i new-result data)))))

(define (max-diff-test)
  (assert (max-diff (list 4 3 9 1 8 2 6 7 5)) (list 7 3 4))
  (assert (max-diff (list 4 2 9 1 8 3 6 7 5)) (list 7 1 2))
  (assert (max-diff (list 4 3 9 1 2 6 7 8 5)) (list 7 3 7))
  (assert (max-diff (list 5 4 3)) (list 0 0 0))
  (assert (max-diff (list 1 3 3)) (list 2 0 1))
  (assert (max-diff-iter 0 0 (list 0 0 0) (list 4 3 9 1 8 2 6 7 5)) (list 7 3 4))
  (assert (max-diff-iter 0 0 (list 0 0 0) (list 4 2 9 1 8 3 6 7 5)) (list 7 1 2))
  (assert (max-diff-iter 0 0 (list 0 0 0) (list 4 3 9 1 2 6 7 8 5)) (list 7 3 7))
  (assert (max-diff-iter 0 0 (list 0 0 0) (list 5 4 3)) (list 0 0 0))
  (assert (max-diff-iter 0 0 (list 0 0 0) (list 1 3 3)) (list 2 0 1)))

(max-diff-test)