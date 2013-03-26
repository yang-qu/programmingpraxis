;;http://programmingpraxis.com/2011/04/26/miscellanea/
;FizzBuzz

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (fizz-buzz n)
  (for-each (lambda (x) (display x) (newline))
            (map fizz-buzz-func (enumerate-interval 1 20))))

(define (fizz-buzz-func n)
  (cond ((and (= (remainder n 3) 0) (= (remainder n 5) 0))
         'fizz-buzz)
        ((= (remainder n 3) 0)
         'fizz)
        ((= (remainder n 5) 0)
         'buzz) 
        (else n)))

(fizz-buzz 20)
;prime words
;prime? is from SICP
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(define (string-to-number-list s n)
  (let ((numbers "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (cond ((= n (string-length s)) '())
          (else
           (cons (cons (string-find-next-char
                        numbers
                        (string-ref s n))
                       (- (string-length s) (+ n 1)))
                 (string-to-number-list s (+ n 1)))))))

(define (prime-words? w)
  (prime? (fold-left + 0 (map (lambda (x)
                                (* (car x)
                                   (expt 36 (cdr x))))
                              (string-to-number-list w 0)))))

(prime-words? "LISP") ;#t
(prime-words? "PRAXIS") ;#f
;split a list
;if length is odd, put the center element in first half
(define (col first-half second-half)
  (cons first-half second-half))

(define (split-a-list l col)
  (define (split-a-list-internal l c cn col)
    (cond ((null? l) (col '() '()))
          ((< c cn)
           (split-a-list-internal (cdr l) (+ c 1) cn
                                  (lambda (first-half second-half)
                                    (col (cons (car l) first-half)
                                         second-half))))
          (else
           (split-a-list-internal (cdr l) (+ c 1) cn
                                  (lambda (first-half second-half)
                                    (col first-half
                                         (cons (car l) second-half)))))))
  (split-a-list-internal l 0 (/ (length l) 2) col))

(define test-list (list 1 2 3 4 5))
;return a list contains two lists (first-half and second-half)
(split-a-list test-list col)
;output: ((1 2 3) 4 5)