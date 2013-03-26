;run in chez scheme 8.4
;http://programmingpraxis.com/2013/01/15/translate-csv-to-html/
;TODO: read csv file
;      parse csv file to a list
(define (string-split sep str)
  (define (f cs xs) (cons (list->string (reverse cs)) xs))
  (let loop ((ss (string->list str)) (cs '()) (xs '()))
    (cond ((null? ss) (reverse (if (null? cs) xs (f cs xs))))
          ((char=? (car ss) sep) (loop (cdr ss) '() (f cs xs)))
          (else (loop (cdr ss) (cons (car ss) cs) xs)))))

(define (flatten xs)
  (cond ((null? xs) xs)
        ((pair? xs)
          (append (flatten (car xs))
                  (flatten (cdr xs))))
        (else (list xs))))

(string-split #\, "name,age,location")

(html-tr (list "name" "age" "location"))

(define (html-td cell)
  (list "<td>" cell "</td>"))

(define (html-tr row)
  (string-append "<tr>" 
  	 (apply string-append 
			(flatten (map html-td row))) 
		 "</tr>"))

(define (html-table rows)
  (string-append "<table>"
		 (apply string-append
			(flatten (map html-tr rows)))
		 "</table>"))

(html-td "name")

(html-table (list (list "name" "age" "location")
		  (list "aa" "17" "TSV")))