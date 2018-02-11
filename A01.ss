;Yifei Li
;Homework 1
;1
(define Fahrenheit->Celsius
	(lambda (a)
		(  / (* (- a 32) 5) 9)
			))
;2
(define interval-contains? 
	(lambda (l b)
		(and (>= b (car l)) (<= b (cadr l)) 
			)))
;3
(define interval-intersects? 
	(lambda (l1 l2)
		(if (< (car l2) (car l1))
			(>= (cadr l2) (car l1)) 
			(>= (cadr l1) (car l2))
			)))
;4
(define interval-union
	(lambda (l1 l2)
		(if (interval-intersects? l1 l2)
			(list (list (if(< (car l1) (car l2) ) (car l1) (car l2) ) (if(> (cadr l1) (cadr l2) ) (cadr l1) (cadr l2) )) )
			(list l1 l2)
			)
			))
;5
(define divisible-by-7?
	(lambda (a)
		(integer? (/ a 7) )
			))
;6
(define ends-with-7?
	(lambda (a)
		(integer? ( / (+ a 3) 10))
			))
;7
(define first
	(lambda (list)
		(car list)
		))
(define second
	(lambda (list)
		(cadr list)
		))
(define third
	(lambda (list)
		(caddr list)
		))