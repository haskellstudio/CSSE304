;Yifei Li
;Homework 1b

;1
(define countdown
	(lambda  (a)
		(cond
			[(zero? a) '(0)]
			[else (cons a (countdown (- a 1)))]
			)))
;2
(define range
	(lambda (a b) 
		(cond
		[(>= a b) '()]
		[else (cons a (range (+ 1 a) b)) ]
		)))	
;3
(define sum-of-squares
	(lambda (ls)
		(cond
			[ (null? ls) 0 ]
			[else (+ (* (car ls) (car ls)) (sum-of-squares (cdr ls)) )]
			)))
;4
(define map
	(lambda (proce a)
		(cond
			[ (null? a) '()]
			[else (cons (proce (car a)) (map proce (cdr a) ) )]
			)))
;5
(define minus
	(lambda (a b)
		(cond
			[ (zero? b) a]
			[else (sub1 (minus a (sub1 b)))]
			)))
;6
(define div
	(lambda (a b)
		(cond
			[(zero? a) 0]
			[else (add1 (div (minus a b) b))]
			)))
;7
(define filter
	(lambda (proce ls)
		(cond
			[(null? ls) '()]
			[(proce (car ls)) (cons (car ls) (filter proce (cdr ls)))]
			[else (filter proce (cdr ls)) ]
			)))
;8
(define le->natural
	(lambda (ls)
		(cond
			[(null? ls) 0]
			[else (+ (car ls) (* 2 (le->natural (cdr ls))))]
			)))