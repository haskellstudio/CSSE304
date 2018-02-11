;;Yifei Li
;;A02

;;1
(define memv
	(lambda (a ls)
		(cond
			[(null? ls) #f]
			[(eqv? a (car ls) ) ls]
			[else (memv a (cdr ls))]
			)))
;;2
(define append
	(lambda (ls1 ls2)
		(cond
			[(null? ls1) ls2]
			[else (cons (car ls1) (append (cdr ls1) ls2) )]
			)))
;;3
(define reverse
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[else (append (reverse (cdr ls)) (list (car ls)) )]
			)))
;;4
(define union
	(lambda (ls1 ls2)
		(cond
			[(null? ls2) ls1]
			[(eqv? (memv (car ls2) ls1 ) #f) (append (union ls1 (cdr ls2)) (list (car ls2)))]
			[else (union ls1 (cdr ls2))]
			)))
;;5
(define extend
	(lambda (x pred)
		(lambda (y) 
		(cond
			[(eqv? y x) #t]
			[else (pred y)]
			))))
;;6
(define walk-symbol
	(lambda (x ls)
		(cond
			[(eqv? #f (assv x ls)) x ]
			[(not (symbol? (cdr (assv x ls)))) (cdr (assv x ls))]
			[else (walk-symbol (cdr (assv x ls)) ls)]
			)))
;;7
(define list-ref
	(lambda (ls n)
		(letrec
			((nth-cdr
				(lambda (n)
					(cond
						[(eqv? n 0) ls]
						[else (cdr(nth-cdr (- n 1) ))]
						))))
		(car (nth-cdr n)))))
