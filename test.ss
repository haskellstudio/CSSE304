(load "pmatch.ss")

(define interp-arith
	(lambda (exp)
		(pmatch exp
			[,y (guard (number? y)) y]
			[(,a + ,b) (+ (interp-arith a) (interp-arith b))]
			[(,a * ,b) (* (interp-arith a) (interp-arith b))]
			[(,a - ,b) (- (interrp-arith a) (interp-arith b))]
			[(,a ^ ,b) (expt (interp-arith a) (interp-arith b))]
			[(add1 ,a) (add1 (interp-arith a))]
			[(sub1 ,a) (sub1 (interp-arith a))]
			)))

(define takef
	(lambda (f ls)
		(cond
			[(eqv? (f  (car ls)) #f ) '()]
			[else ( cons (car ls) (takef f (cdr ls)))]
			)))

(define add-between
	(lambda (x ls)
		(cond
			[(null? ls) '()]
			[(null? (cdr ls)) (list (car ls))]
			[else ( cons (car ls) (cons x (add-between x (cdr ls)) ) )]
			)))

(define foo
	(lambda (x y)
		(cond
			[(null? x) '()]
			[(memv (car x) y) (cons (car x) (foo (cdr x) y))]
			[else (foo (cdr x) y)]
			)))