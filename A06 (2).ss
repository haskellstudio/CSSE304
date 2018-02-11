;;A06
;;Yifei Li

(load "pmatch.ss")
;;empty-k defined 
(define empty-k
	(lambda ()
		(let ((once-only #f))
			(lambda (v)
				(if once-only
					(error 'empty-k "You can only invoke the empty continuation once")
					(begin (set! once-only #t) v))))))
;;1
(define binary-to-decimal-cps
	(lambda (n k)
		(cond
			[(null? n) (k 0)]
			[else (binary-to-decimal-cps (cdr n) (lambda (v) (k (+ (car n) (* 2 v)))))])))

;;2
(define times-cps
	(lambda (ls k)
		(cond
			[(null? ls) (k 1)]
			[(zero? (car ls)) (k 0)]
			[else (times-cps (cdr ls) (lambda (v) (k (* (car ls) v))))])))

;;3
(define times-cps-shortcut
	(lambda (ls k)
		(cond
			[(null? ls) (k 1)]
			[(zero? (car ls)) 0 ]
			[else (times-cps (cdr ls) (lambda (v) (k (* (car ls) v))))])))

;;4
(define plus-cps
	(lambda (m k)
		( k (lambda (n k)
			(k (+ m n)) 
			))))

;;5
(define remv-first-9*-cps
	(lambda (ls k)
		(cond
		[(null? ls) (k '())]
		[(pair? (car ls)) (remv-first-9*-cps (car ls) (lambda (v) 
										(remv-first-9*-cps (cdr ls) (lambda (w) 
																		(cond 
																			[(equal? (car ls) v) (k (cons (car ls) w) )]
																			[else (k (cons v (cdr ls) ) )]
																			)) ) ))]
		[(eqv? (car ls) '9) (k (cdr ls) )]
		[else (remv-first-9*-cps (cdr ls) (lambda (v) (k (cons (car ls) v))) )])))

;;6
(define cons-cell-count-cps
	(lambda (ls k)
		(cond
		[(pair? ls)
			(cons-cell-count-cps (car ls) ( lambda (v) 
												(cons-cell-count-cps (cdr ls) (lambda (w) 
																					(k (add1 (+ v w)) )))) )]
		[else (k 0)]
		)))

;;7
(define find-cps
	(lambda (u s k)
		(let ((pr (assv u s )))
			(if pr (find-cps (cdr pr) s k) (k u) ))))

;;8
(define ack-cps
	(lambda (m n k)
		(cond
			[(zero? m) (k (add1 n))]
			[(zero? n) (ack-cps (sub1 m) 1 k)]
			[else (ack-cps m (sub1 n) (lambda (v) 
											(ack-cps (sub1 m) v k) ))])))

;;9
(define M-cps
	(lambda (f k)
		(k (lambda (ls k)
			(cond
				((null? ls) (k '()) )
				(else (M-cps f (lambda (v)
										(f (car ls) (lambda (w) 
														(v (cdr ls) (lambda (m) (k (cons w m)) ) )) ) ) )))))))

;;10
(define use-of-M-cps
	(lambda (k)
		(M-cps (lambda (n k) (k (add1 n)) ) (lambda (v) (v '(1 2 3 4 5) k )) )
		))