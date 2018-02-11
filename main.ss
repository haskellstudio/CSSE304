;;Yifei Li
;;A03b
(load "pmatch.ss")
;;1
(define value-of-fn
	(lambda (exp env)
		(pmatch exp
			[,n (guard(number? n)) n]
			[,b (guard(boolean? b)) b]
			[,y (guard(symbol? y)) (apply-env-fn env y) ]
			[(lambda(,x) ,body) 
			 (make-closure-fn x body env)]
			[(zero? ,a) (zero? (value-of-fn a env))]
			[(sub1 ,a) (sub1 (value-of-fn a env))]
			[(* ,num1 ,num2) (* (value-of-fn num1 env) (value-of-fn num2 env))]
			[(if ,cond ,t ,f) (if (value-of-fn cond env) (value-of-fn t env) (value-of-fn f env))]
			[(let ((,x ,e)) ,body) (value-of-fn body (extend-env-fn x (value-of-fn e env) env))]
			[(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))]
			)))
;;apply-env-fn
(define apply-env-fn
	(lambda (env y)
		(env y)))
;;empty-env-fn
(define empty-env-fn
	(lambda()
		(lambda(y)  (error 'value-of-fn "unbound variable ~a" y))
		))
;;extend-env-fn
(define extend-env-fn
	(lambda (x a env)
		(lambda (y) (if (eqv? x y) a (env y))
			)))
;;make-closure-fn
(define make-closure-fn
	(lambda (x body env)
		(lambda(a) (value-of-fn body (extend-env-fn x a env))
			)))
;;apply-closure-fn
(define apply-closure-fn
	(lambda (c a)
		(c a)))

;;2
(define value-of-ds
	(lambda (exp env)
		(pmatch exp
			[,n (guard(number? n)) n]
			[,b (guard(boolean? b)) b]
			[,y (guard(symbol? y)) (apply-env-ds env y) ]
			[(lambda(,x) ,body) 
			 (make-closure-ds x body env)]
			[(zero? ,a) (zero? (value-of-ds a env))]
			[(sub1 ,a) (sub1 (value-of-ds a env))]
			[(* ,num1 ,num2) (* (value-of-ds num1 env) (value-of-ds num2 env))]
			[(if ,cond ,t ,f) (if (value-of-ds cond env) (value-of-ds t env) (value-of-ds f env))]
			[(let ((,x ,e)) ,body) (value-of-ds body (extend-env-ds x (value-of-ds e env) env))]
			[(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))]
			)))
;;apply-env-ds
(define apply-env-ds
	(lambda (env y)
		(cond
			[(eqv? env '()) (error 'value-of-ds "unbound variable ~a" y)]
			[else (cdr (assv y env))]
			)))
;;empty-env-ds
(define empty-env-ds
	(lambda()
		'()
		))
;;extend-env-ds
(define extend-env-ds
	(lambda (x a env)
		(cons (cons x a) env)
		))
;;make-closure-ds
(define make-closure-ds
	(lambda (x body env)
		(lambda(a) (list x body (extend-env-ds x a env))
			)))
;;apply-closure-ds
(define apply-closure-ds
	(lambda (c a)
		(pmatch (c a)
			[(,x ,body ,env) (value-of-ds body env)]
			)))