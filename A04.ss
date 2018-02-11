(load "pmatch.ss")
;;A04
;;Yifei Li

;;1
(define lex
	(lambda (e ls)
		(pmatch e
			[,y (guard (symbol? y))  (list 'var (list-index-of-eqv? y ls)) ]
			[,y (guard (number? y))  (list 'const y) ]
			[(lambda (,x) ,body) `(lambda ,(lex body (cons x ls)))]
			[(zero? ,a) `(zero? ,(lex a ls))]
			[(sub1 ,a) `(sub1 ,(lex a ls))]
			[(* ,num1 ,num2) `(* ,(lex num1 ls) ,(lex num2 ls))]
			[(if ,cond ,t ,f) `(if ,(lex cond ls) ,(lex t ls) ,(lex f ls))]
			[(let ((,x ,e)) ,body) `( let ,(lex e (cons x ls)) ,(lex body (cons x ls)))]
			[(,rator ,rand) `(,(lex rator ls) ,(lex rand ls)) ]
			)))

(define list-index-of-eqv?
	(lambda (x ls)
		(cond
			((eqv? x (car ls)) 0)
			(else (add1 (list-index-of-eqv? x (cdr ls)))
				))))
;;2
(define value-of-dynamic
	(lambda (exp env)
		(pmatch exp
			[,n (guard(number? n)) n]
			[,b (guard(boolean? b)) b]
			[,y (guard(symbol? y)) (apply-env env y) ]
			[(lambda(,x) ,body) 
			 (lambda(a env1) (value-of-dynamic body (extend-env x a env1)))]
			[(zero? ,a) (zero? (value-of-dynamic a env))]
			[(sub1 ,a) (sub1 (value-of-dynamic a env))]
			[(* ,num1 ,num2) (* (value-of-dynamic num1 env) (value-of-dynamic num2 env))]
			[(if ,cond ,t ,f) (if (value-of-dynamic cond env) (value-of-dynamic t env) (value-of-dynamic f env))]
			[(let ((,x ,e)) ,body) (value-of-dynamic body (extend-env x (value-of-dynamic e env) env))]
			[(car ,ls) (car (value-of-dynamic ls env))]
			[(null? ,a) (null? (value-of-dynamic a env))]
			[(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
			[(cdr ,ls) (cdr (value-of-dynamic ls env))]
			[('quote ,v) v]
			[(,rator ,rand) ((value-of-dynamic rator env) (value-of-dynamic rand env) env)]
			)))
;;apply-env
(define apply-env
	(lambda (env y) 
		(env y)))
;;empty-env
(define empty-env
	(lambda()
		(lambda(y)  (error 'value-of-dynamic "unbound variable ~a" y))
		))
;;extend-env
(define extend-env
	(lambda (x a env)
		(lambda (y) (if (eqv? x y) a (env y))
			)))

