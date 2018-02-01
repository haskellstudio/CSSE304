;;Yifei Li
;;A03
(load "pmatch.ss")
;;1
(define value-of
	(lambda (exp env)
		(pmatch exp
			[,n (guard(number? n)) n]
			[,b (guard(boolean? b)) b]
			[,y (guard(symbol? y)) (env y)]
			[(lambda(,x) ,body) 
			 (lambda(a) (value-of body (lambda(y) (if (eqv? x y) a (env y)))))]
			[(zero? ,a) (zero? (value-of a env))]
			[(sub1 ,a) (sub1 (value-of a env))]
			[(* ,num1 ,num2) (* (value-of num1 env) (value-of num2 env))]
			[(if ,cond ,t ,f) (if (value-of cond env) (value-of t env) (value-of f env))]
			[(let ((,x ,e)) ,body) (value-of body (lambda (y) (if (eqv? x y) (value-of e env) (env y))))]
			[(,rator ,rand) ((value-of rator env) (value-of rand env))]
			)))
;;2
(define fo-eulav
	(lambda (exp env)
		(pmatch exp
			[,n (guard(number? n)) n]
			[,y (guard(symbol? y)) (env y)]
			[(,body (,x) adbmal) 
			 (lambda(a) (fo-eulav body (lambda(y) (if (eqv? x y) a (env y)))))]
			[(,a ?orez) (zero? (fo-eulav a env))]
			[(,a 1bus) (sub1 (fo-eulav a env))]
			[(,num1 ,num2 *) (* (fo-eulav num1 env) (fo-eulav num2 env))]
			[(,f ,t ,cond fi) (if (fo-eulav cond env) (fo-eulav t env) (fo-eulav f env))]
			[(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))]
			)))