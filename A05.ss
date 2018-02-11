;;A05
;;Yifei Li
(load "pmatch.ss")
;;1
(define value-of-cbv
	(lambda (exp env)
		(pmatch exp
			[,c (guard (or (number? c) (boolean? c))) c]
			[(begin2 ,e1 ,e2) (begin (value-of-cbv e1 env) (value-of-cbv e2 env)) ]
			[(zero? ,n) (zero? (value-of-cbv n env))]
			[(sub1 ,n) (sub1 (value-of-cbv n env))]
			[(* ,ne1 ,ne2) (* (value-of-cbv ne1 env) (value-of-cbv ne2 env))]
			[(if ,test ,conseq ,alt) (if (value-of-cbv test env)
										 (value-of-cbv conseq env)
										 (value-of-cbv alt env))]
			[,y (guard (symbol? y)) (unbox (apply-env env y))]
			[(lambda (,x) ,body) (lambda (a) (value-of-cbv body (extend-env x a env)))]
			[(,rator ,y) (guard (symbol? y)) ((value-of-cbv rator env) (box (unbox(apply-env env y))))]
			[(,rator ,rand) ((value-of-cbv rator env)
							(box (value-of-cbv rand env)))]
			[(set! ,y ,exp)　(set-box! (apply-env env y) (value-of-cbv exp env))])))
;;2
(define value-of-cbr
	(lambda (exp env)
		(pmatch exp
			[,c (guard (or (number? c) (boolean? c))) c]
			[(begin2 ,e1 ,e2) (begin (value-of-cbr e1 env) (value-of-cbr e2 env)) ]
			[(zero? ,n) (zero? (value-of-cbr n env))]
			[(sub1 ,n) (sub1 (value-of-cbr n env))]
			[(* ,ne1 ,ne2) (* (value-of-cbr ne1 env) (value-of-cbr ne2 env))]
			[(if ,test ,conseq ,alt) (if (value-of-cbr test env)
										 (value-of-cbr conseq env)
										 (value-of-cbr alt env))]
			[,y (guard (symbol? y)) (unbox (apply-env env y))]
			[(,rator ,y) (guard (symbol? y)) ((value-of-cbr rator env)  (apply-env env y))]
			[(lambda (,x) ,body) (lambda (a) (value-of-cbr body (extend-env x a env)))]
			[(,rator ,rand) ((value-of-cbr rator env)
							 (box (value-of-cbr rand env)))]
			[(set! ,y ,exp)　(set-box! (apply-env env y) (value-of-cbr exp env))])))
;;3
(define value-of-cbname
	(lambda (exp env)
		(pmatch exp
			[,c (guard (or (number? c) (boolean? c))) c]
			[(random ,exp) (random (value-of-cbname exp env))]
			[(zero? ,n) (zero? (value-of-cbname n env))]
			[(sub1 ,n) (sub1 (value-of-cbname n env))]
			[(* ,ne1 ,ne2) (* (value-of-cbname ne1 env) (value-of-cbname ne2 env))]
			[(if ,test ,conseq ,alt) (if (value-of-cbname test env)
										 (value-of-cbname conseq env)
										 (value-of-cbname alt env))]
			[,y (guard (symbol? y)) ((unbox (apply-env env y)))]
			[(,rator ,y) (guard (symbol? y)) ((value-of-cbname rator env)  (apply-env env y))]
			[(lambda (,x) ,body) (lambda (a) (value-of-cbname body (extend-env x a env)))]
			[(,rator ,rand) ((value-of-cbname rator env)
							 (box (lambda () (value-of-cbname rand env)) ))])))
;;4
(define value-of-cbneed
	(lambda (exp env)
		(pmatch exp
			[,c (guard (or (number? c) (boolean? c))) c]
			[(random ,exp) (random (value-of-cbneed exp env))]
			[(zero? ,n) (zero? (value-of-cbneed n env))]
			[(sub1 ,n) (sub1 (value-of-cbneed n env))]
			[(* ,ne1 ,ne2) (* (value-of-cbneed ne1 env) (value-of-cbneed ne2 env))]
			[(if ,test ,conseq ,alt) (if (value-of-cbneed test env)
										 (value-of-cbneed conseq env)
										 (value-of-cbneed alt env))]
			[,y (guard (symbol? y)) ((unbox/need (apply-env env y)))]
			[(,rator ,y) (guard (symbol? y)) ((value-of-cbneed rator env)  (apply-env env y))]
			[(lambda (,x) ,body) (lambda (a) (value-of-cbneed body (extend-env x a env)))]
			[(,rator ,rand) ((value-of-cbneed rator env)
							 (box (lambda () (value-of-cbneed rand env)) ))])))

;;other stuff
;;apply-env
(define apply-env
	(lambda (env y) 
		(env y)))
;;empty-env
(define empty-env
	(lambda()
		(lambda(y)  (error 'valof "unbound variable ~a" y))
		))
;;extend-env
(define extend-env
	(lambda (x a env)
		(lambda (y) (if (eqv? x y) a (env y))
			)))
;;unbox/need
(define unbox/need
	(lambda (b)
		(let ( (v ((unbox b)))) 
			(set-box! b (lambda () v))
			(unbox b)
			)))