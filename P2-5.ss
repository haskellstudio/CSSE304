;;Yifei Li
;;P01
(load "pmatch.ss")
;;lex
(define lex
	(lambda (e acc)
		(pmatch e
			[,y (guard (symbol? y))  (list 'var (list-index-of-eqv? y acc)) ]
			[,y (guard (number? y))  (list 'const y) ]
			[(lambda (,x) ,body) `(lambda ,(lex body (cons x acc)))]
			[(zero? ,nexp) `(zero ,(lex nexp acc))]
			[(sub1 ,a) `(sub1 ,(lex a acc))]
			[(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
			[(if ,cond ,t ,f) `(if ,(lex cond acc) ,(lex t acc) ,(lex f acc))]
			[(let ((,x ,e)) ,body) `( let ,(lex e (cons x acc)) ,(lex body (cons x acc)))]
			[(let/cc ,x ,b) `(letcc ,(lex b (cons x acc)))]
			[(throw ,kexp ,nexp) `(throw ,(lex kexp acc) ,(lex nexp acc)) ]
			[(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]

			)))

(define list-index-of-eqv?
	(lambda (x acc)
		(cond
			((eqv? x (car acc)) 0)
			(else (add1 (list-index-of-eqv? x (cdr acc)))
				))))

;;interpreter-cps
(trace-define value-of-cps
	(lambda (expr env k)
		(pmatch expr
			[(const ,expr) (apply-k k expr)]
			[(mult ,x1 ,x2) (value-of-cps x1 env (make-outer-k-mult x2 env k))]
			[(sub1 ,x) (value-of-cps x env (make-sub1-k k))]
			[(zero ,x) (value-of-cps x env (make-zero-k k))]
			[(if ,test ,conseq ,alt) (value-of-cps test env (make-if-k conseq alt env k))]
			[(letcc ,body) (value-of-cps body (extend-env k env) k) ]
			[(throw ,k-exp ,v-exp) (value-of-cps k-exp env (make-throw-k v-exp env))]
			[(let ,e ,body) (value-of-cps e env (make-let-k body env k))]
			[(var ,expr) (apply-env env expr k)]
			[(lambda ,body) (apply-k k (make-closure body env k))]
			[(app ,rator ,rand) (value-of-cps rator env (make-outer-k-app rand env k)) ]
			)))

(define make-let-k
	(lambda (body^ env^ k^)
		(lambda (a) (value-of-cps body^ (extend-env a env^) k^))))

(define make-throw-k
	(lambda (v-exp^ env^)
		(lambda (k) (value-of-cps v-exp^ env^ k))))

(define make-if-k
	(lambda (conseq^ alt^ env^ k^)
		(lambda (v)  (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^) ))))

(define make-zero-k
	(lambda (k^)
		(lambda (v) (apply-k k^ (zero? v)))))

(define make-sub1-k
	(lambda (k^)
		(lambda (v) (apply-k k^ (sub1 v)))))

(define make-inner-k-mult
	(lambda (c^ k^)
		(lambda (v) (apply-k k^ (* c^ v)))))

(define make-outer-k-mult
	(lambda (x2^ env^ k^)
		(lambda (c) (value-of-cps x2^ env^ (make-inner-k-mult c k^)))))

(define make-inner-k-app
	(lambda (c^ k^)
		(lambda (v) (apply-closure c^ v k^))))

(define make-outer-k-app
	(lambda (rand^ env^ k^)
		(lambda (c) (value-of-cps rand^ env^ (make-inner-k-app c k^)))))

(define empty-env
	(lambda ()
		`(empty)))

(define empty-k
	(lambda ()
		(lambda (v)
			v)))

(define apply-env
	(lambda (env y k^)
		(pmatch env
			[(empty) (error 'value-of "unbound identifier")]
			[(env-cps ,a ,env) (if (zero? y) (apply-k k^ a) (apply-env env (sub1 y) k^))]
			)))

(define apply-closure
	(lambda (c a k)
		(pmatch c
			[(closure ,body ,env ,x) (value-of-cps body (extend-env a env) k)]
			)))

(define apply-k
	(lambda (k v)
		(k v)))

(define extend-env
	(lambda (a env)
			`(env-cps ,a ,env)))

(define make-closure
	 (lambda (body env k)
	 	`(closure ,body ,env ,k)))