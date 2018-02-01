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
			[(mult ,x1 ,x2) (value-of-cps x1 env (lambda (v) 
													(value-of-cps x2 env (lambda(w)
																			(apply-k k (* v w))))))]
			[(sub1 ,x) (value-of-cps x env (lambda (v) (apply-k k (sub1 v))))]
			[(zero ,x) (value-of-cps x env (lambda (v) (apply-k k (zero? v))))]
			[(if ,test ,conseq ,alt) (value-of-cps test env (lambda (v) (if v (value-of-cps conseq env k) (value-of-cps alt env k) )))]
			[(letcc ,body) (value-of-cps body (extend-env k env) k) ]
			[(throw ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k) (value-of-cps v-exp env k)))]
			[(let ,e ,body) (value-of-cps e env (lambda (a) (value-of-cps body (extend-env a env) k)))]
			[(var ,expr) (apply-env env expr k)]
			[(lambda ,body) (apply-k k (lambda (a k) (value-of-cps body (extend-env a env) k)))]
			[(app ,rator ,rand) (value-of-cps rator env (lambda (v) (value-of-cps rand env (lambda (w) (apply-closure v w k))))) ]
			)))


(define empty-env
	(lambda ()
		(lambda (y)
			(error 'value-of "unbound identifier"))))

(define empty-k
	(lambda ()
		(lambda (v)
			v)))

(define apply-env
	(lambda (env y k^)
		(pmatch env
			[(env-cps ,a ,env) (if (zero? y) (apply-k k^ a) (apply-env env (sub1 y) k^))]
			)))

(define apply-closure
	(lambda (c a k)
		(c a k)))

(define apply-k
	(lambda (k v)
		(k v)))

(define extend-env
	(lambda (a env)
			`(env-cps ,a ,env)))