;;Yifei Li
;;P01
(load "pmatch.ss")
(load "parenthec.ss")
;;define-union exprn val-of-cps
(define-union exprn
    (const cexp)
    (var n)
    (if test conseq alt)
    (mult nexp1 nexp2)
    (sub1 nexp)
    (zero nexp)
    (letcc body)
    (throw kexp vexp)
    (let exp body)
    (lambda body)
    (app rator rand))
  
;;interpreter-cps
(define value-of-cps
	(lambda (expr env k)
		(union-case expr exprn
			[(const cexp) (apply-k k cexp)]
			[(mult nexp1 nexp2) (value-of-cps nexp1 env (make-outer-k-mult nexp2 env k))]
			[(sub1 nexp) (value-of-cps nexp env (make-sub1-k k))]
			[(zero nexp) (value-of-cps nexp env (make-zero-k k))]
			[(if test conseq alt) (value-of-cps test env (make-if-k conseq alt env k))]
			[(letcc body) (value-of-cps body (envr_env-cps k env) k) ]
			[(throw kexp vexp) (value-of-cps kexp env (make-throw-k vexp env))]
			[(let exp body) (value-of-cps exp env (make-let-k body env k))]
			[(var n) (apply-env env n k)]
			[(lambda body) (apply-k k (clos_closure body env k))]
			[(app rator rand) (value-of-cps rator env (make-outer-k-app rand env k)) ]
			)))

(define make-let-k
	(lambda (body^ env^ k^)
		`(let ,body^ ,env^ ,k^)))

(define make-throw-k
	(lambda (v-exp^ env^)
		`(throw ,v-exp^ ,env^)))

(define make-if-k
	(lambda (conseq^ alt^ env^ k^)
		`(if ,conseq^ ,alt^ ,env^ ,k^)))

(define make-zero-k
	(lambda (k^)
		`(zero ,k^)))

(define make-sub1-k
	(lambda (k^)
		`(sub1 ,k^)))

(define make-inner-k-mult
	(lambda (c^ k^)
		`(inner-mult ,c^ ,k^)))

(define make-outer-k-mult
	(lambda (x2^ env^ k^)
		`(outer-mult ,x2^ ,env^ ,k^)))

(define make-inner-k-app
	(lambda (c^ k^)
		`(app-inner ,c^ ,k^)))

(define make-outer-k-app
	(lambda (rand^ env^ k^)
		`(app-outer ,rand^ ,env^ ,k^)))

(define empty-env
	(lambda ()
		`(envr_empty)))

(define empty-k
	(lambda ()
        `(empty) ))

;;define-union envr apply-env
(define-union envr
    (env-cps a env)
    (empty))

(define apply-env
	(lambda (env y k^)
		(union-case env envr
			[(empty) (error 'value-of "unbound identifier")]
			[(env-cps a env) (if (zero? y) (apply-k k^ a) (apply-env env (sub1 y) k^))]
			)))
;;define-union clos apply-clousre
(define-union clos
    (closure body env x)
    )

(define apply-closure
    (lambda (c a k)
		(union-case c clos
			[(closure body env x) (value-of-cps body (envr_env-cps a env) k)]
			)))

(define apply-k
	(lambda (k v)
		(pmatch k
			[(empty) v]
			[(sub1 ,k^) (apply-k k^ (sub1 v))]
			[(zero ,k^) (apply-k k^ (zero? v))]
			[(if ,conseq^ ,alt^ ,env^ ,k^ ) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^) )]
			[(throw ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v)]
			[(let ,body^ ,env^ ,k^) (value-of-cps body^ (envr_env-cps v env^) k^)]
			[(inner-mult ,c^ ,k^) (apply-k k^ (* c^ v))]
			[(outer-mult ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (make-inner-k-mult v k^))]
			[(app-inner ,c^ ,k^) (apply-closure c^ v k^)]
			[(app-outer ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (make-inner-k-app v k^))]
			)))
         
;;main
(define main
    (lambda ()
        (value-of-cps
            (exprn_let
                (exprn_lambda
                    (exprn_lambda
                        (exprn_if
                            (exprn_zero (exprn_var 0))
                            (exprn_const 1)
                            (exprn_mult (exprn_var 0)
                                (exprn_app (exprn_app (exprn_var 1) (exprn_var 1))
                                            (exprn_sub1 (exprn_var 0)))))))
                (exprn_mult
                    (exprn_letcc
                        (exprn_app
                            (exprn_app (exprn_var 1) (exprn_var 1))
                            (exprn_throw (exprn_var 0)
                                (exprn_app (exprn_app (exprn_var 1) (exprn_var 1))
                                        (exprn_const 4)))))
            (exprn_const 5)))
        (empty-env)
		(empty-k))))
(main)
    
