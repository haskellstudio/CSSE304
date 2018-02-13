;;Yifei Li
;;P01
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
			[(mult nexp1 nexp2) (value-of-cps nexp1 env (kt_outer-mult nexp2 env k))]
			[(sub1 nexp) (value-of-cps nexp env (kt_sub1 k))]
			[(zero nexp) (value-of-cps nexp env (kt_zero k))]
			[(if test conseq alt) (value-of-cps test env (kt_if conseq alt env k))]
			[(letcc body) (value-of-cps body (envr_env-cps k env) k) ]
			[(throw kexp vexp) (value-of-cps kexp env (kt_throw vexp env))]
			[(let exp body) (value-of-cps exp env (kt_let body env k))]
			[(var n) (apply-env env n k)]
			[(lambda body) (apply-k k (clos_closure body env k))]
			[(app rator rand) (value-of-cps rator env (kt_app-outer rand env k)) ]
			)))

;;environment
(define-union envr
    (env-cps a env)
    (empty))

(define apply-env
	(lambda (env y k^)
		(union-case env envr
			[(empty) (error 'value-of "unbound identifier")]
			[(env-cps a env) (if (zero? y) (apply-k k^ a) (apply-env env (sub1 y) k^))]
            )))
(define empty-env
    (lambda ()
        (envr_empty)))

;;closure
(define-union clos
    (closure body env x)
    )

(define apply-closure
    (lambda (c a k)
		(union-case c clos
			[(closure body env x) (value-of-cps body (envr_env-cps a env) k)]
			)))
;;continuation        
(define-union kt
    (let ,body^ ,env^ ,k^)
    (throw ,v-exp^ ,env^)
    (if ,conseq^ ,alt^ ,env^ ,k^)
    (zero ,k^)
    (sub1 ,k^)
    (inner-mult ,c^ ,k^)
    (outer-mult ,x2^ ,env^ ,k^)
    (app-inner ,c^ ,k^)
    (app-outer ,rand^ ,env^ ,k^)
    (empty)
    )
(define apply-k
    (lambda (k v)
        (union-case k kt
            [(empty) v]
            [(sub1 k^) (apply-k k^ (sub1 v))]
            [(zero k^) (apply-k k^ (zero? v))]
            [(if conseq^ alt^ env^ k^ ) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^) )]
            [(throw v-exp^ env^) (value-of-cps v-exp^ env^ v)]
            [(let body^ env^ k^) (value-of-cps body^ (envr_env-cps v env^) k^)]
            [(inner-mult c^ k^) (apply-k k^ (* c^ v))]
            [(outer-mult x2^ env^ k^) (value-of-cps x2^ env^ (kt_inner-mult v k^))]
            [(app-inner c^ k^) (apply-closure c^ v k^)]
            [(app-outer rand^ env^ k^) (value-of-cps rand^ env^ (kt_app-inner v k^))]
            )))

(define empty-k
    (lambda ()
        (kt_empty))) 

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
        (empty-k) )))
                
        
(main)
    
