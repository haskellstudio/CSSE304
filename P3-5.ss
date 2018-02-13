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
            [(const cexp) (let* ((k k)
                                 (v cexp))
                                 [apply-k k v]
            )]
            [(mult nexp1 nexp2) (let* ((expr nexp1)
                                        (env env)
                                        (k (kt_outer-mult nexp2 env k))) 
                                        [value-of-cps expr env k]
            )]
			[(sub1 nexp) (let* ((expr nexp)
                                (env env)
                                (k (kt_sub1 k))) 
                                [value-of-cps expr env k]
            )]
			[(zero nexp) (let* ((expr nexp)
                                (env env)
                                (k (kt_zero k))) 
                                [value-of-cps expr env k]
            )]
			[(if test conseq alt) (let* ((expr test)
                                         (env env)
                                         (k (kt_if conseq alt env k))) 
                                         [value-of-cps expr env k]
            )]
			[(letcc body) (let* ((expr body)
                                 (env (envr_env-cps k env))
                                 (k k)) 
                                 [value-of-cps expr env k]
            )]
			[(throw kexp vexp) (let* ((expr kexp)
                                      (env env)
                                      (k (kt_throw vexp env))) 
                                      [value-of-cps expr env k]
            )]
			[(let exp body) (let* ((expr exp)
                                   (env env)
                                   (k (kt_let body env k))) 
                                   [value-of-cps expr env k]
            )]
			[(var n) (let* ((env env)
                            (y n)
                            (k^ k))
                            [apply-env env y k^]
            )]
			[(lambda body) (let* ((k k)
                                  (v (clos_closure body env k)))
                                  [apply-k k v]
            )]
			[(app rator rand) (let* ((expr rator)
                                     (env env)
                                     (k (kt_app-outer rand env k))) 
                                     [value-of-cps expr env k]
            )]
			)))

;;environment
(define-union envr
    (env-cps a env)
    (empty))

(define apply-env
	(lambda (env y k^)
		(union-case env envr
			[(empty) (error 'value-of "unbound identifier")]
            [(env-cps a env) (if (zero? y)
                                 (let* ((k k^) (v a) )
                                       [apply-k k v]) 
                                 (let* ((env env) (y (sub1 y)) (k^ k^) )
                                       [apply-env env y k^]))]
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
            [(closure body env x) (let* ((expr body) (env (envr_env-cps a env)) (k k))
                                        [value-of-cps expr env k])]
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
            [(sub1 k^) (let* ((k k^) (v (sub1 v)))
                             [apply-k k v])]
            [(zero k^) (let* ((k k^) (v (zero? v)))
                             [apply-k k v])]
            [(if conseq^ alt^ env^ k^ ) (if v 
                                            (let* ((expr conseq^) (env env^) (k k^))
                                                  [value-of-cps expr env k]) 
                                            (let* ((expr alt^) (env env^) (k k^))
                                                  [value-of-cps expr env k])
                                        )]
            [(throw v-exp^ env^) (let* ((expr v-exp^) (env env^) (k v))
                                        [value-of-cps expr env k])]
            [(let body^ env^ k^) (let* ((expr body^) (env (envr_env-cps v env^)) (k k^))
                                        [value-of-cps expr env k])]
            [(inner-mult c^ k^) (let* ((k k^) (v (* c^ v)))
                                        [apply-k k v])]
            [(outer-mult x2^ env^ k^) (let* ((expr x2^) (env env^) (k (kt_inner-mult v k^)))
                                            [value-of-cps expr env k])]
            [(app-inner c^ k^) (let* ((c c^) (a v) (k k^))
                                     [apply-closure c a k])]
            [(app-outer rand^ env^ k^) (let* ((expr rand^) (env env^) (k (kt_app-inner v k^)))
                                             [value-of-cps expr env k])]
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
    
