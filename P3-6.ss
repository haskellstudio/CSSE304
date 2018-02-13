;;Yifei Li
;;P01
(load "parenthec.ss")
(define-registers expr env k y c a v)
(define-program-counter pc)
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
(define-label value-of-cps
	    (union-case expr exprn
            [(const cexp) (begin 
                            (set! k k)
                            (set! v cexp)
                            [apply-k]
            )]
            [(mult nexp1 nexp2) (begin  (set! expr nexp1)
                                        (set! env env)
                                        (set! k (kt_outer-mult nexp2 env k)) 
                                        [value-of-cps]
            )]
			[(sub1 nexp) (begin (set! expr nexp)
                                (set! env env)
                                (set! k (kt_sub1 k))
                                [value-of-cps]
            )]
			[(zero nexp) (begin (set! expr nexp)
                                (set! env env)
                                (set! k (kt_zero k)) 
                                [value-of-cps]
            )]
			[(if test conseq alt) (begin (set! expr test)
                                         (set! env env)
                                         (set! k (kt_if conseq alt env k)) 
                                         [value-of-cps]
            )]
			[(letcc body) (begin (set! expr body)
                                 (set! env (envr_env-cps k env))
                                 (set! k k) 
                                 [value-of-cps]
            )]
			[(throw kexp vexp) (begin (set! expr kexp)
                                      (set! env env)
                                      (set! k (kt_throw vexp env)) 
                                      [value-of-cps]
            )]
			[(let exp body) (begin (set! expr exp)
                                   (set! env env)
                                   (set! k (kt_let body env k)) 
                                   [value-of-cps]
            )]
			[(var n) (begin (set! env env)
                            (set! y n)
                            (set! k^ k)
                            [apply-env]
            )]
			[(lambda body) (begin (set! k k)
                                  (set! v (clos_closure body env k))
                                  [apply-k]
            )]
			[(app rator rand) (begin (set! expr rator)
                                     (set! env env)
                                     (set! k (kt_app-outer rand env k)) 
                                     [value-of-cps]
            )]
			))

;;environment
(define-union envr
    (env-cps a env^)
    (empty))

(define-label apply-env
		(union-case env envr
			[(empty) (error 'value-of "unbound identifier")]
            [(env-cps a env^) (if (zero? y)
                                 (begin (set! k k^) (set! v a) 
                                       [apply-k]) 
                                 (begin (set! env env^) (set! y (sub1 y)) (set! k k^) 
                                       [apply-env]))]
            ))
(define empty-env
    (lambda ()
        (envr_empty)))

;;closure
(define-union clos
    (closure body env^ k^)
    )

(define-label apply-closure
		(union-case c clos
            [(closure body env^ k^) (begin (set! expr body) (set! env (envr_env-cps a env^)) (set! k k)
                                        [value-of-cps])]
			))
;;continuation        
(define-union kt
    (empty)
    (let body^ env^ k^)
    (throw v-exp^ env^)
    (if conseq^ alt^ env^ k^)
    (zero k^)
    (sub1 k^)
    (inner-mult c^ k^)
    (outer-mult x2^ env^ k^)
    (app-inner c^ k^)
    (app-outer rand^ env^ k^)
    )
(define-label apply-k
        (union-case k kt
            [(empty) v]
            [(sub1 k^) (begin (set! k k^) (set! v (sub1 v))
                             [apply-k])]
            [(zero k^) (begin (set! k k^) (set! v (zero? v))
                             [apply-k])]
            [(if conseq^ alt^ env^ k^ ) (if v 
                                            (begin (set! expr conseq^) (set! env env^) (set! k k^)
                                                  [value-of-cps]) 
                                            (begin (set! expr alt^) (set! env env^) (set! k k^)
                                                  [value-of-cps])
                                        )]
            [(throw v-exp^ env^) (begin (set! expr v-exp^) (set! env env^) (set! k v)
                                        [value-of-cps])]
            [(let body^ env^ k^) (begin (set! expr body^) (set! env (envr_env-cps v env^)) (set! k k^)
                                        [value-of-cps])]
            [(inner-mult c^ k^) (begin (set! k k^) (set! v (* c^ v))
                                        [apply-k] )]
            [(outer-mult x2^ env^ k^) (begin (set! expr x2^) (set! env env^) (set! k (kt_inner-mult v k^))
                                            [value-of-cps])]
            [(app-inner c^ k^) (begin (set! c c^) (set! a v) (set! k k^)
                                     [apply-closure] )]
            [(app-outer rand^ env^ k^) (begin (set! expr rand^) (set! env env^) (set! k (kt_app-inner v k^))
                                              [value-of-cps] )]
            ))


;;main
(define-label main
        (begin
            (set! expr (exprn_let
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
            (exprn_const 5))) )
            (set! env (empty-env))
            (set! k (kt_empty))
            [value-of-cps] ))

