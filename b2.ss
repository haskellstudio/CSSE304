;;Yifei Li
;;Bonus 2

(load "pmatch.ss")
(define value-of
    (lambda (exp)
        (pmatch exp
            [,n (guard (number? n))
                (lambda (env) n)]
            [,x (guard (symbol? x))
                (lambda (env) (env x))]
            [(lambda (,x) ,body)
                (let ((sbody (value-of body)))
                    (lambda (env)
                        (lambda (a)
                            (sbody (lambda (y) (if (eqv? x y) a (env y)))))))]
            [(zero? ,nexp)
                (let ((s (value-of nexp)))
                    (lambda (env)
                        (zero? (s env))))]      
            [(sub1 ,nexp)
                (let ((s (value-of nexp)))
                    (lambda (env)
                        (sub1 (s env))))]
            [(,rator ,rand)
                (let ((r (value-of rator)) (s (value-of rand)))
                    (lambda (env)
                        ((r env) (s env))))]
            [(let ((,x ,e)) ,b)
                (let ((r (value-of e)) (s (value-of b)))
                    (lambda (env)
                        (let ((a (r env)))
                            (s (lambda (y) (if (eqv? x y) a (env y)))))))]
            [(if ,t ,c ,a)
                (let ((x (value-of t)) (y (value-of c)) (z (value-of a)))
                    (lambda (env)
                        (if (x env) (y env) (z env))))])))

(define eval-exp
    (lambda (exp)   
        (let ((f (value-of exp)))
;; to test, instead call (time (f (init-env)))
            (f (init-env)))))
            
(define (init-env)
    (lambda (y)
        (error 'value-of "badness :-(")))