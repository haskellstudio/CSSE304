;;Yifei Li
;;A02b
(load "pmatch.ss")
(define union
	(lambda (ls1 ls2)
		(cond
			[(null? ls2) ls1]
			[(eqv? (memv (car ls2) ls1 ) #f) (append (union ls1 (cdr ls2)) (list (car ls2)))]
			[else (union ls1 (cdr ls2))]
			)))

;;1
(define lambda->lumbda
	(lambda (e)
		(pmatch e
			[,y (guard (symbol? y)) y]
			[(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body))]
			[(,rator ,rand) `( ,(lambda->lumbda rator) ,(lambda->lumbda rand))]
			)))
;;2
(define var-occurs?
	(lambda (v e)
		(pmatch e
			[,y (guard (symbol? y)) (eqv? y v)]
			[(lambda (,x) ,body) (var-occurs? v body)]
			[(,rator ,rand) (or (var-occurs? v rator) (var-occurs? v rand))] 
			)))
;;3
(define vars
	(lambda (e)
		(pmatch e
			[,y (guard (symbol? y)) (list y)]
			[(lambda (,x) ,body) (vars body)]
			[(,rator ,rand) (append (vars rator) (vars rand))]
			)))
;;4
(define unique-vars
	(lambda (e)
		(pmatch e
			[,y (guard (symbol? y)) (list y)]
			[(lambda (,x) ,body) (union (unique-vars body) (list x))]
			[(,rator ,rand) (union (unique-vars rator) (unique-vars rand))]
			)))
;;5
(define var-occurs-free?
	(lambda (v e)
		(pmatch e
			[,y (guard (symbol? y)) (eqv? y v)]
			[(lambda (,x) ,body) (and (not (eqv? v x)) (var-occurs-free? v body) )]
			[(,rator ,rand) (or (var-occurs-free? v rator) (var-occurs-free? v rand))] 
			)))
;;6
(define var-occurs-bound?
	(lambda (v e)
		(pmatch e
			[,y (guard (symbol? y)) #f]
			[(lambda (,x) ,body) (if (eqv? v x) (var-occurs? v body) (var-occurs-bound? v body) )]
			[(,rator ,rand) (or (var-occurs-bound? v rator) (var-occurs-bound? v rand))] 
			)))
;;7
(define unique-free-vars
	(lambda (e)
		(pmatch e
			[,y (guard (symbol? y)) (list y)]
			[(lambda (,x) ,body) (if (var-occurs-free? x e) (unique-free-vars body) (remv x (unique-free-vars body)))]
			[(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand)) ]
			)))
;;8
(define unique-bound-vars
	(lambda (e)
		(pmatch e
			[,y (guard (symbol? y)) '() ]
			[(lambda (,x) ,body) (if (var-occurs-bound? x e) (union (unique-bound-vars body) (list x)) (unique-bound-vars body))]
			[(,rator ,rand) (union (unique-bound-vars rator) (unique-bound-vars rand))]
			)))
;;9
(define lex
	(lambda (e ls)
		(pmatch e
			[,y (guard (symbol? y))  (list 'var (list-index-of-eqv? y ls)) ]
			[(lambda (,x) ,body) `(lambda ,(lex body (cons x ls)))]
			[(,rator ,rand) `(,(lex rator ls) ,(lex rand ls)) ]
			)))



(define list-index-of-eqv?
	(lambda (x ls)
		(cond
			((eqv? x (car ls)) 0)
			(else (add1 (list-index-of-eqv? x (cdr ls)))
				))))