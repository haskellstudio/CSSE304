(load "pmatch.ss")
;;A04
;;Yifei Li

;;1
(define lex
	(lambda (e ls)
		(pmatch e
			[,y (guard (symbol? y))  (list 'var (list-index-of-eqv? y ls)) ]
			[,y (guard (number? y))  (list 'const y) ]
			[(lambda (,x) ,body) `(lambda ,(lex body (cons x ls)))]
			[(zero? ,a) `(zero? ,(lex a ls))]
			[(sub1 ,a) `(sub1 ,(lex a ls))]
			[(* ,num1 ,num2) `(* ,(lex num1 ls) ,(lex num2 ls))]
			[(if ,cond ,t ,f) `(if ,(lex cond ls) ,(lex t ls) ,(lex f ls))]
			[(let ((,x ,e)) ,body) `( let ,(lex e (cons x ls)) ,(lex body (cons x ls)))]
			[(,rator ,rand) `(,(lex rator ls) ,(lex rand ls)) ]
			)))

(define list-index-of-eqv?
	(lambda (x ls)
		(cond
			((eqv? x (car ls)) 0)
			(else (add1 (list-index-of-eqv? x (cdr ls)))
				))))