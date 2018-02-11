
(define (test-lambda->lumbda)
  (let ([correct '(x 
		   (lumbda (x) x)
		   (lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a)))))
		   (lumbda (lambda) lambda)
		   ((lumbda (lambda) lambda) (lumbda (y) y))
		   ((lumbda (x) x) (lumbda (x) x)))]
	[answers (list
		  (lambda->lumbda 'x)
		  (lambda->lumbda '(lambda (x) x))
		  (lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
		  (lambda->lumbda '(lambda (lambda) lambda)) 
		  (lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y)))
		  (lambda->lumbda '((lambda (x) x) (lambda (x) x)))
		  )])
  (display-results correct answers equal?)))

(define (test-var-occurs?)
  (let ([correct '(   #t   #f   #t   #f   #t)]
	[answers (list
		  (var-occurs? 'x 'x) 
		  (var-occurs? 'x '(lambda (x) y))
		  (var-occurs? 'x '(lambda (y) x))
		  (var-occurs? 'lambda '(lambda (x) y))
		  (var-occurs? 'x '((z y) x))
		  )])
    (display-results correct answers equal?)))

(define (test-vars)
  (let ([correct '(   (x)   (x)   (x x x y)   (a z h h a))]
	[answers
	 (list
	  (vars 'x)
	  (vars '(lambda (x) x))
	  (vars '((lambda (y) (x x)) (x y)))
	  (vars '(lambda (z) ((lambda (y) (a z))
			      (h (lambda (x) (h a))))))
	  )])
    (display-results correct answers set-equals?)))

(define (test-unique-vars)
  (let ([correct '(   (x y)   (z y x)   (c b a))]
	[answers
	 (list
	  (unique-vars '((lambda (y) (x x)) (x y)))
	  (unique-vars '((lambda (z) (lambda (y) (z y))) x))
	  (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))
	  )])
    (display-results correct answers set-equals?)))

(define (test-var-occurs-free?)
  (let ([correct '(   #t   #f   #f   #f   #t   #t   #t)]
	[answers
	 (list
	  (var-occurs-free? 'x 'x)
	  (var-occurs-free? 'x '(lambda (y) y))
	  (var-occurs-free? 'x '(lambda (x) (x y)))
	  (var-occurs-free? 'x '(lambda (x) (lambda (x) x))) 
	  (var-occurs-free? 'y '(lambda (x) (x y)))
	  (var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))
	  (var-occurs-free? 'x '((lambda (x) (x x)) (x x)))
	  )])
    (display-results correct answers equal?)))

(define (test-var-occurs-bound?)
  (let ([correct '(    #f    #t    #f    #t    #f    #t    #f    #t)]
	[answers
	 (list
	  (var-occurs-bound? 'x 'x)
	  (var-occurs-bound? 'x '(lambda (x) x))
	  (var-occurs-bound? 'y '(lambda (x) x))
	  (var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
	  (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
	  (var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
	  (var-occurs-bound? 'x '(lambda (x) y))
	  (var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))
	  )])
    (display-results correct answers equal?)))

(define (test-unique-free-vars)
  (let ([correct '(    (x)    (y)    (y e x))]
	[answers
	 (list
	  (unique-free-vars 'x)
	  (unique-free-vars '(lambda (x) (x y)))
	  (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
	  )])
    (display-results correct answers set-equals?)))

(define (test-unique-bound-vars) 
  (let ([correct '(    ()    ()    (x)    (x c)    (y)    ()    (x))]
	[answers
	 (list
	  (unique-bound-vars 'x)
	  (unique-bound-vars '(lambda (x) y))
	  (unique-bound-vars '(lambda (x) (x y)))
	  (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
	  (unique-bound-vars '(lambda (y) y))
	  (unique-bound-vars '(lambda (x) (y z)))
	  (unique-bound-vars '(lambda (x) (lambda (x) x)))
	  )])
    (display-results correct answers set-equals?)))


(define (test-lex)
  (let ([answers '((lambda (var 0))
		   (lambda (lambda (var 1)))
		   (lambda (lambda ((var 0) (var 1))))
		   (lambda (lambda ((var 0) (var 0))))
		   (lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1))))))
		   (lambda
		       (lambda
			   (lambda
			       (lambda
				   (lambda
				       (lambda
					   (lambda
					       (lambda
						   ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1))))))))))
		   (lambda 
		       (lambda 
			   (lambda 
			       (lambda 
				   (lambda 
				       (lambda 
					   ((lambda
						(lambda
						    (lambda
							((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
					    (lambda
						(lambda
						    (lambda
							((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))]
	[correct
	 (list
	  (lex '(lambda (x) x) '())
	  (lex '(lambda (y) (lambda (x) y)) '())
	  (lex '(lambda (y) (lambda (x) (x y))) '())
	  (lex '(lambda (x) (lambda (x) (x x))) '())
	  (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
	  (lex '(lambda (a)
		  (lambda (b)
		    (lambda (c)
		      (lambda (a)
			(lambda (b)
			  (lambda (d)
			    (lambda (a)
			      (lambda (e)
				(((((a b) c) d) e) a))))))))) '())
	  (lex '(lambda (a)
		  (lambda (b)
		    (lambda (c)
		      (lambda (w)
			(lambda (x)
			  (lambda (y)
			    ((lambda (a)
			       (lambda (b)
				 (lambda (c)
				   (((((a b) c) w) x) y))))
			     (lambda (w)
			       (lambda (x)
				 (lambda (y)
				   (((((a b) c) w) x) y))))))))))) '()))])
    (display-results correct answers equal?)))


;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))

(define set-equals?  ; are these list-of-symbols equal when
  (lambda (s1 s2)    ; treated as sets?
    (if (or (not (list? s1)) (not (list? s2)))
        #f
        (not (not (and (is-a-subset? s1 s2) (is-a-subset? s2 s1)))))))

(define is-a-subset?
  (lambda (s1 s2)
    (andmap (lambda (x) (member x s2))
      s1)))

;; You can run the tests individually, or run them all
;; by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'test-lambda->lumbda)
  (test-lambda->lumbda)
  (display 'test-var-occurs?)
  (test-var-occurs?)
  (display 'test-vars)
  (test-vars)
  (display 'test-unique-vars)
  (test-unique-vars)
  (display 'test-var-occurs-free?)
  (test-var-occurs-free?)
  (display 'test-var-occurs-bound?)
  (test-var-occurs-bound?)
  (display 'test-unique-free-vars)
  (test-unique-free-vars)
  (display 'test-unique-bound-vars)
  (test-unique-bound-vars)
  (display 'test-lex)
  (test-lex))

(define r run-all)



