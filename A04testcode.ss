
(define (test-lex)
  (let ([correct '(((lambda (var 0)) (const 5))
		   (lambda
		       (lambda
			   (if (zero? (var 0))
			       (const 1)
			       (* (var 0) ((var 1) (sub1 (var 0)))))))
		   (let (lambda
			    (lambda
				(if (zero? (var 0))
				    (const 1)
				    (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
		     (((var 0) (var 0)) (const 5))))]
	[answers (list 
		  (lex '((lambda (x) x) 5)  '())
		  
		  (lex '(lambda (!)
			  (lambda (n)
			    (if (zero? n) 1 (* n (! (sub1 n))))))
		       '())
		  
		  (lex '(let ((! (lambda (!)
				   (lambda (n)
				     (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
			  ((! !) 5))
		       '()))])
    (display-results correct answers equal?)))

(define (test-value-of-dynamic)
  (let ([correct '(5 120 120 ((1 1 2 3) (2 2 3) (3 3)))]
	[answers 
	 (list 
	  (value-of-dynamic 
	   '(let ([x 2])
	      (let ([f (lambda (e) x)])
		(let ([x 5])
		  (f 0))))
	   (empty-env))
	  
	  (value-of-dynamic
	   '(let ([! (lambda (n)
		       (if (zero? n) 
			   1
			   (* n (! (sub1 n)))))])
	      (! 5))
	   (empty-env))
	  
	  (value-of-dynamic
	   '((lambda (!) (! 5))
	     (lambda (n)
	       (if (zero? n) 
		   1
		   (* n (! (sub1 n))))))
	   (empty-env))
	  
	  (value-of-dynamic
	   '(let ([f (lambda (x) (cons x l))])
	      (let ([cmap 
		     (lambda (f)
		       (lambda (l)               
			 (if (null? l) 
			     '()
			     (cons (f (car l)) ((cmap f) (cdr l))))))])
		((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
	   (empty-env)))])
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
  (display 'test-lex)
  (test-lex)
  (display 'test-value-of-dynamic)
  (test-value-of-dynamic))

(define r run-all)


