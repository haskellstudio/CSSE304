
(define (test-value-of-cbv)
  (let ([correct '(3 55 33 120)]
	[answers 
	 (list
	  (value-of-cbv
	   '((lambda (a)
	       ((lambda (p) (begin2 (p a) a)) (lambda (x) (set! x 4))))
	     3)
	   (empty-env))
	  (value-of-cbv
	   '((lambda (f)
	       ((lambda (g) ((lambda (z) (begin2 (g z) z)) 55))
		(lambda (y) (f y))))
	     (lambda (x) (set! x 44)))
	   (empty-env))
	  (value-of-cbv
	   '((lambda (swap)
	       ((lambda (a) ((lambda (b) (begin2 ((swap a) b) a)) 44)) 33))
	     (lambda (x)
	       (lambda (y)
		 ((lambda (temp) (begin2 (set! x y) (set! y temp))) x))))
	   (empty-env))
	  (value-of-cbv
	   '(((lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y))))))
	      (lambda (!)
		(lambda (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
	     5)
	   (empty-env)))])
    
    (display-results correct answers equal?)))

(define (test-value-of-cbr) 
  (let ([correct '(3 4 44 44 4 3 120)]
	[answers
	 (list
	  (value-of-cbr
	   '((lambda (x) (begin2 (set! x #t) (if x 3 5))) #f)
	   (empty-env))
	  (value-of-cbr
	   '((lambda (a)
	       ((lambda (p) (begin2 (p a) a)) (lambda (x) (set! x 4))))
	     3)
	   (empty-env))
	  (value-of-cbr
	   '((lambda (f)
	       ((lambda (g) ((lambda (z) (begin2 (g z) z)) 55))
		(lambda (y) (f y))))
	     (lambda (x) (set! x 44)))
	   (empty-env))
	  (value-of-cbr
	   '((lambda (swap)
	       ((lambda (a) ((lambda (b) (begin2 ((swap a) b) a)) 44)) 33))
	     (lambda (x)
	       (lambda (y)
		 ((lambda (temp) (begin2 (set! x y) (set! y temp))) x))))
	   (empty-env))
	  (value-of-cbr
	   '((lambda (swap)
	       (((lambda (a) (lambda (b) (begin2 ((swap a) b) (sub1 a))))
		 5)
		3))
	     (lambda (x)
	       (lambda (y)
		 ((lambda (temp) (begin2 (set! y x) (set! x temp))) y))))
	   (empty-env))
	  (value-of-cbr
	   '((lambda (x)
	       ((lambda (func) (begin2 (set! x 3) (func 1)))
		(lambda (_) x)))
	     5)
	   (empty-env))
	  (value-of-cbr
	   '(((lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y))))))
	      (lambda (!)
		(lambda (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
	     5)
	   (empty-env)))])
    (display-results correct answers equal?)))

(define (test-value-of-cbname)
  (let ([correct '(#f 100 120)]
	[answers
	 (list 
	  (let ([random-sieve '((lambda (n)
				  (if (zero? n)
				      (if (zero? n)
					  (if (zero? n)
					      (if (zero? n)
						  (if (zero? n)
						      (if (zero? n)
							  (if (zero? n) #t #f)
							  #f)
						      #f)
						  #f)
					      #f)
					  #f)
				      (if (zero? n)
					  #f
					  (if (zero? n)
					      #f
					      (if (zero? n)
						  #f
						  (if (zero? n)
						      #f
						      (if (zero? n)
							  #f
							  (if (zero? n)
							      #f
							      #t))))))))
				(random 2))])
	    (value-of-cbname random-sieve (empty-env)))
	  (value-of-cbname
	   '((lambda (z) 100) ((lambda (x) (x x)) (lambda (x) (x x))))
	   (empty-env))
	  (value-of-cbname 
	   '(((lambda (f) ((lambda (x) (x x)) (lambda (x) (f (x x)))))
	      (lambda (!)
		(lambda (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
	     5)
	   (empty-env)))])
    (display-results correct answers equal?)))

(define (test-value-of-cbneed)
  (let ([correct '(100 #t 120)]
	[answers
	 (list 
	  (value-of-cbneed
	   '((lambda (z) 100) ((lambda (x) (x x)) (lambda (x) (x x))))
	   (empty-env))
	  (let ([random-sieve '((lambda (n)
				  (if (zero? n)
				      (if (zero? n)
					  (if (zero? n)
					      (if (zero? n)
						  (if (zero? n)
						      (if (zero? n)
							  (if (zero? n) #t #f)
							  #f)
						      #f)
						  #f)
					      #f)
					  #f)
				      (if (zero? n)
					  #f
					  (if (zero? n)
					      #f
					      (if (zero? n)
						  #f
						  (if (zero? n)
						      #f
						      (if (zero? n)
							  #f
							  (if (zero? n)
							      #f
							      #t))))))))
				(random 2))])
	    (value-of-cbneed random-sieve (empty-env)))
	  (value-of-cbneed
	   '(((lambda (f) ((lambda (x) (x x)) (lambda (x) (f (x x)))))
	      (lambda (!)
		(lambda (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
	     5)
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
  (display 'test-value-of-cbv)
  (test-value-of-cbv)
  (display 'test-value-of-cbr)
  (test-value-of-cbr)
  (display 'test-value-of-cbname)
  (test-value-of-cbname)
  (display 'test-value-of-cbneed)
  (test-value-of-cbneed))

(define r run-all)



	  ;; (value-of-cbname
	  ;;  '(((lambda (pi)
	  ;; 	((lambda (omega) (omega omega))
	  ;; 	 (lambda (beta) (pi (beta beta)))))
	  ;;     (lambda (fact)
	  ;; 	(lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))))
	  ;;    5)
	  ;;  (empty-env))
