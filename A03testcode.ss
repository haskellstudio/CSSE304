(define (test-value-of)
  (let ([correct '(#f #t 3 3 #f #t #t 12 30 25 #t 1 #f 5 6 6 5 5 6 5 60 30 25 1)]
	[answers
	 (list
	  (value-of
	   '#f
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '#t
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '3
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(sub1 4)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(zero? 3)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(zero? 0)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(zero? (sub1 1))
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(* 3 4)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(if #t 30 25)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(if #f 30 25)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(if #f #f #t)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(if (zero? 5) 0 1)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(if (zero? 0) #f #t)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '((lambda (x) 5) 6)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '((lambda (x) x) 6)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '((lambda (z) (* z 3)) 2)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '((lambda (y) ((lambda (x) y) 6)) 5)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(((lambda (y) (lambda (x) y)) 5) 6)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(let ([x 5]) 6)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(let ([x 5]) x)
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(let ([y (* 3 4)]) ((lambda (x) (* x y)) (sub1 6)))
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(let ([x (* 2 3)]) (let ([y (sub1 x)]) (* x y)))
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(let ([x (* 2 3)]) (let ([x (sub1 x)]) (* x x)))
	   (lambda (y) (error 'value-of "unbound variable ~a" y)))
	  (value-of
	   '(let ([f (lambda (f)
		       (lambda (n) (if (zero? n) 1 ((f f) (sub1 n)))))])
	      ((f f) 5))
	   (lambda (y) (error 'value-of "unbound variable ~a" y))))])
    (display-results correct answers equal?)))


(define (test-fo-eulav)
  (let ([correct '(  5  4  120)]
	[answers
	 (list
	  (fo-eulav '(5 (x (x) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
	  (fo-eulav '(((x 1bus) (x) adbmal) ((5 f) (f) adbmal)) 
		    (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
	  (fo-eulav   '(5
			(((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
			   (n) adbmal)
			  (f) adbmal)
			 ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
			   (n) adbmal)
			  (f) adbmal))) 
		      (lambda (y) (error 'fo-eulav "unbound variable ~s" y))))])
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
  (display 'test-value-of)
  (test-value-of)
  (display 'test-fo-eulav)
  (test-fo-eulav))

(define r run-all)


