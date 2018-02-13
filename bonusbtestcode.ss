(define (test-eval-exp)
  (let ([correct '(1 5 6 #t 5 5 6 5 1)]
	[answers 
	 (list
	  (eval-exp '(if (zero? 5) 0 1))
	  (eval-exp '((lambda (x) 5) 6))
	  (eval-exp '((lambda (x) x) 6))
	  (eval-exp '((lambda (z) (zero? (sub1 (sub1 z)))) 2))
	  (eval-exp '((lambda (y) ((lambda (x) y) 6)) 5))
	  (eval-exp '(((lambda (y) (lambda (x) y)) 5) 6))
	  (eval-exp '(let ([x 5]) 6))
	  (eval-exp '(let ([x 5]) x))
	  (eval-exp '(let ([f (lambda (f)
				(lambda (n) (if (zero? n) 1 ((f f) (sub1 n)))))])
		       ((f f) 99999))))])
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
  (display 'test-eval-exp)
  (test-eval-exp))

(define r run-all)


