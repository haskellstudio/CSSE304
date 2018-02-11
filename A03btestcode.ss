(define (test-value-of-fn)
  (let ([correct '(#f #t 3 3 #f #t #t 12 30 25 #t 1 #f 5 6 6 5 5 6 5 60 30 25 1)]
	[answers
	 (list
	  (value-of-fn
	   '#f
	   (empty-env-fn))
	  (value-of-fn
	   '#t
	   (empty-env-fn))
	  (value-of-fn
	   '3
	   (empty-env-fn))
	  (value-of-fn
	   '(sub1 4)
	   (empty-env-fn))
	  (value-of-fn
	   '(zero? 3)
	   (empty-env-fn))
	  (value-of-fn
	   '(zero? 0)
	   (empty-env-fn))
	  (value-of-fn
	   '(zero? (sub1 1))
	   (empty-env-fn))
	  (value-of-fn
	   '(* 3 4)
	   (empty-env-fn))
	  (value-of-fn
	   '(if #t 30 25)
	   (empty-env-fn))
	  (value-of-fn
	   '(if #f 30 25)
	   (empty-env-fn))
	  (value-of-fn
	   '(if #f #f #t)
	   (empty-env-fn))
	  (value-of-fn
	   '(if (zero? 5) 0 1)
	   (empty-env-fn))
	  (value-of-fn
	   '(if (zero? 0) #f #t)
	   (empty-env-fn))
	  (value-of-fn
	   '((lambda (x) 5) 6)
	   (empty-env-fn))
	  (value-of-fn
	   '((lambda (x) x) 6)
	   (empty-env-fn))
	  (value-of-fn
	   '((lambda (z) (* z 3)) 2)
	   (empty-env-fn))
	  (value-of-fn
	   '((lambda (y) ((lambda (x) y) 6)) 5)
	   (empty-env-fn))
	  (value-of-fn
	   '(((lambda (y) (lambda (x) y)) 5) 6)
	   (empty-env-fn))
	  (value-of-fn
	   '(let ([x 5]) 6)
	   (empty-env-fn))
	  (value-of-fn
	   '(let ([x 5]) x)
	   (empty-env-fn))
	  (value-of-fn
	   '(let ([y (* 3 4)]) ((lambda (x) (* x y)) (sub1 6)))
	   (empty-env-fn))
	  (value-of-fn
	   '(let ([x (* 2 3)]) (let ([y (sub1 x)]) (* x y)))
	   (empty-env-fn))
	  (value-of-fn
	   '(let ([x (* 2 3)]) (let ([x (sub1 x)]) (* x x)))
	   (empty-env-fn))
	  (value-of-fn
	   '(let ([f (lambda (f)
		       (lambda (n) (if (zero? n) 1 ((f f) (sub1 n)))))])
	      ((f f) 5))
	   (empty-env-fn)))])
    (display-results correct answers equal?)))

(define (test-value-of-ds)
  (let ([correct '(#f #t 3 3 #f #t #t 12 30 25 #t 1 #f 5 6 6 5 5 6 5 60 30 25 1)]
	[answers
	 (list
	  (value-of-ds
	   '#f
	   (empty-env-ds))
	  (value-of-ds
	   '#t
	   (empty-env-ds))
	  (value-of-ds
	   '3
	   (empty-env-ds))
	  (value-of-ds
	   '(sub1 4)
	   (empty-env-ds))
	  (value-of-ds
	   '(zero? 3)
	   (empty-env-ds))
	  (value-of-ds
	   '(zero? 0)
	   (empty-env-ds))
	  (value-of-ds
	   '(zero? (sub1 1))
	   (empty-env-ds))
	  (value-of-ds
	   '(* 3 4)
	   (empty-env-ds))
	  (value-of-ds
	   '(if #t 30 25)
	   (empty-env-ds))
	  (value-of-ds
	   '(if #f 30 25)
	   (empty-env-ds))
	  (value-of-ds
	   '(if #f #f #t)
	   (empty-env-ds))
	  (value-of-ds
	   '(if (zero? 5) 0 1)
	   (empty-env-ds))
	  (value-of-ds
	   '(if (zero? 0) #f #t)
	   (empty-env-ds))
	  (value-of-ds
	   '((lambda (x) 5) 6)
	   (empty-env-ds))
	  (value-of-ds
	   '((lambda (x) x) 6)
	   (empty-env-ds))
	  (value-of-ds
	   '((lambda (z) (* z 3)) 2)
	   (empty-env-ds))
	  (value-of-ds
	   '((lambda (y) ((lambda (x) y) 6)) 5)
	   (empty-env-ds))
	  (value-of-ds
	   '(((lambda (y) (lambda (x) y)) 5) 6)
	   (empty-env-ds))
	  (value-of-ds
	   '(let ([x 5]) 6)
	   (empty-env-ds))
	  (value-of-ds
	   '(let ([x 5]) x)
	   (empty-env-ds))
	  (value-of-ds
	   '(let ([y (* 3 4)]) ((lambda (x) (* x y)) (sub1 6)))
	   (empty-env-ds))
	  (value-of-ds
	   '(let ([x (* 2 3)]) (let ([y (sub1 x)]) (* x y)))
	   (empty-env-ds))
	  (value-of-ds
	   '(let ([x (* 2 3)]) (let ([x (sub1 x)]) (* x x)))
	   (empty-env-ds))
	  (value-of-ds
	   '(let ([f (lambda (f)
		       (lambda (n) (if (zero? n) 1 ((f f) (sub1 n)))))])
	      ((f f) 5))
	   (empty-env-ds)))])
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
  (display 'test-value-of-fn)
  (test-value-of-fn)
  (display 'test-value-of-ds)
  (test-value-of-ds))

(define r run-all)


