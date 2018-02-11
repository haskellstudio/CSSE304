
(define (test-memv)
  (let ([correct '((a b c) #f (b c b))]
	[answers (list
		  (memv 'a '(a b c))
		  (memv 'b '(a ? c))
		  (memv 'b '(a b c b)))])
    (display-results correct answers equal?)))

(define (test-append)
  (let ([correct '((a b c 1 2 3))]
	[answers (list  (append '(a b c) '(1 2 3)))]
	)
    (display-results correct answers equal?)))

(define (test-reverse)
 (let ([correct '(   (x 3 a))]
       [answers (list  (reverse '(a 3 x)))])
   (display-results correct answers equal?)))

(define (test-union)
  (let ([correct '(   ()   (x)   (x)   (x y z))]
	[answers (list
		  (union '() '())
		  (union '(x) '())
		  (union '(x) '(x))
		  (union '(x y) '(x z))
		  )])
   (display-results correct answers set-equals?) 
    ))

(define (test-extend)
  (let ([correct '(   #t   #t   #t   #f   (0 1 2 4)   (0 1 2 3 4)   (0 1 2 3 4))]
	[answers
	 (list
	  ((extend 1 even?) 0)
	  ((extend 1 even?) 1)
	  ((extend 1 even?) 2)
	  ((extend 1 even?) 3)
	  (filter (extend 1 even?) '(0 1 2 3 4 5))
	  (filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))
	  (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))
	  )])
    (display-results correct answers equal?) 
    ))

(define (test-walk-symbol)
  (let ([correct '(   5   c   5   5   ((c . a))   5   f)]
	[answers
	 (list 
	  (walk-symbol 'a '((a . 5)))
	  (walk-symbol 'a '((b . c) (a . b)))
	  (walk-symbol 'a '((a . 5) (b . 6) (c . a)))
	  (walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))
	  (walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
	  (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
	  (walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))
	  )])
    (display-results correct answers equal?) ))

(define (test-list-ref)
  (let ([correct '(   c   a)]
	[answers
	 (list
	  (list-ref '(a b c) 2)
	  (list-ref '(a b c) 0))])
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
  (display 'test-memv)
  (test-memv)
  (display 'test-append)
  (test-append)
  (display 'test-reverse)
  (test-reverse)
  (display 'test-union)
  (test-union)
  (display 'test-extend)
  (test-extend)
  (display 'test-walk-symbol)
  (test-walk-symbol)
  (display 'test-list-ref)
  (test-list-ref)
  )

(define r run-all)


