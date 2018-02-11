
(define (test-countdown)
    (let ([correct '((5 4 3 2 1 0))]
          [answers 
            (list 
              (countdown 5) 
            )])
    (display-results correct answers equal?)))

(define (test-range)
  (let ([correct '(() (0 1 2 3 4) (5 6 7 8) (25 26 27 28 29) (31) ())]
        [answers 
         (list (range 0 0) 
               (range 0 5)       
               (range 5 9)
	       (range 25 30)
               (range 31 32)
               (range 7 4))])
    (display-results correct answers equal?)))

(define (test-sum-of-squares)
  (let ([correct '(84 0)]
        [answers 
          (list
            (sum-of-squares '(1 3 5 7)) 
            (sum-of-squares '()))])
    (display-results correct answers equal?)))

(define (test-map)
  (let ([correct '((2 3 4 5)
		   ((1 . 1) (2 . 2) (3 . 3) (4 . 4)))]
        [answers 
	 (list
	     (map add1 '(1 2 3 4)) 
	     (map (lambda (a) (cons a a)) '(1 2 3 4))
          )])
    (display-results correct answers equal?)))

(define (test-minus)
  (let ([correct '(2 50)]
        [answers 
	 (list
	     (minus 5 3) 
	     (minus 100 50)
          )])
    (display-results correct answers equal?)))

(define (test-div)
  (let ([correct '(5 6)]
        [answers 
	 (list
	     (div 25 5) 
	     (div 36 6)
          )])
    (display-results correct answers equal?)))


(define (test-filter)
  (let ([correct '((2 4 6) (cat fish))]
        [answers 
	 (list
	   (filter even? '(1 2 3 4 5 6))
	   (filter symbol? '(1 2 cat 4 5 fish 6))
          )])
    (display-results correct answers equal?)))

(define (test-le->natural)
  (let ([correct '(0 4 12 15 21 8191)]
        [answers 
	 (list
	  (le->natural '())
	  (le->natural '(0 0 1))
	  (le->natural '(0 0 1 1))
	  (le->natural '(1 1 1 1))
	  (le->natural '(1 0 1 0 1))
	  (le->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))
	  )])
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
  (display 'test-countdown)
  (test-countdown)
  (display 'test-range)
  (test-range)
  (display 'test-sum-of-squares)
  (test-sum-of-squares)
  (display 'test-map)
  (test-map)
  (display 'test-minus)
  (test-minus)
  (display 'test-div)
  (test-div)
  (display 'test-filter)
  (test-filter)
  (display 'test-le->natural)
  (test-le->natural)
  )

(define r run-all)



