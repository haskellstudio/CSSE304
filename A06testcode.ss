
(define (test-binary-to-decimal-cps) 
  (let ([correct '(0 4 12 15 21 8191)]
	[answers
	 (list
	  (binary-to-decimal-cps '(0) (empty-k))
	  (binary-to-decimal-cps '(0 0 1) (empty-k))
	  (binary-to-decimal-cps '(0 0 1 1) (empty-k))
	  (binary-to-decimal-cps '(1 1 1 1) (empty-k))
	  (binary-to-decimal-cps '(1 0 1 0 1) (empty-k))
	  (binary-to-decimal-cps '(1 1 1 1 1 1 1 1 1 1 1 1 1) (empty-k))
	  )])
    (display-results correct answers equal?)))    

(define (test-times-cps) 
  (let ([correct '(120 0)]
	[answers
	 (list
	  (times-cps '(1 2 3 4 5) (empty-k))
	  (times-cps '(1 2 3 4 5 0) (empty-k)))])
    (display-results correct answers equal?)))


(define (test-times-cps-shortcut) 
  (let ([correct '(120 0 0 0)]
	[answers
	 (list
	  (times-cps-shortcut '(1 2 3 4 5) (empty-k))
	  (times-cps-shortcut '(1 2 3 4 5 0) (empty-k))
	  (times-cps-shortcut (letrec ((i (lambda (n) (if (= n 500) `(,n) `(,n . ,(i (+ 1 n))))))) (i 0)) (empty-k))
	  (times-cps-shortcut '(1 2 3 0 a) (empty-k))
	  )])
    (display-results correct answers equal?)))


(define (test-plus-cps) 
  (let ([correct '(5 11 20)]
	[answers
	 (list
	  (plus-cps 2 (lambda (p) (p 3 (empty-k))))
	  (plus-cps 5 (lambda (p) (p 6 (empty-k))))
	  (plus-cps 0 (lambda (p) (p 20 (empty-k)))))])
    (display-results correct answers equal?)))


(define (test-remv-first-9*-cps) 
  (let ([correct '((x y 9 z) ((())) ((a b) (c d) 9 (e 9)) (((a b)) 9 (c 9)))]
	[answers
	 (list
	  (remv-first-9*-cps '(x 9 y 9 z) (empty-k))
	  (remv-first-9*-cps '(((9)))  (empty-k))
	  (remv-first-9*-cps '((a b) (c 9 d) 9 (e 9)) (empty-k))
	  (remv-first-9*-cps '(((a b) 9) 9 (c 9)) (empty-k))
	  )])
    (display-results correct answers equal?)))    


(define (test-cons-cell-count-cps) 
  (let ([correct '(0 4 11 13 13 7 7 6 7)]
	[answers
	 (list
	  (cons-cell-count-cps '() (empty-k))
	  (cons-cell-count-cps '(0 1 2 0) (empty-k))
	  (cons-cell-count-cps '(2 2 2 (2 2) 2 (2 (2))) (empty-k))
	  (cons-cell-count-cps '(9 (15 (23 (43 (4 (98 (12))))))) (empty-k))
	  (cons-cell-count-cps '(((((((9) 15) 23) 43) 4) 98) 12) (empty-k))
	  (cons-cell-count-cps '(0 0 0 0 0 0 0) (empty-k))
	  (cons-cell-count-cps '(0 (0 (0 0) 0)) (empty-k))
	  (cons-cell-count-cps '((((((1)))))) (empty-k))
	  (cons-cell-count-cps '((((3 3 3 3)))) (empty-k))
	  )])
    (display-results correct answers equal?)))

(define (test-find-cps) 
  (let ([correct '(5 5 ((c . a)) 5 f e)]
	[answers
	 (list
	  (find-cps 'a '((a . 5) (b . 6) (c . a)) (empty-k))
	  (find-cps 'c '((a . 5) (b . (a . c)) (c . a)) (empty-k))
	  (find-cps 'b '((a . 5) (b . ((c . a))) (c . a)) (empty-k))
	  (find-cps 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)) (empty-k))
	  (find-cps 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)) (empty-k))
	  (find-cps 'a '((a . b) (c . d) (d . e) (b . c)) (empty-k))
	  )])
    (display-results correct answers equal?)))

(define (test-ack-cps) 
  (let ([correct '(2 10 3 5 5 7 61)]
	[answers
	 (list
	  (ack-cps 0 1 (empty-k))
	  (ack-cps 0 9 (empty-k))
	  (ack-cps 1 1 (empty-k))
	  (ack-cps 1 3 (empty-k))
	  (ack-cps 2 1 (empty-k))
	  (ack-cps 2 2 (empty-k))
	  (ack-cps 3 3 (empty-k))
	  )])
    (display-results correct answers equal?)))

(define (test-M-cps) 
  (let ([correct '((2 3 4 5 6) (#t #f #f #f #t #f) ((1) (2) ((5 6))) ((3 . 4) (5 . 6) (6 . 7) (8 . 9) (2 . 3)))]
	[answers
	 (list
	  (M-cps (lambda (n k) (k (add1 n))) (lambda (p) (p '(1 2 3 4 5) (empty-k))))
	  (M-cps (lambda (n k) (k (zero? n))) (lambda (p) (p '(0 2 1 3 0 5) (empty-k))))
	  (M-cps (lambda (e k) (k (list e))) (lambda (p) (p '(1 2 (5 6)) (empty-k))))
	  (M-cps (lambda (e k) (k (cons e (add1 e)))) (lambda (p) (p '(3 5 6 8 2) (empty-k))))
	  )])
    (display-results correct answers equal?)))

(define (test-use-of-M-cps) 
  (let ([correct '((2 3 4 5 6))]
	[answers
	 (list
	  (use-of-M-cps (empty-k)))])
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
  (display 'test-binary-to-decimal-cps)
  (test-binary-to-decimal-cps)
  (display 'test-times-cps)
  (test-times-cps)
  (display 'test-times-cps-shortcut)
  (test-times-cps-shortcut)
  (display 'test-plus-cps)
  (test-plus-cps)
  (display 'test-remv-first-9*-cps)
  (test-remv-first-9*-cps)
  (display 'test-cons-cell-count-cps)
  (test-cons-cell-count-cps)
  (display 'test-find-cps)
  (test-find-cps)
  (display 'test-ack-cps)
  (test-ack-cps)
  (display 'test-M-cps)
  (test-M-cps)
  (display 'test-use-of-M-cps)
  (test-use-of-M-cps))

(define r run-all)
