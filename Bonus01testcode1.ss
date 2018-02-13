(define (test-and*)
  (let ([correct '(3 #f #t a #f)]
        [answers (list (and* 1 2 3) (and* #f) (and*) (and* 'a)
                   (and* #t #t #t #t #t #t #t #t #f))])
    (display-results correct answers equal?)))

(define (test-list*)
  (let ([correct '((a b c . d) a)]
	[answers (list (list* 'a 'b 'c 'd)
		       (list* 'a))])
    (display-results correct answers equal?)))

(define (test-macro-list)
  (let ([correct '(() (1 b 2 d))]
	[answers 
	 (list (macro-list)
	       (macro-list 1 'b 2 'd))])
    (display-results correct answers equal?)))

(define (test-mcond)
  (let ([correct '(dog cat #t)]
        [answers (list
                   (mcond (#f #t) (else 'dog))
                   (mcond (else 'cat))
                   (mcond (#t #t) (unbound variables)))])
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
  (display 'test-and*)
  (test-and*)  
  (display 'test-list*)
  (test-list*)  
  (display 'test-macro-list)
  (test-macro-list)  
  (display 'test-mcond)
  (test-mcond)  
  (display 'test-macro-map)
  (test-macro-map))

(define r run-all)


