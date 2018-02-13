;; (load "pmatch.ss")
(load "parenthec.ss") 

(define-union ct 
 (fact-c n^ c^)
 (empty-c))

(define-union kt
  (empty-k))

(define-label apply-c
  (union-case c ct ;; c is one of the shapes in ct
	      [(fact-c n^ c^) 
	       (begin
		 (set! c c^)
		 (set! v (* n^ v)) 
		 (apply-c))]
	      [(empty-c)
	       (begin
		 (apply-k))]
					; (else (c v k))
	      ))

(define-label apply-k
  (union-case k kt 
	      [(empty-k) v]))

(define-label fact-cps-cps
  (if (zero? n)
      (begin
	(set! v 1) 
	(apply-c))
      (begin
	(set! c (ct_fact-c n c))
	(set! n (sub1 n))
	(fact-cps-cps))))

(define-label main
  (begin
    (set! n 5)
    (set! c (ct_empty-c))
    (set! k (kt_empty-k))
    (printf "~d\n" (fact-cps-cps))))

(main)
