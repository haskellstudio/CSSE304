(define v 0)
(define k 0)
(define n 0)

;; Macros

;; (define-syntax <macro-name>
;;   (syntax-rules ()
;;     ((<macro-name> <args>) <rewrite>)
;;     ...))

(define-syntax let/cc
  (syntax-rules ()
    ((let/cc x b) (call/cc (lambda (x) b)))))

(define-syntax throw
  (syntax-rules ()
    ((throw ke ve) (ke ve))))

(define-syntax ifte
  (syntax-rules ()
    ((ifte a #t b #f c) (if a b c))))

;; Bad option
;; (define-syntax orr
;;   (syntax-rules ()
;;     ((orr) #f)    ;; 0 base ಠ_ಠ
;;     ((orr e es ...) ;; recursion
;;      (let ((v e))
;;        (if v v (orr es ...))))))


(trace-define-syntax orr
  (syntax-rules ()
    ((orr) #f)    ;; 0 base ಠ_ಠ
    ((orr e1) e1) ;; 1 base 
    ((orr e1 e2 es ...) ;; recursion
     (let ((v e1))
       (if v v (orr e2 es ...))))))

(define-syntax thunkify
  (syntax-rules ()
    ((_ e) (lambda () e))))

(define-syntax cons$
  (syntax-rules ()
    ((cons$ e1 e2) (cons (thunkify e1) (thunkify e2)))))

(define cdr$
  (lambda ($c)
    ((cdr $c))))

(define car$
  (lambda ($c)
    ((car $c))))

;; _ is a wildcard character
(define-syntax reg-diff
  (syntax-rules ()
    ((_ e) 
     (begin
       (printf "before- v: ~s n: ~s k: ~s\n" v n k)
       (printf "expression ~s\n" 'e)
       (let ((v2 e))
	 (printf "after-  v: ~s n: ~s k: ~s\n" v n k)
	 v2)))))

(define-syntax Msymbol?
  (syntax-rules ()
    ((_ atom)
     '(letrec-syntax 
	  ((test (syntax-rules ()
		   ((_ atom) #t)
		   ((_ . whatever) #f))))
	(test (#f))))))
