;;Bonus 1A
;;Yifei Li
(define v 0)
;;1
(define-syntax and*
    (syntax-rules ()
        ((and*) #t)
        ((and* e1) e1)
        ((and* e1 e2 es ...)
            (let ((v e1))
                (if (not v) #f (and* e2 es ...))
                ))))

(define-syntax list*
    (syntax-rules ()
        ((list*) (syntax-error "Incorrect argument-count to list*"))
        ((list* e1) e1)
        ((list* e1 e2 es ...)
            (let ((v e1))
                (cons v (list* e2 es ...) )
                ))))

(define-syntax macro-list
    (syntax-rules () 
        ((macro-list) '())
        ((macro-list e1) (cons e1 '()))
        ((macro-list e1 e2 es ...)
            (let ((v e1))
                (cons v (macro-list e2 es ...))))
        ))

(define-syntax mcond
    (syntax-rules ()
        ((mcond) (syntax-error "Incorrect argument-count to mcond"))
        ((mcond (else a)) a)
        ((mcond (e1 a1) (e2 a2) es ...)
            (let* ((v e1) (w a1))
                (if v w (mcond (e2 a2) es ...))))
        ))

(define-syntax macro-map
    (syntax-rules ()
        ((macro-map) (syntax-error "Incorrect argument-count to macro-map"))
        ((macro-map f) (syntax-error "Incorrect argument-count to macro-map"))
        ((macro-map f '(e1) ) (cons (f e1) '() ))   
        ((macro-map f '(e1 e2 es ...))
            (cons (f e1) (macro-map f '(e2 es ...))))
        ))
