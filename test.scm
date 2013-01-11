(define x 10)
(define y (+ x x))

(define flip
  (lambda (f)
    (lambda (y x)
      (f x y))))

(define (test st ch) ; a comment
	;; some more comments
	(if (and (or (= st "248u1924 19; 18u123") (= st ";")) ;; hahaha 
		 (= ch #\;)) 1 0))    ;; hahahahahah

(define (account bal) (lambda (amt) (set! bal (+ bal amt)) bal))
(define a1 (account 100))
(a1 0)
(a1 10)
(a1 10)
