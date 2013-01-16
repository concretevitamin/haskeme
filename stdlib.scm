;;;;;; Standard library defined in Scheme for Haskeme.

;;;; Ordinary functions.
(define (not x) (if x #f #t)) 
(define (list . objs) objs)

;;;; Enhancements added for Haskeme. 
(define (id x) x)
(define (flip f) (lambda (y x) (f x y)))
(define (curry f arg1) (lambda (arg2) (apply f (list arg1 arg2))))
(define (compose f g) (lambda (x) (f (g x))))

(define combine
  (lambda (f)
    (lambda (x y)
      (if (null? x)
        '()
        (f (list (car x) (car y))
           ((combine f) (cdr x) (cdr y)))))))
(define zip (combine cons))
;; TODO: (zip (list 1 2 3 4) (list 5 6 7 8)) FAILS

(define zero? (curry = 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))
