(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)

(define (id x) x)
(define (flip f) (lambda (y x) (f x y)))
(define (compose f g) (lambda (x) (f (g x))))
