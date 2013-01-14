;; Standard library defined in Scheme for Haskeme.

;; Ordinary functions.
(define (not x) (if x #f #t)) 
(define (list . objs) objs)

;; Enhancements.
(define (id x) x);; this is just my comment lol
(define (flip f) (lambda (y x) (f x y)))
(define (compose f g) (lambda (x) (f (g x))))

(define combine (lambda (f)
   (lambda (x y)
     (if (null? x) (quote ())
         (f (list (car x) (car y))
            ((combine f) (cdr x) (cdr y)))))))
(define zip (combine cons))
;; TODO: (zip (list 1 2 3 4) (list 5 6 7 8)) FAILS
