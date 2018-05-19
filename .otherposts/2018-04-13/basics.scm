
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))

(define (take n lst)
	(if (zero? n)
		nil
		(cons
			(car lst)
			(take (- n 1) (cdr lst)))))

(define (zip-with f xs ys)
	(if (or (null? xs) (null? ys))
		nil
		(cons
			(f (car xs) (car ys))
			(zip-with f (cdr xs) (cdr ys)))))
