(load 'basics.scm)

(define (process-cases expr)
	(define cases (map split expr))
	(define matches (map car cases))
	(define bodies (map cadr cases))
	(define siglen (signature-length matches))
	(define function (to-same (map car matches) 'inconsistent-function-name))
	(define params (take siglen PARAMS))
	(define param-matches (map cdr matches))
	(define cond-forms (map (lambda (m) (cond-form params m)) param-matches))
	(define cond-bodies (zip-with append cond-forms bodies))
	`(define (,function	,@params) (cond ,@cond-bodies)))

(define PARAMS '(p1 p2 p3 p4 p5 p6 p7 p8 p9))

(define (signature sym match)
	(cond
		((null? match)
			`((null? ,sym) ()))
		((symbol? match)
			`(#t ((define ,match ,sym))))
		((eq? (car match) 'cons)
			`((pair? ,sym)
				(
					(define ,(cadr match) (car ,sym))
					(define ,(caddr match) (cdr ,sym)))))
		(else (error 'bad-match))))

(define (cond-form syms matches)
	(define signatures (zip-with (lambda (sym match) (signature sym match)) syms matches))
	(define filts (map car signatures))
	(define defs (map cadr signatures))
	(define all-defs (reduce append (cons nil defs)))
	(append (list (cons 'and filts)) all-defs))

(define (signature-length matches)
	(define lens (map length matches))
	(- (to-same lens 'inconsistent-number-of-parameters-in-cases) 1))

(define (to-same values msg)
	(cond
		((null? values)
			(error 'no-cases))
		((null? (cdr values))
			(car values))
		(else
			(define rest (to-same (cdr values) msg))
			(if (eq? rest (car values))
				rest
				(error msg)))))

(define (split expr)
	(cond
		((null? expr)
			(error 'no-equal-found))
		((eq? (car expr) '=)
			(list nil (cdr expr)))
		(else
			(begin
				(define rest (split (cdr expr)))
				(define before (car rest))
				(define after (cadr rest))
				`((,(car expr) ,@before) ,after)))))

(define-macro (define-cases cases)
	(process-cases cases))

(define-cases (
	(map f nil = nil)
	(map f (cons x xs) = (cons (f x) (map f xs)))))

(define-cases (
	(filter f nil = nil)
	(filter f (cons x xs) | (f x) = (cons x (filter f xs))
						  | #t	  = (filter f xs))))


(define-cases (
	(filter f nil = nil)
	(filter f (cons x xs) =
		(if (f x)
			(cons x (filter f xs))
			(filter f xs)))))
