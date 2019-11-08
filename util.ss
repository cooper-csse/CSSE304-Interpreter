; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

; Check if a specific expression is a literal
(define (lit? exp)
	(ormap (lambda (pred) (pred exp))
		(list number? vector? boolean? symbol? string? pair? null?)
	)
)

(define (is-ref? datum)
	(cond
		[(symbol? datum) #f]
		[(null? datum) (eopl:error 'parse-exp "unexpected token: ~s" datum)]
		[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected parentheses around argument: ~s" datum)]
		[(not (eq? (car datum) 'ref)) (eopl:error 'parse-exp "missing token <ref>: ~s" datum)]
		[else #t]
	)
)

(define (make-parameter datum)
	(if (is-ref? datum)
		(ref-arg (2th datum))
		(val-arg datum)
	)
)

; Ease of access for getting first, second, third... in a list
(define 1th car)
(define 2th cadr)
(define 3th caddr)
(define 4th cadddr)
(define (5th ls) (car (cddddr ls)))
(define (6th ls) (cadr (cddddr ls)))
