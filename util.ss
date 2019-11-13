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

(define (or-comp . preds)
	(lambda (item)
		(let loop ([preds preds])
			(cond
				[(null? preds) #f]
				[((car preds) item) #t]
				[else (loop (cdr preds))]
			)
		)
	)
)

; Ease of access for getting first, second, third... in a list
(define 1th car)
(define 2th cadr)
(define 3th caddr)
(define 4th cadddr)
(define (5th ls) (car (cddddr ls)))
(define (6th ls) (cadr (cddddr ls)))

(define (DEBUG msg)
	(if #t (pretty-print msg))
)
