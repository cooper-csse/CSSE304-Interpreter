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

; Ease of access for getting first, second, third... in a list
(define 1th car)
(define 2th cadr)
(define 3th caddr)
(define 4th cadddr)
