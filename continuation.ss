; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-11-12
; CSSE304-03

(define-datatype continuation continuation?
	[init-k]
	[var-k
		(env environment?)
		(k continuation?)
	]
	[test-k
		(consequent expression?)
		(alternative expression?)
		(env environment?)
		(k continuation?)
	]
	[rator-k
		(rands (list-of? expression?))
		(env environment?)
		(k continuation?)
	]
	[rands-k
		(proc-value scheme-value?)
		(k continuation?)
	]
)

(define (apply-k k v)
	(if (procedure? k) (k v)
		(cases continuation k
			[init-k () v]
			[else 'error]
		)
	)
)
