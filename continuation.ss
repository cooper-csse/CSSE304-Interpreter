; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-11-12
; CSSE304-03

(define-datatype continuation continuation?
	[init-k]
	[success-k
		(success scheme-value?)
		(k continuation?)
	]
	[if-else-k
		(consequent expression?)
		(alternative (or-comp expression? null?))
		(env environment?)
		(k continuation?)
	]
	[rator-k
		(rands (list-of expression?))
		(env environment?)
		(k continuation?)
	]
	[rands-k
		(proc-value scheme-value?)
		(k continuation?)
	]
	[bodies-k
		(bodies (list-of expression?))
		(env environment?)
		(k continuation?)
	]
	[closure-k
		(bodies (list-of expression?))
		(env environment?)
		(k continuation?)
	]
	[let-k
		(vars (list-of symbol?))
		(env environment?)
		(bodies (list-of expression?))
		(k continuation?)
	]
	[letrec-k
		(vars (list-of symbol?))
		(env environment?)
		(bodies (list-of expression?))
		(k continuation?)
	]
	[define-k
		(var symbol?)
		(k continuation?)
	]
	[set!-k
		(env environment?)
		(var symbol?)
		(k continuation?)
	]
	[while-k
		(predicate expression?)
		(bodies (list-of expression?))
		(env environment?)
		(k continuation?)
	]
	[while-helper-k
		(predicate expression?)
		(bodies (list-of expression?))
		(env environment?)
		(k continuation?)
	]
	[map-k
		(proc procedure?)
		(ls (or-comp pair? null?))
		(k continuation?)
	]
	[cons-k
		(ls (or-comp pair? null?))
		(k continuation?)
	]
)

(define (apply-k k v)
	(if (procedure? k) (k v)
		(cases continuation k
			[init-k () v]
			[success-k (success k) (apply-k k (success v))]
			[if-else-k (consequent alternative env k)
				(if v
					(eval-exp consequent env k)
					(eval-exp alternative env k)
				)
			]
			[rator-k (rands env k)
				(eval-rands rands env (rands-k v k))
			]
			[rands-k (proc-value k) (apply-proc proc-value v k)]
			[bodies-k (bodies env k)
				(if (null? (cdr bodies))
					(eval-exp (car bodies) env k)
					(eval-exp (car bodies) env (bodies-k (cdr bodies) env k))
				)
			]
			[closure-k (bodies env k)
				(if (null? (cdr bodies))
					(eval-exp (car bodies) env k)
					(eval-exp (car bodies) env (closure-k (cdr bodies) env k))
				)
			]
			[let-k (vars env bodies k)
				(eval-bodies bodies (extend-env vars v env) k)
			]
			[letrec-k (vars env bodies k)
				(let ([new-env (extend-env vars v env)])
					(let loop ([old-vals v])
						(if (null? old-vals) '()
							(cons (cases proc-val (car old-vals)
								[closure (syms arg bodies env)
									(set-car! (cddddr (car old-vals)) new-env)
								]
								[else (void)]
							) (loop (cdr old-vals)))
						)
					)
					(eval-bodies bodies (extend-env vars v new-env) k)
				)
			]
			[define-k (var k)
				(set-car! (cdr global-env) (cons var (cadr global-env)))
				(set-car! (cddr global-env) (cons (cell v) (caddr global-env)))
				(apply-k k (void))
			]
			[set!-k (env var k)
				(apply-env env var
					(success-k (lambda (c) (cell-set! c v)) k)
					(lambda () (eopl:error 'apply-env
						"variable not found in environment: ~s"
						var
					))
				)
			]
			[while-k (predicate bodies env k)
				(if v
					(eval-bodies bodies env
						(while-helper-k predicate bodies env k)
					)
					(apply-k k (void))
				)
			]
			[while-helper-k (predicate bodies env k)
				(eval-exp predicate env (while-k predicate bodies env k))
			]
			[map-k (proc ls k) (proc (car ls) (cons-k v k))]
			[cons-k (ls k) (apply-k k (cons v ls))]
			[else 'error]
		)
	)
)

(define (map-cps proc-cps ls k)
	(if (null? ls) (apply-k k '())
		(map-cps proc-cps (cdr ls)
			(map-k proc-cps ls k)
		)
	)
)

(define (proc-to-cps proc)
	(lambda (proc v k) (apply-k (proc v)))
)
