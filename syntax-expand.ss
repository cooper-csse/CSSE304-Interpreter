; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-11-11
; CSSE304-03

(define (syntax-expand exp)
	(cases expression exp
		[lit-exp (datum) exp]
		[var-exp (id) exp]
		[if-exp (predicate consequent) (if-exp (syntax-expand predicate) (syntax-expand consequent))]
		[if-else-exp (predicate consequent alternative) (if-else-exp (syntax-expand predicate) (syntax-expand consequent) (syntax-expand alternative))]
		[let-exp (inner)
			(let-exp (cases let-type inner
				[normal-let (vars vals bodies) (normal-let vars (map syntax-expand vals) (map syntax-expand bodies))]
				[let*-let (vars vals bodies)
					(if (null? vars) (normal-let '() '() (map syntax-expand bodies))
						(normal-let
							(list (car vars))
							(list (syntax-expand (car vals)))
							(list (syntax-expand (let-exp (let*-let (cdr vars) (cdr vals) bodies))))
						)
					)
				]
				[letrec-let (vars vals bodies)
					(letrec-let vars (map syntax-expand vals) (map syntax-expand bodies))
				]
				[namedlet-let (name vars vals bodies)
					(letrec-let (list name)
						(list (lambda-exp vars '() (map syntax-expand bodies)))
						(list (app-exp (var-exp name) (map syntax-expand vals)))
					)
				]
			))
		]
		[lambda-exp (syms arg bodies) (lambda-exp syms arg (map syntax-expand bodies))]
		[while-exp (predicate bodies) (while-exp (syntax-expand predicate) (map syntax-expand bodies))]
		[for-exp (var start end bodies)
			(syntax-expand (let-exp (namedlet-let 'for-loop (list var 'loop-end) (list start end)
				(list (if-exp (app-exp (var-exp '<=) (list (var-exp var) (var-exp 'loop-end)))
					(app-exp (var-exp 'begin) (append bodies (list
						(app-exp (var-exp 'for-loop) (list
							(app-exp (var-exp 'add1) (list (var-exp var)))
							(var-exp 'loop-end)
						))
					)))
				))
			)))
		]
		[define-exp (var val) (define-exp var (syntax-expand val))]
		[set!-exp (var val) (set!-exp var (syntax-expand val))]
		[app-exp (rator rands)
			(cases expression rator
				[var-exp (id) (parse-expand rator (map syntax-expand rands))]
				[else (app-exp rator (map syntax-expand rands))]
			)
		]
		[else (eopl:error 'syntax-expand "Bad: ~a" exp)]
	)
)

(define (parse-expand rator rands)
	(case (2th rator)
		[(begin) (let-exp (normal-let '() '() rands))]
		[(or) (let loop ([rands rands])
		 	(cond
		 		[(null? rands) (lit-exp #f)]
				[(null? (cdr rands)) (let-exp (normal-let '(super-secret-hidden-variable-name) (list (car rands))
					(list (if-else-exp
						(var-exp 'super-secret-hidden-variable-name)
						(var-exp 'super-secret-hidden-variable-name)
						(lit-exp #f)
					))
				))]
		 		[else (let-exp (normal-let '(super-secret-hidden-variable-name) (list (car rands))
					(list (if-else-exp
						(var-exp 'super-secret-hidden-variable-name)
						(var-exp 'super-secret-hidden-variable-name)
						(loop (cdr rands))
					))
				))]
		 	)
		)]
		[(and) (let loop ([rands rands])
		 	(cond
		 		[(null? rands) (lit-exp #t)]
		 		[(null? (cdr rands)) (if-else-exp (car rands) (lit-exp #t) (lit-exp #f))]
		 		[else (if-else-exp (car rands) (loop (cdr rands)) (lit-exp #f))]
		 	)
		)]
		[(cond) (let loop ([rands rands])
			(if (null? rands) (void) (let ([current (car rands)])
				(cases expression current
					[app-exp (predicate consequent)
						(cases expression predicate
							[var-exp (id) (if (eq? id 'else) (let-exp (normal-let '() '() consequent))
								(eopl:error 'parse-expand "unexpected token in cond: ~a" id)
							)]
							[else (let ([next (loop (cdr rands))])
								(if (expression? next)
									(if-else-exp (syntax-expand predicate) (let-exp (normal-let '() '() consequent)) next)
									(if-exp (syntax-expand predicate) (let-exp (normal-let '() '() consequent)))
								)
							)]
						)
					]
					[else (eopl:error 'parse-expand "unexpected token in cond: ~a" current)]
				)
			))
		)]
		[(case) (let ([check (car rands)])
			(let loop ([rands (cdr rands)])
				(if (null? rands) (void)
					(cases expression (car rands)
						[app-exp (predicate consequent)
							(letrec
								([get-id (lambda (exp)
									(cases expression exp
										[lit-exp (id) id]
										[var-exp (id) id]
										[else eopl:error 'parse-expand "unexpected token in case: ~a" exp]
									)
								)])
								(if (eq? (cadr predicate) 'else) (let-exp (normal-let '() '() consequent))
									(let
										(
											[ls (lit-exp (cons (get-id (cadr predicate)) (map get-id (caddr predicate))))]
											[next (loop (cdr rands))]
										)
										(if-else-exp
											(app-exp (var-exp 'member) (list check ls))
											(let-exp (normal-let '() '() consequent))
											next
										)
									)
								)
							)
						]
						[else eopl:error 'parse-expand "unexpected token in case: ~a" (car rands)]
					)
				)
			)
		)]
		[else (app-exp rator rands)]
	)
)
