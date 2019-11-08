; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

(load "util.ss")

(define parse-exp
	(let (
		[let-helper-car (lambda (set)
			(cond
				[(not (list? set)) (eopl:error 'parse-exp "improper list in let:" set)]
				[(or (pair? set) (null? set))
					(map (lambda (x)
						(if (symbol? (car x)) (make-parameter (car x))
							(eopl:error 'parse-exp "variable must be a symbol:" set)
						)
					) set)
				]
				[else (eopl:error 'parse-exp "item must be list:" set)]
			)
		)]
		[let-helper-cadr (lambda (set)
			(if (or (pair? set) (null? set))
				(map (lambda (x)
					(cond
						[(not (list? x)) (eopl:error 'parse-exp "improper list in variable-value pair:" set)]
						[(null? (cdr x)) (eopl:error 'parse-exp "variable assignment missing value:" set)]
						[(not (null? (cddr x))) (eopl:error 'parse-exp "variable assignment has too many arguments:" set)]
						[else (parse-exp (cadr x))]
					)
				) set)
				(eopl:error 'parse-exp "item must be list:" set)
			)
		)])
		(lambda (datum)
			(cond
				[(symbol? datum) (var-exp datum)]
				[(and (not (list? datum)) (pair? datum)) (lit-exp datum)]
				[(pair? datum)
					(cond
						[(eqv? (1th datum) 'lambda)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <lambda>:" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing arguments in <lambda>: ~s" datum)]
								[(symbol? (2th datum)) (lambda-exp '() (make-parameter (2th datum)) (map parse-exp (cddr datum)))]
								[(and (not (list? (2th datum))) (pair? (2th datum)))
									(let ([args (separate-parameters (2th datum))])
										(lambda-exp (car args) (cadr args) (map parse-exp (cddr datum)))
									)
								]
								[(not (andmap symbol? (2th datum))) (eopl:error 'parse-exp "incorrect variable type in <lambda>: ~s" datum)]
								[else (lambda-exp
									(map make-parameter (2th datum))
								'() (map parse-exp (cddr datum)))]
							)
						]
						[(eqv? (1th datum) 'let)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <let>:" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing arguments in <let>: ~s" datum)]
								[else
									(let-exp (if (symbol? (2th datum))
										(if (null? (cdddr datum))
											(eopl:error 'parse-exp "missing body in <let>:" datum)
											(namedlet-let
												(2th datum)
												(let-helper-car (3th datum))
												(let-helper-cadr (3th datum))
												(map parse-exp (cdddr datum))
											)
										)
										(normal-let
											(let-helper-car (2th datum))
											(let-helper-cadr (2th datum))
											(map parse-exp (cddr datum))
										)
									))
								]
							)

						]
						[(eqv? (1th datum) 'let*)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <let*>:" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing body in <let*>: ~s" datum)]
								[else (let-exp (let*-let
									(let-helper-car (2th datum))
									(let-helper-cadr (2th datum))
									(map parse-exp (cddr datum))
								))]
							)

						]
						[(eqv? (1th datum) 'letrec)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <letrec>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing body in <letrec>: ~s" datum)]
								[else (let-exp (letrec-let
									; (map car-symbol (2th datum))
									(let-helper-car (2th datum))
									(let-helper-cadr (2th datum))
									(map parse-exp (cddr datum))
								))]
							)

						]
						[(eqv? (1th datum) 'if)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <if>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing consequent in <if>: ~s" datum)]
								[(null? (cdddr datum)) (if-exp (parse-exp (2th datum)) (parse-exp (3th datum)))]
								[else (if-else-exp (parse-exp (2th datum)) (parse-exp (3th datum)) (parse-exp (4th datum)))]
							)
						]
						[(eqv? (1th datum) 'define)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <define>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing value in <define>: ~s" datum)]
								[(not (null? (cdddr datum))) (eopl:error 'parse-exp "unexpected token in <define>: ~s" datum)]
								[else (define-exp (2th datum) (parse-exp (3th datum)))]
							)
						]
						[(eqv? (1th datum) 'set!)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <set!>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing value in <set!>: ~s" datum)]
								[(not (null? (cdddr datum))) (eopl:error 'parse-exp "unexpected token in <set!>: ~s" datum)]
								[else (set!-exp (2th datum) (parse-exp (3th datum)))]
							)
						]
						[(eqv? (1th datum) 'while)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <while>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing predicate in <while>: ~s" datum)]
								[else (while-exp (parse-exp (2th datum)) (map parse-exp (cddr datum)))]
							)
						]
						[(eqv? (1th datum) 'for)
							(cond
								; [(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <for>: ~s" datum)]
								; [(or (null? (cddr datum)) (null? (cdddr datum))) (eopl:error 'parse-exp "missing start condition in <for>: ~s" datum)]
								; [(not (eqv? (cddr datum) 'from)) (eopl:error 'parse-exp "missing token <from> in <for>: ~s" datum)]
								; [(or (null? (cddddr datum)) (null? (cdr (cddddr datum)))) (eopl:error 'parse-exp "missing start condition in <for>: ~s" datum)]
								; [(not (eqv? (cddddr datum) 'to)) (eopl:error 'parse-exp "missing token <to> in <for>: ~s" datum)]
								[else (for-exp (2th datum) (parse-exp (4th datum)) (parse-exp (6th datum)) (map parse-exp (cddr (cddddr datum))))]
							)
						]
						[else (app-exp (parse-exp (1th datum))
							(map parse-exp (cdr datum))
						)]
					)
				]
				[(lit? datum) (lit-exp datum)]
				[else (eopl:error 'parse-exp "bad expression: ~s" datum)]
			)
		)
	)
)

(define unparse-exp
	(let (
		[let-helper (lambda (type vars vals bodies)
			(append
				(list type
					(map (lambda (var val)
						(list var (unparse-exp val))
					) vars vals)
				)
				(map unparse-exp bodies)
			)
		)])
		(lambda (datum)
			(cases expression datum
				[lit-exp (id) id]
				[var-exp (id) id]
				[lambda-exp (syms args bodies) (append (list 'lambda (append syms args)) (map unparse-exp bodies)) ]
				[if-exp (predicate consequent) (cons 'if (map unparse-exp (list predicate consequent)))]
				[if-else-exp (predicate consequent alternative) (cons 'if (map unparse-exp (list predicate consequent alternative)))]
				[let-exp (inner)
					(cases let-type inner
						[normal-let (vars vals bodies)
							(let-helper 'let vars vals bodies)
						]
						[let*-let (vars vals bodies)
							(let-helper 'let* vars vals bodies)
						]
						[letrec-let (vars vals bodies)
							(let-helper 'letrec vars vals bodies)
						]
						[namedlet-let (name vars vals bodies)
							(append
								(list 'let name
									(map (lambda (var val)
										(list var (unparse-exp val))
									) vars vals)
								)
								(map unparse-exp bodies)
							)
						]
					)
				]
				[set!-exp (var val) (list ('set! var (unparse-exp val)))]
				[while-exp (predicate bodies) (append (list 'while (unparse-exp predicate)) (map unparse-exp bodies))]
				[app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
				[else (display "test")]
			)
		)
	)
)

(define (separate-parameters args)
	(if (pair? (cdr args))
		(let ([next (separate-parameters (cdr args))])
			(cons (cons
				(if (is-ref? (car args))
					(ref-arg (cadr args))
					(val-arg (car args))
				)
				(car next)
			) (cdr next))
		)
		(list (list (val-arg (car args))) (val-arg (cdr args)))
	)
)
