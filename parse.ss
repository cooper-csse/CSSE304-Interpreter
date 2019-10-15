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
						(if (symbol? (car x)) (car x)
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
				[(and (not (list? datum)) (pair? datum)) (eopl:error 'parse-exp "improper list in datum:" datum)]
				[(pair? datum)
					(cond
						[(eqv? (1th datum) 'lambda)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <lambda>:" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing arguments in <lambda>: ~s" datum)]
								[(symbol? (2th datum)) (lambda-n-exp (2th datum) (map parse-exp (cddr datum)))]
								[(not (andmap symbol? (2th datum))) (eopl:error 'parse-exp "incorrect variable type in <lambda>: ~s" datum)]
								[else (lambda-exp (2th datum) (map parse-exp (cddr datum)))]
							)
						]
						[(eqv? (car datum) 'let)
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
						[(eqv? (car datum) 'let*)
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
						[(eqv? (car datum) 'letrec)
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
						[(eqv? (car datum) 'set!)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <set!>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing value in <set!>: ~s" datum)]
								[(not (null? (cdddr datum))) (eopl:error 'parse-exp "unexpected token in <set!>: ~s" datum)]
								[else (set!-exp (2th datum) (parse-exp (3th datum)))]
							)

						]
						[(eqv? (car datum) 'if)
							(cond
								[(null? (cdr datum)) (eopl:error 'parse-exp "unexpected token <if>: ~s" datum)]
								[(null? (cddr datum)) (eopl:error 'parse-exp "missing consequent in <if>: ~s" datum)]
								[(null? (cdddr datum)) (if-exp (parse-exp (2th datum)) (parse-exp (3th datum)))]
								[else (if-else-exp (parse-exp (2th datum)) (parse-exp (3th datum)) (parse-exp (4th datum)))]
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
		[let-helper (lambda (type vars vals body)
			(append
				(list type
					(map (lambda (var val)
						(list var (unparse-exp val))
					) vars vals)
				)
				(map unparse-exp body)
			)
		)])
		(lambda (datum)
			(cases expression datum
				[lit-exp (id) id]
				[var-exp (id) id]
				[lambda-exp (id body) (append (list 'lambda id) (map unparse-exp body)) ]
				[lambda-n-exp (id body) (append (list 'lambda id) (map unparse-exp body)) ]
				[if-exp (predicate consequent) (cons 'if (map unparse-exp (list predicate consequent)))]
				[if-else-exp (predicate consequent alternative) (cons 'if (map unparse-exp (list predicate consequent alternative)))]
				[let-exp (inner)
					(cases let-type inner
						[normal-let (vars vals body)
							(let-helper 'let vars vals body)
						]
						[let*-let (vars vals body)
							(let-helper 'let* vars vals body)
						]
						[letrec-let (vars vals body)
							(let-helper 'letrec vars vals body)
						]
						[namedlet-let (name vars vals body)
							(append
								(list 'let name
									(map (lambda (var val)
										(list var (unparse-exp val))
									) vars vals)
								)
								(map unparse-exp body)
							)
						]
					)
				]
				[set!-exp (var val) (list ('set! var (unparse-exp val)))]
				[app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
			)
		)
	)
)
