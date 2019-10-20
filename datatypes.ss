; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

(load "util.ss")

(define-datatype expression expression?
	[lit-exp
		(id lit?)]
	[var-exp
		(id symbol?)
	]
	[lambda-exp
		(syms (lambda (x) (or ((list-of symbol?) x) (null? x))))
		(arg (lambda (x) (or (symbol? x) (null? x))))
		(body (list-of expression?))
	]
	; [lambda-n-exp
	; 	(vars (list-of symbol?))
	; 	(id symbol?)
	; 	(body (list-of expression?))
	; ]
	[if-exp
		(predicate expression?)
		(consequent expression?)
	]
	[if-else-exp
		(predicate expression?)
		(consequent expression?)
		(alternative expression?)
	]
	[let-exp
		(inner let-type?)
	]
	[set!-exp
		(var symbol?)
		(val expression?)
	]
	[app-exp
		(rator expression?)
		(rand (list-of expression?))
	]
)

(define-datatype let-type let-type?
	[normal-let
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))
	]
	[let*-let
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))
	]
	[letrec-let
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))
	]
	[namedlet-let
		(name symbol?)
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))
	]
)

;; environment type definitions

(define scheme-value?
	(lambda (x) #t)
)

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms (list-of (lambda (x) (or (symbol? x) (null? x)))))
		; (syms (list-of symbol?))
		(vals (list-of scheme-value?))
		(env environment?)
	)
)


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
	[prim-proc
		(name symbol?)
	]
	[closure
		(syms (lambda (x) (or ((list-of symbol?) x) (null? x))))
		;(syms (list-of symbol?))
		(arg (lambda (x) (or (symbol? x) (null? x))))
		(bodies (list-of expression?))
		(env environment?)
	]
)


