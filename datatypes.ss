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
		(bodies (list-of expression?))
	]
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
	[define-exp
		(var symbol?)
		(val expression?)
	]
	[set!-exp
		(var symbol?)
		(val expression?)
	]
	[while-exp
		(predicate expression?)
		(bodies (list-of expression?))
	]
	[for-exp
		(var symbol?)
		(start expression?)
		(end expression?)
		(bodies (list-of expression?))
	]
	[app-exp
		(rator expression?)
		(rand (list-of expression?))
	]
	[void-exp]
)

(define-datatype let-type let-type?
	[normal-let
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))
	]
	[let*-let
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))
	]
	[letrec-let
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))
	]
	[namedlet-let
		(name symbol?)
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))
	]
)

;; environment type definitions

(define scheme-value?
	(lambda (x) #t)
)

(define (cell value) (cons 'cell value))
(define cell? pair?)
(define cell-ref cdr)
(define cell-set! set-cdr!)

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms (list-of (lambda (x) (or (symbol? x) (null? x)))))
		(vals (list-of cell?))
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
		(arg (lambda (x) (or (symbol? x) (null? x))))
		(bodies (list-of expression?))
		(env environment?)
	]
)


