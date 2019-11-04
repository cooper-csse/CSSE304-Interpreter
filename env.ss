; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

(define empty-env
	(lambda ()
		(empty-env-record)
	)
)

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (map cell vals) env)
	)
)

(define list-find-position
	(lambda (sym los)
		(list-index (lambda (xsym) (eqv? sym xsym)) los)
	)
)

(define list-index
	(lambda (pred ls)
		(cond
			((null? ls) #f)
				((pred (car ls)) 0)
				(else (let ((list-index-r (list-index pred (cdr ls))))
					(if (number? list-index-r)
						(+ 1 list-index-r)
						#f
					)
				)
			)
		)
	)
)

(define apply-env
	(lambda (env sym succeed fail) ; succeed and fail are "callback procedures,
		(cases environment env       ;  succeed is appluied if sym is found, otherwise
			[empty-env-record ()       ;  fail is applied.
				(fail)
			]
			[extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env env sym succeed fail)
					)
				)
			]
		)
	)
)

(define *prim-proc-names*
	'(+ - * / add1 sub1 zero? not cons list null? assq eq?
	equal? atom? length list->vector list? pair?
	procedure? vector->list vector make-vector
	vector-ref vector? number? symbol? set-car!
	set-cdr! vector-set! display newline
	car cdr caar cadr cdar cddr caaar caadr
	cadar caddr cdaar cdadr cddar cdddr
	apply map quotient member append eqv? list-tail
	even? = < > <= >=)
)

(define (init-env)         ; for now, our initial global environment only contains
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc *prim-proc-names*)
		(empty-env)
	)
)

(define global-env (extended-env-record '() '() (init-env)))
(define (reset-global-env)
	(set-car! (cdr global-env) '())
	(set-car! (cddr global-env) '())
)
;(define global-env (init-env))
;(define (reset-global-env) (set! global-env (init-env)))

