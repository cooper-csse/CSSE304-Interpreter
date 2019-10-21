; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

; top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form)
		; later we may add things that are not expressions.
		(eval-exp form init-env)
	)
)

; eval-exp is the main component of the interpreter

(define (eval-exp exp env)
	(cases expression exp
		[lit-exp (datum) datum]
		[var-exp (id)
			(if (eq? id 'quote) 'quote
				(apply-env env id; look up its value.
					(lambda (x) x) ; procedure to call if id is in the environment
					(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
						"variable not found in environment: ~s"
						id
					))
				)
			)
		]
		[if-exp (predicate consequent)
			(if (eval-exp predicate env)
				(eval-exp consequent env)
				(void)
			)
		]
		[if-else-exp (predicate consequent alternative)
			(if (eval-exp predicate env)
				(eval-exp consequent env)
				(eval-exp alternative env)
			)
		]
		[let-exp (inner)
			(cases let-type inner
				[normal-let (vars vals bodies)
					(eval-bodies bodies (extend-env
						vars
						(map (lambda (item) (eval-exp item env)) vals)
						env
					))
				]
				[else (void)]
			)
		]
		[lambda-exp (syms arg bodies)
			(closure syms arg bodies env)
		]
		[while-exp (predicate bodies)
			(let loop ()
				(if (eval-exp predicate env)
					(begin (eval-bodies bodies env) (loop))
				)
			)
		]
		[app-exp (rator rands)
			(let ([proc-value (eval-exp rator env)])
				(if (eq? proc-value 'quote) (unparse-exp (1th rands))
					(apply-proc proc-value (eval-rands rands env))
				)
			)
		]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
	)
)

; evaluate the list of operands, putting results into a list

(define (eval-rands rands env)
	(map (lambda (item) (eval-exp item env)) rands)
)

(define (eval-bodies bodies env)
	(if (null? (cdr bodies))
		(eval-exp (car bodies) env)
		(begin
			(eval-exp (car bodies) env)
			(eval-bodies (cdr bodies) env)
		)
	)
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define (apply-proc proc-value args)
	(cases proc-val proc-value
		[prim-proc (op) (apply-prim-proc op args)]
		; You will add other cases
		[closure (syms arg bodies env)
			(cond
				[(null? syms)
					(run-closure bodies (extend-env (list arg) (list args) env))
				]
				[(null? arg)
					(if (= (length args) (length syms))
						(run-closure bodies (extend-env syms args env))
						(error 'apply-proc "wrong number of arguments to #<procedure>")
					)
				]
				[else
					(run-closure bodies (extend-env
						(append syms (list arg))
						(set-args (append syms arg) args)
					env))
				]
			)
		]
		[else (error 'apply-proc
			"Attempt to apply bad procedure: ~s"
			proc-value)
		]
	)
)

(define (set-args vars args)
	(if (pair? vars)
		(if (null? args)
			(error 'apply-proc "not enough arguments to #<procedure>")
			(cons (car args) (set-args (cdr vars) (cdr args)))
		)
		(list args)
	)
)


(define (run-closure bodies env)
	(if (null? (cdr bodies))
		(eval-exp (car bodies) env)
		(begin
			(eval-exp (car bodies) env)
			(run-closure (cdr bodies) env)
		)
	)
)

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not cons list null? assq eq?
								equal? atom? length list->vector list? pair?
								procedure? vector->list vector make-vector
								vector-ref vector? number? symbol? set-car!
								set-cdr! vector-set! display newline
								car cdr caar cadr cdar cddr caaar caadr
								cadar caddr cdaar cdadr cddar cdddr
								apply map quotient member and or
								= < > <= >=))

(define init-env         ; for now, our initial global environment only contains
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc *prim-proc-names*)
		(empty-env)
	)
)

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define (apply-prim-proc prim-proc args)
	(case prim-proc
		[(+) (apply + args)]
		[(-) (apply - args)]
		[(*) (apply * args)]
		[(/) (if (ormap (lambda (x) (= 0 x)) (cdr args)) (error '/ "Divide is undefined for 0") (apply / args))]
		[(add1) (if (check-args args 1) (+ (1th args) 1) (error-num-args prim-proc))]
		[(sub1) (if (check-args args 1) (- (1th args) 1) (error-num-args prim-proc))]
		[(zero?) (zero? (1th args))]
		[(not) (not (1th args))]
		[(cons) (cons (1th args) (2th args))]
		[(list) (apply list args)]
		[(null?) (if (check-args args 1) (null? (1th args)) (error-num-args prim-proc))]
		[(assq) (if (check-args args 2) (assq (1th args) (2th args)) (error-num-args prim-proc))]
		[(eq?) (if (check-args args 2) (eq? (1th args) (2th args)) (error-num-args prim-proc))]
		[(equal?) (if (check-args args 2) (equal? (1th args) (2th args)) (error-num-args prim-proc))]
		[(atom?) (if (check-args args 1) (atom? (1th args)) (error-num-args prim-proc))]
		[(length) (if (check-args args 1) (length (1th args)) (error-num-args prim-proc))]
		[(list->vector) (if (check-args args 1) (list->vector (1th args)) (error-num-args prim-proc))]
		[(list?) (if (check-args args 1) (list? (1th args)) (error-num-args prim-proc))]
		[(pair?) (if (check-args args 1) (pair? (1th args)) (error-num-args prim-proc))]
		[(procedure?) (if (check-args args 1) (proc-val? (1th args)) (error-num-args prim-proc))]
		[(vector->list) (if (check-args args 1) (vector->list (1th args)) (error-num-args prim-proc))]
		[(vector) (apply vector args)]
		[(make-vector) (if (check-args args 1 2) (apply make-vector args ) (error-num-args prim-proc))]
		[(vector-ref) (if (check-args args 2) (vector-ref (1th args) (2th args)) (error-num-args prim-proc))]
		[(vector?) (if (check-args args 1) (vector? (1th args)) (error-num-args prim-proc))]
		[(number?) (if (check-args args 1) (number? (1th args)) (error-num-args prim-proc))]
		[(symbol?) (if (check-args args 1) (symbol? (1th args)) (error-num-args prim-proc))]
		[(set-car!) (if (check-args args 2) (set-car! (1th args) (2th args)) (error-num-args prim-proc))]
		[(set-cdr!) (if (check-args args 2) (set-cdr! (1th args) (2th args)) (error-num-args prim-proc))]
		[(vector-set!) (if (check-args args 3) (vector-set! (1th args) (2th args) (3th args)) (error-num-args prim-proc))]
		[(display) (if (check-args args 1) (display (1th args)) (error-num-args prim-proc))]
		[(newline) (if (check-args args 0) (newline) (error-num-args prim-proc))]
		[(car) (if (check-args args 1) (car (1th args)) (error-num-args prim-proc))]
		[(cdr) (if (check-args args 1) (cdr (1th args)) (error-num-args prim-proc))]
		[(caar) (if (check-args args 1) (caar (1th args)) (error-num-args prim-proc))]
		[(cadr) (if (check-args args 1) (cadr (1th args)) (error-num-args prim-proc))]
		[(cdar) (if (check-args args 1) (cdar (1th args)) (error-num-args prim-proc))]
		[(cddr) (if (check-args args 1) (cddr (1th args)) (error-num-args prim-proc))]
		[(caaar) (if (check-args args 1) (caaar (1th args)) (error-num-args prim-proc))]
		[(cadar) (if (check-args args 1) (cadar (1th args)) (error-num-args prim-proc))]
		[(caadr) (if (check-args args 1) (caadr (1th args)) (error-num-args prim-proc))]
		[(caddr) (if (check-args args 1) (caddr (1th args)) (error-num-args prim-proc))]
		[(cdaar) (if (check-args args 1) (cdaar (1th args)) (error-num-args prim-proc))]
		[(cddar) (if (check-args args 1) (cddar (1th args)) (error-num-args prim-proc))]
		[(cdadr) (if (check-args args 1) (cdadr (1th args)) (error-num-args prim-proc))]
		[(cdddr) (if (check-args args 1) (cdddr (1th args)) (error-num-args prim-proc))]
		[(apply) (apply-proc (1th args) (2th args))]
		[(map) (let loop ([proc (1th args)] [args (cdr args)])
			(if (null? (car args)) '()
				(cons (apply-proc proc (map car args)) (loop proc (map cdr args)))
			)
		)]
		[(quotient) (quotient (1th args) (2th args))]
		[(member) (member (1th args) (2th args))]
		[(and) (andmap (lambda (x) x) args)]
		[(or) (ormap (lambda (x) x) args)]
		[(=) (apply = args)]
		[(<) (apply < args)]
		[(>) (apply > args)]
		[(<=) (apply <= args)]
		[(>=) (apply >= args)]
		[else (error 'apply-prim-proc
			"Bad primitive procedure name: ~s"
			prim-proc
		)]
	)
)

(define (check-args args . nums)
	(let ([len (length args)])
		(ormap (lambda (x) (= x len)) nums)
	)
)

(define (error-num-args proc)
	(error 'apply-prim-proc "Incorrect number of arguments to ~s" proc)
)

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
				[else (void)]
			))
		]
		[lambda-exp (syms arg bodies) (lambda-exp syms arg (map syntax-expand bodies))]
		[while-exp (predicate bodies) (while-exp (syntax-expand predicate) (map syntax-expand bodies))]
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
		; [(or) (let loop ([rands rands])
		; 	(cond
		; 		[(null? rands) (lit-exp #f)]
		; 		[(null? (cdr rands)) (if-else-exp (car rands) (car rands) (lit-exp #f))]
		; 		[else (if-else-exp (car rands) (car rands) (loop (cdr rands)))]
		; 	)
		; )]
		; [(and) (let loop ([rands rands])
		; 	(cond
		; 		[(null? rands) (lit-exp #t)]
		; 		[(null? (cdr rands)) (if-else-exp (car rands) (lit-exp #t) (lit-exp #f))]
		; 		[else (if-else-exp (car rands) (loop (cdr rands)) (lit-exp #f))]
		; 	)
		; )]
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
							; (pretty-print (caddr predicate))
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

(define (rep)      ; "read-eval-print" loop.
	(display "--> ")
	;; notice that we don't save changes to the environment...
	(let ([answer (top-level-eval (parse-exp (read)))])
		;; TODO: are there answers that should display differently?
		(eopl:pretty-print answer) (newline)
		(rep) ; tail-recursive, so stack doesn't grow.
	)
)

(define (eval-one-exp exp)
	(top-level-eval (syntax-expand (parse-exp exp)))
)
