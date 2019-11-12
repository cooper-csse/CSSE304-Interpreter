; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

; top-level-eval evaluates a form in the global environment
(define top-level-eval
	(lambda (form)
		; later we may add things that are not expressions.
		(eval-exp form global-env (init-k))
	)
)

; eval-exp is the main component of the interpreter
(define (eval-exp exp env k)
	(cases expression exp
		[lit-exp (datum) (apply-k k datum)]
		[var-exp (id)
			(if (eq? id 'quote) 'quote
				(apply-env env id
					cell-ref
					(lambda () (eopl:error 'apply-env
						"variable not found in environment: ~s"
						id
					))
				)
			)
		]
		[if-exp (predicate consequent)
			(if (eval-exp predicate env k)
				(eval-exp consequent env k)
				(void)
			)
		]
		[if-else-exp (predicate consequent alternative)
			(if (eval-exp predicate env k)
				(eval-exp consequent env k)
				(eval-exp alternative env k)
			)
		]
		[let-exp (inner)
			(cases let-type inner
				[normal-let (vars vals bodies)
					(eval-bodies bodies (extend-env
						vars
						(map (lambda (item) (eval-exp item env k)) vals)
						env
					) k)
				]
				[letrec-let (vars vals bodies)
					(let*
						(
							[old-vals (map (lambda (item) (eval-exp item env k)) vals)]
							[new-env (extend-env vars old-vals env)]
						)
						(let loop ([old-vals old-vals])
							(if (null? old-vals) '()
								(cons (cases proc-val (car old-vals)
									[closure (syms arg bodies env)
										(set-car! (cddddr (car old-vals)) new-env)
									]
									[else (void)]
								) (loop (cdr old-vals)))
							)
						)
						(eval-bodies bodies new-env k)
					)
				]
				[else (void)]
			)
		]
		[lambda-exp (syms arg bodies)
			(apply-k k (closure syms arg bodies env))
		]
		[while-exp (predicate bodies)
			(let loop ()
				(if (eval-exp predicate env k)
					(begin (eval-bodies bodies env k) (loop))
				)
			)
		]
		[define-exp (var val)
			(let ([eval-val (eval-exp val env k)])
				(set-car! (cdr global-env) (cons var (cadr global-env)))
				(set-car! (cddr global-env) (cons (cell eval-val) (caddr global-env)))
			)
		]
		[set!-exp (var val)
			(apply-env env var
				(lambda (c) (cell-set! c (eval-exp val env k)))
				(lambda () (eopl:error 'apply-env
					"variable not found in environment: ~s"
					var
				))
			)
		]
		[app-exp (rator rands)
			(let ([proc-value (eval-exp rator env k)])
				(if (eq? proc-value 'quote) (unparse-exp (1th rands))
					(apply-proc proc-value (eval-rands rands env k) k)
				)
			)
		]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
	)
)

; evaluate the list of operands, putting results into a list
(define (eval-rands rands env k)
	(map (lambda (item) (eval-exp item env k)) rands)
)

(define (eval-bodies bodies env k)
	(if (null? (cdr bodies))
		(eval-exp (car bodies) env k)
		(begin
			(eval-exp (car bodies) env k)
			(eval-bodies (cdr bodies) env k)
		)
	)
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.
(define (apply-proc proc-value args k)
	(cases proc-val proc-value
		[prim-proc (op) (apply-prim-proc op args k)]
		[closure (syms arg bodies env)
			(cond
				[(null? syms)
					(run-closure bodies (extend-env (list arg) (list args) env) k)
				]
				[(null? arg)
					(if (= (length args) (length syms))
						(run-closure bodies (extend-env syms args env) k)
						(error 'apply-proc "wrong number of arguments to #<procedure>")
					)
				]
				[else
					(run-closure bodies (extend-env
						(append syms (list arg))
						(set-args (append syms arg) args)
					env) k)
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

(define (run-closure bodies env k)
	(if (null? (cdr bodies))
		(eval-exp (car bodies) env k)
		(begin
			(eval-exp (car bodies) env k)
			(run-closure (cdr bodies) env k)
		)
	)
)

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.
(define (apply-prim-proc prim-proc args k)
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
		[(apply) (apply-proc (1th args) (2th args) k)]
		[(map) (let loop ([proc (1th args)] [args (cdr args)])
			(if (null? (car args)) '()
				(cons (apply-proc proc (map car args) k) (loop proc (map cdr args)))
			)
		)]
		[(quotient) (quotient (1th args) (2th args))]
		[(member) (member (1th args) (2th args))]
		[(append) (apply append args)]
		[(eqv?) (apply eqv? args)]
		[(list-tail) (if (check-args args 2) (list-tail (1th args) (2th args)) (error-num-args prim-proc))]
		[(even?) (if (check-args args 1) (even? (1th args)) (error-num-args prim-proc))]
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

; "read-eval-print" loop.
(define (rep)
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
