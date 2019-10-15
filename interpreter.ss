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
		; [let-type (inner)
		; 	(cases let-type inner
		; 		[normal-let (vars vals body)
		; 			(extend-env
		; 				vars
		; 				(map (lambda (item) (eval-exp item env)) vals)
		; 				env
		; 			)
		; 		]
		; 	)
		; ]
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

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define (apply-proc proc-value args)
	(cases proc-val proc-value
		[prim-proc (op) (apply-prim-proc op args)]
		; You will add other cases
		[else (error 'apply-proc
			"Attempt to apply bad procedure: ~s"
			proc-value)
		]
	)
)

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not cons list null? assq eq?
								equal? atom? length list->vector list? pair?
								procedure? vector->list vector make-vector
								vector-ref vector? number? symbol? set-car!
								set-cdr! vector-set! display newline
								car cdr caar cadr cdar cddr caaar caadr
								cadar caddr cdaar cdadr cddar cdddr
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
		[(procedure?) (if (check-args args 1) (pair? (1th args)) (error-num-args prim-proc))]
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
		[(=) (apply = args)]
		[(<) (apply < args)]
		[(>) (apply > args)]
		[(<=) (apply <= args)]
		[(>=) (apply >= args)]
		[else (error 'apply-prim-proc
			"Bad primitive procedure name: ~s"
			prim-op)
		]
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
	(top-level-eval (parse-exp exp))
)
