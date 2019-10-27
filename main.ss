; Cooper Anderson, Joe Peters
; Interpreter Project
; 2019-10-14
; CSSE304-03

(load "chez-init.ss")

(define load-all
	(lambda ()
		(load "datatypes.ss")
		(load "parse.ss")
		(load "env.ss")
		(load "interpreter.ss")
	)
)

(load-all)

(define run-13 #t)
(define run-14 #t)
(define run-16 #t)
(define run-17 #f)
(define test-mode #f)
(define run-tests #f)

(if run-tests
	(if test-mode (begin
		(map pretty-print (reverse (list
		; (unparse-exp (syntax-expand (parse-exp '
	    ;   (let ([n 5])
		; (let f ([n n] [acc 1])
		;   (if (= n 0)
		;       acc
		;       (f (sub1 n) (* acc n))))))
		; ))
		(eval-one-exp '
			(let ([n 5])
				(letrec
					(
						[f (lambda (n acc)
							(if (= n 0) acc
								(f (sub1 n) (* acc n))
							)
						)]
					)
					(f n 1)
				)
			)
		)
		)))
	) (begin
		(if run-13 (begin (newline) (pretty-print 'assignment_13) (load "tests/tests_13.ss") (r)))
		(if run-14 (begin (newline) (pretty-print 'assignment_14) (load "tests/tests_14.ss") (r)))
		(if run-16 (begin (newline) (pretty-print 'assignment_16) (load "tests/tests_16.ss") (r)))
		(if run-17 (begin (newline) (pretty-print 'assignment_17) (load "tests/tests_17.ss") (r)))
	))
)
