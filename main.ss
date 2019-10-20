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

(define run-test #t)

(load "tests_14.ss")

(if (not run-test) (r) (begin
	(display 'lambda-regression-tests)
	(test-lambda-regression-tests)
	(display 'lambda-with-variable-args)
	(test-lambda-with-variable-args)
))
