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

(define l load-all)

; (load "tests.ss")
; (r)
