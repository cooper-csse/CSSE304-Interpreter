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
(load "tests_14.ss")


(define run-all
	#t
) ; Modify this line to switch testing modes

(if run-all (r) (begin
	(map pretty-print (reverse (list

	)))
))
