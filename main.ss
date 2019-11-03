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

(define run-13 #f)
(define run-14 #f)
(define run-16 #f)
(define run-17 #f)
(define run-exam_02 #f)
(define test-mode #f)
(define run-tests #f)

(if run-tests
	(if test-mode (begin
		(map pretty-print (reverse (list
			#f
		)))
	) (begin
		(if run-13 (begin (newline) (pretty-print 'assignment_13) (load "tests/tests_13.ss") (r)))
		(if run-14 (begin (newline) (pretty-print 'assignment_14) (load "tests/tests_14.ss") (r)))
		(if run-16 (begin (newline) (pretty-print 'assignment_16) (load "tests/tests_16.ss") (r)))
		(if run-exam_02 (begin (newline) (pretty-print 'exam_02) (load "tests/tests_exam_02.ss")))
		(if run-17 (begin (newline) (pretty-print 'assignment_17) (load "tests/tests_17.ss") (r)))
	))
)
