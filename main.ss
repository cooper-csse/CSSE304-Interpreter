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
		(load "syntax-expand.ss")
		(load "continuation.ss")
		(load "interpreter.ss")
	)
)

(load-all)

(define run-13 #f)
(define run-14 #f)
(define run-16 #f)
(define run-exam_02 #f)
(define run-17a #f)
(define run-17b #f)
(define run-18a #f)
(define run-18b #f)
(define test-mode #f)
(define run-tests #f)

(if run-tests
	(if test-mode (begin
		(map pretty-print (reverse (list

		)))
	) (begin
		(if run-13 (begin (newline) (pretty-print 'assignment_13) (load "tests/tests_13.ss") (r)))
		(if run-14 (begin (newline) (pretty-print 'assignment_14) (load "tests/tests_14.ss") (r)))
		(if run-16 (begin (newline) (pretty-print 'assignment_16) (load "tests/tests_16.ss") (r)))
		(if run-exam_02 (begin (newline) (pretty-print 'exam_02) (load "tests/tests_exam_02.ss")))
		(when (or run-17a run-17b)
			(load "tests/tests_17.ss")
			(if run-17a (begin (newline) (pretty-print 'assignment_17a) (run-all-a)))
			(if run-17b (begin (newline) (pretty-print 'assignment_17b) (run-all-b)))
		)
		(when (or run-18a run-18b)
			(load "tests/tests_18.ss")
			(if run-18a (begin (newline) (pretty-print 'assignment_18a) (run-all-a)))
			(if run-18b (begin (newline) (pretty-print 'assignment_18b) (run-all-b)))
		)
	))
)
