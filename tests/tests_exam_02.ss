
(map pretty-print (reverse (list
(eval-one-exp ; 14
 '(let ([sum (list 0)])
  (for i from 2 to 5
       (set-car! sum (+ i (car sum))))
  (car sum)))

(eval-one-exp ; (8 14)
 '(let ([sum (list 0)]
        [i 8])
    (for i from 2 to 5
       (set-car! sum (+ i (car sum))))
    (list i (car sum))))

(eval-one-exp  ; 12
 '(let ([sum (list 0)])
  (for i from 2 to 7
       (if (even? i)
	   (set-car! sum (+ i (car sum)))))
  (car sum)))

(eval-one-exp ; 0
 '(let ([sum (list 0)])
  (for i from 2 to 1
       (set-car! sum (+ i (car sum))))
  (car sum)))

(eval-one-exp ; 18
 '(let ([f (lambda (n)
	   (let ([sum (list 0)])
	     (for i from n to (* 2 n)
		  (set-car! sum (+ i (car sum))))
	     (car sum)))])
    (f 3)))

(eval-one-exp ; (18 30 45)
 '(let ([f (lambda (n)
	   (let ([sum (list 0)])
	     (for i from n to (* 2 n)
		  (set-car! sum (+ i (car sum))))
	     (car sum)))])
  (map f '(3 4 5))))

(eval-one-exp ; 10
 '(let ([double-sum (lambda (n)
		    (let ([sum (list 0)])
		      (for i from 1 to n
			   (for j from 1 to i
				(set-car! sum (+ j (car sum)))))
		      (car sum)))])
  (double-sum 3)))

(eval-one-exp ; (1 4 10 20 35 56)
 '(let ([double-sum (lambda (n)
		    (let ([sum (list 0)])
		      (for i from 1 to n
			   (for j from 1 to i
				(set-car! sum (+ j (car sum)))))
		      (car sum)))])
    (map double-sum '(1 2 3 4 5 6))))

(eval-one-exp ; 18
 '(let ([sum (list 0)] [a (list 1)])
  (for i from 2 to (begin (set-car! a (+ 2 (car a))) 4)
	   (set-car! sum (+ i (car a) (car sum))))
  (car sum)))
)))
