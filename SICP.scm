;
;(define (new_if predicate then_clause else_clause)
;  (cond (predicate then_clause)
;        (else else_clause)))


;(define (mysqrt x)
;  (define (good_enough? guess x)
;    (< (abs (- (square guess) x)) 0.000001))
	;根据差值计算精度
;  (define (improve guess x)
;    (averarg (/ x guess) guess))

;  (define (sqrt_iter guess x)
;    (if (good_enough? guess x)
;        guess
;        (sqrt_iter (improve guess x) x)))
;  (sqrt_iter 1.0 x))

(define (square x)
  (* x x))

(define (averarg x y)
  (/ (+ x y) 2))


(define (cube x)
  * x x x)



;牛顿法平方根
(define (mysqrt x)
  (define (good_enough? new_guess last_guess)
    (< (abs (/ (- new_guess last_guess) last_guess)) 0.0001))
  ;根据变化率计算精度
  (define (improve guess x)
    (averarg (/ x guess) guess))

  (define (sqrt_iter guess x)
    (if (good_enough? guess (improve guess x))
        guess
        (sqrt_iter (improve guess x) x)))

  (sqrt_iter 1.0 x))

;牛顿法立方根
(define (mycbrt x)
  (define (good_enough? new_guess last_guess)
    (< (abs (/ (- new_guess last_guess) last_guess)) 0.0001))
  (define (improve guess x)
    (/ (+ (/ x (* guess guess))
    	(* 2 guess))
     3))

  (define (cbrt_iter guess x)
    (if (good_enough? guess (improve guess x))
        guess
        (cbrt_iter (improve guess x) x)))

  (cbrt_iter 1.0 x))

;Ackermann函数
(define (A x y)
  (display x)(newline)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
 		((= y 1) 2)
 		(else (A (- x 1) 
 				 (A x (- y 1))))))


;(define (f n)
;  (A 0 n))
;(define (g n)
;  (A 1 n))
;(define (h n)
;  (A 2 n))

;斐波拉契数列树型递归
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
    	(else (+ (fib (- n 1))
    			 (fib (- n 2))))))


(define (count_change amount)
  (cc amount 5))
  (define (cc amount kinds_of_coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds_of_coins 0)) 0)
    	  (else (+ (cc amount
    	  			   (- kinds_of_coins 1))
    			   (cc (- amount
    					  (first_denomination kinds_of_coins))
    				   kinds_of_coins)))))

  (define (first_denomination kinds_of_coins)
    (cond ((= kinds_of_coins 1) 1)
          ((= kinds_of_coins 2) 5)
          ((= kinds_of_coins 3) 10)
          ((= kinds_of_coins 4) 25)
          ((= kinds_of_coins 5) 50)))


;1.11
(define (func n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (func1 n)
  (define (func_iter n1 n2 n3 i n)
  	(if (= n i)
      n3
      (func_iter n1
      			 (* 2 n2) 
      			 (* 3 n3) 
      			 (+ i 1)
      			 n )))
  (func_iter 2 1 0 0 n))

;1.12
;Pasccal Triangle
(define (Pascal row col)
    (if (or (= row col) (= col 1))
        1
        (+ (Pascal (- row 1) 
        		 (- col 1)) 
        	(Pascal (- row 1) 
        		  col ))))
  
;1.12答案
(define (pascal row col)
    (cond ((> col row)
            (error "unvalid col value"))
          ((or (= col 0) (= row col))
            1)
          (else (+ (pascal (- row 1) (- col 1))
                   (pascal (- row 1) col)))))
  

;1.15


(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p(sine (/ angle 3.0)))))

;pow
(define (pow x n)
  (if (= n 0)
      1
      (* x(pow x (- n 1)))))

(define (pow x n)
  (define (iter x count all)
    (if (= count all)
        x
        (* x (iter x (+ count 1) all))))
  (iter x 1 n))

 ;1.16
(define (fast_pow x n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (iter x n a)
    (cond ((= n 0)
            a)
          ((even? n) 
            (iter (square x) 
                  (/ n 2) 
                  a))
          (else 
            (iter x 
                  (- n 1) 
                  (* x a )))))
(iter x n 1))


;1.17递归加法
(define (fast_multiplicative a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (odd? n)
    (not (even? n)))
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  
  (define (iter a b)
    (cond ((= b 0) a)
          ((even? b) (iter (double a) (halve b)))
          (else (+ a (iter a (- b 1))))))
  (iter a b))


;1.18迭代加法
(define (faster_multiplicative a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (odd? n)
    (not (even? n)))
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  
  (define (iter a b temp)
    (cond (( = b 0) temp)
          ((even? b) (iter (double a) (halve b) temp))
          ((odd? b) (iter a (- b 1) (+ a temp)))))
  (iter a b 0))

;1.19
(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) 
           (iter a
                 b
                 (+ (* p p)(* q q))
                 (+ (* 2 p q) (* q q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))







