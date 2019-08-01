; 10 => 10
; (+ 5 3 4) => 12
; (- 9 1) => 8
; (/ 6 2) => 3
; (+ (* 2 4) (- 4 6)) => 6
; (define a 3) =>
; (define b (+ a 1) =>
; (+ a b (* a b)) => 19
; (= a b) => #f
; (if (and (> b a) (< b (* a b))) b a) => 4
; (cond ((= a 4) 6)
;       ((= b 4) (+ 6 7 a))
;       (else 25)) => 16
; (+ 2 (if (> b a) b a)) => 6
; (* (cond ((> a b) a)
;          ((< a b) b)
;          (else -1))
;    (+ a 1)) => 16

(/ (+ 5 4 (- 2 3 (+ 6 (/ 4 5))))
   (* 3 (- 6 2) (- 2 7)))

(define (square a)
  (* a a))

(define (sum-of-squares a b c)
  (cond ((and (> b a) (> c a)) (+ (square b) (square c)))
        ((and (> a b) (> c b)) (+ (square a) (square c)))
        ((and (> a c) (> b c)) (+ (square a) (square b)))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.6: the problem with Eva Lu Ator's implementation is that since the interpreter uses applicative-order, any recursive
; function call in the if block will result in an infinite loop.

(define (new-good-enough? guess last-guess x)
  (< (/ (abs (- guess last-guess)) guess) 0.001))

(define (new-sqrt-iter guess last-guess x)
  (if (new-good-enough? guess last-guess x)
      guess
      (new-sqrt-iter (improve guess x) guess
                 x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x x))

(define (cubert x)
  (cubert-iter 1.0 x x))

(define (cubert-iter guess last-guess x)
  (if (new-good-enough? guess last-guess x)
      guess
      (cubert-iter (cubert-improve guess x) guess x)))

(define (cubert-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))

; Exercise 1.9

;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

; Demonstrate: (+ 4 5)
; (inc (+ (dec 4) 5)
; (inc (+ 3 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9

; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10)
; (A (- 1 1) (A 1 (- 10 1)))
; (A 0 (A 1 9))
; (A 0 (this expands a lot))
; => 1024
; (A 2 4) => 65536
; (A 3 3) => 65536

(define (f n) (A 0 n))
; => 2n
(define (g n) (A 1 n))
; => 2 * (g (- n 1))
;    2 * 2 * (g (- n 2))
; etc. 2^n
(define (h n) (A 2 n))
; => (A 1 (A 2 (- n 1))
; => (g (h (- n 1))
; 2 ^ n ^ n
; (h 1) => 2 (2^1)
; (h 2) => 4 (2^2)
; (h 3) => 16 (2^4)
; (h 4) => 65536 (2^16)
; 2 ^ (n)*(n - 1)
; 2 ^ (2 * 1)
; 2 ^ (3 * 2)
; 2 ^ (h (n - 1)) => this is it!
(define (k n) (* 5 n n)) ; 5n^2

; Exercise 1.11

; f(n) = n if n < 3
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3

; f also equals
;

(define (exercise1-11-recursive n)
  (if (< n 3)
      n
      (+ (f (- n 1) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (exercise1-11-i n)
  (if (< n 3)
      n
      (exercise1-11-iter 0 1 2 3 n)))

; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = f(2) + 2f(1) + 3f(0)
;      = 2 + 2 + 0 = 4
; f(4) = f(3) + 2f(2) + 3f(1)
;      = 4 + 4 + 3 = 11
; f(5) = f(4) + 2f(3) + 3f(2)
;      = 11 + 8 + 6 = 25

(define (exercise1-11-iter a b c i n)
  (if (> i n)
      c
      (exercise1-11-iter b c
                         (+ c (* 2 b) (* 3 a)) (+ i 1)
                         n)))

; Pascal
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;1 5 10 10 5 1
;
; P(0) = 1
; P(1) = 1
; P(2) = 1
; P(3) = 1
; P(4) = 2 -> P(2) + P(3)
; P(5) = 1
; P(6) = 1
; P(7) = 3 -> P(4) + P(5)
; P(8) = 3 -> P(5) + P(6)
; P(9) = 1
; P(10) = 1
; P(11) = 4 -> P(7) + P(8)
; P(12) = 6 -> P(8) + P(9)
; P(13) = 4 -> P(9) + P(10)
; P(14) = 1
; P(15) = 1
; P(16) = 5 -> P(11) + P(12)
; P(17) = 10 -> P(12) + P(13)
; P(18) = 10 -> P(13) + P(14)
; P(19) = 5 -> P(14) + P(15)
; P(20) = 1
; P(21) = 1

; R(1) = 1
; R(2) = 2
; R(3) = 2
; R(4) = 4

(define (pascal n)
  (pascal-iter 0 0 1 n))

(define (pascal-iter i i-in-row r n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        ((= n 2) 1)
        ((and (= i n) (= i-in-row 0)) 1) ; If we're at n, and we're at the beginning of a row, return 1
        ((and (= i n) (= i-in-row (- r 1))) 1) ; If we're at n, and we're at the end of a row, return 1
        ((= i n) (+ (pascal (- n r)) (pascal (- n (- r 1))))) ; The main clause. If we're at n, and we're within a row, do (pascal (- n r-len)) (pascal (- n r-len 1))
        ((= i-in-row (- r 1)) (pascal-iter (+ i 1) 0 (+ r 1) n)) ; If we're at the end of a row, reset our state and increase the row length by 1
        (else (pascal-iter (+ i 1) (+ i-in-row 1) r n)))) ; Continue iterating through the row

; Exercise 1.14
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (count-change 11)
; (cc 11 5)
; (+ (cc 11 4) (cc -39 5))
; (+ (cc 11 4) 0)
; (cc 11 4)
; (+ (cc 11 3) (cc (- 11 25) 4)
; (cc 11 3)
; (+ (cc 11 2) (cc (- 11 10) 3))
; (+ (cc 11 2) (cc 1 3))
; (+ (cc 11 2) (+ (cc 1 2) (cc (- 1 10) 3))
; (+ (cc 11 2) (cc 1 2))
; (+ (cc 11 2) (+ (cc 1 1) (cc -3 2))
; (+ (cc 11 2) (cc 1 1))
; (+ (cc 11 2) (+ (cc 1 0) (cc 0 1))
; (+ (cc 11 2) (+ 0 1))
; (+ (cc 11 2) 1)
; (+ (+ (cc 11 1) (cc 6 2) 1)
; (+ (+ (cc 11 1) (+ (cc 6 1) (cc 1 2)) 1)
; (+ (+ (cc 11 1) (+ (cc 6 1) (+ (cc 1 1) (cc -4 2))) 1)
; (+ (+ (cc 11 1) (+ (cc 6 1) (cc 1 1)) 1)
; (+ (+ (cc 11 1) (+ (cc 6 1) (+ (cc 1 0) (cc 0 1))) 1)
; (+ (+ (cc 11 1) (+ (cc 6 1) 1) 1)
; (+ (+ (cc 11 1) (+ (+ (cc 6 0) (cc 5 1)) 1) 1))
; (+ (+ (cc 11 1) (+ (cc 5 1)) 1) 1))
; (+ (cc 11 1) (+ (cc 5 0) (cc 4 1)) 1 1)
; (+ (cc 11 1) 1 1 1)
; (+ 1 1 1 1)
; 4

; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; (sine 12.15)
; (p (sine 4.05))
; (- (* 3 (sine 4.05) (* 4 (cube (sine 4.05)))))
; => (sine 4.05)
; (p (sine 1.349999999)
; (- (* 3 (sine 1.34999999) (* 4 (cube (sine 1.349999))
; Basically just keep diving by 3.
; => .44999996666
; => 0.149999999988888
; one more. So, 4 times? Maybe 5?

; 1.15b: log3(a)? something like that

; Exercise 1.16

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; invariant: a * b^n is unchanged
(define (fast-expt-iter b n a)
  (cond ((= n 0) a) ; b^n = 0, so return a
        ((= n 1) (* b a))
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

; Exercise 1.17

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((even? a) (fast-mult (halve a) (double b)))
        (else (+ b (fast-mult (- a 1) b)))))

; Exercise 1.18

(define (fast-mult-iter a b x)
  (cond ((= a 0) x)
        ((even? a) (fast-mult-iter (halve a) (double b) x)) ; ab+x is invariant
        (else (fast-mult-iter (- a 1) b (+ x b))))) ; ab+x is invariant

; (fast-mult-iter 2 5 0)
; (fast-mult-iter 1 10 0)
; (fast-mult-iter 0 10 10)
; (+ 5 10)

; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
