; ch01.scm
; To load this file, type (load "ch01.scm")
;
; SICP 
; chapter 01
; Building Abstractions with Procedures
; 1.1 The Elements of Programming
; 1.1.1 Expressions
; 1.1.2 Naming and the Environment
; 1.1.3 Evaluating Combinations
; 1.1.4 Compound Procedures
; 1.1.5 The Substitution Model for Procedure Application
; 1.1.6 Conditional Expressions and Predicates
; Exercise 1.3
(define (sum-of-largers a b c)
  (define (larger-one a b)
    (if (> a b) a b))
  (define (smaller-one a b)
    (if (< a b) a b))
  (+ (larger-one a b) (larger-one c (smaller-one a b))))

(define (sum-of-square-of-largers a b c)
  (sum-of-largers (* a a) (* b b) (* c c)))
  
; 1.1.7 Example: Square Roots by Newton's Method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b) (/ (+ a b) 2))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; exercise 1.7
(define (good-enough? g x)
  (< (helper g x) 0.001))

(define (helper g x)
  (abs (/ (- (square g) x) x)))

; 1.1.8 Procedures as Black-Box Abstractions
(define (another-sqrt x)
  (define (good? g x)
    (< (abs (/ (- (square g) x) x)) 0.001))
  (define (improve g x) (average g (/ x g)))
  (define (another-sqrt-iter g x)
    (if (good? g x)
        g
        (another-sqrt-iter (improve g x) x)))
  (another-sqrt-iter 1.0 x))
  
; 1.2 Procedures and the Processes They Generate
; 1.2.1 Linear Recursion and Iteration
; The factorial function of recursion version.
(define (rfactorial n)
  (if (or (= n 1) (< n 1))
      1
      (* n (rfactorial (- n 1)))))
; The tail-recursion version.
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

; exercise 1.9
(define (rplus a b)
  (if (= a 0) b
              (inc (rplus (dec a) b))))
; The tail-recursive version.
(define (plus a b)
  (if (= a 0) b
              (plus (dec a) (inc b))))
(define (inc a) (+ a 1))
(define (dec a) (- a 1))

; excercise 1.10
; Ackermann's function.
(define (ack x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ack (- x 1) (ack x (- y 1))))))

; 1.2.2
(define (t_fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (t_fib (- n 1))
                 (t_fib (- n 2))))))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

; Example: Counting charge
(define (count-change amount) (cc amount 5))
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

; Exercise 1.11
(define (f11 n)
  (cond ((< n 3) n)
        (else (+ (f11 (- n 1))
                 (* 2 (f11 (- n 2)))
                 (* 3 (f11 (- n 3)))))))

; 1.2.3 Orders of Growth                 

; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (< (abs angle) 0.01)
      angle
      (p (sine (/ angle 3.0)))))

; 1.2.4 Exponentiation
; Linear recursive exp, O(n)-steps and O(n)-space.
(define (rexp b n)
  (if (= n 0)
      1
      (* b (rexp b (- n 1)))))
; Tail recursive version, O(n)-steps and O(1)-space.
(define (texp b n) (texp-iter b n 1))
(define (texp-iter b counter product)
  (if (= counter 0)
      product
      (texp-iter b (- counter 1) (* b product))))
; A fast version, O(log n)-steps
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

; Exercise 1.17
; Linear recursive version.
(define (lin-mult a b)
  (if (= b 0)
      0
      (+ a (lin-mult a (- b 1)))))
; Tail-recursive version.
(define (tail-mult a b) 
  (define (iter a b p)
    (if (= b 0) 
        p
        (iter a (- b 1) (+ p a))))
  (iter a b 0))
; Fast version.
(define (mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (mult a (/ b 2))))
        (else (+ a (mult a (- b 1))))))
(define (double x) (* 2 x))

; Exercise 1.19
(define (fib n) 
  (define (fib-iter a b p q count)
    (cond 
      ((= count 0) b)
      ((even? count)
         (fib-iter 
           a
           b
           (+ (square p) (square q))
           (+ (* 2 p q) (square q))
           (/ count 2)))
      (else (fib-iter 
              (+ (* b q) (* a q) (* a p))
              (+ (* b p) (* a q))
              p
              q
              (- count 1)))))
  (fib-iter 1 0 0 1 n))

; 1.2.5 Greatest Common Divisors
(define (my-gcd a b)
  (cond ((= a b) a)
        ((< a b) (my-gcd b a))
        (else (my-gcd (- a b) b))))
(define (my-gcd2 a b)
  (if (= b 0) 
      a
      (my-gcd2 b (remainder a b))))

; 1.2.6 Example: Testing for Primality
; O(sqrt n) algorithm
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))
