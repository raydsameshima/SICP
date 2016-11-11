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

; example, 100cent

