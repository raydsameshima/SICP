; ch01.scm
; To load this file, type (load "ch01.scm")
;
; SICP chapter 01
;
; 1.1.7 sqrt with Newton method
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

; 1.1.8
(define (another-sqrt x)
  (define (good? g x)
    (< (abs (/ (- (square g) x) x)) 0.001))
  (define (improve g x) (average g (/ x g)))
  (define (another-sqrt-iter g x)
    (if (good? g x)
        g
        (another-sqrt-iter (improve g x) x)))
  (another-sqrt-iter 1.0 x))
  
; 1.2.1
(define (rfactorial n)
  (if (or (= n 1) (< n 1))
      1
      (* n (rfactorial (- n 1)))))
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

; excercise 1.10
(define (accurman x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (accurman (- x 1) (accurman x (- y 1))))))







