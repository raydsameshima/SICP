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

  







