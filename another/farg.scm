; hello with name
(define hello
  (lambda (name)
    (string-append "Hello " name "!")))

; sum of three numbers
(define sum3
  (lambda (a b c)
    (+ a b c)))

; rest-parameter
(define three-args+
  (lambda (a b c . d)
    (list a b c d)))

; 11 error> (load "farg.scm")
; 
; ;Loading "farg.scm"... done
; ;Value: three-args+
; 
; 11 error> (three-args+ 2 3 4)
; 
; ;Value 20: (2 3 4 ())
; 
; 11 error> (three-args+ 2 3 4 5)
; 
; ;Value 21: (2 3 4 (5))
; 
; 11 error> (three-args+ 2 3 4 5 6)
; 
; ;Value 22: (2 3 4 (5 6))
; 
; 11 error> (three-args+ 2 3 4 5 6 7 8)
; 
; ;Value 23: (2 3 4 (5 6 7 8))

; instead of lambda
(define (hello name)
  (string-append "hello " name "!"))

; inc and dec
(define (inc a)
  (+ a 1))
(define (dec a)
  (- a 1))

; from degree to radian
(define pi (* 4 (atan 1.0)))
(define (radian d)
  (* pi (/ d 180)))

; const. velocity motion
(define (distance vx t)
  (* vx t))

; free fall time
(define (ff-time vy)
  (/ (* 2 vy) 9.81))

;
(define (landing-site v theta)
  (* v (cos (radian theta)) (ff-time (* v (sin (radian theta))))))

; 12 error> (load "farg.scm")
; 
; ;  Loading "farg.scm"... done
; ;Value: landing-site
; 
; 12 error> (landing-site 40 30)
; 
; ;Value: 141.2477722788075
