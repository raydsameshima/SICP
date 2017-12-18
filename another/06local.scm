; 06 local variable
; let

; 
(define (quad-eq a b c)
  (if (zero? a)
     ('error)
     (let ((d (- (* b b) (* 4 a c)))) 
       (if (negative? d)
         ('())
         (let ((e (/ b a -2)))
           (if (zero? d)
             (list e)
             (let ((f (/ (sqrt d) a 2)))
               (list (+ e f) (- e f)))))))))

; The follwoing does not work.
; Since the scope is in the body (let binds body)
; (define (throw0 v deg)
;   (let ((pi (* 4 (atan 1.0)))     ; binds
;         (rad (* pi (/ deg 180)))
;         (vx (* v (cos rad)))
;         (vy (* v (sin rad)))
;         (t (/ (* 2 vy) 9.81)))
;     (* vx t)))                    ; body

; So we can use let*: 
(define (throw- v a)
  (let* ((pi (* 4 (atan 1.0)))
         (rad (* pi (/ a 180)))
         (vx (* v (cos rad)))
         (vy (* v (sin rad)))
         (t (/ (* 2 vy) 9.81)))
    (* vx t)))

;
(define (throw v a)
  (let ((r (/ (* 4 a (atan 1.0)) 180)))
       (/ (* 2 v v (cos r) (sin r)) 9.81)))

