; fractal curves
; http://www.shido.info/lisp/scheme8.html
;
; This does not work, the error message is
;  ;The object (), passed as an argument to safe-car, is not a pair.

; (x . y) as a dot pair
(define _x car)
(define _y cdr)
(define point cons)

; (1 2 3) (4 5 6) -> (3 2 1 4 5 6) 
(define (rappend l r)
  (let loop ((l0 l) (r0 r))
    (if (null? l0)
        r0
        (loop (_y l0) (cons (_x l0) r0)))))

; To determine the (r, 1-r) point for p1 and p2.
(define (divide p1 p2 r)
  (point (+ (* r (_x p1)) (* (- 1.0 r) (_x p2))) 
         (+ (* r (_y p1)) (* (- 1.0 r) (_y p2)))))

; To print out data points to a file.
(define (print-curve points fout)
  (with-output-to-file fout
    (lambda ()
      (for-each ; map for "side effects" 
        (lambda (p)
          (display (_x p))
          (display " ")
          (display (_y p))
          (newline))
        points))))

; A higher order function for fractal curve.
;   proc   :: a process for the "next" point
;   n      :: the number of iteration
;   points :: a list of given points
;   fout   :: out put file
(define (fractal proc n points fout)
  (let loop ((i 0) (p points))
    (if (= n i)
        (print-curve p fout)
        (loop
          (+ i 1)
          (let iter ((q points) (acc '()))
            (if (null? (_y p))
                (reverse (cons (_x q) acc))
                (iter
                  (_y q)
                  (rappend (proc (first q) (second q)) acc)))))))
        'done)

;
(define (c-curve p1 p2)
  (let ((p3 (divide p1 p2 0.5))) ; (0.5,0.5) division
    (list
      p1
      (point (+ (_x p3) (- (_y p3) (_y p2)))
             (+ (_y p3) (- (_x p2) (_y p3)))))))

;
(define dragon-curve
  (let ((n 0))
    (lambda (p1 p2)
      (let ((op (if (even? n)
                    +
                    -))
            (p3 (divide p1 p2 0.5)))
      (set! n (+ n 1))
      (list p1
            (point (op (_x p3) (- (_y p3) (_y p2)))
                   (op (_y p3) (- (_x p2) (_x p3)))))))))

;
(define (koch p1 p2)
  (let ((p3 (divide p1 p2 2/3))
        (p4 (divide p1 p2 1/3))
        (p5 (divide p1 p2 0.5))
        (c  (/ (sqrt 3) 2)))
       (list
         p1
         p3
         (point (- (_x p5) (* c (- (_y p4) (_y p3))))
                (+ (_y p5) (* c (- (_x p4) (_x p3)))))
         p4)))
