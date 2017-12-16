; if
(define (sum-gp a0 r n)
  (* a0
     (if (= r 1)
         n
         (/ (- 1 (expt r n)) (- 1 r)))))

; absolute value
(define (absolute a)
  (if (> a 0)
      a
      (- a)))

; reciprocal
(define (reciprocal a)
  (if (not (= a 0))
      (/ 1 a)
      #f))

; to ascii (33-126)
(define (ascii n)
  (if (and (<= 33 n) (>= 126 n))
      (integer->char n)
      #f))

;
(define (positive-product a b c)
  (if (and (> a 0) (> b 0) (> c 0))
      (* a b c)
      #f))

;
(define (fee age)
  (cond
    ((or (<= age 3) (>= age 65)) 0)
    ((and (>= age 4) (<= age 6)) 50)
    ((and (>= age 7) (<= age 12)) 50)
    ((and (>= age 13) (<= age 15)) 50)
    ((and (>= age 16) (<= age 18)) 50)
    (else 200)))

;
(define (grade pt)
  (cond
    ((and (>= 100 pt) (<= 80 pt)) 'A)
    ((and (>= 79 pt) (<= 60 pt)) 'B)
    ((and (>= 59 pt) (<= 40 pt)) 'C)
    ((and (>= 0 pt) (<= 39 pt)) 'D)
    (else #f)))

; begin
(define (foo)
  (begin 
    (display "hello scheme.")
    (newline)
    (display "I love scheme (and Haskell).")
    (newline)
    'done))

; eq?, eqv?, equal?
; eq? : address comparison
; eqv? : data-type and value comparison
; equall? : for list and string

; 1 ]=> (define str "hello")
; 
; ;Value: str
; 
; 1 ]=> (eq? str str)
; 
; ;Value: #t
; 
; 1 ]=> (eq? str "hello")
; 
; ;Value: #f
; 
; 1 ]=> (eqv? str "hello")
; 
; ;Value: #f
; 
; 1 ]=> (equal? str "hello")
; 
; ;Value: #t
; 1 ]=> (eq? 1 1)
; 
; ;Value: #t
; 
; 1 ]=> (eq? 1 1.0)
; 
; ;Value: #f
; 
; 1 ]=> (eqv? 1 1.0)
; 
; ;Value: #f
; 
; 1 ]=> (equal? 1 1.0)
; 
; ;Value: #f
; 
; 1 ]=> (equal? 1 1)
; 
; ;Value: #t
; 
; 1 ]=> (eqv? 1 1)
; 
; ;Value: #t 
; 
; 1 ]=> (define ls1 (list 1 2 3))
; 
; ;Value: ls1
; 
; 1 ]=> (define ls2 (list 1 2 3))
; 
; ;Value: ls2
; 
; 1 ]=> (equal? ls1 ls2)
; 
; ;Value: #t
; 
; 1 ]=> (eq? ls1 ls2)
; 
; ;Value: #f
; 
; 1 ]=> (eqv? ls1 ls2)
; 
; ;Value: #f
