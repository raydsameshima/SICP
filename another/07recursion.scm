; 07 recursion
; factorial
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

; using tail-recursion and accumulator
(define (fact2 n)
  (helper n 1))
(define (fact2helper a b)
  (if (<= a 1)
      b
      (fact2helper (- a 1) (* b a))))

; (define (fact3 n)
;   (let (((h a b) (if (<= a 1)
;                      b
;                      (h (- a 1) (* b a)))))
;        (h n 1)))

(define (my-length ls)
  (if (null? ls)
      0
      (+ 1 (my-length (cdr ls)))))

(define (my-sum ls)
  (if (null? ls)
      0
      (+ (car ls) (my-sum (cdr ls)))))

; from solution
(define (remove x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)))
           ((if (eqv? x h)
                (lambda (y) y) ; if x=h then id hits (remove x (cdr ls))
                (lambda (y) (cons h y)))
            (remove x (cdr ls))))))

(define (remove2 x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)) 
            (hs (cdr ls)))
           (if (eqv? x h)
               (remove2 x hs)
               (cons h (remove2 x hs))))))

; tail-recursion
(define (position x ls)
  (position-aux x ls 0))
(define (position-aux x ls i)
  (cond
    ((null? ls) #f)
    ((eqv? x (car ls)) i)
    (else (position-aux x (cdr ls) (+ i 1)))))

; tail-recursion
(define (my-reverse ls)
  (revtail ls ()))
(define (revtail l r)
  (if (null? l)
      r
      (revtail (cdr l) (cons (car l) r))))

;
(define (my-sum-tail ls)
  (sum-tail ls 0))
(define (sum-tail ls a)
  (if (null? ls)
      a
      (sum-tail (cdr ls) (+ a (car ls)))))

; named let
(define (fact-let n)
  (let loop ((n1 n) (p n))         ; initialization (both n1 and p become n)
    (if (= n1 1)                    
        p
        (let ((m (- n1 1)))
             (loop m (* p m)))))) ; iteration

; original version (above version is better)
(define (fact-let- n)
  (let loop ((n1 n) (p n))
    (if (= n1 1)
        p
        (loop (- n1 1) (* p (- n1 1)))))) ; m=(- n1 1)

;
(define (rem x ls)
  (let loop ((ls0 ls) (ls1 ()))
    (if (null? ls0)                         ; no-match
        (my-reverse ls1)                    ; naturally reverse order
        (loop (cdr ls0)                     ; iteration
              (if (eqv? x (car ls0))
                  ls1
                  (cons (car ls0) ls1)))))) ; not-match => cons

;
(define (position x ls)
  (let loop ((ls0 ls) (i 0))
    (cond
      ((null? ls0) #f)
      ((eqv? x (car ls0)) i)
      (else (loop (cdr ls0) (+ i 1))))))

;
(define (my-rev-let ls)
  (let loop ((ls0 ls) (ls1 ()))
    (if (null? ls0)
        ls1
        (loop (cdr ls0) (cons (car ls0) ls1)))))

;
(define (my-sum-let ls)
  (let loop ((ls0 ls) (n 0))
    (if (null? ls0)
        n
        (loop (cdr ls0) (+ (car ls0) n)))))
;
(define (my-product ls)
  (let loop ((ls0 ls) (n 1))
    (if (null? ls0)
        n
        (loop (cdr ls0) (* (car ls0) n)))))

; 
(define (range n)
  (let loop ((i 0) (ls ()))
    (if (= i n)
        (reverse ls)
        (loop (+ i 1) (cons i ls)))))

; ave takes arbitrary parameters
(define (ave . ls)
  (let loop ((sum 0) (ls1 ls))
    (if (null? ls1)
        (/ sum (length ls))
        (loop (+ sum (car ls1)) (cdr ls1)))))

; letrec (we can call itself in it (*))
(define (fact-letrec n)
  (letrec 
    ((iter (lambda (n1 p)
                   (if (= n1 1)
                       p
                       (let ((m (- n1 1)))
                         (iter m (* p m)))))))     ; *
    (iter n n)))

;
(define (my-reverse-letrec ls)
  (letrec ((itr (lambda (l0 l1)
                        (if (null? l0)
                            l1
                            (itr (cdr l0) (cons (car l0) l1))))))
          (itr ls ())))

;
(define (my-sum-letrec ls)
  (letrec ((itr (lambda (l0 n)
                        (if (null? l0)
                            n
                            (itr (cdr l0) (+ (car l0) n))))))
  (itr ls 0)))
