; 08 higher order functions
; http://www.shido.info/lisp/scheme8.html
; 
; 1 ]=> (sort '(7883 9099 6729 2828 7754 4179 5340 2644 2958 2239) <)
; 
; ;Value 7: (2239 2644 2828 2958 4179 5340 6729 7754 7883 9099)
; 
; 1 ]=> (define (two x y) (< (modulo x 100) (modulo y 100)))
; 
; ;Value: two
; 
; 1 ]=> (sort '(7883 9099 6729 2828 7754 4179 5340 2644 2958 2239) two)
; 
; ;Value 8: (2828 6729 2239 5340 2644 7754 2958 4179 7883 9099)
; 
; 1 ]=> (map * '(1 2 3) '(4 5 6))
; 
; ;Value 14: (4 10 18)

; map
(define (double ls)
  (map (lambda (x) (* 2 x)) ls))

(define (diff ls rs)
  (map (lambda (l r) (- l r)) ls rs))

; filter
(define (filter-even ls)
  (filter even? ls))

(define (filter-10-100 ls)
; (filter (lambda (x) (and (<= 10 x) (<= x 100))) ls))
  (filter (lambda (x) (<= 10 x 100)) ls))
; <= can take arbitrary number of arguments.

; fold
(define (sum ls)
  (fold + 0 ls))
(define (sqrt-sum-sq ls)
  (sqrt (sum (map (lambda (x) (* x x)) ls))))
 
; 4 error> (sqrt-sum-sq '(1 2 3 4 5))
; 
; ;Value: 7.416198487095663
; 
; 4 error> (map (lambda (x) (* x x)) '(1 2 3 4 5))
; 
; ;Value 7: (1 4 9 16 25)
; 
; 4 error> (sum '(1 4 9 16 25))
; 
; ;Value: 55
; 
; 4 error> (sqrt 55)
; 
; ;Value: 7.416198487095663
 
; sort
(define (kyori x y)
  (abs (- x y)))
(define (sort-ave ls)
  (let ((ave (/ (apply + ls) (length ls))))
    (sort ls (lambda (x y) (< (kyori x ave) (kyori y ave))))))

;
(define (sort-string cs)
  (sort cs (lambda (x y) (> (string-length x) (string-length y)))))

; apply
(define (sqrt-sum-sq-a ls)
  (sqrt (apply + (map (lambda (x) (* x x)) ls))))

; arbitrary number of inputs
(define (ave . ls)
  (/ (apply + ls) (length ls)))

; homemade higher order functions
(define (member-if proc ls)
  (cond
    ((null? ls) #f)
    ((proc (car ls)) ls)
    (else (member-if proc (cdr ls)))))

(define (my-member proc obj ls)
  (cond
    ((null? ls) #f)
    ((proc obj (car ls)) ls)
    (else (my-member proc obj (cdr ls)))))

;
(define (my-filter f ls)
  (cond
    ((null? ls) '())
    ((f (car ls)) (cons (car ls) (my-filter f (cdr ls))))
    (else (my-filter f (cdr ls)))))

;
(define (my-map fun . lss)
  (letrec ((iter (lambda (fun lss) ; this iteration is the map for one list
                         (if (null? lss)
                             '()
                             (cons (fun (car lss))
                                   (iter fun (cdr lss))))))
           (map-rec (lambda (fun lss)
                            (if (memq '() lss) ; membership with eq?
                                '()
                                (cons (apply fun (iter car lss))
                                      (map-rec fun (iter cdr lss)))))))
  (map-rec fun lss)))
; 
; 1 ]=> (my-map + '(1 2 3) '(10 20 30) '(100 200 300))
; 
; ;Value 21: (111 222 333)
