; 10 assignement

; !!! SIDE EFFECTS !!!

; set!
; 1 ]=> (define var 1)
; 
; ;Value: var
; 
; 1 ]=> var
; 
; ;Value: 1
; 
; 1 ]=> (set! var (* var 10))
; 
; ;Value: 1
; 
; 1 ]=> var
; 
; ;Value: 10
; 
; 1 ]=> (let ((i 1))
;            (set! i (+ i 3))
;                       i)
; 
; ;Value: 4

(define (make-bank-account amount)
  (lambda (n)
    (let ((m (+ amount n)))
      (if (negative? m)
          'error
          (begin
            (set! amount m)
            amount)))))

; set-car!, set-cdr!
; queue(Abstract Data Type): First In, First Out 
(define (make-queue)
  (cons '() '()))
;
(define (enqueue! queue obj)
  (let ((lobj (cons obj '())))
    (if (null? (car queue))
      (begin
        (set-car! queue lobj)
        (set-cdr! queue lobj))
      (begin
        (set-cdr! (cdr queue) lobj)
        (set-cdr! queue lobj)))
    (car queue)))
;
(define (dequeue! queue)
  (let ((obj (car (car queue))))
    (set-car! queue (cdr (car queue)))
    obj))

