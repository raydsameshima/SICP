; 12 symbol type
; http://www.shido.info/lisp/scheme_sym.html
;
; A symbol is the address for string.
; We do not need to compare each character, but the address only.
;
; 1 ]=> (eq? (string->symbol "Hello") 'Hello)
; 
; ;Value: #f
; 
; 1 ]=> (eq? (string->symbol "Hello") (string->symbol "Hello"))
; 
; ;Value: #t

; http://sicp.ai.mit.edu/Fall-2003/manuals/scheme-7.5.5/doc/scheme_12.html#SEC107
(load-option 'hash-table)

(define (list->symbol ls)
  (string->symbol (list->string (reverse ls))))

(define (char-in c . ls)
  (let loop ((ls0 ls))
    (if (null? ls0)
        #f
        (or (char=? c (car ls0))
            (loop (cdr ls0))))))

(define (read-words file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((w '()) (wls '()))
        (let ((c (read-char)))
          (cond
            ((eof-object? c)
             (reverse (if (pair? w)
                          (cons (list->symbol w) wls)
                          wls)))
            ((char-in c #\Space #\Linefeed #\Tab #\, #\. #\  #\( #\) 
                        #\= #\? #\! #\; #\:)
             (loop '() (if (pair? w)
                           (cons (list->symbol w) wls)
                           wls)))
            (else
             (loop (cons (char-downcase c) w wls)))))))))

(define (sort-by-freq al)
  (sort al (lambda (x y) (> cdr x) (cdr y))))

(define (hashtable->alist h)
  (let ((keys (hashtable-keys h)) 
        (size (hashtable-size h)))
    (let loop ((i 0) (ls1 '()))
      (if (= i size)
          ls1
          (let* ((k (vector-ref keys i)) (v (hashtable-ref h k #f)))
            (loop (+ i 1) (cons (cons k v) ls1)))))))

(define (wc file-name)
  (let ((wh (make-eq-hash-table))) ; different name
    (let loop ((ls (read-words file-name)))
      (if (null? ls)
          (sort-by-freq (hashtable->alist wh))
          (let ((k (car ls)))
            (hashtable-set! wh k (+ 1 (hashtable-ref wh k 0)))
            (loop (cdr ls)))))))

; It does not work...why?
