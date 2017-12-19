; 09 inout

; (open-input-file filename)
; (read-char port)
; eof-object?
(define (read-file file-name)
  (let ((port (open-input-file file-name)))
    (let loop ((ls1 '()) 
               (c (read-char port))) ; c is a word of port
      (if (eof-object? c)
          (begin
            (close-input-port port)      ; when we hit eof, close the file
            (list->string (reverse ls1))); and returns the containts
          (loop (cons c ls1) 
                (read-char port))))))

; 1 ]=> (read-file "hello.scm")
; 
; ;Value 24: "; Hello world as a variable.\n(define vhello \"Hello world!\")\n\n; Hello world as a function.\n(define fhello (lambda ()\n                  \"Hellow world!\"))\n\n; 7 error> (load \"hello.scm\")\n; \n; ;Loading \"hello.scm\"... done\n; ;Value: fhello\n; \n; 7 error> vhello\n; \n; ;Value 15: \"Hello world!\"\n; \n; 7 error> fhello\n; \n; ;Value 16: #[compound-procedure 16 fhello]\n; \n; 7 error> (fhello)\n; \n; ;Value 17: \"Hellow world!\"\n"
; 
; 1 ]=> (display (read-file "hello.scm"))
; ; Hello world as a variable.
; (define vhello "Hello world!")
; 
; ; Hello world as a function.
; (define fhello (lambda ()
;                   "Hellow world!"))
; 
; ; 7 error> (load "hello.scm")
; ; 
; ; ;Loading "hello.scm"... done
; ; ;Value: fhello
; ; 
; ; 7 error> vhello
; ; 
; ; ;Value 15: "Hello world!"
; ; 
; ; 7 error> fhello
; ; 
; ; ;Value 16: #[compound-procedure 16 fhello]
; ; 
; ; 7 error> (fhello)
; ; 
; ; ;Value 17: "Hellow world!"
; ;Unspecified return value

; (call-with-input-file filename procedure)
(define (read-file2 file-name)
  (call-with-input-file file-name
    (lambda (p)
      (let loop ((ls1 '()) (c (read-char p)))
        (if (eof-object? c)
            (begin
              (close-input-port p)
              (list->string (reverse ls1)))
            (loop (cons c ls1) (read-char p)))))))

; (with-input-from-file filename procedure)
; We do not have to close file explicitly.
(define (read-file3 file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls1 '()) (c (read-char)))
        (if (eof-object? c)
            (list->string (reverse ls1))         ; procedure
            (loop (cons c ls1) (read-char)))))))

; read
; It can read Scheme expression in a port.
(define (s-read file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls '()) (s (read)))
        (if (eof-object? s)
            (reverse ls)
            (loop (cons s ls) (read)))))))

; 1 ]=> (s-read "hello.scm")
; 
; ;Value 26: ((define vhello "Hello world!") (define fhello (lambda () "Hellow world!")))
; 

; list separator
(define (group-list ls sep)
  (letrec 
    ((iter (lambda (ls0 ls1)
             (cond
               ((null? ls0) (list ls1))
               ((eqv? (car ls0) sep) 
                  (cons (cons sep ls1) (iter (cdr ls0) '())))
               (else (iter (cdr ls0) (cons (car ls0) ls1)))))))
    (map reverse (iter ls '()))))

; Using newline as a separator.
(define (read-lines file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls1 '()) (c (read-char)))
        (if (eof-object? c)
            (map list->string (group-list (reverse ls1) #\Newline))  ; *
            (loop (cons c ls1) (read-char)))))))

;
(define (my-copy-file from to)
  (let ((port-from (open-input-file from))
        (port-to   (open-output-file to)))
    (let loop ((c (read-char port-from)))
      (if (eof-object? c)
          (begin
            (close-input-port port-from)
            (close-output-port port-to))
          (begin
            (write-char c port-to)
            (loop (read-char port-from)))))))
            
;
(define (print-lines . cs)
  (let loop ((ls cs))
    (if (pair? ls) ; singleton or longer list
        (begin
          (display (car ls))
          (newline)
          (loop (cdr ls)))
        '())))

(define (print-lines- . cs)
  (let loop ((ls cs))
    (if (null? ls)
        '()
        (begin
          (display (car ls))
          (newline)
          (loop (cdr ls))))))

