; 09 inout

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
          (begin
            (close-input-port p)         ; when we hit eof, close the file
            (list->string (reverse ls1))); and returns the containts
          (loop (cons c ls1) (read-char p))))))

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
