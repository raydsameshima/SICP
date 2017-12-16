; Hello world as a variable.
(define vhello "Hello world!")

; Hello world as a function.
(define fhello (lambda ()
                  "Hellow world!"))

; 7 error> (load "hello.scm")
; 
; ;Loading "hello.scm"... done
; ;Value: fhello
; 
; 7 error> vhello
; 
; ;Value 15: "Hello world!"
; 
; 7 error> fhello
; 
; ;Value 16: #[compound-procedure 16 fhello]
; 
; 7 error> (fhello)
; 
; ;Value 17: "Hellow world!"
