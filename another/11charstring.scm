; 11 char string
(define (id x) x)
;
(define (title-style str)
  (let loop ((ls (string->list str))
             (w #t) ; initial value (true for the first letter)
             (acc '()))
    (if (null? ls)
        (list->string (reverse acc))
        (let ((c (car ls)))
          (loop (cdr ls)
                (char-whitespace? c)
                (cons ((if w 
                           char-upcase 
                           id         ) c) acc))))))

; 1 ]=> (title-style "the world is not beautiful...")
; 
; ;Value 37: "The World Is Not Beautiful..."
