; 13 association list and hashtable
; http://www.shido.info/lisp/scheme_ah.html

; association list
; An association list is a list of pairs.
; E.g., 
;   '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 
;   '((1 2 3) (4 5 6) (7 8 9))
; Use assoc for search, it uses symbols and fast.
; 
; 1 ]=> (define wc '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8)))
; 
; ;Value: wc
; 
; 1 ]=> wc
; 
; ;Value 4: ((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8))
; 
; 1 ]=> (assq 'hi wc)
; 
; ;Value 5: (hi . 3)
; 
; 1 ]=> (assq 'you wc)
; 
; ;Value 6: (you . 8)
; 
; 1 ]=> (assq 'i wc)
; 
; ;Value: #f

; hash table




