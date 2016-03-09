(;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* abab = 1 impl ba = abb)
(all x,y (x = y) impl (a(x) = a(y)))
(all x,y (x = y) impl (b(x) = b(y)))
(all x (a(a(x)) = x))
(all x (b(b(b(x))) = x))
(all x (a(b(a(b(x)))) = x))


(all x b(a(x)) = a(b(b(x))))






D
; Edited:  11-MAY-1990 10:30
; Authors: KALMAN
; Input:   
; Edited:  11-MAY-1990 10:30
; Authors: KALMAN
; Input:   




; Effect:  
; Value:   
; Effect:  
; Value:   