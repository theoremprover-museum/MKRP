;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(* Distributivity *)
(all x15,x16,x14 * (+ (X15 X16) X14) = + (* (X15 X14) * (X16 X14))) 
(all x12,x13,x11 * (X11 + (X12 X13)) = + (* (X11 X12) * (X11 X13))) 

(* *iplication *)
(all x9,x10,x8 * (* (X8 X9) X10) = * (X8 * (X9 X10))) 
(all x7 * (X7 1) = X7) 
(all x6 * (1 X6) = X6) 

(* Addition *)
(all x3,x4,x5 + (+ (X3 X4) X5) = + (X3 + (X4 X5))) 
(all x2 + (- (X2) X2) = 0) 
(all x1 + (0 X1) = X1)





(* theorem *)

(all y *(0 y) = 0)