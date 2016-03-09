;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(* Distributivity *)
(all x15,x16,x14 MULT (PLUS (X15 X16) X14) = PLUS (MULT (X15 X14) MULT (X16 X14))) 
(all x12,x13,x11 MULT (X11 PLUS (X12 X13)) = PLUS (MULT (X11 X12) MULT (X11 X13))) 

(* Multiplication *)
(all x9,x10,x8 MULT (MULT (X8 X9) X10) = MULT (X8 MULT (X9 X10))) 
(all x7 MULT (X7 1) = X7) 
(all x6 MULT (1 X6) = X6) 

(* Addition *)
(all x3,x4,x5 PLUS (PLUS (X3 X4) X5) = PLUS (X3 PLUS (X4 X5))) 
(all x2 PLUS (MINUS (X2) X2) = 0) 
(all x1 PLUS (0 X1) = X1)





(* theorem *)

(all x,y mult(minus(x) y) = mult(x minus(y)))