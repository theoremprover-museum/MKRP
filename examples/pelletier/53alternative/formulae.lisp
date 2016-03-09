;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(ex z,w (all x,y P(x y) eqv (x = z) and y = w))

(* Theorems *)


((EX Z (ALL X ((EX W (ALL Y (P (X Y) EQV Y = W)))
	       EQV
	       X = Z)))
 EQV
 (EX Z (ALL X ((EX W (ALL Y (P (Y X) EQV Y = W)))
	       EQV
	       X = Z))))