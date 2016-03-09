;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-



(* THEOREMS *)
((EX Z (ALL X ((EX W (ALL Y (P (X Y) EQV Y = W)))
	       EQV
	       X = Z)))
 EQV
 (EX Z (ALL X ((EX W (ALL Y (P (Y X) EQV Y = W)))
	       EQV
	       X = Z))))