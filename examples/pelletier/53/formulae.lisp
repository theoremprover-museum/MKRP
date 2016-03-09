;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-


(* AXIOMS *)
(EX X,Y NOT X = Y AND (ALL Z Z = X OR Z = Y))

(* THEOREMS *)
((EX Z (ALL X ((EX W (ALL Y (P (X Y) EQV Y = W)))
	       EQV
	       X = Z)))
 EQV
 (EX Z (ALL X ((EX W (ALL Y (P (Y X) EQV Y = W)))
	       EQV
	       X = Z))))