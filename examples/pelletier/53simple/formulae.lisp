;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-


(* AXIOMS *)
(NOT 1 = 2)

(* THEOREMS *)


(* THEOREMS *)
(((((((P (1 1) EQV 1 = 1) and (P (1 2) EQV 2 = 1))
	  or
	  ((P (1 1) EQV 1 = 2) and (P (1 2) EQV 2 = 2)))
	 EQV
	 1 = 1)
	and
	((((P (2 1) EQV 1 = 1) and (P (2 2) EQV 2 = 1))
	  or
	  ((P (2 1) EQV 1 = 2) and (P (2 2) EQV 2 = 2)))
	 EQV
	 2 = 1))
  or
  (((((P (1 1) EQV 1 = 1) and (P (1 2) EQV 2 = 1))
	  or
	  ((P (1 1) EQV 1 = 2) and (P (1 2) EQV 2 = 2)))
	 EQV
	 1 = 2)
	and
	((((P (2 1) EQV 1 = 1) and (P (2 2) EQV 2 = 1))
	  or
	  ((P (2 1) EQV 1 = 2) and (P (2 2) EQV 2 = 2)))
	 EQV
	 2 = 2)))
 EQV
 ((((((P (1 1) EQV 1 = 1) and (P (2 1) EQV 2 = 1))
	  or
	  ((P (1 1) EQV 1 = 2) and (P (2 1) EQV 2 = 2)))
	 EQV
	 1 = 1)
	and
	((((P (1 2) EQV 1 = 1) and (P (2 2) EQV 2 = 1))
	  or
	  ((P (1 2) EQV 1 = 2) and (P (2 2) EQV 2 = 2)))
	 EQV
	 2 = 1))
  or
  (((((P (1 1) EQV 1 = 1) and (P (2 1) EQV 2 = 1))
	  or
	  ((P (1 1) EQV 1 = 2) and (P (2 1) EQV 2 = 2)))
	 EQV
	 1 = 2)
	and
	((((P (1 2) EQV 1 = 1) and (P (2 2) EQV 2 = 1))
	  or
	  ((P (1 2) EQV 1 = 2) and (P (2 2) EQV 2 = 2)))
	 EQV
	 2 = 2))))