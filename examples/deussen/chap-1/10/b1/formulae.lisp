;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-
(* Axioms *)
(SORT ELEM,MENGE,VERKN : ANY)
(TYPE APPLY (VERKN ELEM ELEM) : ELEM)
(TYPE INVERS (VERKN ELEM) :ELEM)
(ALL PHI:VERKN  ALL U,F:MENGE  ALL E:ELEM  
     G (F PHI E) AND ME (U F) AND EL (E U) IMPL EINS (E PHI U))
(ALL U,V:MENGE ALL X:ELEM ME (U V) AND EL (X U) IMPL EL (X V))
(ALL PHI:VERKN  ALL F:MENGE  ALL X,E:ELEM  
     G (F PHI E) AND EL (X F) IMPL APPLY (PHI INVERS (PHI X) X) = E)


(* Theorem *)
(ALL PHI:VERKN  ALL U,F:MENGE  ALL Z,E:ELEM  
     G (F PHI E) AND ME (U F)
     IMPL  
     (ALL X,Y:ELEM  
	  EL (X U) AND EL (Y U)
	  IMPL  
	  EL (APPLY (PHI INVERS (PHI Y) X) U)
	  AND  
	  EL (Z U)
	  IMPL  
	  EINS (E PHI U)))

;;; Attention: graph collapses