;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*- 

(SORT ELEM,MENGE,VERKN:ANY)
(TYPE APPLY (VERKN ELEM ELEM) :ELEM)
(TYPE INVERS (VERKN ELEM) :ELEM)
(ALL U,V:MENGE ALL X:ELEM ME (U V) AND EL (X U) IMPL EL (X V))
(ALL F:MENGE  ALL PHI:VERKN  ALL X,E:ELEM  
     G (F PHI E) AND EL (X F) IMPL INVERS (PHI INVERS (PHI X)) = X)

(ALL PHI:VERKN  ALL U,F:MENGE  ALL X,Y,E:ELEM  
     G (F PHI E) AND ME (U F)
     IMPL  
     ALL S,T:ELEM  
     EL (S U) AND EL (T U)
     IMPL  
     EL (APPLY (PHI INVERS (PHI T) S) U)
     AND  
     (ALL Z:ELEM EL (Z U) IMPL EL (INVERS (PHI Z) U))
     AND  
     EL (X U)
     AND  
     EL (Y U)
     IMPL  
     EL (APPLY (PHI X Y) U))