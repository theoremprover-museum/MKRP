;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
		
(SORT ELEM,VERKN,MENGE,ABB:ANY)
(TYPE APPLYA (ABB ELEM) :ELEM)
(TYPE APPLYV (VERKN ELEM ELEM) :ELEM)
(ALL U,F:MENGE  ALL PHI:VERKN  ALL E:ELEM  
     G (F PHI E) AND ME (U F) AND EL (E U) AND UHG (U F PHI)
     IMPL  
     EINS (E PHI U))
(ALL U,F:MENGE  ALL PHI:VERKN  ALL CHI:ABB  ALL C,Z,E:ELEM  
     G (F PHI E)
     AND  
     ME (U F)
     AND  
     BIJEKTIV (CHI U U)
     AND  
     EL (Z U)
     AND  
     EL (C U)
     AND  
     APPLYA (CHI Z) = APPLYV (PHI Z C)
     IMPL  
     (EX X:ELEM APPLYV (PHI X C) = Z AND EL (X U)))
(ALL U,V:MENGE ALL X:ELEM ME (U V) AND EL (X U) IMPL EL (X V))
(ALL F:MENGE  ALL PHI:VERKN  ALL X,Y,E:ELEM  
     G (F PHI E) AND EL (X F) AND APPLYV (PHI Y X) = X IMPL Y = E)

(ALL U,F:MENGE  ALL PHI:VERKN  ALL Z,E:ELEM  
     G (F PHI E) AND ME (U F) AND ENDLICH (U)
     IMPL  
     UHG (U F PHI)
     AND  
     EL (Z U)
     AND  
     ALL C:ELEM  
     EL (C U)
     IMPL  
     EX CHI:ABB  
     BIJEKTIV (CHI U U)
     AND  
     ALL X:ELEM  
     EL (X U)
     IMPL  
     APPLYA (CHI X) = APPLYV (PHI X C)
     IMPL  
     EINS (E PHI U))

; Kein Beweis auf Siemens