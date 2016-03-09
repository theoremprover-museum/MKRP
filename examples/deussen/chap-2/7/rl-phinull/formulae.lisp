;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*- 

(SORT MENGE,ELEMENT,ABB,VERKN:ANY)
(TYPE COMP(ABB ABB) :ABB)
(TYPE INVERS(ABB) :ABB)
(TYPE W(MENGE) :MENGE)
(TYPE RESTRICTION(ABB MENGE) :ABB)
(TYPE PSI:ABB)
(TYPE CONC:VERKN)
(TYPE T:MENGE)
(TYPE MAL:VERKN)
(TYPE EHG(MENGE VERKN) :MENGE)
(TYPE BILD(ABB MENGE) :MENGE)
(ALL PHI:ABB ALL A,B:MENGE ISOMORPH(PHI A B) IMPL HOMOMORPH(PHI A B))
(ALL PHI1,PHI2:ABB  ALL A,B,C:MENGE  
     HOMOMORPH(PHI1 A B) AND HOMOMORPH(PHI2 B C) IMPL HOMOMORPH(COMP(PHI2 PHI1) A C))
(ALL PHI1,PHI2:ABB  ALL A,B,C,D:MENGE  
     ISOMORPH(PHI1 A B) AND ME(D B) AND ABBILDUNG(PHI2 BILD(INVERS(PHI1) D) C)
     IMPL ABBILDUNG(COMP(PHI2 INVERS(PHI1)) D C))
(ALL A:MENGE FREI(W(A) CONC))
(ALL A:MENGE EHG(A CONC) = W(A))
(ALL A:MENGE ME(A W(A)))
(ALL A,B,H:MENGE  ALL MAL1,MAL2:VERKN  ALL PHINULL:ABB  
     FREI(A MAL1) AND HG(H MAL2) AND ABBILDUNG(PHINULL B H) AND EHG(B MAL1) = A
     IMPL (EX PHI:ABB HOMOMORPH(PHI EHG(B MAL1) H) AND RESTRICTION(PHI B) = PHINULL))
(* LEMMA 2.7.RL.PHINULL.RESTR *)
(ALL X,H:MENGE  ALL MAL2:VERKN  ALL PHI,PHINULL:ABB  
     HG(H MAL2)
     AND ABBILDUNG(PHINULL BILD(INVERS(PSI) X) H)
     AND HOMOMORPH(COMP(PHI PSI) F H)
     AND HOMOMORPH(PHI W(X) H)
     AND ISOMORPH(PSI F W(X))
     AND RESTRICTION(PHI X) = COMP(PHINULL INVERS(PSI))
     IMPL RESTRICTION(COMP(PHI PSI) BILD(INVERS(PSI) X)) = PHINULL)

(ALL X,H:MENGE  ALL MAL2:VERKN  ALL PHINULL:ABB  EX PHI:ABB  
     HG(F MAL) AND ISOMORPH(PSI F W(X)) AND HG(H MAL2) AND ABBILDUNG(PHINULL BILD(INVERS(PHI) X) H)
     IMPL HOMOMORPH(PHI F H) AND RESTRICTION(PHI BILD(INVERS(PSI) X)) = PHINULL)