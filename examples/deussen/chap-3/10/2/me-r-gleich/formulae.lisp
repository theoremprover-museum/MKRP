;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*- 

(SORT MENGE,VERKN,ELEMENT,INDEX,INDEXMENGE:ANY)
(TYPE LIDEAL(MENGE MENGE VERKN))
(TYPE RIDEAL(MENGE MENGE VERKN))
(TYPE UHG(MENGE MENGE VERKN))
(TYPE ME(MENGE MENGE))
(TYPE NICHTLEER(MENGE))
(TYPE HG(MENGE VERKN))
(TYPE NICHTLEER-IND(INDEXMENGE))
(TYPE GLEICH(MENGE MENGE))
(TYPE EL-IND(INDEX INDEXMENGE))
(TYPE EL(ELEMENT MENGE))
(TYPE MINIMAL(MENGE MENGE VERKN))
(TYPE MINIMAL-BED-L(MENGE VERKN))
(TYPE MINIMAL-BED-R(MENGE VERKN))
(TYPE MPROD(MENGE MENGE VERKN) :MENGE)
(TYPE U(INDEX) :MENGE)
(TYPE VER-IND(INDEXMENGE) :MENGE)
(TYPE SM-IND(INDEXMENGE) :MENGE)
(TYPE APPLYV(VERKN ELEMENT ELEMENT) :ELEMENT)
(TYPE F*X(MENGE ELEMENT VERKN) :MENGE)
(TYPE SM(MENGE MENGE) :MENGE)
(TYPE T*(MENGE VERKN MENGE ELEMENT) :MENGE)
(* SATZ 3,6 : A IMPL B *)
(ALL F,R:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
     HG(F MAL) AND RIDEAL(R F MAL) AND MINIMAL(R F MAL) AND EL(X R) IMPL F*X(F X MAL) = R)

(ALL R1,R2,F:MENGE  ALL MAL:VERKN  ALL X  
     :ELEMENT RIDEAL(R1 F MAL)
     AND RIDEAL(R2 F MAL)
     AND MINIMAL(R1 F MAL)
     AND MINIMAL(R2 F MAL)
     AND EL(X R1)
     AND EL(X R2)
     AND HG(F MAL)
     IMPL R1 = R2)