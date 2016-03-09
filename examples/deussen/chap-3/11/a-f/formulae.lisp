;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
(SORT MENGE,VERKN,ELEMENT,INDEX,INDEXMENGE:ANY)
(TYPE LIDEAL(MENGE MENGE VERKN))
(TYPE RIDEAL(MENGE MENGE VERKN))
(TYPE UHG(MENGE MENGE VERKN))
(TYPE ME(MENGE MENGE))
(TYPE NICHTLEER(MENGE))
(TYPE HG(MENGE VERKN))
(TYPE NICHTLEER-IND(INDEXMENGE))
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
(TYPE GLEICH(MENGE MENGE))
(TYPE GLEICH-E(ELEMENT ELEMENT))
(* LEMMA 3.11.A *)
(ALL F:MENGE  ALL I1,I2:INDEXMENGE  ALL MAL:VERKN  ALL J1,J2,J5,J6:INDEX  
     HG(F MAL)
     AND MINIMAL-BED-L(F MAL)
     AND MINIMAL-BED-R(F MAL)
     AND (ALL J3:INDEX EL-IND(J3 I1) IMPL RIDEAL(U(J3) F MAL) AND MINIMAL(U(J3) F MAL))
     AND (ALL J4:INDEX EL-IND(J4 I2) IMPL LIDEAL(U(J4) F MAL) AND MINIMAL(U(J4) F MAL))
     AND EL-IND(J1 I1)
     AND EL-IND(J2 I2)
     AND GLEICH(MPROD(U(J1) U(J2) MAL) SM(U(J1) U(J2)))
     AND NICHTLEER(SM(U(J1) U(J2)))
     AND ME(SM(U(J1) U(J2)) F)
     AND EL-IND(J5 I1)
     AND EL-IND(J6 I2)
     IMPL (ALL G:ELEMENT EL(G SM(U(J5) U(J6))) IMPL GLEICH(F*X(SM(U(J1) U(J2)) G MAL) SM(U(J1) U(J6)))))
(* F*X FUER ELEMENTE *)
(ALL A,F:MENGE  ALL X:ELEMENT  ALL MAL:VERKN  
     HG(F MAL) AND ME(A F) AND EL(X A) AND GLEICH(F*X(A X MAL) A)
     IMPL (EX Y:ELEMENT GLEICH-E(APPLYV(MAL Y X) X) AND EL(Y A)))

(ALL F:MENGE  ALL I1,I2:INDEXMENGE  ALL MAL:VERKN  ALL J1,J2  
     :INDEX HG(F MAL)
     AND MINIMAL-BED-L(F MAL)
     AND MINIMAL-BED-R(F MAL)
     AND (ALL J3:INDEX EL-IND(J3 I1) IMPL RIDEAL(U(J3) F MAL) AND MINIMAL(U(J3) F MAL))
     AND (ALL J4:INDEX EL-IND(J4 I2) IMPL LIDEAL(U(J4) F MAL) AND MINIMAL(U(J4) F MAL))
     AND EL-IND(J1 I1)
     AND EL-IND(J2 I2)
     AND GLEICH(MPROD(U(J1) U(J2) MAL) SM(U(J1) U(J2)))
     AND NICHTLEER(SM(U(J1) U(J2)))
     AND ME(SM(U(J1) U(J2)) F)
     IMPL( ALL G:ELEMENT  EX G0:ELEMENT  
	  EL(G SM(U(J1) U(J2))) IMPL GLEICH-E(APPLYV(MAL G0 G) G) AND EL(G0 SM(U(J1) U(J2)))))