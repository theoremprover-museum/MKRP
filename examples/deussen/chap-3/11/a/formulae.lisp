;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
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
(TYPE G(INDEX INDEX) :MENGE)
(TYPE BIJEKTIV(ABB MENGE MENGE))
(TYPE HOMOMORPH(ABB MENGE MENGE))
(TYPE ISOMORPH(ABB MENGE MENGE))
(TYPE ABVOR(ABB ELEMENT ELEMENT))
(* EIGENSCHAFT DER SCHNITTMENGE *)
(ALL A,B:MENGE ALL X:ELEMENT EL(X SM(A B)) IMPL(EL(X A) AND EL(X B)))
(* SATZ 3.799997 *)
(ALL L,M,H:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
     HG(H MAL)
     AND LIDEAL(L H MAL)
     AND LIDEAL(M H MAL)
     AND MINIMAL(L H MAL)
     AND MINIMAL(M H MAL)
     AND EL(X M)
     IMPL F*X(L X MAL) = M)
(* ASSOZIATIVITAET VON MAL *)
(ALL A,B,F:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
     HG(F MAL) AND ME(A F) AND ME(B F) IMPL F*X(MPROD(A B MAL) X MAL) = MPROD(A F*X(B X MAL) MAL))
(* FOLGERUNG AUS DER DEFINITION RECHTSIDEAL *)
(ALL R,F:MENGE ALL MAL:VERKN RIDEAL(R F MAL) IMPL ME(R F))
(* FOLGERUNG AUS DER DEFINITION LINKSIDEAL *)
(ALL L,F:MENGE ALL MAL:VERKN LIDEAL(L F MAL) IMPL ME(L F))

(ALL F:MENGE  ALL I1,I2:INDEXMENGE  ALL MAL:VERKN  ALL J1,J2,J3,J4:INDEX  
     HG(F MAL)
     AND MINIMAL-BED-L(F MAL)
     AND MINIMAL-BED-R(F MAL)
     AND (ALL J5:INDEX EL-IND(J5 I1) IMPL RIDEAL(U(J5) F MAL) AND MINIMAL(U(J5) F MAL))
     AND (ALL J6:INDEX EL-IND(J6 I2) IMPL LIDEAL(U(J6) F MAL) AND MINIMAL(U(J6) F MAL))
     AND (ALL K1,K2:INDEX EL-IND(K1 I1) AND EL-IND(K2 I2) IMPL MPROD(U(K1) U(K2) MAL) = SM(U(K1) U(K2)))
     AND EL-IND(J1 I1)
     AND EL-IND(J2 I2)
     AND EL-IND(J3 I1)
     AND EL-IND(J4 I2)
     IMPL (ALL X:ELEMENT EL(X SM(U(J3) U(J4))) IMPL F*X(SM(U(J1) U(J2)) X MAL) = SM(U(J1) U(J4))))