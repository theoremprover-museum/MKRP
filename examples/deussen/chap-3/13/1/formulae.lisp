;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
(SORT MENGE,VERKN,ELEMENT,INDEX,INDEXMENGE,ABB:ANY)
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
(TYPE ABBVOR(ABB ELEMENT ELEMENT))
(* DEFINITION 3.500000 *)
(ALL T,F:MENGE  ALL MAL:VERKN  
     LIDEAL(T F MAL) IMPL(MINIMAL(T F MAL) IMPL(ALL M:MENGE LIDEAL(M F MAL) IMPL(ME(M T) IMPL GLEICH(M T)))))
(* TEILMENGE UND SCHNITTMENGE *)
(ALL A,B:MENGE ME(SM(A B) B))
(* SATZ 3.10.3 *)
(ALL L,R,F:MENGE  ALL MAL:VERKN  
     LIDEAL(L F MAL) AND MINIMAL(L F MAL) AND RIDEAL(R F MAL) AND MINIMAL(R F MAL)
     IMPL (EX E:ELEMENT UG(SM(R L) F MAL E)))
(* FOLGERUNG 3.700000 *)
(ALL G,F:MENGE ALL MAL:VERKN ALL E:ELEMENT UG(G F MAL E) IMPL LIDEAL(G F MAL))
(* FOLGERUNG AUS SATZ 3.3.1 *)
(ALL F:MENGE  ALL MAL:VERKN  ALL I:INDEXMENGE  ALL L:MENGE  
     (ALL J:INDEX EL-IND(J I) IMPL LIDEAL(SM(U(J) L) F MAL)) IMPL LIDEAL(SM(VER-IND(I) L) F MAL))

(ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL LAMBDA:INDEX  
     HG(F MAL)
     AND (ALL J1:INDEX EL-IND(J1 I1) IMPL RIDEAL(U(J1) F MAL) AND MINIMAL(U(J1) F MAL))
     AND (ALL J2:INDEX EL-IND(J2 I2) IMPL LIDEAL(U(J2) F MAL) AND MINIMAL(U(J2) F MAL))
     AND EL-IND(LAMBDA I2)
     IMPL GLEICH(SM(VER-IND(I1) U(LAMBDA)) U(LAMBDA)))