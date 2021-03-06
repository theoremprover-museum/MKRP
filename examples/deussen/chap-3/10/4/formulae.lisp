';;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
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
(TYPE ABBVOR(ABB ELEMENT ELEMENT))
(* ABKUERZUNG *)
(ALL I,J :INDEX ALL MAL:VERKN G(I J) = MPROD(U(I) U(J) MAL))
(* DEFINITION ISOMORPH *)
(ALL PHI:ABB ALL A,B:MENGE HOMOMORPH(PHI A B) AND BIJEKTIV(PHI A B) EQV ISOMORPH(PHI A B))
(* VORAUSSTZUNG3104(F MAL PHI I1 I2 J1 J2 J3 E1 E2) BEDEUTET : *)
(* HG(F MAL) AND MINIMAL-BED-L(F MAL) AND MINIMAL-BED-R(F MAL) AND *)
(*  (ALL J1:INDEX EL-IND (J1 I1) IMPL RIDEAL (U (J1) F MAL) AND MINIMAL (U (J1) F MAL)) AND                   )
(*  (ALL J2:INDEX EL-IND (J2 I2) IMPL LIDEAL (U (J2) F MAL) AND MINIMAL (U (J2) F MAL)) AND                   )
(*  (ALL J1,J2:INDEX EL-IND (J1 I1) AND EL-IND (J2 I2) IMPL GLEICH (MPROD (U (J1) U (J2) MAL) SM (U (J1)
												    U (J2)))))                                                                                              
(*  AND EL-IND (J1 I1) AND EL-IND (J2 I2) AND EL-IND (J3 I2) AND GR (G (J1 J2) MAL E1) AND GR                 )
(*  (G (J1 J3) MAL E2) AND ABBILDUNG (PHI G (J1 J2) G (J1 J3)) AND                                            )
(*  (ALL X:ELEMENT EL (X G (J1 J2)) IMPL ABBVOR (PHI X APPLYV (MAL X E2)))                                    )
(* LEMMA 3.10.4.BIJ *)
(ALL PHI:ABB  ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2,J3:INDEX  ALL E1,E2:ELEMENT
     VORAUSSETZUNG3104(F MAL PHI I1 I2 J1 J2 J3 E1 E2) IMPL BIJEKTIV(PHI G(J1 J2) G(J1 J3)))
(* LEMMA 3.10.4.HOM *)
(ALL PHI:ABB  ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2,J3:INDEX  ALL E1,E2:ELEMENT
     VORAUSSETZUNG3104(F MAL PHI I1 I2 J1 J2 J3 E1 E2) IMPL HOMOMORPH(PHI G(J1 J2) G(J1 J3)))

(ALL PHI:ABB  ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2,J3:INDEX  ALL E1,E2:ELEMENT
     HG(F MAL)
     AND MINIMAL-BED-L(F MAL)
     AND MINIMAL-BED-R(F MAL)
     AND (ALL J1:INDEX EL-IND(J1 I1) IMPL RIDEAL(U(J1) F MAL) AND MINIMAL(U(J1) F MAL))
     AND (ALL J2:INDEX EL-IND(J2 I2) IMPL LIDEAL(U(J2) F MAL) AND MINIMAL(U(J2) F MAL))
     AND EL-IND(J1 I1)
     AND EL-IND(J2 I2)
     AND( ALL J1,J2:INDEX         EL-IND(J1 I1) AND EL-IND(J2 I2)
	 IMPL GLEICH(MPROD(U(J1) U(J2) MAL) SM(U(J1) U(J2))))
     AND EL-IND(J3 I2)
     AND GR(G(J1 J2) MAL E1)
     AND GR(G(J1 J3) MAL E2)
     AND ABBILDUNG(PHI G(J1 J2) G(J1 J3))
     AND (ALL X:ELEMENT EL(X G(J1 J2)) IMPL ABBVOR(PHI X APPLYV(MAL X E2)))
     AND VORAUSSETZUNG3104(F MAL PHI I1 I2 J1 J2 J3 E1 E2)
     IMPL ISOMORPH(PHI G(J1 J2) G(J1 J3)))