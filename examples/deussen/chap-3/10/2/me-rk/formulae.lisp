;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-

(SORT MENGE,VERKN,ELEMENT,INDEX,INDEXMENGE:ANY)
(TYPE LIDEAL (MENGE MENGE VERKN))
(TYPE RIDEAL (MENGE MENGE VERKN))
(TYPE UHG (MENGE MENGE VERKN))
(TYPE ME (MENGE MENGE))
(TYPE NICHTLEER (MENGE))
(TYPE HG (MENGE VERKN))
(TYPE EL (ELEMENT MENGE))
(TYPE MINIMAL (MENGE MENGE VERKN))
(TYPE MPROD (MENGE MENGE VERKN) :MENGE)
(TYPE APPLYV (VERKN ELEMENT ELEMENT) :ELEMENT)
(TYPE F*X (MENGE ELEMENT VERKN) :MENGE)
(TYPE SM (MENGE MENGE) :MENGE)
(TYPE SET (ELEMENT) :MENGE)
(TYPE VER (MENGE MENGE) :MENGE)
(TYPE GR (MENGE VERKN ELEMENT))
(TYPE INVERS (VERKN ELEMENT) :ELEMENT)
(TYPE MINIMAL-BED-L (MENGE VERKN))
(TYPE MINIMAL-BED-R (MENGE VERKN))
(TYPE EL-IND (INDEX INDEXMENGE))
(TYPE U (INDEX) : MENGE)
(TYPE E (INDEX INDEX) : ELEMENT)
(TYPE NICHTLEER-IND (INDEXMENGE))
(TYPE GLEICH (MENGE MENGE))
(TYPE VER-IND (INDEXMENGE) : MENGE)
(TYPE SM-IND (INDEXMENGE) : MENGE)
(TYPE IDEAL (MENGE MENGE VERKN))
(TYPE G (INDEX INDEX) :MENGE)
(TYPE GLEICH-E (ELEMENT ELEMENT))
(* SCHNITTMENGENAXIOM *)
(ALL A,B:MENGE ALL X:ELEMENT EL (X SM (A B)) IMPL EL (X A) AND EL (X B))
(* SATZ 3.9 *)
(* VOR (F MAL I1) BEDEUTET : HG (F MAL)
   AND  
   MINIMAL-BED-L (F MAL)
   AND  
   (ALL K1:INDEX EL-IND (K1 I1) IMPL RIDEAL (U (K1) F MAL) AND MINIMAL (U (K1) F MAL)) *)
(ALL F:MENGE ALL MAL:VERKN ALL I1:INDEXMENGE VOR (F MAL I1) IMPL LIDEAL (VER-IND (I1) F MAL))
(* SATZ 3.8 *)
(ALL F,L,M:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
     HG (F MAL) AND LIDEAL (L F MAL) AND LIDEAL (M F MAL) AND EL (X M) IMPL GLEICH (F*X (L X MAL) M))
(* EIGENSCAHFT F*X *)
(ALL F,L,M:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
     GLEICH (F*X (L X MAL) M) AND EL (X M) AND HG (F MAL) AND ME (M F) AND ME (L F)
     IMPL  
     (EX R:ELEMENT EL (R L) AND X = APPLYV (MAL R X)))
(* TEIL DEFINITION LIDEAL *)
(ALL F,L:MENGE ALL MAL:VERKN LIDEAL (L F MAL) IMPL ME (L F))

(ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2:INDEX  ALL X:ELEMENT  
     HG (F MAL)
     AND  
     MINIMAL-BED-L (F MAL)
     AND  
     MINIMAL-BED-R (F MAL)
     AND  
     (ALL K1:INDEX EL-IND (K1 I1) IMPL RIDEAL (U (K1) F MAL) AND MINIMAL (U (K1) F MAL))
     AND  
     (ALL K2:INDEX EL-IND (K2 I2) IMPL LIDEAL (U (K2) F MAL) AND MINIMAL (U (K2) F MAL))
     AND  
     EL-IND (J1 I1)
     AND  
     EL-IND (J2 I2)
     AND  
     EL (X SM (U (J1) U (J2)))
     AND  
     VOR (F MAL I1)
     IMPL  
     (EX R:ELEMENT EL (R VER-IND (I1)) AND X = APPLYV (MAL R X)))