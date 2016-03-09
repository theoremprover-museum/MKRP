;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-

                                                                        





(SORT MENGE,VERKN,ELEMENT,INDEX,INDEXMENGE:ANY)
(           TYPE LIDEAL (MENGE MENGE VERKN))
(           TYPE RIDEAL (MENGE MENGE VERKN))
(           TYPE UHG (MENGE MENGE VERKN))
(           TYPE ME (MENGE MENGE))
(           TYPE NICHTLEER (MENGE))
(           TYPE HG (MENGE VERKN))
(           TYPE EL (ELEMENT MENGE))
(           TYPE MINIMAL (MENGE MENGE VERKN))
(           TYPE MPROD (MENGE MENGE VERKN) :MENGE)
(           TYPE APPLYV (VERKN ELEMENT ELEMENT) :ELEMENT)
(           TYPE F*X (MENGE ELEMENT VERKN) :MENGE)
(           TYPE SM (MENGE MENGE) :MENGE)
(           TYPE SET (ELEMENT) :MENGE)
(           TYPE VER (MENGE MENGE) :MENGE)
(           TYPE GR (MENGE VERKN ELEMENT))
(           TYPE INVERS (VERKN ELEMENT) :ELEMENT)
(           TYPE MINIMAL-BED-L (MENGE VERKN))
(           TYPE MINIMAL-BED-R (MENGE VERKN))
(           TYPE EL-IND (INDEX INDEXMENGE))
(           TYPE U (INDEX) : MENGE)
(           TYPE E (INDEX INDEX) : ELEMENT)
(           TYPE NICHTLEER-IND (INDEXMENGE))
(           TYPE GLEICH (MENGE MENGE))
(           TYPE VER-IND (INDEXMENGE) : MENGE)
(           TYPE SM-IND (INDEXMENGE) : MENGE)
(           TYPE IDEAL (MENGE MENGE VERKN))
(           TYPE G (INDEX INDEX) :MENGE)
(           TYPE GLEICH-E (ELEMENT ELEMENT))
(           * LEMMA 3.10.2.ME-RK *)
(           ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2:INDEX  ALL X:ELEMENT  
             VORAUSSETZUNG3102MER (F MAL I1 I2 J1 J2 X) IMPL (EX R:ELEMENT EL (R VER-IND (I1)) AND X = APPLYV (MAL R X)))
(           * EIGENSCHAFT VER-IND *)
(           ALL F:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2:INDEX  ALL X:ELEMENT  
               VORAUSSETZUNG3102MER (F MAL I1 I2 J1 J2 X)
               IMPL  
               (ALL Y:ELEMENT EL (Y VER-IND (I1)) IMPL (EX J:INDEX EL-IND (J I1) AND EL (Y U (J)))))
(           * EIGENSCHAFT RECHTSIDEAL *)
(           ALL R,F:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
            RIDEAL (R F MAL) AND EL (X R) IMPL (ALL Y:ELEMENT EL (Y F) IMPL EL (APPLYV (MAL X Y) R)))
(           * SCHNITTMENGENAXIOM *)
(           ALL A,B:MENGE ALL X:ELEMENT EL (X SM (A B)) IMPL EL (X A) AND EL (X B))
(           * TEIL DEFINITION LINKSIDEAL *)
(           ALL L,F:MENGE ALL MAL:VERKN LIDEAL (L F MAL) IMPL ME (L F))
(           * TEILMENGENAXIOM *)
(           ALL A,B:MENGE ME (A B) IMPL (ALL X:ELEMENT EL (X A) IMPL EL (X B)))
(           * LEMMA 3.10.ME.R.GLEICH *)
(           ALL R1,R2,F:MENGE  ALL MAL:VERKN  ALL X:ELEMENT  
                 RIDEAL (R1 F MAL)
                 AND  
                 RIDEAL (R2 F MAL)
                 AND  
                 MINIMAL (R1 F MAL)
                 AND  
                 MINIMAL (R2 F MAL)
                 AND  
                 EL (X R1)
                 AND  
                 EL (X R2)
                 AND  
                 HG (F MAL)
               IMPL  
               R1 = R2)

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
                 VORAUSSETZUNG3102MER (F MAL I1 I2 J1 J2 X)
              IMPL  
              (EX R:ELEMENT EL (R U (J1)) AND X = APPLYV (MAL R X)))
