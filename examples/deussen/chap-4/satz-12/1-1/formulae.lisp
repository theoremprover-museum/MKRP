;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-
(* ************************************************)
(* DEFINITIONEN DER MENGENLEHRE)
(SORT SET:ANY)
(SORT ELEMENT:SET)
(TYPE EL (ELEMENT SET))
(* DEFINITION 1.2: MENGENGLEICHHEIT)
(TYPE SET.EQUAL (SET SET))
(* ALL X,Y:SET (ALL A:ELEMENT EL (A X) EQV EL (A Y)) IMPL SET.EQUAL (X Y))
(* DEFINITION TEILMENGE)
(TYPE SUBSET (SET SET))
(ALL X,Y:SET (ALL A:ELEMENT EL (A X) IMPL EL (A Y)) IMPL SUBSET (X Y))
(* DEFINITION 1.3: KARTESISCHES PRODUKT)
(TYPE CARTES (SET SET) :SET)
(TYPE PAIR (ELEMENT ELEMENT) :ELEMENT)
(* DEFINITION 1.7: FUNKTION)
(SORT SYMB.OF.FUNCTION:ANY)
(TYPE MAPPING (SYMB.OF.FUNCTION SET SET))
(TYPE APPLY (SYMB.OF.FUNCTION ELEMENT) :ELEMENT)
(* ************************************************************************)
(* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S)
(SORT REL:SET)
(TYPE S:SET)
(SORT EL.OF.S:ELEMENT)
(* DEFINITION 2.8: AEQUIVALENZRELATION)
(SORT EQU.RELATION:REL)
(* DEFINITION 2.13: DIE VON EINER ABBILDUNG PHI VON S NACH U INDUZIERTE AEQUIVALENZRELATION)
(TYPE IND.EQU (SYMB.OF.FUNCTION SET) :EQU.RELATION)
(ALL PHI:SYMB.OF.FUNCTION ALL U:SET 
     MAPPING (PHI S U)
     IMPL 
     ALL A:ELEMENT 
     EL (A IND.EQU (PHI U)) EQV (EX B,C:EL.OF.S A = PAIR (B C) AND APPLY (PHI B) = APPLY (PHI C)))
(* DEFINITION 2.14: VORBEREICH EINER RELATION UND EINES ELEMENTES)
(TYPE PREIMAGE (REL EL.OF.S) :SET)
(* DEFINITION 2.15: QUOTIENTENMENGE S MODULO RHO)
(TYPE MODULO (EQU.RELATION) :SET)
(* DEFINITION 2.16: KANONISCHE PROJEKTION VON S AUF (S MODULO RHO))
(TYPE CAN.PROJ (EQU.RELATION) :SYMB.OF.FUNCTION)
(ALL RHO:EQU.RELATION )
(MAPPING (CAN.PROJ (RHO) S MODULO (RHO)) AND (ALL A:EL.OF.S APPLY (CAN.PROJ (RHO) A) = PREIMAGE (RHO A)))
(* LEMMA2.14)
(ALL RHO:EQU.RELATION ALL A,B:EL.OF.S EL (PAIR (A B) RHO) EQV PREIMAGE (RHO A) = PREIMAGE (RHO B))

(* SATZ 4-12-1-1)
(ALL RHO:EQU.RELATION SUBSET (IND.EQU (CAN.PROJ (RHO) MODULO (RHO)) RHO))
