;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-)
(* ************************************************)
(* DEFINITIONEN DER MENGENLEHRE)
(SORT SET:ANY)
(SORT ELEMENT:SET)
(TYPE EL (ELEMENT SET))
(* DEFINITION 1.1: TEILMENGE)
(TYPE SUBSET (SET SET))
(ALL X,Y:SET SUBSET (X Y) EQV (ALL A:ELEMENT EL (A X) IMPL EL (A Y)))
(* DEFINITION 1.3: KARTESISCHES PRODUKT)
(TYPE CARTES (SET SET) :SET)
(TYPE PAIR (ELEMENT ELEMENT) :ELEMENT)
(* DEFINITION 1.7: FUNKTION)
(SORT SYMB.OF.FUNCTION:ANY)
(TYPE MAPPING (SYMB.OF.FUNCTION SET SET))
(TYPE APPLY (SYMB.OF.FUNCTION ELEMENT) :ELEMENT)
(ALL F:SYMB.OF.FUNCTION ALL X,Y:SET MAPPING (F X Y) EQV (ALL A:ELEMENT EL (A X) IMPL EL (APPLY (F A) Y)))
(* SORTENPROBLEM!!!!!!!)
(TYPE MAPPING.S (SYMB.OF.FUNCTION SET SET))
(TYPE S:SET)
(SORT EL.OF.S:ELEMENT)
(ALL F:SYMB.OF.FUNCTION ALL Y:SET MAPPING.S (F S Y) EQV (ALL A:EL.OF.S EL (APPLY (F A) Y)))
(* ************************************************************************)
(* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S)
(SORT REL:SET)
(* DEFINITION 2.8: AEQUIVALENZRELATION)
(SORT EQU.RELATION:REL)
(* DEFINITION 2.13: DIE VON EINER ABBILDUNG PHI VON S NACH U INDUZIERTE AEQUIVALENZRELATION)
(TYPE IND.EQU (SYMB.OF.FUNCTION SET) :EQU.RELATION)
(ALL PHI:SYMB.OF.FUNCTION
     (ALL U:SET 
	  MAPPING.S (PHI S U)
	  IMPL 
	  (ALL A:ELEMENT 
	       EL (A IND.EQU (PHI U)) EQV (EX B,C:EL.OF.S A = PAIR (B C) AND APPLY (PHI B) = APPLY (PHI C)))))

(ALL U,V:SET ALL PHIeins,PHIzwei:SYMB.OF.FUNCTION 
     MAPPING.S (PHIeins S U) AND MAPPING.S (PHIzwei S U)
     IMPL 
     (ALL GR.PHI:SYMB.OF.FUNCTION 
	  MAPPING (GR.PHI U V) AND (ALL T:EL.OF.S APPLY (GR.PHI APPLY (PHIeins T)) = APPLY (PHIzwei T))
	  IMPL 
	  SUBSET (IND.EQU (PHIeins U) IND.EQU (PHIzwei V))))