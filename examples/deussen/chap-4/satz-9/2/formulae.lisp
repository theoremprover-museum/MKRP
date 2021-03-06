;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-)
(* ************************************************)
(* DEFINITIONEN DER MENGENLEHRE)
(SORT SET:ANY)
(SORT ELEMENT:SET)
(TYPE EL (ELEMENT SET))
(* DEFINITION 1.3: KARTESISCHES PRODUKT)
(TYPE CARTES (SET SET) :SET)
(TYPE PAIR (ELEMENT ELEMENT) :ELEMENT)
(* SORTENPROBLEM)
(SORT EL.OF.S:ELEMENT)
(TYPE S:SET)
(ALL A,B:EL.OF.S EL (PAIR (A B) CARTES (S S)))
(* DEFINITION 1.7: FUNKTION)
(SORT SYMB.OF.FUNCTION:ANY)
(TYPE MAPPING (SYMB.OF.FUNCTION SET SET))
(TYPE APPLY (SYMB.OF.FUNCTION ELEMENT) :ELEMENT)
(ALL F:SYMB.OF.FUNCTION ALL X,Y:SET MAPPING (F X Y) EQV (ALL A:ELEMENT EL (A X) IMPL EL (APPLY (F A) Y)))
(* ************************************************************************)
(* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S)
(SORT REL:SET)
(* DEFINITION 2.5: REFLEXIV)
(TYPE REFLEXIV (REL))
(* LEMMA 2,5)
(ALL RHO:REL (ALL A:EL.OF.S EL (PAIR (A A) RHO)) IMPL REFLEXIV (RHO))
(* DEFINITION 2.6: SYMMETRISCH)
(TYPE SYMMETRISCH (REL))
(* LEMMA 2,6)
(ALL RHO:REL (ALL A,B:EL.OF.S EL (PAIR (A B) RHO) IMPL EL (PAIR (B A) RHO)) IMPL SYMMETRISCH (RHO))
(* DEFINITION 2.7: TRANSITIV)
(TYPE TRANSITIV (REL))
(* LEMMA 2,7)
(ALL RHO:REL 
     (ALL A,B,C:EL.OF.S EL (PAIR (A B) RHO) AND EL (PAIR (B C) RHO) IMPL EL (PAIR (A C) RHO))
     IMPL 
     TRANSITIV (RHO))
(* DEFINITION 2.8: AEQUIVALENZRELATION)
(TYPE EQU.REL (REL))
(ALL RHO:REL REFLEXIV (RHO) AND SYMMETRISCH (RHO) AND TRANSITIV (RHO) IMPL EQU.REL (RHO))
(SORT EQU.RELATION:REL)
(* DEFINITION 2.13: DIE VON EINER ABBILDUNG PHI VON S NACH U INDUZIERTE AEQUIVALENZRELATION)
(TYPE IND.EQU (SYMB.OF.FUNCTION SET) :REL)
(ALL PHI:SYMB.OF.FUNCTION ALL U:SET 
     MAPPING (PHI S U) IMPL (ALL A,B:EL.OF.S EL (PAIR (A B) IND.EQU (PHI U)) EQV APPLY (PHI A) = APPLY (PHI B)))
(ALL PHI:SYMB.OF.FUNCTION ALL U:SET NOT MAPPING (PHI S U) IMPL IND.EQU (PHI U) = CARTES (S S))

(ALL PHI:SYMB.OF.FUNCTION ALL U:SET NOT MAPPING (PHI S U) IMPL EQU.REL (IND.EQU (PHI U)))
