;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-)
(* ************************************************)
(* DEFINITIONEN DER MENGENLEHRE)
(SORT SET:ANY)
(SORT ELEMENT:SET)
(TYPE EL (ELEMENT SET))
(* DEFINITION 1.1: TEILMENGE)
(TYPE SUBSET (SET SET))
(* DEFINITION 1.3: KARTESISCHES PRODUKT)
(TYPE CARTES (SET SET) :SET)
(TYPE PAIR (ELEMENT ELEMENT) :ELEMENT)
(* DEFINITION 1.7: FUNKTION)
(SORT SYMB.OF.FUNCTION:ANY)
(TYPE MAPPING (SYMB.OF.FUNCTION SET SET))
(TYPE APPLY (SYMB.OF.FUNCTION ELEMENT) :ELEMENT)
(* SORTENPROBLEM!!!!!!!)
(TYPE MAPPING.S (SYMB.OF.FUNCTION SET SET))
(TYPE SURJECTIVE.S (SYMB.OF.FUNCTION SET SET))
(TYPE S:SET)
(SORT EL.OF.S:ELEMENT)
(* ************************************************************************)
(* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S)
(SORT REL:SET)
(* DEFINITION 2.8: AEQUIVALENZRELATION)
(SORT EQU.RELATION:REL)
(* DEFINITION 2.13: DIE VON EINER ABBILDUNG PHI VON S NACH U INDUZIERTE AEQUIVALENZRELATION)
(TYPE IND.EQU (SYMB.OF.FUNCTION SET) :EQU.RELATION)
(* DEFINITION 2.15)
(TYPE MODULO (EQU.RELATION) :SET)
(* DEFINITION 2.16 KANONISCHE PROJEKTION S AUF SRHO)
(TYPE CAN.PROJ (EQU.RELATION) :SYMB.OF.FUNCTION)
(TYPE PREIMAGE (REL EL.OF.S) :ELEMENT)
(ALL RHO:EQU.RELATION MAPPING.S (CAN.PROJ (RHO) S MODULO (RHO)))
(* SATZ 4-11-1)
(ALL U,V:SET ALL PHI1,PHI2:SYMB.OF.FUNCTION 
     MAPPING.S (PHI1 S U) AND MAPPING.S (PHI2 S V)
     IMPL 
     (ALL GR.PHI:SYMB.OF.FUNCTION 
     MAPPING (GR.PHI U V) AND (ALL T:EL.OF.S APPLY (GR.PHI APPLY (PHI1 T)) = APPLY (PHI2 T)))
     IMPL 
     SUBSET (IND.EQU (PHI1 U) IND.EQU (PHI2 V)))
(TYPE CAN.PROJ (EQU.RELATION) :SYMB.OF.FUNCTION)
(* DIREKTE FOLGERUNG AUS DER DEFINITION DERSURJEKTIVITAET)
(ALL PHI:SYMB.OF.FUNCTION ALL U,V:SET SURJECTIVE (PHI U V) IMPL MAPPING (PHI U V))
(* SATZ 4-12-1-1)
(ALL RHO:EQU.RELATION IND.EQU (CAN.PROJ (RHO) MODULO (RHO)) = RHO)

(ALL RHO1,RHO2:EQU.RELATION ALL GR.PHI:SYMB.OF.FUNCTION 
     SURJECTIVE (GR.PHI MODULO (RHO1) MODULO (RHO2))
     AND 
     (ALL A:EL.OF.S APPLY (GR.PHI APPLY (CAN.PROJ (RHO1) A)) = APPLY (CAN.PROJ (RHO2) A))
     IMPL 
     SUBSET (RHO1 RHO2))
