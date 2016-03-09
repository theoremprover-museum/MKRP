;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-
(SORT MENGE,ELEMENT:ANY)
(SORT REL:MENGE)
(SORT EL.VON.S:ELEMENT)
(TYPE EL (ELEMENT MENGE))
(TYPE PAAR (ELEMENT ELEMENT) :ELEMENT)
(* DEFINITION VON DURCHSCHNITT)
(TYPE DURCHSCHNITT (REL REL) :REL)
(ALL X,Y:REL ALL A:ELEMENT EL (A DURCHSCHNITT (X Y)) EQV EL (A X) AND EL (A Y))
(* DEFINITION REFLEXIV)
(TYPE REFLEXIV (REL))
(ALL RHO:REL REFLEXIV (RHO) EQV (ALL A:EL.VON.S EL (PAAR (A A) RHO)))
(* DEFINITION SYMMETRISCH)
(TYPE SYMMETRISCH (REL))
(ALL RHO:REL SYMMETRISCH (RHO) EQV (ALL A,B:EL.VON.S EL (PAAR (A B) RHO) IMPL EL (PAAR (B A) RHO)))
(* DEFINITION TRANSITIV)
(TYPE TRANSITIV (REL))
(ALL RHO:REL  
 TRANSITIV (RHO)
 EQV  
 (ALL A,B,C:EL.VON.S EL (PAAR (A B) RHO) AND EL (PAAR (B C) RHO) IMPL EL (PAAR (A C) RHO)))
(* DEFINITION AEQUIVALENZRELATION)
(TYPE EQU.REL (REL))
(ALL RHO:REL EQU.REL (RHO) EQV REFLEXIV (RHO) AND SYMMETRISCH (RHO) AND TRANSITIV (RHO))

(ALL RHO,SIGMA:REL EQU.REL (RHO) AND EQU.REL (SIGMA) IMPL SYMMETRISCH (DURCHSCHNITT (RHO SIGMA)))

