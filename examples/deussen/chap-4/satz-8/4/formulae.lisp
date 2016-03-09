;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-


(* ************************************************)
(* DEFINITIONEN DER MENGENLEHRE)
(SORT SET:ANY)
(SORT ELEMENT:SET)
(TYPE EL (ELEMENT SET))
(SORT REL:SET)
(SORT EL.OF.S:ELEMENT)
(TYPE PAIR (ELEMENT ELEMENT) :ELEMENT)
(* DEFINITION 1.1: TEILMENGE)
(TYPE SUBSET.REL (REL REL))
(ALL X,Y:REL SUBSET.REL (X Y) EQV (ALL A,B:EL.OF.S EL (PAIR (A B) X) IMPL EL (PAIR (A B) Y)))
(* DEFINITION 1.3: KARTESISCHES PRODUKT)
(TYPE S:SET)
(* DEFINITION 1.4: VEREINIGUNG)
(TYPE UNION (REL REL) :REL)
(ALL X,Y:REL ALL A,B:EL.OF.S EL (PAIR (A B) X) OR EL (PAIR (A B) Y) IMPL EL (PAIR (A B) UNION (X Y)))
(* DEFINITION 1.6: NATUERLICHE ZAHLEN)
(SORT NAT:SET)
(TYPE 1:NAT)
(TYPE PLUS (NAT NAT) :NAT)
(* ************************************************************************)
(* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S)
(* DEFINITION 2.3: IDENTISCHE RELATION)
(TYPE IDENTITY:REL)
(* DEFINITION 2.9: POTENZ EINER RELATION)
(TYPE POWER (REL NAT) :REL)
(ALL RHO:REL POWER (RHO 1.) = RHO)
(* DEFINITION 2.11:TRANSITIVER ABSCHLUSS)
(TYPE TRANS.CLOS (REL) :REL)
(ALL RHO:REL ALL A:ELEMENT (EX N:NAT EL (A POWER (RHO N))) IMPL EL (A TRANS.CLOS (RHO)))
(TYPE TRANS.UNION (REL REL) : REL)
(ALL RHO,SIGMA:REL TRANS.UNION (RHO SIGMA) = TRANS.CLOS (UNION (RHO SIGMA)))

(ALL RHO,SIGMA:REL SUBSET.REL (RHO TRANS.UNION (RHO SIGMA)))
