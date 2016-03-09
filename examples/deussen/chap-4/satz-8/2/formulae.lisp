;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP; Base: 10 -*-
(SORT SET:ANY)
(* SORTENPROBLEM!!!)
(* DEFINITION 1.4: VEREINIGUNG)
(TYPE UNION (REL REL) :REL)
(SORT REL:SET)
(SORT EQU.RELATION:REL)
(* DEFINITION 2.12: ERZEUGTE AEQUIVALENZRELATION)
(TYPE GEN.EQU.REL (REL) :EQU.RELATION)
(ALL RHO,SIGMA:REL TRANS.UNION (RHO SIGMA) = GEN.EQU.REL (UNION (RHO SIGMA)))

(ALL RHO,SIGMA:EQU.RELATION EX TAU:EQU.RELATION TAU = TRANS.UNION (RHO SIGMA))