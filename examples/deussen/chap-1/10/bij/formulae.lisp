;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(SORT ELEMENT,MENGE,VERKN,ABB:ANY)
(ALL A,B:MENGE A = B IMPL GLEICHMAECHTIG (A B))
(ALL CHI:ABB  ALL A,B:MENGE  
     INJEKTIV (CHI A B)
     AND  
     GLEICHMAECHTIG (A B)
     AND  
     ENDLICH (A)
     AND  
     ENDLICH (B)
     IMPL  
     BIJEKTIV (CHI A B))

(ALL CHI:ABB  ALL U:MENGE  
     INJEKTIV (CHI U U) AND ENDLICH (U) IMPL BIJEKTIV (CHI U U))