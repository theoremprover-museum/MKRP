;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* 68B. SECOND TEST.)
(* IF THE SUITOR PASSED THE FIRST TEST, HE WAS TAKEN INTO ANOTHER)
(* ROOM IN WHICH THERE WERE THREE MORE CASKETS. AGAIN EACH CASKET)
(* HAD TWO SENTENCES INSCRIBED
ON THE LID. PORTIA EXPLAINED THAT)
(* ON ONE OF THE LIDS, BOTH STATEMENTS WERE TRUE; ON ANOTHER,) COMMENT COMMENT) ((* BOTH STATEMENTS WERE FALSE; AND ON THE THIRD, ONE STATEMENT) COMMENT COMMENT) ((* WAS TRUE AND ONE WAS FALSE.)
COMMENT COMMENT)
(* (2) IT IS IN THE SILVER CASKET)
(* SILVER: (1) THE PORTRAIT IS NOT IN THE GOLD CASKET)
(* (2) IT
IS IN THE LEAD CASKET)
(* LEAD: (1) THE PORTRAIT IS NOT IN THIS CASKET)
(* (2) IT IS IN THE GOLD CASKET)
(*)
(*)
(* WE DEFINE THE FOLLOWING FUNCTIONS:)
(* FIRST (X) SECOND (X) , WHICH MAP AN ELEMENT OF THE SORT CASKET)
(* TO AN ELEMENT OF THE SORT INSCRIPT.)
(*)
(*)
(* WE DEFINE THE FOLLOWING PREDICATES:)
(*)
(* THE PREDICATES PORTRAIT, TRUTH, NEGATIVE.INSCRIPTION AND)
(* INSCRIPTION MEAN THE SAME AS IN 66, 67 AND 68A.)
(* BOTH.TRUE (X Y) - BOTH INSCRIPTIONS, X AND Y, ARE TRUE)
(* BOTH.FALSE (X Y) - BOTH INSCRIPTIONS, X AND Y, ARE FALSE)
(* TRUE.FALSE (X Y) - ONE OF THE INSCRIPTIONS, X AND Y, IS TRUE)
(* AND ONE OF THEM IS FALSE)
(*)
(*)
(* DECLARATIONS:)
(*)
(TYPE G,S,L : CASKET)
(TYPE FIRST (CASKET) : INSCRIPT)
(TYPE SECOND (CASKET) : INSCRIPT)
(*)
(*)
(* FORMULAS:)
(*)
(* THE PORTRAIT IS IN EXACTLY ONE CASKET)
(PORTRAIT (GOLD) OR PORTRAIT (SILVER) OR PORTRAIT (LEAD))
(NOT (PORTRAIT (GOLD) AND PORTRAIT (SILVER) OR PORTRAIT (SILVER) AND PORTRAIT (LEAD) OR PORTRAIT (LEAD) AND PORTRAIT (GOLD)))
(*)
(* IMPLICATIONS TO BE DRAWN FROM INSCRIPTIONS:)
(ALL X:INSCRIPT ALL Y:CASKET INSCRIPTION (X Y) AND TRUTH (X) IMPL PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET INSCRIPTION (X Y) AND NOT TRUTH (X) IMPL NOT PORTRAIT (Y))
(ALL X:INSCRIPT
ALL Y:CASKET NEGATIVE.INSCRIPTION (X Y) AND TRUTH (X) IMPL NOT PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEGATIVE.INSCRIPTION (X Y) AND NOT
TRUTH (X) IMPL PORTRAIT (Y))
(*)
(* INFORMATION ABOUT TRUTH OF INSCRIPTIONS:)
(BOTH.TRUE (FIRST (GOLD)
SECOND (GOLD)) OR BOTH.TRUE (FIRST (SILVER) SECOND (SILVER)) OR BOTH.TRUE (FIRST (LEAD) SECOND (LEAD)))
(BOTH.FALSE (FIRST (GOLD) SECOND (GOLD))
OR BOTH.FALSE (FIRST (SILVER) SECOND (SILVER)) OR BOTH.FALSE (FIRST (LEAD) SECOND (LEAD)))
(TRUE.FALSE (FIRST (GOLD) SECOND (GOLD)) OR TRUE.FALSE
(FIRST (SILVER) SECOND (SILVER)) OR TRUE.FALSE (FIRST (LEAD) SECOND (LEAD)))
(ALL X,Y:INSCRIPT BOTH.TRUE (X Y) EQV TRUTH (X) AND TRUTH (Y))
(ALL X,Y:INSCRIPT BOTH.FALSE (X Y) EQV NOT TRUTH (X) AND NOT TRUTH (Y))
(ALL X,Y:INSCRIPT TRUE.FALSE (X Y) EQV NOT BOTH.TRUE (X Y) AND NOT BOTH.FALSE (X Y))
(*)
(* ACTUAL INSCRIPTIONS:)
(NEGATIVE.INSCRIPTION (FIRST (GOLD) GOLD))
(INSCRIPTION (SECOND (GOLD) SILVER))
(NEGATIVE.INSCRIPTION (FIRST (SILVER) GOLD))
(INSCRIPTION (SECOND (SILVER) LEAD))
(NEGATIVE.INSCRIPTION (FIRST (LEAD) LEAD))
(INSCRIPTION (SECOND (LEAD) GOLD))

(PORTRAIT(LEAD))