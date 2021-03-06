;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PORTIA AND HER HUSBAND DID,AS A MATTER OF FACT, LIVE HAPPILY)
(* EVER AFTER. THEY HAD A DAUGHTER PORTIA II - HENCEFORTH TO BE)
(* CALLED PORTIA. WHEN THE YOUNG PORTIA GREW TO YOUNG WOMANHOOD)
(* SHE WAS BOTH CLEVER AND BEAUTIFUL, JUST LIKE HER MOMMY. SHE)
(* ALSO DECIDED TO SELECT HER HUSBAND BY THE CASKET METHOD. THE)
(* SUITOR HAD TO PASS TWO TESTS IN ORDER TO WIN HER.)
(*)
(* 68A. THE FIRST TEST.)
(*)
(* IN THIS TEST EACH LID CONTAINED TWO STATEMENTS, AND PORTIA EX-)
(* PLAINED THAT NO LID CONTAINED MORE THAN ONE FALSE STATEMENT.)
(*)
(* GOLD: (1) THE PORTRAIT IS NOT IN HERE.)
(* (2) THE ARTIST OF THE PORTRAIT IS FROM VENICE)
(* SILVER: (1) THE PORTRAIT IS NOT IN THE GOLD CASKET)
(* (2) THE
ARTIST OF THE PORTRAIT IS REALLY FROM FLORENCE)
(* LEAD: (1) THE PORTRAIT IS NOT IN HERE)
(* (2) THE PORTRAIT IS REALLY IN THE SILVER CASKET)
(*)
(* WHICH CASKET CONTAINS
THE PORTRAIT?)
(*)
(* WE DEFINE THE FOLLOWING PREDICATES:)
(* THE PREDICATES PORTRAIT,TRUTH,INSRIPTION,NEGATIVE.INSCRIPTION)
(* MEAN THE SAME AS IN 66 AND 67)
(TYPE PORTRAIT (CASKET))
(TYPE TRUTH (INSCRIPT))
(TYPE INSCRIPTION (INSCRIPT CASKET))
(TYPE NEGATIVE.INSCRIPTION (INSCRIPT CASKET))
(* V---THE ARTIST OF THE PORTRAIT IS FROM
VENICE)
(* F---THE ARTIST OF THE PORTRAIT IS FROM FLORENCE)
(* INSCRIPTION.VENICE (X) --- THE INSCRIPTION X TELLS THAT THE ARTIST OF THE PORTRAIT)
(* IS FROM VENICE.)
(* 
INSCRIPTION.FLORENCE (X) --- THE INSCRIPTION X TELLS THAT THE ARTIST OF THE PORTRAIT)
(* IS FROM FLORENCE)
(TYPE INSCRIPTION.VENICE (INSCRIPT))
(TYPE INSCRIPTION.FLORENCE (INSCRIPT))
(*)
(* WE DEFINE THE FOLLOWING FUNCTIONS:)
(* FIRST (X) ---ASSIGNS EACH CASKET AN INSCRIPTION)
(* SECOND (X) ---ASSIGNS EACH CASKET AN INSCRIPTION)
(TYPE 
GOLD,SILVER,LEAD:CASKET)
(TYPE FIRST (CASKET) :INSCRIPT)
(TYPE SECOND (CASKET) :INSCRIPT)
(*)
(* THEN THE FOLLOWING PROPOSITIONS HOLD.)
(*)
(* THERE IS EXACTLY ONE PORTRAIT:)
(PORTRAIT (GOLD) OR PORTRAIT (SILVER) OR PORTRAIT (LEAD))
(NOT (PORTRAIT (GOLD) AND PORTRAIT (SILVER) OR PORTRAIT
(GOLD) AND PORTRAIT (LEAD) OR PORTRAIT (SILVER) AND PORTRAIT (LEAD)))
(*)
(* NO MORE THAN ONE INSCRIPTION IS FALSE ON ANY CASKET:)
(ALL X:CASKET TRUTH (FIRST (X)) OR TRUTH (SECOND (X)))
(*)
(* THE ARTIST
OF THE PORTRAIT IS EITHER FROM VENICE OR FROM FLORENCE)
(NOT (VENICE AND FLORENCE))
(*)
(* IMPLICATIONS TO BE DRAWN FROM INSCRIPTION:)
(ALL X:INSCRIPT ALL Y:CASKET INSCRIPTION (X Y) AND TRUTH (X) IMPL PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET INSCRIPTION (X Y) AND NOT
TRUTH (X) IMPL NOT PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEGATIVE.INSCRIPTION (X Y) AND TRUTH (X) IMPL NOT PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEGATIVE.INSCRIPTION (X Y) AND NOT TRUTH (X) IMPL PORTRAIT (Y))
(ALL X:INSCRIPT INSCRIPTION.VENICE (X) AND TRUTH (X) IMPL VENICE)
(ALL X:INSCRIPT
INSCRIPTION.VENICE (X) AND NOT TRUTH (X) IMPL NOT VENICE)
(ALL X:INSCRIPT INSCRIPTION.FLORENCE (X) AND TRUTH (X) IMPL FLORENCE)
(ALL X:INSCRIPT INSCRIPTION.FLORENCE (X) AND NOT TRUTH (X) IMPL NOT FLORENCE)
(NEGATIVE.INSCRIPTION (FIRST (GOLD) GOLD) AND INSCRIPTION.VENICE (SECOND (GOLD)))
(NEGATIVE.INSCRIPTION (FIRST (SILVER) GOLD) AND INSCRIPTION.FLORENCE
(SECOND (SILVER)))
(NEGATIVE.INSCRIPTION (FIRST (LEAD) LEAD) AND INSCRIPTION (SECOND (LEAD) SILVER))

(* THEOREMS FOR THE THIRD PORTIA PROBLEM (SM.E68A))
(*)
(PORTRAIT (SILVER))