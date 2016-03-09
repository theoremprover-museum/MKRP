;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* E67B)
(*)
(* PORTIA^S SUITOR CHOSE CORRECTLY, SO THEY MARRIED AND LIVED QUITE HAPPILY -)
(* AT LEAST FOR A WHILE. THEN ONE DAY, PORTIA HAD THE FOLLOWING THOUGHTS:)
(* THOUGH MY HUSBAND SHOWED SOME INTELLIGENCE IN CHOOSING THE RIGHT CASKET,)
(* THE PROBLEM WASN^T REALLY THAT DIFFICULT. SURELY I COULD HAVE MADE THE)
(* PROBLEM HARDER AND GOTTEN A REALLY CLEVER HUSBAND.)
(* SO SHE FORTHWITH DIVORCED HER HUSBAND AND DECIDED TO GET A CLEVERER ONE.)
(* THIS TIME SHE HAD THE FOLLOWING INSCRIPTIONS PUT ON THE CASKETS:)
(*)
(* GOLD: THE PORTRAIT
IS NOT IN THE SILVER CASKET)
(* SILVER: THE PORTRAIT IS NOT IN THIS CASKET)
(* LEAD: THE PORTRAIT IS IN THIS CASKET)
(*)
(* PORTIA EXPLAINED TO THE SUITOR THAT AT LEAST ONE
OF THE THREE STATEMENTS)
(* WAS TRUE AND AT LEAST ONE OF THEM WAS FALSE.)
(* WHICH CASKET CONTAINS THE PORTRAIT?)
(*)
(* AXIOMS FOR THE 2ND PROBLEM OF PORTIA I. (E67B))
(*)
(* THE SAME FUNCTION, PREDICATES AND SORTS AS IN E67A)
(* DECLARATIONS:)
(*)
(TYPE GOLD,SILVER,LEAD:CASKET)
(TYPE INSCRIPTION (CASKET) :INSCRIPT)
(TYPE PORTRAIT (CASKET))
(TYPE TRUTH (INSCRIPT))
(TYPE INSCR.PORTRAIT (INSCRIPT CASKET))
(TYPE NEG.INSCR.PORTRAIT (INSCRIPT CASKET))
(*)
(*
THE SAME GENERAL KNOWLEDGE IS USED AS IN E67A)
(*)
(PORTRAIT (GOLD) OR PORTRAIT (SILVER) OR PORTRAIT (LEAD))
(NOT (PORTRAIT (GOLD) AND
PORTRAIT (SILVER) OR PORTRAIT (SILVER) AND PORTRAIT (LEAD) OR PORTRAIT (LEAD) AND PORTRAIT (GOLD)))
(ALL X:INSCRIPT ALL Y:CASKET INSCR.PORTRAIT (X Y) AND TRUTH (X) IMPL PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET INSCR.PORTRAIT (X Y) AND NOT TRUTH
(X) IMPL NOT PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEG.INSCR.PORTRAIT (X Y) AND TRUTH (X) IMPL NOT PORTRAIT (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEG.INSCR.PORTRAIT (X Y) AND NOT TRUTH (X) IMPL PORTRAIT (Y))
(*)
(* IN ADDITION TO THIS KNOWLEDGE THERE IS INFORMATION SPECIAL TO)
(* THIS PROBLEM:)
(* ACTUAL INSCRIPTIONS ON THE CASKETS :)
(
NEG.INSCR.PORTRAIT (INSCRIPTION (GOLD) SILVER))
(NEG.INSCR.PORTRAIT (INSCRIPTION (SILVER) SILVER))
(INSCR.PORTRAIT (INSCRIPTION (LEAD) LEAD))
(*)
(* AT LEAST ONE OF THE STATEMENTS IS TRUE, AND AT LEAST ONE IS FALSE:)
(TRUTH (INSCRIPTION (GOLD)) OR TRUTH (INSCRIPTION (SILVER)) OR TRUTH (INSCRIPTION (LEAD)))
(NOT TRUTH (INSCRIPTION (GOLD)) OR NOT TRUTH (INSCRIPTION (SILVER)) OR NOT TRUTH (INSCRIPTION (LEAD)))
(*)
(* WHICH CASKET CONTAINS THE PORTRAIT ?)

(* THEOREMS FOR THE 2ND PORTIA PROBLEM (SM.E67B))
(*)
(PORTRAIT (GOLD))