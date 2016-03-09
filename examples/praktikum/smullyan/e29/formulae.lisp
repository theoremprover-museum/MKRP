;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS FOR PROBLEM 29 %.)
(*)
(* THERE ARE TWO PERSONS, A AND B)
(* A SAYS: EITHER I AM A KNAVE OR B IS A KNIGHT)
(*)
(* WHAT ARE A AND B)
(*)
(* EITHER (X) MEANS
X SAYS, EITHER I AM A KNAVE OR B IS A KNIGHT)
(*)
(ALL X KNIGHT (X) EQV NOT
KNAVE (X))
(ALL X KNIGHT (X) AND EITHER (X) IMPL KNAVE (X) OR KNIGHT (B))
(ALL X KNAVE (X) AND EITHER (X) IMPL NOT (KNAVE (X) OR KNIGHT
(B)))
(EITHER (A))

(* THEOREMS OF PROBLEM E29 (SMULLYAN))
(*)
(* WE TRY ALL POSSIBLE CASES)
(KNIGHT (A) AND KNAVE (A) AND KNIGHT (B) AND KNAVE (B))