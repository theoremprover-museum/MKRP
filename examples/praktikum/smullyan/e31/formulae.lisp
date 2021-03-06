;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS OF PROBLEM 31 %.)
(* WE HAVE THREE PEOPLE:
A, B, C.)
(* A SAYS: ALL OF US ARE KNAVES.)
(* B SAYS: EXACTLY ONE OF US IS
A KNIGHT.)
(* WHAT ARE A, B, C?)
(*)
(* EVERY (X) MEANS:
X SAYS: WE ARE ALL KNAVES.)
(* EXACT (X) MEANS: X SAYS: EXACTLY ONE OF US IS A KNIGHT.)
(*)
(ALL X KNIGHT (X) EQV NOT KNAVE (X))
(ALL X NOT (KNIGHT (X) AND EVERY (X)))
(ALL X KNAVE (X) AND EXACT (X) IMPL NOT ((KNIGHT (A) OR KNIGHT
(B) OR KNIGHT (C)) AND NOT (KNIGHT (A) AND KNIGHT (B)) AND NOT (KNIGHT (A) AND KNIGHT (C)) AND NOT (KNIGHT (B)
AND KNIGHT (C))))
(ALL X (KNAVE (X) AND EVERY (X)) IMPL
NOT (KNAVE (A) AND KNAVE (B) AND KNAVE (C)))
(ALL X KNIGHT (X) AND EXACT (X) IMPL (KNIGHT
(A) OR KNIGHT (B) OR KNIGHT (C)) AND NOT (KNIGHT (A) AND KNIGHT (B)) AND NOT (KNIGHT (A) AND KNIGHT (C)) AND NOT
(KNIGHT (B) AND KNIGHT (C)))
(EVERY (A))
(EXACT (B))


(KNAVE (A) AND NOT KNAVE (B) AND KNAVE (C))


