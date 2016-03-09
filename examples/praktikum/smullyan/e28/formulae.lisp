;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS FOR PROBLEM 28 %.)
(* TWO PEOPLE, A AND
B %.)
(* A SAYS: AT LEAST ONE OF US IS A KNAVE.)
(*)
(* WHAT
ARE A AND B.)
(*)
(* ATLEAST (X Y) MEANS: X SAYS: AT LEAST ONE OF US IS A KNAVE.)
(*)
(ALL X,Y ATLEAST (X Y) AND KNIGHT (X) IMPL KNAVE (X) OR KNAVE (Y))
(ALL X,Y NOT (ATLEAST (X Y) AND KNAVE (X)))
(ALL X KNIGHT (X) EQV NOT KNAVE (X))
(ATLEAST (A B))

(KNIGHT (A) AND KNIGHT (B) AND KNAVE (A) AND KNAVE (B))