;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS FOR PROBLEM 33 %.)
(* THERE ARE TWO PERSONS A AND B)
(* A SAYS : I AM A KNAVE ,BUT B IS NOT.)
(* WHAT ARE A AND B ?)
(*)
(* BUT (X) MEANS : X SAYS : I AM A KNAVE ,BUT B IS NOT.)
(*)
(ALL X KNIGHT (X) EQV NOT KNAVE (X))
(ALL x (KNave (X) AND BUT (X)))

(KNave (A) AND KNave (B))


