;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* AXIOMS *)
(EX Z,W (ALL X,Y F (X Y) EQV (X = Z AND Y = W)))
             
(* THEOREMS *)
(EX W (ALL Y (EX Z (ALL X F (X Y) EQV X = Z)) EQV Y = W))
             