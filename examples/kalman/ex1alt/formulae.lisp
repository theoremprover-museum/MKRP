;;; -*- Mode: LISP; Package: MARKGRAF-KARL; Syntax: Common-Lisp -*-
(* Axioms *)
(ALL X,Y NOT P (X) OR NOT P (F (X Y)) OR P (Y))
(ALL X,Y,Z P (F (F (X Y) F (F (X Z) F (Y Z)))))

(* Theorem *)
(ALL X P (F (X X)))