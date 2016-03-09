;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* AXIOMS *)
(* GROUP *)
(ALL X,Y,Z PLUS (PLUS (X Y) Z) = PLUS (X PLUS (Y Z)))

(ALL X PLUS (0 X) = X)
(ALL X PLUS (MINUS (X) X) = 0)


(* Theorem *)

(ALL X,Y PLUS (X Y) = PLUS (Y X)
 EQV (ALL X,Y PLUS (PLUS (X Y) PLUS (X Y)) = PLUS (PLUS (X X) PLUS (y y)))
 EQV (ALL X,Y MINUS (PLUS (X Y)) = PLUS (MINUS (X) MINUS (Y))))