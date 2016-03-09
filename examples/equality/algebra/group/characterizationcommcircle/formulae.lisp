;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* AXIOMS *)
(* GROUP *)
(ALL X,Y,Z PLUS (PLUS (X Y) Z) = PLUS (X PLUS (Y Z)))
(ALL X PLUS (0 X) = X)
(ALL X PLUS (MINUS (X) X) = 0)


(* Theorem *)

(((ALL X,Y PLUS (X Y) = PLUS (Y X))
  impl (ALL X,Y PLUS (PLUS (X Y) PLUS (X Y)) = PLUS (PLUS (X X) PLUS (y y))))
 and
 ((ALL X,Y PLUS (PLUS (X Y) PLUS (X Y)) = PLUS (PLUS (X X) PLUS (y y)))
  impl (ALL X,Y MINUS (PLUS (X Y)) = PLUS (MINUS (X) MINUS (Y))))
 and
 ((ALL X,Y MINUS (PLUS (X Y)) = PLUS (MINUS (X) MINUS (Y)))
  impl (ALL X,Y PLUS (X Y) = PLUS (Y X))))