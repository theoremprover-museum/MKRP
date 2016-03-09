;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(* Axioms *)

(ALL X,Y,Z PLUS (PLUS (X Y) Z) = PLUS (X PLUS (Y Z)))
;(ALL X PLUS (X 0) = X)
(ALL X PLUS (0 X) = X)
(ALL X PLUS (minus (X) X) = 0)
;(ALL X PLUS (X minus (X)) = 0)



(* Theorem *)

((ALL X,Y,Z X = Y OR z = y OR X = Z) IMPL (ALL U minus (U) = u))