;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
(ALL X,Y,Z P (X Y) AND P (Y Z) IMPL P (X Z))
(ALL X,Y,Z Q (X Y) AND Q (Y Z) IMPL Q (X Z))
(ALL X,Y Q (X Y) IMPL Q (Y X))
(ALL X,Y P (X Y) OR Q (X Y))

(* Theorem)

((ALL X,Y P (X Y)) OR (ALL X,Y Q (X Y)))