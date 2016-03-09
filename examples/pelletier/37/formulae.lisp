;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all z (ex w (all x (ex y (p(x z) impl p(y w)) and p(y z) and (p(y w) impl (ex u q(u w)))))))
(all x,z not p(x z) impl (ex y q(y z)))
(ex x,y q(x y) impl (all x r(x x)))

(* Theorems *)

(all x (ex y r(x y)))