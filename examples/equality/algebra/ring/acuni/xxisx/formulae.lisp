;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)



(* Abelian Group *)

(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(ac(+))

(* * *)

(all x,y,z *(*(x y) z) = *(x *(y z)))

(* * group connection *)

(all x,y,z *(+(x y) z) = +(*(x z) *(y z)))
(all x,y,z *(z +(x y)) = +(*(z x) *(z y)))


(* additional axiom and theorem *)

(all x *(x x) = x)

(all x,y *(x y) = *(y x))