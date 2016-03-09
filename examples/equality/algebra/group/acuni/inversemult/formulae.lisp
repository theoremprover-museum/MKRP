;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Abelian Group *)

(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(ac(+))

(* Theorem *)

(all x,y +(-(x) -(y)) = -(+(x y)))