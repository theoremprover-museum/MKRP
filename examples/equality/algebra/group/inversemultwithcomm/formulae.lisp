;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(all x,y +(x y) = +(y x))

(* Theorem *)

(all x,y +(-(x) -(y)) = -(+(x y)))