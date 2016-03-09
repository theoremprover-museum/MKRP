;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x ) x) = 0)

(* member *)

(all x,y s(x) and s(y) impl s(+(x -(y))))

(* theorem *)

(all x s(x) impl s(-(x)))