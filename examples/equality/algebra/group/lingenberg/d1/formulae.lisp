;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)
(all x +(x -(x)) = 0)

(* Theorem *)
(all x +(x 0) = x)