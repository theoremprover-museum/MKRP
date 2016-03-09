;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)
(all x +(x 0) = x)
(all x +(x -(x)) = 0)
(all x,z +(-(x) +(x z)) = z)
(all x,z +(x +(-(x) z)) = z)
(all x -(-(x)) = x)
(-(0) = 0)
(all x,y -(+(x y)) = +(-(y) -(x)))

(* Theorem *)

(all x +(-(+(+(+(+(-(x) -(x)) -(x)) -(x)) -(x))) -(+(+(+(+(x x) x) x) x))) = 0)