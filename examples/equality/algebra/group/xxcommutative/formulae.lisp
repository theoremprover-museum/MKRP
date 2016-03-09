;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
;(associative (+))
;(all x,y +(x y) = +(y x))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(* NIL potent *)

(all x +(x x) = 0)



(* theorem *)

(all x,y +(x y) = +(y x))