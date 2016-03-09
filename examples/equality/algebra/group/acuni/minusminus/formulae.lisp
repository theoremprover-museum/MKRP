;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)


(* Commutative Group *)

(ac (+))
(all x +(-(x ) x) = 0)
(all x +(0 x) = x)



(* theorem *)

(all x -(-(x)) = x)