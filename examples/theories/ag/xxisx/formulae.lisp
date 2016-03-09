;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)


(* Group *)

(ag (+ - 0))


(all x,y,z *(*(x y) z) = *(x *(y z)))

(* * group connection *)

(all x,y,z *(+(x y) z) = +(*(x z) *(y z)))
(all x,y,z *(z +(x y)) = +(*(z x) *(z y)))


(* additional axiom and theorem *)

(all x *(x x) = x)

(all x,y *(x y) = *(y x))