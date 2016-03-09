;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x ) x) = 0)




(* theorems *)

(all x +(0 x) = x and +(01 x) = x impl 0 = 01)
(+(a1 a) = 0 and +(a2 a) = 0 impl a1 = a2)