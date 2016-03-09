;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(* Axioms *)


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x ) x) = 0)