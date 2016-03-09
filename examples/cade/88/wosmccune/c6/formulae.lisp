;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x,y,z a(a(a(s x) y) z) = a(a(x z) a(y z)))
(all x,y a(a(k x) y) = x)

(* Theorem *)

(ex y all x a(y x) = a(x a(x y)))
