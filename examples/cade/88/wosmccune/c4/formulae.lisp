;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x,y,z a(a(a(s x) y) z) = a(a(x z) a(y z)))
(all x,y a(a(k x) y) = x)

(* Theorem *)

(ex u all x,y a(a(u x) y) = a(y a(a(x x) y)))