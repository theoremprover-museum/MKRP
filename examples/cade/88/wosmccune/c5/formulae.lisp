;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x,y,z a(a(a(s x) y) z) = a(a(x z) a(y z)))
(all x,y a(a(w x) y) = a(a(x y) y))

(* Theorem *)

(all f ex y y = a(f y))