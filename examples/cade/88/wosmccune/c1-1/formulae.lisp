;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x,y,z a(a(a(s x) y) z) = a(a(x z) a(y z)))
(all x,y,z a(a(a(b x) y) z) = a(x a(y z)))
(all x,y,z a(a(a(c x) y) z) = a(a(x z) y))
(all x a(i x) = x)

(* Theorem *)

(all f ex y y = a(f y))