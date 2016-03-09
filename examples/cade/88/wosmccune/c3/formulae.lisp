;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x,y,z a(a(a(b x) y) z) = a(x a(y z)))
(all x,y a(a(l x) y) = a(x a(l a(y y))))

(* Theorem *)

(not (ex y all f a(y f) = a(f a(y f))))