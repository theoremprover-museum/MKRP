;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(p(f(a b) f(b c)))
(p(f(b c) f(a c)))
(all x,y,z  p(x y) and p(y z) impl p(x z))

(* Theorems *)

(p(f(a b) f(a c)))