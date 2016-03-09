;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x,y q(x y) eqv (all z f(z x) eqv f(z y)))

(* Theorems *)

(all x.y q(x y) eqv q(y x))