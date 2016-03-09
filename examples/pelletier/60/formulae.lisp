;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No Axioms *)



(* Theorems *)

(all x p(x f(x)) eqv (ex y (all z p(z y) impl p(z f(x))) and p(x y)))