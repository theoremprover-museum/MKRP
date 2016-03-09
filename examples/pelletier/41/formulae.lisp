;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all z (ex y (all x f(x y) eqv (f(x z) and not f(x x)))))

(* Theorems *)

(not(ex z (all x f(x z))))