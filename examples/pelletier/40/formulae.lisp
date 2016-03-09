;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No Axioms *)

(* Theorems *)

((ex y (all x f(x y) eqv not f(x x))) impl not(all x (ex y (all z f(x y) eqv not f(z x)))))