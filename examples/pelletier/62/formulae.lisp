;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No Axioms *)



(* Theorems *)

((all x p(a) and p(x) impl p(f(x)))
 eqv
 (all x (not p(a) or p(x) or p(f(f(x)))) and (not p(a) or not p(f(x)) or p(f(f(x))))))