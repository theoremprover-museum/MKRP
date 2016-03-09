;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No Axioms *)



(* Theorems *)

((all x (ex y p(y) and x = f(y)) impl p(x)) eqv (all x p(x) impl p(f(x))))