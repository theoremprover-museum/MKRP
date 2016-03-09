;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No axioms *)


(* Theorems *)

((all x p(a) and (p(x) impl p(b)) impl p(c)) eqv (all x (not p(a) or (p(x) or p(c))) and (not p(a) or (not p(b) or p(c)))))