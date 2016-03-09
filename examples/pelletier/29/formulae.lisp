;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

((ex x f(x)) and (ex x g(x)))

(* Theorems *)

(((all x f(x) impl h(x)) and (all x g(x) impl j(x))) eqv (all x,y f(x) and g(y) impl h(x) and j(y)))