;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(not (ex x f(x) and (g(x) or h(x))))
(ex x i(x) and f(x))
(all x not h(x) impl j(x))

(* Theorems *)

(ex x i(x) and j(x))