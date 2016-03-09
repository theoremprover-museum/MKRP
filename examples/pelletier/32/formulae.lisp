;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x f(x) and (g(x) or h(x)) impl i(x))
(all x i(x) and h(x) impl j(x))
(all x k(x) impl h(x))

(* Theorems *)

(all x f(x) and k(x) impl j(x))