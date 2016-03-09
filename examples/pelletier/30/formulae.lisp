;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x f(x) or g(x) impl not h(x))
(all x (g(x) impl not i(x)) impl f(x) and h(x))

(* Theorems *)

(all x i(x))