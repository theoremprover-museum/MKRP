;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No axioms *)

(ex x p impl f(x))
(ex x f(x) impl p)

(* Theorems *)

(ex x p eqv f(x))