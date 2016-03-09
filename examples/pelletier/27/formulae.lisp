;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(ex x f(x) and not(g(x)))
(all x f(x) impl h(x))
(all x j(x) and i(x) impl f(x))
((ex x h(x) and not(g(x))) impl (all x i(x) impl not(h(x))))

(* Theorems *)

(all x j(x) impl not(i(x)))