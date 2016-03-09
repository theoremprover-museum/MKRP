;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(ex x p(x))
(all x f(x) impl (not(g(x)) and r(x)))
(all x p(x) impl (g(x) and f(x)))
((all x p(x) impl q(x)) or (ex x p(x) and r(x)))

(* Theorems *)

(ex x q(x) and p(x))