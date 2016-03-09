;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

((all x p(x)) impl (all x q(x)))
((all x q(x) or r(x)) impl (ex x q(x) and s(x)))
((ex x s(x)) impl (all x f(x) impl g(x)))

(* Theorems *)

(all x p(x) and f(x) impl g(x))