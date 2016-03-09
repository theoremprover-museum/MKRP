;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(not(ex x s(x) and q(x)))
(all x p(x) impl (q(x) or r(x)))
(not(ex x p(x)) impl (ex x q(x)))
(all x q(x) or r(x) impl s(x))

(* Theorems *)

(ex x p(x) and r(x))