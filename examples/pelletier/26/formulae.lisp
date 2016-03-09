;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

((ex x p(x)) eqv (ex x q(x)))
(all x,y p(x) and q(y) impl (r(x) eqv s(y)))

(* Theorems *)

((all x p(x) impl r(x)) eqv (all x q(x) impl s(x)))