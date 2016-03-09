;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(q impl r)
(r impl (p and q))
(p impl (q or r))

(* Theorems *)

(p eqv q)