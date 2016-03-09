;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No axioms *)


(* Theorems *)

(((p and (q impl r)) impl s) eqv ((not(p) or q or s) and (not(p) or not(r) or s)))