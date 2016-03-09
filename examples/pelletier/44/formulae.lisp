;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x f(x) impl (ex y g(y) and h(x y)) and (ex y g(y) and not h(x y)))
(ex x j(x) and (all y g(y) impl h(x y)))

(* Theorems *)

(ex x j(x) and not f(x))