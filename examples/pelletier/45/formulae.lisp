;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x f(x) and (all y g(y) and h(x y) impl j(x y)) impl (all y g(y) and h(x y) impl k(y)))
(not(ex y l(y) and k(y)))
(ex x f(x) and (all y h(x y) impl l(y)) and (all y g(y) and h(x y) impl j(x y)))

(* Theorems *)

(ex x f(x) and not(ex y g(y) and h(x y)))