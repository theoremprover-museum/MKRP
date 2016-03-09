;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x f(x) and (all y f(y) and h(y x) impl g(y)) impl g(x))
((ex x f(x) and not g(x)) impl (ex x f(x) and not g(x) and (all y f(y) and not g(y) impl j(x y))))
(all x,y f(x) and f(y) and h(x y) impl not j(y x))

(* Theorems *)

(all x f(x) impl g(x))