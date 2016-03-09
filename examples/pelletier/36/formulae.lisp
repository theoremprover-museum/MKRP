;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x (ex y f(x y)))
(all x (ex y g(x y)))
(all x (all y f(x y) or g(x y) impl (all z f(y z) or g(y z) impl h(x z))))

(* Theorems *)

(all x (ex y h(x y)))