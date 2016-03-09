;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x,y,z f(x f(y z)) = f(f(x y) z))
(all x f(a x) = x)
(all x (ex y f(y x) = a))

(* Theorems *)

(all x,y,z f(x y) = f(z y) impl x = z)