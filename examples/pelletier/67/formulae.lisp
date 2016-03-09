;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x,y t(i(x i(y x))))
(all x,y,z t(i(i(x i(y z)) i(i(x y) i(x z)))))
(all x,y t(i(i(n(x) n(y)) i(y x))))
(all x,y t(i(x y)) and t(x) impl t(y))

(* Theorems *)

(all x t(i(n(n(x)) x)))