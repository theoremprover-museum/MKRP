;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x,y t(i(x i(y x))))
(all x,y,z t(i(i(x i(y z)) i(i(x y) i(x z)))))
(all x,y t(i(i(n(x) n(y)) i(y x))))
(all x,y t(i(x y)) and t(x) impl t(y))

(all x,y t(i(b1(i(x y)) i(b1(x) b1(y)))))
(all x t(i(b1(x) x)))
(all x t(x) impl t(b1(x)))

(* Theorems *)

(all x t(i(b1(x) n(b1(n(x))))))