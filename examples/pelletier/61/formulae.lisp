;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x,y,z f(x f(y z)) = f(f(x y) z))

(* Theorems *)

(all x,y,z,w f(x f(y f(z w))) = f(f(f(x y) z) w))