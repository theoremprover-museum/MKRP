;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(ex z,w (all x,y f(x y) eqv (x = z) and y = w))

(* Theorems *)

(ex z (all x (ex w (all y f(x y) eqv y = w)) eqv x = z))