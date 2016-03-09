;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Group *)

(all x,y,z plus(plus(x y) z) = plus(x plus(y z)))
(all x plus(0 x) = x)
(all x plus(x 0) = x)
(all x plus(minus(x) x) = 0)
(all x plus(x minus(x)) = 0)

(* Theorem *)

(all x,y plus(minus(x) minus(y)) = minus(plus(y x)))