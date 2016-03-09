;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Group *)

(all x,y,z plus(plus(x y) z) = plus(x plus(y z)))
(all x plus(0 x) = x)
(all x plus(minus(x) x) = 0)

(commutative(plus))

(* Theorem *)

(all x,y plus(minus(x) minus(y)) = minus(plus(x y)))