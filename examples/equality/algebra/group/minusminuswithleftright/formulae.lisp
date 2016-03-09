;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)


(* Group *)

(all x,y,z plus(plus(x y) z) = plus(x plus(y z)))
;(associative (plus))
;(all x,y plus(x y) = plus(y x))
(all x plus(0 x) = x)
(all x plus(x 0) = x)
(all x plus(minus(x) x) = 0)
(all x plus(x minus(x)) = 0)




(* theorem *)

(all x minus(minus(x)) = x)