;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-




(* Axioms *)


(* Group *)

(all x,y,z plus(plus(x y) z) = plus(x plus(y z)))
(all x plus(0 x) = x)
(all x plus(x 0) = x)
(all x plus(minus(x) x) = 0)
(all x plus(x minus(x)) = 0)

(all x minus(minus(x)) = x)
(minus(0) = 0)
(all x,y plus(minus(x) minus(y)) = minus(plus(y x)))
(all x,y plus(minus(x) plus(x y)) = y)
(all x,y plus(x plus(minus(x) y)) = y)

(* Mult *)

(all x,y,z mult(mult(x y) z) = mult(x mult(y z)))

(* mult group connection *)

(all x,y,z mult(plus(x y) z) = plus(mult(x z) mult(y z)))
(all x,y,z mult(z plus(x y)) = plus(mult(z x) mult(z y)))

(all x mult(0 x) = 0)
(all x mult(x 0) = 0)

(all x,y mult(minus(y) x) = minus(mult(y x)))
(all x,y mult(x minus(y)) = minus(mult(x y)))

(* additional axiom and theorem *)

(all x mult(x x) = x)

(all x,y mult(x y) = mult(y x))