;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)


(* Group *)

(all x,y,z plus(plus(x y) z) = plus(x plus(y z)))
(all x plus(0 x) = x)

(all x plus(minus(x) x) = 0)


(* Mult *)

(all x,y,z mult(mult(x y) z) = mult(x mult(y z)))

(* mult group connection *)

(all x,y,z mult(plus(x y) z) = plus(mult(x z) mult(y z)))
(all x,y,z mult(z plus(x y)) = plus(mult(z x) mult(z y)))


(* additional axiom and theorem *)

((all x mult(x x) = x) impl (all x,y mult(x y) = mult(y x)))