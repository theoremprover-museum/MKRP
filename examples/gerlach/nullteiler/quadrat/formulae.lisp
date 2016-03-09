;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)


(* komm. Ring *)

(all x,y,z plus(plus(x y) z) = plus(x plus(y z)))
(all x,y plus(x y) = plus(y x))
(all x plus(0 x) = x)
(all x plus(minus(x) x) = 0)

(all x,y,z mult(mult(x y) z) = mult(x mult(y z)))
(all x,y mult(x y) = mult(y x))

(all x,y,z mult(x plus(y z)) = plus(mult(x y) mult(x z)))

(* mit Eins *)

(all x mult(1 x) = x)

(* Nullteilerfrei *)

(all x,y mult(x y) = 0 impl x = 0 or y = 0)




(* theorem *)

(all x plus(mult(x x) minus(1)) = 0 impl x = 1 or x = minus(1))