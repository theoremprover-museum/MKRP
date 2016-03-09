;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(all x,y o(x y) = o(y x))
(all x,y a(x y) = a(y x))

(all x o(0 x) = x)
(all x a(1 x) = x)
(all x a(0 x) = 0)
(all x o(1 x) = 1)

(all x a(x x) = x)
(all x o(x x) = x)

(all x a(x n(x)) = 0)
(all x o(x n(x)) = 1)

(all x,y o(x a(x y)) = x)
(all x,y a(x o(x y)) = x)
(all x,y,z a(x a(y z)) = a(a(x y) z))
(all x,y,z o(x o(y z)) = o(o(x y) z))


(* Theorem *)

(all x,y ((a(n(x) n(y)) = n(o(x y)))
	  and
	  (o(n(x) n(y)) = n(a(x y)))))