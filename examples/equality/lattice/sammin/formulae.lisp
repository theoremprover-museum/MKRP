;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x o(0 x) = x)
(all x a(0 x) = 0)
(all x a(x n(x)) = 0)
(all x,y o(x a(x y)) = x)
(all x,y a(x o(x y)) = x)
(all x,y,z a(x a(y z)) = a(a(x y) z))
(all x,y,z a(x z) = z impl a(x o(y z)) = o(z a(x y)))




(* Theorem *)

(all x,y a(o(n(o(x y)) a(n(a(x y)) y)) o(n(o(x y)) a(n(a(x y)) x))) = a(n(x) n(y)))
