;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x +(0 x) = x)
(all x,y +(s(x) y) = s(+(x y)))

(all x *(0 x) = 0)
(all x,y *(s(x) y) = +(y *(x y)))

(all x,y +(x y) = +(y x))
(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x,y *(x y) = *(y x))
(all x,y,z *(*(x y) z) = *(x *(y z)))

(all x,y,z *(x +(y z)) = +(*(x y) *(x z)))

(all x exp(x 0) = s(0))
(all x,y exp(x s(y)) = *(x exp(x y)))

(all x,y,z exp(x +(y z)) = *(exp(x y) exp(x z)))


(* Theorem *)

(all x,y exp(x *(y 0)) = exp(exp(x y) 0))
(all x,y,z exp(x *(y z)) = exp(exp(x y) z) impl exp(x *(y s(z))) = exp(exp(x y) s(z)))