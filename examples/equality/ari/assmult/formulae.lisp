;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)

(all x +(0 x) = x)
(all x,y +(s(x) y) = s(+(x y)))

(all x *(0 x) = 0)
(all x,y *(s(x) y) = +(y *(x y)))

(all x,y +(x y) = +(y x))
(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x,y *(x y) = *(y x))

(all x,y,z *(x +(y z)) = +(*(x y) *(x z)))
(all x,y,z *(+(y z) x) = +(*(y x) *(z x)))


(* Theorem *)


(all x,y,z *(*(0 y) z) = *(0 *(y z)))
(all x,y,z *(*(x y) z) = *(x *(y z)) impl *(*(s(x) y) z) = *(s(x) *(y z)))