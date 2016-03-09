;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)


(* Ring *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(all x,y,z *(*(x y) z) = *(x *(y z)))

(all x,y,z *(x +(y z)) = +(*(x y) *(x z)))
(all x,y,z *(+(y z) x) = +(*(y x) *(z x)))

(* with ONE *)

(all x *(1 x) = x)
(all x *(x 1) = x)

(* Zero Divisor Free *)

(all x,y *(x y) = 0 impl x = 0 or y = 0)




(* theorem *)
(* Zeros *)

(all x +(*(x x) -(1)) = 0 impl x = 1 or x = -(1))