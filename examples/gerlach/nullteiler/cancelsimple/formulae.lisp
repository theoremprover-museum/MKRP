;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)


(* Ausgewaehlte Ringaxiome *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(all x,y,z *(*(x y) z) = *(x *(y z)))


(all x,y,z *(+(y z) x) = +(*(y x) *(z x)))
(all x,y -(*(x y)) = *(-(x) y))

(* Nullteilerfrei *)

(all x,y *(x y) = 0 impl x = 0 or y = 0)




(* theorem *)

(all x,y,z *(x y) = *(z y) and not(y = 0) impl x = z)