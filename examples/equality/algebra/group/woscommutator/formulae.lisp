;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(-(x) x) = 0)

(* NIL potent 3 *)

(all x +(+(x x) x) = 0)

(* commutator *)

(all x,y comm(x y) = +(x +(y +(-(x) -(y)))))


(* Theorem *)

(all x,y comm(comm(x y) y) = 0)
