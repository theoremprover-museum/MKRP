;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)


(* Group *)

(all x,y,z +(+(x y) z) = +(x +(y z)))
(all x +(0 x) = x)
(all x +(x 0) = x)
(all x +(-(x) x) = 0)
(all x +(x -(x)) = 0)



(* theorems *)

(all x1,x2,a +(a x1) = +(a x2) impl x1 = x2)
(all y1,y2,b +(y1 b) = +(y2 b) impl y1 = y2)