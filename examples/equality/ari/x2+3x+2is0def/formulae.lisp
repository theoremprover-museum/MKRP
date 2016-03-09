;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)

(* Constructors pred and succ *)

(all x s(p(x)) = x)
(all x p(s(x)) = x)

(* Def + *)

(all x +(0 x) = x)
(all x,y +(s(x) y) = s(+(x y)))
(all x,y +(p(x) y) = p(+(x y)))
(all x,y,z +(+(x y) z) = +(x +(y z)))


(* Def - *)

(-(0) = 0)
(all x -(s(x)) = p(-(x)))
(all x -(p(x)) = s(-(x)))

(* Def * *)

(all x *(0 x) = 0)
(all x,y *(s(x) y) = +(y *(x y)))
(all x,y *(p(x) y) = +(-(y) *(x y)))



(* Theorem *)

(ex x s(s(+(*(x x) +(x +(x x))))) = 0)