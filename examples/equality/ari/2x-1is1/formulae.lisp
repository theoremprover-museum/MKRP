;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(* Constructors pred and succ *)

(all x s(p(x)) = x)
(all x p(s(x)) = x)

(* Def + *)

(all x +(0 x) = x)
(all x,y +(s(x) y) = s(+(x y)))
(all x,y +(p(x) y) = p(+(x y)))

(* Props + *)

(all x +(x 0) = x)
(all x,y +(y s(x)) = s(+(y x)))
(all x,y +(y p(x)) = p(+(y x)))
(all x,y,z +(+(x y) z) = +(x +(y z)))

(* Def - *)

(-(0) = 0)
(all x -(s(x)) = p(-(x)))
(all x -(p(x)) = s(-(x)))

(* Props - *)

(all x -(-(x)) = x)
(all x +(-(x) x) = 0)
(all x +(x -(x)) = 0)
(all x,y +(x +(-(x) y)) = y)
(all x,y +(-(x) +(x y)) = y)
(all x,y -(+(x y)) = +(-(y) -(x)))

(* Def * *)

(all x *(0 x) = 0)
(all x,y *(s(x) y) = +(y *(x y)))
(all x,y *(p(x) y) = +(-(y) *(x y)))

(* Props * *)

(all x *(x 0) = 0)
(all x,y *(y s(x)) = +(*(y x) y))
(all x,y *(y p(x)) = +(*(y x) -(y)))


(* Theorem *)

(ex x +(x +(x p(0))) = s(0))