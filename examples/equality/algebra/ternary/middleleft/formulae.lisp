;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x,y,z,v,w compose(compose(v w x) y compose(v w z)) = compose(v w compose(x y z)))

(all x,y compose(y x x) = x)
(all x,y compose(x y x) = x)
(all x,y compose(x x y) = x)

(all x,y compose(x y minus(y)) = x)
(all x,y compose(minus(y) y x) = x)


(* Theorem *)

(all x,y compose(x minus(x) y) = y)
