;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x,y,z max(max(x y) z) = max(x max(y z)))
(all x,y max(x y) = max(y x))

(all x,y,z min(min(x y) z) = min(x min(y z)))
(all x,y min(x y) = min(y x))

(all x max(x 0) = x)
(all x min(x x) = x)
(all x,y min(x max(x y)) = x)
(all x,y,z min(x y) = x impl max(x min(y z)) = min(y max(x z)))
(max(a b) = d1)
(min(a b) = d2)
(min(c1 d1) = 0)
(min(c2 d2) = 0)
(min(b c2) = e)
(min(a c2) = f)
(max(c1 e) = g)
(max(c1 f) = h)


(* Theorem *)

(min(g h) = c1)