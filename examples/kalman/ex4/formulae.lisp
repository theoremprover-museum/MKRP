;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-




(* composition of surjections)
(all y ex x eq(f(x) y))
(all z ex y eq(g(y) z))
(all x,y,z eq(x y) and eq(y z) impl eq(x z))
(all x,y eq(x y) impl eq(g(x) g(y)))



(all z ex x eq(g(f(x)) z))

