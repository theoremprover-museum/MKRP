;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x,y +(1 x) = +(1 y) eqv x = y)
(all x not +(1 x) = 0)
(all x +(0 x) = x)
(all x +(x 0) = x)
(all x,y +(+(1 x) y) = +(1 +(x y)))
(all x,y,z +(+(z x) y) = +(z +(x y)))

(all x,y +(x y) = +(y x) impl +(+(1 x) y) = +(y +(1 x)))