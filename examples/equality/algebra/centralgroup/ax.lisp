;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x,y,z *(*(x y) *(y z)) = y)
(all x *(*(x x) x) = f(x))
(all x *(x *(x x)) = g(x))
(all x,y *(g(x) y) = *(x y))