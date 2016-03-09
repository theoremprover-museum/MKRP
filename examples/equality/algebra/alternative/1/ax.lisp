;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x +(0 x) = x)
(all x +(-(x) x) = 0)
(all x,y +(x y) = +(y x))
(all x,y,z +(+(x y) z) = +(x +(y z)))

(all x,y,z *(x +(y z)) = +(*(x y) *(x z)))
(all x,y,z *(+(y z) x) = +(*(y x) *(z x)))

(all x,y *(*(x y) y) = *(x *(y y)))
(all x,y *(*(x x) y) = *(x *(x y)))