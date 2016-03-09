;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(e(a p))
(all x,y,z e(i(i(x y) i(i(y z) i(x z))) p))
(all x:p (all y (not e(i(x y) p) or e(y p))))
(all x e(i(i(n(x) x) x) p))
(all x,y e(i(x i(n(x) y)) p))


(all x e(i(x x) p))