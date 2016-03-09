


;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(all x P(e(x x)))
(all x,y P(e(e(x y) e(y x))))
(all x,y,z P(e(e(x y) e(e(y z) e(x z)))))
(all x,y P(e(x y)) and P(x) impl P(y))





(* Theorems)

(all x,y,z P(e(e(x y) e(e(z x) e(y z)))))

(all x,y,z P(e(e(x y) e(e(x z) e(z y)))))

