;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(not Sm(0 0))
(Sm(0 s(0)))
(all x,y Sm(s(x) y) eqv Sm(x p(y)))
(all x,y Sm(p(x) y) eqv Sm(x s(y)))
(all x,y Sm(y x) eqv Sm(y s(x)))
(all x,y not Sm(y x) eqv not Sm(y p(x)))
(all x s(p(x)) = x)
(all x p(s(x)) = x)