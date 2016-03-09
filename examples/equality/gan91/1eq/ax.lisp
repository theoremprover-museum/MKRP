;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(Sm(0 0) = FF)
(Sm(0 s(0)) = tt)
(all x,y Sm(s(x) y) = Sm(x p(y)))
(all x,y Sm(p(x) y) = Sm(x s(y)))
(all x,y Sm(y x) = TT impl Sm(y s(x)) = TT)
(all x,y Sm(y x) = FF impl Sm(y p(x)) = FF)
(all x s(p(x)) = x)
(all x p(s(x)) = x) 