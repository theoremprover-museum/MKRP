;;; -*- Mode: Lisp; Package: MKRP; Syntax: Common-lisp -*-
(k(abs(plus(minus(a(c6 c2) c3) minus(a(c7 c2) c4))) c1))


(all x,y k(abs(plus(x y)) plus(abs(x) abs(y))))
(all x,y,z k(x y) and k(y z) impl k(x z))
(all x k(plus(f(x) f(x)) x))
(all x,y k(abs(minus(y c5)) f1(y)) impl k(abs(minus(a(c6 y) c3)) x))
(all x,y k(abs(minus(y c5)) f2(y)) impl k(abs(minus(a(c7 y) c4)) x))
(all y k(abs(minus(c2 c5)) y))
(all x,y,z,u k(u z) and k(y x) impl k(plus(u y) plus(z x)))