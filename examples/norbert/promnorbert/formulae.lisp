;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(p(n))
(p(i))
(all x p(x) impl k(x) or w(f(x) x))
(all x,y w(x y) impl g(x y))
(all x (w(n x) impl not(k(n))))
(all x,y k(x) and k(y) impl g(x y))
(f(n) = i)
(f(i) = n)
(symmetric (w))
(symmetric (g))

(g(i n))