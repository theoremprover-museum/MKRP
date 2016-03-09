;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x a(b(x)) = c(x))
(all x b(c(x)) = d(x))
(all x c(d(x)) = e(x))
(all x d(e(x)) = f(x))
(all x e(f(x)) = g(x))
(all x f(g(x)) = a(x))
(all x g(a(x)) = b(x))
(all x a(a1(x)) = x)
(all x b(b1(x)) = x)
(all x c(c1(x)) = x)
(all x d(d1(x)) = x)
(all x e(e1(x)) = x)
(all x f(f1(x)) = x)
(all x g(g1(x)) = x)
(all x a1(a(x)) = x)
(all x b1(b(x)) = x)
(all x c1(c(x)) = x)
(all x d1(d(x)) = x)
(all x e1(e(x)) = x)
(all x f1(f(x)) = x)
(all x g1(g(x)) = x)



(all eps f(eps) = d1(d1(d1(d1(eps)))))