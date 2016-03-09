;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP; Base: 10 -*-
(p (a b c))
(all x p(e x x))
(all x p(x e x))
(all x,y,z,u,v,w p(x y u) and p(y z v) and p(x v w) impl p(u z w))
(all x,y,z,u,v,w p(x y u) and p(y z v) and p(u z w) impl p(x v w))
(all x p(x x e))

(p(b a c))