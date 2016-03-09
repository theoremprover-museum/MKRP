;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* ar90/sec8/ex14a)
(* f(x y) means x + y)
(* g(x y) means x - y)
(* P(u v) means u > 0 and v = 0)
(all x,y,u P(x u) and P(y u) impl P(f(x y) u))
(all x,u,v P(x u) and P(x g(u v)) impl P(x v))
(all x,y,z P(a g(g(x y) g(g(x z) g(y z)))))
(all x,y,z P(b g(g(x y) g(g(x z) g(y z)))))


(all x P(f(f(a f(b a)) f(b f(a b))) g(x x)))