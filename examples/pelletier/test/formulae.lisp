;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(all x f(x) = g(x) or x = b)
(q(f(a)) or a = b)
(not q(g(a)))
(not a = b)



(* Axioms 2 *)

(all x,y c = x or f(x y) = b or y = a)
(all x,y b = x or f(x y) = c or y = a)


(* Axioms 3 *)

(all x,y P(y c) or not p(c f(y)) or p(x c) or not g(c y) = x)
(all x,y not p(y c) or not p(x y))
(all x g(h(x) h(x)) = x)
(p(c f(c)))


(* Axioms 4 *)

(all x,y P(y c) or not p(c f(y)) or p(x c) or not g(c y) = x)
(all x,y not p(y c) or not p(x y))
(all x x = g(x x))
(p(c f(c)))


(* Axioms 5 *)

(all x,y P(y c) or not p(c f(y)) or p(x c) or not eq(g(c y) x))
(all x,y not p(y c) or not p(x y) or q(a))
(all x eq(g(x x) x))
(all x not q(x) or not r(x))
(all x not s(x) or r(x))
(s(a))
(p(c f(c)))