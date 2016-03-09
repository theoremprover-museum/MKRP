;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(ex x l(x) and k(x a))
(l(a) and l(b) and l(c))
(all x l(x) impl x = a or x = b or x = c)
(all y,x k(x y) impl h(x y))
(all x,y k(x y) impl not r(x y))
(all x h(a x) impl not h(c x))
(all x not x = b impl h(a x))
(all x not r(x a) impl h(b x))
(all x h(a x) impl h(b x))
(all x (ex y not h(x y)))
(not a = b)

(* Theorems *)

(k(a a))