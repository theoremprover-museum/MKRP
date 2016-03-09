;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms)

(all x a(b(c(x))) = d(x))
(all x b(c(d(x))) = e(x))
(all x c(d(e(x))) = a(x))
(all x d(e(a(x))) = b(x))
(all x e(a(b(x))) = c(x))
(all x a(a1(x)) = x)
(all x a1(a(x)) = x)
(all x b(b1(x)) = x)
(all x b1(b(x)) = x)
(all x c(c1(x)) = x)
(all x c1(c(x)) = x)
(all x d(d1(x)) = x)
(all x d1(d(x)) = x)
(all x e(e1(x)) = x)
(all x e1(e(x)) = x)


(* Theorems)



((all x d(x) = a(a(a(x)))) and
 (all x d(x) = e(e(e(e(e(x)))))))
