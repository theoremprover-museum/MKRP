;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)

(all x,y p(x) and p(y) impl p(f(x y)))
(p(a))
(p(b))

(* Theorems *)

(p(f(f(b a) f(a b))))

(p(f(f(f(b a) b) f(f(a b) a))))
