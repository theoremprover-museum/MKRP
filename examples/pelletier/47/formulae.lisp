;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

((all x w(x) impl a(x)) and (ex x w(x)))
((all x f(x) impl a(x)) and (ex x f(x)))
((all x b(x) impl a(x)) and (ex x b(x)))
((all x c(x) impl a(x)) and (ex x c(x)))
((all x s(x) impl a(x)) and (ex x s(x)))

((ex x g(x)) and (all x g(x) impl p(x)))

(all x a(x) impl (all y p(y) impl e(x y)) or (all y (a(y) and sm(y x) and (ex z p(z) and e(y z))) impl e(x y)))

(all x,y b(y) and (s(x) or c(x)) impl sm(x y))
(all x,y b(x) and f(y) impl sm(x y))
(all x,y f(x) and w(y) impl sm(x y))
(all x,y w(x) and (f(y) or g(y)) impl not e(x y))
(all x,y b(x) and c(y) impl e(x y))
(all x,y b(x) and s(y) impl not e(x y))
(all x c(x) or s(x) impl (ex y p(y) and e(x y)))

(* Theorems *)

(ex x,y a(x) and a(y) and (ex z g(z) and e(y z) and e(x y)))
