;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No Axioms *)

(* Theorems *)

((all x p(a) and (p(x) impl (ex y p(y) and r(x y))) impl (ex z,w p(z) and r(x w) and r(w z)))
 eqv
 (all x (not p(a) or p(x) or (ex z,w p(z) and r(x w) and r(w z)))
      and
      (not p(a) or not(ex y p(y) and r(x y)) or (ex z,w p(z) and r(x w) and r(w z)))))