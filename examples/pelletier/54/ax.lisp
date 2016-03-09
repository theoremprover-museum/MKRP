;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Montague's paradox of grounded classes *)
(* Axioms *)

(all y (ex z (all x element-of(x z) eqv x = y)))
(* z = {y} *)

(all x Reg(x) eqv (all k element-of(x k) impl (ex y element-of(y k) and not(ex z element-of(z k) and element-of(z y)))))
