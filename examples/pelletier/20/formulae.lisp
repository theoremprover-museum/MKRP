;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* No axioms *)


(* Theorems *)

(all x,y (ex z (all w ((p(x) and q(y)) impl (r(z) and s(w))) impl (ex x,y (p(x) and q(y)) impl ex z r(z)))))