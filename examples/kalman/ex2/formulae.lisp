;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* transitive)
(all x p(x) impl q(x))
(all x q(x) impl r(x))


(* Theorem *)
(all x p(x) impl r(x))

