;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(* Axioms *)

(ALL X,Y,Z + (+ (X Y) Z) = + (X + (Y Z)))
(ALL X + (0 X) = X)
(ALL X + (- (X) X) = 0)
