;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Theorem *)

(((ALL X,Y + (X Y) = + (Y X))
  impl (ALL X,Y + (+ (X Y) + (X Y)) = + (+ (X X) + (y y))))
 and
 ((ALL X,Y + (+ (X Y) + (X Y)) = + (+ (X X) + (y y)))
  impl (ALL X,Y - (+ (X Y)) = + (- (X) - (Y))))
 and
 ((ALL X,Y - (+ (X Y)) = + (- (X) - (Y)))
  impl (ALL X,Y + (X Y) = + (Y X))))