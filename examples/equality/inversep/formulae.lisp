;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-

(ALL X E (+ (O X) X))
(ALL X E (+ (X O) X))
(ALL X,Y,Z E (+ (X + (Y Z)) + (+ (X Y) Z)))
(ALL X E (+ (- (X) X) O))

(all x e(x x))
(symmetric (e))
(all x,y,z e(x y) and e(y z) impl e(x z))

(all x,y,u,v e(x u) and e(y v) impl e(+(x y) +(u v)))
(all x,y e(x y) impl e(-(x) -(y)))

(* Theorem)

(E (- (- (A)) A))