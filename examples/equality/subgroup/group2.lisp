;;; -*- Syntax: Common-Lisp; Package: MKRP; Base: 10; Mode: LISP -*-
(ALL X   P( E X X))
(ALL X   P( X E X))
(ex x s(x))
(ALL X ex y  P (y X E) and p(x y e))
(ALL X,Y,Z   S (Z) and S (Y) and P (Z I (Y) X) impl  S (X))

(s(e) and (all x S(x) impl (ex y s(y) and p(x y e))))
