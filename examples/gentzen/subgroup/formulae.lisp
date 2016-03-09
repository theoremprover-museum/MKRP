;;; -*- Syntax: Common-Lisp; Package: MKRP; Base: 10; Mode: LISP -*-

(ALL X   P( E X X))
(ALL X   P( X E X))
(ALL X   P (I (X) X E))
(ALL X   p( X I (X) E))
(ALL X,Y,Z   S (Z) and S (Y) and P (Z I (Y) X) impl  S (X))

(all x S(x) impl 
     S (I (x)))
