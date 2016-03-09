;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(ALL X,Y P (X) AND P (F (X Y)) IMPL P (Y))
(ALL X,Y,Z P (F (X F (F (Y Z) F (F (Z X) Y)))))
(ALL X,Y,Z P (X) IMPL P (F (F (Y Z) F (F (Z X) Y))))

(ALL X P (F (X X)))