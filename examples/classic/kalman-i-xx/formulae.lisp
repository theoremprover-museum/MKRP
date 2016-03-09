;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(ALL X,Y P (X) AND P (F (X Y)) IMPL P (Y))
(ALL X,Y,Z P (F (X F (F (Y F (Z X)) F (Z Y)))))


(ALL X P (F (X X)))