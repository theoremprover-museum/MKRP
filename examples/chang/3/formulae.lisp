;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* LEFT IDENTITY = RIGHT IDENTITY)
(ALL X P (I (X)
X E))
(ALL X P (E X X))
(ALL U,V,W,X,Y,Z P (X Y U) AND P (Y Z V) AND P (U Z W) IMPL P (X V W))
(ALL U,V,W,X,Y,Z P (X Y U) AND P (Y Z V) AND P (X V W) IMPL P (U Z W))

(ALL X P (X E X))