;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* IF S IS A NONEMPTY SUBSET OF A GROUP SUCH THAT IF X,Y BELONG TO
S,)
(* THEN XY**-1 BELONGS TO S, THEN THE IDENTITY E BELONGS TO S.)
(ALL X
P (I (X) X E))
(ALL X P (X I (X) E))
(ALL X P (E X X))
(ALL X P (X E X))
(S (A))
(ALL X,Y,Z S (X) AND S (Y) AND P (X I (Y)
Z) IMPL S (Z))
(ALL U,V,W,X,Y,Z P (X Y U) AND P (Y Z V) AND P (U Z W) IMPL P (X V W))
(ALL U,V,W,X,Y,Z P (X Y U) AND P (Y Z V) AND P (X V W)
IMPL P (U Z W))

(S (E))