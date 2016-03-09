;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* IF A IS A PRIME AND A = B**2/C**2 THEN A DIVIDES B.)
(P (A))
(M (A S (C) S (B)))
(ALL X M
(X X S (X)))
(ALL X,Y,Z M (X Y Z) IMPL D (X Z))
(ALL U,X,Y,Z P (X) AND M (Y Z U) AND
D (X U) IMPL D (X Y) OR D (X Z))

(D (A B))