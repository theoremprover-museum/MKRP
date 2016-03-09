;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(P(C) OR R(C))
(NOT W(B C) AND NOT V(D B))
(ALL X V(X B) OR W(X B))
(ALL X,Y (R(X) AND V(X Y)) IMPL Q(Y))
(ALL X,Y (R(X) AND W(X Y)) IMPL (W(Y X) OR Q(Y)))
(ALL X,Y,Z P(X) IMPL (Q(Z) OR V(Z Y) OR W(Y X)))

(*THEOREM)

(EX x Q(X))