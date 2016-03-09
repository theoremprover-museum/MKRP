;;; -*- Mode: Lisp; Package: MKRP; Base: 10.; Syntax: Common-lisp -*-

(Q(A)  OR Q(B))
(ALL X:ANY  NOT Q(X)  OR P(X A)  OR P(X B))
(ALL X:ANY  NOT Q(X)  OR NOT P(X A)  OR P(B X))
(ALL X:ANY  NOT Q(X)  OR P(A X)  OR NOT P(X B))
(ALL X:ANY  NOT Q(X)  OR NOT P(A X)  OR NOT P(B X))
