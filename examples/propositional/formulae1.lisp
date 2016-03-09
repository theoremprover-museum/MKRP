;;; -*- Mode: Lisp; Package: MKRP; Base: 10.; Syntax: Common-lisp -*-


(EX X A(X) OR B(X))
(ALL X B(X) IMPL C(X))
(ALL X (A(X) OR D(X)) IMPL C(X))


(*Theorems)
(EX X C(X))