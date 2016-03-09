;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(ALL X R(X) IMPL Q(X))
(NOT R(a) IMPL P(a))
(ALL x (Q(x) IMPL P(X)) AND (P(x) IMPL Q(X)))


(*THEOREM)

(EX x Q(X))