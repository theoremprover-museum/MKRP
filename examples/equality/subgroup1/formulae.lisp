;;; -*- Syntax: Common-Lisp; Package: MKRP; Base: 10; Mode: LISP -*-
(ALL W  P( E W W))
(ALL U  P(U I(U) E))
(EX X S(X))
(ALL X,Y,Z   S(X) and S(Y) and P(X I(Y) Z) impl  S (Z))

(S(E) and (ALL X S(x) impl S(I(X))))
