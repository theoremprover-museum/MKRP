;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* POSITIVE2)
(ALL x,y P(x) and P(y) impl P(f(x y)))
(P(a))
(P(b))

(P(f(f(a f(b a)) f(b f(a b)))))