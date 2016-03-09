;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-

(((ex x all y p(x) eqv p(y)) eqv ((ex x q(x)) eqv (all y p(y))))
 eqv
 ((ex x all y q(x) eqv q(y)) eqv ((ex x p(x)) eqv (all y q(y)))))