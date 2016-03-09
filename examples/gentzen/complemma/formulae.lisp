;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(NOT Q(c1))
(ALL X NOT P(X) OR Q(X) OR A(X))
(ALL X P(X) IMPL B(X))
(ALL X P(X) OR C(X))
(ALL X P(X) OR D(X))
(ALL X (NOT F (X)) IMPL (NOT ((A(X) AND B(X)) OR (C(X) AND D(X)))))

(*THEOREM)

(EX X F(X))