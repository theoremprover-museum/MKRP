;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(ALL X   +(0 X) = X)
(ALL X,y  +(S(X) y) = S(+(X y)))
(ALL X   *(0 X) = 0)
(ALL X,y *(S(X) y) = +(Y *(X y)))

(ex X,y P(+(*(X X) *(Y Y))) or  not P(+(x y)))