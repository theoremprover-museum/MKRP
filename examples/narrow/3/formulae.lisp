;;; -*- Package: MARKGRAF-KARL; Base:10.; Syntax: Common-lisp; Mode: LISP -*-
             

(ALL X   +(0 X) = X)
(ALL X,y  +(S(X) y) = S(+(X y)))
(ALL X   *(0 X) = 0)
(ALL X,y *(S(X) y) = +(Y *(X y)))

(ex X,y +(*(X X) *(Y Y)) =   S(S(0)))