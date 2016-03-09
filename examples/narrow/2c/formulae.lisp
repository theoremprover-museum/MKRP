;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-




(ALL X  *(X X) = X)
(ALL X  not *(*(A *(A x)) *(A *(A B))) = *(A B))


(ex T,U,V,W,X,Y,Z P(*(*(X Y)      *(*(*(A B) C) Z))) and not Z = *(A B) or not
                  P(*(*(U *(V W)) *(T      *(A B)))))
