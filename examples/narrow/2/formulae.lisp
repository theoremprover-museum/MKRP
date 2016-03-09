;;; -*- Package: MARKGRAF-KARL; Base:10.; Syntax: Common-lisp; Mode: LISP -*-
      
(ALL X  *(X X) = X)


(ex T,U,V,W,X,Y,Z *(*(X Y) *(*(*(A B) C) Z)) =
    *(*(U *(V W)) *(T *(A B))))