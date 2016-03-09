;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(ALL X,Y R (++ (X Y) ++ (Y X)))
(ALL X,Y,Z R (++ (X ++ (Y Z)) ++ (++ (X Y) Z)))
(ALL X,Y R (-- (++ (X Y) Y) X))
(ALL X,Y R (X -- (++ (X Y) Y)))
(ALL X,Y,Z R (++ (-- (X Y) Z) -- (++ (X Z) Y)))
(ALL X,Y,Z R (-- (++ (X Y) Z) ++ (-- (X Z) Y)))
(ALL X,Y,Z R (X Y) AND R (Y Z) IMPL R (X
Z))
(ALL X R (X X))
(ALL U,V,X,Y R (X Y) AND R (U ++ (X V)) IMPL R (U
++ (Y V)))
(ALL U,V,X,Y R (X Y) AND R (U -- (X V)) IMPL R (U -- (Y V)))
(ALL U,V,X,Y R (X Y) AND R (U -- (V X)) IMPL R (U -- (V Y)))

(R (++ (++ (A B) C) ++ (A ++ (B C))) AND R (++ (-- (A B) C) ++ (A --
(C B))) AND R (++ (A -- (B C)) ++ (-- (A C) B)) AND R (-- (++ (A B) C) ++ (A -- (B C))))