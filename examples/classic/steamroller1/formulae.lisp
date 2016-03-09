;;; -*- Mode: Lisp; Package: MKRP; Base: 10.; Syntax: Common-lisp -*-
(EX Xa,Xb,Xc,Xd,Xe,Xf WOLF (Xa) AND FOX (Xb) AND BIRD (Xc) AND CATERPILLAR (Xd) AND SNAIL (Xe) AND GRAIN (Xf))
(ALL X (WOLF (X) OR FOX (X) OR BIRD (X) OR CATERPILLAR (X) OR SNAIL (X)) IMPL ANIMAL (X))
(ALL X GRAIN (X) IMPL PLANT (X))
(ALL X  
     ANIMAL (X)
     IMPL  
     (ALL Y PLANT (Y) IMPL EATS (X Y))
     OR  
     (ALL Y ANIMAL (Y) AND SMALLER (Y X) AND (EX Z PLANT (Z) AND EATS (Y Z)) IMPL EATS (X Y)))
(ALL X,Y (CATERPILLAR (X) OR SNAIL (X)) AND BIRD (Y) IMPL SMALLER (X Y))
(ALL X,Y BIRD (X) AND FOX (Y) IMPL SMALLER (X Y))
(ALL X,Y FOX (X) AND WOLF (Y) IMPL SMALLER (X Y))
(ALL X,Y (FOX (X) OR GRAIN (X)) AND WOLF (Y) IMPL NOT EATS (Y X))
(ALL X,Y BIRD (X) AND CATERPILLAR (Y) IMPL EATS (X Y))
(ALL X,Y BIRD (X) AND SNAIL (Y) IMPL NOT EATS (X Y))
(ALL X (CATERPILLAR (X) OR SNAIL (X)) IMPL (EX Y PLANT (Y) AND EATS (X Y)))

(EX X,Y ANIMAL (X) AND ANIMAL (Y) AND (ALL Z GRAIN (Z) IMPL EATS (X Y) AND EATS (Y Z)))