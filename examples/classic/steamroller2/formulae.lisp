;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-
(EX X1,X2,X3,X4,X5,X6 WOLF (X1) AND FOX (X2) AND BIRD (X3) AND CATERPILLAR (X4) AND SNAIL (X5) AND GRAIN (X6))
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

(EX X,Y (ANIMAL (X) AND ANIMAL (Y) AND (EATS (X Y) AND (EX Z (GRAIN (Z) AND EATS (Y Z))))))

