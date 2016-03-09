;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(e(lupo wolf)
  and e(foxy fox)
  and e(tweety bird)
  and e(schnecki snail)
  and e(stalky grain))

(all x:wolf e(x animal))
(all x:fox e(x animal))
(all x:bird e(x animal))
(all x:snail e(x animal))

(all x:grain e(x plant))

(ALL X:ANIMAL
     (ALL Y:PLANT EATS (X Y)) OR (ALL Y:ANIMAL SMALLER (Y X) AND (EX Z:(PLANT) EATS (Y Z)) IMPL EATS (X Y)))
(ALL X:SNAIL ALL Y:BIRD SMALLER (X Y))
(ALL X:BIRD ALL Y:FOX SMALLER (X Y))
(ALL X:FOX ALL Y:WOLF SMALLER (X Y))
(ALL X:FOX ALL Y:WOLF NOT EATS (Y X))
(ALL X:GRAIN ALL Y:WOLF NOT EATS (Y X))
(ALL X:BIRD ALL Y:SNAIL NOT EATS (X Y))
(ALL X:SNAIL EX Y:PLANT EATS (X Y))



(* THEOREMS ) 

(EX X,Y:ANIMAL ALL Z:GRAIN EATS (X Y) AND EATS (Y Z))


; (all x (ex z:(g(x a)) P(z x)))
