;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
(NOT LE (A (P) A (Q)))
(LE (Q N))
(LE (P Q))
(LE (M P))
(LT (J I))
(ALL X,Y (LE (A (X) A (Y)) AND LE (I X) AND LE (X Y) AND LE (Y N)) EQV (LE (I X) AND LE (X Y) AND LE (Y N)))
(ALL X,Y (LE (A (X) A (Y)) AND LE (M X) AND LE (X Y) AND LE (Y J)) EQV (LE (M X) AND LE (X Y) AND LE (Y J)))
(ALL X,Y (LE (A (X) A (Y)) AND LE (M X) AND LE (Y N) AND LT (J Y) AND LT (X I))
     EQV (LE (M X) AND LE (Y N) AND LT (J Y) AND LT (X I)))
(ALL X,Y (LE (X Y) AND LT (Y X)) EQV (LE (X Y) EQV LT (Y X)))

