;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* ANY NUMBER GREATER THAN 1 HAS A PRIME DIVISOR.)
(ALL X D (X X))
(ALL X,Y,Z D (X Y) AND D (Y Z) IMPL D (X Z))
(ALL X P (X) OR D (G
(X) X))
(ALL X P (X) OR L (1 G (X)))
(ALL X P (X) OR L (G (X) X))
(L (1 A))
(ALL X NOT P (X) OR NOT
D (X A))
(ALL X NOT L (1 X) OR NOT
L (X A) OR P (F (X)))
(ALL X NOT L (1 X) OR NOT L (X A) OR D (F (X) X))

(* THERE IS NO THEOREM)