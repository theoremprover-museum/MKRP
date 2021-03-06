;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
(* options)
(* TERM:UNITS                            T                     TERM:ITERATIONS                       10.)
(* TERM:SET.OF.SUPPORT                   NIL                   TERM:BREADTH.FIRST                    NIL)

(ALL X MAX (1. X 1.))
(ALL X MAX (X X X))
(ALL X MAX (0. X X))
(ALL X MIN (0. X 0.))
(ALL X MIN (X X X))
(ALL X MIN (1. X X))
(ALL X,Y,Z MIN (X Y Z) IMPL MAX (X Z X))
(ALL X,Y,Z MAX (X Y Z) IMPL MIN (X Z X))
(ALL X,Y,Z,XY,YZ,XYZ MIN (X Y XY) AND MIN (Y Z YZ) IMPL (MIN (X YZ XYZ) EQV MIN (XY Z XYZ)))
(ALL X,Y,Z,XY,YZ,XYZ MAX (X Y XY) AND MAX (Y Z YZ) IMPL (MAX (X YZ XYZ) EQV MAX (XY Z XYZ)))
(ALL X,Y,Z,X1,Y1,Z1 MIN (X Z X) AND MAX (X Y X1) AND MIN (Y Z Y1) AND MIN (Z X1 Z1) IMPL MAX (X Y1 Z1))
(ALL X,Y,Z,X1,Y1,Z1 MIN (X Z X) AND MAX (X Y X1) AND MIN (Y Z Y1) AND MAX (X Y1 Z1) IMPL MIN (Z X1 Z1))
(ALL X,Y,Z,X1,Y1,Z1 MIN (X Z Z) AND MAX (Y Z Y1) AND MIN (X Y X1) AND MIN (X Y1 Z1) IMPL MAX (Z X1 Z1))
(ALL X,Y,Z,X1,Y1,Z1 MIN (X Z Z) AND MAX (Y Z Y1) AND MIN (X Y X1) AND MAX (Z X1 Z1) IMPL MIN (X Y1 Z1))
(SYMMETRIC (MIN))
(SYMMETRIC (MAX))
;(ALL X,Y,Z MIN (X Y Z) IMPL MIN (Y X Z))
;(ALL X,Y,Z MAX (X Y Z) IMPL MAX (Y X Z))

(* Theorem)

(MIN (A B C)
     AND MAX (C D 1.)
     AND MIN (B D E)
     AND MIN (A E 0.)
     AND MAX (B A2 B2)
     AND MAX (A B2 1.)
     AND MAX (A B C2)
     AND MIN (A2 C2 0.)
     AND MIN (D A D2)
     AND MAX (A2 D2 E2)
     AND MIN (D B A3)
     AND MAX (A2 A3 B3)
     IMPL MIN (B3 E2 A2))