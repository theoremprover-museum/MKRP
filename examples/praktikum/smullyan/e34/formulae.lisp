;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS OF PROBLEM 34 %.)
(* WE HAVE THREE PERSONS
A,B,C.)
(* TWO PEOPLE ARE OF THE SAME TYPE , IF THEY ARE BOTH KNIGHTS)
(* OR
BOTH KNAVES.)
(* A SAYS : B IS A KNAVE.)
(* B SAYS : A AND C ARE OF THE SAME
TYPE.)
(* WHAT IS C ?)
(*)
(* KNAVE2 (X,Y) MEANS : X SAYS
: Y IS A KNAVE.)
(* SAME3 (X,Y,Z) MEANS : X SAYS : Y AND Z ARE OF THE SAME TYPE)
(* SAME2 (X Y) MEANS X AND Y ARE OF THE SAME TYPE)
(*)
(ALL X KNIGHT (X) EQV
NOT KNAVE (X))
(ALL X,Y KNAVE (X) AND KNAVE2 (X Y) IMPL KNIGHT (Y))
(ALL X,Y KNIGHT (X) AND KNAVE2 (X Y) IMPL KNAVE (Y))
(ALL X,Y SAME2 (X Y) EQV (KNIGHT
(X) AND KNIGHT (Y)) OR (KNAVE (X) AND KNAVE (Y)))
(ALL X,Y,Z KNIGHT (X) AND SAME3 (X Y Z) IMPL SAME2 (Y Z))
(ALL
X,Y,Z KNAVE (X) AND SAME3 (X Y Z) IMPL NOT SAME2 (Y Z))
(KNAVE2 (A B))
(SAME3 (B A C))

(* Theorem)

(KNAVE (C))


