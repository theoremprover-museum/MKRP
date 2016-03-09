;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS OF PROBLAM 35 %.)
(* THERE ARE THREE PEOPLE:A, B, C.)
(* A SAYS: B AND C ARE OF THE SAME TYPE.)
(* SOMEONE AKS C: ARE A AND B OF THE SAME TYPE?)
(* WHAT DOES C ANSWER?)
(*)
(* SAME2 (X Y) MEANS: X AND Y ARE OF THE SAME TYPE.)
(* SAME3 (X Y Z) MEANS: X SAYS THAT Y AND Z ARE OF THE SAME TYPE.)
(* NSAME3 (X Y Z) MEANS: X SAYS THAT Y AND Z ARE NOT OF THE SAME TYPE.)
(*)
(ALL X KNIGHT (X) EQV NOT KNAVE (X))
(ALL X,Y SAME2 (X Y) EQV (KNIGHT (X) AND KNIGHT (Y)) OR (KNAVE (X) AND KNAVE (Y)))

(ALL X,Y,Z SAME3 (X Y Z) AND KNIGHT (X) IMPL SAME2 (Y Z))
(ALL X,Y,Z SAME3 (X Y Z) AND KNAVE (X) IMPL NOT SAME2 (Y Z))

(ALL X,Y,Z NSAME3 (X Y Z) AND KNIGHT (X) IMPL NOT SAME2 (Y Z))
(ALL X,Y,Z NSAME3 (X Y Z) AND KNAVE (X) IMPL SAME2 (Y Z))

(SAME3 (A B C))
(SAME3 (C A B) or nSAME3 (C A B))


(* Theorem )
(SAME3 (C A B))


