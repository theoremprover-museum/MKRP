;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS FOR PROBLEM 40 %.)
(* THERE ARE TWO PEOPLE A AND B.)
(* EACH OF WHOM IS EITHER A KNIGHT,OR A KNAVE,OR A NORMAL.)
(* A SAYS :B IS A KNIGHT.)
(* B SAYS : A IS NOT A KNIGHT.)
(* PROVE: AT LEAST ONE OF THEM IS TELLING THE TRUTH BUT IS NOT A KNIGHT.)
(*)
(* KNIGHT2 (X Y) MEANS X SAYS Y IS A KNIGHT.)
(* NKNIGHT2 (X Y) MEANS X SAYS Y IS NOT A KNIGHT.)
(* TRUTH (X) MEANS X SAYS THE TRUTH.)
(* LIE (X) MEANS X IS TELLING A LIE.)
(*)
(ALL X NORMAL (X) EQV NOT KNIGHT (X) AND NOT KNAVE (X))
(ALL X KNAVE (X) EQV NOT NORMAL (X) AND NOT KNIGHT (X))
(ALL X KNIGHT (X) EQV NOT NORMAL (X) AND NOT KNAVE (X))
(ALL X,Y KNIGHT2 (X Y) AND KNAVE (X) IMPL NOT KNIGHT (Y))
(ALL X,Y KNIGHT2 (X Y) AND KNIGHT (X) IMPL KNIGHT (Y))
(ALL X,Y NKNIGHT2 (X Y) AND KNIGHT (X) IMPL NOT KNIGHT (Y))
(ALL X,Y NKNIGHT2 (X Y) AND KNAVE (X) IMPL KNIGHT (Y))
(ALL X,Y KNIGHT2 (X Y) AND TRUTH (X) IMPL KNIGHT (Y))
(ALL X,Y KNIGHT2 (X Y) AND LIE (X) IMPL NOT KNIGHT (Y))
(ALL X,Y NKNIGHT2 (X Y) AND TRUTH (X) IMPL NOT KNIGHT (Y))
(ALL X,Y NKNIGHT2 (X Y) AND LIE (X) IMPL KNIGHT (Y))
(ALL X,Y KNIGHT2 (X Y) AND NOT KNIGHT (Y) IMPL LIE (X))
(ALL X,Y KNIGHT2 (X Y) AND KNIGHT (Y) IMPL TRUTH (X))
(ALL X,Y NKNIGHT2 (X Y) AND KNIGHT (Y) IMPL LIE (X))
(ALL X,Y NKNIGHT (X Y) AND NOT KNIGHT (Y) IMPL TRUTH (X))
(ALL X TRUTH (X) EQV NOT LIE (X))
(ALL X TRUTH (X) IMPL NORMAL (X) OR KNIGHT (X))
(ALL X LIE (X) IMPL KNAVE (X) OR NORMAL (X))
(KNIGHT2 (A B))
(NKNIGHT2 (B A))

(TRUTH (A) AND NOT KNIGHT (A) OR TRUTH (B) AND NOT KNIGHT (B))