;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* AXIOMS FOR PROBLEM 45 %.)
(* THE ISLAND OF BAHAVA)
(* A KNIGHT COULD MARRY ONLY A KNAVE.)
(* A NORMAL COULD ONLY MARRY A NORMAL.)
(* A KNAVE COULD ONLY MARRY A KNAVE.)
(*)
(* WE FIRST CONSIDER
A MARRIED COUPLE,MRS. AND MR.ABNORMAL.)
(* MR.ABNORMAL SAYS: MY WIFE IS NORMAL.)
(* MRS.ABNORMAL SAYS: MY HUSBAND IS NORMAL.)
(* WHAT ARE MRS. AND MR.ABNORMAL?)
(*)
(* MARRIED (X Y) MEANS X AND Y ARE MARRIED.)
(* NORMAL2 (X Y) MEANS X
SAYS : Y IS NORMAL.)
(*)
(ALL X KNIGHT (X) EQV NOT KNAVE (X) AND NOT NORMAL
(X))
(ALL X KNAVE (X) EQV NOT KNIGHT (X) AND NOT NORMAL (X))
(ALL X NORMAL (X) EQV NOT KNIGHT (X) AND NOT KNAVE (X))
(ALL X,Y MARRIED (X Y) IMPL KNIGHT (X) AND KNAVE (Y) OR KNAVE (X) AND KNIGHT (Y) OR NORMAL (X)
AND NORMAL (Y))
(ALL
X,Y NORMAL2 (X Y) AND KNAVE (X) IMPL NOT NORMAL (Y))
(ALL X,Y NORMAL2 (X Y) AND KNIGHT (X) IMPL NORMAL (Y))
(MARRIED
(MRSABNORMAL MRABNORMAL))
(NORMAL2 (MRABNORMAL MRSABNORMAL))
(NORMAL2 (MRSABNORMAL MRABNORMAL))

(NORMAL (MRABNORMAL) AND NORMAL (MRSABNORMAL))