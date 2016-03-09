;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PROBLEM 51)
(*)
(* IN THE FOREST
THERE ARE OFTEN TWO VISITORS:TWEEDLEDUM AND TWEEDLEDEE.)
(* ONE OF THE TWO IS LIKE THE LION ,THE
OTHER ONE IS LIKE THE UNICORN.)
(* ALICE DID NOT KNOW WHICH ONE WAS LIKE THE LION AND WHICH ONE
WAS LIKE)
(* THE UNICORN.THEY LOOKED LIKE TWINS AND SO ALICE COULD NOT DETERMINE)
(* WHO WAS WHO.)
(*)
(* ONE DAY ALICE MET THE BROTHERS.)
(* THEY MADE THE FOLLOWING STATEMENTS:)
(*)
(* FIRST ONE / I AM TWEEDLEDUM.)
(* SECOND ONE / I AM TWEEDLEDEE.)
(*)
(* WHICH ONE WAS TWEEDLEDEE
AND WHICH ONE TWEEDLEDUM.)
(*)
(*)
(* FORMALIZING THE PROBLEM
IN FIRST ORDER LOGIC)
(*)
(TYPE TWEEDLEDEE,TWEEDLEDUM:PERSONS)
(TYPE LION,UNICORN:ANIMAL)
(*)
(ALL X:DAYS MO (X) EQV (NOT TU (X) AND NOT WE
(X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TU (X) EQV (NOT MO (X) AND NOT WE
(X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS WE (X) EQV (NOT MO (X) AND NOT TU
(X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TH (X) EQV (NOT MO (X) AND NOT TU
(X) AND NOT WE (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS FR (X) EQV (NOT MO (X) AND NOT TU
(X) AND NOT WE (X) AND NOT TH (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS SA (X) EQV (NOT MO (X) AND NOT TU
(X) AND NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SU (X)))
(ALL X:DAYS SU (X) EQV (NOT MO (X) AND NOT TU
(X) AND NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X)))
(*)
(ALL X:DAYS MEMBER (X
LYING-DAYS (LION)) EQV (MO (X) OR TU (X) OR WE (X)))
(ALL X:DAYS MEMBER (X LYING-DAYS (UNICORN)) EQV
(TH (X) OR FR (X) OR SA (X)))
(*)
(TYPE 
MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY,SATURDAY,SUNDAY:DAYS)
(MO (MONDAY))
(TU (TUESDAY))
(WE (WEDNESDAY))
(TH (THURSDAY))
(FR (FRIDAY))
(SA (SATURDAY))
(SU (SUNDAY))
(*)
(DEE
(TWEEDLEDEE))
(DUM (TWEEDLEDUM))
(ALL P:PERSONS
DEE (P) EQV NOT DUM (P))
(*)
(* TRANS (X Y) IS TRUE, IF PERSON X ACTS LIKE ANIMAL Y)
(ALL P:PERSONS TRANS (P LION)
EQV NOT TRANS (P UNICORN))
(ALL
P:PERSONS TRANS (P LION) OR TRANS (P UNICORN))
(ALL A:ANIMAL TRANS (TWEEDLEDEE A) EQV NOT TRANS (TWEEDLEDUM A))
(*)
(ALL X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS NOT MEMBER (X
LYING-DAYS (A)) AND TRANS (P A) AND DEE (Q) IMPL YOUDEE (X P Q))
(ALL X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS NOT MEMBER (X LYING-DAYS (A)) AND TRANS (P A) AND DUM
(Q) IMPL NOT YOUDEE (X P Q))
(ALL X:DAYS ALL
A:ANIMAL ALL P,Q:PERSONS MEMBER (X LYING-DAYS (A)) AND TRANS (P A) AND DEE (Q) IMPL NOT YOUDEE (X P Q))
(ALL X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS MEMBER (X LYING-DAYS
(A)) AND TRANS (P A) AND DUM (Q) IMPL YOUDEE (X P Q))
(ALL
X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS MEMBER (X LYING-DAYS (A)) AND TRANS (P A) AND DUM (Q) IMPL YOUDUM (X P Q))
(ALL X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS MEMBER (X LYING-DAYS
(A)) AND TRANS (P A) AND DEE (Q) IMPL NOT YOUDUM (X P Q))
(ALL X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS NOT MEMBER (X LYING-DAYS (A)) AND TRANS (P A) AND DEE (Q) IMPL NOT YOUDUM
(X P Q))
(ALL X:DAYS ALL A:ANIMAL ALL P,Q:PERSONS
NOT MEMBER (X LYING-DAYS (A)) AND TRANS (P A) AND DUM (Q) IMPL YOUDUM (X P Q))

(EX X:DAYS EX FIRST,SECOND:PERSONS YOUDEE (X FIRST FIRST) AND YOUDUM
(X SECOND SECOND) AND DUM (FIRST) AND DEE (SECOND))