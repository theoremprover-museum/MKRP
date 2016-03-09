;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PROBLEM 50)
(*)
(* ONE DAY ALICE
MET THE LION.)
(* HE MADE THE FOLLOWING STATEMENT:)
(*)
(*
I LIED YESTERDAY AND)
(* I WILL LIE AGAIN TOMORROW.)
(*)
(*
ON WHAT DAYS OF THE WEEK IS THIS POSSIBLE?)
(*)
(*)
(* FORMALIZING
THE PROBLEM IN FIRST ORDER LOGIC)
(*)
(ALL X:DAYS MO (X) EQV (NOT TU (X) AND
NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TU (X) EQV (NOT MO (X) AND
NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
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
(TYPE YESTERDAY (DAYS) :DAYS)
(ALL X:DAYS MO (YESTERDAY (X)) EQV TU (X))
(ALL X:DAYS TU (YESTERDAY (X)) EQV WE (X))
(ALL X:DAYS WE (YESTERDAY (X)) EQV TH (X))
(ALL X:DAYS TH (YESTERDAY (X)) EQV FR (X))
(ALL X:DAYS FR (YESTERDAY (X)) EQV SA (X))
(ALL X:DAYS SA (YESTERDAY (X)) EQV SU (X))
(ALL X:DAYS SU (YESTERDAY (X)) EQV MO (X))
(*)
(TYPE TOMORROW (DAYS) :DAYS)
(ALL X:DAYS MO (TOMORROW (X)) EQV SU (X))
(ALL X:DAYS TU (TOMORROW (X)) EQV MO (X))
(ALL X:DAYS WE (TOMORROW (X)) EQV TU (X))
(ALL X:DAYS TH (TOMORROW (X)) EQV WE (X))
(ALL X:DAYS FR (TOMORROW (X)) EQV TH (X))
(ALL X:DAYS SA (TOMORROW (X)) EQV FR (X))
(ALL X:DAYS SU (TOMORROW (X)) EQV SA (X))
(*)
(* THE PREDICATE LA2 HAS THE FOLLOWING MEANING:)
(* LA(X Y Z U) MEANS THAT X COULD SAY AT THE DAY Y :I LIE AT DAY Z AND U.)
(*)
(TYPE LA2 (ANIMAL DAYS DAYS DAYS))
(TYPE LION:ANIMAL)

(ALL X:DAYS MEMBER(X LYING.DAYS(LION)) EQV (MO(X) OR TU(X) OR WE(X)))
(*)
(ALL T:ANIMAL ALL X,Y,U:DAYS
     NOT MEMBER (X LYING.DAYS (T))
     AND MEMBER (Y LYING.DAYS (T))
     AND MEMBER (U LYING.DAYS (T))
     IMPL LA2 (T X Y U))
(ALL T:ANIMAL ALL X,Y,U:DAYS
     MEMBER (X LYING.DAYS (T))
     AND MEMBER (Y LYING.DAYS (T))
     AND MEMBER (U LYING.DAYS (T))
     IMPL NOT LA2 (T X Y U))
(ALL T:ANIMAL ALL X,Y,U:DAYS
     NOT MEMBER (X LYING.DAYS (T))
     AND NOT (MEMBER (Y LYING.DAYS (T))
		     AND MEMBER (U LYING.DAYS (T)))
     IMPL NOT LA2 (T X Y U))
(ALL T:ANIMAL ALL X,Y,U:DAYS
     MEMBER (X LYING.DAYS (T))
     AND NOT (MEMBER (Y LYING.DAYS (T))
		     AND MEMBER (U LYING.DAYS (T)))
     IMPL LA2 (T X Y U))
(*)
(*)
(TYPE MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY,SATURDAY,SUNDAY:DAYS)
(MO (MONDAY))
(TU (TUESDAY))
(WE (WEDNESDAY))
(TH (THURSDAY))
(FR (FRIDAY))
(SA (SATURDAY))
(SU (SUNDAY))
(*)

(EX X : DAYS LA2 (LION X YESTERDAY (X) TOMORROW (X)))