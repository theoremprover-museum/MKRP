;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PROBLEM 54)
(*)
(* ON ANOTHER
OCCASSION THEY MADE THE FOLLOWING STATEMENTS.)
(*)
(* FIRST ONE / I LIE ON
SATURDAYS.I LIE ON SUNDAYS.)
(* SECOND ONE / I WILL LIE TOMORROW.)
(*)
(* WHAT DAY OF THE WEEK WAS IT?)
(*)
(*)
(* FORMALIZING
THE PROBLEM IN FIRST ORDER LOGIC)
(*)
(TYPE FIRSTONE,SECONDONE:PERSON)
(TYPE LION,UNICORN:ANIMAL)
(* LIKE (X Y) MEANS : THE LYING-BEHAVIOR OF X IS LIKE THE
ANIMAL Y.)
(ALL X:PERSON LIKE (X LION) OR LIKE (X UNICORN))
(ALL X:PERSON NOT (LIKE (X LION) AND LIKE (X UNICORN)))
(*)
(ALL X:DAYS MO (X) EQV
(NOT TU (X) AND NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TU (X) EQV (NOT
MO (X) AND NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS WE (X) EQV (NOT
MO (X) AND NOT TU (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TH (X) EQV (NOT
MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT FR (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS FR (X) EQV (NOT
MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT TH (X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS SA (X) EQV (NOT
MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SU (X)))
(ALL X:DAYS SU (X) EQV (NOT
MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT TH (X) AND NOT FR (X) AND NOT SA (X)))
(*)
(TYPE
TOMORROW (DAYS) :DAYS)
(ALL X:DAYS MO (TOMORROW (X)) EQV SU (X))
(ALL X:DAYS TU (TOMORROW (X)) EQV MO (X))
(ALL X:DAYS WE (TOMORROW (X)) EQV TU (X))
(ALL X:DAYS TH (TOMORROW (X)) EQV WE (X))
(ALL X:DAYS FR (TOMORROW (X)) EQV TH (X))
(ALL X:DAYS SA (TOMORROW (X)) EQV FR (X))
(ALL X:DAYS SU (TOMORROW (X)) EQV SA (X))
(*)
(* THE PREDICATE LA
HAS THE FOLLOWING MEANING:)
(* LA (X Y Z) IS TRUE, IF IT IS POSSIBLE FOR X ,TO STATE AT THE DAY
Y , THAT HE LIES AT DAY Z.)
(*)
(*)
(ALL T:PERSON ALL X,Y:DAYS
NOT MEMBER (X LYING-DAYS (T)) AND NOT MEMBER (Y LYING-DAYS (T)) IMPL NOT LA (T X Y))
(ALL T:PERSON ALL X,Y:DAYS NOT MEMBER (X LYING-DAYS (T)) AND MEMBER (Y LYING-DAYS (T)) IMPL LA
(T X Y))
(ALL T:PERSON ALL X,Y:DAYS MEMBER (X LYING-DAYS (T)) AND NOT MEMBER
(Y LYING-DAYS (T)) IMPL LA (T X Y))
(ALL T:PERSON ALL X,Y:DAYS MEMBER (X LYING-DAYS
(T)) AND MEMBER (Y LYING-DAYS (T)) IMPL NOT LA (T X Y))
(*)
(ALL X:DAYS MEMBER (X LYING-DAYS (LION)) EQV (MO (X) OR TU (X) OR WE (X)))
(ALL X:DAYS MEMBER (X LYING-DAYS
(UNICORN)) EQV (TH (X) OR FR (X) OR SA (X)))
(*)
(ALL X:PERSON ALL Y:ANIMAL LIKE
(X Y) IMPL (ALL Z:DAYS (MEMBER (Z LYING-DAYS (Y)) EQV MEMBER (Z LYING-DAYS (X)))))
(*)
(TYPE MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRYDAY,SATURDAY,SUNDAY: DAYS)
(MO (MONDAY))
(TU (TUESDAY))
(WE (WEDNESDAY))
(TH (THURSDAY))
(FR (FRYDAY))
(SA (SATURDAY))
(SU (SUNDAY))

((LIKE (FIRSTONE UNICORN) AND LIKE (SECONDONE LION) OR LIKE (FIRSTONE
LION) AND LIKE (SECONDONE UNICORN)) IMPL (EX X:DAYS LA (FIRSTONE X SATURDAY) AND LA (FIRSTONE X SUNDAY) AND LA
(SECONDONE X TOMORROW (X))))