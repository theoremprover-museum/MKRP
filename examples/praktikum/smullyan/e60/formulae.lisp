;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PROBLEM 60)
(*)
(* ONE DAY ALICE MET ONE OF THE BROTHERS.)
(* SHE ASKED HIM:)
(* WHO REALLY OWNS THE RATTLE?)
(* HE ANSWERED:THE TRUE OWNER OF THE RATTLE IS LYING TODAY.)
(*)
(* WHAT ARE THE CHANCES THAT THE SPEAKER OWNS THE RATTLE?)
(*)
(* FORMALIZING
THE PROBLEM IN FIRST ORDER LOGIC)
(*)
(TYPE FIRSTONE,SECONDONE :PERSON)
(*)
(ALL X:DAYS MO (X) EQV (NOT TU (X) AND NOT WE (X) AND NOT TH (X) AND NOT FR
(X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TU (X) EQV (NOT MO (X) AND NOT WE (X) AND NOT TH (X) AND NOT FR
(X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS WE (X) EQV (NOT MO (X) AND NOT TU (X) AND NOT TH (X) AND NOT FR
(X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS TH (X) EQV (NOT MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT FR
(X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS FR (X) EQV (NOT MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT TH
(X) AND NOT SA (X) AND NOT SU (X)))
(ALL X:DAYS SA (X) EQV (NOT MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT TH
(X) AND NOT FR (X) AND NOT SU (X)))
(ALL X:DAYS SU (X) EQV (NOT MO (X) AND NOT TU (X) AND NOT WE (X) AND NOT TH
(X) AND NOT FR (X) AND NOT SA (X)))
(*)
(ALL Y:PERSON TWEEDLEDUM (Y) IMPL (ALL X:DAYS MEMBER
(X LYING-DAYS (Y)) EQV (MO (X) OR TU (X) OR WE (X))))
(ALL Y:PERSON
TWEEDLEDEE (Y) IMPL (ALL X:DAYS MEMBER (X LYING-DAYS (Y)) EQV (TH (X) OR FR (X) OR SA (X))))
(*)
(*)
(*)
(TWEEDLEDEE (FIRSTONE)
AND TWEEDLEDUM (SECONDONE) OR TWEEDLEDUM (FIRSTONE) AND TWEEDLEDEE (SECONDONE))
(*)
(* RATTLE-OWNER (X)
MEANS: X OWNS THE RATTLE.)
(*)
(RATTLE-OWNER (FIRSTONE) OR RATTLE-OWNER (SECONDONE))
(NOT (RATTLE-OWNER (FIRSTONE) AND RATTLE-OWNER (SECONDONE)))
(*)
(* OWNER-LIES-TODAY (X Y)
MEANS X COULD STATE AT DAY Y: THE TRUE OWNER OF THE RATTLE LIES TODAY.)
(*)
(ALL X:PERSON ALL Y:DAYS NOT MEMBER (Y LYING-DAYS (X)) AND NOT (RATTLE-OWNER (FIRSTONE) AND MEMBER (Y LYING-DAYS
(FIRSTONE)) OR RATTLE-OWNER (SECONDONE) AND MEMBER (Y LYING-DAYS (SECONDONE))) IMPL NOT OWNER-LIES-TODAY (X Y))
(ALL X:PERSON ALL Y:DAYS NOT MEMBER (Y LYING-DAYS (X)) AND (RATTLE-OWNER (FIRSTONE) AND MEMBER (Y LYING-DAYS (FIRSTONE))
OR RATTLE-OWNER (SECONDONE) AND MEMBER (Y LYING-DAYS (SECONDONE))) IMPL OWNER-LIES-TODAY (X Y))
(ALL X:PERSON ALL Y:DAYS MEMBER
(Y LYING-DAYS (X)) AND NOT (RATTLE-OWNER (FIRSTONE) AND MEMBER (Y LYING-DAYS (FIRSTONE)) OR RATTLE-OWNER (SECONDONE)
AND MEMBER (Y LYING-DAYS (SECONDONE))) IMPL OWNER-LIES-TODAY (X Y))
(ALL X:PERSON ALL Y:DAYS MEMBER (Y LYING-DAYS (X)) AND
(RATTLE-OWNER (FIRSTONE) AND MEMBER (Y LYING-DAYS (FIRSTONE)) OR RATTLE-OWNER (SECONDONE) AND MEMBER (Y LYING-DAYS
(SECONDONE))) IMPL NOT OWNER-LIES-TODAY (X Y))
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
(TYPE
TODAY:DAYS)
(OWNER-LIES-TODAY (FIRSTONE TODAY))

(NOT RATTLE-OWNER (FIRSTONE))