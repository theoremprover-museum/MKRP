;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PROBLEM E90.)
(*)
(* IN THIS AND THE NEXT TWO PROBLEMS THERE ARE AGAIN THREE INHABITANTS)
(* A,B,C, EACH OF WHOM IS EITHER A KNIGHT OR A KNAVE. HOWEVER ONLY TWO)
(*
OF THEM, A, B, MAKE STATEMENTS. BUT IN THESE STATEMENTS, THE WORD US)
(* REFERS TO THE THREE PEOPLE A,B,C - NOT TO JUST A AND B. SUPPOSE A,B MAKE)
(* THE FOLLOWING STATEMENTS:)
(* A: AT LEAST
ONE OF THE THREE OF US IS A KNIGHT.)
(* B: AT LEAST ONE OF THE THREE OF US IS A KNAVE.)
(* GIVEN THAT AT LEAST ONE OF THEM IS A WEREWOLF, AND THAT NONE OF THEM)
(* IS BOTH A KNIGHT AND A WEREWOLF,
WHICH ONE ARE WEREWOLVES?)
(*)
(* WE DEFINE THE FOLLOWING PREDICATES:)
(*)
(* KNIGHT (X) ---MEANS THAT X IS A KNIGHT.)
(* KNAVE (X) ---MEANS THAT X IS A
KNAVE.)
(* WEREWOLF (X) ---MEANS THAT X IS A WEREWOLF.)
(* TRUTH (X) --- MEANS THAT THE STATEMENT X IS TRUE.)
(* AT.LEAST.ONE.KNIGHT (X Y Z) ---MEANS THAT AT LEAST ONE OF X,Y,Z IS A KNIGHT.)
(* AT.LEAST.ONE.KNAVE (X Y Z) ---MEANS THAT AT LEAST ONE OF X,Y,Z IS A KNAVE.)
(* STATE.AT.LEAST.ONE.KNIGHT (U V W X) ---THE STATEMENT U MEANS THAT AT LEAST ONE OF V,W,X IS A KNIGHT.)
(*)
(* STATE.AT.LEAST.ONE.KNAVE (U V W X) ---THE STATEMENT U MEANS THAT AT LEAST)
(* ONE OF V,W,X IS A KNAVE.)
(*)
(* WE DEFINE THE FOLLOWING FUNCTION:)
(*)
(* STATE (X) ---ASSIGNS EACH PERSON A STATEMENT.)
(*)
(TYPE A,B,C:PERSON)
(TYPE KNAVE (PERSON))
(TYPE KNIGHT (PERSON))
(TYPE WEREWOLF (PERSON))
(TYPE TRUTH (STATEMENT))
(TYPE AT.LEAST.ONE.KNIGHT (PERSON PERSON PERSON))
(TYPE AT.LEAST.ONE.KNAVE (PERSON PERSON PERSON))
(TYPE STATE.AT.LEAST.ONE.KNIGHT (STATEMENT PERSON PERSON
PERSON))
(TYPE STATE.AT.LEAST.ONE.KNAVE (STATEMENT PERSON PERSON PERSON))
(TYPE STATE (PERSON) :STATEMENT)
(*)
(* EVERY INHABITANT IS EITHER A KNIGHT OR A KNAVE:)
(ALL X:PERSON KNIGHT (X) OR KNAVE (X))
(ALL X:PERSON NOT (KNIGHT (X) AND KNAVE (X)))
(*)
(* KNIGHTS ALWAYS TELL THE TRUTH AND KNAVES ALWAYS LIE:)
(ALL X:PERSON KNIGHT (X) EQV TRUTH (STATE (X)))
(ALL X:PERSON KNAVE (X) EQV NOT TRUTH (STATE
(X)))
(*)
(* AT LEAST ONE OF THEM IS A WEREWOLF:)
(WEREWOLF (A) OR WEREWOLF (B) OR WEREWOLF (C))
(*)
(* NONE OF THEM IS BOTH A KNIGHT AND A WEREWOLF:)
(ALL X:PERSON NOT (KNIGHT (X) AND WEREWOLF (X)))
(*)
(* EQUIVALENCES:)
(ALL X,Y,Z:PERSON AT.LEAST.ONE.KNIGHT (X Y Z) EQV KNIGHT (X) OR KNIGHT (Y) OR KNIGHT (Z))
(ALL X,Y,Z:PERSON AT.LEAST.ONE.KNAVE (X Y Z) EQV KNAVE (X) OR KNAVE (Y) OR KNAVE (Z))
(*)
(* IMPLICATIONS ABOUT STATEMENTS:)
(ALL U:STATEMENT ALL V,W,X:PERSON STATE.AT.LEAST.ONE.KNIGHT (U V W X) AND TRUTH (U) IMPL AT.LEAST.ONE.KNIGHT (V W X))
(ALL U:STATEMENT ALL V,W,X:PERSON STATE.AT.LEAST.ONE.KNIGHT (U V W X) AND NOT TRUTH (U) IMPL NOT AT.LEAST.ONE.KNIGHT (V W X))
(ALL U:STATEMENT ALL V,W,X:PERSON STATE.AT.LEAST.ONE.KNAVE (U V W X) AND TRUTH (U) IMPL AT.LEAST.ONE.KNAVE
(V W X))
(ALL U:STATEMENT ALL V,W,X:PERSON STATE.AT.LEAST.ONE.KNAVE (U V W X) AND NOT TRUTH (U) IMPL
NOT AT.LEAST.ONE.KNAVE (V W X))
(*)
(* ACTUAL STATEMENTS:)
(STATE.AT.LEAST.ONE.KNIGHT (STATE (A) A B C))
(STATE.AT.LEAST.ONE.KNAVE (STATE (B) A B C))

(WEREWOLF (C))