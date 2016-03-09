;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* PROBLEM E89)
(*)
(* AGAIN, EACH OF A,B,C IS A KNIGHT OR A KNAVE AND AXACTLY ONE OF THEM)
(* IS A WEREWOLF. THEY MAKE THE FOLLOWING STATEMENTS:)
(* A: I AM A WEREWOLF)
(* B: I AM A WEREWOLF)
(* C: AT MOST ONE OF US IS A KNIGHT)
(* GIVE A COMPLETE CLASSIFICATION OF A,B, AND C.)
(*)
(*)
(*)
(* WE DEFINE THE FOLLOWING FUNCTION:)
(* STATE (X) MAPS AN ELEMENT OF THE SORT PERSON TO AN ELEMENT OF THE)
(* SORT STATEMENT. IT MEANS THAT PERSON X MADE STATEMENT STATE (X))
(*)
(* WE DEFINE THE FOLLOWING PREDICATES:)
(* TRUTH (X) - STATEMENT X IS TRUE)
(* KNIGHT (X) - PERSON X IS A KNIGHT)
(* KNAVE (X) - PERSON X IS A KNAVE)
(* WEREWOLF
(X) - PERSON X IS A WEREWOLF)
(* STATE.WEREWOLF (Y X) - STATEMENT X TELLS THAT PERSON Y IS A WEREWOLF)
(* AT-MOST-ONE KNIGHT (X Y Z) - AT MOST ONE PERSON X,Y OR Z IS A KNIGHT)
(* 
STATE.AT-MOST-ONE-KNIGHT (S X Y Z) - STATEMENT S TELLS THAT AT MOST ONE)
(* PERSON X,Y OR Z IS A KNIGHT)
(*)
(*)
(*)
(* DECLARATIONS:)
(TYPE
A,B,C : PERSON)
(TYPE STATE (PERSON) :STATEMENT)
(TYPE KNIGHT (PERSON))
(TYPE KNAVE (PERSON))
(TYPE WEREWOLF (PERSON))
(TYPE TRUTH (STATEMENT))
(TYPE STATE.WEREWOLF (STATEMENT PERSON))
(TYPE AT-MOST-ONE-KNIGHT (PERSON PERSON PERSON))
(TYPE STATE.AT-MOST-ONE-KNIGHT (STATEMENT PERSON PERSON PERSON))
(*)
(* EVERY INHABITANT
IS EITHER A KNIGHT OR A KNAVE:)
(ALL X:PERSON KNIGHT (X) OR KNAVE (X))
(ALL X:PERSON NOT (KNIGHT (X) AND KNAVE (X)))
(*)
(* KNIGHT ALWAYS TELL THE TRUTH AND KNAVE ALWAYS LIE:)
(ALL X:PERSON KNIGHT (X) EQV TRUTH (STATE (X)))
(ALL X:PERSON KNAVE
(X) EQV NOT (TRUTH (STATE (X))))
(*)
(* A WEREWOLF CAN BE A KNIGHT OR A KNAVE:)
(ALL X:PERSON WEREWOLF (X) IMPL KNIGHT (X) OR KNAVE (X))
(*)
(* THERE IS EXACTLY ONE WEREWOLF:)
(WEREWOLF (A) OR WEREWOLF (B) OR WEREWOLF (C))
(NOT (WEREWOLF (A) AND WEREWOLF (B) OR WEREWOLF (B) AND WEREWOLF (C) OR WEREWOLF (C) AND WEREWOLF (A)))
(*)
(* EXPLANATION OF AT-MOST-ONE-KNIGHT:)
(ALL X,Y,Z:PERSON AT-MOST-ONE-KNIGHT (X Y Z) EQV NOT (KNIGHT (X) AND KNIGHT (Y) OR KNIGHT (Y) AND KNIGHT (Z) OR KNIGHT (Z) AND KNIGHT (X)))
(*)
(* IMPLICATIONS ABOUT STATEMENTS:)
(ALL X:STATEMENT ALL Y:PERSON STATE.WEREWOLF (X Y) AND TRUTH (X) IMPL WEREWOLF (Y))
(ALL X:STATEMENT ALL Y:PERSON STATE.WEREWOLF
(X Y) AND NOT (TRUTH (X)) IMPL NOT (WEREWOLF (Y)))
(ALL S:STATEMENT ALL X,Y,Z:PERSON STATE.AT-MOST-ONE-KNIGHT (S X Y Z) AND TRUTH (S)
IMPL AT-MOST-ONE-KNIGHT (X Y Z))
(ALL S:STATEMENT ALL X,Y,Z:PERSON STATE.AT-MOST-ONE-KNIGHT (S X Y Z) AND NOT (TRUTH
(S)) IMPL NOT (AT-MOST-ONE-KNIGHT (X Y Z)))
(*)
(* ACTUAL STATEMENTS:)
(STATE.WEREWOLF (STATE (A) A))
(STATE.WEREWOLF (STATE (B) B))
(STATE.AT-MOST-ONE-KNIGHT (STATE (C) A B C))

(* THEOREMS FOR E89:)
(* WE TRY ONLY THE CORRECT CLASSIFICATIONS)
(*)
(KNAVE (A) AND KNAVE (B) AND KNIGHT (C) AND WEREWOLF (C))