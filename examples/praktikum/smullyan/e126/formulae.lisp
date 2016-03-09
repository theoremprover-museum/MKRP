;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* SMULLYAN, CHAPTER 8, PROBLEM 126)
(* SUPPOSE THERE ARE TWO NEIGHBORING ISLANDS EACH EXCLUSIVELY)
(* INHABITED BY KNIGHTS AND KNAVES, THERE ARE NO NORMALS.)
(ALL X KNIGHT (X) EQV NOT KNAVE (X))
(ALL X,Y KNIGHT (X) AND STAT (X Y) IMPL T (Y))
(ALL X,Y KNAVE (X) AND STAT (X Y) IMPL NOT T (Y))
(* YOU ARE TOLD THAT ON ONE OF THE TWO ISLANDS THERE IS AN EVEN)
(* NUMBER OF KNIGHTS AND ON THE OTHER ONE THERE IS AN ODD NUMBER)
(* OF KNIGHTS.)
(ALL X EVEN (X) EQV NOT ODD (X))
(* YOU ARE ALSO TOLD THAT THERE IS GOLD ON THE ISLAND CONTAINING)
(* THE EVEN NUMBER OF KNIGHTS, BUT THERE IS NO GOLD ON THE OTHER)
(* ISLAND. )
(EVEN (KNIGHTS) EQV THERE-IS-GOLD-ON-THE-ISLAND)
(EVEN (KNAVES) AND EVEN (NATIVES) IMPL EVEN (KNIGHTS))
(ODD (KNAVES) AND ODD (NATIVES) IMPL EVEN (KNIGHTS))
(* YOU PICK ONE OF THE TWO ISLANDS AT RANDOM AND VISIT IT.)
(* ALL THE INHABITANTS KNOW HOW MANY KNIGHTS AND HOW MANY KNAVES)
(* LIVE ON THE ISLAND. YOU ARE INTERVIEWING THREE INHABITANTS,)
(* A, B, C, AND THEY MAKE THE FOLLOWING STATEMENTS:)
(* A: THERE IS AN EVEN NUMBER OF KNAVES ON THIS ISLAND. )
(STAT (A EVENNUMBER (KNAVES)))
(* B: RIGHT NOW, THERE IS AN ODD NUMBER OF PEOPLE ON THE ISLAND.)
(STAT (B ODDNUMBER (PEOPLE)))
(* C: I AM A KNIGHT IF AND ONLY IF A AND B ARE OF THE SAME TYPE.)
(STAT (C KNIGHTIFFEQV))
(ALL X T (EVENNUMBER (X)) EQV EVEN (X))
(ALL X T (ODDNUMBER (X)) EQV ODD (X))
(T (KNIGHTIFFEQV) EQV KNIGHT (C) EQV (KNIGHT (A) EQV KNIGHT (B)))
(* ASSUME THAT YOU ARE NEITHER A KNIGHT NOR A KNAVE AND THAT AT)
(* THE MOMENT YOU ARE THE ONLY VISITOR ON THE ISLAND.)
(ODD (PEOPLE) EQV EVEN (NATIVES))


(* IS THERE GOLD ON THE ISLAND OR NOT?)
(NOT THERE-IS-GOLD-ON-THE-ISLAND)100
;(THERE-IS-GOLD-ON-THE-ISLAND)123