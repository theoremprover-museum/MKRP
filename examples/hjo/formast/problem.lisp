;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((ALL X0,XP0,XP1 NOT E (X0) OR NOT EXP (F (X0 XP0)) OR GT (F (X0 AP0) F (X0 XP1)) OR NOT EXP (F (X0 XP1))) (ALL X0,XP0 NOT E (X0) OR NOT EXP (F (X0 XP0)) OR S (F (X0 AP0))) (ALL X0,XP0 NOT E (X0) OR NOT EXP (F (X0 XP0)) OR INE (F (X0 AP0))) (ALL X0 EXP (F (X0 AP1)) OR E (X0)) (ALL X0,XP2,XP3 NOT S (F (X0 XP2)) OR NOT INE (F (X0 XP2)) OR GE (F (X0 AP2) F (X0 XP3)) OR NOT S (F (X0 XP3)) OR NOT INE (F (X0 XP3)) OR E (X)) (ALL X0,XP2 NOT S (F (X0 XP2)) OR NOT INE (F (X0 XP2)) OR EXP (F (X0 AP2)) OR E (X0)) (ALL X1,XP4 NOT S (X1) OR GT (F (X1 AP3) F (X1 XP4)) OR NOT INE (F (X1 XP4))) (ALL X1 NOT S (X1) OR E (F (X1 AP3))) (ALL X1 NOT S (X1) OR EXP (F (X1 AP3))) (ALL X1,XP5,XP6 NOT E (F (X1 XP5)) OR NOT EXP (F (X1 XP5)) OR GE (F (X1 AP4) F (X1 XP6)) OR NOT E (F (X1 XP6)) OR NOT EXP (F (X1 XP6)) OR S (X1)) (ALL X1,XP5 NOT E (F (X1 XP5)) OR NOT EXP (F (X1 XP5)) OR INE (F (X1 AP4)) OR S (X1)) (ALL X2 NOT S (X2) OR NOT BS (X2) OR OS (F (X2 AF0))) (ALL X3 NOT OS (X3) OR S (F (X3 AP5))) (ALL X3 NOT OS (X3) OR BS (F (X3 AP5))) (ALL X4 NOT S (X4) OR NOT INI (X4) OR OSA (F (X4 AF1))) (ALL X5 NOT OSA (X5) OR S (F (X5 AP6))) (ALL X5 NOT OSA (X5) OR INI (F (X5 AP6))) (ALL X6 NOT S (X6) OR NOT INE (X6) OR R (X6)) (ALL X7 NOT BC (X7) OR OSE (F (X7 AF2))) (ALL X8 NOT OSE (X8) OR BC (F (X8 AP7))) (ALL X9 BS (X9) EQV PS (X9)) (ALL X10 BC (X10) EQV PC (X10)))) (QUOTE ((ALL 5 (ALL 4 (ALL 3 (OR (NOT (+ 6 (3) NIL)) (OR (NOT (+ 8 ((7 3 4)) NIL)) (OR (+ 10 ((7 3 9) (7 3 5)) NIL) (NOT (+ 8 ((7 3 5)) NIL)))))))) (ALL 12 (ALL 11 (OR (NOT (+ 6 (11) NIL)) (OR (NOT (+ 8 ((7 11 12)) NIL)) (+ 13 ((7 11 9)) NIL))))) (ALL 15 (ALL 14 (OR (NOT (+ 6 (14) NIL)) (OR (NOT (+ 8 ((7 14 15)) NIL)) (+ 16 ((7 14 9)) NIL))))) (ALL 17 (OR (+ 8 ((7 17 18)) NIL) (+ 6 (17) NIL))) (ALL 21 (ALL 20 (ALL 19 (OR (NOT (+ 13 ((7 19 20)) NIL)) (OR (NOT (+ 16 ((7 19 20)) NIL)) (OR (+ 23 ((7 19 22) (7 19 21)) NIL) (OR (NOT (+ 13 ((7 19 21)) NIL)) (OR (NOT (+ 16 ((7 19 21)) NIL)) (+ 6 (24) NIL))))))))) (ALL 26 (ALL 25 (OR (NOT (+ 13 ((7 25 26)) NIL)) (OR (NOT (+ 16 ((7 25 26)) NIL)) (OR (+ 8 ((7 25 22)) NIL) (+ 6 (25) NIL)))))) (ALL 28 (ALL 27 (OR (NOT (+ 13 (27) NIL)) (OR (+ 10 ((7 27 29) (7 27 28)) NIL) (NOT (+ 16 ((7 27 28)) NIL)))))) (ALL 30 (OR (NOT (+ 13 (30) NIL)) (+ 6 ((7 30 29)) NIL))) (ALL 31 (OR (NOT (+ 13 (31) NIL)) (+ 8 ((7 31 29)) NIL))) (ALL 34 (ALL 33 (ALL 32 (OR (NOT (+ 6 ((7 32 33)) NIL)) (OR (NOT (+ 8 ((7 32 33)) NIL)) (OR (+ 23 ((7 32 35) (7 32 34)) NIL) (OR (NOT (+ 6 ((7 32 34)) NIL)) (OR (NOT (+ 8 ((7 32 34)) NIL)) (+ 13 (32) NIL))))))))) (ALL 37 (ALL 36 (OR (NOT (+ 6 ((7 36 37)) NIL)) (OR (NOT (+ 8 ((7 36 37)) NIL)) (OR (+ 16 ((7 36 35)) NIL) (+ 13 (36) NIL)))))) (ALL 38 (OR (NOT (+ 13 (38) NIL)) (OR (NOT (+ 39 (38) NIL)) (+ 41 ((7 38 40)) NIL)))) (ALL 42 (OR (NOT (+ 41 (42) NIL)) (+ 13 ((7 42 43)) NIL))) (ALL 44 (OR (NOT (+ 41 (44) NIL)) (+ 39 ((7 44 43)) NIL))) (ALL 45 (OR (NOT (+ 13 (45) NIL)) (OR (NOT (+ 46 (45) NIL)) (+ 48 ((7 45 47)) NIL)))) (ALL 49 (OR (NOT (+ 48 (49) NIL)) (+ 13 ((7 49 50)) NIL))) (ALL 51 (OR (NOT (+ 48 (51) NIL)) (+ 46 ((7 51 50)) NIL))) (ALL 52 (OR (NOT (+ 13 (52) NIL)) (OR (NOT (+ 16 (52) NIL)) (+ 53 (52) NIL)))) (ALL 54 (OR (NOT (+ 55 (54) NIL)) (+ 57 ((7 54 56)) NIL))) (ALL 58 (OR (NOT (+ 57 (58) NIL)) (+ 55 ((7 58 59)) NIL))) (ALL 60 (EQV (+ 39 (60) (KIND (EQV T 220))) (+ 61 (60) (KIND (EQV NIL 220))))) (ALL 62 (EQV (+ 55 (62) (KIND (EQV T 230))) (+ 63 (62) (KIND (EQV NIL 230))))))) (QUOTE ((NOT ((PC (A0) OR OSE (A1)) AND (ALL XF0 NOT OSE (F (A0 XF0)) OR OSE (A1)) AND (ALL XP7 PC (A0) OR NOT PC (F (A1 XP7))) AND (ALL XF0,XP7 NOT OSE (F (A0 XF0)) OR NOT PC (F (A1 XP7))))))) (QUOTE ((NOT (AND (OR (+ 63 (64) NIL) (+ 57 (65) NIL)) (AND (ALL 66 (OR (NOT (+ 57 ((7 64 66)) NIL)) (+ 57 (65) NIL))) (AND (ALL 67 (OR (+ 63 (64) NIL) (NOT (+ 63 ((7 65 67)) NIL)))) (ALL 69 (ALL 68 (OR (NOT (+ 57 ((7 64 68)) NIL)) (NOT (+ 63 ((7 65 69)) NIL))))))))))) (QUOTE ("Edit:     Axioms and Theorems edited: 22-MAR,1990 11:24 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(24465 24470 24475 24480 24485 24490 24505 24510 24525 24530 24535 24540 24555 24560
          24575 24580 24595 24600 24605 24610 24615 24630 24635 24650 24655 24660 24665 24670
          24685 24690 24705 24710 24715 24720 24725 24730 24735 24740 24745 24750 24755 24760
          24765 24770 24775 24780 24795 24800 24805 24810 24815 24820 24825 24840 24845 24850
          24865 24870 24875 24890 24895 24910 24924 24939 24944 24949 24954 24969 24984
          24999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" NIL NIL (ANY ANY)
          (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) NIL ((- 2 (POSITIVE . SYMMETRIC)))
          ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "E" NIL NIL (ANY) NIL NIL
          ((- 6 (POSITIVE))) ((+ 6 (NEGATIVE))) ((+ 6 (NIL))) ((- 6 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 14 "F" NIL ANY (ANY ANY) NIL NIL NIL NIL NIL 2 ANY
          (DT*ST-KIND NIL) FUNCTION 15 "EXP" NIL NIL (ANY) NIL NIL ((- 8 (POSITIVE)))
          ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5
          ANY "AP0" (DT*ST-KIND NIL) CONSTANT 15 "GT" NIL NIL (ANY ANY) NIL NIL
          ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL))) ((- 10 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "S" NIL NIL (ANY) NIL NIL
          ((- 13 (POSITIVE))) ((+ 13 (NEGATIVE))) ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "INE" NIL NIL (ANY) NIL NIL
          ((- 16 (POSITIVE))) ((+ 16 (NEGATIVE))) ((+ 16 (NIL))) ((- 16 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          "AP1" (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "AP2" (DT*ST-KIND NIL) CONSTANT 15
          "GE" NIL NIL (ANY ANY) NIL NIL ((- 23 (POSITIVE))) ((+ 23 (NEGATIVE))) ((+ 23 (NIL)))
          ((- 23 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "X" (DT*ST-KIND NIL)
          CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "AP3"
          (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "AP4"
          (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "BS" NIL NIL (ANY) NIL NIL
          ((- 39 (POSITIVE))) ((+ 39 (NEGATIVE))) ((+ 39 (NIL))) ((- 39 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY "AF0" (DT*ST-KIND NIL) CONSTANT 15 "OS" NIL NIL
          (ANY) NIL NIL ((- 41 (POSITIVE))) ((+ 41 (NEGATIVE))) ((+ 41 (NIL))) ((- 41 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY "AP5" (DT*ST-KIND NIL) CONSTANT 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "INI" NIL NIL (ANY) NIL NIL ((- 46 (POSITIVE))) ((+ 46 (NEGATIVE)))
          ((+ 46 (NIL))) ((- 46 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "AF1"
          (DT*ST-KIND NIL) CONSTANT 15 "OSA" NIL NIL (ANY) NIL NIL ((- 48 (POSITIVE)))
          ((+ 48 (NEGATIVE))) ((+ 48 (NIL))) ((- 48 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "AP6" (DT*ST-KIND NIL)
          CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "R" NIL NIL (ANY) NIL NIL
          ((- 53 (POSITIVE))) ((+ 53 (NEGATIVE))) ((+ 53 (NIL))) ((- 53 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15
          "BC" NIL NIL (ANY) NIL NIL ((- 55 (POSITIVE))) ((+ 55 (NEGATIVE))) ((+ 55 (NIL)))
          ((- 55 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "AF2" (DT*ST-KIND NIL)
          CONSTANT 15 "OSE" NIL NIL (ANY) NIL NIL ((- 57 (POSITIVE))) ((+ 57 (NEGATIVE)))
          ((+ 57 (NIL))) ((- 57 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "AP7" (DT*ST-KIND NIL) CONSTANT 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "PS" NIL NIL (ANY) NIL NIL
          ((- 61 (POSITIVE))) ((+ 61 (NEGATIVE))) ((+ 61 (NIL))) ((- 61 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15
          "PC" NIL NIL (ANY) NIL NIL ((- 63 (POSITIVE))) ((+ 63 (NEGATIVE))) ((+ 63 (NIL)))
          ((- 63 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "A0" (DT*ST-KIND NIL)
          CONSTANT 5 ANY "A1" (DT*ST-KIND NIL) CONSTANT 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 25000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 25000)
             (SETQ COUNTER1 70)
             (MAPC #'(LAMBDA (RADDR)
                       (SETF (AREF MEM*MEMORY (DECF COUNTER1))
                              (COND ((EQL RADDR 'ATP.MEMORY.NIL) RADDR)
                                    ((EQL 'END RADDR) RADDR)
                                    ((OR (EQL RADDR 0)
                                         (MINUSP RADDR))
                                     RADDR)
                                    (T (+ RADDR INCREMENT)))))
                   ADDRLIST)
             (SETQ COUNTER1 MEM*SIZE)
             (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            (T (UNLESS (= MEM*SIZE 25000)
                 (MEM-INITIALIZE 25000))
               (SETQ COUNTER1 70)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 25000) (+ 24460 INCREMENT) 24460))
      (SETQ MEM*NEXT.VADR 70
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (65 64 59 56 50 47 43 40 35 29 24 22 18 9))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (7))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (63 61 57 55 53 48 46 41 39 23 16 13 10 8 6 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (63 61 57 55 53 48 46 41 39 23 16 13 10 8 6 2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))