;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 RPC 415.0 Embedding Support 407.0 UX Support 416.0 Experimental Network RPC 415.0 Experimental NFS Client 415.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Remote Screen 418.1 KKL 20.1 HADES 16.0 Waltz 7.0 COLUMN 7.0 MKRP 49.3 GENTRAFO 1.0" "31-OCT,1991 22:40" 
   ("Edit:     Axioms and Theorems edited: 31-OCT,1991 22:36 "
   ))

(AXIOMS.INFIX    ((WOLF (LUPO))
   (FOX (FOXI))
   (BIRD (TWEETY))
   (CATERPILLAR (RAUPI))
   (SNAIL (SCHNECKI))
   (GRAIN (MUESLI))
   (ALL X (WOLF (X) OR FOX (X) OR BIRD (X) OR CATERPILLAR (X) OR SNAIL (X)) IMPL ANIMAL (X))
   (ALL X GRAIN (X) IMPL PLANT (X))
   (ALL X ANIMAL (X) IMPL (ALL Y PLANT (Y) IMPL EATS (X Y)) OR (ALL Y ANIMAL (Y) AND SMALLER (Y X) AND (EX Z PLANT (Z) AND EATS (Y Z)) IMPL EATS (X Y)))
   (ALL |X,Y| (CATERPILLAR (X) OR SNAIL (X)) AND BIRD (Y) IMPL SMALLER (X Y))
   (ALL |X,Y| BIRD (X) AND FOX (Y) IMPL SMALLER (X Y))
   (ALL |X,Y| FOX (X) AND WOLF (Y) IMPL SMALLER (X Y))
   (ALL |X,Y| (FOX (X) OR GRAIN (X)) AND WOLF (Y) IMPL NOT EATS (Y X))
   (ALL |X,Y| BIRD (X) AND CATERPILLAR (Y) IMPL EATS (X Y))
   (ALL |X,Y| BIRD (X) AND SNAIL (Y) IMPL NOT EATS (X Y))
   (ALL X (CATERPILLAR (X) OR SNAIL (X)) IMPL (EX Y PLANT (Y) AND EATS (X Y)))))

(THEOREMS.INFIX ((EX |X,Y| (ANIMAL (X) AND ANIMAL (Y) AND (EATS (X Y) AND (EX Z (GRAIN (Z) AND EATS (Y Z))))))))

(AXIOMS.PREFIX   ((+ 4 (3) NIL)
   (+ 6 (5) NIL)
   (+ 8 (7) NIL)
   (+ 10 (9) NIL)
   (+ 12 (11) NIL)
   (+ 14 (13) NIL)
   (ALL 15 (IMPL (OR (+ 4 (15) NIL) (OR (+ 6 (15) NIL) (OR (+ 8 (15) NIL) (OR (+ 10 (15) NIL) (+ 12 (15) NIL))))) (+ 16 (15) NIL)))
   (ALL 17 (IMPL (+ 14 (17) NIL) (+ 18 (17) NIL)))
   (ALL 19 (IMPL (+ 16 (19) NIL) (OR (ALL 20 (IMPL (+ 18 (20) NIL) (+ 21 (19 20) NIL))) (ALL 22 (IMPL (AND (+ 16 (22) NIL) (AND (+ 23 (22 19) NIL) (EX 24 (AND (+ 18 (24) NIL) (+ 21 (22 24) NIL))))) (+ 21 (19 22) NIL))))))
   (ALL 26 (ALL 25 (IMPL (AND (OR (+ 10 (25) NIL) (+ 12 (25) NIL)) (+ 8 (26) NIL)) (+ 23 (25 26) NIL))))
   (ALL 28 (ALL 27 (IMPL (AND (+ 8 (27) NIL) (+ 6 (28) NIL)) (+ 23 (27 28) NIL))))
   (ALL 30 (ALL 29 (IMPL (AND (+ 6 (29) NIL) (+ 4 (30) NIL)) (+ 23 (29 30) NIL))))
   (ALL 32 (ALL 31 (IMPL (AND (OR (+ 6 (31) NIL) (+ 14 (31) NIL)) (+ 4 (32) NIL)) (NOT (+ 21 (32 31) NIL)))))
   (ALL 34 (ALL 33 (IMPL (AND (+ 8 (33) NIL) (+ 10 (34) NIL)) (+ 21 (33 34) NIL))))
   (ALL 36 (ALL 35 (IMPL (AND (+ 8 (35) NIL) (+ 12 (36) NIL)) (NOT (+ 21 (35 36) NIL)))))
   (ALL 37 (IMPL (OR (+ 10 (37) NIL) (+ 12 (37) NIL)) (EX 38 (AND (+ 18 (38) NIL) (+ 21 (37 38) NIL)))))))

(THEOREMS.PREFIX ((EX 40 (EX 39 (AND (+ 16 (39) NIL) (AND (+ 16 (40) NIL) (AND (+ 21 (39 40) NIL) (EX 41 (AND (+ 14 (41) NIL) (+ 21 (40 41) NIL))))))))))

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.I_CLAUSE.PURITY . P)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
    (RED.I_CLAUSE.REPL.FACTORING . T)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.I_CLAUSE.REWRITING . DEM)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.D_CLAUSE.PURITY . P)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION.RECHECK)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_R.SELECTION * 10 (+ 2 VARIABLES (* 2 DEPTH) (* 3 NOLIT)))
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_FINITE.DOMAIN . T)
    (STR_SORT.LITERALS)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 10)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_P.SELECTION * WEIGHT (IF SUPPORT 3 1) (IF EQUATIONAL 1 2))
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ X (* 2 Y))) (* (X Y) (+ X (* X Y))) (- (X) (+ (* 2 X) -1)) (0 NIL 2) (1 NIL 2))
    (ER_NARROW.DEPTH . 0)
    (ER_NARROW.NEXT . DEPTH)
    (ER_NARROW.TEST NORM C DELTA SL N)
    (ER_COMPILE)
    (GEN_SPLITTING . 0)
    (GEN_PRESIMPLIFICATION . T)
    (GEN_MIN.EXPRESSION.LENGTH.FOR.FILE)
    (GEN_MIN.EXPRESSION.SIZE.FOR.FILE)
    (GEN_MANUAL.CONTROL)
    (GEN_MAXIMUM.STEPS)
    (GEN_GRAPH.SAVING)
    (GEN_SAVE.FILE . SAVEDEFAULT)
    (GEN_LISP.GARBAGE.COLLECTION)
    (GEN_COMMON.LISP)
    (TR_PREPROCESSING)
    (TR_STEP.MODE . LR)
    (TR_DUMP)
    (TR_CLAUSE.MODE . I)
    (TR_LINK.MODE . I)
    (TR_TRACE.FILE)
    (TR_TERMINAL . T)
    (PR_LATEX)
    (PR_INFIX.FORM . T)
    (PR_PREFIX.FORM)
    (PR_OPTIONS)
    (PR_AXIOM.CLAUSES . T)
    (PR_SYMBOLS)
    (PR_DIRECT.PROOF . T)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
    (PR_LITERALS)
   )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 67301229)
   (PARTIAL   (CLAUSE 55 R.= AXIOM ((53 . ANY)) ((+ 2 (53 53)) ))(CLAUSE 56 A1 AXIOM () ((+ 4 (3)) )))
   (PARTIAL   (CLAUSE 57 A2 AXIOM () ((+ 6 (5)) )))
   (PARTIAL   (CLAUSE 58 A3 AXIOM () ((+ 8 (7)) )))
   (PARTIAL   (CLAUSE 59 A4 AXIOM () ((+ 10 (9)) )))
   (PARTIAL   (CLAUSE 60 A5 AXIOM () ((+ 12 (11)) )))
   (PARTIAL   (CLAUSE 61 A6 AXIOM () ((+ 14 (13)) )))
   (PARTIAL   (CLAUSE 63 A7 AXIOM ((15 . ANY))
   ((- 4 (15)) (+ 16 (15)) )))
   (PARTIAL   (CLAUSE 66 A8 AXIOM ((42 . ANY))
   ((- 6 (42)) (+ 16 (42)) )))
   (PARTIAL   (CLAUSE 70 A9 AXIOM ((43 . ANY))
   ((- 8 (43)) (+ 16 (43)) )))
   (PARTIAL   (CLAUSE 75 A10 AXIOM ((44 . ANY))
   ((- 10 (44)) (+ 16 (44)) )))
   (PARTIAL   (CLAUSE 81 A11 AXIOM ((45 . ANY))
   ((- 12 (45)) (+ 16 (45)) )))
   (PARTIAL   (CLAUSE 88 A12 AXIOM ((17 . ANY))
   ((- 14 (17)) (+ 18 (17)) )))
   (PARTIAL   (CLAUSE 91 A13 AXIOM ((37 . ANY))
   ((- 10 (37)) (+ 18 ((50 37))) )))
   (PARTIAL   (CLAUSE 96 A14 AXIOM ((51 . ANY))
   ((- 10 (51)) (+ 21 (51 (50 51))) )))
   (PARTIAL   (CLAUSE 101 A15 AXIOM ((38 . ANY))
   ((- 12 (38)) (+ 18 ((50 38))) )))
   (PARTIAL   (CLAUSE 107 A16 AXIOM ((52 . ANY))
   ((- 12 (52)) (+ 21 (52 (50 52))) )))
   (PARTIAL   (CLAUSE 114 A17 AXIOM ((26 . ANY)(25 . ANY))
   ((- 10 (25)) (- 8 (26)) (+ 23 (25 26)) )))
   (PARTIAL   (CLAUSE 123 A18 AXIOM ((46 . ANY)(47 . ANY))
   ((- 12 (47)) (- 8 (46)) (+ 23 (47 46)) )))
   (PARTIAL   (CLAUSE 134 A19 AXIOM ((28 . ANY)(27 . ANY))
   ((- 8 (27)) (- 6 (28)) (+ 23 (27 28)) )))
   (PARTIAL   (CLAUSE 145 A20 AXIOM ((30 . ANY)(29 . ANY))
   ((- 6 (29)) (- 4 (30)) (+ 23 (29 30)) )))
   (PARTIAL   (CLAUSE 156 A21 AXIOM ((32 . ANY)(31 . ANY))
   ((- 6 (31)) (- 4 (32)) (- 21 (32 31)) )))
   (PARTIAL   (CLAUSE 168 A22 AXIOM ((48 . ANY)(49 . ANY))
   ((- 14 (49)) (- 4 (48)) (- 21 (48 49)) )))
   (PARTIAL   (CLAUSE 180 A23 AXIOM ((34 . ANY)(33 . ANY))
   ((- 8 (33)) (- 10 (34)) (+ 21 (33 34)) )))
   (PARTIAL   (CLAUSE 197 A24 AXIOM ((36 . ANY)(35 . ANY))
   ((- 8 (35)) (- 12 (36)) (- 21 (35 36)) )))
   (PARTIAL   (CLAUSE 218 A25 AXIOM ((24 . ANY)(22 . ANY)(20 . ANY)(19 . ANY))
   ((- 16 (19)) (- 18 (20)) (+ 21 (19 20)) (- 16 (22)) (- 23 (22 19)) (- 18 (24)) (- 21 (22 24)) (+ 21 (19 22)) )))
   (END.TIME 67330528)
   (FINAL 55 56 57 58 59 60 61 63 66 70 75 81 88 91 96 101 107 114 123 134 145 156 168 180 197 218)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   23 (DT-PREDICATE.CREATE "SMALLER" '(ANY ANY)))
   (CONS   21 (DT-PREDICATE.CREATE "EATS" '(ANY ANY)))
   (CONS   18 (DT-PREDICATE.CREATE "PLANT" '(ANY)))
   (CONS   16 (DT-PREDICATE.CREATE "ANIMAL" '(ANY)))
   (CONS   14 (DT-PREDICATE.CREATE "GRAIN" '(ANY)))
   (CONS   12 (DT-PREDICATE.CREATE "SNAIL" '(ANY)))
   (CONS   10 (DT-PREDICATE.CREATE "CATERPILLAR" '(ANY)))
   (CONS    8 (DT-PREDICATE.CREATE "BIRD" '(ANY)))
   (CONS    6 (DT-PREDICATE.CREATE "FOX" '(ANY)))
   (CONS    4 (DT-PREDICATE.CREATE "WOLF" '(ANY)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   13 (DT-CONSTANT.CREATE "muesli" 'ANY))
   (CONS   11 (DT-CONSTANT.CREATE "schnecki" 'ANY))
   (CONS    9 (DT-CONSTANT.CREATE "raupi" 'ANY))
   (CONS    7 (DT-CONSTANT.CREATE "tweety" 'ANY))
   (CONS    5 (DT-CONSTANT.CREATE "foxi" 'ANY))
   (CONS    3 (DT-CONSTANT.CREATE "lupo" 'ANY))
   (CONS   50 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 67330771)
   (PARTIAL   (CLAUSE 270 T26 THEOREM ((41 . ANY)(40 . ANY)(39 . ANY))
   ((- 16 (39)) (- 16 (40)) (- 21 (39 40)) (- 14 (41)) (- 21 (40 41)) )))
   (INITIAL   (CLAUSE 55 R.= AXIOM ((53 . ANY)) ((+ 2 (53 53)) ))
(CLAUSE 56 A1 AXIOM () ((+ 4 (3)) ))
(CLAUSE 57 A2 AXIOM () ((+ 6 (5)) ))
(CLAUSE 58 A3 AXIOM () ((+ 8 (7)) ))
(CLAUSE 59 A4 AXIOM () ((+ 10 (9)) ))
(CLAUSE 60 A5 AXIOM () ((+ 12 (11)) ))
(CLAUSE 61 A6 AXIOM () ((+ 14 (13)) ))
(CLAUSE 63 A7 AXIOM ((15 . ANY))
   ((- 4 (15)) (+ 16 (15)) ))
(CLAUSE 66 A8 AXIOM ((42 . ANY))
   ((- 6 (42)) (+ 16 (42)) ))
(CLAUSE 70 A9 AXIOM ((43 . ANY))
   ((- 8 (43)) (+ 16 (43)) ))
(CLAUSE 75 A10 AXIOM ((44 . ANY))
   ((- 10 (44)) (+ 16 (44)) ))
(CLAUSE 81 A11 AXIOM ((45 . ANY))
   ((- 12 (45)) (+ 16 (45)) ))
(CLAUSE 88 A12 AXIOM ((17 . ANY))
   ((- 14 (17)) (+ 18 (17)) ))
(CLAUSE 91 A13 AXIOM ((37 . ANY))
   ((- 10 (37)) (+ 18 ((50 37))) ))
(CLAUSE 96 A14 AXIOM ((51 . ANY))
   ((- 10 (51)) (+ 21 (51 (50 51))) ))
(CLAUSE 101 A15 AXIOM ((38 . ANY))
   ((- 12 (38)) (+ 18 ((50 38))) ))
(CLAUSE 107 A16 AXIOM ((52 . ANY))
   ((- 12 (52)) (+ 21 (52 (50 52))) ))
(CLAUSE 123 A18 AXIOM ((46 . ANY)(47 . ANY))
   ((- 12 (47)) (- 8 (46)) (+ 23 (47 46)) ))
(CLAUSE 134 A19 AXIOM ((28 . ANY)(27 . ANY))
   ((- 8 (27)) (- 6 (28)) (+ 23 (27 28)) ))
(CLAUSE 145 A20 AXIOM ((30 . ANY)(29 . ANY))
   ((- 6 (29)) (- 4 (30)) (+ 23 (29 30)) ))
(CLAUSE 156 A21 AXIOM ((32 . ANY)(31 . ANY))
   ((- 6 (31)) (- 4 (32)) (- 21 (32 31)) ))
(CLAUSE 168 A22 AXIOM ((48 . ANY)(49 . ANY))
   ((- 14 (49)) (- 4 (48)) (- 21 (48 49)) ))
(CLAUSE 180 A23 AXIOM ((34 . ANY)(33 . ANY))
   ((- 8 (33)) (- 10 (34)) (+ 21 (33 34)) ))
(CLAUSE 197 A24 AXIOM ((36 . ANY)(35 . ANY))
   ((- 8 (35)) (- 12 (36)) (- 21 (35 36)) ))
(CLAUSE 218 A25 AXIOM ((24 . ANY)(22 . ANY)(20 . ANY)(19 . ANY))
   ((- 16 (19)) (- 18 (20)) (+ 21 (19 20)) (- 16 (22)) (- 23 (22 19)) (- 18 (24)) (- 21 (22 24)) (+ 21 (19 22)) ))
(CLAUSE 270 T26 THEOREM ((41 . ANY)(40 . ANY)(39 . ANY))
   ((- 16 (39)) (- 16 (40)) (- 21 (39 40)) (- 14 (41)) (- 21 (40 41)) ))   )
   (END.TIME 67342292)
   (FINAL 55 56 57 58 59 60 61 63 66 70 75 81 88 91 96 101 107 123 134 145 156 168 180 197 218 270)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   23 (DT-PREDICATE.CREATE "SMALLER" '(ANY ANY)))
   (CONS   21 (DT-PREDICATE.CREATE "EATS" '(ANY ANY)))
   (CONS   18 (DT-PREDICATE.CREATE "PLANT" '(ANY)))
   (CONS   16 (DT-PREDICATE.CREATE "ANIMAL" '(ANY)))
   (CONS   14 (DT-PREDICATE.CREATE "GRAIN" '(ANY)))
   (CONS   12 (DT-PREDICATE.CREATE "SNAIL" '(ANY)))
   (CONS   10 (DT-PREDICATE.CREATE "CATERPILLAR" '(ANY)))
   (CONS    8 (DT-PREDICATE.CREATE "BIRD" '(ANY)))
   (CONS    6 (DT-PREDICATE.CREATE "FOX" '(ANY)))
   (CONS    4 (DT-PREDICATE.CREATE "WOLF" '(ANY)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   13 (DT-CONSTANT.CREATE "muesli" 'ANY))
   (CONS   11 (DT-CONSTANT.CREATE "schnecki" 'ANY))
   (CONS    9 (DT-CONSTANT.CREATE "raupi" 'ANY))
   (CONS    7 (DT-CONSTANT.CREATE "tweety" 'ANY))
   (CONS    5 (DT-CONSTANT.CREATE "foxi" 'ANY))
   (CONS    3 (DT-CONSTANT.CREATE "lupo" 'ANY))
   (CONS   50 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
   (RESULT))
(indices ((270 47 48 49 50 51) (218 15 16 17 18 19 20 21 22) (197 40 41 42) (180 37 38 39) (168 34 35 36) (156 33 35 36) (145 30 31 32) (134 27 28 29) (123 24 25 26) (114 23 25 26) (107 44 46) (101 44 45) (96 43 46) (91 43 45) (88 13 14) (81 11 12) (75 10 12) (70 9 12) (66 8 12) (63 7 12) (61 6) (60 5) (59 4) (58 3) (57 2) (56 1) (55 NIL)))

(SPLITPARTS "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 RPC 415.0 Embedding Support 407.0 UX Support 416.0 Experimental Network RPC 415.0 Experimental NFS Client 415.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Remote Screen 418.1 KKL 20.1 HADES 16.0 Waltz 7.0 COLUMN 7.0 MKRP 49.3 GENTRAFO 1.0" "31-OCT,1991 22:41" NIL)

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.I_CLAUSE.PURITY . P)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
    (RED.I_CLAUSE.REPL.FACTORING . T)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.I_CLAUSE.REWRITING . DEM)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.D_CLAUSE.PURITY . P)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION.RECHECK)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_R.SELECTION * 10 (+ 2 VARIABLES (* 2 DEPTH) (* 3 NOLIT)))
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_FINITE.DOMAIN . T)
    (STR_SORT.LITERALS)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 10)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_P.SELECTION * WEIGHT (IF SUPPORT 3 1) (IF EQUATIONAL 1 2))
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ X (* 2 Y))) (* (X Y) (+ X (* X Y))) (- (X) (+ (* 2 X) -1)) (0 NIL 2) (1 NIL 2))
    (ER_NARROW.DEPTH . 0)
    (ER_NARROW.NEXT . DEPTH)
    (ER_NARROW.TEST NORM C DELTA SL N)
    (ER_COMPILE)
    (GEN_SPLITTING . 0)
    (GEN_PRESIMPLIFICATION . T)
    (GEN_MIN.EXPRESSION.LENGTH.FOR.FILE)
    (GEN_MIN.EXPRESSION.SIZE.FOR.FILE)
    (GEN_MANUAL.CONTROL)
    (GEN_MAXIMUM.STEPS)
    (GEN_GRAPH.SAVING)
    (GEN_SAVE.FILE . SAVEDEFAULT)
    (GEN_LISP.GARBAGE.COLLECTION)
    (GEN_COMMON.LISP)
    (TR_PREPROCESSING)
    (TR_STEP.MODE . LR)
    (TR_DUMP)
    (TR_CLAUSE.MODE . I)
    (TR_LINK.MODE . I)
    (TR_TRACE.FILE)
    (TR_TERMINAL . T)
    (PR_LATEX)
    (PR_INFIX.FORM . T)
    (PR_PREFIX.FORM)
    (PR_OPTIONS)
    (PR_AXIOM.CLAUSES . T)
    (PR_SYMBOLS)
    (PR_DIRECT.PROOF . T)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
    (PR_LITERALS)
   )

(REFUTATION (START.TIME 67344817)

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.I_CLAUSE.PURITY . P)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
    (RED.I_CLAUSE.REPL.FACTORING . T)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.I_CLAUSE.REWRITING . DEM)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.D_CLAUSE.PURITY . P)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION.RECHECK)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_R.SELECTION * 10 (+ 2 VARIABLES (* 2 DEPTH) (* 3 NOLIT)))
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_FINITE.DOMAIN . T)
    (STR_SORT.LITERALS)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 10)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_P.SELECTION * WEIGHT (IF SUPPORT 3 1) (IF EQUATIONAL 1 2))
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ X (* 2 Y))) (* (X Y) (+ X (* X Y))) (- (X) (+ (* 2 X) -1)) (0 NIL 2) (1 NIL 2))
    (ER_NARROW.DEPTH . 0)
    (ER_NARROW.NEXT . DEPTH)
    (ER_NARROW.TEST NORM C DELTA SL N)
    (ER_COMPILE)
    (GEN_SPLITTING . 0)
    (GEN_PRESIMPLIFICATION . T)
    (GEN_MIN.EXPRESSION.LENGTH.FOR.FILE)
    (GEN_MIN.EXPRESSION.SIZE.FOR.FILE)
    (GEN_MANUAL.CONTROL)
    (GEN_MAXIMUM.STEPS)
    (GEN_GRAPH.SAVING)
    (GEN_SAVE.FILE . SAVEDEFAULT)
    (GEN_LISP.GARBAGE.COLLECTION)
    (GEN_COMMON.LISP)
    (TR_PREPROCESSING)
    (TR_STEP.MODE . LR)
    (TR_DUMP)
    (TR_CLAUSE.MODE . I)
    (TR_LINK.MODE . I)
    (TR_TRACE.FILE)
    (TR_TERMINAL . T)
    (PR_LATEX)
    (PR_INFIX.FORM . T)
    (PR_PREFIX.FORM)
    (PR_OPTIONS)
    (PR_AXIOM.CLAUSES . T)
    (PR_SYMBOLS)
    (PR_DIRECT.PROOF . T)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
    (PR_LITERALS)
   )
   (INITIAL   (CLAUSE 55 R.= AXIOM ((53 . ANY)) ((+ 2 (53 53)) ))
(CLAUSE 56 A1 AXIOM () ((+ 4 (3)) ))
(CLAUSE 57 A2 AXIOM () ((+ 6 (5)) ))
(CLAUSE 58 A3 AXIOM () ((+ 8 (7)) ))
(CLAUSE 59 A4 AXIOM () ((+ 10 (9)) ))
(CLAUSE 60 A5 AXIOM () ((+ 12 (11)) ))
(CLAUSE 61 A6 AXIOM () ((+ 14 (13)) ))
(CLAUSE 63 A7 AXIOM ((15 . ANY))
   ((- 4 (15)) (+ 16 (15)) ))
(CLAUSE 66 A8 AXIOM ((42 . ANY))
   ((- 6 (42)) (+ 16 (42)) ))
(CLAUSE 70 A9 AXIOM ((43 . ANY))
   ((- 8 (43)) (+ 16 (43)) ))
(CLAUSE 75 A10 AXIOM ((44 . ANY))
   ((- 10 (44)) (+ 16 (44)) ))
(CLAUSE 81 A11 AXIOM ((45 . ANY))
   ((- 12 (45)) (+ 16 (45)) ))
(CLAUSE 88 A12 AXIOM ((17 . ANY))
   ((- 14 (17)) (+ 18 (17)) ))
(CLAUSE 91 A13 AXIOM ((37 . ANY))
   ((- 10 (37)) (+ 18 ((50 37))) ))
(CLAUSE 96 A14 AXIOM ((51 . ANY))
   ((- 10 (51)) (+ 21 (51 (50 51))) ))
(CLAUSE 101 A15 AXIOM ((38 . ANY))
   ((- 12 (38)) (+ 18 ((50 38))) ))
(CLAUSE 107 A16 AXIOM ((52 . ANY))
   ((- 12 (52)) (+ 21 (52 (50 52))) ))
(CLAUSE 123 A18 AXIOM ((46 . ANY)(47 . ANY))
   ((- 12 (47)) (- 8 (46)) (+ 23 (47 46)) ))
(CLAUSE 134 A19 AXIOM ((28 . ANY)(27 . ANY))
   ((- 8 (27)) (- 6 (28)) (+ 23 (27 28)) ))
(CLAUSE 145 A20 AXIOM ((30 . ANY)(29 . ANY))
   ((- 6 (29)) (- 4 (30)) (+ 23 (29 30)) ))
(CLAUSE 156 A21 AXIOM ((32 . ANY)(31 . ANY))
   ((- 6 (31)) (- 4 (32)) (- 21 (32 31)) ))
(CLAUSE 168 A22 AXIOM ((48 . ANY)(49 . ANY))
   ((- 14 (49)) (- 4 (48)) (- 21 (48 49)) ))
(CLAUSE 180 A23 AXIOM ((34 . ANY)(33 . ANY))
   ((- 8 (33)) (- 10 (34)) (+ 21 (33 34)) ))
(CLAUSE 197 A24 AXIOM ((36 . ANY)(35 . ANY))
   ((- 8 (35)) (- 12 (36)) (- 21 (35 36)) ))
(CLAUSE 218 A25 AXIOM ((24 . ANY)(22 . ANY)(20 . ANY)(19 . ANY))
   ((- 16 (19)) (- 18 (20)) (+ 21 (19 20)) (- 16 (22)) (- 23 (22 19)) (- 18 (24)) (- 21 (22 24)) (+ 21 (19 22)) ))
(CLAUSE 270 T26 THEOREM ((41 . ANY)(40 . ANY)(39 . ANY))
   ((- 16 (39)) (- 16 (40)) (- 21 (39 40)) (- 14 (41)) (- 21 (40 41)) ))   )
   (OPERATION (CLAUSE 317 R1 (270 61) ((321 . ANY)(320 . ANY))
   ((- 16 (320)) (- 16 (321)) (- 21 (320 321)) (- 21 (321 13)) ))
   (RESOLUTION 61 1 270 4 NIL (41 13) 317))
   (OPERATION (CLAUSE 344 R1.F1 (317) ()
   ((- 21 (13 13)) (- 16 (13)) ))
   (FACTORIZATION 317 NIL (321 13 320 13) 344))
   (OPERATION (CLAUSE 370 R2 (197 58) ((373 . ANY))
   ((- 12 (373)) (- 21 (7 373)) ))
   (RESOLUTION 58 1 197 1 NIL (35 7) 370))
   (OPERATION (CLAUSE 328 R3 (370 60) () ((- 21 (7 11)) ))
   (RESOLUTION 60 1 370 1 NIL (373 11) 328))
   (OPERATION (CLAUSE 374 R4 (180 58) ((212 . ANY))
   ((- 10 (212)) (+ 21 (7 212)) ))
   (RESOLUTION 58 1 180 1 NIL (33 7) 374))
   (OPERATION (CLAUSE 360 R5 (374 59) () ((+ 21 (7 9)) ))
   (RESOLUTION 59 1 374 1 NIL (212 9) 360))
   (OPERATION (CLAUSE 285 R6 (168 61) ((371 . ANY))
   ((- 4 (371)) (- 21 (371 13)) ))
   (RESOLUTION 61 1 168 1 NIL (49 13) 285))
   (OPERATION (CLAUSE 182 R7 (285 56) () ((- 21 (3 13)) ))
   (RESOLUTION 56 1 285 1 NIL (371 3) 182))
   (OPERATION (CLAUSE 293 R8 (156 57) ((295 . ANY))
   ((- 4 (295)) (- 21 (295 5)) ))
   (RESOLUTION 57 1 156 1 NIL (31 5) 293))
   (OPERATION (CLAUSE 120 R9 (293 56) () ((- 21 (3 5)) ))
   (RESOLUTION 56 1 293 1 NIL (295 3) 120))
   (OPERATION (CLAUSE 127 R10 (145 57) ((298 . ANY))
   ((- 4 (298)) (+ 23 (5 298)) ))
   (RESOLUTION 57 1 145 1 NIL (29 5) 127))
   (OPERATION (CLAUSE 206 R11 (127 56) () ((+ 23 (5 3)) ))
   (RESOLUTION 56 1 127 1 NIL (298 3) 206))
   (OPERATION (CLAUSE 192 R12 (134 58) ((162 . ANY))
   ((- 6 (162)) (+ 23 (7 162)) ))
   (RESOLUTION 58 1 134 1 NIL (27 7) 192))
   (OPERATION (CLAUSE 175 R13 (192 57) () ((+ 23 (7 5)) ))
   (RESOLUTION 57 1 192 1 NIL (162 5) 175))
   (OPERATION (CLAUSE 153 R14 (123 60) ((147 . ANY))
   ((- 8 (147)) (+ 23 (11 147)) ))
   (RESOLUTION 60 1 123 1 NIL (47 11) 153))
   (OPERATION (CLAUSE 126 R15 (153 58) () ((+ 23 (11 7)) ))
   (RESOLUTION 58 1 153 1 NIL (147 7) 126))
   (OPERATION (CLAUSE 136 R16 (107 60) () ((+ 21 (11 (50 11))) ))
   (RESOLUTION 60 1 107 1 NIL (52 11) 136))
   (OPERATION (CLAUSE 313 R17 (101 60) () ((+ 18 ((50 11))) ))
   (RESOLUTION 60 1 101 1 NIL (38 11) 313))
   (OPERATION (CLAUSE 363 R18 (96 59) () ((+ 21 (9 (50 9))) ))
   (RESOLUTION 59 1 96 1 NIL (51 9) 363))
   (OPERATION (CLAUSE 108 R19 (91 59) () ((+ 18 ((50 9))) ))
   (RESOLUTION 59 1 91 1 NIL (37 9) 108))
   (OPERATION (CLAUSE 104 R20 (88 61) () ((+ 18 (13)) ))
   (RESOLUTION 61 1 88 1 NIL (17 13) 104))
   (OPERATION (CLAUSE 221 R21 (81 60) () ((+ 16 (11)) ))
   (RESOLUTION 60 1 81 1 NIL (45 11) 221))
   (OPERATION (CLAUSE 151 R22 (75 59) () ((+ 16 (9)) ))
   (RESOLUTION 59 1 75 1 NIL (44 9) 151))
   (OPERATION (CLAUSE 288 R23 (70 58) () ((+ 16 (7)) ))
   (RESOLUTION 58 1 70 1 NIL (43 7) 288))
   (OPERATION (CLAUSE 369 R24 (66 57) () ((+ 16 (5)) ))
   (RESOLUTION 57 1 66 1 NIL (42 5) 369))
   (OPERATION (CLAUSE 341 R25 (63 56) () ((+ 16 (3)) ))
   (RESOLUTION 56 1 63 1 NIL (15 3) 341))
   (OPERATION (CLAUSE 297 R26 (317 360) ()
   ((- 16 (7)) (- 16 (9)) (- 21 (9 13)) ))
   (RESOLUTION 360 1 317 3 NIL (320 7 321 9) 297))
   (OPERATION (CLAUSE 297 R26 (317 360) ()
   ((- 16 (7)) (- 16 (9)) (- 21 (9 13)) ))
   (REPLACEMENT.OPERATION NIL (297 151)
  (((297 . 2) (151 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 297 R26 (317 360) ()
   ((- 16 (7)) (- 21 (9 13)) ))
   (REPLACEMENT.OPERATION NIL (297 288)
  (((297 . 1) (288 . 1) NIL))
  (NIL)
  NIL))(OPERATION (CLAUSE 77 R27 (218 288) ()((- 18 (13))(+ 21 (7 13))(- 16 (11))(- 23 (11 7))(- 18 ((50 11)))(- 21 (11 (50 11)))(+ 21 (7 11))))(RESOLUTION 288 1 218 1 NIL (19 7 20 13 22 11 24 (50 11)) 77))(OPERATION (CLAUSE 77 R27 (218 288 104 221 328 136 313 126) () ((+ 21 (7 13)) ))(REPLACEMENT.OPERATION NIL (77 104 221 328 136 313 126 )(((77 . 1)(104 . 1)NIL)((77 . 3)(221 . 1)NIL)((77 . 7)(328 . 1)NIL)((77 . 6)(136 . 1)NIL)((77 . 5)(313 . 1)NIL)((77 . 4)(126 . 1)NIL))( NIL  NIL  NIL  NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 366 R28 (218 120) ()((- 16 (3))(- 18 (13))(+ 21 (3 13))(- 16 (5))(- 23 (5 3))(- 18 (13))(- 21 (5 13))))(RESOLUTION 218 8 120 1 NIL (19 3 20 13 22 5 24 13) 366))(OPERATION (CLAUSE 366 R28 (218 120 369 341 104 182 206) () ((- 21 (5 13)) ))(REPLACEMENT.OPERATION NIL (366 369 341 104 182 206 )(((366 . 4)(369 . 1)NIL)((366 . 1)(341 . 1)NIL)((366 . 2)(104 . 1)NIL)((366 . 3)(182 . 1)NIL)((366 . 5)(206 . 1)NIL))((((366 . 2) (366 . 6) NIL)) NIL  NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 233 R29 (317 369) ()((- 16 (7))(- 21 (5 7))(- 21 (7 13))))(RESOLUTION 369 1 317 1 NIL (320 5 321 7) 233))(OPERATION (CLAUSE 233 R29 (317 369 288 77) () ((- 21 (5 7)) ))(REPLACEMENT.OPERATION NIL (233 288 77 )(((233 . 1)(288 . 1)NIL)((233 . 3)(77 . 1)NIL))( NIL  NIL )NIL))(OPERATION (CLAUSE 242 R30 (218 233) ()((- 16 (5))(- 18 (13))(+ 21 (5 13))(- 16 (7))(- 23 (7 5))(- 18 (13))(- 21 (7 13))))(RESOLUTION 218 8 233 1 NIL (19 5 20 13 22 7 24 13) 242))(OPERATION (CLAUSE 242 R30 (218 233 104 288 369 104 366 175 77) () ())(REPLACEMENT.OPERATION NIL (242 104 288 369 104 366 175 77 )(((242 . 6)(104 . 1)NIL)((242 . 4)(288 . 1)NIL)((242 . 1)(369 . 1)NIL)((242 . 2)(104 . 1)NIL)((242 . 3)(366 . 1)NIL)((242 . 5)(175 . 1)NIL)((242 . 7)(77 . 1)NIL))( NIL  NIL  NIL  NIL  NIL  NIL  NIL )NIL))
   (END.TIME 67410471)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   23 (DT-PREDICATE.CREATE "SMALLER" '(ANY ANY)))
   (CONS   21 (DT-PREDICATE.CREATE "EATS" '(ANY ANY)))
   (CONS   18 (DT-PREDICATE.CREATE "PLANT" '(ANY)))
   (CONS   16 (DT-PREDICATE.CREATE "ANIMAL" '(ANY)))
   (CONS   14 (DT-PREDICATE.CREATE "GRAIN" '(ANY)))
   (CONS   12 (DT-PREDICATE.CREATE "SNAIL" '(ANY)))
   (CONS   10 (DT-PREDICATE.CREATE "CATERPILLAR" '(ANY)))
   (CONS    8 (DT-PREDICATE.CREATE "BIRD" '(ANY)))
   (CONS    6 (DT-PREDICATE.CREATE "FOX" '(ANY)))
   (CONS    4 (DT-PREDICATE.CREATE "WOLF" '(ANY)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   13 (DT-CONSTANT.CREATE "muesli" 'ANY))
   (CONS   11 (DT-CONSTANT.CREATE "schnecki" 'ANY))
   (CONS    9 (DT-CONSTANT.CREATE "raupi" 'ANY))
   (CONS    7 (DT-CONSTANT.CREATE "tweety" 'ANY))
   (CONS    5 (DT-CONSTANT.CREATE "foxi" 'ANY))
   (CONS    3 (DT-CONSTANT.CREATE "lupo" 'ANY))
   (CONS   50 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 242)
)