;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 7.1 PS 3.10 IP-TCP 52.16 KKL 15.0 HADES 9.11 WALTZ 3.1 COLUMN 4.3 MKRP 33.16 EQUALITY 16.1" "12-OCT,1989 18:07" 
   ("Edit:     Axioms and Theorems edited: 12-OCT,1989 18:05 "
   ))

(AXIOMS.INFIX    ((WOLF (LUPO))
   (FOX (FOXI))
   (BIRD (TWEETY))
   (CATERPILLAR (RAUPI))
   (SNAIL (SCHNECKI))
   (GRAIN (MUESLI))
   (* (EX |X1,X2,X3,X4,X5,X6| WOLF (X1) AND FOX (X2) AND BIRD (X3) AND CATERPILLAR (X4) AND SNAIL (X5) AND GRAIN (X6)))
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

(AXIOMS.PREFIX   ((+ 3 (2) NIL)
   (+ 5 (4) NIL)
   (+ 7 (6) NIL)
   (+ 9 (8) NIL)
   (+ 11 (10) NIL)
   (+ 13 (12) NIL)
   COMMENT
   (ALL 14 (IMPL (OR (+ 3 (14) NIL) (OR (+ 5 (14) NIL) (OR (+ 7 (14) NIL) (OR (+ 9 (14) NIL) (+ 11 (14) NIL))))) (+ 15 (14) NIL)))
   (ALL 16 (IMPL (+ 13 (16) NIL) (+ 17 (16) NIL)))
   (ALL 18 (IMPL (+ 15 (18) NIL) (OR (ALL 19 (IMPL (+ 17 (19) NIL) (+ 20 (18 19) NIL))) (ALL 21 (IMPL (AND (+ 15 (21) NIL) (AND (+ 22 (21 18) NIL) (EX 23 (AND (+ 17 (23) NIL) (+ 20 (21 23) NIL))))) (+ 20 (18 21) NIL))))))
   (ALL 25 (ALL 24 (IMPL (AND (OR (+ 9 (24) NIL) (+ 11 (24) NIL)) (+ 7 (25) NIL)) (+ 22 (24 25) NIL))))
   (ALL 27 (ALL 26 (IMPL (AND (+ 7 (26) NIL) (+ 5 (27) NIL)) (+ 22 (26 27) NIL))))
   (ALL 29 (ALL 28 (IMPL (AND (+ 5 (28) NIL) (+ 3 (29) NIL)) (+ 22 (28 29) NIL))))
   (ALL 31 (ALL 30 (IMPL (AND (OR (+ 5 (30) NIL) (+ 13 (30) NIL)) (+ 3 (31) NIL)) (NOT (+ 20 (31 30) NIL)))))
   (ALL 33 (ALL 32 (IMPL (AND (+ 7 (32) NIL) (+ 9 (33) NIL)) (+ 20 (32 33) NIL))))
   (ALL 35 (ALL 34 (IMPL (AND (+ 7 (34) NIL) (+ 11 (35) NIL)) (NOT (+ 20 (34 35) NIL)))))
   (ALL 36 (IMPL (OR (+ 9 (36) NIL) (+ 11 (36) NIL)) (EX 37 (AND (+ 17 (37) NIL) (+ 20 (36 37) NIL)))))))

(THEOREMS.PREFIX ((EX 39 (EX 38 (AND (+ 15 (38) NIL) (AND (+ 15 (39) NIL) (AND (+ 20 (38 39) NIL) (EX 40 (AND (+ 13 (40) NIL) (+ 20 (39 40) NIL))))))))))

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.I_CLAUSE.PURITY . T)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
    (RED.I_CLAUSE.REPL.FACTORING . T)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.I_CLAUSE.REWRITING . T)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.D_CLAUSE.PURITY . T)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION.RECHECK)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_E-RESOLUTION)
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_R.DEMODULATION . T)
    (STR_P.DEMODULATION . T)
    (STR_INDUCTION)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 5)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_COMPLETION . IGNORING)
    (ER_ORDERING . KNUTH-BENDIX)
    (ER_LINK.DEPTH.WEIGHT . 0)
    (ER_KNUTH.BENDIX.WEIGHT (PLUS 5) (MULT 5) (MINUS 2) (0 2) (1 2))
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
    (PR_INFIX.FORM . T)
    (PR_PREFIX.FORM)
    (PR_OPTIONS)
    (PR_AXIOM.CLAUSES . T)
    (PR_SYMBOLS)
    (PR_DIRECT.PROOF . T)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_STATISTICS)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
   )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD REIW RIWD RE))

(AXIOMS (START.TIME 15131796)
   (PARTIAL   (CLAUSE 52 A1 AXIOM () ((+ 3 (2)) )))
   (PARTIAL   (CLAUSE 53 A2 AXIOM () ((+ 5 (4)) )))
   (PARTIAL   (CLAUSE 54 A3 AXIOM () ((+ 7 (6)) )))
   (PARTIAL   (CLAUSE 55 A4 AXIOM () ((+ 9 (8)) )))
   (PARTIAL   (CLAUSE 56 A5 AXIOM () ((+ 11 (10)) )))
   (PARTIAL   (CLAUSE 57 A6 AXIOM () ((+ 13 (12)) )))
   (PARTIAL   (CLAUSE 59 A7 AXIOM ((14 . ANY))
   ((- 3 (14)) (+ 15 (14)) )))
   (PARTIAL   (CLAUSE 62 A8 AXIOM ((41 . ANY))
   ((- 5 (41)) (+ 15 (41)) )))
   (PARTIAL   (CLAUSE 66 A9 AXIOM ((42 . ANY))
   ((- 7 (42)) (+ 15 (42)) )))
   (PARTIAL   (CLAUSE 71 A10 AXIOM ((43 . ANY))
   ((- 9 (43)) (+ 15 (43)) )))
   (PARTIAL   (CLAUSE 77 A11 AXIOM ((44 . ANY))
   ((- 11 (44)) (+ 15 (44)) )))
   (PARTIAL   (CLAUSE 84 A12 AXIOM ((16 . ANY))
   ((- 13 (16)) (+ 17 (16)) )))
   (PARTIAL   (CLAUSE 87 A13 AXIOM ((36 . ANY))
   ((- 9 (36)) (+ 17 ((49 36))) )))
   (PARTIAL   (CLAUSE 92 A14 AXIOM ((50 . ANY))
   ((- 9 (50)) (+ 20 (50 (49 50))) )))
   (PARTIAL   (CLAUSE 97 A15 AXIOM ((37 . ANY))
   ((- 11 (37)) (+ 17 ((49 37))) )))
   (PARTIAL   (CLAUSE 103 A16 AXIOM ((51 . ANY))
   ((- 11 (51)) (+ 20 (51 (49 51))) )))
   (PARTIAL   (CLAUSE 110 A17 AXIOM ((25 . ANY)(24 . ANY))
   ((- 9 (24)) (- 7 (25)) (+ 22 (24 25)) )))
   (PARTIAL   (CLAUSE 119 A18 AXIOM ((45 . ANY)(46 . ANY))
   ((- 11 (46)) (- 7 (45)) (+ 22 (46 45)) )))
   (PARTIAL   (CLAUSE 130 A19 AXIOM ((27 . ANY)(26 . ANY))
   ((- 7 (26)) (- 5 (27)) (+ 22 (26 27)) )))
   (PARTIAL   (CLAUSE 141 A20 AXIOM ((29 . ANY)(28 . ANY))
   ((- 5 (28)) (- 3 (29)) (+ 22 (28 29)) )))
   (PARTIAL   (CLAUSE 152 A21 AXIOM ((31 . ANY)(30 . ANY))
   ((- 5 (30)) (- 3 (31)) (- 20 (31 30)) )))
   (PARTIAL   (CLAUSE 164 A22 AXIOM ((47 . ANY)(48 . ANY))
   ((- 13 (48)) (- 3 (47)) (- 20 (47 48)) )))
   (PARTIAL   (CLAUSE 176 A23 AXIOM ((33 . ANY)(32 . ANY))
   ((- 7 (32)) (- 9 (33)) (+ 20 (32 33)) )))
   (PARTIAL   (CLAUSE 193 A24 AXIOM ((35 . ANY)(34 . ANY))
   ((- 7 (34)) (- 11 (35)) (- 20 (34 35)) )))
   (PARTIAL   (CLAUSE 214 A25 AXIOM ((23 . ANY)(21 . ANY)(19 . ANY)(18 . ANY))
   ((- 15 (18)) (- 17 (19)) (+ 20 (18 19)) (- 15 (21)) (- 22 (21 18)) (- 17 (23)) (- 20 (21 23)) (+ 20 (18 21)) )))
   (END.TIME 15158446)
   (FINAL 52 53 54 55 56 57 59 62 66 71 77 84 87 92 97 103 110 119 130 141 152 164 176 193 214)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   22 (DT-PREDICATE.CREATE "SMALLER" '(ANY ANY)))
   (CONS   20 (DT-PREDICATE.CREATE "EATS" '(ANY ANY)))
   (CONS   17 (DT-PREDICATE.CREATE "PLANT" '(ANY)))
   (CONS   15 (DT-PREDICATE.CREATE "ANIMAL" '(ANY)))
   (CONS   13 (DT-PREDICATE.CREATE "GRAIN" '(ANY)))
   (CONS   11 (DT-PREDICATE.CREATE "SNAIL" '(ANY)))
   (CONS    9 (DT-PREDICATE.CREATE "CATERPILLAR" '(ANY)))
   (CONS    7 (DT-PREDICATE.CREATE "BIRD" '(ANY)))
   (CONS    5 (DT-PREDICATE.CREATE "FOX" '(ANY)))
   (CONS    3 (DT-PREDICATE.CREATE "WOLF" '(ANY)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   12 (DT-CONSTANT.CREATE "muesli" 'ANY))
   (CONS   10 (DT-CONSTANT.CREATE "schnecki" 'ANY))
   (CONS    8 (DT-CONSTANT.CREATE "raupi" 'ANY))
   (CONS    6 (DT-CONSTANT.CREATE "tweety" 'ANY))
   (CONS    4 (DT-CONSTANT.CREATE "foxi" 'ANY))
   (CONS    2 (DT-CONSTANT.CREATE "lupo" 'ANY))
   (CONS   49 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 15158768)
   (PARTIAL   (CLAUSE 266 T26 THEOREM ((40 . ANY)(39 . ANY)(38 . ANY))
   ((- 15 (38)) (- 15 (39)) (- 20 (38 39)) (- 13 (40)) (- 20 (39 40)) )))
   (INITIAL   (CLAUSE 52 A1 AXIOM () ((+ 3 (2)) ))
(CLAUSE 53 A2 AXIOM () ((+ 5 (4)) ))
(CLAUSE 54 A3 AXIOM () ((+ 7 (6)) ))
(CLAUSE 55 A4 AXIOM () ((+ 9 (8)) ))
(CLAUSE 56 A5 AXIOM () ((+ 11 (10)) ))
(CLAUSE 57 A6 AXIOM () ((+ 13 (12)) ))
(CLAUSE 59 A7 AXIOM ((14 . ANY))
   ((- 3 (14)) (+ 15 (14)) ))
(CLAUSE 62 A8 AXIOM ((41 . ANY))
   ((- 5 (41)) (+ 15 (41)) ))
(CLAUSE 66 A9 AXIOM ((42 . ANY))
   ((- 7 (42)) (+ 15 (42)) ))
(CLAUSE 71 A10 AXIOM ((43 . ANY))
   ((- 9 (43)) (+ 15 (43)) ))
(CLAUSE 77 A11 AXIOM ((44 . ANY))
   ((- 11 (44)) (+ 15 (44)) ))
(CLAUSE 84 A12 AXIOM ((16 . ANY))
   ((- 13 (16)) (+ 17 (16)) ))
(CLAUSE 87 A13 AXIOM ((36 . ANY))
   ((- 9 (36)) (+ 17 ((49 36))) ))
(CLAUSE 92 A14 AXIOM ((50 . ANY))
   ((- 9 (50)) (+ 20 (50 (49 50))) ))
(CLAUSE 97 A15 AXIOM ((37 . ANY))
   ((- 11 (37)) (+ 17 ((49 37))) ))
(CLAUSE 103 A16 AXIOM ((51 . ANY))
   ((- 11 (51)) (+ 20 (51 (49 51))) ))
(CLAUSE 110 A17 AXIOM ((25 . ANY)(24 . ANY))
   ((- 9 (24)) (- 7 (25)) (+ 22 (24 25)) ))
(CLAUSE 119 A18 AXIOM ((45 . ANY)(46 . ANY))
   ((- 11 (46)) (- 7 (45)) (+ 22 (46 45)) ))
(CLAUSE 130 A19 AXIOM ((27 . ANY)(26 . ANY))
   ((- 7 (26)) (- 5 (27)) (+ 22 (26 27)) ))
(CLAUSE 141 A20 AXIOM ((29 . ANY)(28 . ANY))
   ((- 5 (28)) (- 3 (29)) (+ 22 (28 29)) ))
(CLAUSE 152 A21 AXIOM ((31 . ANY)(30 . ANY))
   ((- 5 (30)) (- 3 (31)) (- 20 (31 30)) ))
(CLAUSE 164 A22 AXIOM ((47 . ANY)(48 . ANY))
   ((- 13 (48)) (- 3 (47)) (- 20 (47 48)) ))
(CLAUSE 176 A23 AXIOM ((33 . ANY)(32 . ANY))
   ((- 7 (32)) (- 9 (33)) (+ 20 (32 33)) ))
(CLAUSE 193 A24 AXIOM ((35 . ANY)(34 . ANY))
   ((- 7 (34)) (- 11 (35)) (- 20 (34 35)) ))
(CLAUSE 214 A25 AXIOM ((23 . ANY)(21 . ANY)(19 . ANY)(18 . ANY))
   ((- 15 (18)) (- 17 (19)) (+ 20 (18 19)) (- 15 (21)) (- 22 (21 18)) (- 17 (23)) (- 20 (21 23)) (+ 20 (18 21)) ))
(CLAUSE 266 T26 THEOREM ((40 . ANY)(39 . ANY)(38 . ANY))
   ((- 15 (38)) (- 15 (39)) (- 20 (38 39)) (- 13 (40)) (- 20 (39 40)) ))   )
   (END.TIME 15200493)
   (FINAL 52 53 54 55 56 57 59 62 66 71 77 84 87 92 97 103 110 119 130 141 152 164 176 193 214 266)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   22 (DT-PREDICATE.CREATE "SMALLER" '(ANY ANY)))
   (CONS   20 (DT-PREDICATE.CREATE "EATS" '(ANY ANY)))
   (CONS   17 (DT-PREDICATE.CREATE "PLANT" '(ANY)))
   (CONS   15 (DT-PREDICATE.CREATE "ANIMAL" '(ANY)))
   (CONS   13 (DT-PREDICATE.CREATE "GRAIN" '(ANY)))
   (CONS   11 (DT-PREDICATE.CREATE "SNAIL" '(ANY)))
   (CONS    9 (DT-PREDICATE.CREATE "CATERPILLAR" '(ANY)))
   (CONS    7 (DT-PREDICATE.CREATE "BIRD" '(ANY)))
   (CONS    5 (DT-PREDICATE.CREATE "FOX" '(ANY)))
   (CONS    3 (DT-PREDICATE.CREATE "WOLF" '(ANY)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   12 (DT-CONSTANT.CREATE "muesli" 'ANY))
   (CONS   10 (DT-CONSTANT.CREATE "schnecki" 'ANY))
   (CONS    8 (DT-CONSTANT.CREATE "raupi" 'ANY))
   (CONS    6 (DT-CONSTANT.CREATE "tweety" 'ANY))
   (CONS    4 (DT-CONSTANT.CREATE "foxi" 'ANY))
   (CONS    2 (DT-CONSTANT.CREATE "lupo" 'ANY))
   (CONS   49 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
   (RESULT))

(SPLITPARTS "Genera 7.1 PS 3.10 IP-TCP 52.16 KKL 15.0 HADES 9.11 WALTZ 3.1 COLUMN 4.3 MKRP 33.16 EQUALITY 16.1" "12-OCT,1989 18:08" NIL)

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.I_CLAUSE.PURITY . T)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
    (RED.I_CLAUSE.REPL.FACTORING . T)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.I_CLAUSE.REWRITING . T)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.D_CLAUSE.PURITY . T)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION.RECHECK)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_E-RESOLUTION)
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_R.DEMODULATION . T)
    (STR_P.DEMODULATION . T)
    (STR_INDUCTION)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 5)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_COMPLETION . IGNORING)
    (ER_ORDERING . KNUTH-BENDIX)
    (ER_LINK.DEPTH.WEIGHT . 0)
    (ER_KNUTH.BENDIX.WEIGHT (PLUS 5) (MULT 5) (MINUS 2) (0 2) (1 2))
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
    (PR_INFIX.FORM . T)
    (PR_PREFIX.FORM)
    (PR_OPTIONS)
    (PR_AXIOM.CLAUSES . T)
    (PR_SYMBOLS)
    (PR_DIRECT.PROOF . T)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_STATISTICS)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
   )

(REFUTATION (START.TIME 15225303)

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.I_CLAUSE.PURITY . T)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
    (RED.I_CLAUSE.REPL.FACTORING . T)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.I_CLAUSE.REWRITING . T)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
    (RED.D_CLAUSE.PURITY . T)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK . T)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION.RECHECK)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_E-RESOLUTION)
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_R.DEMODULATION . T)
    (STR_P.DEMODULATION . T)
    (STR_INDUCTION)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 5)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_COMPLETION . IGNORING)
    (ER_ORDERING . KNUTH-BENDIX)
    (ER_LINK.DEPTH.WEIGHT . 0)
    (ER_KNUTH.BENDIX.WEIGHT (PLUS 5) (MULT 5) (MINUS 2) (0 2) (1 2))
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
    (PR_INFIX.FORM . T)
    (PR_PREFIX.FORM)
    (PR_OPTIONS)
    (PR_AXIOM.CLAUSES . T)
    (PR_SYMBOLS)
    (PR_DIRECT.PROOF . T)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_STATISTICS)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
   )
   (INITIAL   (CLAUSE 52 A1 AXIOM () ((+ 3 (2)) ))
(CLAUSE 53 A2 AXIOM () ((+ 5 (4)) ))
(CLAUSE 54 A3 AXIOM () ((+ 7 (6)) ))
(CLAUSE 55 A4 AXIOM () ((+ 9 (8)) ))
(CLAUSE 56 A5 AXIOM () ((+ 11 (10)) ))
(CLAUSE 57 A6 AXIOM () ((+ 13 (12)) ))
(CLAUSE 59 A7 AXIOM ((14 . ANY))
   ((- 3 (14)) (+ 15 (14)) ))
(CLAUSE 62 A8 AXIOM ((41 . ANY))
   ((- 5 (41)) (+ 15 (41)) ))
(CLAUSE 66 A9 AXIOM ((42 . ANY))
   ((- 7 (42)) (+ 15 (42)) ))
(CLAUSE 71 A10 AXIOM ((43 . ANY))
   ((- 9 (43)) (+ 15 (43)) ))
(CLAUSE 77 A11 AXIOM ((44 . ANY))
   ((- 11 (44)) (+ 15 (44)) ))
(CLAUSE 84 A12 AXIOM ((16 . ANY))
   ((- 13 (16)) (+ 17 (16)) ))
(CLAUSE 87 A13 AXIOM ((36 . ANY))
   ((- 9 (36)) (+ 17 ((49 36))) ))
(CLAUSE 92 A14 AXIOM ((50 . ANY))
   ((- 9 (50)) (+ 20 (50 (49 50))) ))
(CLAUSE 97 A15 AXIOM ((37 . ANY))
   ((- 11 (37)) (+ 17 ((49 37))) ))
(CLAUSE 103 A16 AXIOM ((51 . ANY))
   ((- 11 (51)) (+ 20 (51 (49 51))) ))
(CLAUSE 110 A17 AXIOM ((25 . ANY)(24 . ANY))
   ((- 9 (24)) (- 7 (25)) (+ 22 (24 25)) ))
(CLAUSE 119 A18 AXIOM ((45 . ANY)(46 . ANY))
   ((- 11 (46)) (- 7 (45)) (+ 22 (46 45)) ))
(CLAUSE 130 A19 AXIOM ((27 . ANY)(26 . ANY))
   ((- 7 (26)) (- 5 (27)) (+ 22 (26 27)) ))
(CLAUSE 141 A20 AXIOM ((29 . ANY)(28 . ANY))
   ((- 5 (28)) (- 3 (29)) (+ 22 (28 29)) ))
(CLAUSE 152 A21 AXIOM ((31 . ANY)(30 . ANY))
   ((- 5 (30)) (- 3 (31)) (- 20 (31 30)) ))
(CLAUSE 164 A22 AXIOM ((47 . ANY)(48 . ANY))
   ((- 13 (48)) (- 3 (47)) (- 20 (47 48)) ))
(CLAUSE 176 A23 AXIOM ((33 . ANY)(32 . ANY))
   ((- 7 (32)) (- 9 (33)) (+ 20 (32 33)) ))
(CLAUSE 193 A24 AXIOM ((35 . ANY)(34 . ANY))
   ((- 7 (34)) (- 11 (35)) (- 20 (34 35)) ))
(CLAUSE 214 A25 AXIOM ((23 . ANY)(21 . ANY)(19 . ANY)(18 . ANY))
   ((- 15 (18)) (- 17 (19)) (+ 20 (18 19)) (- 15 (21)) (- 22 (21 18)) (- 17 (23)) (- 20 (21 23)) (+ 20 (18 21)) ))
(CLAUSE 266 T26 THEOREM ((40 . ANY)(39 . ANY)(38 . ANY))
   ((- 15 (38)) (- 15 (39)) (- 20 (38 39)) (- 13 (40)) (- 20 (39 40)) ))   )
   (OPERATION (CLAUSE 330 R1 (266 57) ((334 . ANY)(333 . ANY))
   ((- 15 (333)) (- 15 (334)) (- 20 (333 334)) (- 20 (334 12)) ))
   (RESOLUTION 57 1 266 4 NIL (40 12) 330))
   (OPERATION (CLAUSE 355 R1.F1 (330) ()
   ((- 20 (12 12)) (- 15 (12)) ))
   (FACTORIZATION 330 NIL (334 12 333 12) 355))
   (OPERATION (CLAUSE 385 R2 (193 54) ((388 . ANY))
   ((- 11 (388)) (- 20 (6 388)) ))
   (RESOLUTION 54 1 193 1 NIL (34 6) 385))
   (OPERATION (CLAUSE 410 R3 (385 56) () ((- 20 (6 10)) ))
   (RESOLUTION 56 1 385 1 NIL (388 10) 410))
   (OPERATION (CLAUSE 427 R4 (193 56) ((429 . ANY))
   ((- 7 (429)) (- 20 (429 10)) ))
   (RESOLUTION 56 1 193 2 NIL (35 10) 427))
   (OPERATION (CLAUSE 440 R5 (176 54) ((443 . ANY))
   ((- 9 (443)) (+ 20 (6 443)) ))
   (RESOLUTION 54 1 176 1 NIL (32 6) 440))
   (OPERATION (CLAUSE 469 R6 (440 55) () ((+ 20 (6 8)) ))
   (RESOLUTION 55 1 440 1 NIL (443 8) 469))
   (OPERATION (CLAUSE 493 R7 (176 55) ((495 . ANY))
   ((- 7 (495)) (+ 20 (495 8)) ))
   (RESOLUTION 55 1 176 2 NIL (33 8) 493))
   (OPERATION (CLAUSE 502 R8 (164 57) ((505 . ANY))
   ((- 3 (505)) (- 20 (505 12)) ))
   (RESOLUTION 57 1 164 1 NIL (48 12) 502))
   (OPERATION (CLAUSE 529 R9 (502 52) () ((- 20 (2 12)) ))
   (RESOLUTION 52 1 502 1 NIL (505 2) 529))
   (OPERATION (CLAUSE 550 R10 (164 52) ((552 . ANY))
   ((- 13 (552)) (- 20 (2 552)) ))
   (RESOLUTION 52 1 164 2 NIL (47 2) 550))
   (OPERATION (CLAUSE 561 R11 (152 53) ((564 . ANY))
   ((- 3 (564)) (- 20 (564 4)) ))
   (RESOLUTION 53 1 152 1 NIL (30 4) 561))
   (OPERATION (CLAUSE 587 R12 (561 52) () ((- 20 (2 4)) ))
   (RESOLUTION 52 1 561 1 NIL (564 2) 587))
   (OPERATION (CLAUSE 613 R13 (152 52) ((615 . ANY))
   ((- 5 (615)) (- 20 (2 615)) ))
   (RESOLUTION 52 1 152 2 NIL (31 2) 613))
   (OPERATION (CLAUSE 624 R14 (141 53) ((627 . ANY))
   ((- 3 (627)) (+ 22 (4 627)) ))
   (RESOLUTION 53 1 141 1 NIL (28 4) 624))
   (OPERATION (CLAUSE 643 R15 (624 52) () ((+ 22 (4 2)) ))
   (RESOLUTION 52 1 624 1 NIL (627 2) 643))
   (OPERATION (CLAUSE 665 R16 (141 52) ((667 . ANY))
   ((- 5 (667)) (+ 22 (667 2)) ))
   (RESOLUTION 52 1 141 2 NIL (29 2) 665))
   (OPERATION (CLAUSE 633 R17 (130 54) ((634 . ANY))
   ((- 5 (634)) (+ 22 (6 634)) ))
   (RESOLUTION 54 1 130 1 NIL (26 6) 633))
   (OPERATION (CLAUSE 682 R18 (633 53) () ((+ 22 (6 4)) ))
   (RESOLUTION 53 1 633 1 NIL (634 4) 682))
   (OPERATION (CLAUSE 706 R19 (130 53) ((708 . ANY))
   ((- 7 (708)) (+ 22 (708 4)) ))
   (RESOLUTION 53 1 130 2 NIL (27 4) 706))
   (OPERATION (CLAUSE 674 R20 (119 56) ((707 . ANY))
   ((- 7 (707)) (+ 22 (10 707)) ))
   (RESOLUTION 56 1 119 1 NIL (46 10) 674))
   (OPERATION (CLAUSE 729 R21 (674 54) () ((+ 22 (10 6)) ))
   (RESOLUTION 54 1 674 1 NIL (707 6) 729))
   (OPERATION (CLAUSE 755 R22 (119 54) ((757 . ANY))
   ((- 11 (757)) (+ 22 (757 6)) ))
   (RESOLUTION 54 1 119 2 NIL (45 6) 755))
   (OPERATION (CLAUSE 681 R23 (110 55) ((756 . ANY))
   ((- 7 (756)) (+ 22 (8 756)) ))
   (RESOLUTION 55 1 110 1 NIL (24 8) 681))
   (OPERATION (CLAUSE 777 R24 (681 54) () ((+ 22 (8 6)) ))
   (RESOLUTION 54 1 681 1 NIL (756 6) 777))
   (OPERATION (CLAUSE 781 R25 (110 54) ((785 . ANY))
   ((- 9 (785)) (+ 22 (785 6)) ))
   (RESOLUTION 54 1 110 2 NIL (25 6) 781))
   (OPERATION (CLAUSE 807 R26 (103 56) () ((+ 20 (10 (49 10))) ))
   (RESOLUTION 56 1 103 1 NIL (51 10) 807))
   (OPERATION (CLAUSE 848 R27 (97 56) () ((+ 17 ((49 10))) ))
   (RESOLUTION 56 1 97 1 NIL (37 10) 848))
   (OPERATION (CLAUSE 855 R28 (92 55) () ((+ 20 (8 (49 8))) ))
   (RESOLUTION 55 1 92 1 NIL (50 8) 855))
   (OPERATION (CLAUSE 877 R29 (87 55) () ((+ 17 ((49 8))) ))
   (RESOLUTION 55 1 87 1 NIL (36 8) 877))
   (OPERATION (CLAUSE 883 R30 (84 57) () ((+ 17 (12)) ))
   (RESOLUTION 57 1 84 1 NIL (16 12) 883))
   (OPERATION (CLAUSE 893 R31 (77 56) () ((+ 15 (10)) ))
   (RESOLUTION 56 1 77 1 NIL (44 10) 893))
   (OPERATION (CLAUSE 923 R32 (71 55) () ((+ 15 (8)) ))
   (RESOLUTION 55 1 71 1 NIL (43 8) 923))
   (OPERATION (CLAUSE 947 R33 (66 54) () ((+ 15 (6)) ))
   (RESOLUTION 54 1 66 1 NIL (42 6) 947))
   (OPERATION (CLAUSE 971 R34 (62 53) () ((+ 15 (4)) ))
   (RESOLUTION 53 1 62 1 NIL (41 4) 971))
   (OPERATION (CLAUSE 995 R35 (59 52) () ((+ 15 (2)) ))
   (RESOLUTION 52 1 59 1 NIL (14 2) 995))
   (OPERATION (CLAUSE 1017 R36 (385 493) ()
   ((- 7 (6)) (- 11 (8)) ))
   (RESOLUTION 493 2 385 2 NIL (495 6 388 8) 1017))
   (OPERATION (CLAUSE 1017 R36 (385 493) ()
   ((- 7 (6)) (- 11 (8)) ))
   (REPLACEMENT.OPERATION NIL (1017 54)
  (((1017 . 1) (54 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 72 R37 (330 469) ()
   ((- 15 (6)) (- 15 (8)) (- 20 (8 12)) ))
   (RESOLUTION 469 1 330 3 NIL (333 6 334 8) 72))
   (OPERATION (CLAUSE 72 R37 (330 469) ()
   ((- 15 (6)) (- 15 (8)) (- 20 (8 12)) ))
   (REPLACEMENT.OPERATION (43 8) (72 71 55)
  (((72 . 2) (71 . 2) NIL) ((71 . 1) (55 . 1) NIL))
  (NIL NIL)
  NIL))
   (OPERATION (CLAUSE 72 R37 (330 469) ()
   ((- 15 (6)) (- 20 (8 12)) ))
   (REPLACEMENT.OPERATION (42 6) (72 66 54)
  (((72 . 1) (66 . 2) NIL) ((66 . 1) (54 . 1) NIL))
  (NIL NIL)
  NIL))
   (OPERATION (CLAUSE 827 R38 (193 469) ()
   ((- 7 (6)) (- 11 (8)) ))
   (RESOLUTION 469 1 193 3 NIL (34 6 35 8) 827))
   (OPERATION (CLAUSE 952 R39 (427 440) ()
   ((- 9 (10)) (- 7 (6)) ))
   (RESOLUTION 440 2 427 2 NIL (429 6 443 10) 952))
   (OPERATION (CLAUSE 952 R39 (427 440) ()
   ((- 9 (10)) (- 7 (6)) ))
   (REPLACEMENT.OPERATION NIL (952 54)
  (((952 . 2) (54 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 926 R40 (410 176) ()
   ((- 7 (6)) (- 9 (10)) ))
   (RESOLUTION 176 3 410 1 NIL (32 6 33 10) 926))(OPERATION (CLAUSE 1092 R41 (214 947) ()((- 17 (12))(+ 20 (6 12))(- 15 (10))(- 22 (10 6))(- 17 ((49 10)))(- 20 (10 (49 10)))(+ 20 (6 10))))(RESOLUTION 947 1 214 1 NIL (18 6 19 12 21 10 23 (49 10)) 1092))(OPERATION (CLAUSE 1092 R41 (214 947 883 893 410 807 848 729) () ((+ 20 (6 12)) ))(REPLACEMENT.OPERATION NIL (1092 883 893 410 807 848 729 )(((1092 . 1)(883 . 1)NIL)((1092 . 3)(893 . 1)NIL)((1092 . 7)(410 . 1)NIL)((1092 . 6)(807 . 1)NIL)((1092 . 5)(848 . 1)NIL)((1092 . 4)(729 . 1)NIL))( NIL  NIL  NIL  NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 1075 R42 (214 587) ()((- 15 (2))(- 17 (12))(+ 20 (2 12))(- 15 (4))(- 22 (4 2))(- 17 (12))(- 20 (4 12))))(RESOLUTION 214 8 587 1 NIL (18 2 19 12 21 4 23 12) 1075))(OPERATION (CLAUSE 1075 R42 (214 587 971 995 883 529 643) () ((- 20 (4 12)) ))(REPLACEMENT.OPERATION NIL (1075 971 995 883 529 643 )(((1075 . 4)(971 . 1)NIL)((1075 . 1)(995 . 1)NIL)((1075 . 2)(883 . 1)NIL)((1075 . 3)(529 . 1)NIL)((1075 . 5)(643 . 1)NIL))((((1075 . 2) (1075 . 6) NIL)) NIL  NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 1059 R43 (330 947) ()((- 15 (4))(- 20 (4 6))(- 20 (6 12))))(RESOLUTION 947 1 330 2 NIL (333 4 334 6) 1059))(OPERATION (CLAUSE 1059 R43 (330 947 971 1092) () ((- 20 (4 6)) ))(REPLACEMENT.OPERATION NIL (1059 971 1092 )(((1059 . 1)(971 . 1)NIL)((1059 . 3)(1092 . 1)NIL))( NIL  NIL )NIL))(OPERATION (CLAUSE 1046 R44 (214 1059) ()((- 15 (4))(- 17 (12))(+ 20 (4 12))(- 15 (6))(- 22 (6 4))(- 17 (12))(- 20 (6 12))))(RESOLUTION 214 8 1059 1 NIL (18 4 19 12 21 6 23 12) 1046))(OPERATION (CLAUSE 1046 R44 (214 1059 883 947 971 883 1075 682 1092) () ())(REPLACEMENT.OPERATION NIL (1046 883 947 971 883 1075 682 1092 )(((1046 . 6)(883 . 1)NIL)((1046 . 4)(947 . 1)NIL)((1046 . 1)(971 . 1)NIL)((1046 . 2)(883 . 1)NIL)((1046 . 3)(1075 . 1)NIL)((1046 . 5)(682 . 1)NIL)((1046 . 7)(1092 . 1)NIL))( NIL  NIL  NIL  NIL  NIL  NIL  NIL )NIL))
   (END.TIME 15403460)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   22 (DT-PREDICATE.CREATE "SMALLER" '(ANY ANY)))
   (CONS   20 (DT-PREDICATE.CREATE "EATS" '(ANY ANY)))
   (CONS   17 (DT-PREDICATE.CREATE "PLANT" '(ANY)))
   (CONS   15 (DT-PREDICATE.CREATE "ANIMAL" '(ANY)))
   (CONS   13 (DT-PREDICATE.CREATE "GRAIN" '(ANY)))
   (CONS   11 (DT-PREDICATE.CREATE "SNAIL" '(ANY)))
   (CONS    9 (DT-PREDICATE.CREATE "CATERPILLAR" '(ANY)))
   (CONS    7 (DT-PREDICATE.CREATE "BIRD" '(ANY)))
   (CONS    5 (DT-PREDICATE.CREATE "FOX" '(ANY)))
   (CONS    3 (DT-PREDICATE.CREATE "WOLF" '(ANY)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   12 (DT-CONSTANT.CREATE "muesli" 'ANY))
   (CONS   10 (DT-CONSTANT.CREATE "schnecki" 'ANY))
   (CONS    8 (DT-CONSTANT.CREATE "raupi" 'ANY))
   (CONS    6 (DT-CONSTANT.CREATE "tweety" 'ANY))
   (CONS    4 (DT-CONSTANT.CREATE "foxi" 'ANY))
   (CONS    2 (DT-CONSTANT.CREATE "lupo" 'ANY))
   (CONS   49 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 1046)
)