;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 PS 415.1 RPC 415.0 Embedding Support 407.0 UX Support 416.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Documentation 403.0 KKL 19.2 HADES 14.20 Waltz 6.0 COLUMN 6.0 Experimental MKRP 48.4" "09-JUL,1991 16:11" 
   ("Edit:     Axioms and Theorems edited: 09-JUL,1991 16:11 "
   ))

(AXIOMS.INFIX    ((* PROBLEM E91)
   (*)
   (* THIS TIME WE GET THE FOLLOWING STATEMENTS |:|)
   (* A |:| AT LEAST ONE OF THE THREE OF US IS A KNAVE.)
   (* B |:| C IS A KNIGHT.)
   (* GIVEN THAT THERE IS EXACTLY ONE WEREWOLF AND THAT HE IS A |KNIGHT,|)
   (* WHO IS THE WEREWOLF?)
   (*)
   (*)
   (* THE FUNCTION STATE (X) AND THE PREDICATES TRUTH (X) |,| KNIGHT (X) |,| KNAVE (X) |,|)
   (* AND WEREWOLF (X) MEAN THE SAME AS IN E89.)
   (* THE FOLLOWING PREDICATES ARE NEW |:|)
   (* STATE.KNIGHT (X Y) - STATEMENT X TELLS THAT PERSON Y IS A KNIGHT)
   (* AT-LEAST-ONE-KNAVE (X Y Z) - AT LEAST ONE PERSON |X,Y,OR| Z IS A KNAVE)
   (* STATE.AT-LEAST-ONE-KNAVE (S X Y Z) - STATEMENT S TELLS THAT AT LEAST ONE)
   (* PERSON |X,Y,| OR Z IS A KNAVE)
   (*)
   (* DECLARATIONS |:|)
   (TYPE |A,B,C| |:| PERSON)
   (TYPE STATE (PERSON) |:| STATEMENT)
   (TYPE TRUTH (STATEMENT))
   (TYPE KNIGHT (PERSON))
   (TYPE KNAVE (PERSON))
   (TYPE WEREWOLF (PERSON))
   (TYPE STATE.KNIGHT (STATEMENT PERSON))
   (TYPE AT-LEAST-ONE-KNAVE (PERSON PERSON PERSON))
   (TYPE STATE.AT-LEAST-ONE-KNAVE (STATEMENT PERSON PERSON PERSON))
   (*)
   (* EVERY INHABITANT IS EITHER A KNIGHT OR A KNAVE |:|)
   (ALL X |:| PERSON KNIGHT (X) OR KNAVE (X))
   (ALL X |:| PERSON NOT (KNIGHT (X) AND KNAVE (X)))
   (*)
   (* KNIGHT ALWAYS TELL THE TRUTH AND KNAVE ALWAYS LIE |:|)
   (ALL X |:| PERSON KNIGHT (X) EQV TRUTH (STATE (X)))
   (ALL X |:| PERSON KNAVE (X) EQV NOT (TRUTH (STATE (X))))
   (*)
   (* A WEREWOLF CAN BE A KNIGHT OR A KNAVE |:|)
   (ALL X |:| PERSON WEREWOLF (X) IMPL KNIGHT (X) OR KNAVE (X))
   (*)
   (* THERE IS EXACTLY ONE WEREWOLF |:|)
   (WEREWOLF (A) OR WEREWOLF (B) OR WEREWOLF (C))
   (NOT (WEREWOLF (A) AND WEREWOLF (B) OR WEREWOLF (B) AND WEREWOLF (C) OR WEREWOLF (C) AND WEREWOLF (A)))
   (*)
   (* EXPLANATION OF AT-LEAST-ONE-KNAVE |:|)
   (ALL |X,Y,Z| |:| PERSON AT-LEAST-ONE-KNAVE (X Y Z) EQV KNAVE (X) OR KNAVE (Y) OR KNAVE (Z))
   (*)
   (* IMPLICATIONS ABOUT STATEMENTS |:|)
   (ALL X |:| STATEMENT ALL Y |:| PERSON STATE.KNIGHT (X Y) AND TRUTH (X) IMPL KNIGHT (Y))
   (ALL X |:| STATEMENT ALL Y |:| PERSON STATE.KNIGHT (X Y) AND NOT (TRUTH (X)) IMPL NOT (KNIGHT (Y)))
   (ALL S |:| STATEMENT ALL |X,Y,Z| |:| PERSON STATE.AT-LEAST-ONE-KNAVE (S X Y Z) AND TRUTH (S) IMPL AT-LEAST-ONE-KNAVE (X Y Z))
   (ALL S |:| STATEMENT ALL |X,Y,Z| |:| PERSON STATE.AT-LEAST-ONE-KNAVE (S X Y Z) AND NOT (TRUTH (S)) IMPL NOT (AT-LEAST-ONE-KNAVE (X Y Z)))
   (*)
   (* THE WEREWOLF IS A KNIGHT |:|)
   (ALL X |:| PERSON WEREWOLF (X) IMPL KNIGHT (X))
   (*)
   (* ACTUAL STATEMENTS |:|)
   (STATE.AT-LEAST-ONE-KNAVE (STATE (A) A B C))
   (STATE.KNIGHT (STATE (B) C))))

(THEOREMS.INFIX ((* THEOREM FOR E91)
   (WEREWOLF (A))))

(AXIOMS.PREFIX   (COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   COMMENT
   COMMENT
   (ALL 14 (OR (+ 8 (14) NIL) (+ 9 (14) NIL)))
   (ALL 15 (NOT (AND (+ 8 (15) NIL) (+ 9 (15) NIL))))
   COMMENT
   COMMENT
   (ALL 16 (EQV (+ 8 (16) (KIND (EQV T 350))) (+ 7 ((6 16)) (KIND (EQV NIL 350)))))
   (ALL 17 (EQV (+ 9 (17) (KIND (EQV T 360))) (NOT (+ 7 ((6 17)) (KIND (EQV NIL 360))))))
   COMMENT
   COMMENT
   (ALL 18 (IMPL (+ 10 (18) NIL) (OR (+ 8 (18) NIL) (+ 9 (18) NIL))))
   COMMENT
   COMMENT
   (OR (+ 10 (3) NIL) (OR (+ 10 (4) NIL) (+ 10 (5) NIL)))
   (NOT (OR (AND (+ 10 (3) NIL) (+ 10 (4) NIL)) (OR (AND (+ 10 (4) NIL) (+ 10 (5) NIL)) (AND (+ 10 (5) NIL) (+ 10 (3) NIL)))))
   COMMENT
   COMMENT
   (ALL 21 (ALL 20 (ALL 19 (EQV (+ 12 (19 20 21) (KIND (EQV T 460))) (OR (+ 9 (19) (KIND (EQV NIL 460))) (OR (+ 9 (20) (KIND (EQV NIL 460))) (+ 9 (21) (KIND (EQV NIL 460)))))))))
   COMMENT
   COMMENT
   (ALL 22 (ALL 23 (IMPL (AND (+ 11 (22 23) NIL) (+ 7 (22) NIL)) (+ 8 (23) NIL))))
   (ALL 24 (ALL 25 (IMPL (AND (+ 11 (24 25) NIL) (NOT (+ 7 (24) NIL))) (NOT (+ 8 (25) NIL)))))
   (ALL 26 (ALL 29 (ALL 28 (ALL 27 (IMPL (AND (+ 13 (26 27 28 29) NIL) (+ 7 (26) NIL)) (+ 12 (27 28 29) NIL))))))
   (ALL 30 (ALL 33 (ALL 32 (ALL 31 (IMPL (AND (+ 13 (30 31 32 33) NIL) (NOT (+ 7 (30) NIL))) (NOT (+ 12 (31 32 33) NIL)))))))
   COMMENT
   COMMENT
   (ALL 34 (IMPL (+ 10 (34) NIL) (+ 8 (34) NIL)))
   COMMENT
   COMMENT
   (+ 13 ((6 3) 3 4 5) NIL)
   (+ 11 ((6 4) 5) NIL)))

(THEOREMS.PREFIX (COMMENT
   (+ 10 (3) NIL)))

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
    (STR_PAR-RES . 10)
    (STR_E-RESOLUTION)
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_R.DEMODULATION . T)
    (STR_P.DEMODULATION . T)
    (STR_INDUCTION)
    (STR_FINITE.DOMAIN . T)
    (STR_SORT.LITERALS)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 0)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.VARIABLES X Y Z U V W)
    (ER_POLYNOMIAL.WEIGHT (+ (+ X (* 2 Y))) (* (+ X (* X Y))) (- (+ (* 2 X) -1)) (0 2) (1 2))
    (ER_LINK.DEPTH.WEIGHT . 0)
    (ER_UNIT.FOCUS . 0)
    (ER_UNIT.FOCUS.WEIGHT . 1)
    (ER_EQUATION.FOCUS . 2)
    (ER_NARROW.DEPTH . 0)
    (ER_NARROW.NEXT . DEPTH)
    (ER_NARROW.TEST NORM C DELTA SL N)
    (ER_NARROW.LIMIT.FACTOR . 0)
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
    (PR_STATISTICS)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
    (PR_LITERALS . T)
   )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 148655621)
   (PARTIAL   (CLAUSE 45 R.= AXIOM ((43 . ANY)) ((+ 2 (43 43)) ))(CLAUSE 46 A1 AXIOM () ((+ 13 ((6 3) 3 4 5)) )))
   (PARTIAL   (CLAUSE 47 A2 AXIOM () ((+ 11 ((6 4) 5)) )))
   (PARTIAL   (CLAUSE 49 A3 AXIOM ((14 . PERSON))
   ((+ 7 ((6 14))) (- 7 ((6 14))) )))
   (PARTIAL   (CLAUSE 51 A4 AXIOM ((15 . PERSON))
   ((- 7 ((6 15))) (+ 7 ((6 15))) )))
   (PARTIAL   (CLAUSE 52 A5 AXIOM ()
   ((- 10 (3)) (- 10 (4)) )))
   (PARTIAL   (CLAUSE 53 A6 AXIOM ()
   ((- 10 (4)) (- 10 (5)) )))
   (PARTIAL   (CLAUSE 55 A7 AXIOM ()
   ((- 10 (5)) (- 10 (3)) )))
   (PARTIAL   (CLAUSE 59 A8 AXIOM ((34 . PERSON))
   ((- 10 (34)) (+ 7 ((6 34))) )))
   (PARTIAL   (CLAUSE 67 A9 AXIOM ((18 . PERSON))
   ((- 10 (18)) (+ 7 ((6 18))) (- 7 ((6 18))) )))
   (PARTIAL   (CLAUSE 68 A10 AXIOM ()
   ((+ 10 (3)) (+ 10 (4)) (+ 10 (5)) )))
   (PARTIAL   (CLAUSE 80 A11 AXIOM ((23 . PERSON)(22 . STATEMENT))
   ((- 11 (22 23)) (- 7 (22)) (+ 7 ((6 23))) )))
   (PARTIAL   (CLAUSE 88 A12 AXIOM ((25 . PERSON)(24 . STATEMENT))
   ((- 11 (24 25)) (+ 7 (24)) (- 7 ((6 25))) )))
   (PARTIAL   (CLAUSE 103 A13 AXIOM ((33 . PERSON)(32 . PERSON)(31 . PERSON)(30 . STATEMENT))
   ((- 13 (30 31 32 33)) (+ 7 (30)) (+ 7 ((6 31))) )))
   (PARTIAL   (CLAUSE 121 A14 AXIOM ((39 . PERSON)(37 . PERSON)(41 . PERSON)(35 . STATEMENT))
   ((- 13 (35 41 37 39)) (+ 7 (35)) (+ 7 ((6 37))) )))
   (PARTIAL   (CLAUSE 144 A15 AXIOM ((40 . PERSON)(38 . PERSON)(42 . PERSON)(36 . STATEMENT))
   ((- 13 (36 42 38 40)) (+ 7 (36)) (+ 7 ((6 40))) )))
   (PARTIAL   (CLAUSE 172 A16 AXIOM ((29 . PERSON)(28 . PERSON)(27 . PERSON)(26 . STATEMENT))
   ((- 13 (26 27 28 29)) (- 7 (26)) (- 7 ((6 27))) (- 7 ((6 28))) (- 7 ((6 29))) )))
   (END.TIME 148677825)
   (FINAL 45 46 47 52 53 55 59 68 80 88 103 121 144 172)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   13 (DT-PREDICATE.CREATE "STATE.AT-LEAST-ONE-KNAVE" '(STATEMENT PERSON PERSON PERSON)))
   (CONS   12 (DT-PREDICATE.CREATE "AT-LEAST-ONE-KNAVE" '(PERSON PERSON PERSON)))
   (CONS   11 (DT-PREDICATE.CREATE "STATE.KNIGHT" '(STATEMENT PERSON)))
   (CONS   10 (DT-PREDICATE.CREATE "WEREWOLF" '(PERSON)))
   (CONS    9 (DT-PREDICATE.CREATE "KNAVE" '(PERSON)))
   (CONS    8 (DT-PREDICATE.CREATE "KNIGHT" '(PERSON)))
   (CONS    7 (DT-PREDICATE.CREATE "TRUTH" '(STATEMENT)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    5 (DT-CONSTANT.CREATE "c" 'PERSON))
   (CONS    4 (DT-CONSTANT.CREATE "b" 'PERSON))
   (CONS    3 (DT-CONSTANT.CREATE "a" 'PERSON))
   (CONS    6 (DT-FUNCTION.CREATE "state" 'STATEMENT '(PERSON) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 148677999)
   (PARTIAL   (CLAUSE 233 T17 THEOREM () ((- 10 (3)) )))
   (OPERATION (CLAUSE 68 A10 AXIOM ()
   ((+ 10 (3)) (+ 10 (4)) (+ 10 (5)) ))
   (REPLACEMENT.OPERATION NIL (68 233)
  (((68 . 1) (233 . 1) NIL))
  (NIL)
  NIL))
   (INITIAL   (CLAUSE 45 R.= AXIOM ((43 . ANY)) ((+ 2 (43 43)) ))
(CLAUSE 46 A1 AXIOM () ((+ 13 ((6 3) 3 4 5)) ))
(CLAUSE 47 A2 AXIOM () ((+ 11 ((6 4) 5)) ))
(CLAUSE 59 A8 AXIOM ((34 . PERSON))
   ((- 10 (34)) (+ 7 ((6 34))) ))
(CLAUSE 68 A10 THEOREM ()
   ((+ 10 (4)) (+ 10 (5)) ))
(CLAUSE 80 A11 AXIOM ((23 . PERSON)(22 . STATEMENT))
   ((- 11 (22 23)) (- 7 (22)) (+ 7 ((6 23))) ))
(CLAUSE 88 A12 AXIOM ((25 . PERSON)(24 . STATEMENT))
   ((- 11 (24 25)) (+ 7 (24)) (- 7 ((6 25))) ))
(CLAUSE 103 A13 AXIOM ((33 . PERSON)(32 . PERSON)(31 . PERSON)(30 . STATEMENT))
   ((- 13 (30 31 32 33)) (+ 7 (30)) (+ 7 ((6 31))) ))
(CLAUSE 121 A14 AXIOM ((39 . PERSON)(37 . PERSON)(41 . PERSON)(35 . STATEMENT))
   ((- 13 (35 41 37 39)) (+ 7 (35)) (+ 7 ((6 37))) ))
(CLAUSE 144 A15 AXIOM ((40 . PERSON)(38 . PERSON)(42 . PERSON)(36 . STATEMENT))
   ((- 13 (36 42 38 40)) (+ 7 (36)) (+ 7 ((6 40))) ))
(CLAUSE 172 A16 AXIOM ((29 . PERSON)(28 . PERSON)(27 . PERSON)(26 . STATEMENT))
   ((- 13 (26 27 28 29)) (- 7 (26)) (- 7 ((6 27))) (- 7 ((6 28))) (- 7 ((6 29))) ))   )
   (END.TIME 148686832)
   (FINAL 45 46 47 59 68 80 88 103 121 144 172)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   13 (DT-PREDICATE.CREATE "STATE.AT-LEAST-ONE-KNAVE" '(STATEMENT PERSON PERSON PERSON)))
   (CONS   12 (DT-PREDICATE.CREATE "AT-LEAST-ONE-KNAVE" '(PERSON PERSON PERSON)))
   (CONS   11 (DT-PREDICATE.CREATE "STATE.KNIGHT" '(STATEMENT PERSON)))
   (CONS   10 (DT-PREDICATE.CREATE "WEREWOLF" '(PERSON)))
   (CONS    9 (DT-PREDICATE.CREATE "KNAVE" '(PERSON)))
   (CONS    8 (DT-PREDICATE.CREATE "KNIGHT" '(PERSON)))
   (CONS    7 (DT-PREDICATE.CREATE "TRUTH" '(STATEMENT)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    5 (DT-CONSTANT.CREATE "c" 'PERSON))
   (CONS    4 (DT-CONSTANT.CREATE "b" 'PERSON))
   (CONS    3 (DT-CONSTANT.CREATE "a" 'PERSON))
   (CONS    6 (DT-FUNCTION.CREATE "state" 'STATEMENT '(PERSON) 'NIL)))))
   (RESULT))
(indices ((233 37) (172 23 24 25 26 27) (144 28 29 32) (121 28 29 31) (103 28 29 30) (88 20 21 22) (80 17 18 19) (68 8 9 10) (67 5 6 7) (59 33 34) (55 15 16) (53 13 14) (52 11 12) (51 3 4) (49 1 2) (47 36) (46 35) (45 NIL)))

(SPLITPARTS "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 PS 415.1 RPC 415.0 Embedding Support 407.0 UX Support 416.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Documentation 403.0 KKL 19.2 HADES 14.20 Waltz 6.0 COLUMN 6.0 Experimental MKRP 48.4" "09-JUL,1991 16:12" NIL)

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
    (STR_PAR-RES . 10)
    (STR_E-RESOLUTION)
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_R.DEMODULATION . T)
    (STR_P.DEMODULATION . T)
    (STR_INDUCTION)
    (STR_FINITE.DOMAIN . T)
    (STR_SORT.LITERALS)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 0)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.VARIABLES X Y Z U V W)
    (ER_POLYNOMIAL.WEIGHT (+ (+ X (* 2 Y))) (* (+ X (* X Y))) (- (+ (* 2 X) -1)) (0 2) (1 2))
    (ER_LINK.DEPTH.WEIGHT . 0)
    (ER_UNIT.FOCUS . 0)
    (ER_UNIT.FOCUS.WEIGHT . 1)
    (ER_EQUATION.FOCUS . 2)
    (ER_NARROW.DEPTH . 0)
    (ER_NARROW.NEXT . DEPTH)
    (ER_NARROW.TEST NORM C DELTA SL N)
    (ER_NARROW.LIMIT.FACTOR . 0)
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
    (PR_STATISTICS)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
    (PR_LITERALS . T)
   )

(REFUTATION (START.TIME 148689398)

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
    (STR_PAR-RES . 10)
    (STR_E-RESOLUTION)
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_R.DEMODULATION . T)
    (STR_P.DEMODULATION . T)
    (STR_INDUCTION)
    (STR_FINITE.DOMAIN . T)
    (STR_SORT.LITERALS)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 0)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST)
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.VARIABLES X Y Z U V W)
    (ER_POLYNOMIAL.WEIGHT (+ (+ X (* 2 Y))) (* (+ X (* X Y))) (- (+ (* 2 X) -1)) (0 2) (1 2))
    (ER_LINK.DEPTH.WEIGHT . 0)
    (ER_UNIT.FOCUS . 0)
    (ER_UNIT.FOCUS.WEIGHT . 1)
    (ER_EQUATION.FOCUS . 2)
    (ER_NARROW.DEPTH . 0)
    (ER_NARROW.NEXT . DEPTH)
    (ER_NARROW.TEST NORM C DELTA SL N)
    (ER_NARROW.LIMIT.FACTOR . 0)
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
    (PR_STATISTICS)
    (PR_PROTOCOL . T)
    (PR_LEFT.MARGIN . 3)
    (PR_RIGHT.MARGIN . 117)
    (PR_LINELENGTH . 114)
    (PR_LITERALS . T)
   )
   (INITIAL   (CLAUSE 45 R.= AXIOM ((43 . ANY)) ((+ 2 (43 43)) ))
(CLAUSE 46 A1 AXIOM () ((+ 13 ((6 3) 3 4 5)) ))
(CLAUSE 47 A2 AXIOM () ((+ 11 ((6 4) 5)) ))
(CLAUSE 59 A8 AXIOM ((34 . PERSON))
   ((- 10 (34)) (+ 7 ((6 34))) ))
(CLAUSE 68 A10 THEOREM ()
   ((+ 10 (4)) (+ 10 (5)) ))
(CLAUSE 80 A11 AXIOM ((23 . PERSON)(22 . STATEMENT))
   ((- 11 (22 23)) (- 7 (22)) (+ 7 ((6 23))) ))
(CLAUSE 88 A12 AXIOM ((25 . PERSON)(24 . STATEMENT))
   ((- 11 (24 25)) (+ 7 (24)) (- 7 ((6 25))) ))
(CLAUSE 103 A13 AXIOM ((33 . PERSON)(32 . PERSON)(31 . PERSON)(30 . STATEMENT))
   ((- 13 (30 31 32 33)) (+ 7 (30)) (+ 7 ((6 31))) ))
(CLAUSE 121 A14 AXIOM ((39 . PERSON)(37 . PERSON)(41 . PERSON)(35 . STATEMENT))
   ((- 13 (35 41 37 39)) (+ 7 (35)) (+ 7 ((6 37))) ))
(CLAUSE 144 A15 AXIOM ((40 . PERSON)(38 . PERSON)(42 . PERSON)(36 . STATEMENT))
   ((- 13 (36 42 38 40)) (+ 7 (36)) (+ 7 ((6 40))) ))
(CLAUSE 172 A16 AXIOM ((29 . PERSON)(28 . PERSON)(27 . PERSON)(26 . STATEMENT))
   ((- 13 (26 27 28 29)) (- 7 (26)) (- 7 ((6 27))) (- 7 ((6 28))) (- 7 ((6 29))) ))   )
   (OPERATION (CLAUSE 240 R1 (172 46) ()
   ((- 7 ((6 3))) (- 7 ((6 3))) (- 7 ((6 4))) (- 7 ((6 5))) ))
   (RESOLUTION 46 1 172 1 NIL (26 (6 3) 27 3 28 4 29 5) 240))
   (OPERATION (CLAUSE 240 R1 (172 46) ()
   ((- 7 ((6 3))) (- 7 ((6 4))) (- 7 ((6 5))) ))
   (DOUBLE.LITERAL 1 2 NIL 240))
   (OPERATION (CLAUSE 266 R2 (144 46) ()
   ((+ 7 ((6 3))) (+ 7 ((6 5))) ))
   (RESOLUTION 46 1 144 1 NIL (36 (6 3) 42 3 38 4 40 5) 266))
   (OPERATION (CLAUSE 188 R3 (121 46) ()
   ((+ 7 ((6 3))) (+ 7 ((6 4))) ))
   (RESOLUTION 46 1 121 1 NIL (35 (6 3) 41 3 37 4 39 5) 188))
   (OPERATION (CLAUSE 213 R4 (103 46) ()
   ((+ 7 ((6 3))) (+ 7 ((6 3))) ))
   (RESOLUTION 46 1 103 1 NIL (30 (6 3) 31 3 32 4 33 5) 213))
   (OPERATION (CLAUSE 213 R4 (103 46) () ((+ 7 ((6 3))) ))
   (DOUBLE.LITERAL 1 2 NIL 213))
   (OPERATION (CLAUSE 240 R1 (172 46) ()
   ((- 7 ((6 3))) (- 7 ((6 4))) (- 7 ((6 5))) ))
   (REPLACEMENT.OPERATION NIL (240 213)
  (((240 . 1) (213 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 149 R5 (88 47) ()
   ((+ 7 ((6 4))) (- 7 ((6 5))) ))
   (RESOLUTION 47 1 88 1 NIL (24 (6 4) 25 5) 149))
   (OPERATION (CLAUSE 149 R5 (88 47) ()
   ((+ 7 ((6 4))) (- 7 ((6 5))) ))
   (REPLACEMENT.OPERATION NIL (149 240)
  (((149 . 1) (240 . 1) NIL))
  ((((149 . 2) (240 . 2) NIL)))
  NIL))
   (OPERATION (CLAUSE 160 R6 (80 47) ()
   ((- 7 ((6 4))) (+ 7 ((6 5))) ))
   (RESOLUTION 47 1 80 1 NIL (22 (6 4) 23 5) 160))
   (OPERATION (CLAUSE 160 R6 (80 47) ()
   ((- 7 ((6 4))) (+ 7 ((6 5))) ))
   (REPLACEMENT.OPERATION NIL (160 149)
  (((160 . 2) (149 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 152 R7 (59 68) ()
   ((+ 10 (5)) (+ 7 ((6 4))) ))
   (RESOLUTION 68 1 59 1 NIL (34 4) 152))
   (OPERATION (CLAUSE 152 R7 (59 68) ()
   ((+ 10 (5)) (+ 7 ((6 4))) ))
   (REPLACEMENT.OPERATION NIL (152 160)
  (((152 . 2) (160 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 152 R7 (59 68) () ((+ 10 (5)) ))
   (REPLACEMENT.OPERATION (34 5) (152 59 149)
  (((152 . 1) (59 . 1) NIL) ((59 . 2) (149 . 1) NIL))
  (NIL NIL)
  NIL))
   (END.TIME 148711910)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   13 (DT-PREDICATE.CREATE "STATE.AT-LEAST-ONE-KNAVE" '(STATEMENT PERSON PERSON PERSON)))
   (CONS   12 (DT-PREDICATE.CREATE "AT-LEAST-ONE-KNAVE" '(PERSON PERSON PERSON)))
   (CONS   11 (DT-PREDICATE.CREATE "STATE.KNIGHT" '(STATEMENT PERSON)))
   (CONS   10 (DT-PREDICATE.CREATE "WEREWOLF" '(PERSON)))
   (CONS    9 (DT-PREDICATE.CREATE "KNAVE" '(PERSON)))
   (CONS    8 (DT-PREDICATE.CREATE "KNIGHT" '(PERSON)))
   (CONS    7 (DT-PREDICATE.CREATE "TRUTH" '(STATEMENT)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    5 (DT-CONSTANT.CREATE "c" 'PERSON))
   (CONS    4 (DT-CONSTANT.CREATE "b" 'PERSON))
   (CONS    3 (DT-CONSTANT.CREATE "a" 'PERSON))
   (CONS    6 (DT-FUNCTION.CREATE "state" 'STATEMENT '(PERSON) 'NIL)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 152)
)