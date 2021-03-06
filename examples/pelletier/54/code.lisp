;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Unix" "06-AUG,1992 03:24" 
              ("Edit:     Axioms and Theorems edited: 05-MAR,1992 19:56 "
               ))

(AXIOMS.INFIX    ((* MONTAGUE (QUOTE S) PARADOX OF GROUNDED CLASSES *)
                  (* AXIOMS *)
                  (ALL Y (EX Z (ALL X ELEMENT-OF (X Z) EQV X = Y)))
                  (* Z = {Y} *)
                  (ALL X REG (X) EQV (ALL K ELEMENT-OF (X K) IMPL (EX Y ELEMENT-OF (Y K) AND NOT (EX Z ELEMENT-OF (Z K) AND ELEMENT-OF (Z Y)))))))

(THEOREMS.INFIX ((* THEOREMS *)
                  (NOT (EX W (ALL X ELEMENT-OF (X W) EQV REG (X))))
                  (* REG OF MONTAGUE DOESN (QUOTE T) EXIST *)))

(AXIOMS.PREFIX   (COMMENT
                  COMMENT
                  (ALL 3 (EX 4 (ALL 5 (EQV (+ 6 (5 4) (KIND (EQV T 40))) (+ 2 (5 3) (KIND (EQV NIL 40)))))))
                  COMMENT
                  (ALL 7 (EQV (+ 8 (7) (KIND (EQV T 60))) (ALL 9 (IMPL (+ 6 (7 9) (KIND (EQV NIL 60))) (EX 10 (AND (+ 6 (10 9) (KIND (EQV NIL 60))) (NOT (EX 11 (AND (+ 6 (11 9) (KIND (EQV NIL 60))) (+ 6 (11 10) (KIND (EQV NIL 60))))))))))))))

(THEOREMS.PREFIX (COMMENT
                  (NOT (EX 12 (ALL 13 (EQV (+ 6 (13 12) (KIND (EQV T 30))) (+ 8 (13) (KIND (EQV NIL 30)))))))
                  COMMENT))

(OPTIONS (TWO_RULES)
          (TWO_RULES.MAXLEVEL . 1)
          (TWO_SUPPRESS.NORULES)
          (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.I_CLAUSE.PURITY . PARTIAL)
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
          (RED.D_CLAUSE.PURITY . PARTIAL)
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
          (STR_R.SELECTION * 8 (+ 2 VARIABLES (* 2 DEPTH) (* 4 NOLIT)))
          (STR_LINK.DEPTH . 1)
          (STR_TERM.DEPTH)
          (STR_FINITE.DOMAIN . T)
          (TERM_UNITS . T)
          (TERM_ITERATIONS . 5)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS)
          (SORT_MAX.UNIFICATION.RULE.STEPS . 100)
          (SORT_MAX.UNIFICATION.TREE.DEPTH . 100)
          (SORT_MAX.UNIFICATION.TREE.OPEN.NODES . 20)
          (SORT_UNIFIER.STOP.NUMBER . 100)
          (SORT_SHOW.VARIABLE.SORTS . T)
          (ER_PARAMODULATION . HEURISTIC-COMPLETION)
          (ER_WEIGHT.POLYNOMIALS)
          (ER_P.SELECTION * WEIGHT (IF SUPPORT 1 1.5) (IF EQUATIONAL 1 2))
          (ER_COMPLETION . UNFAILING)
          (ER_CP.REDUCTION . T)
          (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
          (ER_OPERATOR.ORDERING * - + 0 1)
          (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
          (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ (* 2 Y) X)) (* (X Y) (+ (* X Y) X)) (- (X) (* X X)) (0 NIL 2) (1 NIL 2))
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
          (GEN_MAXIMUM.TIME)
          (GEN_GRAPH.SAVING)
          (GEN_SAVE.FILE . SAVEDEFAULT)
          (GEN_LISP.GARBAGE.COLLECTION)
          (GEN_COMMON.LISP . T)
          (GEN_OTHER.PROVER . MKRP)
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
          (PR_PROTOCOL . STANDARD)
          (PR_LEFT.MARGIN . 0)
          (PR_RIGHT.MARGIN . 117)
          (PR_LATEX)
          (PR_LINELENGTH . 117)
          (PR_LITERALS)
         )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 10950000)
        (PARTIAL   (CLAUSE 21 R.= AXIOM ((19 . ANY)) ((+ 2 (19 19)) ))(CLAUSE 24 A1 AXIOM ((3 . ANY)(5 . ANY))
                         ((+ 6 (5 (18 3))) (- 2 (5 3)) )))
        (PARTIAL   (CLAUSE 28 A2 AXIOM ((17 . ANY)(4 . ANY))
                         ((- 6 (4 (18 17))) (+ 2 (4 17)) )))
        (END.TIME 12460000)
        (FINAL 21 24 28)
        (SYMBOLS 
                 (LET (NEW.ADDRESS)
          (LIST (CONS    8 (DT-PREDICATE.CREATE "REG" '(ANY)))
                         (CONS    6 (DT-PREDICATE.CREATE "ELEMENT-OF" '(ANY ANY)))
                         (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
                         (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (CONS   18 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
        (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
          (START.TIME 12620000)
          (PARTIAL   (CLAUSE 43 T3 THEOREM ((13 . ANY))
                           ((+ 6 (13 37)) (+ 6 (13 (38 13))) )))
          (PARTIAL   (CLAUSE 46 T4 THEOREM ((40 . ANY)(41 . ANY))
                           ((+ 6 (41 37)) (- 6 (40 (38 41))) (+ 6 ((39 40 41) (38 41))) )))
          (PARTIAL   (CLAUSE 52 T5 THEOREM ((15 . ANY)(42 . ANY))
                           ((+ 6 (42 37)) (- 6 (15 (38 42))) (+ 6 ((39 15 42) 15)) )))
          (PARTIAL   (CLAUSE 68 T6 THEOREM ((34 . ANY)(12 . ANY))
                           ((- 6 (12 37)) (- 6 (12 34)) (+ 6 ((16 34 12) 34)) )))
          (PARTIAL   (CLAUSE 97 T7 THEOREM ((35 . ANY)(33 . ANY)(14 . ANY))
                           ((- 6 (14 37)) (- 6 (14 33)) (- 6 (35 33)) (- 6 (35 (16 33 14))) )))
          (INITIAL   (CLAUSE 21 R.= AXIOM ((19 . ANY)) ((+ 2 (19 19)) ))
(CLAUSE 24 A1 AXIOM ((3 . ANY)(5 . ANY))
                           ((+ 6 (5 (18 3))) (- 2 (5 3)) ))
(CLAUSE 28 A2 AXIOM ((17 . ANY)(4 . ANY))
                           ((- 6 (4 (18 17))) (+ 2 (4 17)) ))
(CLAUSE 43 T3 THEOREM ((13 . ANY))
                           ((+ 6 (13 37)) (+ 6 (13 (38 13))) ))
(CLAUSE 46 T4 THEOREM ((40 . ANY)(41 . ANY))
                           ((+ 6 (41 37)) (- 6 (40 (38 41))) (+ 6 ((39 40 41) (38 41))) ))
(CLAUSE 52 T5 THEOREM ((15 . ANY)(42 . ANY))
                           ((+ 6 (42 37)) (- 6 (15 (38 42))) (+ 6 ((39 15 42) 15)) ))
(CLAUSE 68 T6 THEOREM ((34 . ANY)(12 . ANY))
                           ((- 6 (12 37)) (- 6 (12 34)) (+ 6 ((16 34 12) 34)) ))
(CLAUSE 97 T7 THEOREM ((35 . ANY)(33 . ANY)(14 . ANY))
                           ((- 6 (14 37)) (- 6 (14 33)) (- 6 (35 33)) (- 6 (35 (16 33 14))) )) )
          (END.TIME 20920000)
          (FINAL 21 24 28 43 46 52 68 97)
          (SYMBOLS 
                   (LET (NEW.ADDRESS)
          (LIST (CONS    8 (DT-PREDICATE.CREATE "REG" '(ANY)))
                           (CONS    6 (DT-PREDICATE.CREATE "ELEMENT-OF" '(ANY ANY)))
                           (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
                           (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (CONS   37 (DT-CONSTANT.CREATE "c_1" 'ANY))
                           (CONS   16 (DT-FUNCTION.CREATE "f_4" 'ANY '(ANY ANY) 'NIL))
                           (CONS   39 (DT-FUNCTION.CREATE "f_3" 'ANY '(ANY ANY) 'NIL))
                           (CONS   38 (DT-FUNCTION.CREATE "f_2" 'ANY '(ANY) 'NIL))
                           (CONS   18 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
          (RESULT))
(indices ((97 3 4 6 7) (68 3 4 5) (52 3 5 7) (46 3 5 6) (43 3 4) (28 1 2) (24 1 2) (21 NIL)))

(SPLITPARTS "Unix" "06-AUG,1992 03:25" NIL)

(OPTIONS (TWO_RULES)
          (TWO_RULES.MAXLEVEL . 1)
          (TWO_SUPPRESS.NORULES)
          (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.I_CLAUSE.PURITY . PARTIAL)
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
          (RED.D_CLAUSE.PURITY . PARTIAL)
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
          (STR_R.SELECTION * 8 (+ 2 VARIABLES (* 2 DEPTH) (* 4 NOLIT)))
          (STR_LINK.DEPTH . 1)
          (STR_TERM.DEPTH)
          (STR_FINITE.DOMAIN . T)
          (TERM_UNITS . T)
          (TERM_ITERATIONS . 5)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS)
          (SORT_MAX.UNIFICATION.RULE.STEPS . 100)
          (SORT_MAX.UNIFICATION.TREE.DEPTH . 100)
          (SORT_MAX.UNIFICATION.TREE.OPEN.NODES . 20)
          (SORT_UNIFIER.STOP.NUMBER . 100)
          (SORT_SHOW.VARIABLE.SORTS . T)
          (ER_PARAMODULATION . HEURISTIC-COMPLETION)
          (ER_WEIGHT.POLYNOMIALS)
          (ER_P.SELECTION * WEIGHT (IF SUPPORT 1 1.5) (IF EQUATIONAL 1 2))
          (ER_COMPLETION . UNFAILING)
          (ER_CP.REDUCTION . T)
          (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
          (ER_OPERATOR.ORDERING * - + 0 1)
          (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
          (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ (* 2 Y) X)) (* (X Y) (+ (* X Y) X)) (- (X) (* X X)) (0 NIL 2) (1 NIL 2))
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
          (GEN_MAXIMUM.TIME)
          (GEN_GRAPH.SAVING)
          (GEN_SAVE.FILE . SAVEDEFAULT)
          (GEN_LISP.GARBAGE.COLLECTION)
          (GEN_COMMON.LISP . T)
          (GEN_OTHER.PROVER . MKRP)
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
          (PR_PROTOCOL . STANDARD)
          (PR_LEFT.MARGIN . 0)
          (PR_RIGHT.MARGIN . 117)
          (PR_LATEX)
          (PR_LINELENGTH . 117)
          (PR_LITERALS)
         )

(REFUTATION (START.TIME 21550000)

(OPTIONS (TWO_RULES)
          (TWO_RULES.MAXLEVEL . 1)
          (TWO_SUPPRESS.NORULES)
          (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.I_CLAUSE.PURITY . PARTIAL)
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
          (RED.D_CLAUSE.PURITY . PARTIAL)
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
          (STR_R.SELECTION * 8 (+ 2 VARIABLES (* 2 DEPTH) (* 4 NOLIT)))
          (STR_LINK.DEPTH . 1)
          (STR_TERM.DEPTH)
          (STR_FINITE.DOMAIN . T)
          (TERM_UNITS . T)
          (TERM_ITERATIONS . 5)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS)
          (SORT_MAX.UNIFICATION.RULE.STEPS . 100)
          (SORT_MAX.UNIFICATION.TREE.DEPTH . 100)
          (SORT_MAX.UNIFICATION.TREE.OPEN.NODES . 20)
          (SORT_UNIFIER.STOP.NUMBER . 100)
          (SORT_SHOW.VARIABLE.SORTS . T)
          (ER_PARAMODULATION . HEURISTIC-COMPLETION)
          (ER_WEIGHT.POLYNOMIALS)
          (ER_P.SELECTION * WEIGHT (IF SUPPORT 1 1.5) (IF EQUATIONAL 1 2))
          (ER_COMPLETION . UNFAILING)
          (ER_CP.REDUCTION . T)
          (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
          (ER_OPERATOR.ORDERING * - + 0 1)
          (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
          (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ (* 2 Y) X)) (* (X Y) (+ (* X Y) X)) (- (X) (* X X)) (0 NIL 2) (1 NIL 2))
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
          (GEN_MAXIMUM.TIME)
          (GEN_GRAPH.SAVING)
          (GEN_SAVE.FILE . SAVEDEFAULT)
          (GEN_LISP.GARBAGE.COLLECTION)
          (GEN_COMMON.LISP . T)
          (GEN_OTHER.PROVER . MKRP)
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
          (PR_PROTOCOL . STANDARD)
          (PR_LEFT.MARGIN . 0)
          (PR_RIGHT.MARGIN . 117)
          (PR_LATEX)
          (PR_LINELENGTH . 117)
          (PR_LITERALS)
         )
            (INITIAL   (CLAUSE 21 R.= AXIOM ((19 . ANY)) ((+ 2 (19 19)) ))
(CLAUSE 24 A1 AXIOM ((3 . ANY)(5 . ANY))
                             ((+ 6 (5 (18 3))) (- 2 (5 3)) ))
(CLAUSE 28 A2 AXIOM ((17 . ANY)(4 . ANY))
                             ((- 6 (4 (18 17))) (+ 2 (4 17)) ))
(CLAUSE 43 T3 THEOREM ((13 . ANY))
                             ((+ 6 (13 37)) (+ 6 (13 (38 13))) ))
(CLAUSE 46 T4 THEOREM ((40 . ANY)(41 . ANY))
                             ((+ 6 (41 37)) (- 6 (40 (38 41))) (+ 6 ((39 40 41) (38 41))) ))
(CLAUSE 52 T5 THEOREM ((15 . ANY)(42 . ANY))
                             ((+ 6 (42 37)) (- 6 (15 (38 42))) (+ 6 ((39 15 42) 15)) ))
(CLAUSE 68 T6 THEOREM ((34 . ANY)(12 . ANY))
                             ((- 6 (12 37)) (- 6 (12 34)) (+ 6 ((16 34 12) 34)) ))
(CLAUSE 97 T7 THEOREM ((35 . ANY)(33 . ANY)(14 . ANY))
                             ((- 6 (14 37)) (- 6 (14 33)) (- 6 (35 33)) (- 6 (35 (16 33 14))) )) )(OPERATION (CLAUSE 149 R1 (24 21) ((153 . ANY))((+ 6 (153 (18 153)))))(RESOLUTION 21 1 24 2 SYMMETRIC (5 19 3 19) 149))
            (OPERATION (CLAUSE 147 R2 (46 43) ((157 . ANY))
                               ((+ 6 (157 37)) (+ 6 (157 37)) (+ 6 ((39 157 157) (38 157))) ))
                       (RESOLUTION 43 2 46 2 NIL (40 13 41 13) 147))
            (OPERATION (CLAUSE 147 R2 (46 43) ((157 . ANY))
                               ((+ 6 (157 37)) (+ 6 ((39 157 157) (38 157))) ))
                       (DOUBLE.LITERAL 2 1 NIL 147))
            (OPERATION (CLAUSE 170 R3 (52 43) ((183 . ANY))
                               ((+ 6 (183 37)) (+ 6 (183 37)) (+ 6 ((39 183 183) 183)) ))
                       (RESOLUTION 43 2 52 2 NIL (15 13 42 13) 170))
            (OPERATION (CLAUSE 170 R3 (52 43) ((183 . ANY))
                               ((+ 6 (183 37)) (+ 6 ((39 183 183) 183)) ))
                       (DOUBLE.LITERAL 2 1 NIL 170))
            (OPERATION (CLAUSE 215 R4 (68 43) ((208 . ANY))
                               ((+ 6 (208 (38 208))) (- 6 (208 37)) (+ 6 ((16 37 208) 37)) ))
                       (RESOLUTION 43 1 68 2 NIL (12 13 34 37) 215))
            (OPERATION (CLAUSE 215 R4 (68 43) ((208 . ANY))
                               ((+ 6 (208 (38 208))) (- 6 (208 37)) (+ 6 ((16 37 208) 37)) ))
                       (REPLACEMENT.OPERATION (208 13) (215 43)
  (((215 . 2) (43 . 1) NIL))
  ((((215 . 1) (43 . 2) NIL)))
  ((13 . ANY))))
            (OPERATION (CLAUSE 222 R5 (28 52) ((224 . ANY)(225 . ANY))
                               ((+ 6 (225 37)) (- 6 ((18 224) (38 225))) (+ 2 ((39 (18 224) 225) 224)) ))
                       (RESOLUTION 52 3 28 1 NIL (4 (39 (18 17) 42) 15 (18 17)) 222))
            (OPERATION (CLAUSE 54 R6 (52 46) ((251 . ANY)(252 . ANY))
                               ((+ 6 (252 37)) (- 6 (251 (38 252))) (+ 6 (252 37)) (+ 6 ((39 (39 251 252) 252) (39 251 252))) ))
                       (RESOLUTION 46 3 52 2 NIL (15 (39 40 41) 42 41) 54))
            (OPERATION (CLAUSE 54 R6 (52 46) ((252 . ANY)(251 . ANY))
                               ((- 6 (251 (38 252))) (+ 6 (252 37)) (+ 6 ((39 (39 251 252) 252) (39 251 252))) ))
                       (DOUBLE.LITERAL 3 1 NIL 54))
            (OPERATION (CLAUSE 289 R7 (28 68) ((275 . ANY)(294 . ANY))
                               ((- 6 (294 37)) (- 6 (294 (18 275))) (+ 2 ((16 (18 275) 294) 275)) ))
                       (RESOLUTION 68 3 28 1 NIL (4 (16 (18 17) 12) 34 (18 17)) 289))
            (OPERATION (CLAUSE 325 R8 (68 24) ((321 . ANY)(295 . ANY))
                               ((- 2 (295 321)) (- 6 (295 37)) (+ 6 ((16 (18 321) 295) (18 321))) ))
                       (RESOLUTION 24 1 68 2 NIL (12 5 34 (18 3)) 325))
            (OPERATION (CLAUSE 362 R9 (68 43) ((324 . ANY)(363 . ANY))
                               ((+ 6 (363 (38 363))) (- 6 (363 324)) (+ 6 ((16 324 363) 324)) ))
                       (RESOLUTION 43 1 68 1 NIL (12 13) 362))
            (OPERATION (CLAUSE 438 R10 (97 43) ((434 . ANY)(432 . ANY))
                               ((+ 6 (432 (38 432))) (- 6 (434 37)) (- 6 (434 37)) (- 6 (432 (16 37 434))) ))
                       (RESOLUTION 43 1 97 3 NIL (35 13 33 37) 438))
            (OPERATION (CLAUSE 438 R10 (97 43) ((434 . ANY)(432 . ANY))
                               ((+ 6 (432 (38 432))) (- 6 (434 37)) (- 6 (432 (16 37 434))) ))
                       (DOUBLE.LITERAL 3 2 NIL 438))
            (OPERATION (CLAUSE 105 R11 (46 52) ((482 . ANY)(484 . ANY))
                               ((+ 6 (484 37)) (- 6 ((38 482) (38 484))) (+ 6 (482 37)) (+ 6 ((39 (39 (38 482) 484) 482) (38 482))) ))
                       (RESOLUTION 52 3 46 2 NIL (40 (39 (38 41) 42) 15 (38 41)) 105))
            (OPERATION (CLAUSE 548 R12 (52 68) ((545 . ANY)(551 . ANY))
                               ((- 6 (551 37)) (- 6 (551 (38 545))) (+ 6 (545 37)) (+ 6 ((39 (16 (38 545) 551) 545) (16 (38 545) 551))) ))
                       (RESOLUTION 68 3 52 2 NIL (15 (16 (38 42) 12) 34 (38 42)) 548))
            (OPERATION (CLAUSE 627 R13 (46 68) ((624 . ANY)(630 . ANY))
                               ((- 6 (630 37)) (- 6 (630 (38 624))) (+ 6 (624 37)) (+ 6 ((39 (16 (38 624) 630) 624) (38 624))) ))
                       (RESOLUTION 68 3 46 2 NIL (40 (16 (38 41) 12) 34 (38 41)) 627))
            (OPERATION (CLAUSE 715 R14 (68 52) ((718 . ANY)(719 . ANY))
                               ((- 6 (719 (38 718))) (+ 6 ((39 719 718) 719)) (- 6 (718 37)) (+ 6 ((16 37 718) 37)) ))
                       (RESOLUTION 52 1 68 2 NIL (12 42 34 37) 715))
            (OPERATION (CLAUSE 715 R14 (68 52) ((718 . ANY)(719 . ANY))
                               ((- 6 (719 (38 718))) (+ 6 ((39 719 718) 719)) (- 6 (718 37)) (+ 6 ((16 37 718) 37)) ))
                       (REPLACEMENT.OPERATION (718 42 719 15) (715 52)
  (((715 . 3) (52 . 1) NIL))
  ((((715 . 1) (52 . 2) NIL) ((715 . 2) (52 . 3) NIL)))
  ((15 . ANY) (42 . ANY))))
            (OPERATION (CLAUSE 758 R15 (68 52) ((748 . ANY)(747 . ANY))
                               ((+ 6 (747 37)) (- 6 (748 (38 747))) (- 6 ((39 748 747) 37)) (+ 6 ((16 748 (39 748 747)) 748)) ))
                       (RESOLUTION 52 3 68 2 NIL (12 (39 15 42) 34 15) 758))
            (OPERATION (CLAUSE 745 R16 (68 46) ((933 . ANY)(935 . ANY))
                               ((- 6 (935 (38 933))) (+ 6 ((39 935 933) (38 933))) (- 6 (933 37)) (+ 6 ((16 37 933) 37)) ))
                       (RESOLUTION 46 1 68 2 NIL (12 41 34 37) 745))
            (OPERATION (CLAUSE 745 R16 (68 46) ((933 . ANY)(935 . ANY))
                               ((- 6 (935 (38 933))) (+ 6 ((39 935 933) (38 933))) (- 6 (933 37)) (+ 6 ((16 37 933) 37)) ))
                       (REPLACEMENT.OPERATION (933 41 935 40) (745 46)
  (((745 . 3) (46 . 1) NIL))
  ((((745 . 1) (46 . 2) NIL) ((745 . 2) (46 . 3) NIL)))
  ((40 . ANY) (41 . ANY))))
            (OPERATION (CLAUSE 983 R17 (68 46) ((977 . ANY)(978 . ANY))
                               ((+ 6 (978 37)) (- 6 (977 (38 978))) (- 6 ((39 977 978) 37)) (+ 6 ((16 (38 978) (39 977 978)) (38 978))) ))
                       (RESOLUTION 46 3 68 2 NIL (12 (39 40 41) 34 (38 41)) 983))
            (OPERATION (CLAUSE 1120 R18 (68 52) ((979 . ANY)(1123 . ANY))
                               ((+ 6 (1123 37)) (- 6 (37 (38 1123))) (- 6 ((39 37 1123) 979)) (+ 6 ((16 979 (39 37 1123)) 979)) ))
                       (RESOLUTION 52 3 68 1 NIL (12 (39 37 42) 15 37) 1120))
            (OPERATION (CLAUSE 1287 R19 (97 43) ((1285 . ANY)(1278 . ANY))
                               ((+ 6 (1278 37)) (- 6 (1285 37)) (- 6 (1285 (38 1278))) (- 6 (1278 (16 (38 1278) 1285))) ))
                       (RESOLUTION 43 2 97 3 NIL (35 13 33 (38 13)) 1287))
            (OPERATION (CLAUSE 1398 R20 (97 43) ((1403 . ANY)(1404 . ANY))
                               ((+ 6 (1404 (38 1404))) (- 6 (1404 37)) (- 6 (1403 37)) (- 6 (1403 (16 37 1404))) ))
                       (RESOLUTION 43 1 97 2 NIL (14 13 33 37) 1398))
            (OPERATION (CLAUSE 1398 R20 (97 43) ((1403 . ANY)(1404 . ANY))
                               ((+ 6 (1404 (38 1404))) (- 6 (1404 37)) (- 6 (1403 37)) (- 6 (1403 (16 37 1404))) ))
                       (REPLACEMENT.OPERATION (1404 13) (1398 43)
  (((1398 . 2) (43 . 1) NIL))
  ((((1398 . 1) (43 . 2) NIL)))
  ((13 . ANY))))
            (OPERATION (CLAUSE 1460 R21 (68 52) ((1465 . ANY)(1466 . ANY)(1467 . ANY))
                               ((- 6 (1467 (38 1466))) (+ 6 ((39 1467 1466) 1467)) (- 6 (1466 1465)) (+ 6 ((16 1465 1466) 1465)) ))
                       (RESOLUTION 52 1 68 1 NIL (12 42) 1460))
            (OPERATION (CLAUSE 1724 R22 (68 46) ((1719 . ANY)(1718 . ANY)(1717 . ANY))
                               ((- 6 (1717 (38 1718))) (+ 6 ((39 1717 1718) (38 1718))) (- 6 (1718 1719)) (+ 6 ((16 1719 1718) 1719)) ))
                       (RESOLUTION 46 1 68 1 NIL (12 41) 1724))
            (OPERATION (CLAUSE 1948 R23 (97 52) ((1944 . ANY)(1941 . ANY)(1924 . ANY))
                               ((- 6 (1924 (38 1941))) (+ 6 ((39 1924 1941) 1924)) (- 6 (1944 37)) (- 6 (1944 37)) (- 6 (1941 (16 37 1944))) ))
                       (RESOLUTION 52 1 97 3 NIL (35 42 33 37) 1948))
            (OPERATION (CLAUSE 1948 R23 (97 52) ((1924 . ANY)(1944 . ANY)(1941 . ANY))
                               ((- 6 (1924 (38 1941))) (+ 6 ((39 1924 1941) 1924)) (- 6 (1944 37)) (- 6 (1941 (16 37 1944))) ))
                       (DOUBLE.LITERAL 3 4 NIL 1948))
            (OPERATION (CLAUSE 1959 R24 (97 46) ((1963 . ANY)(1966 . ANY)(1967 . ANY))
                               ((- 6 (1967 (38 1966))) (+ 6 ((39 1967 1966) (38 1966))) (- 6 (1963 37)) (- 6 (1963 37)) (- 6 (1966 (16 37 1963))) ))
                       (RESOLUTION 46 1 97 3 NIL (35 41 33 37) 1959))
            (OPERATION (CLAUSE 1959 R24 (97 46) ((1967 . ANY)(1963 . ANY)(1966 . ANY))
                               ((- 6 (1967 (38 1966))) (+ 6 ((39 1967 1966) (38 1966))) (- 6 (1963 37)) (- 6 (1966 (16 37 1963))) ))
                       (DOUBLE.LITERAL 3 4 NIL 1959))
            (OPERATION (CLAUSE 1986 R25 (97 24) ((2002 . ANY)(2159 . ANY)(2160 . ANY))
                               ((- 2 (2160 2159)) (- 6 (2002 37)) (- 6 (2002 (18 2159))) (- 6 (2160 (16 (18 2159) 2002))) ))
                       (RESOLUTION 24 1 97 3 NIL (35 5 33 (18 3)) 1986))
            (OPERATION (CLAUSE 2384 R26 (97 24) ((2390 . ANY)(2392 . ANY)(2391 . ANY))
                               ((- 2 (2391 2392)) (- 6 (2391 37)) (- 6 (2390 (18 2392))) (- 6 (2390 (16 (18 2392) 2391))) ))
                       (RESOLUTION 24 1 97 2 NIL (14 5 33 (18 3)) 2384))
            (OPERATION (CLAUSE 2479 R27 (97 68) ((2485 . ANY)(2484 . ANY)(2486 . ANY))
                               ((- 6 (2486 37)) (- 6 (2486 37)) (- 6 ((16 37 2486) 2484)) (- 6 (2485 2484)) (- 6 (2485 (16 2484 (16 37 2486)))) ))
                       (RESOLUTION 68 3 97 1 NIL (14 (16 37 12) 34 37) 2479))
            (OPERATION (CLAUSE 2479 R27 (97 68) ((2486 . ANY)(2484 . ANY)(2485 . ANY))
                               ((- 6 (2486 37)) (- 6 ((16 37 2486) 2484)) (- 6 (2485 2484)) (- 6 (2485 (16 2484 (16 37 2486)))) ))
                       (DOUBLE.LITERAL 1 2 NIL 2479))
            (OPERATION (CLAUSE 2713 R28 (97 43) ((2707 . ANY)(2708 . ANY)(2706 . ANY))
                               ((+ 6 (2706 (38 2706))) (- 6 (2706 2708)) (- 6 (2707 2708)) (- 6 (2707 (16 2708 2706))) ))
                       (RESOLUTION 43 1 97 1 NIL (14 13) 2713))
            (OPERATION (CLAUSE 3047 R29 (97 68) ((3051 . ANY)(3054 . ANY)(3056 . ANY))
                               ((- 6 (3056 37)) (- 6 (3056 (16 3054 3051))) (- 6 (3051 37)) (- 6 (3051 3054)) (- 6 ((16 (16 3054 3051) 3056) 3054)) ))
                       (RESOLUTION 68 3 97 4 NIL (35 (16 (16 33 14) 12) 34 (16 33 14)) 3047))