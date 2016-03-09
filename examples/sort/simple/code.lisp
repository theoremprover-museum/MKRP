;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Unix" "03-JUN,1992 18:07" 
              ("Edit:     Axioms and Theorems edited: 26-FEB,1992 14:38 "
               ))

(AXIOMS.INFIX    ((E (A SHIT) OR E (A BULLSHIT))
                  (ALL X |:| SHIT P (X))
                  (ALL X |:| BULLSHIT P (X))
                  (ALL X |:| SHIT NOT P (X))))

(THEOREMS.INFIX ((EX X |:| BULLSHIT P (X))))

(AXIOMS.PREFIX   ((OR (+ 4 (5 6) NIL) (+ 4 (5 7) NIL))
                  (ALL 8 (+ 9 (8) NIL))
                  (ALL 10 (+ 9 (10) NIL))
                  (ALL 11 (NOT (+ 9 (11) NIL)))))

(THEOREMS.PREFIX ((EX 12 (+ 9 (12) NIL))))

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
          (STR_R.SELECTION * 10 (+ 2 VARIABLES (* 2 DEPTH) (* 3 NOLIT)))
          (STR_LINK.DEPTH)
          (STR_TERM.DEPTH)
          (STR_FINITE.DOMAIN . T)
          (TERM_UNITS . T)
          (TERM_ITERATIONS . 0)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS . T)
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
          (GEN_GRAPH.SAVING)
          (GEN_SAVE.FILE . SAVEDEFAULT)
          (GEN_LISP.GARBAGE.COLLECTION)
          (GEN_COMMON.LISP . T)
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

(AXIOMS (START.TIME 16480000)
        (PARTIAL   (CLAUSE 15 R.= AXIOM ((13 . 3)) ((+ 2 (13 13)) ))(CLAUSE 17 A1 AXIOM ((8 . 6)) ((+ 9 (8)) )))
        (PARTIAL   (CLAUSE 19 A2 AXIOM ((10 . 7)) ((+ 9 (10)) )))
        (PARTIAL   (CLAUSE 21 A3 AXIOM ((11 . 6)) ((- 9 (11)) )))
        (PARTIAL   (CLAUSE 22 A4 AXIOM ()
                         ((+ 4 (5 6)) (+ 4 (5 7)) )))
        (END.TIME 17180000)
        (FINAL 15 17 19 21 22)
        (SYMBOLS 
                 (LET (NEW.ADDRESS)
          (LIST (CONS    9 (DT-PREDICATE.CREATE "P" '(ANY)))
                         (PROG1 (CONS    4 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "E" '(ANY ANY))))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
                         (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (CONS    7 (DT-CONSTANT.CREATE "bullshit" 'ANY))
                         (CONS    6 (DT-CONSTANT.CREATE "shit" 'ANY))
                         (CONS    5 (DT-CONSTANT.CREATE "a" 'ANY))
                         (CONS    3 (DT-CONSTANT.CREATE "omega" 'ANY)))))
        (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
          (START.TIME 17220000)
          (PARTIAL   (CLAUSE 24 T5 THEOREM ((12 . 7)) ((- 9 (12)) )))
          (OPERATION (CLAUSE 19 A2 AXIOM ((10 . 7)) ((+ 9 (10)) ))
                     (REPLACEMENT.OPERATION (12 10) (19 24)
  (((19 . 1) (24 . 1) NIL))
  (NIL)
  ((10 . 7))))
          (END.TIME 17540000)
          (FINAL 15 19 22 24)
          (SYMBOLS 
                   (LET (NEW.ADDRESS)
          (LIST (CONS    9 (DT-PREDICATE.CREATE "P" '(ANY)))
                           (PROG1 (CONS    4 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "E" '(ANY ANY))))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
                           (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (CONS    7 (DT-CONSTANT.CREATE "bullshit" 'ANY))
                           (CONS    6 (DT-CONSTANT.CREATE "shit" 'ANY))
                           (CONS    5 (DT-CONSTANT.CREATE "a" 'ANY))
                           (CONS    3 (DT-CONSTANT.CREATE "omega" 'ANY)))))
          (RESULT SUCCESS 19))
(indices ((24 6) (22 1 2) (21 5) (19 4) (17 3) (15 NIL)))