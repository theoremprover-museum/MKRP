;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Unix" "03-JUN,1992 21:52" 
              ("Edit:     Axioms and Theorems edited: 02-NOV,1989 20:11 "
               ))

(AXIOMS.INFIX    ((* NO AXIOMS *)
                  (P (F (A B) F (B C)))
                  (P (F (B C) F (A C)))
                  (ALL |X,Y,Z| P (X Y) AND P (Y Z) IMPL P (X Z))))

(THEOREMS.INFIX ((* THEOREMS *)
                  (P (F (A B) F (A C)))))

(AXIOMS.PREFIX   (COMMENT
                  (+ 6 ((4 2 3) (4 3 5)) NIL)
                  (+ 6 ((4 3 5) (4 2 5)) NIL)
                  (ALL 9 (ALL 8 (ALL 7 (IMPL (AND (+ 6 (7 8) NIL) (+ 6 (8 9) NIL)) (+ 6 (7 9) NIL)))))))

(THEOREMS.PREFIX (COMMENT
                  (+ 6 ((4 2 3) (4 2 5)) NIL)))

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

(AXIOMS (START.TIME 6420000)
        (PARTIAL   (CLAUSE 10 A1 AXIOM () ((+ 6 ((4 2 3) (4 3 5))) )))
        (PARTIAL   (CLAUSE 11 A2 AXIOM () ((+ 6 ((4 3 5) (4 2 5))) )))
        (PARTIAL   (CLAUSE 15 A3 AXIOM ((9 . ANY)(8 . ANY)(7 . ANY))
                         ((- 6 (7 8)) (- 6 (8 9)) (+ 6 (7 9)) )))
        (END.TIME 7180000)
        (FINAL 10 11 15)
        (SYMBOLS 
                 (LET (NEW.ADDRESS)
          (LIST (CONS    6 (DT-PREDICATE.CREATE "P" '(ANY ANY)))
                         (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (CONS    5 (DT-CONSTANT.CREATE "c" 'ANY))
                         (CONS    3 (DT-CONSTANT.CREATE "b" 'ANY))
                         (CONS    2 (DT-CONSTANT.CREATE "a" 'ANY))
                         (CONS    4 (DT-FUNCTION.CREATE "f" 'ANY '(ANY ANY) 'NIL)))))
        (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
          (START.TIME 7210000)
          (PARTIAL   (CLAUSE 28 T4 THEOREM () ((- 6 ((4 2 3) (4 2 5))) )))