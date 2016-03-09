;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.2 IP-TCP 422.9 IP-TCP-DOC 404.0 CLX 419.3 X Remote Screen 418.1 X Documentation 403.0 Network RPC 415.5 NFS Client 415.3 NFS Documentation 404.0 Logical Pathnames Translation Files NEWEST KKL 24.0 HADES 19.0 Waltz 8.0 COLUMN 9.0 Experimental MKRP 53.7 GENTRAFO 3.0" "22-APR,1992 11:12" 
   ("Edit:     Axioms and Theorems edited: 22-APR,1992 11:12 "
   ))

(AXIOMS.INFIX    ((* AXIOMS *)
   (ALL X F (X) OR G (X) IMPL NOT H (X))
   (ALL X (G (X) IMPL NOT I (X)) IMPL F (X) AND H (X))))

(THEOREMS.INFIX ((* THEOREMS *)
   (ALL X I (X))))

(AXIOMS.PREFIX   (COMMENT
   (ALL 3 (IMPL (OR (+ 4 (3) NIL) (+ 5 (3) NIL)) (NOT (+ 6 (3) NIL))))
   (ALL 7 (IMPL (IMPL (+ 5 (7) NIL) (NOT (+ 8 (7) NIL))) (AND (+ 4 (7) NIL) (+ 6 (7) NIL))))))

(THEOREMS.PREFIX (COMMENT
   (ALL 9 (+ 8 (9) NIL))))

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
    (PR_PROTOCOL . POST)
    (PR_LEFT.MARGIN . 0)
    (PR_RIGHT.MARGIN . 117)
    (PR_LATEX)
    (PR_LINELENGTH . 117)
    (PR_LITERALS)
   )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 61112546)
   (PARTIAL   (CLAUSE ((X_14 ANY)) (= X_14 X_14))(CLAUSE ((X_3 ANY)) (NOT (F X_3)) (NOT (H X_3))))
   (PARTIAL   (CLAUSE ((X_10 ANY)) (NOT (G X_10)) (NOT (H X_10))))
   (PARTIAL   (CLAUSE ((X_7 ANY)) (G X_7) (F X_7)))
   (PARTIAL   (CLAUSE ((X_12 ANY)) (G X_12) (H X_12)))
   (PARTIAL   (CLAUSE ((X_11 ANY)) (I X_11) (F X_11)))
   (PARTIAL   (CLAUSE ((X_13 ANY)) (I X_13) (H X_13)))
   (END.TIME 61113927)
   (FINAL 16 18 20 23 27 33 37)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS    8 (DT-PREDICATE.CREATE "I" '(ANY)))
   (CONS    6 (DT-PREDICATE.CREATE "H" '(ANY)))
   (CONS    5 (DT-PREDICATE.CREATE "G" '(ANY)))
   (CONS    4 (DT-PREDICATE.CREATE "F" '(ANY)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED))))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 61113946)
   (PARTIAL   (CLAUSE () (NOT (I C_1))))
   (INITIAL   (CLAUSE ((X_14 ANY)) (= X_14 X_14))
(CLAUSE ((X_3 ANY)) (NOT (F X_3)) (NOT (H X_3)))
(CLAUSE ((X_10 ANY)) (NOT (G X_10)) (NOT (H X_10)))
(CLAUSE ((X_7 ANY)) (G X_7) (F X_7))
(CLAUSE ((X_11 ANY)) (I X_11) (F X_11))
(CLAUSE ((X_13 ANY)) (I X_13) (H X_13))
(CLAUSE () (NOT (I C_1)))   )
   (END.TIME 61114580)
   (FINAL 16 18 20 23 33 37 9)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS    8 (DT-PREDICATE.CREATE "I" '(ANY)))
   (CONS    6 (DT-PREDICATE.CREATE "H" '(ANY)))
   (CONS    5 (DT-PREDICATE.CREATE "G" '(ANY)))
   (CONS    4 (DT-PREDICATE.CREATE "F" '(ANY)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   42 (DT-CONSTANT.CREATE "c_1" 'ANY)))))
   (RESULT))
(indices ((9 8) (37 5 7) (33 5 6) (27 4 7) (23 4 6) (20 2 3) (18 1 3) (16 NIL)))

(SPLITPARTS "Genera 8.0.2 IP-TCP 422.9 IP-TCP-DOC 404.0 CLX 419.3 X Remote Screen 418.1 X Documentation 403.0 Network RPC 415.5 NFS Client 415.3 NFS Documentation 404.0 Logical Pathnames Translation Files NEWEST KKL 24.0 HADES 19.0 Waltz 8.0 COLUMN 9.0 Experimental MKRP 53.7 GENTRAFO 3.0" "22-APR,1992 11:12" NIL)

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
    (PR_PROTOCOL . POST)
    (PR_LEFT.MARGIN . 0)
    (PR_RIGHT.MARGIN . 117)
    (PR_LATEX)
    (PR_LINELENGTH . 117)
    (PR_LITERALS)
   )

(REFUTATION (START.TIME 61115724)

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
    (PR_PROTOCOL . POST)
    (PR_LEFT.MARGIN . 0)
    (PR_RIGHT.MARGIN . 117)
    (PR_LATEX)
    (PR_LINELENGTH . 117)
    (PR_LITERALS)
   )
   (INITIAL   (CLAUSE ((X_14 ANY)) (= X_14 X_14))
(CLAUSE ((X_3 ANY)) (NOT (F X_3)) (NOT (H X_3)))
(CLAUSE ((X_10 ANY)) (NOT (G X_10)) (NOT (H X_10)))
(CLAUSE ((X_7 ANY)) (G X_7) (F X_7))
(CLAUSE ((X_11 ANY)) (I X_11) (F X_11))
(CLAUSE ((X_13 ANY)) (I X_13) (H X_13))
(CLAUSE () (NOT (I C_1)))   )
(30 (CLAUSE () (F C_1))
 (RESOLUTION ((33 1) (9 1) NIL) (substitution (X_11) (C_1)))))
(28 (CLAUSE () (H C_1))
 (RESOLUTION ((37 1) (9 1) NIL) (substitution (X_13) (C_1)))))(CLAUSE () (H C_1))
   (REPLACEMENT.OPERATION (3 42) (28 18 30)
  (((28 . 1) (18 . 2) NIL) ((18 . 1) (30 . 1) NIL))
  (NIL NIL)
  NIL))
   (END.TIME 61116941)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS    8 (DT-PREDICATE.CREATE "I" '(ANY)))
   (CONS    6 (DT-PREDICATE.CREATE "H" '(ANY)))
   (CONS    5 (DT-PREDICATE.CREATE "G" '(ANY)))
   (CONS    4 (DT-PREDICATE.CREATE "F" '(ANY)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   42 (DT-CONSTANT.CREATE "c_1" 'ANY)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 28)
)