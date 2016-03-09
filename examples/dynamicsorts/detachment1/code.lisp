;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 RPC 415.0 Embedding Support 407.0 UX Support 416.0 Experimental Network RPC 415.0 Experimental NFS Client 415.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Remote Screen 418.1 KKL 24.0 HADES 19.0 Experimental MKRP 53.11" "02-NOV,1992 17:04" 
   ("Edit:     Axioms and Theorems edited: 26-JUN,1992 16:17 "
   ))

(AXIOMS.INFIX    ((ALL |X,Y,Z| E (I (I (X Y) I (I (Y Z) I (X Z))) P))
   (ALL X |:| P (ALL Y (NOT E (I (X Y) P) OR E (Y P))))
   (ALL X E (I (I (N (X) X) X) P))
   (ALL |X,Y| E (I (X I (N (X) Y)) P))))

(THEOREMS.INFIX ((ALL X E (I (X X) P))))

(AXIOMS.PREFIX   ((ALL 7 (ALL 6 (ALL 5 (+ 4 ((8 (8 5 6) (8 (8 6 7) (8 5 7))) 9) NIL))))
   (ALL 10 (ALL 11 (OR (NOT (+ 4 ((8 10 11) 9) NIL)) (+ 4 (11 9) NIL))))
   (ALL 12 (+ 4 ((8 (8 (13 12) 12) 12) 9) NIL))
   (ALL 15 (ALL 14 (+ 4 ((8 14 (8 (13 14) 15)) 9) NIL)))))

(THEOREMS.PREFIX ((ALL 16 (+ 4 ((8 16 16) 9) NIL))))

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS)
    (RED.I_CLAUSE.PURITY)
    (RED.I_CLAUSE.TAUTOLOGY)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.I_CLAUSE.SUBSUMPTION)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.I_CLAUSE.REPL.FACTORING)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK)
    (RED.I_CLAUSE.REPL.RESOLUTION)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK)
    (RED.I_CLAUSE.REWRITING)
    (RED.I_LINK.INCOMPATIBILITY)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS)
    (RED.D_CLAUSE.PURITY . PARTIAL)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION)
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
    (GEN_COMMON.LISP)
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

(AXIOMS (START.TIME 3486037918)
   (PARTIAL   (CLAUSE 19 R.= AXIOM ((17 . 3)) ((+ 2 (17 17)) ))(CLAUSE 23 A1 AXIOM ((7 . 3)(6 . 3)(5 . 3)) ((+ 4 ((8 (8 5 6) (8 (8 6 7) (8 5 7))) 9)) )))
   (PARTIAL   (CLAUSE 25 A2 AXIOM ((12 . 3)) ((+ 4 ((8 (8 (13 12) 12) 12) 9)) )))
   (PARTIAL   (CLAUSE 28 A3 AXIOM ((15 . 3)(14 . 3)) ((+ 4 ((8 14 (8 (13 14) 15)) 9)) )))
   (PARTIAL   (CLAUSE 31 A4 AXIOM ((11 . 3)(10 . 9))
   ((- 4 ((8 10 11) 9)) (+ 4 (11 9)) )))
   (END.TIME 3486041747)
   (FINAL 19 23 25 28 31)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    4 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "E" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    9 (DT-CONSTANT.CREATE "p" 'ANY))
   (CONS    3 (DT-CONSTANT.CREATE "omega" 'ANY))
   (CONS   13 (DT-FUNCTION.CREATE "n" 'ANY '(ANY) 'NIL))
   (CONS    8 (DT-FUNCTION.CREATE "i" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 3486041864)
   (PARTIAL   (CLAUSE 16 T5 THEOREM () ((- 4 ((8 32 32) 9)) )))
   (PARTIAL   (CLAUSE 33 T6 THEOREM () ((+ 4 (32 3)) )))
   (INITIAL   (CLAUSE 19 R.= AXIOM ((17 . 3)) ((+ 2 (17 17)) ))
(CLAUSE 23 A1 AXIOM ((7 . 3)(6 . 3)(5 . 3)) ((+ 4 ((8 (8 5 6) (8 (8 6 7) (8 5 7))) 9)) ))
(CLAUSE 25 A2 AXIOM ((12 . 3)) ((+ 4 ((8 (8 (13 12) 12) 12) 9)) ))
(CLAUSE 28 A3 AXIOM ((15 . 3)(14 . 3)) ((+ 4 ((8 14 (8 (13 14) 15)) 9)) ))
(CLAUSE 31 A4 AXIOM ((11 . 3)(10 . 9))
   ((- 4 ((8 10 11) 9)) (+ 4 (11 9)) ))
(CLAUSE 16 T5 THEOREM () ((- 4 ((8 32 32) 9)) ))
(CLAUSE 33 T6 THEOREM () ((+ 4 (32 3)) ))   )
   (END.TIME 3486046046)
   (FINAL 19 23 25 28 31 16 33)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    4 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "E" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   32 (DT-CONSTANT.CREATE "c_1" '3))
   (CONS    9 (DT-CONSTANT.CREATE "p" 'ANY))
   (CONS    3 (DT-CONSTANT.CREATE "omega" 'ANY))
   (CONS   13 (DT-FUNCTION.CREATE "n" 'ANY '(ANY) 'NIL))
   (CONS    8 (DT-FUNCTION.CREATE "i" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT))
(indices ((33 NIL) (16 6) (31 2 3) (28 5) (25 4) (23 1) (19 NIL)))

(SPLITPARTS "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 RPC 415.0 Embedding Support 407.0 UX Support 416.0 Experimental Network RPC 415.0 Experimental NFS Client 415.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Remote Screen 418.1 KKL 24.0 HADES 19.0 Experimental MKRP 53.11" "02-NOV,1992 17:04" NIL)

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS)
    (RED.I_CLAUSE.PURITY)
    (RED.I_CLAUSE.TAUTOLOGY)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.I_CLAUSE.SUBSUMPTION)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.I_CLAUSE.REPL.FACTORING)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK)
    (RED.I_CLAUSE.REPL.RESOLUTION)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK)
    (RED.I_CLAUSE.REWRITING)
    (RED.I_LINK.INCOMPATIBILITY)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS)
    (RED.D_CLAUSE.PURITY . PARTIAL)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION)
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
    (GEN_COMMON.LISP)
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

(REFUTATION (START.TIME 3486048231)

(OPTIONS (TWO_RULES)
    (TWO_RULES.MAXLEVEL . 1)
    (TWO_SUPPRESS.NORULES)
    (RED.I_CLAUSE.MULTIPLE.LITERALS)
    (RED.I_CLAUSE.PURITY)
    (RED.I_CLAUSE.TAUTOLOGY)
    (RED.I_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.I_CLAUSE.SUBSUMPTION)
    (RED.I_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.I_CLAUSE.REPL.FACTORING)
    (RED.I_CLAUSE.REPL.FACTORING.RECHECK)
    (RED.I_CLAUSE.REPL.RESOLUTION)
    (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK)
    (RED.I_CLAUSE.REWRITING)
    (RED.I_LINK.INCOMPATIBILITY)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.TAUTOLOGY.RECHECK)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.MULTIPLE.LITERALS)
    (RED.D_CLAUSE.PURITY . PARTIAL)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.TAUTOLOGY.RECHECK)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.SUBSUMPTION.RECHECK)
    (RED.D_CLAUSE.REPL.FACTORING . T)
    (RED.D_CLAUSE.REPL.FACTORING.RECHECK . T)
    (RED.D_CLAUSE.REPL.RESOLUTION . SIMPLE)
    (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.TAUTOLOGY.RECHECK)
    (RED.D_LINK.SUBSUMPTION)
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
    (GEN_COMMON.LISP)
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
   (INITIAL   (CLAUSE 19 R.= AXIOM ((17 . 3)) ((+ 2 (17 17)) ))
(CLAUSE 23 A1 AXIOM ((7 . 3)(6 . 3)(5 . 3)) ((+ 4 ((8 (8 5 6) (8 (8 6 7) (8 5 7))) 9)) ))
(CLAUSE 25 A2 AXIOM ((12 . 3)) ((+ 4 ((8 (8 (13 12) 12) 12) 9)) ))
(CLAUSE 28 A3 AXIOM ((15 . 3)(14 . 3)) ((+ 4 ((8 14 (8 (13 14) 15)) 9)) ))
(CLAUSE 31 A4 AXIOM ((11 . 3)(10 . 9))
   ((- 4 ((8 10 11) 9)) (+ 4 (11 9)) ))
(CLAUSE 16 T5 THEOREM () ((- 4 ((8 32 32) 9)) ))
(CLAUSE 33 T6 THEOREM () ((+ 4 (32 3)) ))   )
   (OPERATION (CLAUSE 59 R1 (16 31) ((57 . 9)) ((- 4 ((8 57 (8 32 32)) 9)) ))
   (RESOLUTION 31 2 16 1 NIL (11 (8 32 32)) 59))
   (OPERATION (CLAUSE 75 R2 (59 31) ((71 . 9)(72 . 9)) ((- 4 ((8 72 (8 71 (8 32 32))) 9)) ))
   (RESOLUTION 31 2 59 1 NIL (11 (8 57 (8 32 32))) 75))
   (OPERATION (CLAUSE 75 R2 (59 31) ((71 . 9)(72 . 9)) ((- 4 ((8 72 (8 71 (8 32 32))) 9)) ))
   (REPLACEMENT.OPERATION (86 32 6 (8 (13 32) 32) 71 (8 (8 (13 32) 32) 32) 7 32 5 32 72 (8 32 (8 (13 32) 32))) (75 23)
  (((75 . 1) (23 . 1) NIL))
  (NIL)
  NIL))
   (END.TIME 3486055715)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    4 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "E" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   32 (DT-CONSTANT.CREATE "c_1" '3))
   (CONS    9 (DT-CONSTANT.CREATE "p" 'ANY))
   (CONS    3 (DT-CONSTANT.CREATE "omega" 'ANY))
   (CONS   13 (DT-FUNCTION.CREATE "n" 'ANY '(ANY) 'NIL))
   (CONS    8 (DT-FUNCTION.CREATE "i" 'ANY '(ANY ANY) 'NIL)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 75)
)