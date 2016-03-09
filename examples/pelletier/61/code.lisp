;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.2 IP-TCP 422.9 IP-TCP-DOC 404.0 CLX 419.3 X Remote Screen 418.1 X Documentation 403.0 Network RPC 415.5 NFS Client 415.3 NFS Documentation 404.0 Logical Pathnames Translation Files NEWEST KKL 24.1 HADES 19.0 Waltz 8.0 COLUMN 9.0 Experimental MKRP 53.9 GENTRAFO 3.0" "05-MAY,1992 20:29" 
   ("Edit:     Axioms and Theorems edited: 29-JUN,1990 00:26 "
   ))

(AXIOMS.INFIX    ((* AXIOMS *)
   (ALL |X,Y,Z| F (X F (Y Z)) = F (F (X Y) Z))))

(THEOREMS.INFIX ((* THEOREMS *)
   (ALL |X,Y,Z,W| F (X F (Y F (Z W))) = F (F (F (X Y) Z) W))))

(AXIOMS.PREFIX   (COMMENT
   (ALL 5 (ALL 4 (ALL 3 (+ 2 ((6 3 (6 4 5)) (6 (6 3 4) 5)) NIL))))))

(THEOREMS.PREFIX (COMMENT
   (ALL 10 (ALL 9 (ALL 8 (ALL 7 (+ 2 ((6 7 (6 8 (6 9 10))) (6 (6 (6 7 8) 9) 10)) NIL)))))))

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
    (ER_PARAMODULATION . BACHMAIR-GANZINGER)
    (ER_WEIGHT.POLYNOMIALS)
    (ER_P.SELECTION * WEIGHT (IF SUPPORT 1 1.5) (IF EQUATIONAL 1 2))
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING P F_2 F_3 F_1)
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
    (PR_DIRECT.PROOF)
    (PR_VARIABLE.PRINT.NAMES X Y Z U V W XX YY ZZ UU VV WW XXX YYY ZZZ UUU VVV WWW)
    (PR_PROTOCOL . STANDARD)
    (PR_LEFT.MARGIN . 0)
    (PR_RIGHT.MARGIN . 117)
    (PR_LATEX)
    (PR_LINELENGTH . 117)
    (PR_LITERALS)
   )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 84956973)
   (PARTIAL   (CLAUSE 13 R.= AXIOM ((11 . ANY)) ((+ 2 (11 11)) ))(CLAUSE 17 A1 AXIOM ((5 . ANY)(4 . ANY)(3 . ANY)) ((+ 2 ((6 3 (6 4 5)) (6 (6 3 4) 5))) )))
   (END.TIME 84957481)
   (FINAL 13 17)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    6 (DT-FUNCTION.CREATE "f" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 84957508)
   (PARTIAL   (CLAUSE 7 T2 THEOREM () ((- 2 ((6 26 (6 25 (6 24 23))) (6 (6 (6 26 25) 24) 23))) )))
   (OPERATION (CLAUSE 7 T2 THEOREM () ((- 2 ((6 26 (6 25 (6 24 23))) (6 (6 26 25) (6 24 23)))) ))
   (REWRITE 17 1 7))
   (OPERATION (CLAUSE 7 T2 THEOREM () ((- 2 ((6 26 (6 25 (6 24 23))) (6 26 (6 25 (6 24 23))))) ))
   (REWRITE 17 1 7))
   (OPERATION (CLAUSE 7 T2 THEOREM () ())
   (REPLACEMENT.OPERATION NIL (7 13)
  (((7 . 1) (13 . 1) NIL))
  (NIL)
  NIL))
   (END.TIME 84957834)
   (FINAL 13 17 7)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   26 (DT-CONSTANT.CREATE "c_4" 'ANY))
   (CONS   25 (DT-CONSTANT.CREATE "c_3" 'ANY))
   (CONS   24 (DT-CONSTANT.CREATE "c_2" 'ANY))
   (CONS   23 (DT-CONSTANT.CREATE "c_1" 'ANY))
   (CONS    6 (DT-FUNCTION.CREATE "f" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT SUCCESS 7))
(indices ((7 2) (17 1) (13 NIL)))