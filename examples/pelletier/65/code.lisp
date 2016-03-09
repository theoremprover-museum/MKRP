;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.2 IP-TCP 422.9 IP-TCP-DOC 404.0 CLX 419.3 X Remote Screen 418.1 X Documentation 403.0 Network RPC 415.5 NFS Client 415.3 NFS Documentation 404.0 Logical Pathnames Translation Files NEWEST KKL 24.1 HADES 19.0 Waltz 8.0 COLUMN 9.0 Experimental MKRP 53.9 GENTRAFO 3.0" "05-MAY,1992 20:27" 
   ("Edit:     Axioms and Theorems edited: 29-JUN,1990 00:36 "
   ))

(AXIOMS.INFIX    ((* AXIOMS *)
   (ALL |X,Y,Z| F (X F (Y Z)) = F (F (X Y) Z))
   (ALL X F (A X) = X)
   (ALL X (EX Y F (Y X) = A))))

(THEOREMS.INFIX ((* THEOREMS *)
   ((ALL X F (X X) = A) IMPL (ALL |X,Y| F (X Y) = F (Y X)))))

(AXIOMS.PREFIX   (COMMENT
   (ALL 5 (ALL 4 (ALL 3 (+ 2 ((6 3 (6 4 5)) (6 (6 3 4) 5)) NIL))))
   (ALL 7 (+ 2 ((6 8 7) 7) NIL))
   (ALL 9 (EX 10 (+ 2 ((6 10 9) 8) NIL)))))

(THEOREMS.PREFIX (COMMENT
   (IMPL (ALL 11 (+ 2 ((6 11 11) 8) NIL)) (ALL 13 (ALL 12 (+ 2 ((6 12 13) (6 13 12)) NIL))))))

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

(AXIOMS (START.TIME 84793965)
   (PARTIAL   (CLAUSE 16 R.= AXIOM ((10 . ANY)) ((+ 2 (10 10)) ))(CLAUSE 20 A1 AXIOM ((5 . ANY)(4 . ANY)(3 . ANY)) ((+ 2 ((6 3 (6 4 5)) (6 (6 3 4) 5))) )))
   (PARTIAL   (CLAUSE 27 A2 AXIOM ((7 . ANY)) ((+ 2 ((6 8 7) 7)) )))
   (PARTIAL   (CLAUSE 31 A3 AXIOM ((9 . ANY)) ((+ 2 ((6 (14 9) 9) 8)) )))
   (END.TIME 84794816)
   (FINAL 16 20 27 31)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    8 (DT-CONSTANT.CREATE "a" 'ANY))
   (CONS   14 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL))
   (CONS    6 (DT-FUNCTION.CREATE "f" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 84794839)
   (PARTIAL   (CLAUSE 12 T4 THEOREM ((11 . ANY)) ((+ 2 ((6 11 11) 8)) )))
   (PARTIAL   (CLAUSE 43 T5 THEOREM () ((- 2 ((6 35 34) (6 34 35))) )))
   (INITIAL   (CLAUSE 16 R.= AXIOM ((10 . ANY)) ((+ 2 (10 10)) ))
(CLAUSE 20 A1 AXIOM ((5 . ANY)(4 . ANY)(3 . ANY)) ((+ 2 ((6 3 (6 4 5)) (6 (6 3 4) 5))) ))
(CLAUSE 27 A2 AXIOM ((7 . ANY)) ((+ 2 ((6 8 7) 7)) ))
(CLAUSE 31 A3 AXIOM ((9 . ANY)) ((+ 2 ((6 (14 9) 9) 8)) ))
(CLAUSE 12 T4 THEOREM ((11 . ANY)) ((+ 2 ((6 11 11) 8)) ))
(CLAUSE 43 T5 THEOREM () ((- 2 ((6 35 34) (6 34 35))) ))   )
   (END.TIME 84795881)
   (FINAL 16 20 27 31 12 43)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   35 (DT-CONSTANT.CREATE "c_2" 'ANY))
   (CONS   34 (DT-CONSTANT.CREATE "c_1" 'ANY))
   (CONS    8 (DT-CONSTANT.CREATE "a" 'ANY))
   (CONS   14 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL))
   (CONS    6 (DT-FUNCTION.CREATE "f" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT))
(indices ((43 5) (12 4) (31 3) (27 2) (20 1) (16 NIL)))

(SPLITPARTS "Genera 8.0.2 IP-TCP 422.9 IP-TCP-DOC 404.0 CLX 419.3 X Remote Screen 418.1 X Documentation 403.0 Network RPC 415.5 NFS Client 415.3 NFS Documentation 404.0 Logical Pathnames Translation Files NEWEST KKL 24.1 HADES 19.0 Waltz 8.0 COLUMN 9.0 Experimental MKRP 53.9 GENTRAFO 3.0" "05-MAY,1992 20:27" NIL)

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

(REFUTATION (START.TIME 84797056)

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
   (INITIAL   (CLAUSE 16 R.= AXIOM ((10 . ANY)) ((+ 2 (10 10)) ))
(CLAUSE 20 A1 AXIOM ((5 . ANY)(4 . ANY)(3 . ANY)) ((+ 2 ((6 3 (6 4 5)) (6 (6 3 4) 5))) ))
(CLAUSE 27 A2 AXIOM ((7 . ANY)) ((+ 2 ((6 8 7) 7)) ))
(CLAUSE 31 A3 AXIOM ((9 . ANY)) ((+ 2 ((6 (14 9) 9) 8)) ))
(CLAUSE 12 T4 THEOREM ((11 . ANY)) ((+ 2 ((6 11 11) 8)) ))
(CLAUSE 43 T5 THEOREM () ((- 2 ((6 35 34) (6 34 35))) ))   )
   (OPERATION (CLAUSE 22 P1 (20 12) ((39 . ANY)(28 . ANY)) ((+ 2 ((6 28 (6 28 39)) (6 8 39))) ))
   (PARAMODULATION 12 1 (1) 20 1 (2 1) NIL (11 4 3 4) 22))
   (OPERATION (CLAUSE 22 P1 (20 12) ((39 . ANY)(28 . ANY)) ((+ 2 ((6 28 (6 28 39)) 39)) ))
   (REWRITE 27 1 22))
   (OPERATION (CLAUSE 82 P2 (22 12) ((76 . ANY)) ((+ 2 ((6 76 8) 76)) ))
   (PARAMODULATION 12 1 (1) 22 1 (1 2) NIL (11 39 28 39) 82))
   (OPERATION (CLAUSE 129 P3 (22 31) ((125 . ANY)) ((+ 2 ((6 (14 125) 8) 125)) ))
   (PARAMODULATION 31 1 (1) 22 1 (1 2) NIL (28 (14 39) 9 39) 129))
   (OPERATION (CLAUSE 129 P3 (22 31) ((125 . ANY)) ((+ 2 ((14 125) 125)) ))
   (REWRITE 82 1 129))
   (OPERATION (CLAUSE 31 A3 AXIOM ((9 . ANY)) ((+ 2 ((6 9 9) 8)) ))
   (REWRITE 129 1 31))
   (OPERATION (CLAUSE 31 A3 AXIOM () ((+ 2 (8 8)) ))
   (REWRITE 12 1 31))
   (OPERATION (CLAUSE 32 P4 (20 12) ((81 . ANY)(130 . ANY)) ((+ 2 ((6 130 (6 81 (6 130 81))) 8)) ))
   (PARAMODULATION 12 1 (1) 20 1 (2) NIL (11 (6 3 4) 5 (6 3 4)) 32))
   (OPERATION (CLAUSE 173 P5 (22 32) ((37 . ANY)(168 . ANY)) ((+ 2 ((6 168 8) (6 37 (6 168 37)))) ))
   (PARAMODULATION 32 1 (1) 22 1 (1 2) NIL (130 28 39 (6 81 (6 28 81))) 173))
   (OPERATION (CLAUSE 173 P5 (22 32) ((37 . ANY)(168 . ANY)) ((+ 2 (168 (6 37 (6 168 37)))) ))
   (REWRITE 82 1 173))
   (OPERATION (CLAUSE 32 P4 (20 12) ((130 . ANY)) ((+ 2 ((6 130 130) 8)) ))
   (REWRITE 173 1 32))
   (OPERATION (CLAUSE 32 P4 (20 12) () ((+ 2 (8 8)) ))
   (REWRITE 12 1 32))
   (OPERATION (CLAUSE 182 P6 (22 173) ((128 . ANY)(169 . ANY)) ((+ 2 ((6 169 128) (6 128 169))) ))
   (PARAMODULATION 173 1 (2) 22 1 (1 2) NIL (37 28 39 (6 168 28)) 182))
   (OPERATION (CLAUSE 182 P6 (22 173) ((128 . ANY)(169 . ANY)) ((+ 2 ((6 169 128) (6 128 169))) ))
   (REPLACEMENT.OPERATION (128 35 169 34) (182 43)
  (((182 . 1) (43 . 1) SYMMETRIC))
  (NIL)
  NIL))
   (END.TIME 84801817)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   35 (DT-CONSTANT.CREATE "c_2" 'ANY))
   (CONS   34 (DT-CONSTANT.CREATE "c_1" 'ANY))
   (CONS    8 (DT-CONSTANT.CREATE "a" 'ANY))
   (CONS   14 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL))
   (CONS    6 (DT-FUNCTION.CREATE "f" 'ANY '(ANY ANY) 'NIL)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 182)
)