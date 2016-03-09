;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 PS 415.1 RPC 415.0 Embedding Support 407.0 UX Support 416.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Documentation 403.0 KKL 19.2 HADES 14.21 Waltz 6.0 COLUMN 6.0 Experimental MKRP 48.4" "12-JUL,1991 21:22" 
   ("Edit:     Axioms and Theorems edited: 12-JUL,1991 21:22 "
   ))

(AXIOMS.INFIX    ((* AXIOMS *)
   (ALL X + (0 X) = X)
   (ALL |X,Y| + (S (X) Y) = S (+ (X Y)))
   (ALL X * (0 X) = 0)
   (ALL |X,Y| * (S (X) Y) = + (Y * (X Y)))
   (ALL |X,Y| + (X Y) = + (Y X))
   (ALL |X,Y,Z| + (+ (X Y) Z) = + (X + (Y Z)))
   (ALL |X,Y| * (X Y) = * (Y X))
   (ALL |X,Y,Z| * (X + (Y Z)) = + (* (X Y) * (X Z)))
   (ALL |X,Y,Z| * (+ (Y Z) X) = + (* (Y X) * (Z X)))))

(THEOREMS.INFIX ((* THEOREM *)
   (ALL |X,Y,Z| * (* (0 Y) Z) = * (0 * (Y Z)))
   (ALL |X,Y,Z| * (* (X Y) Z) = * (X * (Y Z)) IMPL * (* (S (X) Y) Z) = * (S (X) * (Y Z)))))

(AXIOMS.PREFIX   (COMMENT
   (ALL 3 (+ 2 ((5 4 3) 3) NIL))
   (ALL 7 (ALL 6 (+ 2 ((5 (8 6) 7) (8 (5 6 7))) NIL)))
   (ALL 9 (+ 2 ((10 4 9) 4) NIL))
   (ALL 12 (ALL 11 (+ 2 ((10 (8 11) 12) (5 12 (10 11 12))) NIL)))
   (ALL 14 (ALL 13 (+ 2 ((5 13 14) (5 14 13)) NIL)))
   (ALL 17 (ALL 16 (ALL 15 (+ 2 ((5 (5 15 16) 17) (5 15 (5 16 17))) NIL))))
   (ALL 19 (ALL 18 (+ 2 ((10 18 19) (10 19 18)) NIL)))
   (ALL 22 (ALL 21 (ALL 20 (+ 2 ((10 20 (5 21 22)) (5 (10 20 21) (10 20 22))) NIL))))
   (ALL 25 (ALL 24 (ALL 23 (+ 2 ((10 (5 24 25) 23) (5 (10 24 23) (10 25 23))) NIL))))))

(THEOREMS.PREFIX (COMMENT
   (ALL 28 (ALL 27 (ALL 26 (+ 2 ((10 (10 4 27) 28) (10 4 (10 27 28))) NIL))))
   (ALL 31 (ALL 30 (ALL 29 (IMPL (+ 2 ((10 (10 29 30) 31) (10 29 (10 30 31))) NIL) (+ 2 ((10 (10 (8 29) 30) 31) (10 (8 29) (10 30 31))) NIL)))))))

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
    (ER_PARAMODULATION . KAPUR-ZHANG)
    (ER_COMPLETION . CONSTANT-CONGRUENCE)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING EXP * - + 0 1)
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
    (PR_LITERALS)
   )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 82099495)
   (PARTIAL   (CLAUSE 34 R.= AXIOM ((32 . ANY)) ((+ 2 (32 32)) ))(CLAUSE 36 A1 AXIOM ((3 . ANY)) ((+ 2 ((5 4 3) 3)) )))
   (PARTIAL   (CLAUSE 41 A2 AXIOM ((7 . ANY)(6 . ANY)) ((+ 2 ((5 (8 6) 7) (8 (5 6 7)))) )))
   (PARTIAL   (CLAUSE 45 A3 AXIOM ((9 . ANY)) ((+ 2 ((10 4 9) 4)) )))
   (PARTIAL   (CLAUSE 49 A4 AXIOM ((12 . ANY)(11 . ANY)) ((+ 2 ((10 (8 11) 12) (5 12 (10 11 12)))) )))
   (PARTIAL   (CLAUSE 53 A5 AXIOM ((14 . ANY)(13 . ANY)) ((+ 2 ((5 13 14) (5 14 13))) )))
   (PARTIAL   (CLAUSE 70 A6 AXIOM ((17 . ANY)(16 . ANY)(15 . ANY)) ((+ 2 ((5 (5 15 16) 17) (5 15 (5 16 17)))) )))
   (PARTIAL   (CLAUSE 86 A7 AXIOM ((19 . ANY)(18 . ANY)) ((+ 2 ((10 18 19) (10 19 18))) )))
   (PARTIAL   (CLAUSE 103 A8 AXIOM ((22 . ANY)(21 . ANY)(20 . ANY)) ((+ 2 ((10 20 (5 21 22)) (5 (10 20 21) (10 20 22)))) )))
   (PARTIAL   (CLAUSE 121 A9 AXIOM ((23 . ANY)(25 . ANY)(24 . ANY)) ((+ 2 ((10 (5 24 25) 23) (5 (10 24 23) (10 25 23)))) )))
   (END.TIME 82110460)
   (FINAL 34 36 41 45 49 53 70 86 103 121)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS    4 (DT-CONSTANT.CREATE "0" 'ANY))
   (CONS   10 (DT-FUNCTION.CREATE "*" 'ANY '(ANY ANY) 'NIL))
   (CONS    8 (DT-FUNCTION.CREATE "s" 'ANY '(ANY) 'NIL))
   (CONS    5 (DT-FUNCTION.CREATE "+" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 82124074)
   (PARTIAL   (CLAUSE 29 T10 THEOREM () ((- 2 ((10 (10 4 134) 26) (10 4 (10 134 26)))) )))
   (OPERATION (CLAUSE 29 T10 THEOREM () ((- 2 ((10 4 26) (10 4 (10 134 26)))) ))
   (REWRITE 45 1 29))
   (OPERATION (CLAUSE 29 T10 THEOREM () ((- 2 (4 (10 4 (10 134 26)))) ))
   (REWRITE 45 1 29))
   (OPERATION (CLAUSE 29 T10 THEOREM () ((- 2 (4 4)) ))
   (REWRITE 45 1 29))
   (OPERATION (CLAUSE 29 T10 THEOREM () ())
   (REPLACEMENT.OPERATION NIL (29 34)
  (((29 . 1) (34 . 1) NIL))
  (NIL)
  NIL))
   (END.TIME 82125340)
   (FINAL 34 36 41 45 49 53 70 86 103 121 29)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS  135 (DT-CONSTANT.CREATE "c_5" 'ANY))
   (CONS   28 (DT-CONSTANT.CREATE "c_4" 'ANY))
   (CONS   27 (DT-CONSTANT.CREATE "c_3" 'ANY))
   (CONS  134 (DT-CONSTANT.CREATE "c_2" 'ANY))
   (CONS   26 (DT-CONSTANT.CREATE "c_1" 'ANY))
   (CONS    4 (DT-CONSTANT.CREATE "0" 'ANY))
   (CONS   10 (DT-FUNCTION.CREATE "*" 'ANY '(ANY ANY) 'NIL))
   (CONS    8 (DT-FUNCTION.CREATE "s" 'ANY '(ANY) 'NIL))
   (CONS    5 (DT-FUNCTION.CREATE "+" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT SUCCESS 29))

(THEOREMS (SPLITPART.IDENTIFIER 2)
   (START.TIME 82126034)
   (PARTIAL   (CLAUSE 29 T10 THEOREM () ((+ 2 ((10 (10 135 28) 27) (10 135 (10 28 27)))) )))
   (OPERATION (CLAUSE 29 T10 THEOREM () ((+ 2 ((10 27 (10 135 28)) (10 135 (10 28 27)))) ))
   (REWRITE 86 1 29))
   (PARTIAL   (CLAUSE 141 T11 THEOREM () ((- 2 ((10 (10 (8 135) 28) 27) (10 (8 135) (10 28 27)))) )))
   (OPERATION (CLAUSE 141 T11 THEOREM () ((- 2 ((10 (5 28 (10 135 28)) 27) (10 (8 135) (10 28 27)))) ))
   (REWRITE 49 1 141))
   (OPERATION (CLAUSE 141 T11 THEOREM () ((- 2 ((5 (10 28 27) (10 (10 135 28) 27)) (10 (8 135) (10 28 27)))) ))
   (REWRITE 121 1 141))
   (OPERATION (CLAUSE 141 T11 THEOREM () ((- 2 ((5 (10 28 27) (10 (10 135 28) 27)) (5 (10 28 27) (10 135 (10 28 27))))) ))
   (REWRITE 49 1 141))
   (OPERATION (CLAUSE 141 T11 THEOREM () ((- 2 ((5 (10 28 27) (10 27 (10 135 28))) (5 (10 28 27) (10 135 (10 28 27))))) ))
   (REWRITE 86 1 141))
   (OPERATION (CLAUSE 141 T11 THEOREM () ((- 2 ((5 (10 28 27) (10 135 (10 28 27))) (5 (10 28 27) (10 135 (10 28 27))))) ))
   (REWRITE 29 1 141))
   (OPERATION (CLAUSE 141 T11 THEOREM () ())
   (REPLACEMENT.OPERATION NIL (141 34)
  (((141 . 1) (34 . 1) NIL))
  (NIL)
  NIL))
   (END.TIME 82144237)
   (FINAL 34 36 41 45 49 53 70 86 103 121 29 141)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS  135 (DT-CONSTANT.CREATE "c_5" 'ANY))
   (CONS   28 (DT-CONSTANT.CREATE "c_4" 'ANY))
   (CONS   27 (DT-CONSTANT.CREATE "c_3" 'ANY))
   (CONS  134 (DT-CONSTANT.CREATE "c_2" 'ANY))
   (CONS   26 (DT-CONSTANT.CREATE "c_1" 'ANY))
   (CONS    4 (DT-CONSTANT.CREATE "0" 'ANY))
   (CONS   10 (DT-FUNCTION.CREATE "*" 'ANY '(ANY ANY) 'NIL))
   (CONS    8 (DT-FUNCTION.CREATE "s" 'ANY '(ANY) 'NIL))
   (CONS    5 (DT-FUNCTION.CREATE "+" 'ANY '(ANY ANY) 'NIL)))))
   (RESULT SUCCESS 141))
(indices ((141 12) (29 11) (29 10) (121 9) (103 8) (86 7) (70 6) (53 5) (49 4) (45 3) (41 2) (36 1) (34 NIL)))

(SPLITPARTS "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 PS 415.1 RPC 415.0 Embedding Support 407.0 UX Support 416.0 CLX 419.0 C Runtime 416.0 Compiler Tools Package 411.0 Compiler Tools Runtime 411.0 C Packages 413.0 Minimal Lexer Runtime 416.0 Lexer Package 415.0 Syntax Editor Runtime 411.0 Experimental X Server 409.0 X Documentation 403.0 KKL 19.2 HADES 14.21 Waltz 6.0 COLUMN 6.0 Experimental MKRP 48.4" "12-JUL,1991 21:23" NIL)

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
    (ER_PARAMODULATION . KAPUR-ZHANG)
    (ER_COMPLETION . CONSTANT-CONGRUENCE)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING EXP * - + 0 1)
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
    (PR_LITERALS)
   )

(REFUTATION (START.TIME 82147810)

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
    (ER_PARAMODULATION . KAPUR-ZHANG)
    (ER_COMPLETION . CONSTANT-CONGRUENCE)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING EXP * - + 0 1)
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
    (PR_LITERALS)
   )
   (END.TIME 82149248)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST )))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 29)
)

(REFUTATION (START.TIME 82149513)

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
    (ER_PARAMODULATION . KAPUR-ZHANG)
    (ER_COMPLETION . CONSTANT-CONGRUENCE)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING EXP * - + 0 1)
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
    (PR_LITERALS)
   )
   (END.TIME 82150667)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST )))
   (SPLITPART.IDENTIFIER 2)
   (RESULT SUCCESS 141)
)