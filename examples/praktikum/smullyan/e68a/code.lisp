;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 RPC 415.0 Embedding Support 407.0 UX Support 416.0 KKL 19.3 HADES 14.21 Waltz 6.0 COLUMN 6.0 Experimental MKRP 48.5" "23-JUL,1991 09:25" 
   ("Edit:     Axioms and Theorems edited: 16-JUL,1991 10:50 "
   ))

(AXIOMS.INFIX    ((* PORTIA AND HER HUSBAND |DID,AS| A MATTER OF |FACT,| LIVE HAPPILY)
   (* EVER AFTER. THEY HAD A DAUGHTER PORTIA II - HENCEFORTH TO BE)
   (* CALLED PORTIA. WHEN THE YOUNG PORTIA GREW TO YOUNG WOMANHOOD)
   (* SHE WAS BOTH CLEVER AND |BEAUTIFUL,| JUST LIKE HER MOMMY. SHE)
   (* ALSO DECIDED TO SELECT HER HUSBAND BY THE CASKET METHOD. THE)
   (* SUITOR HAD TO PASS TWO TESTS IN ORDER TO WIN HER.)
   (*)
   (* 68A. THE FIRST TEST.)
   (*)
   (* IN THIS TEST EACH LID CONTAINED TWO |STATEMENTS,| AND PORTIA EX-)
   (* PLAINED THAT NO LID CONTAINED MORE THAN ONE FALSE STATEMENT.)
   (*)
   (* GOLD |:| (1) THE PORTRAIT IS NOT IN HERE.)
   (* (2) THE ARTIST OF THE PORTRAIT IS FROM VENICE)
   (* SILVER |:| (1) THE PORTRAIT IS NOT IN THE GOLD CASKET)
   (* (2) THE ARTIST OF THE PORTRAIT IS REALLY FROM FLORENCE)
   (* LEAD |:| (1) THE PORTRAIT IS NOT IN HERE)
   (* (2) THE PORTRAIT IS REALLY IN THE SILVER CASKET)
   (*)
   (* WHICH CASKET CONTAINS THE PORTRAIT?)
   (*)
   (* WE DEFINE THE FOLLOWING PREDICATES |:|)
   (* THE PREDICATES |PORTRAIT,TRUTH,INSRIPTION,NEGATIVE.INSCRIPTION|)
   (* MEAN THE SAME AS IN 66 AND 67)
   (TYPE PORTRAIT (CASKET))
   (TYPE TRUTH (INSCRIPT))
   (TYPE INSCRIPTION (INSCRIPT CASKET))
   (TYPE NEGATIVE.INSCRIPTION (INSCRIPT CASKET))
   (* V---THE ARTIST OF THE PORTRAIT IS FROM VENICE)
   (* F---THE ARTIST OF THE PORTRAIT IS FROM FLORENCE)
   (* INSCRIPTION.VENICE (X) --- THE INSCRIPTION X TELLS THAT THE ARTIST OF THE PORTRAIT)
   (* IS FROM VENICE.)
   (* INSCRIPTION.FLORENCE (X) --- THE INSCRIPTION X TELLS THAT THE ARTIST OF THE PORTRAIT)
   (* IS FROM FLORENCE)
   (TYPE INSCRIPTION.VENICE (INSCRIPT))
   (TYPE INSCRIPTION.FLORENCE (INSCRIPT))
   (*)
   (* WE DEFINE THE FOLLOWING FUNCTIONS |:|)
   (* FIRST (X) ---ASSIGNS EACH CASKET AN INSCRIPTION)
   (* SECOND (X) ---ASSIGNS EACH CASKET AN INSCRIPTION)
   (TYPE |GOLD,SILVER,LEAD| |:| CASKET)
   (TYPE FIRST (CASKET) |:| INSCRIPT)
   (TYPE SECOND (CASKET) |:| INSCRIPT)
   (*)
   (* THEN THE FOLLOWING PROPOSITIONS HOLD.)
   (*)
   (* THERE IS EXACTLY ONE PORTRAIT |:|)
   (PORTRAIT (GOLD) OR PORTRAIT (SILVER) OR PORTRAIT (LEAD))
   (NOT (PORTRAIT (GOLD) AND PORTRAIT (SILVER) OR PORTRAIT (GOLD) AND PORTRAIT (LEAD) OR PORTRAIT (SILVER) AND PORTRAIT (LEAD)))
   (*)
   (* NO MORE THAN ONE INSCRIPTION IS FALSE ON ANY CASKET |:|)
   (ALL X |:| CASKET TRUTH (FIRST (X)) OR TRUTH (SECOND (X)))
   (*)
   (* THE ARTIST OF THE PORTRAIT IS EITHER FROM VENICE OR FROM FLORENCE)
   (NOT (VENICE AND FLORENCE))
   (*)
   (* IMPLICATIONS TO BE DRAWN FROM INSCRIPTION |:|)
   (ALL X |:| INSCRIPT ALL Y |:| CASKET INSCRIPTION (X Y) AND TRUTH (X) IMPL PORTRAIT (Y))
   (ALL X |:| INSCRIPT ALL Y |:| CASKET INSCRIPTION (X Y) AND NOT TRUTH (X) IMPL NOT PORTRAIT (Y))
   (ALL X |:| INSCRIPT ALL Y |:| CASKET NEGATIVE.INSCRIPTION (X Y) AND TRUTH (X) IMPL NOT PORTRAIT (Y))
   (ALL X |:| INSCRIPT ALL Y |:| CASKET NEGATIVE.INSCRIPTION (X Y) AND NOT TRUTH (X) IMPL PORTRAIT (Y))
   (ALL X |:| INSCRIPT INSCRIPTION.VENICE (X) AND TRUTH (X) IMPL VENICE)
   (ALL X |:| INSCRIPT INSCRIPTION.VENICE (X) AND NOT TRUTH (X) IMPL NOT VENICE)
   (ALL X |:| INSCRIPT INSCRIPTION.FLORENCE (X) AND TRUTH (X) IMPL FLORENCE)
   (ALL X |:| INSCRIPT INSCRIPTION.FLORENCE (X) AND NOT TRUTH (X) IMPL NOT FLORENCE)
   (NEGATIVE.INSCRIPTION (FIRST (GOLD) GOLD) AND INSCRIPTION.VENICE (SECOND (GOLD)))
   (NEGATIVE.INSCRIPTION (FIRST (SILVER) GOLD) AND INSCRIPTION.FLORENCE (SECOND (SILVER)))
   (NEGATIVE.INSCRIPTION (FIRST (LEAD) LEAD) AND INSCRIPTION (SECOND (LEAD) SILVER))))

(THEOREMS.INFIX ((* THEOREMS FOR THE THIRD PORTIA PROBLEM (SM.E68A))
   (*)
   (PORTRAIT (SILVER))))

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
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   (+ 0 NIL NIL)
   COMMENT
   COMMENT
   COMMENT
   COMMENT
   (OR (+ 3 (9) NIL) (OR (+ 3 (10) NIL) (+ 3 (11) NIL)))
   (NOT (OR (AND (+ 3 (9) NIL) (+ 3 (10) NIL)) (OR (AND (+ 3 (9) NIL) (+ 3 (11) NIL)) (AND (+ 3 (10) NIL) (+ 3 (11) NIL)))))
   COMMENT
   COMMENT
   (ALL 14 (OR (+ 4 ((12 14)) NIL) (+ 4 ((13 14)) NIL)))
   COMMENT
   COMMENT
   (NOT (AND (+ 15 NIL NIL) (+ 16 NIL NIL)))
   COMMENT
   COMMENT
   (ALL 17 (ALL 18 (IMPL (AND (+ 5 (17 18) NIL) (+ 4 (17) NIL)) (+ 3 (18) NIL))))
   (ALL 19 (ALL 20 (IMPL (AND (+ 5 (19 20) NIL) (NOT (+ 4 (19) NIL))) (NOT (+ 3 (20) NIL)))))
   (ALL 21 (ALL 22 (IMPL (AND (+ 6 (21 22) NIL) (+ 4 (21) NIL)) (NOT (+ 3 (22) NIL)))))
   (ALL 23 (ALL 24 (IMPL (AND (+ 6 (23 24) NIL) (NOT (+ 4 (23) NIL))) (+ 3 (24) NIL))))
   (ALL 25 (IMPL (AND (+ 7 (25) NIL) (+ 4 (25) NIL)) (+ 15 NIL NIL)))
   (ALL 26 (IMPL (AND (+ 7 (26) NIL) (NOT (+ 4 (26) NIL))) (NOT (+ 15 NIL NIL))))
   (ALL 27 (IMPL (AND (+ 8 (27) NIL) (+ 4 (27) NIL)) (+ 16 NIL NIL)))
   (ALL 28 (IMPL (AND (+ 8 (28) NIL) (NOT (+ 4 (28) NIL))) (NOT (+ 16 NIL NIL))))
   (AND (+ 6 ((12 9) 9) NIL) (+ 7 ((13 9)) NIL))
   (AND (+ 6 ((12 10) 9) NIL) (+ 8 ((13 10)) NIL))
   (AND (+ 6 ((12 11) 11) NIL) (+ 5 ((13 11) 10) NIL))))

(THEOREMS.PREFIX (COMMENT
   COMMENT
   (+ 3 (10) NIL)))

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

(AXIOMS (START.TIME 77046958)
   (PARTIAL   (CLAUSE 31 R.= AXIOM ((29 . ANY)) ((+ 2 (29 29)) ))(CLAUSE 32 A1 AXIOM () ((+ 6 ((12 9) 9)) )))
   (PARTIAL   (CLAUSE 33 A2 AXIOM () ((+ 7 ((13 9))) )))
   (PARTIAL   (CLAUSE 34 A3 AXIOM () ((+ 6 ((12 10) 9)) )))
   (PARTIAL   (CLAUSE 35 A4 AXIOM () ((+ 8 ((13 10))) )))
   (PARTIAL   (CLAUSE 36 A5 AXIOM () ((+ 6 ((12 11) 11)) )))
   (PARTIAL   (CLAUSE 37 A6 AXIOM () ((+ 5 ((13 11) 10)) )))
   (PARTIAL   (CLAUSE 38 A7 AXIOM ()
   ((- 3 (9)) (- 3 (10)) )))
   (PARTIAL   (CLAUSE 39 A8 AXIOM ()
   ((- 3 (9)) (- 3 (11)) )))
   (PARTIAL   (CLAUSE 41 A9 AXIOM ()
   ((- 3 (10)) (- 3 (11)) )))
   (PARTIAL   (CLAUSE 45 A10 AXIOM ((14 . CASKET))
   ((+ 4 ((12 14))) (+ 4 ((13 14))) )))
   (PARTIAL   (CLAUSE 46 A11 AXIOM ()
   ((- 15 NIL) (- 16 NIL) )))
   (PARTIAL   (CLAUSE 47 A12 AXIOM ()
   ((+ 3 (9)) (+ 3 (10)) (+ 3 (11)) )))
   (PARTIAL   (CLAUSE 56 A13 AXIOM ((18 . CASKET)(17 . INSCRIPT))
   ((- 5 (17 18)) (- 4 (17)) (+ 3 (18)) )))
   (PARTIAL   (CLAUSE 71 A14 AXIOM ((20 . CASKET)(19 . INSCRIPT))
   ((- 5 (19 20)) (+ 4 (19)) (- 3 (20)) )))
   (PARTIAL   (CLAUSE 89 A15 AXIOM ((22 . CASKET)(21 . INSCRIPT))
   ((- 6 (21 22)) (- 4 (21)) (- 3 (22)) )))
   (PARTIAL   (CLAUSE 110 A16 AXIOM ((24 . CASKET)(23 . INSCRIPT))
   ((- 6 (23 24)) (+ 4 (23)) (+ 3 (24)) )))
   (PARTIAL   (CLAUSE 133 A17 AXIOM ((25 . INSCRIPT))
   ((- 7 (25)) (- 4 (25)) (+ 15 NIL) )))
   (PARTIAL   (CLAUSE 143 A18 AXIOM ((26 . INSCRIPT))
   ((- 7 (26)) (+ 4 (26)) (- 15 NIL) )))
   (PARTIAL   (CLAUSE 156 A19 AXIOM ((27 . INSCRIPT))
   ((- 8 (27)) (- 4 (27)) (+ 16 NIL) )))
   (PARTIAL   (CLAUSE 168 A20 AXIOM ((28 . INSCRIPT))
   ((- 8 (28)) (+ 4 (28)) (- 16 NIL) )))
   (END.TIME 77066367)
   (FINAL 31 32 33 34 35 36 37 38 39 41 45 46 47 56 71 89 110 133 143 156 168)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   16 (DT-PREDICATE.CREATE "FLORENCE" 'NIL))
   (CONS   15 (DT-PREDICATE.CREATE "VENICE" 'NIL))
   (CONS    8 (DT-PREDICATE.CREATE "INSCRIPTION.FLORENCE" '(INSCRIPT)))
   (CONS    7 (DT-PREDICATE.CREATE "INSCRIPTION.VENICE" '(INSCRIPT)))
   (CONS    6 (DT-PREDICATE.CREATE "NEGATIVE.INSCRIPTION" '(INSCRIPT CASKET)))
   (CONS    5 (DT-PREDICATE.CREATE "INSCRIPTION" '(INSCRIPT CASKET)))
   (CONS    4 (DT-PREDICATE.CREATE "TRUTH" '(INSCRIPT)))
   (CONS    3 (DT-PREDICATE.CREATE "PORTRAIT" '(CASKET)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   11 (DT-CONSTANT.CREATE "lead" 'CASKET))
   (CONS   10 (DT-CONSTANT.CREATE "silver" 'CASKET))
   (CONS    9 (DT-CONSTANT.CREATE "gold" 'CASKET))
   (CONS   13 (DT-FUNCTION.CREATE "second" 'INSCRIPT '(CASKET) 'NIL))
   (CONS   12 (DT-FUNCTION.CREATE "first" 'INSCRIPT '(CASKET) 'NIL)))))
   (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
   (START.TIME 77066573)
   (PARTIAL   (CLAUSE 182 T21 THEOREM () ((- 3 (10)) )))
   (OPERATION (CLAUSE 47 A12 AXIOM ()
   ((+ 3 (9)) (+ 3 (10)) (+ 3 (11)) ))
   (REPLACEMENT.OPERATION NIL (47 182)
  (((47 . 2) (182 . 1) NIL))
  (NIL)
  NIL))
   (INITIAL   (CLAUSE 31 R.= AXIOM ((29 . ANY)) ((+ 2 (29 29)) ))
(CLAUSE 32 A1 AXIOM () ((+ 6 ((12 9) 9)) ))
(CLAUSE 33 A2 AXIOM () ((+ 7 ((13 9))) ))
(CLAUSE 34 A3 AXIOM () ((+ 6 ((12 10) 9)) ))
(CLAUSE 35 A4 AXIOM () ((+ 8 ((13 10))) ))
(CLAUSE 36 A5 AXIOM () ((+ 6 ((12 11) 11)) ))
(CLAUSE 37 A6 AXIOM () ((+ 5 ((13 11) 10)) ))
(CLAUSE 39 A8 AXIOM ()
   ((- 3 (9)) (- 3 (11)) ))
(CLAUSE 45 A10 AXIOM ((14 . CASKET))
   ((+ 4 ((12 14))) (+ 4 ((13 14))) ))
(CLAUSE 46 A11 AXIOM ()
   ((- 15 NIL) (- 16 NIL) ))
(CLAUSE 47 A12 THEOREM ()
   ((+ 3 (9)) (+ 3 (11)) ))
(CLAUSE 56 A13 AXIOM ((18 . CASKET)(17 . INSCRIPT))
   ((- 5 (17 18)) (- 4 (17)) (+ 3 (18)) ))
(CLAUSE 89 A15 AXIOM ((22 . CASKET)(21 . INSCRIPT))
   ((- 6 (21 22)) (- 4 (21)) (- 3 (22)) ))
(CLAUSE 110 A16 AXIOM ((24 . CASKET)(23 . INSCRIPT))
   ((- 6 (23 24)) (+ 4 (23)) (+ 3 (24)) ))
(CLAUSE 133 A17 AXIOM ((25 . INSCRIPT))
   ((- 7 (25)) (- 4 (25)) (+ 15 NIL) ))
(CLAUSE 143 A18 AXIOM ((26 . INSCRIPT))
   ((- 7 (26)) (+ 4 (26)) (- 15 NIL) ))
(CLAUSE 156 A19 AXIOM ((27 . INSCRIPT))
   ((- 8 (27)) (- 4 (27)) (+ 16 NIL) ))
(CLAUSE 168 A20 AXIOM ((28 . INSCRIPT))
   ((- 8 (28)) (+ 4 (28)) (- 16 NIL) ))
(CLAUSE 182 T21 THEOREM () ((- 3 (10)) ))   )
   (END.TIME 77073143)
   (FINAL 31 32 33 34 35 36 37 39 45 46 47 56 89 110 133 143 156 168 182)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   16 (DT-PREDICATE.CREATE "FLORENCE" 'NIL))
   (CONS   15 (DT-PREDICATE.CREATE "VENICE" 'NIL))
   (CONS    8 (DT-PREDICATE.CREATE "INSCRIPTION.FLORENCE" '(INSCRIPT)))
   (CONS    7 (DT-PREDICATE.CREATE "INSCRIPTION.VENICE" '(INSCRIPT)))
   (CONS    6 (DT-PREDICATE.CREATE "NEGATIVE.INSCRIPTION" '(INSCRIPT CASKET)))
   (CONS    5 (DT-PREDICATE.CREATE "INSCRIPTION" '(INSCRIPT CASKET)))
   (CONS    4 (DT-PREDICATE.CREATE "TRUTH" '(INSCRIPT)))
   (CONS    3 (DT-PREDICATE.CREATE "PORTRAIT" '(CASKET)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   11 (DT-CONSTANT.CREATE "lead" 'CASKET))
   (CONS   10 (DT-CONSTANT.CREATE "silver" 'CASKET))
   (CONS    9 (DT-CONSTANT.CREATE "gold" 'CASKET))
   (CONS   13 (DT-FUNCTION.CREATE "second" 'INSCRIPT '(CASKET) 'NIL))
   (CONS   12 (DT-FUNCTION.CREATE "first" 'INSCRIPT '(CASKET) 'NIL)))))
   (RESULT))
(indices ((182 44) (168 35 36 37) (156 32 33 34) (143 29 30 31) (133 26 27 28) (110 23 24 25) (89 20 21 22) (71 17 18 19) (56 14 15 16) (47 1 2 3) (46 12 13) (45 10 11) (41 8 9) (39 6 7) (38 4 5) (37 43) (36 42) (35 41) (34 40) (33 39) (32 38) (31 NIL)))

(SPLITPARTS "Genera 8.0.1 Logical Pathnames Translation Files NEWEST IP-TCP 422.2 RPC 415.0 Embedding Support 407.0 UX Support 416.0 KKL 19.3 HADES 14.21 Waltz 6.0 COLUMN 6.0 Experimental MKRP 48.5" "23-JUL,1991 09:25" NIL)

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

(REFUTATION (START.TIME 77075609)

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
   (INITIAL   (CLAUSE 31 R.= AXIOM ((29 . ANY)) ((+ 2 (29 29)) ))
(CLAUSE 32 A1 AXIOM () ((+ 6 ((12 9) 9)) ))
(CLAUSE 33 A2 AXIOM () ((+ 7 ((13 9))) ))
(CLAUSE 34 A3 AXIOM () ((+ 6 ((12 10) 9)) ))
(CLAUSE 35 A4 AXIOM () ((+ 8 ((13 10))) ))
(CLAUSE 36 A5 AXIOM () ((+ 6 ((12 11) 11)) ))
(CLAUSE 37 A6 AXIOM () ((+ 5 ((13 11) 10)) ))
(CLAUSE 39 A8 AXIOM ()
   ((- 3 (9)) (- 3 (11)) ))
(CLAUSE 45 A10 AXIOM ((14 . CASKET))
   ((+ 4 ((12 14))) (+ 4 ((13 14))) ))
(CLAUSE 46 A11 AXIOM ()
   ((- 15 NIL) (- 16 NIL) ))
(CLAUSE 47 A12 THEOREM ()
   ((+ 3 (9)) (+ 3 (11)) ))
(CLAUSE 56 A13 AXIOM ((18 . CASKET)(17 . INSCRIPT))
   ((- 5 (17 18)) (- 4 (17)) (+ 3 (18)) ))
(CLAUSE 89 A15 AXIOM ((22 . CASKET)(21 . INSCRIPT))
   ((- 6 (21 22)) (- 4 (21)) (- 3 (22)) ))
(CLAUSE 110 A16 AXIOM ((24 . CASKET)(23 . INSCRIPT))
   ((- 6 (23 24)) (+ 4 (23)) (+ 3 (24)) ))
(CLAUSE 133 A17 AXIOM ((25 . INSCRIPT))
   ((- 7 (25)) (- 4 (25)) (+ 15 NIL) ))
(CLAUSE 143 A18 AXIOM ((26 . INSCRIPT))
   ((- 7 (26)) (+ 4 (26)) (- 15 NIL) ))
(CLAUSE 156 A19 AXIOM ((27 . INSCRIPT))
   ((- 8 (27)) (- 4 (27)) (+ 16 NIL) ))
(CLAUSE 168 A20 AXIOM ((28 . INSCRIPT))
   ((- 8 (28)) (+ 4 (28)) (- 16 NIL) ))
(CLAUSE 182 T21 THEOREM () ((- 3 (10)) ))   )
   (OPERATION (CLAUSE 51 R1 (168 35) ()
   ((+ 4 ((13 10))) (- 16 NIL) ))
   (RESOLUTION 35 1 168 1 NIL (28 (13 10)) 51))
   (OPERATION (CLAUSE 126 R2 (156 35) ()
   ((- 4 ((13 10))) (+ 16 NIL) ))
   (RESOLUTION 35 1 156 1 NIL (27 (13 10)) 126))
   (OPERATION (CLAUSE 72 R3 (143 33) ()
   ((+ 4 ((13 9))) (- 15 NIL) ))
   (RESOLUTION 33 1 143 1 NIL (26 (13 9)) 72))
   (OPERATION (CLAUSE 53 R4 (133 33) ()
   ((- 4 ((13 9))) (+ 15 NIL) ))
   (RESOLUTION 33 1 133 1 NIL (25 (13 9)) 53))
   (OPERATION (CLAUSE 172 R5 (46 53) ()
   ((- 4 ((13 9))) (- 16 NIL) ))
   (RESOLUTION 53 2 46 1 NIL NIL 172))
   (OPERATION (CLAUSE 83 R6 (56 37) ()
   ((- 4 ((13 11))) (+ 3 (10)) ))
   (RESOLUTION 37 1 56 1 NIL (17 (13 11) 18 10) 83))
   (OPERATION (CLAUSE 83 R6 (56 37) ()
   ((- 4 ((13 11))) (+ 3 (10)) ))
   (REPLACEMENT.OPERATION NIL (83 182)
  (((83 . 2) (182 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 147 R7 (182 110) ((148 . INSCRIPT))
   ((- 6 (148 10)) (+ 4 (148)) ))
   (RESOLUTION 110 3 182 1 NIL (24 10) 147))
   (OPERATION (CLAUSE 139 R8 (89 47) ((174 . INSCRIPT))
   ((+ 3 (11)) (- 6 (174 9)) (- 4 (174)) ))
   (RESOLUTION 47 1 89 3 NIL (22 9) 139))
   (OPERATION (CLAUSE 50 R9 (139 32) ()
   ((+ 3 (11)) (- 4 ((12 9))) ))
   (RESOLUTION 32 1 139 2 NIL (174 (12 9)) 50))
   (OPERATION (CLAUSE 163 R10 (139 34) ()
   ((+ 3 (11)) (- 4 ((12 10))) ))
   (RESOLUTION 34 1 139 2 NIL (174 (12 10)) 163))
   (OPERATION (CLAUSE 101 R11 (50 45) ()
   ((+ 4 ((13 9))) (+ 3 (11)) ))
   (RESOLUTION 45 1 50 2 NIL (14 9) 101))
   (OPERATION (CLAUSE 124 R12 (163 45) ()
   ((+ 4 ((13 10))) (+ 3 (11)) ))
   (RESOLUTION 45 1 163 2 NIL (14 10) 124))
   (OPERATION (CLAUSE 153 R13 (172 101) ()
   ((+ 3 (11)) (- 16 NIL) ))
   (RESOLUTION 101 1 172 1 NIL NIL 153))
   (OPERATION (CLAUSE 125 R14 (126 124) ()
   ((+ 3 (11)) (+ 16 NIL) ))
   (RESOLUTION 124 1 126 1 NIL NIL 125))
   (OPERATION (CLAUSE 125 R14 (126 124) ()
   ((+ 3 (11)) (+ 16 NIL) ))
   (REPLACEMENT.OPERATION NIL (125 153)
  (((125 . 2) (153 . 2) NIL))
  ((((125 . 1) (153 . 1) NIL)))
  NIL))
   (OPERATION (CLAUSE 39 A8 AXIOM ()
   ((- 3 (9)) (- 3 (11)) ))
   (REPLACEMENT.OPERATION NIL (39 125)
  (((39 . 2) (125 . 1) NIL))
  (NIL)
  NIL))
   (OPERATION (CLAUSE 48 R15 (39 110) ((57 . INSCRIPT))
   ((- 6 (57 9)) (+ 4 (57)) ))
   (RESOLUTION 110 3 39 1 NIL (24 9) 48))
   (OPERATION (CLAUSE 105 R16 (83 48) () ((- 6 ((13 11) 9)) ))
   (RESOLUTION 48 2 83 1 NIL (57 (13 11)) 105))
   (OPERATION (CLAUSE 120 R17 (48 32) () ((+ 4 ((12 9))) ))
   (RESOLUTION 32 1 48 1 NIL (57 (12 9)) 120))
   (OPERATION (CLAUSE 189 R18 (48 34) () ((+ 4 ((12 10))) ))
   (RESOLUTION 34 1 48 1 NIL (57 (12 10)) 189))
   (OPERATION (CLAUSE 112 R19 (89 120) ((187 . CASKET))
   ((- 6 ((12 9) 187)) (- 3 (187)) ))
   (RESOLUTION 120 1 89 2 NIL (21 (12 9)) 112))
   (OPERATION (CLAUSE 59 R20 (89 189) ((61 . CASKET))
   ((- 6 ((12 10) 61)) (- 3 (61)) ))
   (RESOLUTION 189 1 89 2 NIL (21 (12 10)) 59))
   (OPERATION (CLAUSE 149 R21 (89 125) ((144 . INSCRIPT))
   ((- 6 (144 11)) (- 4 (144)) ))
   (RESOLUTION 125 1 89 3 NIL (22 11) 149))
   (OPERATION (CLAUSE 149 R21 (89 125) ((144 . INSCRIPT))
   ((- 6 (144 11)) (- 4 (144)) ))
   (REPLACEMENT.OPERATION (144 (12 11) 14 11) (149 45 36 83)
  (((149 . 2) (45 . 1) NIL) ((149 . 1) (36 . 1) NIL) ((45 . 2) (83 . 1) NIL))
  (NIL NIL NIL)
  NIL))
   (END.TIME 77136967)
   (SYMBOLS 
   (LET (NEW.ADDRESS)
   (LIST (CONS   16 (DT-PREDICATE.CREATE "FLORENCE" 'NIL))
   (CONS   15 (DT-PREDICATE.CREATE "VENICE" 'NIL))
   (CONS    8 (DT-PREDICATE.CREATE "INSCRIPTION.FLORENCE" '(INSCRIPT)))
   (CONS    7 (DT-PREDICATE.CREATE "INSCRIPTION.VENICE" '(INSCRIPT)))
   (CONS    6 (DT-PREDICATE.CREATE "NEGATIVE.INSCRIPTION" '(INSCRIPT CASKET)))
   (CONS    5 (DT-PREDICATE.CREATE "INSCRIPTION" '(INSCRIPT CASKET)))
   (CONS    4 (DT-PREDICATE.CREATE "TRUTH" '(INSCRIPT)))
   (CONS    3 (DT-PREDICATE.CREATE "PORTRAIT" '(CASKET)))
   (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
   (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
   (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
   (CONS   11 (DT-CONSTANT.CREATE "lead" 'CASKET))
   (CONS   10 (DT-CONSTANT.CREATE "silver" 'CASKET))
   (CONS    9 (DT-CONSTANT.CREATE "gold" 'CASKET))
   (CONS   13 (DT-FUNCTION.CREATE "second" 'INSCRIPT '(CASKET) 'NIL))
   (CONS   12 (DT-FUNCTION.CREATE "first" 'INSCRIPT '(CASKET) 'NIL)))))
   (SPLITPART.IDENTIFIER 1)
   (RESULT SUCCESS 149)
)