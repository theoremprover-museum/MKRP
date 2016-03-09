;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(CONSTRUCTION  "Unix" "08-JUL,1992 21:39" 
              ("Edit:     Axioms and Theorems edited: 08-APR,1992 21:01 "
               ))

(AXIOMS.INFIX    ((* AXIOMS *)
                  ((ALL X W (X) IMPL A (X)) AND (EX X W (X)))
                  ((ALL X F (X) IMPL A (X)) AND (EX X F (X)))
                  ((ALL X B (X) IMPL A (X)) AND (EX X B (X)))
                  ((ALL X C (X) IMPL A (X)) AND (EX X C (X)))
                  ((ALL X S (X) IMPL A (X)) AND (EX X S (X)))
                  ((EX X G (X)) AND (ALL X G (X) IMPL P (X)))
                  (ALL X A (X) IMPL (ALL Y P (Y) IMPL E (X Y)) OR (ALL Y (A (Y) AND SM (Y X) AND (EX Z P (Z) AND E (Y Z))) IMPL E (X Y)))
                  (ALL |X,Y| B (Y) AND (S (X) OR C (X)) IMPL SM (X Y))
                  (ALL |X,Y| B (X) AND F (Y) IMPL SM (X Y))
                  (ALL |X,Y| F (X) AND W (Y) IMPL SM (X Y))
                  (ALL |X,Y| W (X) AND (F (Y) OR G (Y)) IMPL NOT E (X Y))
                  (ALL |X,Y| B (X) AND C (Y) IMPL E (X Y))
                  (ALL |X,Y| B (X) AND S (Y) IMPL NOT E (X Y))
                  (ALL X C (X) OR S (X) IMPL (EX Y P (Y) AND E (X Y)))))

(THEOREMS.INFIX ((* THEOREMS *)
                  (EX |X,Y| A (X) AND A (Y) AND (EX Z G (Z) AND E (Y Z) AND E (X Y)))))

(AXIOMS.PREFIX   (COMMENT
                  (AND (ALL 3 (IMPL (+ 4 (3) NIL) (+ 5 (3) NIL))) (EX 6 (+ 4 (6) NIL)))
                  (AND (ALL 7 (IMPL (+ 8 (7) NIL) (+ 5 (7) NIL))) (EX 9 (+ 8 (9) NIL)))
                  (AND (ALL 10 (IMPL (+ 11 (10) NIL) (+ 5 (10) NIL))) (EX 12 (+ 11 (12) NIL)))
                  (AND (ALL 13 (IMPL (+ 14 (13) NIL) (+ 5 (13) NIL))) (EX 15 (+ 14 (15) NIL)))
                  (AND (ALL 16 (IMPL (+ 17 (16) NIL) (+ 5 (16) NIL))) (EX 18 (+ 17 (18) NIL)))
                  (AND (EX 19 (+ 20 (19) NIL)) (ALL 21 (IMPL (+ 20 (21) NIL) (+ 22 (21) NIL))))
                  (ALL 23 (IMPL (+ 5 (23) NIL) (OR (ALL 24 (IMPL (+ 22 (24) NIL) (+ 25 (23 24) NIL))) (ALL 26 (IMPL (AND (+ 5 (26) NIL) (AND (+ 27 (26 23) NIL) (EX 28 (AND (+ 22 (28) NIL) (+ 25 (26 28) NIL))))) (+ 25 (23 26) NIL))))))
                  (ALL 30 (ALL 29 (IMPL (AND (+ 11 (30) NIL) (OR (+ 17 (29) NIL) (+ 14 (29) NIL))) (+ 27 (29 30) NIL))))
                  (ALL 32 (ALL 31 (IMPL (AND (+ 11 (31) NIL) (+ 8 (32) NIL)) (+ 27 (31 32) NIL))))
                  (ALL 34 (ALL 33 (IMPL (AND (+ 8 (33) NIL) (+ 4 (34) NIL)) (+ 27 (33 34) NIL))))
                  (ALL 36 (ALL 35 (IMPL (AND (+ 4 (35) NIL) (OR (+ 8 (36) NIL) (+ 20 (36) NIL))) (NOT (+ 25 (35 36) NIL)))))
                  (ALL 38 (ALL 37 (IMPL (AND (+ 11 (37) NIL) (+ 14 (38) NIL)) (+ 25 (37 38) NIL))))
                  (ALL 40 (ALL 39 (IMPL (AND (+ 11 (39) NIL) (+ 17 (40) NIL)) (NOT (+ 25 (39 40) NIL)))))
                  (ALL 41 (IMPL (OR (+ 14 (41) NIL) (+ 17 (41) NIL)) (EX 42 (AND (+ 22 (42) NIL) (+ 25 (41 42) NIL)))))))

(THEOREMS.PREFIX (COMMENT
                  (EX 44 (EX 43 (AND (+ 5 (43) NIL) (AND (+ 5 (44) NIL) (EX 45 (AND (+ 20 (45) NIL) (AND (+ 25 (44 45) NIL) (+ 25 (43 44) NIL))))))))))

(OPTIONS (TWO_RULES)
          (TWO_RULES.MAXLEVEL . 1)
          (TWO_SUPPRESS.NORULES)
          (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.I_CLAUSE.PURITY)
          (RED.I_CLAUSE.TAUTOLOGY . T)
          (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
          (RED.I_CLAUSE.SUBSUMPTION . T)
          (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
          (RED.I_CLAUSE.REPL.FACTORING . T)
          (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
          (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
          (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
          (RED.I_CLAUSE.REWRITING . DEM)
          (RED.I_LINK.INCOMPATIBILITY)
          (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
          (RED.I_LINK.TAUTOLOGY.RECHECK)
          (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
          (RED.I_LINK.SUBSUMPTION.RECHECK)
          (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.D_CLAUSE.PURITY)
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
          (TERM_ITERATIONS . 5)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS)
          (SORT_MAX.UNIFICATION.RULE.STEPS . 100)
          (SORT_MAX.UNIFICATION.TREE.DEPTH . 100)
          (SORT_MAX.UNIFICATION.TREE.OPEN.NODES . 20)
          (SORT_UNIFIER.STOP.NUMBER . 100)
          (SORT_SHOW.VARIABLE.SORTS . T)
          (ER_PARAMODULATION . ZHANG-KAPUR)
          (ER_WEIGHT.POLYNOMIALS)
          (ER_P.SELECTION * WEIGHT (IF SUPPORT 5 1))
          (ER_COMPLETION . UNFAILING)
          (ER_CP.REDUCTION . T)
          (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
          (ER_OPERATOR.ORDERING G S C B F W P A SM E)
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
          (PR_LEFT.MARGIN . 3)
          (PR_RIGHT.MARGIN . 117)
          (PR_LATEX)
          (PR_LINELENGTH . 114)
          (PR_LITERALS)
         )

(LINK.COLOURS (R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(AXIOMS (START.TIME 2105560000)
        (PARTIAL   (CLAUSE 55 R.= AXIOM ((53 . ANY)) ((+ 2 (53 53)) ))(CLAUSE 56 A1 AXIOM () ((+ 4 (46)) )))
        (PARTIAL   (CLAUSE 57 A2 AXIOM () ((+ 8 (6)) )))
        (PARTIAL   (CLAUSE 58 A3 AXIOM () ((+ 11 (9)) )))
        (PARTIAL   (CLAUSE 59 A4 AXIOM () ((+ 14 (12)) )))
        (PARTIAL   (CLAUSE 60 A5 AXIOM () ((+ 17 (15)) )))
        (PARTIAL   (CLAUSE 61 A6 AXIOM () ((+ 20 (18)) )))
        (PARTIAL   (CLAUSE 63 A7 AXIOM ((3 . ANY))
                         ((- 4 (3)) (+ 5 (3)) )))
        (PARTIAL   (CLAUSE 66 A8 AXIOM ((7 . ANY))
                         ((- 8 (7)) (+ 5 (7)) )))
        (PARTIAL   (CLAUSE 70 A9 AXIOM ((10 . ANY))
                         ((- 11 (10)) (+ 5 (10)) )))
        (PARTIAL   (CLAUSE 75 A10 AXIOM ((13 . ANY))
                         ((- 14 (13)) (+ 5 (13)) )))
        (PARTIAL   (CLAUSE 81 A11 AXIOM ((16 . ANY))
                         ((- 17 (16)) (+ 5 (16)) )))
        (PARTIAL   (CLAUSE 88 A12 AXIOM ((19 . ANY))
                         ((- 20 (19)) (+ 22 (19)) )))
        (PARTIAL   (CLAUSE 91 A13 AXIOM ((41 . ANY))
                         ((- 14 (41)) (+ 22 ((50 41))) )))
        (PARTIAL   (CLAUSE 96 A14 AXIOM ((51 . ANY))
                         ((- 14 (51)) (+ 25 (51 (50 51))) )))
        (PARTIAL   (CLAUSE 101 A15 AXIOM ((42 . ANY))
                         ((- 17 (42)) (+ 22 ((50 42))) )))
        (PARTIAL   (CLAUSE 107 A16 AXIOM ((52 . ANY))
                         ((- 17 (52)) (+ 25 (52 (50 52))) )))
        (PARTIAL   (CLAUSE 114 A17 AXIOM ((29 . ANY)(30 . ANY))
                         ((- 11 (30)) (- 17 (29)) (+ 27 (29 30)) )))
        (PARTIAL   (CLAUSE 123 A18 AXIOM ((47 . ANY)(21 . ANY))
                         ((- 11 (21)) (- 14 (47)) (+ 27 (47 21)) )))
        (PARTIAL   (CLAUSE 134 A19 AXIOM ((32 . ANY)(31 . ANY))
                         ((- 11 (31)) (- 8 (32)) (+ 27 (31 32)) )))
        (PARTIAL   (CLAUSE 145 A20 AXIOM ((34 . ANY)(33 . ANY))
                         ((- 8 (33)) (- 4 (34)) (+ 27 (33 34)) )))
        (PARTIAL   (CLAUSE 156 A21 AXIOM ((36 . ANY)(35 . ANY))
                         ((- 4 (35)) (- 8 (36)) (- 25 (35 36)) )))
        (PARTIAL   (CLAUSE 168 A22 AXIOM ((48 . ANY)(49 . ANY))
                         ((- 4 (49)) (- 20 (48)) (- 25 (49 48)) )))
        (PARTIAL   (CLAUSE 180 A23 AXIOM ((38 . ANY)(37 . ANY))
                         ((- 11 (37)) (- 14 (38)) (+ 25 (37 38)) )))
        (PARTIAL   (CLAUSE 197 A24 AXIOM ((40 . ANY)(39 . ANY))
                         ((- 11 (39)) (- 17 (40)) (- 25 (39 40)) )))
        (PARTIAL   (CLAUSE 218 A25 AXIOM ((28 . ANY)(26 . ANY)(24 . ANY)(23 . ANY))
                         ((- 5 (23)) (- 22 (24)) (+ 25 (23 24)) (- 5 (26)) (- 27 (26 23)) (- 22 (28)) (- 25 (26 28)) (+ 25 (23 26)) )))
        (END.TIME 2117220000)
        (FINAL 55 56 57 58 59 60 61 63 66 70 75 81 88 91 96 101 107 114 123 134 145 156 168 180 197 218)
        (SYMBOLS 
                 (LET (NEW.ADDRESS)
          (LIST (CONS   27 (DT-PREDICATE.CREATE "SM" '(ANY ANY)))
                         (CONS   25 (DT-PREDICATE.CREATE "E" '(ANY ANY)))
                         (CONS   22 (DT-PREDICATE.CREATE "P" '(ANY)))
                         (CONS   20 (DT-PREDICATE.CREATE "G" '(ANY)))
                         (CONS   17 (DT-PREDICATE.CREATE "S" '(ANY)))
                         (CONS   14 (DT-PREDICATE.CREATE "C" '(ANY)))
                         (CONS   11 (DT-PREDICATE.CREATE "B" '(ANY)))
                         (CONS    8 (DT-PREDICATE.CREATE "F" '(ANY)))
                         (CONS    5 (DT-PREDICATE.CREATE "A" '(ANY)))
                         (CONS    4 (DT-PREDICATE.CREATE "W" '(ANY)))
                         (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
                         (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                         (CONS   18 (DT-CONSTANT.CREATE "c_6" 'ANY))
                         (CONS   15 (DT-CONSTANT.CREATE "c_5" 'ANY))
                         (CONS   12 (DT-CONSTANT.CREATE "c_4" 'ANY))
                         (CONS    9 (DT-CONSTANT.CREATE "c_3" 'ANY))
                         (CONS    6 (DT-CONSTANT.CREATE "c_2" 'ANY))
                         (CONS   46 (DT-CONSTANT.CREATE "c_1" 'ANY))
                         (CONS   50 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
        (RESULT))

(THEOREMS (SPLITPART.IDENTIFIER 1)
          (START.TIME 2117350000)
          (PARTIAL   (CLAUSE 270 T26 THEOREM ((45 . ANY)(44 . ANY)(43 . ANY))
                           ((- 5 (43)) (- 5 (44)) (- 20 (45)) (- 25 (44 45)) (- 25 (43 44)) )))
          (INITIAL   (CLAUSE 55 R.= AXIOM ((53 . ANY)) ((+ 2 (53 53)) ))
(CLAUSE 56 A1 AXIOM () ((+ 4 (46)) ))
(CLAUSE 57 A2 AXIOM () ((+ 8 (6)) ))
(CLAUSE 58 A3 AXIOM () ((+ 11 (9)) ))
(CLAUSE 59 A4 AXIOM () ((+ 14 (12)) ))
(CLAUSE 60 A5 AXIOM () ((+ 17 (15)) ))
(CLAUSE 61 A6 AXIOM () ((+ 20 (18)) ))
(CLAUSE 63 A7 AXIOM ((3 . ANY))
                           ((- 4 (3)) (+ 5 (3)) ))
(CLAUSE 66 A8 AXIOM ((7 . ANY))
                           ((- 8 (7)) (+ 5 (7)) ))
(CLAUSE 70 A9 AXIOM ((10 . ANY))
                           ((- 11 (10)) (+ 5 (10)) ))
(CLAUSE 75 A10 AXIOM ((13 . ANY))
                           ((- 14 (13)) (+ 5 (13)) ))
(CLAUSE 81 A11 AXIOM ((16 . ANY))
                           ((- 17 (16)) (+ 5 (16)) ))
(CLAUSE 88 A12 AXIOM ((19 . ANY))
                           ((- 20 (19)) (+ 22 (19)) ))
(CLAUSE 91 A13 AXIOM ((41 . ANY))
                           ((- 14 (41)) (+ 22 ((50 41))) ))
(CLAUSE 96 A14 AXIOM ((51 . ANY))
                           ((- 14 (51)) (+ 25 (51 (50 51))) ))
(CLAUSE 101 A15 AXIOM ((42 . ANY))
                           ((- 17 (42)) (+ 22 ((50 42))) ))
(CLAUSE 107 A16 AXIOM ((52 . ANY))
                           ((- 17 (52)) (+ 25 (52 (50 52))) ))
(CLAUSE 114 A17 AXIOM ((29 . ANY)(30 . ANY))
                           ((- 11 (30)) (- 17 (29)) (+ 27 (29 30)) ))
(CLAUSE 123 A18 AXIOM ((47 . ANY)(21 . ANY))
                           ((- 11 (21)) (- 14 (47)) (+ 27 (47 21)) ))
(CLAUSE 134 A19 AXIOM ((32 . ANY)(31 . ANY))
                           ((- 11 (31)) (- 8 (32)) (+ 27 (31 32)) ))
(CLAUSE 145 A20 AXIOM ((34 . ANY)(33 . ANY))
                           ((- 8 (33)) (- 4 (34)) (+ 27 (33 34)) ))
(CLAUSE 156 A21 AXIOM ((36 . ANY)(35 . ANY))
                           ((- 4 (35)) (- 8 (36)) (- 25 (35 36)) ))
(CLAUSE 168 A22 AXIOM ((48 . ANY)(49 . ANY))
                           ((- 4 (49)) (- 20 (48)) (- 25 (49 48)) ))
(CLAUSE 180 A23 AXIOM ((38 . ANY)(37 . ANY))
                           ((- 11 (37)) (- 14 (38)) (+ 25 (37 38)) ))
(CLAUSE 197 A24 AXIOM ((40 . ANY)(39 . ANY))
                           ((- 11 (39)) (- 17 (40)) (- 25 (39 40)) ))
(CLAUSE 218 A25 AXIOM ((28 . ANY)(26 . ANY)(24 . ANY)(23 . ANY))
                           ((- 5 (23)) (- 22 (24)) (+ 25 (23 24)) (- 5 (26)) (- 27 (26 23)) (- 22 (28)) (- 25 (26 28)) (+ 25 (23 26)) ))
(CLAUSE 270 T26 THEOREM ((45 . ANY)(44 . ANY)(43 . ANY))
                           ((- 5 (43)) (- 5 (44)) (- 20 (45)) (- 25 (44 45)) (- 25 (43 44)) )) )
          (END.TIME 2230460000)
          (FINAL 55 56 57 58 59 60 61 63 66 70 75 81 88 91 96 101 107 114 123 134 145 156 168 180 197 218 270)
          (SYMBOLS 
                   (LET (NEW.ADDRESS)
          (LIST (CONS   27 (DT-PREDICATE.CREATE "SM" '(ANY ANY)))
                           (CONS   25 (DT-PREDICATE.CREATE "E" '(ANY ANY)))
                           (CONS   22 (DT-PREDICATE.CREATE "P" '(ANY)))
                           (CONS   20 (DT-PREDICATE.CREATE "G" '(ANY)))
                           (CONS   17 (DT-PREDICATE.CREATE "S" '(ANY)))
                           (CONS   14 (DT-PREDICATE.CREATE "C" '(ANY)))
                           (CONS   11 (DT-PREDICATE.CREATE "B" '(ANY)))
                           (CONS    8 (DT-PREDICATE.CREATE "F" '(ANY)))
                           (CONS    5 (DT-PREDICATE.CREATE "A" '(ANY)))
                           (CONS    4 (DT-PREDICATE.CREATE "W" '(ANY)))
                           (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
                           (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                  (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                           (CONS   18 (DT-CONSTANT.CREATE "c_6" 'ANY))
                           (CONS   15 (DT-CONSTANT.CREATE "c_5" 'ANY))
                           (CONS   12 (DT-CONSTANT.CREATE "c_4" 'ANY))
                           (CONS    9 (DT-CONSTANT.CREATE "c_3" 'ANY))
                           (CONS    6 (DT-CONSTANT.CREATE "c_2" 'ANY))
                           (CONS   46 (DT-CONSTANT.CREATE "c_1" 'ANY))
                           (CONS   50 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
          (RESULT))
(indices ((270 51 52 53 54 55) (218 19 20 21 22 23 24 25 26) (197 44 45 46) (180 41 42 43) (168 37 39 40) (156 37 38 40) (145 34 35 36) (134 31 32 33) (123 27 29 30) (114 27 28 30) (107 48 50) (101 48 49) (96 47 50) (91 47 49) (88 17 18) (81 13 14) (75 10 11) (70 7 8) (66 4 5) (63 1 2) (61 16) (60 15) (59 12) (58 9) (57 6) (56 3) (55 NIL)))

(SPLITPARTS "Unix" "08-JUL,1992 21:41" NIL)

(OPTIONS (TWO_RULES)
          (TWO_RULES.MAXLEVEL . 1)
          (TWO_SUPPRESS.NORULES)
          (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.I_CLAUSE.PURITY)
          (RED.I_CLAUSE.TAUTOLOGY . T)
          (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
          (RED.I_CLAUSE.SUBSUMPTION . T)
          (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
          (RED.I_CLAUSE.REPL.FACTORING . T)
          (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
          (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
          (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
          (RED.I_CLAUSE.REWRITING . DEM)
          (RED.I_LINK.INCOMPATIBILITY)
          (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
          (RED.I_LINK.TAUTOLOGY.RECHECK)
          (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
          (RED.I_LINK.SUBSUMPTION.RECHECK)
          (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.D_CLAUSE.PURITY)
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
          (TERM_ITERATIONS . 5)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS)
          (SORT_MAX.UNIFICATION.RULE.STEPS . 100)
          (SORT_MAX.UNIFICATION.TREE.DEPTH . 100)
          (SORT_MAX.UNIFICATION.TREE.OPEN.NODES . 20)
          (SORT_UNIFIER.STOP.NUMBER . 100)
          (SORT_SHOW.VARIABLE.SORTS . T)
          (ER_PARAMODULATION . ZHANG-KAPUR)
          (ER_WEIGHT.POLYNOMIALS)
          (ER_P.SELECTION * WEIGHT (IF SUPPORT 5 1))
          (ER_COMPLETION . UNFAILING)
          (ER_CP.REDUCTION . T)
          (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
          (ER_OPERATOR.ORDERING G S C B F W P A SM E)
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
          (PR_LEFT.MARGIN . 3)
          (PR_RIGHT.MARGIN . 117)
          (PR_LATEX)
          (PR_LINELENGTH . 114)
          (PR_LITERALS)
         )

(REFUTATION (START.TIME 2231020000)

(OPTIONS (TWO_RULES)
          (TWO_RULES.MAXLEVEL . 1)
          (TWO_SUPPRESS.NORULES)
          (RED.I_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.I_CLAUSE.PURITY)
          (RED.I_CLAUSE.TAUTOLOGY . T)
          (RED.I_CLAUSE.TAUTOLOGY.RECHECK . PARTIAL)
          (RED.I_CLAUSE.SUBSUMPTION . T)
          (RED.I_CLAUSE.SUBSUMPTION.RECHECK . PARTIAL)
          (RED.I_CLAUSE.REPL.FACTORING . T)
          (RED.I_CLAUSE.REPL.FACTORING.RECHECK . T)
          (RED.I_CLAUSE.REPL.RESOLUTION . SIMPLE)
          (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK . T)
          (RED.I_CLAUSE.REWRITING . DEM)
          (RED.I_LINK.INCOMPATIBILITY)
          (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
          (RED.I_LINK.TAUTOLOGY.RECHECK)
          (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
          (RED.I_LINK.SUBSUMPTION.RECHECK)
          (RED.D_CLAUSE.MULTIPLE.LITERALS . T)
          (RED.D_CLAUSE.PURITY)
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
          (TERM_ITERATIONS . 5)
          (TERM_SET.OF.SUPPORT)
          (TERM_BREADTH.FIRST)
          (SORT_LITERALS)
          (SORT_MAX.UNIFICATION.RULE.STEPS . 100)
          (SORT_MAX.UNIFICATION.TREE.DEPTH . 100)
          (SORT_MAX.UNIFICATION.TREE.OPEN.NODES . 20)
          (SORT_UNIFIER.STOP.NUMBER . 100)
          (SORT_SHOW.VARIABLE.SORTS . T)
          (ER_PARAMODULATION . ZHANG-KAPUR)
          (ER_WEIGHT.POLYNOMIALS)
          (ER_P.SELECTION * WEIGHT (IF SUPPORT 5 1))
          (ER_COMPLETION . UNFAILING)
          (ER_CP.REDUCTION . T)
          (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
          (ER_OPERATOR.ORDERING G S C B F W P A SM E)
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
          (PR_LEFT.MARGIN . 3)
          (PR_RIGHT.MARGIN . 117)
          (PR_LATEX)
          (PR_LINELENGTH . 114)
          (PR_LITERALS)
         )
            (INITIAL   (CLAUSE 55 R.= AXIOM ((53 . ANY)) ((+ 2 (53 53)) ))
(CLAUSE 56 A1 AXIOM () ((+ 4 (46)) ))
(CLAUSE 57 A2 AXIOM () ((+ 8 (6)) ))
(CLAUSE 58 A3 AXIOM () ((+ 11 (9)) ))
(CLAUSE 59 A4 AXIOM () ((+ 14 (12)) ))
(CLAUSE 60 A5 AXIOM () ((+ 17 (15)) ))
(CLAUSE 61 A6 AXIOM () ((+ 20 (18)) ))
(CLAUSE 63 A7 AXIOM ((3 . ANY))
                             ((- 4 (3)) (+ 5 (3)) ))
(CLAUSE 66 A8 AXIOM ((7 . ANY))
                             ((- 8 (7)) (+ 5 (7)) ))
(CLAUSE 70 A9 AXIOM ((10 . ANY))
                             ((- 11 (10)) (+ 5 (10)) ))
(CLAUSE 75 A10 AXIOM ((13 . ANY))
                             ((- 14 (13)) (+ 5 (13)) ))
(CLAUSE 81 A11 AXIOM ((16 . ANY))
                             ((- 17 (16)) (+ 5 (16)) ))
(CLAUSE 88 A12 AXIOM ((19 . ANY))
                             ((- 20 (19)) (+ 22 (19)) ))
(CLAUSE 91 A13 AXIOM ((41 . ANY))
                             ((- 14 (41)) (+ 22 ((50 41))) ))
(CLAUSE 96 A14 AXIOM ((51 . ANY))
                             ((- 14 (51)) (+ 25 (51 (50 51))) ))
(CLAUSE 101 A15 AXIOM ((42 . ANY))
                             ((- 17 (42)) (+ 22 ((50 42))) ))
(CLAUSE 107 A16 AXIOM ((52 . ANY))
                             ((- 17 (52)) (+ 25 (52 (50 52))) ))
(CLAUSE 114 A17 AXIOM ((29 . ANY)(30 . ANY))
                             ((- 11 (30)) (- 17 (29)) (+ 27 (29 30)) ))
(CLAUSE 123 A18 AXIOM ((47 . ANY)(21 . ANY))
                             ((- 11 (21)) (- 14 (47)) (+ 27 (47 21)) ))
(CLAUSE 134 A19 AXIOM ((32 . ANY)(31 . ANY))
                             ((- 11 (31)) (- 8 (32)) (+ 27 (31 32)) ))
(CLAUSE 145 A20 AXIOM ((34 . ANY)(33 . ANY))
                             ((- 8 (33)) (- 4 (34)) (+ 27 (33 34)) ))
(CLAUSE 156 A21 AXIOM ((36 . ANY)(35 . ANY))
                             ((- 4 (35)) (- 8 (36)) (- 25 (35 36)) ))
(CLAUSE 168 A22 AXIOM ((48 . ANY)(49 . ANY))
                             ((- 4 (49)) (- 20 (48)) (- 25 (49 48)) ))
(CLAUSE 180 A23 AXIOM ((38 . ANY)(37 . ANY))
                             ((- 11 (37)) (- 14 (38)) (+ 25 (37 38)) ))
(CLAUSE 197 A24 AXIOM ((40 . ANY)(39 . ANY))
                             ((- 11 (39)) (- 17 (40)) (- 25 (39 40)) ))
(CLAUSE 218 A25 AXIOM ((28 . ANY)(26 . ANY)(24 . ANY)(23 . ANY))
                             ((- 5 (23)) (- 22 (24)) (+ 25 (23 24)) (- 5 (26)) (- 27 (26 23)) (- 22 (28)) (- 25 (26 28)) (+ 25 (23 26)) ))
(CLAUSE 270 T26 THEOREM ((45 . ANY)(44 . ANY)(43 . ANY))
                             ((- 5 (43)) (- 5 (44)) (- 20 (45)) (- 25 (44 45)) (- 25 (43 44)) )) )(OPERATION (CLAUSE 805 R1 (114 58) ()((- 17 (15))(+ 27 (15 9))))(RESOLUTION 58 1 114 1 NIL (30 9 29 15) 805))(OPERATION (CLAUSE 805 R1 (114 58 60) () ((+ 27 (15 9)) ))(REPLACEMENT.OPERATION NIL (805 60 )(((805 . 1)(60 . 1)NIL))( NIL )NIL))(OPERATION (CLAUSE 800 R2 (101 60) ()((+ 22 ((50 15)))))(RESOLUTION 60 1 101 1 NIL (42 15) 800))(OPERATION (CLAUSE 791 R3 (107 60) ()((+ 25 (15 (50 15)))))(RESOLUTION 60 1 107 1 NIL (52 15) 791))(OPERATION (CLAUSE 777 R4 (197 60) ()((- 11 (9))(- 25 (9 15))))(RESOLUTION 60 1 197 2 NIL (39 9 40 15) 777))(OPERATION (CLAUSE 777 R4 (197 60 58) () ((- 25 (9 15)) ))(REPLACEMENT.OPERATION NIL (777 58 )(((777 . 1)(58 . 1)NIL))( NIL )NIL))(OPERATION (CLAUSE 767 R5 (81 60) ()((+ 5 (15))))(RESOLUTION 60 1 81 1 NIL (16 15) 767))(OPERATION (CLAUSE 758 R6 (88 61) ()((+ 22 (18))))(RESOLUTION 61 1 88 1 NIL (19 18) 758))(OPERATION (CLAUSE 751 R7 (70 58) ()((+ 5 (9))))(RESOLUTION 58 1 70 1 NIL (10 9) 751))(OPERATION (CLAUSE 738 R8 (218 751) ()((- 22 (18))(+ 25 (9 18))(- 5 (15))(- 27 (15 9))(- 22 ((50 15)))(- 25 (15 (50 15)))(+ 25 (9 15))))(RESOLUTION 751 1 218 1 NIL (23 9 24 18 26 15 28 (50 15)) 738))(OPERATION (CLAUSE 738 R8 (218 751 758 767 777 791 800 805) () ((+ 25 (9 18)) ))(REPLACEMENT.OPERATION NIL (738 758 767 777 791 800 805 )(((738 . 1)(758 . 1)NIL)((738 . 3)(767 . 1)NIL)((738 . 7)(777 . 1)NIL)((738 . 6)(791 . 1)NIL)((738 . 5)(800 . 1)NIL)((738 . 4)(805 . 1)NIL))( NIL  NIL  NIL  NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 725 R9 (134 58) ()((- 8 (6))(+ 27 (9 6))))(RESOLUTION 58 1 134 1 NIL (31 9 32 6) 725))(OPERATION (CLAUSE 725 R9 (134 58 57) () ((+ 27 (9 6)) ))(REPLACEMENT.OPERATION NIL (725 57 )(((725 . 1)(57 . 1)NIL))( NIL )NIL))(OPERATION (CLAUSE 716 R10 (145 57) ()((- 4 (46))(+ 27 (6 46))))(RESOLUTION 57 1 145 1 NIL (33 6 34 46) 716))(OPERATION (CLAUSE 716 R10 (145 57 56) () ((+ 27 (6 46)) ))(REPLACEMENT.OPERATION NIL (716 56 )(((716 . 1)(56 . 1)NIL))( NIL )NIL))(OPERATION (CLAUSE 711 R11 (168 61) ()((- 4 (46))(- 25 (46 18))))(RESOLUTION 61 1 168 2 NIL (49 46 48 18) 711))(OPERATION (CLAUSE 711 R11 (168 61 56) () ((- 25 (46 18)) ))(REPLACEMENT.OPERATION NIL (711 56 )(((711 . 1)(56 . 1)NIL))( NIL )NIL))(OPERATION (CLAUSE 697 R12 (63 56) ()((+ 5 (46))))(RESOLUTION 56 1 63 1 NIL (3 46) 697))(OPERATION (CLAUSE 688 R13 (66 57) ()((+ 5 (6))))(RESOLUTION 57 1 66 1 NIL (7 6) 688))(OPERATION (CLAUSE 676 R14 (156 57) ()((- 4 (46))(- 25 (46 6))))(RESOLUTION 57 1 156 2 NIL (35 46 36 6) 676))(OPERATION (CLAUSE 676 R14 (156 57 56) () ((- 25 (46 6)) ))(REPLACEMENT.OPERATION NIL (676 56 )(((676 . 1)(56 . 1)NIL))( NIL )NIL))(OPERATION (CLAUSE 638 R15 (218 676) ()((- 5 (46))(- 22 (18))(+ 25 (46 18))(- 5 (6))(- 27 (6 46))(- 22 (18))(- 25 (6 18))))(RESOLUTION 218 8 676 1 NIL (23 46 24 18 26 6 28 18) 638))(OPERATION (CLAUSE 638 R15 (218 676 688 697 758 711 716) () ((- 25 (6 18)) ))(REPLACEMENT.OPERATION NIL (638 688 697 758 711 716 )(((638 . 4)(688 . 1)NIL)((638 . 1)(697 . 1)NIL)((638 . 2)(758 . 1)NIL)((638 . 3)(711 . 1)NIL)((638 . 5)(716 . 1)NIL))((((638 . 2) (638 . 6) NIL)) NIL  NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 647 R16 (270 751) ()((- 5 (6))(- 20 (18))(- 25 (9 18))(- 25 (6 9))))(RESOLUTION 751 1 270 2 NIL (43 6 44 9 45 18) 647))(OPERATION (CLAUSE 647 R16 (270 751 688 61 738) () ((- 25 (6 9)) ))(REPLACEMENT.OPERATION NIL (647 688 61 738 )(((647 . 1)(688 . 1)NIL)((647 . 2)(61 . 1)NIL)((647 . 3)(738 . 1)NIL))( NIL  NIL  NIL )NIL))(OPERATION (CLAUSE 662 R17 (218 647) ()((- 5 (6))(- 22 (18))(+ 25 (6 18))(- 5 (9))(- 27 (9 6))(- 22 (18))(- 25 (9 18))))(RESOLUTION 218 8 647 1 NIL (23 6 24 18 26 9 28 18) 662))(OPERATION (CLAUSE 662 R17 (218 647 758 751 688 758 638 725 738) () ())(REPLACEMENT.OPERATION NIL (662 758 751 688 758 638 725 738 )(((662 . 6)(758 . 1)NIL)((662 . 4)(751 . 1)NIL)((662 . 1)(688 . 1)NIL)((662 . 2)(758 . 1)NIL)((662 . 3)(638 . 1)NIL)((662 . 5)(725 . 1)NIL)((662 . 7)(738 . 1)NIL))( NIL  NIL  NIL  NIL  NIL  NIL  NIL )NIL))
            (END.TIME 2240780000)
            (SYMBOLS 
                     (LET (NEW.ADDRESS)
          (LIST (CONS   27 (DT-PREDICATE.CREATE "SM" '(ANY ANY)))
                             (CONS   25 (DT-PREDICATE.CREATE "E" '(ANY ANY)))
                             (CONS   22 (DT-PREDICATE.CREATE "P" '(ANY)))
                             (CONS   20 (DT-PREDICATE.CREATE "G" '(ANY)))
                             (CONS   17 (DT-PREDICATE.CREATE "S" '(ANY)))
                             (CONS   14 (DT-PREDICATE.CREATE "C" '(ANY)))
                             (CONS   11 (DT-PREDICATE.CREATE "B" '(ANY)))
                             (CONS    8 (DT-PREDICATE.CREATE "F" '(ANY)))
                             (CONS    5 (DT-PREDICATE.CREATE "A" '(ANY)))
                             (CONS    4 (DT-PREDICATE.CREATE "W" '(ANY)))
                             (PROG1 (CONS    2 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "=" '(ANY ANY))))
                                    (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE)))
                             (PROG1 (CONS    1 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "FALSE" 'NIL)))
                                    (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                             (PROG1 (CONS    0 (SETQ NEW.ADDRESS (DT-PREDICATE.CREATE "TRUE" 'NIL)))
                                    (DT-PREDICATE.ADD.ATTRIBUTES NEW.ADDRESS '(DEFINED)))
                             (CONS   18 (DT-CONSTANT.CREATE "c_6" 'ANY))
                             (CONS   15 (DT-CONSTANT.CREATE "c_5" 'ANY))
                             (CONS   12 (DT-CONSTANT.CREATE "c_4" 'ANY))
                             (CONS    9 (DT-CONSTANT.CREATE "c_3" 'ANY))
                             (CONS    6 (DT-CONSTANT.CREATE "c_2" 'ANY))
                             (CONS   46 (DT-CONSTANT.CREATE "c_1" 'ANY))
                             (CONS   50 (DT-FUNCTION.CREATE "f_1" 'ANY '(ANY) 'NIL)))))
            (SPLITPART.IDENTIFIER 1)
            (RESULT SUCCESS 662)
)