;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PROGN (PREP=CREATE.EMPTY.PARTIAL.GRAPH) (PROGN (SETQ TWO*RULES.MAXLEVEL (QUOTE 1)) (SETQ TWO*CONTROL (QUOTE NIL)) (SETQ TWO*SUPPRESS.NORULES (QUOTE NIL)) (SETQ TWO*DIALOG (QUOTE NIL)) (SETQ TWO*EXTERNAL.RULE.COLOURS (QUOTE (R S T))) (SETQ TWO*INTERNAL.RULE.COLOURS (QUOTE (RIW SI SIW TI TIW))) (SETQ TWO*RULES (QUOTE NIL)) (SETQ TWO*ADDED.RULES (QUOTE NIL)) (SETQ TWO*EMPTY.CLAUSE.FOUND (QUOTE NIL)) (SETQ TWO*NEW.RULES (QUOTE NIL)) (SETQ TWO*NORULE.CLAUSES (QUOTE NIL)) (SETQ TWO*TR.RULES (QUOTE NIL)) (SETQ TWO*TR.NORULES (QUOTE NIL))) (PROGN (C "Save expression of reduction.") (SETQ RED*RW_RULES (QUOTE NIL) RDS*RW_ASSIGN (QUOTE NIL) RDS*RW_VARIABLES (QUOTE NIL) RED*RW_RULES.COMPLETION (QUOTE NIL) RED*RW_RULES.UNFAILING (QUOTE ((27 . 1))) RED*GRAPH.ISOLATION (QUOTE T)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.TAUTOLOGY) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.REPL.RESOLUTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.SUBSUMPTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*LINK.SUBSUMPTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*LINK.TAUTOLOGY) (QUOTE NIL))) (PROGN (PROGN (PROGN (PROGN (DS-RESET) (PROGN (DT-RESET) (PROG ((ADDRLIST (QUOTE (9118 9123 9128 9133 9138 END -69 -68 -67 -66 9156 9174 9192 9210 9228 9246 9264 9282 9300 9318 9336 9354 9372 9390 9408 9426 9510 9515 9520 9525 9530 9535 9553 9571 9589 9607 9667 9672 9677 9682 9687 9705 9765 9770 9775 9780 9785 9821 9826 9836 9841 9846 9851 9890 9866 9831 9856 9861 9876 9113 9103 9098 9093 9108 9871 9895 9900 9905 9910 9924 9939 9954 9969 9984 9999))) (DATALIST (QUOTE (15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE))) ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" ((27 1)) NIL (ANY ANY) (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) 27 ((- 2 (POSITIVE . SYMMETRIC))) ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "A[IXITO]" ((48 3) (38 1) (32 1)) ((48 1 2) (38 2) (32 2)) (IXITO I I) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE))) ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "A[ITO]" NIL NIL (ITO I) NIL NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL))) ((- 4 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 14 "COMP" NIL IXITO (IXITO IXITO) NIL NIL NIL NIL NIL 2 IXITO (DT*ST-KIND NIL) FUNCTION 5 IXITO NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 IXITO NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 I NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 I NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "F_1" NIL I (IXITO I I IXITO) NIL NIL NIL NIL NIL 4 I (DT-ST*KIND SYS-FUNCT) FUNCTION 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "R.=" AXIOM 0 (19) (19 26) 1 (REFLEXIVITY) NIL NIL ((1) (2)) NIL T + 2 (19 19) (RED*UNFAIL NIL) (19) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL CLAUSE 5 IXITO NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 60 "A1" AXIOM 0 (7 9 8 6) (7 28 9 29 8 30 6 31) 2 NIL NIL NIL NIL NIL NIL + 3 (6 8 (21 6 9 8 7)) (INDEX 1 KIND (EQV T 80)) (7 9 8 6) (54 52 39) ATP.MEMORY.NOBIND (33) ATP.MEMORY.NOBIND NIL NIL (58) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T - 3 ((5 6 7) 8 9) (INDEX 3 KIND (EQV NIL 80)) (9 8 7 6) (50 40) ATP.MEMORY.NOBIND (33) ATP.MEMORY.NOBIND NIL NIL (62 60 42) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL CLAUSE 18 32 1 2 ((31 (5 6 7) 8 30 9 (21 (5 6 7) 29 30 28))) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL RIW 5 I NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 60 "A2" AXIOM 0 (24 23 22 25) (24 34 23 35 22 36 25 37) 2 NIL NIL NIL NIL NIL NIL + 3 (25 (21 22 23 24 25) 23) (INDEX 2 KIND (EQV T 80)) (24 23 22 25) (53 51 40) ATP.MEMORY.NOBIND (41) ATP.MEMORY.NOBIND NIL NIL (57) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T - 3 ((5 22 25) 24 23) (INDEX 3 KIND (EQV NIL 80)) (23 24 25 22) (49 39) ATP.MEMORY.NOBIND (41) ATP.MEMORY.NOBIND NIL NIL (61 59 42) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL CLAUSE 18 32 1 2 ((6 (5 22 25) 24 8 23 (21 (5 22 25) 9 8 7))) NIL 38 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 38 1 2 ((25 (5 6 7) 8 (21 22 9 24 (5 6 7)) 23 9)) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 38 1 2 ((37 (5 22 25) 24 (21 36 35 34 (5 22 25)) 23 35)) NIL 38 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL RIW 18 38 2 2 ((22 6 25 7 24 8 23 9)) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 I NIL NIL VARIABLE 5 IXITO NIL NIL VARIABLE 84 "A3" AXIOM 0 (20 18 16 17 10) (20 43 18 44 16 45 17 46 10 47) 3 NIL NIL NIL NIL NIL T - 3 (10 17 16) (INDEX 1 KIND (EQV T 80)) (16 17 10) (54 53) ATP.MEMORY.NOBIND (55) ATP.MEMORY.NOBIND NIL NIL (62 61) (63) (64) NIL NIL NIL NIL NIL NIL NIL NIL NIL T - 3 (18 16 20) (INDEX 2 KIND (EQV T 80)) (20 16 18) (52 51) ATP.MEMORY.NOBIND (56) ATP.MEMORY.NOBIND NIL NIL (60 59) (63) (64) NIL NIL NIL NIL NIL NIL NIL NIL NIL T + 3 ((5 10 18) 17 20) (INDEX 3 KIND (EQV NIL 80)) (20 17 18 10) (50 49) ATP.MEMORY.NOBIND (56 55) ATP.MEMORY.NOBIND NIL NIL (58 57) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL CLAUSE 18 48 3 2 ((10 22 18 25 17 24 20 23)) NIL 38 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 48 3 2 ((10 6 18 7 17 8 20 9)) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 38 1 2 ((18 25 16 (21 22 23 24 25) 20 23)) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 32 1 2 ((18 6 16 8 20 (21 6 9 8 7))) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 38 1 1 ((10 25 17 (21 22 23 24 25) 16 23)) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 32 1 1 ((10 6 17 8 16 (21 6 9 8 7))) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL R 18 48 3 1 ((10 (5 47 44) 46 17 43 16)) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL RIW 18 48 3 2 ((18 (5 47 44) 46 16 43 20)) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL RIW 18 48 3 1 ((25 (5 10 18) 17 (21 22 23 24 (5 10 18)) 20 23)) NIL 38 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 48 3 1 ((6 (5 10 18) 17 8 20 (21 (5 10 18) 9 8 7))) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 48 2 2 ((18 (5 22 25) 16 24 20 23)) NIL 38 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 48 2 2 ((18 (5 6 7) 16 8 20 9)) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 48 1 2 ((10 (5 22 25) 17 24 16 23)) NIL 38 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 48 1 2 ((10 (5 6 7) 17 8 16 9)) NIL 32 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 48 2 1 ((18 10 16 17 20 17)) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL SI 18 48 2 1 ((44 10 45 17 43 16)) NIL 48 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL SIW 5 IXITO "C_1" (DT*ST-KIND SYS-CONST) CONSTANT 5 IXITO "C_2" (DT*ST-KIND SYS-CONST) CONSTANT 5 I "C_3" (DT*ST-KIND SYS-CONST) CONSTANT 5 IXITO "C_4" (DT*ST-KIND SYS-CONST) CONSTANT 5 I "C_5" (DT*ST-KIND SYS-CONST) CONSTANT 5 IXITO "C_6" (DT*ST-KIND SYS-CONST) CONSTANT 5 IXITO "C_7" (DT*ST-KIND SYS-CONST) CONSTANT 5 I "C_8" (DT*ST-KIND SYS-CONST) CONSTANT 5 IXITO "C_9" (DT*ST-KIND SYS-CONST) CONSTANT 5 I "C_10" (DT*ST-KIND SYS-CONST) CONSTANT))) (INCREMENT (- MEM*SIZE 10000)) COUNTER1) (MEM-RESET) (COND ((> MEM*SIZE 10000) (SETQ COUNTER1 75) (MAPC (FUNCTION (LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) (COND ((EQL RADDR (QUOTE ATP.MEMORY.NIL)) RADDR) ((EQL (QUOTE END) RADDR) RADDR) ((OR (EQL RADDR 0) (MINUSP RADDR)) RADDR) (T (+ RADDR INCREMENT)))))) ADDRLIST) (SETQ COUNTER1 MEM*SIZE) (MAPC (FUNCTION (LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA))) DATALIST)) (T (UNLESS (= MEM*SIZE 10000) (MEM-INITIALIZE 10000)) (SETQ COUNTER1 75) (MAPC (FUNCTION (LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))) ADDRLIST) (SETQ COUNTER1 MEM*SIZE) (MAPC (FUNCTION (LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA))) DATALIST))) (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9088 INCREMENT) 9088)) (SETQ MEM*NEXT.VADR 75 MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1) MEM*FIRST.REUSABLE.VADR 65 MEM*LAST.REUSABLE.VADR 69)) (PROGN (SETQ DT*SORT.ALL (QUOTE (IXITO ITO I ANY))) (SETQ DT*SORT.NR (QUOTE 5)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*ELEMENT.PREDICATE (QUOTE NIL)) (SETQ DT*OMEGA.CONSTANT (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (12 13 14 11 15 74 73 72 71 70))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (21 5))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (4 3 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (4 3 2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) (PROGN) (PROGN (PROGN)) (PROGN (PROGN (SETF (GET (QUOTE IXITO) (QUOTE DT*SORT.NUMBER)) (QUOTE 4)) (SETF (GET (QUOTE IXITO) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE IXITO) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (IXITO))) (SETF (GET (QUOTE IXITO) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((IXITO IXITO) (ITO) (I) (ANY IXITO)))) (SETF (GET (QUOTE IXITO) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (IXITO))) (SETF (GET (QUOTE IXITO) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE IXITO) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (IXITO ANY)))) (PROGN (SETF (GET (QUOTE ITO) (QUOTE DT*SORT.NUMBER)) (QUOTE 3)) (SETF (GET (QUOTE ITO) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ITO) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ITO))) (SETF (GET (QUOTE ITO) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((IXITO) (ITO ITO) (I) (ANY ITO)))) (SETF (GET (QUOTE ITO) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ITO))) (SETF (GET (QUOTE ITO) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ITO) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ITO ANY)))) (PROGN (SETF (GET (QUOTE I) (QUOTE DT*SORT.NUMBER)) (QUOTE 2)) (SETF (GET (QUOTE I) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE I) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (I))) (SETF (GET (QUOTE I) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((IXITO) (ITO) (I I) (ANY I)))) (SETF (GET (QUOTE I) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (I))) (SETF (GET (QUOTE I) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE I) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (I ANY)))) (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (IXITO ITO I))) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (IXITO ITO I ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((IXITO IXITO) (ITO ITO) (I I) (ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ITO I))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY)))))) (PROGN (SETQ DS*RULES (QUOTE NIL)) (SETQ DS*FINITE.DOMAIN (QUOTE NIL))) (PROGN) (PROGN) (PROGN)) (PROGN (SETQ CG*RECOLOUR (QUOTE ((R . RD) (P . PD) (SI . SID) (RIW . RIWD)))) (SETQ CG*OBJECTCLASSES (QUOTE (CLAUSES R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))) (SETQ CG*SAVESTACK (QUOTE NIL)) (SETQ CG*GRAPH.REGISTER (QUOTE (PROGN (PROG ((ALL.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (INSERTED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (CHANGED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (REMOVED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0))))) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR ALL.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.ALL)) ALL.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR INSERTED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.INSERTED)) INSERTED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR CHANGED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.CHANGED)) CHANGED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR REMOVED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.REMOVED)) REMOVED.PROPLIST))))) (SETQ CG*CHANGE.QUEUE.LITERALS (QUOTE (NIL))) (PROG ((ALL.PROPLIST (QUOTE (CLAUSES ((48) 4 27 32 38 48) R ((54) 8 39 40 49 50 51 52 53 54) RIW ((56) 4 33 41 55 56) RD ((0) 0) S ((62) 7 42 57 58 59 60 61 62) SI ((63) 1 63) SIW ((64) 1 64) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (INSERTED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (CHANGED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (REMOVED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0))))) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR ALL.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.ALL)) ALL.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR INSERTED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.INSERTED)) INSERTED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR CHANGED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.CHANGED)) CHANGED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR REMOVED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.REMOVED)) REMOVED.PROPLIST)))) (PROGN (C "Operation save expression.") (SETQ OP*LINK.COLOURS.LITERAL.INITIAL (QUOTE (R S T RI SI TI RIW SIW TIW))) (SETQ OP*CLAUSECOUNTER (QUOTE 3)) (SETQ OP*COUNTER.RESOLVENTS (QUOTE 0)) (SETQ OP*COUNTER.PARAMODULANTS (QUOTE 0)) (SETQ OP*COUNTER.FACTORS (QUOTE 0)) (SETQ OP*COLOURS.CIRCLE.LINKS (QUOTE (SIW))))) (PROGN (SETQ CONS*CLAUSES (QUOTE (48))) (SETQ CONS*CLAUSECOUNTER (QUOTE NIL)) (SETQ CONS*LINK.COLOURS (QUOTE (R RIW S SI SIW T TI TIW P PIW))) (SETQ CONS*EQ.OCCURRENCES.NEG (QUOTE NIL)) (SETQ CONS*EQ.OCCURRENCES.POS (QUOTE NIL)) (SETQ CONS*RW.CLAUSES (QUOTE NIL)) (SETQ CONS*RW_EQ.OCCURRENCES.NEG (QUOTE NIL)) (SETQ CONS*RW_EQ.OCCURRENCES.POS (QUOTE NIL)) (SETQ CONS*NO.LINK.CLAUSES (QUOTE NIL)))))