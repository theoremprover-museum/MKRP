;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PROGN (PREP=CREATE.EMPTY.PARTIAL.GRAPH) (PROGN (SETQ TWO*RULES.MAXLEVEL (QUOTE 1)) (SETQ TWO*CONTROL (QUOTE NIL)) (SETQ TWO*SUPPRESS.NORULES (QUOTE NIL)) (SETQ TWO*DIALOG (QUOTE NIL)) (SETQ TWO*EXTERNAL.RULE.COLOURS (QUOTE (R S T))) (SETQ TWO*INTERNAL.RULE.COLOURS (QUOTE (RIW SI SIW TI TIW))) (SETQ TWO*RULES (QUOTE NIL)) (SETQ TWO*ADDED.RULES (QUOTE NIL)) (SETQ TWO*EMPTY.CLAUSE.FOUND (QUOTE NIL)) (SETQ TWO*NEW.RULES (QUOTE NIL)) (SETQ TWO*NORULE.CLAUSES (QUOTE NIL)) (SETQ TWO*TR.RULES (QUOTE NIL)) (SETQ TWO*TR.NORULES (QUOTE NIL))) (PROGN (C "Save expression of reduction.") (SETQ RED*RW_RULES (QUOTE NIL) RDS*RW_ASSIGN (QUOTE NIL) RDS*RW_VARIABLES (QUOTE ((ANY (22 23 24) (22 23 24)))) RED*RW_RULES.COMPLETION (QUOTE ((30 . 1) (26 . 1) (19 . 1))) RED*RW_RULES.UNFAILING (QUOTE ((15 . 1))) RED*GRAPH.ISOLATION (QUOTE T)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.TAUTOLOGY) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.REPL.RESOLUTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.SUBSUMPTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*LINK.SUBSUMPTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*LINK.TAUTOLOGY) (QUOTE NIL))) (PROGN (PROGN (PROGN (PROGN (DS-RESET) (PROGN (DT-RESET) (PROG ((ADDRLIST (QUOTE (19594 19612 19630 19666 19671 19689 19707 19743 19748 19753 19758 19763 19781 19799 19835 19840 19845 19850 19886 19891 19896 END 19589 19910 19915 19920 19925 19939 19944 19949 19954 19969 19984 19999))) (DATALIST (QUOTE (15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE))) ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" ((30 1) (26 1) (19 1) (15 1)) NIL (ANY ANY) (DEFINED SYMMETRIC REFLEXIVE) 15 ((- 2 (POSITIVE . SYMMETRIC))) ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "+" NIL ANY (ANY ANY) NIL NIL NIL NIL NIL 2 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "0" (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "-" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "R.=" AXIOM 0 (13) (13 14) 1 (REFLEXIVITY) NIL NIL ((1) (2)) NIL T + 2 (13 13) (RED*UNFAIL NIL) (13) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL CLAUSE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A1" AXIOM 0 (5 4 3) (5 16 4 17 3 18) 1 NIL NIL NIL NIL 1 T + 2 ((6 (6 3 4) 5) (6 3 (6 4 5))) (RED*RULE (19 NIL 0 (6 (6 22 23) 24) (6 22 (6 23 24)) NIL) INDEX 1) (5 4 3) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (31 27) (21 20) NIL NIL CLAUSE 18 19 1 1 ((18 3 17 4 16 5)) NIL 19 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 19 1 1 ((3 (6 18 17) 16 4)) NIL 19 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A2" AXIOM 0 (7) (7 25) 1 NIL NIL NIL NIL 1 T + 2 ((6 8 7) 7) (RED*RULE (26 NIL 0 (6 8 22) 22 NIL) INDEX 2) (7) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (27) (28) NIL NIL CLAUSE 18 26 1 1 ((3 8 7 4)) NIL 19 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 26 1 1 ((25 7)) NIL 26 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 36 "A3" AXIOM 0 (9) (9 29) 1 NIL NIL NIL NIL 1 T + 2 ((6 (10 9) 9) 8) (RED*RULE (30 NIL 0 (6 (10 22) 22) 8 NIL) INDEX 3) (9) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (31) (32) NIL NIL CLAUSE 18 30 1 1 ((3 (10 4) 9 4)) NIL 19 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 30 1 1 ((29 9)) NIL 30 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY "C_1" (DT*ST-KIND SYS-CONST) CONSTANT 5 ANY "C_2" (DT*ST-KIND SYS-CONST) CONSTANT))) (INCREMENT (- MEM*SIZE 20000)) COUNTER1) (MEM-RESET) (COND ((> MEM*SIZE 20000) (SETQ COUNTER1 34) (MAPC (FUNCTION (LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) (COND ((EQL RADDR (QUOTE ATP.MEMORY.NIL)) RADDR) ((EQL (QUOTE END) RADDR) RADDR) ((OR (EQL RADDR 0) (MINUSP RADDR)) RADDR) (T (+ RADDR INCREMENT)))))) ADDRLIST) (SETQ COUNTER1 MEM*SIZE) (MAPC (FUNCTION (LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA))) DATALIST)) (T (UNLESS (= MEM*SIZE 20000) (MEM-INITIALIZE 20000)) (SETQ COUNTER1 34) (MAPC (FUNCTION (LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))) ADDRLIST) (SETQ COUNTER1 MEM*SIZE) (MAPC (FUNCTION (LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA))) DATALIST))) (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 20000) (+ 19584 INCREMENT) 19584)) (SETQ MEM*NEXT.VADR 34 MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1) MEM*FIRST.REUSABLE.VADR 12 MEM*LAST.REUSABLE.VADR 12)) (PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*ELEMENT.PREDICATE (QUOTE NIL)) (SETQ DT*OMEGA.CONSTANT (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (11 33 8))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (10 6))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) (PROGN) (PROGN (PROGN)) (PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY)))))) (PROGN (SETQ DS*RULES (QUOTE NIL)) (SETQ DS*FINITE.DOMAIN (QUOTE NIL))) (PROGN) (PROGN) (PROGN)) (PROGN (SETQ CG*RECOLOUR (QUOTE ((R . RD) (P . PD) (SI . SID) (RIW . RIWD)))) (SETQ CG*OBJECTCLASSES (QUOTE (CLAUSES R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))) (SETQ CG*SAVESTACK (QUOTE NIL)) (SETQ CG*GRAPH.REGISTER (QUOTE (PROGN (PROG ((ALL.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (INSERTED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (CHANGED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (REMOVED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0))))) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR ALL.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.ALL)) ALL.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR INSERTED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.INSERTED)) INSERTED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR CHANGED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.CHANGED)) CHANGED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR REMOVED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.REMOVED)) REMOVED.PROPLIST))))) (SETQ CG*CHANGE.QUEUE.LITERALS (QUOTE (NIL))) (PROG ((ALL.PROPLIST (QUOTE (CLAUSES ((30) 4 15 19 26 30) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((31) 2 27 31) PIW ((32) 4 20 21 28 32) PD ((0) 0) RIWD ((0) 0)))) (INSERTED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (CHANGED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (REMOVED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0))))) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR ALL.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.ALL)) ALL.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR INSERTED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.INSERTED)) INSERTED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR CHANGED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.CHANGED)) CHANGED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR REMOVED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.REMOVED)) REMOVED.PROPLIST)))) (PROGN (C "Operation save expression.") (SETQ OP*LINK.COLOURS.LITERAL.INITIAL (QUOTE (R S T RI SI TI RIW SIW TIW))) (SETQ OP*CLAUSECOUNTER (QUOTE 3)) (SETQ OP*COUNTER.RESOLVENTS (QUOTE 0)) (SETQ OP*COUNTER.PARAMODULANTS (QUOTE 0)) (SETQ OP*COUNTER.FACTORS (QUOTE 0)) (SETQ OP*COLOURS.CIRCLE.LINKS (QUOTE (SIW))))) (PROGN (SETQ CONS*CLAUSES (QUOTE (30))) (SETQ CONS*CLAUSECOUNTER (QUOTE NIL)) (SETQ CONS*LINK.COLOURS (QUOTE (R RIW S SI SIW T TI TIW P PIW))) (SETQ CONS*EQ.OCCURRENCES.NEG (QUOTE NIL)) (SETQ CONS*EQ.OCCURRENCES.POS (QUOTE ((30 1) (26 1) (19 1)))) (SETQ CONS*RW.CLAUSES (QUOTE NIL)) (SETQ CONS*RW_EQ.OCCURRENCES.NEG (QUOTE NIL)) (SETQ CONS*RW_EQ.OCCURRENCES.POS (QUOTE NIL)) (SETQ CONS*NO.LINK.CLAUSES (QUOTE NIL)))))