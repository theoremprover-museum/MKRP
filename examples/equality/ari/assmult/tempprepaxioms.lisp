;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PROGN (PREP=CREATE.EMPTY.PARTIAL.GRAPH) (PROGN (SETQ TWO*RULES.MAXLEVEL (QUOTE 1)) (SETQ TWO*CONTROL (QUOTE NIL)) (SETQ TWO*SUPPRESS.NORULES (QUOTE NIL)) (SETQ TWO*DIALOG (QUOTE NIL)) (SETQ TWO*EXTERNAL.RULE.COLOURS (QUOTE (R S T))) (SETQ TWO*INTERNAL.RULE.COLOURS (QUOTE (RIW SI SIW TI TIW))) (SETQ TWO*RULES (QUOTE NIL)) (SETQ TWO*ADDED.RULES (QUOTE NIL)) (SETQ TWO*EMPTY.CLAUSE.FOUND (QUOTE NIL)) (SETQ TWO*NEW.RULES (QUOTE NIL)) (SETQ TWO*NORULE.CLAUSES (QUOTE NIL)) (SETQ TWO*TR.RULES (QUOTE NIL)) (SETQ TWO*TR.NORULES (QUOTE NIL))) (PROGN (C "Save expression of reduction.") (SETQ RED*RW_RULES (QUOTE NIL) RDS*RW_ASSIGN (QUOTE NIL) RDS*RW_VARIABLES (QUOTE ((ANY (38 43 83) (38 43 83)))) RED*RW_RULES.COMPLETION (QUOTE ((121 . 1) (103 . 1) (70 . 1) (49 . 1) (45 . 1) (41 . 1) (36 . 1))) RED*RW_RULES.UNFAILING (QUOTE ((86 . 1) (53 . 1) (34 . 1))) RED*GRAPH.ISOLATION (QUOTE T)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.TAUTOLOGY) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.REPL.RESOLUTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*CLAUSE.SUBSUMPTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*LINK.SUBSUMPTION.SUBJECT) (QUOTE NIL)) (RDS-RULE.PUT (QUOTE RECHECK.INFO) (QUOTE RED*LINK.TAUTOLOGY) (QUOTE NIL))) (PROGN (PROGN (PROGN (PROGN (DS-RESET) (PROGN (DT-RESET) (PROG ((ADDRLIST (QUOTE (38088 38103 38126 38144 38162 38180 38198 38216 38234 38252 38270 38288 38306 38324 38360 38365 38370 38375 38393 38411 38429 38447 38465 38483 38501 38519 38537 38555 38573 38591 38609 38627 38663 38668 38673 38678 38696 38714 38732 38750 38768 38786 38804 38822 38840 38858 38876 38894 38912 38948 38953 38958 38963 38981 38999 39017 39035 39053 39071 39089 39107 39125 39143 39161 39179 39215 39220 39225 39230 39248 39266 39284 39302 39320 39338 39356 39374 39392 39410 39428 39446 39464 39500 39505 39510 39528 39564 39569 39574 39592 39628 39633 39638 39656 39692 39697 39702 39707 39725 39761 39766 39802 39807 39812 END -31 -30 38093 38098 38108 39817 39822 39827 39832 39837 39842 39847 39852 39857 39862 39867 39872 39877 39882 39887 39901 39906 39920 39925 39930 39944 39949 39954 39969 39984 39999))) (DATALIST (QUOTE (15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE))) ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" ((121 1) (103 1) (86 1) (70 1) (53 1) (49 1) (45 1) (41 1) (36 1) (34 1)) NIL (ANY ANY) (DEFINED SYMMETRIC REFLEXIVE) 34 ((- 2 (POSITIVE . SYMMETRIC))) ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "0" (DT*ST-KIND NIL) CONSTANT 14 "+" NIL ANY (ANY ANY) NIL NIL NIL NIL NIL 2 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "S" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "*" NIL ANY (ANY ANY) NIL NIL NIL NIL NIL 2 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "R.=" AXIOM 0 (32) (32 33) 1 (REFLEXIVITY) NIL NIL ((1) (2)) NIL T + 2 (32 32) (RED*UNFAIL NIL) (32) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL (87 54) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL CLAUSE 5 ANY NIL NIL VARIABLE 36 "A1" AXIOM 0 (3) (3 35) 1 NIL NIL NIL NIL 1 T + 2 ((5 4 3) 3) (RED*RULE (36 NIL 0 (5 4 38) 38 NIL) INDEX 1) (3) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (132 116 80 62 60 56 55) (37) NIL NIL CLAUSE 18 36 1 1 ((35 3)) NIL 36 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A2" AXIOM 0 (7 6) (7 39 6 40) 1 NIL NIL NIL NIL 1 T + 2 ((5 (8 6) 7) (8 (5 6 7))) (RED*RULE (41 NIL 0 (5 (8 38) 43) (8 (5 38 43)) NIL) INDEX 2) (7 6) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (131 115 79 61 59 58 57) (42) NIL NIL CLAUSE 18 41 1 1 ((40 6 39 7)) NIL 41 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A3" AXIOM 0 (9) (9 44) 1 NIL NIL NIL NIL 1 T + 2 ((10 4 9) 4) (RED*RULE (45 NIL 0 (10 4 38) 4 NIL) INDEX 3) (9) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (111 104 95 93 89 88) (46) NIL NIL CLAUSE 18 45 1 1 ((44 9)) NIL 45 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A4" AXIOM 0 (12 11) (12 47 11 48) 1 NIL NIL NIL NIL 1 T + 2 ((10 (8 11) 12) (5 12 (10 11 12))) (RED*RULE (49 NIL 0 (10 (8 38) 43) (5 43 (10 38 43)) NIL) INDEX 4) (12 11) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (110 105 94 92 91 90) (50) NIL NIL CLAUSE 18 49 1 1 ((48 11 47 12)) NIL 49 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A5" AXIOM 0 (14 13) (14 51 13 52) 1 NIL NIL (1) ((1)) NIL T + 2 ((5 13 14) (5 14 13)) (RED*UNFAIL ((53 NIL 0 (5 38 43) (5 43 38) NIL)) INDEX 5) (14 13) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL (72 71 54) NIL NIL NIL NIL NIL NIL (130 129 114 113 78 77 76 75 74 73 62 61 60 59 58 57 56 55) (66 65 64 63) NIL NIL CLAUSE 18 53 1 1 ((32 (5 14 14) 13 14)) SYMMETRIC 34 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 53 1 1 ((13 4 14 3)) NIL 36 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((14 4 13 3)) NIL 36 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((13 (8 6) 14 7)) NIL 41 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((14 (8 6) 13 7)) NIL 41 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 41 1 1 ((13 (8 6) 7 14)) NIL 53 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 36 1 1 ((13 4 3 14)) NIL 53 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 41 1 1 ((14 (8 6) 7 13)) NIL 53 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 36 1 1 ((14 4 3 13)) NIL 53 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((52 13 51 14)) NIL 53 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 53 1 1 ((51 13 52 14)) NIL 53 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 53 1 1 ((52 14 51 13)) NIL 53 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 53 1 1 ((51 14 52 13)) NIL 53 (2) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A6" AXIOM 0 (17 16 15) (17 67 16 68 15 69) 1 NIL NIL NIL NIL 1 T + 2 ((5 (5 15 16) 17) (5 15 (5 16 17))) (RED*RULE (70 NIL 0 (5 (5 38 43) 83) (5 38 (5 43 83)) NIL) INDEX 6) (17 16 15) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL (72 71) NIL NIL NIL NIL NIL NIL (128 112 80 79 78 77 76 75 74 73) (82 81) NIL NIL CLAUSE 18 70 1 1 ((13 (5 14 14) 17 14 15 14 16 14)) SYMMETRIC 53 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 70 1 1 ((15 17 14 (5 17 17) 13 17 16 17)) SYMMETRIC 53 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 70 1 1 ((13 (5 15 16) 17 14)) NIL 53 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 70 1 1 ((14 (5 15 16) 17 13)) NIL 53 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((13 (5 15 16) 14 17)) NIL 70 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((14 (5 15 16) 13 17)) NIL 70 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((13 15 14 16)) NIL 70 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((14 15 13 16)) NIL 70 (2) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 41 1 1 ((15 (8 6) 7 16)) NIL 70 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 36 1 1 ((15 4 3 16)) NIL 70 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 70 1 1 ((69 15 68 16 67 17)) NIL 70 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 70 1 1 ((15 (5 69 68) 67 16)) NIL 70 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A7" AXIOM 0 (19 18) (19 84 18 85) 1 NIL NIL (1) ((1)) NIL T + 2 ((10 18 19) (10 19 18)) (RED*UNFAIL ((86 NIL 0 (10 38 43) (10 43 38) NIL)) INDEX 7) (19 18) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL (87) NIL NIL NIL NIL NIL NIL (127 126 123 122 109 108 107 106 95 94 93 92 91 90 89 88) (99 98 97 96) NIL NIL CLAUSE 18 86 1 1 ((32 (10 19 19) 18 19)) SYMMETRIC 34 NIL NIL NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL S 18 86 1 1 ((18 4 19 9)) NIL 45 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((19 4 18 9)) NIL 45 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((18 (8 11) 19 12)) NIL 49 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((19 (8 11) 18 12)) NIL 49 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 49 1 1 ((18 (8 11) 12 19)) NIL 86 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 45 1 1 ((18 4 9 19)) NIL 86 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 49 1 1 ((19 (8 11) 12 18)) NIL 86 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 45 1 1 ((19 4 9 18)) NIL 86 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((85 18 84 19)) NIL 86 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 86 1 1 ((84 18 85 19)) NIL 86 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 86 1 1 ((85 19 84 18)) NIL 86 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 18 86 1 1 ((84 19 85 18)) NIL 86 (2) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A8" AXIOM 0 (22 21 20) (22 100 21 101 20 102) 1 NIL NIL NIL NIL 1 T + 2 ((10 20 (5 21 22)) (5 (10 20 21) (10 20 22))) (RED*RULE (103 NIL 0 (10 38 (5 43 83)) (5 (10 38 43) (10 38 83)) NIL) INDEX 8) (22 21 20) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (125 124 116 115 114 113 112 111 110 109 108 107 106 105 104) (117) NIL NIL CLAUSE 18 103 1 1 ((20 4 9 (5 21 22))) NIL 45 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 103 1 1 ((20 (8 11) 12 (5 21 22))) NIL 49 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 103 1 1 ((20 18 19 (5 21 22))) NIL 86 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 103 1 1 ((20 19 18 (5 21 22))) NIL 86 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((18 20 19 (5 21 22))) NIL 103 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((19 20 18 (5 21 22))) NIL 103 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 49 1 1 ((20 (8 11) 12 (5 21 22))) NIL 103 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 45 1 1 ((20 4 9 (5 21 22))) NIL 103 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 70 1 1 ((21 (5 15 16) 17 22)) NIL 103 (1) (1 2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((13 21 14 22)) NIL 103 (1) (1 2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((14 21 13 22)) NIL 103 (2) (1 2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 41 1 1 ((21 (8 6) 7 22)) NIL 103 (1) (1 2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 36 1 1 ((21 4 3 22)) NIL 103 (1) (1 2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 103 1 1 ((102 20 101 21 100 22)) NIL 103 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 5 ANY NIL NIL VARIABLE 36 "A9" AXIOM 0 (23 25 24) (23 118 25 119 24 120) 1 NIL NIL NIL NIL 1 T + 2 ((10 (5 24 25) 23) (5 (10 24 23) (10 25 23))) (RED*RULE (121 NIL 0 (10 (5 38 43) 83) (5 (10 38 83) (10 43 83)) NIL) INDEX 9) (23 25 24) NIL ATP.MEMORY.NOBIND NIL ATP.MEMORY.NOBIND NIL NIL NIL NIL NIL NIL NIL NIL NIL (132 131 130 129 128 127 126 125 124 123 122) (133) NIL NIL CLAUSE 18 121 1 1 ((18 (5 24 25) 23 19)) NIL 86 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 121 1 1 ((19 (5 24 25) 23 18)) NIL 86 (1) (2) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 121 1 1 ((20 (5 24 25) 23 (5 21 22))) NIL 103 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 103 1 1 ((20 (5 24 25) 23 (5 21 22))) NIL 121 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((18 (5 24 25) 19 23)) NIL 121 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 86 1 1 ((19 (5 24 25) 18 23)) NIL 121 (2) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 70 1 1 ((24 (5 15 16) 17 25)) NIL 121 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((13 24 14 25)) NIL 121 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 53 1 1 ((14 24 13 25)) NIL 121 (2) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 41 1 1 ((24 (8 6) 7 25)) NIL 121 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 36 1 1 ((24 4 3 25)) NIL 121 (1) (1 1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL P 18 121 1 1 ((120 24 119 25 118 23)) NIL 121 (1) (1) NIL NIL NIL NIL NIL ATP.MEMORY.NOBIND ATP.MEMORY.NOBIND NIL PIW 5 ANY "C_1" (DT*ST-KIND SYS-CONST) CONSTANT 5 ANY "C_2" (DT*ST-KIND SYS-CONST) CONSTANT 5 ANY "C_3" (DT*ST-KIND SYS-CONST) CONSTANT 5 ANY "C_4" (DT*ST-KIND SYS-CONST) CONSTANT 5 ANY "C_5" (DT*ST-KIND SYS-CONST) CONSTANT))) (INCREMENT (- MEM*SIZE 40000)) COUNTER1) (MEM-RESET) (COND ((> MEM*SIZE 40000) (SETQ COUNTER1 136) (MAPC (FUNCTION (LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) (COND ((EQL RADDR (QUOTE ATP.MEMORY.NIL)) RADDR) ((EQL (QUOTE END) RADDR) RADDR) ((OR (EQL RADDR 0) (MINUSP RADDR)) RADDR) (T (+ RADDR INCREMENT)))))) ADDRLIST) (SETQ COUNTER1 MEM*SIZE) (MAPC (FUNCTION (LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA))) DATALIST)) (T (UNLESS (= MEM*SIZE 40000) (MEM-INITIALIZE 40000)) (SETQ COUNTER1 136) (MAPC (FUNCTION (LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))) ADDRLIST) (SETQ COUNTER1 MEM*SIZE) (MAPC (FUNCTION (LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA))) DATALIST))) (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 40000) (+ 38083 INCREMENT) 38083)) (SETQ MEM*NEXT.VADR 136 MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1) MEM*FIRST.REUSABLE.VADR 29 MEM*LAST.REUSABLE.VADR 31)) (PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*ELEMENT.PREDICATE (QUOTE NIL)) (SETQ DT*OMEGA.CONSTANT (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (135 28 27 134 26 4))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (10 8 5))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) (PROGN) (PROGN (PROGN)) (PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY)))))) (PROGN (SETQ DS*RULES (QUOTE NIL)) (SETQ DS*FINITE.DOMAIN (QUOTE NIL))) (PROGN) (PROGN) (PROGN)) (PROGN (SETQ CG*RECOLOUR (QUOTE ((R . RD) (P . PD) (SI . SID) (RIW . RIWD)))) (SETQ CG*OBJECTCLASSES (QUOTE (CLAUSES R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))) (SETQ CG*SAVESTACK (QUOTE NIL)) (SETQ CG*GRAPH.REGISTER (QUOTE (PROGN (PROG ((ALL.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (INSERTED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (CHANGED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (REMOVED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0))))) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR ALL.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.ALL)) ALL.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR INSERTED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.INSERTED)) INSERTED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR CHANGED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.CHANGED)) CHANGED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR REMOVED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.REMOVED)) REMOVED.PROPLIST))))) (SETQ CG*CHANGE.QUEUE.LITERALS (QUOTE (NIL))) (PROG ((ALL.PROPLIST (QUOTE (CLAUSES ((121) 10 34 36 41 45 49 53 70 86 103 121) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((87) 4 54 71 72 87) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((132) 48 55 56 57 58 59 60 61 62 73 74 75 76 77 78 79 80 88 89 90 91 92 93 94 95 104 105 106 107 108 109 110 111 112 113 114 115 116 122 123 124 125 126 127 128 129 130 131 132) PIW ((133) 16 37 42 46 50 63 64 65 66 81 82 96 97 98 99 117 133) PD ((0) 0) RIWD ((0) 0)))) (INSERTED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (CHANGED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0)))) (REMOVED.PROPLIST (QUOTE (CLAUSES ((0) 0) R ((0) 0) RIW ((0) 0) RD ((0) 0) S ((0) 0) SI ((0) 0) SIW ((0) 0) SID ((0) 0) T ((0) 0) TI ((0) 0) TIW ((0) 0) P ((0) 0) PIW ((0) 0) PD ((0) 0) RIWD ((0) 0))))) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR ALL.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.ALL)) ALL.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR INSERTED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.INSERTED)) INSERTED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR CHANGED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.CHANGED)) CHANGED.PROPLIST) (SMAPC (FUNCTION (LAMBDA (OBJECTLIST) (RPLACA OBJECTLIST (LAST OBJECTLIST)))) (FUNCTION CDDR) (CDR REMOVED.PROPLIST)) (SETF (SYMBOL-PLIST (QUOTE CG*OBJECTS.REMOVED)) REMOVED.PROPLIST)))) (PROGN (C "Operation save expression.") (SETQ OP*LINK.COLOURS.LITERAL.INITIAL (QUOTE (R S T RI SI TI RIW SIW TIW))) (SETQ OP*CLAUSECOUNTER (QUOTE 9)) (SETQ OP*COUNTER.RESOLVENTS (QUOTE 0)) (SETQ OP*COUNTER.PARAMODULANTS (QUOTE 0)) (SETQ OP*COUNTER.FACTORS (QUOTE 0)) (SETQ OP*COLOURS.CIRCLE.LINKS (QUOTE (SIW))))) (PROGN (SETQ CONS*CLAUSES (QUOTE (121))) (SETQ CONS*CLAUSECOUNTER (QUOTE NIL)) (SETQ CONS*LINK.COLOURS (QUOTE (R RIW S SI SIW T TI TIW P PIW))) (SETQ CONS*EQ.OCCURRENCES.NEG (QUOTE NIL)) (SETQ CONS*EQ.OCCURRENCES.POS (QUOTE ((121 1) (103 1) (86 1) (70 1) (53 1) (49 1) (45 1) (41 1) (36 1)))) (SETQ CONS*RW.CLAUSES (QUOTE NIL)) (SETQ CONS*RW_EQ.OCCURRENCES.NEG (QUOTE NIL)) (SETQ CONS*RW_EQ.OCCURRENCES.POS (QUOTE NIL)) (SETQ CONS*NO.LINK.CLAUSES (QUOTE NIL)))))