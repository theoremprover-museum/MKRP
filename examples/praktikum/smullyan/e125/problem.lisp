;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* SMULLYAN, CHAPTER 8, PROBLEM 125) (* ANOTHER TIME I WAS VISITING A DIFFERENT ISLAND OF KNIGHTS,) (* KNAVES, AND NORMALS.) (ALL X KNIGHT (X) IMPL NOT KNAVE (X)) (ALL X KNAVE (X) IMPL NOT NORMAL (X)) (ALL X NORMAL (X) IMPL NOT KNIGHT (X)) (ALL X,Y KNIGHT (X) AND ANSWER (X Y) IMPL T (Y)) (ALL X,Y KNAVE (X) AND ANSWER (X Y) IMPL NOT T (Y)) (ALL X KNIGHT (X) OR KNAVE (X) OR NORMAL (X)) (* IT WAS RUMORED THAT THERE WAS GOLD ON THE ISLAND, AND I) (* WANTED TO FIND OUT WHETHER THERE WAS. THE KING OF THE ISLAND,) (* WHO WAS A KNIGHT, GRACIOUSLY INTRODUCED ME TO THREE OF THE) (* NATIVES, A, B, C, AND TOLD ME THAT AT MOST ONE OF THEM WAS) (* NORMAL.) (((KNIGHT (A) OR KNAVE (A)) AND (KNIGHT (B) OR KNAVE (B))) OR ((KNIGHT (A) OR KNAVE (A)) AND (KNIGHT (C) OR KNAVE (C))) OR ((KNIGHT (B) OR KNAVE (B)) AND (KNIGHT (C) OR KNAVE (C)))) (* I WAS ALLOWED TO ASK TWO YES-NO QUESTIONS TO WHICHEVER ONES) (* I WISHED.) (T (YES1) EQV QUESTION1) (T (NO1) EQV NOT QUESTION1) (T (YES2-A) EQV QUESTION2-A) (T (NO2-A) EQV NOT QUESTION2-A) (T (YES2-B) EQV QUESTION2-B) (T (NO2-B) EQV NOT QUESTION2-B))) (QUOTE (COMMENT COMMENT COMMENT (ALL 2 (IMPL (+ 3 (2) NIL) (NOT (+ 4 (2) NIL)))) (ALL 5 (IMPL (+ 4 (5) NIL) (NOT (+ 6 (5) NIL)))) (ALL 7 (IMPL (+ 6 (7) NIL) (NOT (+ 3 (7) NIL)))) (ALL 9 (ALL 8 (IMPL (AND (+ 3 (8) NIL) (+ 10 (8 9) NIL)) (+ 11 (9) NIL)))) (ALL 13 (ALL 12 (IMPL (AND (+ 4 (12) NIL) (+ 10 (12 13) NIL)) (NOT (+ 11 (13) NIL))))) (ALL 14 (OR (+ 3 (14) NIL) (OR (+ 4 (14) NIL) (+ 6 (14) NIL)))) COMMENT COMMENT COMMENT COMMENT COMMENT (OR (AND (OR (+ 3 (15) NIL) (+ 4 (15) NIL)) (OR (+ 3 (16) NIL) (+ 4 (16) NIL))) (OR (AND (OR (+ 3 (15) NIL) (+ 4 (15) NIL)) (OR (+ 3 (17) NIL) (+ 4 (17) NIL))) (AND (OR (+ 3 (16) NIL) (+ 4 (16) NIL)) (OR (+ 3 (17) NIL) (+ 4 (17) NIL))))) COMMENT COMMENT (EQV (+ 11 (18) (KIND (EQV T 190))) (+ 19 NIL (KIND (EQV NIL 190)))) (EQV (+ 11 (20) (KIND (EQV T 200))) (NOT (+ 19 NIL (KIND (EQV NIL 200))))) (EQV (+ 11 (21) (KIND (EQV T 210))) (+ 22 NIL (KIND (EQV NIL 210)))) (EQV (+ 11 (23) (KIND (EQV T 220))) (NOT (+ 22 NIL (KIND (EQV NIL 220))))) (EQV (+ 11 (24) (KIND (EQV T 230))) (+ 25 NIL (KIND (EQV NIL 230)))) (EQV (+ 11 (26) (KIND (EQV T 240))) (NOT (+ 25 NIL (KIND (EQV NIL 240))))))) (QUOTE ((* IS THERE A WAY OF FINDING OUT IN TWO QUESTIONS WHETHER THERE) (* IS GOLD ON THE ISLAND?) ((QUESTION1 EQV (KNIGHT (A) EQV NORMAL (B))) IMPL ((QUESTION2-A EQV (KNIGHT (C) EQV THERE-IS-GOLD-ON-THE-ISLAND)) AND (QUESTION2-B EQV (KNIGHT (B) EQV THERE-IS-GOLD-ON-THE-ISLAND)) IMPL (ANSWER (A YES1) IMPL ((ANSWER (C YES2-A) IMPL THERE-IS-GOLD-ON-THE-ISLAND) AND (ANSWER (C NO2-A) IMPL NOT THERE-IS-GOLD-ON-THE-ISLAND))) AND (ANSWER (A NO1) IMPL ((ANSWER (B YES2-B) IMPL THERE-IS-GOLD-ON-THE-ISLAND) AND (ANSWER (B NO2-B) IMPL NOT THERE-IS-GOLD-ON-THE-ISLAND))))))) (QUOTE (COMMENT COMMENT (IMPL (EQV (+ 19 NIL (KIND (EQV T 41))) (EQV (+ 3 (15) (KIND (EQV T 40 EQV NIL 41))) (+ 6 (16) (KIND (EQV NIL 40 EQV NIL 41))))) (IMPL (AND (EQV (+ 22 NIL (KIND (EQV T 43))) (EQV (+ 3 (17) (KIND (EQV T 42 EQV NIL 43))) (+ 27 NIL (KIND (EQV NIL 42 EQV NIL 43))))) (EQV (+ 25 NIL (KIND (EQV T 45))) (EQV (+ 3 (16) (KIND (EQV T 44 EQV NIL 45))) (+ 27 NIL (KIND (EQV NIL 44 EQV NIL 45)))))) (AND (IMPL (+ 10 (15 18) NIL) (AND (IMPL (+ 10 (17 21) NIL) (+ 27 NIL NIL)) (IMPL (+ 10 (17 23) NIL) (NOT (+ 27 NIL NIL))))) (IMPL (+ 10 (15 20) NIL) (AND (IMPL (+ 10 (16 24) NIL) (+ 27 NIL NIL)) (IMPL (+ 10 (16 26) NIL) (NOT (+ 27 NIL NIL)))))))))) (QUOTE ("Edit:     Axioms and Theorems edited: 14-DEC,1989 18:57 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9764 9769 9784 9789 9794 9809 9814 9819 9834 9839 9844 9849 9854 9859 9864 9869 9884
          9899 9904 9909 9914 9929 9934 9949 9964 9969 9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "KNIGHT" NIL NIL (ANY) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE)))
          ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "KNAVE" NIL NIL
          (ANY) NIL NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL))) ((- 4 (NIL))) NIL
          NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          15 "NORMAL" NIL NIL (ANY) NIL NIL ((- 6 (POSITIVE))) ((+ 6 (NEGATIVE))) ((+ 6 (NIL)))
          ((- 6 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "ANSWER" NIL NIL
          (ANY ANY) NIL NIL ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL)))
          ((- 10 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "T" NIL NIL (ANY) NIL NIL
          ((- 11 (POSITIVE))) ((+ 11 (NEGATIVE))) ((+ 11 (NIL))) ((- 11 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "A" (DT*ST-KIND NIL) CONSTANT 5 ANY
          "B" (DT*ST-KIND NIL) CONSTANT 5 ANY "C" (DT*ST-KIND NIL) CONSTANT 5 ANY "YES1"
          (DT*ST-KIND NIL) CONSTANT 15 "QUESTION1" NIL NIL NIL NIL NIL ((- 19 (POSITIVE)))
          ((+ 19 (NEGATIVE))) ((+ 19 (NIL))) ((- 19 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 ANY "NO1" (DT*ST-KIND NIL) CONSTANT 5 ANY "YES2-A" (DT*ST-KIND NIL) CONSTANT 15
          "QUESTION2-A" NIL NIL NIL NIL NIL ((- 22 (POSITIVE))) ((+ 22 (NEGATIVE)))
          ((+ 22 (NIL))) ((- 22 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "NO2-A"
          (DT*ST-KIND NIL) CONSTANT 5 ANY "YES2-B" (DT*ST-KIND NIL) CONSTANT 15 "QUESTION2-B"
          NIL NIL NIL NIL NIL ((- 25 (POSITIVE))) ((+ 25 (NEGATIVE))) ((+ 25 (NIL)))
          ((- 25 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "NO2-B" (DT*ST-KIND NIL)
          CONSTANT 15 "THERE-IS-GOLD-ON-THE-ISLAND" NIL NIL NIL NIL NIL ((- 27 (POSITIVE)))
          ((+ 27 (NEGATIVE))) ((+ 27 (NIL))) ((- 27 (NIL))) NIL NIL (DT*ST-KIND NIL)
          PREDICATE))
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
             (SETQ COUNTER1 28)
             (MAPC #'(LAMBDA (RADDR)
                       (SETF (AREF MEM*MEMORY (DECF COUNTER1))
                              (COND ((EQL RADDR 'ATP.MEMORY.NIL) RADDR)
                                    ((EQL 'END RADDR) RADDR)
                                    ((OR (EQL RADDR 0)
                                         (MINUSP RADDR))
                                     RADDR)
                                    (T (+ RADDR INCREMENT)))))
                   ADDRLIST)
             (SETQ COUNTER1 MEM*SIZE)
             (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            (T (UNLESS (= MEM*SIZE 10000)
                 (MEM-INITIALIZE 10000))
               (SETQ COUNTER1 28)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9749 INCREMENT) 9749))
      (SETQ MEM*NEXT.VADR 28
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (26 24 23 21 20 18 17 16 15))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE NIL)) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (27 25 22 19 11 10 6 4 3 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (27 25 22 19 11 10 6 4 3 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))