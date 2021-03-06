;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* AXIOMS OF PROBLEM 46 %.) (* WE HAVE TWO MARRIED COUPLES ON THE ISLAND OF BAHAVA.) (* MR AND MRS ABNORMAL AND MR AND MRS BOLD.) (* MR ABNORMAL SAYS : MR BOLD IS A KNIGHT.) (* MRS ABNORMAL SAYS : MY HUSBAND IS RIGHT IS A KNIGHT.) (* MRS BOLD SAYS : THAT IS RIGHT. MY HUSBAND IS INDEED A KNIGHT.) (*) (* WHAT ARE THE 4 PEOPLE AND WHICH STATEMENTS ARE TRUE.) (*) (* KNIGHT2 (X Y) MEANS X SAYS : Y IS A KNIGHT.) (*) (ALL X KNIGHT (X) EQV NOT KNAVE (X) AND NOT NORMAL (X)) (ALL X NORMAL (X) EQV NOT KNAVE (X) AND NOT KNIGHT (X)) (ALL X KNAVE (X) EQV NOT NORMAL (X) AND NOT KNIGHT (X)) (ALL X,Y MARRIED (X Y) IMPL KNAVE (X) AND KNIGHT (Y) OR KNIGHT (X) AND KNAVE (Y) OR NORMAL (X) AND NORMAL (Y)) (ALL X,Y KNIGHT2 (X Y) AND KNIGHT (X) IMPL KNIGHT (Y)) (ALL X,Y KNIGHT2 (X Y) AND KNAVE (X) IMPL NOT KNIGHT (Y)) (MARRIED (MRBOLD MRSBOLD)) (MARRIED (MRABNORMAL MRSABNORMAL)) (KNIGHT2 (MRABNORMAL MRBOLD)) (KNIGHT2 (MRSABNORMAL MRBOLD)) (KNIGHT2 (MRSBOLD MRBOLD)))) (QUOTE (COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT (ALL 2 (EQV (+ 3 (2) (KIND (EQV T 130))) (AND (NOT (+ 4 (2) (KIND (EQV NIL 130)))) (NOT (+ 5 (2) (KIND (EQV NIL 130))))))) (ALL 6 (EQV (+ 5 (6) (KIND (EQV T 140))) (AND (NOT (+ 4 (6) (KIND (EQV NIL 140)))) (NOT (+ 3 (6) (KIND (EQV NIL 140))))))) (ALL 7 (EQV (+ 4 (7) (KIND (EQV T 150))) (AND (NOT (+ 5 (7) (KIND (EQV NIL 150)))) (NOT (+ 3 (7) (KIND (EQV NIL 150))))))) (ALL 9 (ALL 8 (IMPL (+ 10 (8 9) NIL) (OR (AND (+ 4 (8) NIL) (+ 3 (9) NIL)) (OR (AND (+ 3 (8) NIL) (+ 4 (9) NIL)) (AND (+ 5 (8) NIL) (+ 5 (9) NIL))))))) (ALL 12 (ALL 11 (IMPL (AND (+ 13 (11 12) NIL) (+ 3 (11) NIL)) (+ 3 (12) NIL)))) (ALL 15 (ALL 14 (IMPL (AND (+ 13 (14 15) NIL) (+ 4 (14) NIL)) (NOT (+ 3 (15) NIL))))) (+ 10 (16 17) NIL) (+ 10 (18 19) NIL) (+ 13 (18 16) NIL) (+ 13 (19 16) NIL) (+ 13 (17 16) NIL))) (QUOTE ((NORMAL (MRABNORMAL) AND NORMAL (MRSABNORMAL) AND NORMAL (MRBOLD) AND NORMAL (MRSBOLD)))) (QUOTE ((AND (+ 5 (18) NIL) (AND (+ 5 (19) NIL) (AND (+ 5 (16) NIL) (+ 5 (17) NIL)))))) (QUOTE ("Edit:     Axioms and Theorems edited: 15-DEC,1989 01:31 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9834 9839 9844 9849 9854 9859 9874 9879 9884 9899 9904 9909 9914 9919 9934 9949 9964
          9969 9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "KNIGHT" NIL NIL (ANY) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE)))
          ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "KNAVE" NIL NIL
          (ANY) NIL NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL))) ((- 4 (NIL))) NIL
          NIL (DT*ST-KIND NIL) PREDICATE 15 "NORMAL" NIL NIL (ANY) NIL NIL ((- 5 (POSITIVE)))
          ((+ 5 (NEGATIVE))) ((+ 5 (NIL))) ((- 5 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "MARRIED" NIL NIL
          (ANY ANY) NIL NIL ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL)))
          ((- 10 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "KNIGHT2" NIL NIL (ANY ANY) NIL NIL ((- 13 (POSITIVE)))
          ((+ 13 (NEGATIVE))) ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "MRBOLD" (DT*ST-KIND NIL) CONSTANT 5
          ANY "MRSBOLD" (DT*ST-KIND NIL) CONSTANT 5 ANY "MRABNORMAL" (DT*ST-KIND NIL) CONSTANT
          5 ANY "MRSABNORMAL" (DT*ST-KIND NIL) CONSTANT))
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
             (SETQ COUNTER1 20)
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
               (SETQ COUNTER1 20)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9829 INCREMENT) 9829))
      (SETQ MEM*NEXT.VADR 20
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (19 18 17 16))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE NIL)) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (13 10 5 4 3 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (13 10 5 4 3 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))