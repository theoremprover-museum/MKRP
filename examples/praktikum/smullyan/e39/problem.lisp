;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* AXIOMS OF PROBLEM 39 %.) (* THERE ARE THREE PEOPLE : A,B,C.) (* ONE IS A NORMAL, ONE IS A KNIGHT, ONE IS A KNAVE.) (*) (ALL X KNIGHT (X) EQV NOT KNAVE (X) AND NOT NORMAL (X)) (ALL X KNAVE (X) EQV NOT KNIGHT (X) AND NOT NORMAL (X)) (ALL X NORMAL (X) EQV NOT KNAVE (X) AND NOT KNIGHT (X)) (*) (KNAVE (A) OR KNAVE (B) OR KNAVE (C)) (KNIGHT (A) OR KNIGHT (B) OR KNIGHT (C)) (NORMAL (A) OR NORMAL (B) OR NORMAL (C)) (*) (* NORMAL2 (X Y) MEANS : X SAYS Y IS NORMAL.) (* NNORMAL2 (X Y) MEANS : X SAYS Y IS NOT NORMAL.) (*) (ALL X,Y NORMAL2 (X Y) IMPL ((KNAVE (X) IMPL NOT NORMAL (Y)) AND (KNIGHT (X) IMPL NORMAL (Y)))) (ALL X,Y NNORMAL2 (X Y) IMPL ((KNAVE (X) IMPL NORMAL (Y)) AND (KNIGHT (X) IMPL NOT NORMAL (Y)))) (*) (* A SAYS : I AM NORMAL.) (* B SAYS : THAT IS TRUE.) (* C SAYS : I AM NOT NORMAL.) (*) (NORMAL2 (A A)) (NORMAL2 (B A)) (NNORMAL2 (C C)))) (QUOTE (COMMENT COMMENT COMMENT COMMENT (ALL 2 (EQV (+ 3 (2) (KIND (EQV T 60))) (AND (NOT (+ 4 (2) (KIND (EQV NIL 60)))) (NOT (+ 5 (2) (KIND (EQV NIL 60))))))) (ALL 6 (EQV (+ 4 (6) (KIND (EQV T 70))) (AND (NOT (+ 3 (6) (KIND (EQV NIL 70)))) (NOT (+ 5 (6) (KIND (EQV NIL 70))))))) (ALL 7 (EQV (+ 5 (7) (KIND (EQV T 80))) (AND (NOT (+ 4 (7) (KIND (EQV NIL 80)))) (NOT (+ 3 (7) (KIND (EQV NIL 80))))))) COMMENT (OR (+ 4 (8) NIL) (OR (+ 4 (9) NIL) (+ 4 (10) NIL))) (OR (+ 3 (8) NIL) (OR (+ 3 (9) NIL) (+ 3 (10) NIL))) (OR (+ 5 (8) NIL) (OR (+ 5 (9) NIL) (+ 5 (10) NIL))) COMMENT COMMENT COMMENT COMMENT (ALL 12 (ALL 11 (IMPL (+ 13 (11 12) NIL) (AND (IMPL (+ 4 (11) NIL) (NOT (+ 5 (12) NIL))) (IMPL (+ 3 (11) NIL) (+ 5 (12) NIL)))))) (ALL 15 (ALL 14 (IMPL (+ 16 (14 15) NIL) (AND (IMPL (+ 4 (14) NIL) (+ 5 (15) NIL)) (IMPL (+ 3 (14) NIL) (NOT (+ 5 (15) NIL))))))) COMMENT COMMENT COMMENT COMMENT COMMENT (+ 13 (8 8) NIL) (+ 13 (9 8) NIL) (+ 16 (10 10) NIL))) (QUOTE ((* THEOREM) (* WHAT ARE A,B,C ?) (KNAVE (A) AND KNIGHT (C) AND NORMAL (B)))) (QUOTE (COMMENT COMMENT (AND (+ 4 (8) NIL) (AND (+ 3 (10) NIL) (+ 5 (9) NIL))))) (QUOTE ("Edit:     Axioms and Theorems edited: 15-DEC,1989 14:03 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9859 9864 9869 9884 9889 9894 9899 9904 9909 9914 9919 9934 9949 9964 9969 9984 9999)
        )
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
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "A" (DT*ST-KIND NIL) CONSTANT 5 ANY
          "B" (DT*ST-KIND NIL) CONSTANT 5 ANY "C" (DT*ST-KIND NIL) CONSTANT 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "NORMAL2" NIL NIL (ANY ANY) NIL NIL ((- 13 (POSITIVE)))
          ((+ 13 (NEGATIVE))) ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "NNORMAL2" NIL NIL (ANY ANY) NIL NIL
          ((- 16 (POSITIVE))) ((+ 16 (NEGATIVE))) ((+ 16 (NIL))) ((- 16 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE))
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
             (SETQ COUNTER1 17)
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
               (SETQ COUNTER1 17)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9844 INCREMENT) 9844))
      (SETQ MEM*NEXT.VADR 17
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (10 9 8))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE NIL)) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (16 13 5 4 3 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (16 13 5 4 3 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))