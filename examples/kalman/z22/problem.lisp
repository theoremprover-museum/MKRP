;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((ALL X A (B (C (X))) = D (X)) (ALL X B (C (D (X))) = E (X)) (ALL X C (D (E (X))) = A (X)) (ALL X D (E (A (X))) = B (X)) (ALL X E (A (B (X))) = C (X)) (ALL X A (A1 (X)) = X) (ALL X A1 (A (X)) = X) (ALL X B (B1 (X)) = X) (ALL X B1 (B (X)) = X) (ALL X C (C1 (X)) = X) (ALL X C1 (C (X)) = X) (ALL X D (D1 (X)) = X) (ALL X D1 (D (X)) = X) (ALL X E (E1 (X)) = X) (ALL X E1 (E (X)) = X))) (QUOTE ((ALL 3 (+ 2 ((7 (5 (4 3))) (6 3)) NIL)) (ALL 8 (+ 2 ((5 (4 (6 8))) (9 8)) NIL)) (ALL 10 (+ 2 ((4 (6 (9 10))) (7 10)) NIL)) (ALL 11 (+ 2 ((6 (9 (7 11))) (5 11)) NIL)) (ALL 12 (+ 2 ((9 (7 (5 12))) (4 12)) NIL)) (ALL 13 (+ 2 ((7 (14 13)) 13) NIL)) (ALL 15 (+ 2 ((14 (7 15)) 15) NIL)) (ALL 16 (+ 2 ((5 (17 16)) 16) NIL)) (ALL 18 (+ 2 ((17 (5 18)) 18) NIL)) (ALL 19 (+ 2 ((4 (20 19)) 19) NIL)) (ALL 21 (+ 2 ((20 (4 21)) 21) NIL)) (ALL 22 (+ 2 ((6 (23 22)) 22) NIL)) (ALL 24 (+ 2 ((23 (6 24)) 24) NIL)) (ALL 25 (+ 2 ((9 (26 25)) 25) NIL)) (ALL 27 (+ 2 ((26 (9 27)) 27) NIL)))) (QUOTE (((ALL X D (X) = A (A (A (X)))) AND (ALL X D (X) = E (E (E (E (E (X))))))))) (QUOTE ((AND (ALL 28 (+ 2 ((6 28) (7 (7 (7 28)))) NIL)) (ALL 29 (+ 2 ((6 29) (9 (9 (9 (9 (9 29)))))) NIL))))) (QUOTE ("Edit:     Axioms and Theorems edited: 30-JUL,1990 14:06 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(29734 29739 29744 29758 29763 29768 29782 29787 29792 29806 29811 29816 29830 29835
          29840 29854 29859 29864 29869 29874 29888 29893 29907 29921 29935 29949 29954 29969
          29984 29999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" NIL NIL (ANY ANY)
          (DEFINED SYMMETRIC REFLEXIVE) NIL ((- 2 (POSITIVE . SYMMETRIC)))
          ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 14 "C" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 14
          "B" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 14 "D" NIL ANY
          (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 14 "A" NIL ANY (ANY) NIL
          NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "E" NIL ANY (ANY) NIL NIL NIL NIL NIL 1
          ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 14 "A1" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "B1" NIL ANY (ANY) NIL NIL NIL NIL NIL
          1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "C1" NIL ANY (ANY) NIL NIL
          NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 14 "D1" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "E1" NIL ANY (ANY) NIL NIL NIL NIL NIL
          1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 30000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 30000)
             (SETQ COUNTER1 30)
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
            (T (UNLESS (= MEM*SIZE 30000)
                 (MEM-INITIALIZE 30000))
               (SETQ COUNTER1 30)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 30000) (+ 29729 INCREMENT) 29729))
      (SETQ MEM*NEXT.VADR 30
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE NIL)) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (26 23 20 17 14 9 7 6 5 4))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))