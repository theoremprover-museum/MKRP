;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* AXIOMS *) (EX X F (X) AND NOT (G (X))) (ALL X F (X) IMPL H (X)) (ALL X J (X) AND I (X) IMPL F (X)) ((EX X H (X) AND NOT (G (X))) IMPL (ALL X I (X) IMPL NOT (H (X)))))) (QUOTE (COMMENT (EX 2 (AND (+ 3 (2) NIL) (NOT (+ 4 (2) NIL)))) (ALL 5 (IMPL (+ 3 (5) NIL) (+ 6 (5) NIL))) (ALL 7 (IMPL (AND (+ 8 (7) NIL) (+ 9 (7) NIL)) (+ 3 (7) NIL))) (IMPL (EX 10 (AND (+ 6 (10) NIL) (NOT (+ 4 (10) NIL)))) (ALL 11 (IMPL (+ 9 (11) NIL) (NOT (+ 6 (11) NIL))))))) (QUOTE ((* THEOREMS *) (ALL X J (X) IMPL NOT (I (X))))) (QUOTE (COMMENT (ALL 12 (IMPL (+ 8 (12) NIL) (NOT (+ 9 (12) NIL)))))) (QUOTE ("Edit:     Axioms and Theorems edited: 31-OCT,1989 22:51 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST '(9869 9874 9879 9894 9909 9914 9929 9934 9949 9964 9969 9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "F" NIL NIL (ANY) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE)))
          ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "G" NIL NIL (ANY)
          NIL NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL))) ((- 4 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15
          "H" NIL NIL (ANY) NIL NIL ((- 6 (POSITIVE))) ((+ 6 (NEGATIVE))) ((+ 6 (NIL)))
          ((- 6 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "J" NIL NIL (ANY) NIL NIL
          ((- 8 (POSITIVE))) ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "I" NIL NIL (ANY) NIL NIL ((- 9 (POSITIVE)))
          ((+ 9 (NEGATIVE))) ((+ 9 (NIL))) ((- 9 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE))
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
             (SETQ COUNTER1 13)
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
               (SETQ COUNTER1 13)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9864 INCREMENT) 9864))
      (SETQ MEM*NEXT.VADR 13
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE NIL)) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE NIL)) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (9 8 6 4 3 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (9 8 6 4 3 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))