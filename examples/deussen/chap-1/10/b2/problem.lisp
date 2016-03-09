;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((SORT ELEM,MENGE,VERKN : ANY) (TYPE APPLY (VERKN ELEM ELEM) : ELEM) (TYPE INVERS (VERKN ELEM) : ELEM) (ALL U,V : MENGE ALL X : ELEM ME (U V) AND EL (X U) IMPL EL (X V)) (ALL F : MENGE ALL PHI : VERKN ALL X,E : ELEM G (F PHI E) AND EL (X F) IMPL APPLY (PHI X E) = X) (ALL U : MENGE ALL PHI : VERKN ALL E : ELEM EINS (E PHI U) IMPL EL (E U)) (ALL F : MENGE ALL PHI : VERKN ALL X,E : ELEM G (F PHI E) AND EL (X F) IMPL EL (INVERS (PHI X) F)))) (QUOTE ((+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (ALL 5 (ALL 4 (ALL 6 (IMPL (AND (+ 7 (4 5) NIL) (+ 8 (6 4) NIL)) (+ 8 (6 5) NIL))))) (ALL 9 (ALL 10 (ALL 12 (ALL 11 (IMPL (AND (+ 13 (9 10 12) NIL) (+ 8 (11 9) NIL)) (+ 14 ((2 10 11 12) 11) NIL)))))) (ALL 15 (ALL 16 (ALL 17 (IMPL (+ 18 (17 16 15) NIL) (+ 8 (17 15) NIL))))) (ALL 19 (ALL 20 (ALL 22 (ALL 21 (IMPL (AND (+ 13 (19 20 22) NIL) (+ 8 (21 19) NIL)) (+ 8 ((3 20 21) 19) NIL)))))))) (QUOTE ((ALL U,F : MENGE ALL PHI : VERKN ALL Z,E : ELEM G (F PHI E) AND ME (U F) IMPL ALL X,Y : ELEM EL (X U) AND EL (Y U) IMPL EL (APPLY (PHI INVERS (PHI Y) X) U) AND EL (Z U) AND EINS (E PHI U) IMPL EL (INVERS (PHI Z) U)))) (QUOTE ((ALL 24 (ALL 23 (ALL 25 (ALL 27 (ALL 26 (IMPL (AND (+ 13 (24 25 27) NIL) (+ 7 (23 24) NIL)) (ALL 29 (ALL 28 (IMPL (AND (+ 8 (28 23) NIL) (+ 8 (29 23) NIL)) (IMPL (AND (+ 8 ((2 25 (3 25 29) 28) 23) NIL) (AND (+ 8 (26 23) NIL) (+ 18 (27 25 23) NIL))) (+ 8 ((3 25 26) 23) NIL))))))))))))) (QUOTE ("Edit:     Axioms and Theorems edited: 13-DEC,1989 19:55 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(299766 299771 299776 299781 299786 299791 299796 299801 299806 299811 299816 299831
          299836 299841 299846 299861 299876 299881 299886 299891 299896 299911 299926 299931
          299936 299941 299955 299969 299984 299999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 14 "APPLY" NIL ELEM (VERKN ELEM ELEM) NIL NIL NIL
          NIL NIL 3 ELEM (DT*ST-KIND NIL) FUNCTION 14 "INVERS" NIL ELEM (VERKN ELEM) NIL NIL
          NIL NIL NIL 2 ELEM (DT*ST-KIND NIL) FUNCTION 5 MENGE NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 MENGE NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "ME" NIL NIL (ANY ANY) NIL NIL ((- 7 (POSITIVE))) ((+ 7 (NEGATIVE)))
          ((+ 7 (NIL))) ((- 7 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "EL" NIL NIL
          (ANY ANY) NIL NIL ((- 8 (POSITIVE))) ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 MENGE NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 VERKN NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "G" NIL NIL (ANY ANY ANY) NIL NIL ((- 13 (POSITIVE))) ((+ 13 (NEGATIVE)))
          ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" NIL NIL
          (ANY ANY) (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) NIL
          ((- 14 (POSITIVE . SYMMETRIC))) ((+ 14 (NEGATIVE . SYMMETRIC)))
          ((+ 14 (NIL . SYMMETRIC))) ((- 14 (NIL . SYMMETRIC))) NIL NIL (DT*ST-KIND NIL)
          PREDICATE 5 MENGE NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 VERKN NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "EINS" NIL NIL (ANY ANY ANY) NIL NIL ((- 18 (POSITIVE)))
          ((+ 18 (NEGATIVE))) ((+ 18 (NIL))) ((- 18 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 MENGE NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 VERKN NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 MENGE NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 MENGE NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 VERKN NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEM NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE))
       (INCREMENT (- MEM*SIZE 300000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 300000)
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
            (T (UNLESS (= MEM*SIZE 300000)
                 (MEM-INITIALIZE 300000))
               (SETQ COUNTER1 30)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 300000) (+ 299761 INCREMENT) 299761))
      (SETQ MEM*NEXT.VADR 30
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (VERKN MENGE ELEM ANY))) (SETQ DT*SORT.NR (QUOTE 5)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE NIL)) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (3 2))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (14))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (18 13 8 7 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (18 14 13 8 7 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE VERKN) (QUOTE DT*SORT.NUMBER)) (QUOTE 4)) (SETF (GET (QUOTE VERKN) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE VERKN) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (VERKN))) (SETF (GET (QUOTE VERKN) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((VERKN VERKN) (MENGE) (ELEM) (ANY VERKN)))) (SETF (GET (QUOTE VERKN) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (VERKN))) (SETF (GET (QUOTE VERKN) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE VERKN) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY VERKN)))) (PROGN (SETF (GET (QUOTE MENGE) (QUOTE DT*SORT.NUMBER)) (QUOTE 3)) (SETF (GET (QUOTE MENGE) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE MENGE) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (MENGE))) (SETF (GET (QUOTE MENGE) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((VERKN) (MENGE MENGE) (ELEM) (ANY MENGE)))) (SETF (GET (QUOTE MENGE) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (MENGE))) (SETF (GET (QUOTE MENGE) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE MENGE) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY MENGE)))) (PROGN (SETF (GET (QUOTE ELEM) (QUOTE DT*SORT.NUMBER)) (QUOTE 2)) (SETF (GET (QUOTE ELEM) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ELEM) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ELEM))) (SETF (GET (QUOTE ELEM) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((VERKN) (MENGE) (ELEM ELEM) (ANY ELEM)))) (SETF (GET (QUOTE ELEM) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ELEM))) (SETF (GET (QUOTE ELEM) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ELEM) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY ELEM)))) (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (VERKN MENGE ELEM))) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY VERKN MENGE ELEM))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((VERKN VERKN) (MENGE MENGE) (ELEM ELEM) (ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (MENGE ELEM))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))