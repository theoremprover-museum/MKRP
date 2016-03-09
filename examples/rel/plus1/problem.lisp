;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION '(((ALL X,Y
    (NOT (ALL Z,U,V
          (NOT (P (X X X X X Z U) AND P (X U V V V V V) OR Q
                (Y U U U U U U U U U U U U U U U U U U U U U U U U U UU U U U U U)))))))) '((ALL 5
   (ALL 6
    (NOT (ALL 2
          (ALL 3
           (ALL 4
            (NOT (OR (AND (+ 10 (6 6 6 6 6 4 3) NIL)
                          (+ 10 (6 3 2 2 2 2 2) NIL))
                     (+ 12
                        (5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 11 3 3 3 3 3)
                        NIL)))))))))) 'NIL 'NIL '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 25-JUL,1989 14:41 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9924 9929 9944 ATP.MEMORY.NIL ATP.MEMORY.NIL ATP.MEMORY.NIL 9969 9964 9959 9954 9949
          9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "P" NIL NIL
          (ANY ANY ANY ANY ANY ANY ANY) NIL NIL ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE)))
          ((+ 10 (NIL))) ((- 10 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "UU"
          (DT*ST-KIND NIL) CONSTANT 15 "Q" NIL NIL
          (ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY
           ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY ANY)
          NIL NIL ((- 12 (POSITIVE))) ((+ 12 (NEGATIVE))) ((+ 12 (NIL))) ((- 12 (NIL))) NIL
          NIL (DT*ST-KIND NIL) PREDICATE))
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
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9909 INCREMENT) 9909))
      (SETQ MEM*NEXT.VADR 13
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL '(ANY))
       (SETQ DT*SORT.NR '2)
       (SETQ DT*SORT.PROPERTIES
              '(DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE
                DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS
                DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))
       (SETQ DT*SORT.COMMON.COMPUTE.FLAG 'NIL)
       (SETQ DT*VARIABLE.COUNTER '0)
       (SETQ DT*CONSTANT.COUNTER '0)
       (SETQ DT*CONSTANT.ALL '(11))
       (SETQ DT*ABBREVIATIONS 'NIL)
       (SETQ DT*FUNCTION.COUNTER '0)
       (SETQ DT*FUNCTION.ALL 'NIL)
       (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES '(ASSOCIATIVE))
       (SETQ DT*FUNCTION.ACTUAL.THEORIES 'NIL)
       (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES 'NIL)
       (SETQ DT*FUNCTION.COMPONENTS
              '(PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST
                SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))
       (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES '(SYMMETRIC DEFINED REFLEXIVE))
       (SETQ DT*PREDICATE.COUNTER '0)
       (SETQ DT*EQUALITY.SYMBOLS '("=" ":=" "=:" ":=:"))
       (SETQ DT*EQUALITY.PREDICATES 'NIL)
       (SETQ DT*NONEQUALITY.PREDICATES '(12 10 1 0))
       (SETQ DT*PREDICATE.ALL '(12 10 1 0))
       (SETQ DT*PREDICATE.WITH.ATTRIBUTES 'NIL)
       (SETQ DT*PREDICATE.COMPONENTS
              '(PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES
                REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES
                -TOTHERSIDES))
       (SETQ DT*TRUE.PREDICATE '0)
       (SETQ DT*FALSE.PREDICATE '1)
       (SETQ DT*UNI.CREATES.VARIABLES 'NIL)
       (SETQ DT*SIGN.MINUS.SYMBOLS '(- --))
       (SETQ DT*SIGN.PLUS.SYMBOLS '(+ ++))
       (SETQ DT*SYMBOL.KINDS '(CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET 'ANY 'DT*SORT.NUMBER) '1)
              (SETF (GET 'ANY 'DT*DIRECT.SUBSORTS) 'NIL)
              (SETF (GET 'ANY 'DT*TRANSITIVE.CLOSURE) '(ANY))
              (SETF (GET 'ANY 'DT*MAX.SUBSORTS) '((ANY ANY)))
              (SETF (GET 'ANY 'DT*MINIMAL.SUBSORTS) '(ANY))
              (SETF (GET 'ANY 'DT*DIRECT.SUPERSORTS) 'NIL)
              (SETF (GET 'ANY 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY)))) ))