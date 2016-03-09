;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION '((ALL X P (X) OR P (F (X))) (ALL X NOT P (X) OR NOT P (F (F (X))))) '((ALL 2
   (OR (+ 3 (2) NIL)
       (+ 3 ((4 2)) NIL)))
  (ALL 5
   (OR (NOT (+ 3 (5) NIL))
       (NOT (+ 3 ((4 (4 5))) NIL))))) 'NIL 'NIL '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 19-APR,1989 14:52 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST '(59935 59949 59964 59969 59984 59999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "P" NIL NIL (ANY) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE)))
          ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 14 "F" NIL ANY (ANY)
          NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 60000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 60000)
             (SETQ COUNTER1 6)
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
            (T (UNLESS (= MEM*SIZE 60000)
                 (MEM-INITIALIZE 60000))
               (SETQ COUNTER1 6)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 60000) (+ 59930 INCREMENT) 59930))
      (SETQ MEM*NEXT.VADR 6
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
       (SETQ DT*CONSTANT.ALL 'NIL)
       (SETQ DT*ABBREVIATIONS 'NIL)
       (SETQ DT*FUNCTION.COUNTER '0)
       (SETQ DT*FUNCTION.ALL '(4))
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
       (SETQ DT*NONEQUALITY.PREDICATES '(3 1 0))
       (SETQ DT*PREDICATE.ALL '(3 1 0))
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