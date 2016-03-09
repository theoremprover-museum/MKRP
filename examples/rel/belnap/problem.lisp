;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION 'NIL 'NIL '((ALL U,V
   ((NOT ((ALL W,X
           ((NOT ((A (W CONS (R (U W X) CONS (R (0 U V) NNN)))))) OR
            (ALL Y,Z
             ((NOT ((B (Y CONS (R (X Y Z) CONS (R (U W X) CONS (R (0 U V) NNN))))))) OR
              (B (Z CONS (R (X Y Z) CONS (R (U W X) CONS (R (0 U V) NNN)))))))))))
    OR
    (ALL X0001,X0002
     ((NOT ((A (X0001 CONS (R (V X0001 X0002) CONS (R (0 U V) NNN)))))) OR
      (ALL X0003,X0004
       ((NOT ((A
               (X0003 CONS
                (R (X0002 X0003 X0004) CONS (R (V X0001 X0002) CONS (R (0 U V) NNN)))))))
        OR
        (ALL X0005,X0006
         ((NOT ((B
                 (X0005 CONS
                  (R (X0004 X0005 X0006) CONS
                   (R (X0002 X0003 X0004) CONS (R (V X0001 X0002) CONS (R (0 U V) NNN))))))))
          OR
          (B
           (X0006 CONS
            (R (X0004 X0005 X0006) CONS
             (R (X0002 X0003 X0004) CONS (R (V X0001 X0002) CONS (R (0 U V) NNN))))))))))))))) '((ALL 3
   (ALL 2
    (OR (NOT (ALL 5
              (ALL 4
               (OR (NOT (+ 10 (4 (9 (6 2 4 5) (9 (6 7 2 3) 8))) NIL))
                   (ALL 12
                    (ALL 11
                     (OR (NOT (+ 13 (11 (9 (6 5 11 12) (9 (6 2 4 5) (9 (6 7 2 3) 8)))) NIL))
                         (+ 13 (12 (9 (6 5 11 12) (9 (6 2 4 5) (9 (6 7 2 3) 8)))) NIL))))))))
        (ALL 15
         (ALL 14
          (OR (NOT (+ 10 (14 (9 (6 3 14 15) (9 (6 7 2 3) 8))) NIL))
              (ALL 17
               (ALL 16
                (OR (NOT (+ 10 (16 (9 (6 15 16 17) (9 (6 3 14 15) (9 (6 7 2 3) 8)))) NIL))
                    (ALL 19
                     (ALL 18
                      (OR (NOT (+ 13
                                  (18
                                   (9 (6 17 18 19)
                                    (9 (6 15 16 17) (9 (6 3 14 15) (9 (6 7 2 3) 8)))))
                                  NIL))
                          (+ 13
                             (19
                              (9 (6 17 18 19) (9 (6 15 16 17) (9 (6 3 14 15) (9 (6 7 2 3) 8))))
                              )
                             NIL)))))))))))))) '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 11-AUG,1989 03:48 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(59846 59851 59856 59861 59866 59871 59886 59891 59896 59911 59925 59930 59935 59949
          59954 59959 59964 59969 59984 59999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 14 "R" NIL ANY (ANY ANY ANY) NIL NIL NIL NIL NIL 3 ANY (DT*ST-KIND NIL)
          FUNCTION 5 ANY "0" (DT*ST-KIND NIL) CONSTANT 5 ANY "NNN" (DT*ST-KIND NIL) CONSTANT
          14 "CONS" NIL ANY (ANY ANY) NIL NIL NIL NIL NIL 2 ANY (DT*ST-KIND NIL) FUNCTION 15
          "A" NIL NIL (ANY ANY) NIL NIL ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL)))
          ((- 10 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "B" NIL NIL (ANY ANY) NIL NIL ((- 13 (POSITIVE))) ((+ 13 (NEGATIVE)))
          ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 60000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 60000)
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
            (T (UNLESS (= MEM*SIZE 60000)
                 (MEM-INITIALIZE 60000))
               (SETQ COUNTER1 20)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 60000) (+ 59841 INCREMENT) 59841))
      (SETQ MEM*NEXT.VADR 20
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
       (SETQ DT*CONSTANT.ALL '(8 7))
       (SETQ DT*ABBREVIATIONS 'NIL)
       (SETQ DT*FUNCTION.COUNTER '0)
       (SETQ DT*FUNCTION.ALL '(9 6))
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
       (SETQ DT*NONEQUALITY.PREDICATES '(13 10 1 0))
       (SETQ DT*PREDICATE.ALL '(13 10 1 0))
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