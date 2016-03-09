;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION '((ALL X R (X X X)) (ALL X,Y,Z R (X Y Z) IMPL (R (Y X Z) AND R (X Z Y)))
  (ALL X,Y,Z,U,V (R (X Y Z) AND R (Z U V)) IMPL (EX W R (X U W) AND R (W Y V)))) '((ALL 2 (+ 3 (2 2 2) NIL))
  (ALL 6
   (ALL 5
    (ALL 4
     (IMPL (+ 3 (4 5 6) NIL)
      (AND (+ 3 (5 4 6) NIL)
           (+ 3 (4 6 5) NIL))))))
  (ALL 11
   (ALL 10
    (ALL 9
     (ALL 8
      (ALL 7
       (IMPL
        (AND (+ 3 (7 8 9) NIL)
             (+ 3 (9 10 11) NIL))
        (EX 12
         (AND (+ 3 (7 10 12) NIL)
              (+ 3 (12 8 11) NIL)))))))))) '(((NOT (((ALL V,W,VS,WS
           (NOT (R (C E V))
                OR
                NOT
                (R (V W F))
                OR
                NOT
                (R (A G H))
                OR
                NOT
                (R (C E VS))
                OR
                NOT
                (R (VS WS F)))
           AND (NOT R (C E V) OR R (W G H) OR ((NOT (R (V W F))))) AND R (A C D) AND R (D E F))
          ))))) '((NOT (ALL 16
        (ALL 15
         (ALL 14
          (ALL 13
           (AND (OR (NOT (+ 3 (17 18 13) NIL))
                    (OR (NOT (+ 3 (13 14 19) NIL))
                        (OR (NOT (+ 3 (20 21 22) NIL))
                            (OR (NOT (+ 3 (17 18 15) NIL))
                                (NOT (+ 3 (15 16 19) NIL))))))
                (AND (OR (NOT (+ 3 (17 18 13) NIL))
                         (OR (+ 3 (14 21 22) NIL)
                             (NOT (+ 3 (13 14 19) NIL))))
                     (AND (+ 3 (20 17 23) NIL)
                          (+ 3 (23 18 19) NIL)))))))))) '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 25-JUL,1989 15:52 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(19854 19859 19864 19869 19874 19879 19884 19889 19894 19899 19904 19909 19914 19919
          19924 19929 19934 19939 19944 19949 19964 19969 19984 19999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "R" NIL NIL (ANY ANY ANY) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE)))
          ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "C" (DT*ST-KIND NIL) CONSTANT 5 ANY
          "E" (DT*ST-KIND NIL) CONSTANT 5 ANY "F" (DT*ST-KIND NIL) CONSTANT 5 ANY "A"
          (DT*ST-KIND NIL) CONSTANT 5 ANY "G" (DT*ST-KIND NIL) CONSTANT 5 ANY "H"
          (DT*ST-KIND NIL) CONSTANT 5 ANY "D" (DT*ST-KIND NIL) CONSTANT))
       (INCREMENT (- MEM*SIZE 20000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 20000)
             (SETQ COUNTER1 24)
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
            (T (UNLESS (= MEM*SIZE 20000)
                 (MEM-INITIALIZE 20000))
               (SETQ COUNTER1 24)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 20000) (+ 19849 INCREMENT) 19849))
      (SETQ MEM*NEXT.VADR 24
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
       (SETQ DT*CONSTANT.ALL '(23 22 21 20 19 18 17))
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