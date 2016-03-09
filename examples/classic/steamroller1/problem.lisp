;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION '((EX XA,XB,XC,XD,XE,XF WOLF (XA) AND FOX (XB) AND BIRD (XC) AND CATERPILLAR (XD) AND SNAIL
   (XE) AND GRAIN (XF))
  (ALL X (WOLF (X) OR FOX (X) OR BIRD (X) OR CATERPILLAR (X) OR SNAIL (X)) IMPL ANIMAL (X))
  (ALL X GRAIN (X) IMPL PLANT (X))
  (ALL X ANIMAL (X) IMPL (ALL Y PLANT (Y) IMPL EATS (X Y)) OR
   (ALL Y ANIMAL (Y) AND SMALLER (Y X) AND (EX Z PLANT (Z) AND EATS (Y Z)) IMPL EATS (X Y)))
  (ALL X,Y (CATERPILLAR (X) OR SNAIL (X)) AND BIRD (Y) IMPL SMALLER (X Y))
  (ALL X,Y BIRD (X) AND FOX (Y) IMPL SMALLER (X Y))
  (ALL X,Y FOX (X) AND WOLF (Y) IMPL SMALLER (X Y))
  (ALL X,Y (FOX (X) OR GRAIN (X)) AND WOLF (Y) IMPL NOT EATS (Y X))
  (ALL X,Y BIRD (X) AND CATERPILLAR (Y) IMPL EATS (X Y))
  (ALL X,Y BIRD (X) AND SNAIL (Y) IMPL NOT EATS (X Y))
  (ALL X (CATERPILLAR (X) OR SNAIL (X)) IMPL (EX Y PLANT (Y) AND EATS (X Y)))) '((EX 7
   (EX 6
    (EX 5
     (EX 4
      (EX 3
       (EX 2
        (AND (+ 8 (2) NIL)
             (AND (+ 9 (3) NIL)
                  (AND (+ 10 (4) NIL)
                       (AND (+ 11 (5) NIL)
                            (AND (+ 12 (6) NIL)
                                 (+ 13 (7) NIL))))))))))))
  (ALL 14
   (IMPL
    (OR (+ 8 (14) NIL)
        (OR (+ 9 (14) NIL)
            (OR (+ 10 (14) NIL)
                (OR (+ 11 (14) NIL)
                    (+ 12 (14) NIL)))))
    (+ 15 (14) NIL)))
  (ALL 16 (IMPL (+ 13 (16) NIL) (+ 17 (16) NIL)))
  (ALL 18
   (IMPL (+ 15 (18) NIL)
    (OR (ALL 19 (IMPL (+ 17 (19) NIL) (+ 20 (18 19) NIL)))
        (ALL 21
         (IMPL
          (AND (+ 15 (21) NIL)
               (AND (+ 22 (21 18) NIL)
                    (EX 23
                     (AND (+ 17 (23) NIL)
                          (+ 20 (21 23) NIL)))))
          (+ 20 (18 21) NIL))))))
  (ALL 25
   (ALL 24
    (IMPL
     (AND (OR (+ 11 (24) NIL)
              (+ 12 (24) NIL))
          (+ 10 (25) NIL))
     (+ 22 (24 25) NIL))))
  (ALL 27
   (ALL 26
    (IMPL
     (AND (+ 10 (26) NIL)
          (+ 9 (27) NIL))
     (+ 22 (26 27) NIL))))
  (ALL 29
   (ALL 28
    (IMPL
     (AND (+ 9 (28) NIL)
          (+ 8 (29) NIL))
     (+ 22 (28 29) NIL))))
  (ALL 31
   (ALL 30
    (IMPL
     (AND (OR (+ 9 (30) NIL)
              (+ 13 (30) NIL))
          (+ 8 (31) NIL))
     (NOT (+ 20 (31 30) NIL)))))
  (ALL 33
   (ALL 32
    (IMPL
     (AND (+ 10 (32) NIL)
          (+ 11 (33) NIL))
     (+ 20 (32 33) NIL))))
  (ALL 35
   (ALL 34
    (IMPL
     (AND (+ 10 (34) NIL)
          (+ 12 (35) NIL))
     (NOT (+ 20 (34 35) NIL)))))
  (ALL 36
   (IMPL
    (OR (+ 11 (36) NIL)
        (+ 12 (36) NIL))
    (EX 37
     (AND (+ 17 (37) NIL)
          (+ 20 (36 37) NIL)))))) '((EX X,Y ANIMAL (X) AND ANIMAL (Y) AND (ALL Z GRAIN (Z) IMPL EATS (X Y) AND EATS (Y Z)))) '((EX 39
   (EX 38
    (AND (+ 15 (38) NIL)
         (AND (+ 15 (39) NIL)
              (ALL 40
               (IMPL (+ 13 (40) NIL)
                (AND (+ 20 (38 39) NIL)
                     (+ 20 (39 40) NIL))))))))) '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 19-APR,1989 11:43 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(29679 29684 29689 29694 29699 29704 29709 29714 29719 29724 29729 29734 29739 29744
          29749 29754 29759 29764 29779 29784 29799 29804 29809 29824 29829 29844 29849 29864
          29879 29894 29909 29924 29939 29944 29949 29954 29959 29964 29969 29984 29999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "WOLF" NIL NIL (ANY) NIL NIL
          ((- 8 (POSITIVE))) ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "FOX" NIL NIL (ANY) NIL NIL ((- 9 (POSITIVE)))
          ((+ 9 (NEGATIVE))) ((+ 9 (NIL))) ((- 9 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15
          "BIRD" NIL NIL (ANY) NIL NIL ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL)))
          ((- 10 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "CATERPILLAR" NIL NIL (ANY) NIL
          NIL ((- 11 (POSITIVE))) ((+ 11 (NEGATIVE))) ((+ 11 (NIL))) ((- 11 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "SNAIL" NIL NIL (ANY) NIL NIL ((- 12 (POSITIVE)))
          ((+ 12 (NEGATIVE))) ((+ 12 (NIL))) ((- 12 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          15 "GRAIN" NIL NIL (ANY) NIL NIL ((- 13 (POSITIVE))) ((+ 13 (NEGATIVE)))
          ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "ANIMAL" NIL NIL (ANY) NIL NIL
          ((- 15 (POSITIVE))) ((+ 15 (NEGATIVE))) ((+ 15 (NIL))) ((- 15 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15
          "PLANT" NIL NIL (ANY) NIL NIL ((- 17 (POSITIVE))) ((+ 17 (NEGATIVE))) ((+ 17 (NIL)))
          ((- 17 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "EATS" NIL NIL (ANY ANY) NIL NIL ((- 20 (POSITIVE))) ((+ 20 (NEGATIVE)))
          ((+ 20 (NIL))) ((- 20 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "SMALLER" NIL NIL (ANY ANY) NIL NIL
          ((- 22 (POSITIVE))) ((+ 22 (NEGATIVE))) ((+ 22 (NIL))) ((- 22 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 30000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 30000)
             (SETQ COUNTER1 41)
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
               (SETQ COUNTER1 41)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 30000) (+ 29674 INCREMENT) 29674))
      (SETQ MEM*NEXT.VADR 41
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
       (SETQ DT*NONEQUALITY.PREDICATES '(22 20 17 15 13 12 11 10 9 8 1 0))
       (SETQ DT*PREDICATE.ALL '(22 20 17 15 13 12 11 10 9 8 1 0))
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