;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((E (LUPO WOLF) AND E (FOXY FOX) AND E (TWEETY BIRD) AND E (SCHNECKI SNAIL) AND E (STALKY GRAIN)) (ALL X : WOLF E (X ANIMAL)) (ALL X : FOX E (X ANIMAL)) (ALL X : BIRD E (X ANIMAL)) (ALL X : SNAIL E (X ANIMAL)) (ALL X : GRAIN E (X PLANT)) (ALL X : ANIMAL (ALL Y : PLANT EATS (X Y)) OR (ALL Y : ANIMAL SMALLER (Y X) AND (EX Z : (PLANT) EATS (Y Z)) IMPL EATS (X Y))) (ALL X : SNAIL ALL Y : BIRD SMALLER (X Y)) (ALL X : BIRD ALL Y : FOX SMALLER (X Y)) (ALL X : FOX ALL Y : WOLF SMALLER (X Y)) (ALL X : FOX ALL Y : WOLF NOT EATS (Y X)) (ALL X : GRAIN ALL Y : WOLF NOT EATS (Y X)) (ALL X : BIRD ALL Y : SNAIL NOT EATS (X Y)) (ALL X : SNAIL EX Y : PLANT EATS (X Y)))) (QUOTE ((AND (+ 4 (5 6) NIL) (AND (+ 4 (7 8) NIL) (AND (+ 4 (9 10) NIL) (AND (+ 4 (11 12) NIL) (+ 4 (13 14) NIL))))) (ALL 15 (+ 4 (15 16) NIL)) (ALL 17 (+ 4 (17 16) NIL)) (ALL 18 (+ 4 (18 16) NIL)) (ALL 19 (+ 4 (19 16) NIL)) (ALL 20 (+ 4 (20 21) NIL)) (ALL 22 (OR (ALL 23 (+ 24 (22 23) NIL)) (ALL 25 (IMPL (AND (+ 26 (25 22) NIL) (EX 27 (+ 24 (25 27) NIL))) (+ 24 (22 25) NIL))))) (ALL 28 (ALL 29 (+ 26 (28 29) NIL))) (ALL 30 (ALL 31 (+ 26 (30 31) NIL))) (ALL 32 (ALL 33 (+ 26 (32 33) NIL))) (ALL 34 (ALL 35 (NOT (+ 24 (35 34) NIL)))) (ALL 36 (ALL 37 (NOT (+ 24 (37 36) NIL)))) (ALL 38 (ALL 39 (NOT (+ 24 (38 39) NIL)))) (ALL 40 (EX 41 (+ 24 (40 41) NIL))))) (QUOTE ((* THEOREMS) (EX X,Y : ANIMAL ALL Z : GRAIN EATS (X Y) AND EATS (Y Z)))) (QUOTE (COMMENT (EX 43 (EX 42 (ALL 44 (AND (+ 24 (42 43) NIL) (+ 24 (43 44) NIL))))))) (QUOTE ("Edit:     Axioms and Theorems edited: 12-MAR,1991 22:23 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(159719 159724 159729 159734 159739 159744 159749 159754 159759 159764 159769 159774
          159779 159784 159789 159794 159799 159804 159819 159824 159839 159844 159849 159854
          159859 159864 159869 159874 159879 159884 159889 159894 159899 159904 159909 159914
          159919 159924 159929 159934 159949 159954 159969 159984 159999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" NIL NIL (ANY ANY)
          (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) NIL ((- 2 (POSITIVE . SYMMETRIC)))
          ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "OMEGA" (DT*ST-KIND NIL) CONSTANT 15 "E"
          NIL NIL (ANY ANY) (DEFINED) NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL)))
          ((- 4 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "LUPO" (DT*ST-KIND NIL)
          CONSTANT 5 ANY "WOLF" (DT*ST-KIND NIL) CONSTANT 5 ANY "FOXY" (DT*ST-KIND NIL)
          CONSTANT 5 ANY "FOX" (DT*ST-KIND NIL) CONSTANT 5 ANY "TWEETY" (DT*ST-KIND NIL)
          CONSTANT 5 ANY "BIRD" (DT*ST-KIND NIL) CONSTANT 5 ANY "SCHNECKI" (DT*ST-KIND NIL)
          CONSTANT 5 ANY "SNAIL" (DT*ST-KIND NIL) CONSTANT 5 ANY "STALKY" (DT*ST-KIND NIL)
          CONSTANT 5 ANY "GRAIN" (DT*ST-KIND NIL) CONSTANT 5 6 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "ANIMAL" (DT*ST-KIND NIL) CONSTANT 5
          8 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 10 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 12 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 14 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "PLANT"
          (DT*ST-KIND NIL) CONSTANT 5 16 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 21
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "EATS" NIL NIL (ANY ANY) NIL NIL
          ((- 24 (POSITIVE))) ((+ 24 (NEGATIVE))) ((+ 24 (NIL))) ((- 24 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 16 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15
          "SMALLER" NIL NIL (ANY ANY) NIL NIL ((- 26 (POSITIVE))) ((+ 26 (NEGATIVE)))
          ((+ 26 (NIL))) ((- 26 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 21 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 12 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 10 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 10 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 8 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 8 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 6 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 8 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 6 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 14 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 6 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 10 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 12 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 12 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 21 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 16 NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 16 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 14 NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 160000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 160000)
             (SETQ COUNTER1 45)
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
            (T (UNLESS (= MEM*SIZE 160000)
                 (MEM-INITIALIZE 160000))
               (SETQ COUNTER1 45)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 160000) (+ 159714 INCREMENT) 159714))
      (SETQ MEM*NEXT.VADR 45
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*ELEMENT.PREDICATE (QUOTE 4)) (SETQ DT*OMEGA.CONSTANT (QUOTE 3)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (21 16 14 13 12 11 10 9 8 7 6 5 3))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE NIL)) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (26 24 4 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (26 24 4 2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))