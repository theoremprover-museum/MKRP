;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((ALL X A (B (X)) = C (X)) (ALL X B (C (X)) = D (X)) (ALL X C (D (X)) = E (X)) (ALL X D (E (X)) = F (X)) (ALL X E (F (X)) = G (X)) (ALL X F (G (X)) = A (X)) (ALL X G (A (X)) = B (X)) (ALL X A (A1 (X)) = X) (ALL X B (B1 (X)) = X) (ALL X C (C1 (X)) = X) (ALL X D (D1 (X)) = X) (ALL X E (E1 (X)) = X) (ALL X F (F1 (X)) = X) (ALL X G (G1 (X)) = X) (ALL X A1 (A (X)) = X) (ALL X B1 (B (X)) = X) (ALL X C1 (C (X)) = X) (ALL X D1 (D (X)) = X) (ALL X E1 (E (X)) = X) (ALL X F1 (F (X)) = X) (ALL X G1 (G (X)) = X))) (QUOTE ((ALL 3 (+ 2 ((6 (4 3)) (5 3)) NIL)) (ALL 7 (+ 2 ((4 (5 7)) (8 7)) NIL)) (ALL 9 (+ 2 ((5 (8 9)) (10 9)) NIL)) (ALL 11 (+ 2 ((8 (10 11)) (12 11)) NIL)) (ALL 13 (+ 2 ((10 (12 13)) (14 13)) NIL)) (ALL 15 (+ 2 ((12 (14 15)) (6 15)) NIL)) (ALL 16 (+ 2 ((14 (6 16)) (4 16)) NIL)) (ALL 17 (+ 2 ((6 (18 17)) 17) NIL)) (ALL 19 (+ 2 ((4 (20 19)) 19) NIL)) (ALL 21 (+ 2 ((5 (22 21)) 21) NIL)) (ALL 23 (+ 2 ((8 (24 23)) 23) NIL)) (ALL 25 (+ 2 ((10 (26 25)) 25) NIL)) (ALL 27 (+ 2 ((12 (28 27)) 27) NIL)) (ALL 29 (+ 2 ((14 (30 29)) 29) NIL)) (ALL 31 (+ 2 ((18 (6 31)) 31) NIL)) (ALL 32 (+ 2 ((20 (4 32)) 32) NIL)) (ALL 33 (+ 2 ((22 (5 33)) 33) NIL)) (ALL 34 (+ 2 ((24 (8 34)) 34) NIL)) (ALL 35 (+ 2 ((26 (10 35)) 35) NIL)) (ALL 36 (+ 2 ((28 (12 36)) 36) NIL)) (ALL 37 (+ 2 ((30 (14 37)) 37) NIL)))) (QUOTE ((ALL EPS F (EPS) = D1 (D1 (D1 (D1 (EPS))))))) (QUOTE ((ALL 38 (+ 2 ((12 38) (24 (24 (24 (24 38))))) NIL)))) (QUOTE ("                                                                                                                     " "*********************************************************************************************************************" "                                                                                                                     " " FORMULA:  Date: 23-OCT,1991 22:54" "           Axioms und Theorems for this proof have been read from" "              Axiom-File: NIL" "            Theorem-File: NIL" "                                                                                                                     " "*********************************************************************************************************************")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9653 9658 9663 9668 9673 9678 9683 9688 9702 9707 9721 9726 9740 9745 9759 9764 9778
          9783 9797 9802 9816 9821 9826 9831 9845 9850 9864 9869 9883 9888 9902 9907 9921 9935
          9949 9954 9969 9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" NIL NIL (ANY ANY)
          (DEFINED SYMMETRIC REFLEXIVE) NIL ((- 2 (POSITIVE . SYMMETRIC)))
          ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 14 "B" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 14
          "C" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 14 "A" NIL ANY
          (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "D" NIL ANY (ANY) NIL NIL NIL NIL NIL 1
          ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14
          "E" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "F" NIL ANY (ANY) NIL NIL NIL NIL NIL 1
          ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14
          "G" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "A1" NIL ANY (ANY)
          NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "B1" NIL ANY (ANY) NIL NIL NIL NIL NIL
          1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          14 "C1" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "D1" NIL ANY (ANY) NIL NIL NIL NIL NIL
          1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          14 "E1" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "F1" NIL ANY (ANY) NIL NIL NIL NIL NIL
          1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          14 "G1" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE))
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
             (SETQ COUNTER1 39)
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
               (SETQ COUNTER1 39)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9648 INCREMENT) 9648))
      (SETQ MEM*NEXT.VADR 39
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*ELEMENT.PREDICATE (QUOTE NIL)) (SETQ DT*OMEGA.CONSTANT (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE NIL)) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (30 28 26 24 22 20 18 14 12 10 8 6 5 4))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))