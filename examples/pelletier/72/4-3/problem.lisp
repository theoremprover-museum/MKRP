;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* AXIOMS *) (P1A OR P1B OR P1C) (P2A OR P2B OR P2C) (P3A OR P3B OR P3C) (P4A OR P4B OR P4C) (NOT P1A OR NOT P2A) (NOT P1A OR NOT P3A) (NOT P1A OR NOT P4A) (NOT P2A OR NOT P3A) (NOT P2A OR NOT P4A) (NOT P3A OR NOT P4A) (NOT P1B OR NOT P2B) (NOT P1B OR NOT P3B) (NOT P1B OR NOT P4B) (NOT P2B OR NOT P3B) (NOT P2B OR NOT P4B) (NOT P3B OR NOT P4B) (NOT P1C OR NOT P2C) (NOT P1C OR NOT P3C) (NOT P1C OR NOT P4C) (NOT P2C OR NOT P3C) (NOT P2C OR NOT P4C) (NOT P3C OR NOT P4C))) (QUOTE (COMMENT (OR (+ 2 NIL NIL) (OR (+ 3 NIL NIL) (+ 4 NIL NIL))) (OR (+ 5 NIL NIL) (OR (+ 6 NIL NIL) (+ 7 NIL NIL))) (OR (+ 8 NIL NIL) (OR (+ 9 NIL NIL) (+ 10 NIL NIL))) (OR (+ 11 NIL NIL) (OR (+ 12 NIL NIL) (+ 13 NIL NIL))) (OR (NOT (+ 2 NIL NIL)) (NOT (+ 5 NIL NIL))) (OR (NOT (+ 2 NIL NIL)) (NOT (+ 8 NIL NIL))) (OR (NOT (+ 2 NIL NIL)) (NOT (+ 11 NIL NIL))) (OR (NOT (+ 5 NIL NIL)) (NOT (+ 8 NIL NIL))) (OR (NOT (+ 5 NIL NIL)) (NOT (+ 11 NIL NIL))) (OR (NOT (+ 8 NIL NIL)) (NOT (+ 11 NIL NIL))) (OR (NOT (+ 3 NIL NIL)) (NOT (+ 6 NIL NIL))) (OR (NOT (+ 3 NIL NIL)) (NOT (+ 9 NIL NIL))) (OR (NOT (+ 3 NIL NIL)) (NOT (+ 12 NIL NIL))) (OR (NOT (+ 6 NIL NIL)) (NOT (+ 9 NIL NIL))) (OR (NOT (+ 6 NIL NIL)) (NOT (+ 12 NIL NIL))) (OR (NOT (+ 9 NIL NIL)) (NOT (+ 12 NIL NIL))) (OR (NOT (+ 4 NIL NIL)) (NOT (+ 7 NIL NIL))) (OR (NOT (+ 4 NIL NIL)) (NOT (+ 10 NIL NIL))) (OR (NOT (+ 4 NIL NIL)) (NOT (+ 13 NIL NIL))) (OR (NOT (+ 7 NIL NIL)) (NOT (+ 10 NIL NIL))) (OR (NOT (+ 7 NIL NIL)) (NOT (+ 13 NIL NIL))) (OR (NOT (+ 10 NIL NIL)) (NOT (+ 13 NIL NIL))))) (QUOTE NIL) (QUOTE NIL) (QUOTE ("Edit:     Axioms and Theorems edited: 02-NOV,1989 20:24 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(159804 159819 159834 159849 159864 159879 159894 159909 159924 159939 159954 159969
          159984 159999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "P1A" NIL NIL NIL NIL NIL ((- 2 (POSITIVE)))
          ((+ 2 (NEGATIVE))) ((+ 2 (NIL))) ((- 2 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15
          "P1B" NIL NIL NIL NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE))) ((+ 3 (NIL)))
          ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "P1C" NIL NIL NIL NIL NIL
          ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL))) ((- 4 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "P2A" NIL NIL NIL NIL NIL ((- 5 (POSITIVE)))
          ((+ 5 (NEGATIVE))) ((+ 5 (NIL))) ((- 5 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15
          "P2B" NIL NIL NIL NIL NIL ((- 6 (POSITIVE))) ((+ 6 (NEGATIVE))) ((+ 6 (NIL)))
          ((- 6 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "P2C" NIL NIL NIL NIL NIL
          ((- 7 (POSITIVE))) ((+ 7 (NEGATIVE))) ((+ 7 (NIL))) ((- 7 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "P3A" NIL NIL NIL NIL NIL ((- 8 (POSITIVE)))
          ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15
          "P3B" NIL NIL NIL NIL NIL ((- 9 (POSITIVE))) ((+ 9 (NEGATIVE))) ((+ 9 (NIL)))
          ((- 9 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "P3C" NIL NIL NIL NIL NIL
          ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL))) ((- 10 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "P4A" NIL NIL NIL NIL NIL ((- 11 (POSITIVE)))
          ((+ 11 (NEGATIVE))) ((+ 11 (NIL))) ((- 11 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          15 "P4B" NIL NIL NIL NIL NIL ((- 12 (POSITIVE))) ((+ 12 (NEGATIVE))) ((+ 12 (NIL)))
          ((- 12 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "P4C" NIL NIL NIL NIL NIL
          ((- 13 (POSITIVE))) ((+ 13 (NEGATIVE))) ((+ 13 (NIL))) ((- 13 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE))
       (INCREMENT (- MEM*SIZE 160000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 160000)
             (SETQ COUNTER1 14)
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
               (SETQ COUNTER1 14)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 160000) (+ 159789 INCREMENT) 159789))
      (SETQ MEM*NEXT.VADR 14
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE NIL)) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE NIL)) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (13 12 11 10 9 8 7 6 5 4 3 2 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (13 12 11 10 9 8 7 6 5 4 3 2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))