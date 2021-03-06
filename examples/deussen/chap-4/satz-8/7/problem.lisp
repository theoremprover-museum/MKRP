;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* ************************************************) (* DEFINITIONEN DER MENGENLEHRE) (SORT SET : ANY) (SORT ELEMENT : SET) (TYPE EL (ELEMENT SET)) (* DEFINITION 1.1 : TEILMENGE) (TYPE SUBSET (SET SET)) (ALL X,Y : SET SUBSET (X Y) EQV (ALL A : ELEMENT EL (A X) IMPL EL (A Y))) (* DEFINITION 1.3 : KARTESISCHES PRODUKT) (TYPE CARTES (SET SET) : SET) (TYPE PAIR (ELEMENT ELEMENT) : ELEMENT) (* DEFINITION 1.4 : VEREINIGUNG) (TYPE UNION (REL REL) : REL) (* DEFINITION 1.6 : NATUERLICHE ZAHLEN) (SORT NAT : SET) (TYPE 1 : NAT) (TYPE PLUS (NAT NAT) : NAT) (* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S) (SORT REL : SET) (TYPE S : SET) (SORT EL.OF.S : ELEMENT) (* DEFINITION 2.8 : AEQUIVALENZRELATION) (SORT EQU.RELATION : REL) (* DEFINITION 2.9 : POTENZ EINER RELATION) (TYPE POWER (REL NAT) : REL) (* DEFINITION 2.11 : TRANSITIVER ABSCHLUSS) (TYPE TRANS.CLOS (REL) : REL) (ALL RHO : REL ALL A : ELEMENT EL (A TRANS.CLOS (RHO)) EQV (EX N : NAT EL (A POWER (RHO N)))) (* LEMMA 4-8-5 UND 4-8-5 UND INDUKTION) (ALL RHO,SIGMA,TAU : EQU.RELATION SUBSET (UNION (RHO SIGMA) TAU) IMPL (ALL N : NAT SUBSET (POWER (UNION (RHO SIGMA) N) TAU))) (* DEFINITION DER TRANSITIVEN VEREINIGUNG) (TYPE TRANS.UNION (REL REL) : REL) (ALL RHO,SIGMA : REL TRANS.UNION (RHO SIGMA) : = TRANS.CLOS (UNION (RHO SIGMA))))) (QUOTE (COMMENT COMMENT (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) COMMENT (+ 0 NIL) (ALL 17 (ALL 18 (EQV (+ 19 (18 17) (KIND (EQV T 90))) (ALL 16 (IMPL (+ 20 (16 18) (KIND (EQV NIL 90))) (+ 20 (16 17) (KIND (EQV NIL 90)))))))) COMMENT (+ 0 NIL) (+ 0 NIL) COMMENT (+ 0 NIL) COMMENT (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) COMMENT (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) COMMENT (+ 0 NIL) COMMENT (+ 0 NIL) COMMENT (+ 0 NIL) (ALL 25 (ALL 26 (EQV (+ 20 (26 (24 25)) (KIND (EQV T 290))) (EX 27 (+ 20 (26 (23 25 27)) (KIND (EQV NIL 290))))))) COMMENT (ALL 30 (ALL 29 (ALL 28 (IMPL (+ 19 ((5 28 29) 30) NIL) (ALL 31 (+ 19 ((23 (5 28 29) 31) 30) NIL)))))) COMMENT (+ 0 NIL) (ALL 34 (ALL 33 (+ 35 ((32 33 34) (24 (5 33 34))) NIL))))) (QUOTE ((* THEOREM *) (ALL RHO,SIGMA,TAU : EQU.RELATION SUBSET (UNION (RHO SIGMA) TAU) IMPL SUBSET (TRANS.UNION (RHO SIGMA) TAU)))) (QUOTE (COMMENT (ALL 38 (ALL 37 (ALL 36 (IMPL (+ 19 ((5 36 37) 38) NIL) (+ 19 ((32 36 37) 38) NIL))))))) (QUOTE ("Edit:     Axioms and Theorems edited: 10-NOV,1989 18:27 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9746 9751 9756 9771 9776 9781 9795 9800 9805 9810 9815 9820 9825 9830 9844 9858 9863
          9877 9969 9954 9939 9934 9929 9924 ATP.MEMORY.NIL ATP.MEMORY.NIL ATP.MEMORY.NIL
          ATP.MEMORY.NIL ATP.MEMORY.NIL ATP.MEMORY.NIL ATP.MEMORY.NIL ATP.MEMORY.NIL 9910 9896
          9882 ATP.MEMORY.NIL ATP.MEMORY.NIL 9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "EL" NIL NIL (ELEMENT SET) NIL NIL
          ((- 20 (POSITIVE))) ((+ 20 (NEGATIVE))) ((+ 20 (NIL))) ((- 20 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "SUBSET" NIL NIL (SET SET) NIL NIL ((- 19 (POSITIVE)))
          ((+ 19 (NEGATIVE))) ((+ 19 (NIL))) ((- 19 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 SET NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 SET NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEMENT NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "CARTES" NIL SET (SET SET) NIL NIL NIL
          NIL NIL 2 SET (DT*ST-KIND NIL) FUNCTION 14 "PAIR" NIL ELEMENT (ELEMENT ELEMENT) NIL
          NIL NIL NIL NIL 2 ELEMENT (DT*ST-KIND NIL) FUNCTION 14 "UNION" NIL REL (REL REL) NIL
          NIL NIL NIL NIL 2 REL (DT*ST-KIND NIL) FUNCTION 5 NAT "1" (DT*ST-KIND NIL) CONSTANT
          14 "PLUS" NIL NAT (NAT NAT) NIL NIL NIL NIL NIL 2 NAT (DT*ST-KIND NIL) FUNCTION 5
          SET "S" (DT*ST-KIND NIL) CONSTANT 14 "POWER" NIL REL (REL NAT) NIL NIL NIL NIL NIL 2
          REL (DT*ST-KIND NIL) FUNCTION 14 "TRANS.CLOS" NIL REL (REL) NIL NIL NIL NIL NIL 1
          REL (DT*ST-KIND NIL) FUNCTION 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5
          ELEMENT NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 NAT NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EQU.RELATION NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EQU.RELATION NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EQU.RELATION NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 NAT NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 14 "TRANS.UNION" NIL REL (REL REL) NIL NIL NIL NIL NIL 2 REL
          (DT*ST-KIND NIL) FUNCTION 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "=" NIL NIL (ANY ANY)
          (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) NIL ((- 35 (POSITIVE . SYMMETRIC)))
          ((+ 35 (NEGATIVE . SYMMETRIC))) ((+ 35 (NIL . SYMMETRIC))) ((- 35 (NIL . SYMMETRIC)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 EQU.RELATION NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EQU.RELATION NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EQU.RELATION NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
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
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9741 INCREMENT) 9741))
      (SETQ MEM*NEXT.VADR 39
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (EQU.RELATION EL.OF.S NAT REL ELEMENT SET ANY))) (SETQ DT*SORT.NR (QUOTE 14)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (22 4))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (32 24 23 21 5 6 15))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (35))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (19 20 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (35 19 20 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*SORT.NUMBER)) (QUOTE 13)) (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (EQU.RELATION))) (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION EQU.RELATION) (EL.OF.S) (NAT) (REL EQU.RELATION) (ELEMENT) (SET EQU.RELATION) (ANY EQU.RELATION)))) (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (EQU.RELATION))) (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (REL))) (SETF (GET (QUOTE EQU.RELATION) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY SET REL EQU.RELATION)))) (PROGN (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*SORT.NUMBER)) (QUOTE 12)) (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (EL.OF.S))) (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION) (EL.OF.S EL.OF.S) (NAT) (REL) (ELEMENT EL.OF.S) (SET EL.OF.S) (ANY EL.OF.S)))) (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (EL.OF.S))) (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ELEMENT))) (SETF (GET (QUOTE EL.OF.S) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY SET ELEMENT EL.OF.S)))) (PROGN (SETF (GET (QUOTE NAT) (QUOTE DT*SORT.NUMBER)) (QUOTE 11)) (SETF (GET (QUOTE NAT) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE NAT) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (NAT))) (SETF (GET (QUOTE NAT) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION) (EL.OF.S) (NAT NAT) (REL) (ELEMENT) (SET NAT) (ANY NAT)))) (SETF (GET (QUOTE NAT) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (NAT))) (SETF (GET (QUOTE NAT) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (SET))) (SETF (GET (QUOTE NAT) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY SET NAT)))) (PROGN (SETF (GET (QUOTE REL) (QUOTE DT*SORT.NUMBER)) (QUOTE 10)) (SETF (GET (QUOTE REL) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (EQU.RELATION))) (SETF (GET (QUOTE REL) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (REL EQU.RELATION))) (SETF (GET (QUOTE REL) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION EQU.RELATION) (EL.OF.S) (NAT) (REL REL) (ELEMENT) (SET REL) (ANY REL)))) (SETF (GET (QUOTE REL) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (REL))) (SETF (GET (QUOTE REL) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (SET))) (SETF (GET (QUOTE REL) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY SET REL)))) (PROGN (SETF (GET (QUOTE ELEMENT) (QUOTE DT*SORT.NUMBER)) (QUOTE 9)) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (EL.OF.S))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ELEMENT EL.OF.S))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION) (EL.OF.S EL.OF.S) (NAT) (REL) (ELEMENT ELEMENT) (SET ELEMENT) (ANY ELEMENT)))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (EL.OF.S))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (SET))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY SET ELEMENT)))) (PROGN (SETF (GET (QUOTE SET) (QUOTE DT*SORT.NUMBER)) (QUOTE 8)) (SETF (GET (QUOTE SET) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (NAT REL ELEMENT))) (SETF (GET (QUOTE SET) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (EQU.RELATION ELEMENT REL NAT SET EL.OF.S))) (SETF (GET (QUOTE SET) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION EQU.RELATION) (EL.OF.S EL.OF.S) (NAT NAT) (REL REL) (ELEMENT ELEMENT) (SET SET) (ANY SET)))) (SETF (GET (QUOTE SET) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (EL.OF.S REL NAT))) (SETF (GET (QUOTE SET) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE SET) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY SET)))) (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (SET))) (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (EQU.RELATION SET ANY NAT REL ELEMENT EL.OF.S))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EQU.RELATION EQU.RELATION) (EL.OF.S EL.OF.S) (NAT NAT) (REL REL) (ELEMENT ELEMENT) (SET SET) (ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (NAT REL EL.OF.S))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*LEAST.SUPERSORTS)) (QUOTE ((ANY ANY)))))) ))