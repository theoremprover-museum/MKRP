;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((SORT MENGE,ELEMENT : ANY) (SORT REL : MENGE) (SORT EL.VON.S : ELEMENT) (TYPE EL (ELEMENT MENGE)) (TYPE PAAR (ELEMENT ELEMENT) : ELEMENT) (* DEFINITION VON DURCHSCHNITT) (TYPE DURCHSCHNITT (REL REL) : REL) (ALL X,Y : REL ALL A : ELEMENT EL (A DURCHSCHNITT (X Y)) EQV EL (A X) AND EL (A Y)) (* DEFINITION REFLEXIV) (TYPE REFLEXIV (REL)) (ALL RHO : REL REFLEXIV (RHO) EQV (ALL A : EL.VON.S EL (PAAR (A A) RHO))) (* DEFINITION SYMMETRISCH) (TYPE SYMMETRISCH (REL)) (ALL RHO : REL SYMMETRISCH (RHO) EQV (ALL A,B : EL.VON.S EL (PAAR (A B) RHO) IMPL EL (PAAR (B A) RHO))) (* DEFINITION TRANSITIV) (TYPE TRANSITIV (REL)) (ALL RHO : REL TRANSITIV (RHO) EQV (ALL A,B,C : EL.VON.S EL (PAAR (A B) RHO) AND EL (PAAR (B C) RHO) IMPL EL (PAAR (A C) RHO))) (* DEFINITION AEQUIVALENZRELATION) (TYPE EQU.REL (REL)) (ALL RHO : REL EQU.REL (RHO) EQV REFLEXIV (RHO) AND SYMMETRISCH (RHO) AND TRANSITIV (RHO)))) (QUOTE ((+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) COMMENT (+ 0 NIL) (ALL 6 (ALL 5 (ALL 7 (EQV (+ 2 (7 (4 5 6)) (KIND (EQV T 90))) (AND (+ 2 (7 5) (KIND (EQV NIL 90))) (+ 2 (7 6) (KIND (EQV NIL 90)))))))) COMMENT (+ 0 NIL) (ALL 9 (EQV (+ 8 (9) (KIND (EQV T 120))) (ALL 10 (+ 2 ((3 10 10) 9) (KIND (EQV NIL 120)))))) COMMENT (+ 0 NIL) (ALL 12 (EQV (+ 11 (12) (KIND (EQV T 150))) (ALL 14 (ALL 13 (IMPL (+ 2 ((3 13 14) 12) (KIND (EQV NIL 150))) (+ 2 ((3 14 13) 12) (KIND (EQV NIL 150)))))))) COMMENT (+ 0 NIL) (ALL 16 (EQV (+ 15 (16) (KIND (EQV T 180))) (ALL 19 (ALL 18 (ALL 17 (IMPL (AND (+ 2 ((3 17 18) 16) (KIND (EQV NIL 180))) (+ 2 ((3 18 19) 16) (KIND (EQV NIL 180)))) (+ 2 ((3 17 19) 16) (KIND (EQV NIL 180))))))))) COMMENT (+ 0 NIL) (ALL 21 (EQV (+ 20 (21) (KIND (EQV T 210))) (AND (+ 8 (21) (KIND (EQV NIL 210))) (AND (+ 11 (21) (KIND (EQV NIL 210))) (+ 15 (21) (KIND (EQV NIL 210))))))))) (QUOTE ((ALL RHO,SIGMA : REL EQU.REL (RHO) AND EQU.REL (SIGMA) IMPL TRANSITIV (DURCHSCHNITT (RHO SIGMA))))) (QUOTE ((ALL 23 (ALL 22 (IMPL (AND (+ 20 (22) NIL) (+ 20 (23) NIL)) (+ 15 ((4 22 23)) NIL)))))) (QUOTE ("Edit:     Axioms and Theorems edited: 27-SEP,1989 16:00 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(99796 99801 99806 99821 99826 99831 99836 99841 99856 99861 99866 99871 99886 99891
          99896 99911 99916 99921 99926 99940 99954 99969 99984 99999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "EL" NIL NIL (ELEMENT MENGE) NIL NIL
          ((- 2 (POSITIVE))) ((+ 2 (NEGATIVE))) ((+ 2 (NIL))) ((- 2 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 14 "PAAR" NIL ELEMENT (ELEMENT ELEMENT) NIL NIL NIL NIL
          NIL 2 ELEMENT (DT*ST-KIND NIL) FUNCTION 14 "DURCHSCHNITT" NIL REL (REL REL) NIL NIL
          NIL NIL NIL 2 REL (DT*ST-KIND NIL) FUNCTION 5 REL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ELEMENT NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "REFLEXIV" NIL
          NIL (REL) NIL NIL ((- 8 (POSITIVE))) ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 EL.VON.S NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "SYMMETRISCH"
          NIL NIL (REL) NIL NIL ((- 11 (POSITIVE))) ((+ 11 (NEGATIVE))) ((+ 11 (NIL)))
          ((- 11 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 REL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.VON.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.VON.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "TRANSITIV" NIL NIL (REL) NIL NIL
          ((- 15 (POSITIVE))) ((+ 15 (NEGATIVE))) ((+ 15 (NIL))) ((- 15 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5
          EL.VON.S NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.VON.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.VON.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "EQU.REL" NIL NIL (REL) NIL NIL
          ((- 20 (POSITIVE))) ((+ 20 (NEGATIVE))) ((+ 20 (NIL))) ((- 20 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 100000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 100000)
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
            (T (UNLESS (= MEM*SIZE 100000)
                 (MEM-INITIALIZE 100000))
               (SETQ COUNTER1 24)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 100000) (+ 99791 INCREMENT) 99791))
      (SETQ MEM*NEXT.VADR 24
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (EL.VON.S REL ELEMENT MENGE ANY))) (SETQ DT*SORT.NR (QUOTE 6)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE NIL)) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (4 3))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (20 15 11 8 2 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (20 15 11 8 2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*SORT.NUMBER)) (QUOTE 5)) (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (EL.VON.S))) (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EL.VON.S EL.VON.S) (REL) (ELEMENT EL.VON.S) (MENGE) (ANY EL.VON.S)))) (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (EL.VON.S))) (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ELEMENT))) (SETF (GET (QUOTE EL.VON.S) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY ELEMENT EL.VON.S)))) (PROGN (SETF (GET (QUOTE REL) (QUOTE DT*SORT.NUMBER)) (QUOTE 4)) (SETF (GET (QUOTE REL) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE REL) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (REL))) (SETF (GET (QUOTE REL) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EL.VON.S) (REL REL) (ELEMENT) (MENGE REL) (ANY REL)))) (SETF (GET (QUOTE REL) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (REL))) (SETF (GET (QUOTE REL) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (MENGE))) (SETF (GET (QUOTE REL) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY MENGE REL)))) (PROGN (SETF (GET (QUOTE ELEMENT) (QUOTE DT*SORT.NUMBER)) (QUOTE 3)) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (EL.VON.S))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ELEMENT EL.VON.S))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EL.VON.S EL.VON.S) (REL) (ELEMENT ELEMENT) (MENGE) (ANY ELEMENT)))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ELEMENT))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ELEMENT) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY ELEMENT)))) (PROGN (SETF (GET (QUOTE MENGE) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (REL))) (SETF (GET (QUOTE MENGE) (QUOTE DT*SORT.NUMBER)) (QUOTE 2)) (SETF (GET (QUOTE MENGE) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (MENGE REL))) (SETF (GET (QUOTE MENGE) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EL.VON.S) (REL REL) (ELEMENT) (MENGE MENGE) (ANY MENGE)))) (SETF (GET (QUOTE MENGE) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (REL))) (SETF (GET (QUOTE MENGE) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE MENGE) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY MENGE)))) (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (ELEMENT MENGE))) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (EL.VON.S ANY ELEMENT MENGE REL))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((EL.VON.S EL.VON.S) (REL REL) (ELEMENT ELEMENT) (MENGE MENGE) (ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ELEMENT REL))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))