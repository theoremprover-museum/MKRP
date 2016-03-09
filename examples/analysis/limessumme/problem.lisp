;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* TYPISIERTE VERSION) (* AXIOME) (* TYPENVEREINBARUNGEN) (SORT REAL,FUNKTION : ANY) (SORT POSREAL : ANY) (TYPE HALF (POSREAL) : POSREAL) (TYPE FUNPLUS (FUNKTION FUNKTION) : FUNKTION) (TYPE APPLY (FUNKTION REAL) : REAL) (TYPE KLEINERGLEICH (REAL REAL)) (TYPE POSKLEINERGLEICH (POSREAL POSREAL)) (TYPE ABS (REAL) : POSREAL) (TYPE PLUS (REAL REAL) : REAL) (TYPE POSPLUS (POSREAL POSREAL) : POSREAL) (TYPE MINUS (REAL REAL) : REAL) (TYPE LIMES (FUNKTION REAL REAL)) (TYPE F,G : FUNKTION) (* HILFSAXIOME) (* KLEINERGLEICH TRANSITIVITAET UND VERTRAEGLICHKEIT MIT MINIMUM) (ALL X,Y,Z : POSREAL POSKLEINERGLEICH (X Y) AND POSKLEINERGLEICH (Y Z) IMPL POSKLEINERGLEICH (X Z)) (* ARCHIMEDISCHE EIGENSCHAFT VON R) (ALL X : POSREAL EX Y : POSREAL POSKLEINERGLEICH (POSPLUS (Y Y) X)) (* TRANSITIVITAET VON PLUS BZGL. KLEINERGLEICH) (ALL X,Y,Z,U : POSREAL POSKLEINERGLEICH (X Y) AND POSKLEINERGLEICH (Z U) IMPL POSKLEINERGLEICH (POSPLUS (X Z) POSPLUS (Y U))) (* VERTRAEGLICHKEIT VON PLUS UND MINUS) (ALL A,B,C,D : REAL MINUS (PLUS (A B) PLUS (C D)) : = PLUS (MINUS (A C) MINUS (B D))) (* DREIECKSUNGLEICHUNG) (ALL X,Y : REAL POSKLEINERGLEICH (ABS (PLUS (X Y)) POSPLUS (ABS (X) ABS (Y)))) (* DEFINITION DER SUMMENFUNKTION) (ALL X : REAL APPLY (FUNPLUS (F G) X) = PLUS (APPLY (F X) APPLY (G X))) (* DEFINITION VON GRENZWERTEN) (ALL F : FUNKTION ALL X0,A : REAL LIMES (F X0 A) EQV (ALL EPSILON : POSREAL ALL X : REAL EX DELTA : POSREAL POSKLEINERGLEICH (ABS (MINUS (X X0)) DELTA) IMPL POSKLEINERGLEICH (ABS (MINUS (APPLY (F X) A)) EPSILON))))) (QUOTE (COMMENT COMMENT COMMENT (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) (+ 0 NIL) COMMENT COMMENT (ALL 16 (ALL 15 (ALL 14 (IMPL (AND (+ 6 (14 15) NIL) (+ 6 (15 16) NIL)) (+ 6 (14 16) NIL))))) COMMENT (ALL 17 (EX 18 (+ 6 ((9 18 18) 17) NIL))) COMMENT (ALL 22 (ALL 21 (ALL 20 (ALL 19 (IMPL (AND (+ 6 (19 20) NIL) (+ 6 (21 22) NIL)) (+ 6 ((9 19 21) (9 20 22)) NIL)))))) COMMENT (ALL 26 (ALL 25 (ALL 24 (ALL 23 (+ 27 ((10 (8 23 24) (8 25 26)) (8 (10 23 25) (10 24 26))) NIL))))) COMMENT (ALL 29 (ALL 28 (+ 6 ((7 (8 28 29)) (9 (7 28) (7 29))) NIL))) COMMENT (ALL 30 (+ 27 ((4 (3 12 13) 30) (8 (4 12 30) (4 13 30))) NIL)) COMMENT (ALL 31 (ALL 33 (ALL 32 (EQV (+ 11 (31 32 33) (KIND (EQV T 320))) (ALL 34 (ALL 35 (EX 36 (IMPL (+ 6 ((7 (10 35 32)) 36) (KIND (EQV NIL 320))) (+ 6 ((7 (10 (4 31 35) 33)) 34) (KIND (EQV NIL 320))))))))))))) (QUOTE ((* THEOREM) (* DIE GRENZWERT EINER SUMME IST DIE SUMME DER GRENZWERTE, WENN DIESE EXISTIEREN) (ALL A,B,X0 : REAL (LIMES (F X0 A) AND LIMES (G X0 B) IMPL LIMES (FUNPLUS (F G) X0 PLUS (A B)))))) (QUOTE (COMMENT COMMENT (ALL 39 (ALL 38 (ALL 37 (IMPL (AND (+ 11 (12 39 37) NIL) (+ 11 (13 39 38) NIL)) (+ 11 ((3 12 13) 39 (8 37 38)) NIL))))))) (QUOTE ("Edit:     Axioms and Theorems edited: 27-SEP,1989 16:44 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(99681 99686 99691 99696 99701 99706 99711 99716 99721 99726 99731 99736 99751 99756
          99761 99766 99771 99776 99781 99786 99791 99796 99801 99806 99811 99816 99821 99826
          99841 99855 99869 99883 99897 99912 99927 99941 99955 99969 99984 99999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 14 "HALF" NIL POSREAL (POSREAL) NIL NIL NIL NIL
          NIL 1 POSREAL (DT*ST-KIND NIL) FUNCTION 14 "FUNPLUS" NIL FUNKTION (FUNKTION FUNKTION)
          NIL NIL NIL NIL NIL 2 FUNKTION (DT*ST-KIND NIL) FUNCTION 14 "APPLY" NIL REAL
          (FUNKTION REAL) NIL NIL NIL NIL NIL 2 REAL (DT*ST-KIND NIL) FUNCTION 15
          "KLEINERGLEICH" NIL NIL (REAL REAL) NIL NIL ((- 5 (POSITIVE))) ((+ 5 (NEGATIVE)))
          ((+ 5 (NIL))) ((- 5 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "POSKLEINERGLEICH"
          NIL NIL (POSREAL POSREAL) NIL NIL ((- 6 (POSITIVE))) ((+ 6 (NEGATIVE))) ((+ 6 (NIL)))
          ((- 6 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 14 "ABS" NIL POSREAL (REAL) NIL NIL
          NIL NIL NIL 1 POSREAL (DT*ST-KIND NIL) FUNCTION 14 "PLUS" NIL REAL (REAL REAL) NIL
          NIL NIL NIL NIL 2 REAL (DT*ST-KIND NIL) FUNCTION 14 "POSPLUS" NIL POSREAL
          (POSREAL POSREAL) NIL NIL NIL NIL NIL 2 POSREAL (DT*ST-KIND NIL) FUNCTION 14 "MINUS"
          NIL REAL (REAL REAL) NIL NIL NIL NIL NIL 2 REAL (DT*ST-KIND NIL) FUNCTION 15 "LIMES"
          NIL NIL (FUNKTION REAL REAL) NIL NIL ((- 11 (POSITIVE))) ((+ 11 (NEGATIVE)))
          ((+ 11 (NIL))) ((- 11 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 FUNKTION "F"
          (DT*ST-KIND NIL) CONSTANT 5 FUNKTION "G" (DT*ST-KIND NIL) CONSTANT 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "=" NIL NIL (ANY ANY) (DEFINED SYMMETRIC REFLEXIVE) NIL
          ((- 27 (POSITIVE . SYMMETRIC))) ((+ 27 (NEGATIVE . SYMMETRIC)))
          ((+ 27 (NIL . SYMMETRIC))) ((- 27 (NIL . SYMMETRIC))) NIL NIL (DT*ST-KIND NIL)
          PREDICATE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 FUNKTION NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 POSREAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 POSREAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 REAL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REAL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 100000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 100000)
             (SETQ COUNTER1 40)
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
               (SETQ COUNTER1 40)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 100000) (+ 99676 INCREMENT) 99676))
      (SETQ MEM*NEXT.VADR 40
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (POSREAL FUNKTION REAL ANY))) (SETQ DT*SORT.NR (QUOTE 5)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (13 12))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (10 9 8 7 4 3 2))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (27))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (11 6 5 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (27 11 6 5 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE POSREAL) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE POSREAL) (QUOTE DT*SORT.NUMBER)) (QUOTE 4)) (SETF (GET (QUOTE POSREAL) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (POSREAL))) (SETF (GET (QUOTE POSREAL) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((POSREAL POSREAL) (FUNKTION) (REAL) (ANY POSREAL)))) (SETF (GET (QUOTE POSREAL) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (POSREAL))) (SETF (GET (QUOTE POSREAL) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE POSREAL) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY POSREAL)))) (PROGN (SETF (GET (QUOTE FUNKTION) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE FUNKTION) (QUOTE DT*SORT.NUMBER)) (QUOTE 3)) (SETF (GET (QUOTE FUNKTION) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (FUNKTION))) (SETF (GET (QUOTE FUNKTION) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((POSREAL) (FUNKTION FUNKTION) (REAL) (ANY FUNKTION)))) (SETF (GET (QUOTE FUNKTION) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (FUNKTION))) (SETF (GET (QUOTE FUNKTION) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE FUNKTION) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY FUNKTION)))) (PROGN (SETF (GET (QUOTE REAL) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE REAL) (QUOTE DT*SORT.NUMBER)) (QUOTE 2)) (SETF (GET (QUOTE REAL) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (REAL))) (SETF (GET (QUOTE REAL) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((POSREAL) (FUNKTION) (REAL REAL) (ANY REAL)))) (SETF (GET (QUOTE REAL) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (REAL))) (SETF (GET (QUOTE REAL) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE REAL) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY REAL)))) (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (POSREAL FUNKTION REAL))) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY POSREAL FUNKTION REAL))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((POSREAL POSREAL) (FUNKTION FUNKTION) (REAL REAL) (ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (FUNKTION REAL))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))