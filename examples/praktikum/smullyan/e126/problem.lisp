;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* SMULLYAN, CHAPTER 8, PROBLEM 126) (* SUPPOSE THERE ARE TWO NEIGHBORING ISLANDS EACH EXCLUSIVELY) (* INHABITED BY KNIGHTS AND KNAVES, THERE ARE NO NORMALS.) (ALL X KNIGHT (X) EQV NOT KNAVE (X)) (ALL X,Y KNIGHT (X) AND STAT (X Y) IMPL T (Y)) (ALL X,Y KNAVE (X) AND STAT (X Y) IMPL NOT T (Y)) (* YOU ARE TOLD THAT ON ONE OF THE TWO ISLANDS THERE IS AN EVEN) (* NUMBER OF KNIGHTS AND ON THE OTHER ONE THERE IS AN ODD NUMBER) (* OF KNIGHTS.) (ALL X EVEN (X) EQV NOT ODD (X)) (* YOU ARE ALSO TOLD THAT THERE IS GOLD ON THE ISLAND CONTAINING) (* THE EVEN NUMBER OF KNIGHTS, BUT THERE IS NO GOLD ON THE OTHER) (* ISLAND.) (EVEN (KNIGHTS) EQV THERE-IS-GOLD-ON-THE-ISLAND) (EVEN (KNAVES) AND EVEN (NATIVES) IMPL EVEN (KNIGHTS)) (ODD (KNAVES) AND ODD (NATIVES) IMPL EVEN (KNIGHTS)) (* YOU PICK ONE OF THE TWO ISLANDS AT RANDOM AND VISIT IT.) (* ALL THE INHABITANTS KNOW HOW MANY KNIGHTS AND HOW MANY KNAVES) (* LIVE ON THE ISLAND. YOU ARE INTERVIEWING THREE INHABITANTS,) (* A, B, C, AND THEY MAKE THE FOLLOWING STATEMENTS :) (* A : THERE IS AN EVEN NUMBER OF KNAVES ON THIS ISLAND.) (STAT (A EVENNUMBER (KNAVES))) (* B : RIGHT NOW, THERE IS AN ODD NUMBER OF PEOPLE ON THE ISLAND.) (STAT (B ODDNUMBER (PEOPLE))) (* C : I AM A KNIGHT IF AND ONLY IF A AND B ARE OF THE SAME TYPE.) (STAT (C KNIGHTIFFEQV)) (ALL X T (EVENNUMBER (X)) EQV EVEN (X)) (ALL X T (ODDNUMBER (X)) EQV ODD (X)) (T (KNIGHTIFFEQV) EQV KNIGHT (C) EQV (KNIGHT (A) EQV KNIGHT (B))) (* ASSUME THAT YOU ARE NEITHER A KNIGHT NOR A KNAVE AND THAT AT) (* THE MOMENT YOU ARE THE ONLY VISITOR ON THE ISLAND.) (ODD (PEOPLE) EQV EVEN (NATIVES)))) (QUOTE (COMMENT COMMENT COMMENT (ALL 2 (EQV (+ 3 (2) (KIND (EQV T 50))) (NOT (+ 4 (2) (KIND (EQV NIL 50)))))) (ALL 6 (ALL 5 (IMPL (AND (+ 3 (5) NIL) (+ 7 (5 6) NIL)) (+ 8 (6) NIL)))) (ALL 10 (ALL 9 (IMPL (AND (+ 4 (9) NIL) (+ 7 (9 10) NIL)) (NOT (+ 8 (10) NIL))))) COMMENT COMMENT COMMENT (ALL 11 (EQV (+ 12 (11) (KIND (EQV T 110))) (NOT (+ 13 (11) (KIND (EQV NIL 110)))))) COMMENT COMMENT COMMENT (EQV (+ 12 (14) (KIND (EQV T 150))) (+ 15 NIL (KIND (EQV NIL 150)))) (IMPL (AND (+ 12 (16) NIL) (+ 12 (17) NIL)) (+ 12 (14) NIL)) (IMPL (AND (+ 13 (16) NIL) (+ 13 (17) NIL)) (+ 12 (14) NIL)) COMMENT COMMENT COMMENT COMMENT COMMENT (+ 7 (18 (19 16)) NIL) COMMENT (+ 7 (20 (22 21)) NIL) COMMENT (+ 7 (23 24) NIL) (ALL 25 (EQV (+ 8 ((19 25)) (KIND (EQV T 280))) (+ 12 (25) (KIND (EQV NIL 280))))) (ALL 26 (EQV (+ 8 ((22 26)) (KIND (EQV T 290))) (+ 13 (26) (KIND (EQV NIL 290))))) (EQV (+ 8 (24) (KIND (EQV T 302))) (EQV (+ 3 (23) (KIND (EQV T 301 EQV NIL 302))) (EQV (+ 3 (18) (KIND (EQV T 300 EQV NIL 301 EQV NIL 302))) (+ 3 (20) (KIND (EQV NIL 300 EQV NIL 301 EQV NIL 302)))))) COMMENT COMMENT (EQV (+ 13 (21) (KIND (EQV T 330))) (+ 12 (17) (KIND (EQV NIL 330)))))) (QUOTE ((* IS THERE GOLD ON THE ISLAND OR NOT?) (NOT THERE-IS-GOLD-ON-THE-ISLAND))) (QUOTE (COMMENT (NOT (+ 15 NIL NIL)))) (QUOTE ("Edit:     Axioms and Theorems edited: 14-DEC,1989 19:29 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(39761 39766 39771 39776 39790 39795 39800 39814 39819 39824 39829 39844 39849 39864
          39879 39884 39889 39894 39909 39924 39929 39934 39949 39964 39969 39984 39999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "KNIGHT" NIL NIL (ANY) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE)))
          ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "KNAVE" NIL NIL
          (ANY) NIL NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL))) ((- 4 (NIL))) NIL
          NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "STAT" NIL NIL (ANY ANY) NIL
          NIL ((- 7 (POSITIVE))) ((+ 7 (NEGATIVE))) ((+ 7 (NIL))) ((- 7 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 15 "T" NIL NIL (ANY) NIL NIL ((- 8 (POSITIVE)))
          ((+ 8 (NEGATIVE))) ((+ 8 (NIL))) ((- 8 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 15 "EVEN" NIL NIL (ANY) NIL NIL ((- 12 (POSITIVE))) ((+ 12 (NEGATIVE)))
          ((+ 12 (NIL))) ((- 12 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "ODD" NIL NIL
          (ANY) NIL NIL ((- 13 (POSITIVE))) ((+ 13 (NEGATIVE))) ((+ 13 (NIL))) ((- 13 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY "KNIGHTS" (DT*ST-KIND NIL) CONSTANT 15
          "THERE-IS-GOLD-ON-THE-ISLAND" NIL NIL NIL NIL NIL ((- 15 (POSITIVE)))
          ((+ 15 (NEGATIVE))) ((+ 15 (NIL))) ((- 15 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
          5 ANY "KNAVES" (DT*ST-KIND NIL) CONSTANT 5 ANY "NATIVES" (DT*ST-KIND NIL) CONSTANT 5
          ANY "A" (DT*ST-KIND NIL) CONSTANT 14 "EVENNUMBER" NIL ANY (ANY) NIL NIL NIL NIL NIL
          1 ANY (DT*ST-KIND NIL) FUNCTION 5 ANY "B" (DT*ST-KIND NIL) CONSTANT 5 ANY "PEOPLE"
          (DT*ST-KIND NIL) CONSTANT 14 "ODDNUMBER" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY
          (DT*ST-KIND NIL) FUNCTION 5 ANY "C" (DT*ST-KIND NIL) CONSTANT 5 ANY "KNIGHTIFFEQV"
          (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 40000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 40000)
             (SETQ COUNTER1 27)
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
            (T (UNLESS (= MEM*SIZE 40000)
                 (MEM-INITIALIZE 40000))
               (SETQ COUNTER1 27)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 40000) (+ 39756 INCREMENT) 39756))
      (SETQ MEM*NEXT.VADR 27
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (ANY))) (SETQ DT*SORT.NR (QUOTE 2)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (24 23 21 20 18 17 16 14))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (22 19))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE NIL)) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (15 13 12 8 7 4 3 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (15 13 12 8 7 4 3 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))