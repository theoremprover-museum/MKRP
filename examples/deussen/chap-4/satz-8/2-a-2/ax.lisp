;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(COND ((FMT-LOAD "14-NOV-1987 02:07"
  NIL
  (((* ************************************************) COMMENT COMMENT)
 ((* DEFINITIONEN DER MENGENLEHRE) COMMENT COMMENT) ((SORT SET:ANY) (+ 0 NIL) TYPE)
 ((SORT ELEMENT:SET) (+ 0 NIL) TYPE) ((TYPE EL (ELEMENT SET)) (+ 0 NIL) TYPE)
 ((* DEFINITION 1.3: KARTESISCHES PRODUKT) COMMENT COMMENT)
 ((TYPE CARTES (SET SET) :SET) (+ 0 NIL) TYPE)
 ((TYPE PAIR (ELEMENT ELEMENT) :ELEMENT) (+ 0 NIL) TYPE) ((* SORTENPROBLEM!!!) COMMENT COMMENT)
 ((* DEFINITION 1.4: VEREINIGUNG) COMMENT COMMENT)
 ((TYPE UNIONDREI (REL REL REL) :REL) (+ 0 NIL) TYPE)
 ((TYPE UNIONZWEI (REL REL) :REL) (+ 0 NIL) TYPE) ((SORT REL:SET) (+ 0 NIL) TYPE)
 ((SORT EL.OF.S:ELEMENT) (+ 0 NIL) TYPE)
 ((ALL X,Y:REL ALL A,B:EL.OF.S EL (PAIR (A B) UNIONZWEI (X Y)) IMPL EL (PAIR (A B) X) OR EL
   (PAIR (A B) Y))
  (ALL 8
   (ALL 7
    (ALL 10
     (ALL 9
      (IMPL (+ 2 ((4 9 10) (6 7 8)) NIL)
       (OR (+ 2 ((4 9 10) 7) NIL)
           (+ 2 ((4 9 10) 8) NIL)))))))
  QUANTIFICATION)
 ((ALL X,Y,Z:REL ALL A,B:EL.OF.S EL (PAIR (A B) UNIONDREI (X Y Z)) IMPL EL (PAIR (A B) X) OR
   EL (PAIR (A B) Y) OR EL (PAIR (A B) Z))
  (ALL 13
   (ALL 12
    (ALL 11
     (ALL 15
      (ALL 14
       (IMPL (+ 2 ((4 14 15) (5 11 12 13)) NIL)
        (OR (+ 2 ((4 14 15) 11) NIL)
            (OR (+ 2 ((4 14 15) 12) NIL)
                (+ 2 ((4 14 15) 13) NIL)))))))))
  QUANTIFICATION)
 ((* ************************************************************************) COMMENT COMMENT)
 ((* DEFINITIONEN VON RELATIONEN AUF EINER FESTEN MENGE S) COMMENT COMMENT)
 ((TYPE S:SET) (+ 0 NIL) TYPE) ((* DEFINITION 2.3: IDENTISCHE RELATION) COMMENT COMMENT)
 ((TYPE IDENTITY:REL) (+ 0 NIL) TYPE) ((* DEFINITION 2.4: KONVERSE RELATION) COMMENT COMMENT)
 ((TYPE CONVERSE (REL) :REL) (+ 0 NIL) TYPE) ((* SORTENPROBLEM!!!) COMMENT COMMENT)
 ((ALL RHO:REL ALL A,B:EL.OF.S EL (PAIR (A B) CONVERSE (RHO)) EQV EL (PAIR (B A) RHO))
  (ALL 19
   (ALL 21
    (ALL 20
     (EQV (+ 2 ((4 20 21) (18 19)) (KIND (EQV T 260)))
      (+ 2 ((4 21 20) 19) (KIND (EQV NIL 260)))))))
  QUANTIFICATION)
 ((* SORTENPROBLEM) COMMENT COMMENT) ((* DEFINITION1.1: TEILMENGE) COMMENT COMMENT)
 ((TYPE SUBSET (REL REL)) (+ 0 NIL) TYPE)
 ((ALL X,Y:REL SUBSET (X Y) EQV (ALL A,B:EL.OF.S EL (PAIR (A B) X) IMPL EL (PAIR (A B) Y)))
  (ALL 24
   (ALL 23
    (EQV (+ 22 (23 24) (KIND (EQV T 300)))
     (ALL 26
      (ALL 25
       (IMPL (+ 2 ((4 25 26) 23) (KIND (EQV NIL 300)))
        (+ 2 ((4 25 26) 24) (KIND (EQV NIL 300)))))))))
  QUANTIFICATION)
 ((* DEFINITION 2.5: REFLEXIV) COMMENT COMMENT) ((TYPE REFLEXIV (REL)) (+ 0 NIL) TYPE)
 ((ALL RHO:REL REFLEXIV (RHO) EQV SUBSET (IDENTITY RHO))
  (ALL 28 (EQV (+ 27 (28) (KIND (EQV T 330))) (+ 22 (17 28) (KIND (EQV NIL 330)))))
  QUANTIFICATION)
 ((* DEFINITION 2.6: SYMMETRISCH) COMMENT COMMENT) ((TYPE SYMMETRISCH (REL)) (+ 0 NIL) TYPE)
 ((ALL RHO:REL SYMMETRISCH (RHO) EQV
   (ALL A,B:EL.OF.S EL (PAIR (A B) RHO) IMPL EL (PAIR (B A) RHO)))
  (ALL 30
   (EQV (+ 29 (30) (KIND (EQV T 360)))
    (ALL 32
     (ALL 31
      (IMPL (+ 2 ((4 31 32) 30) (KIND (EQV NIL 360))) (+ 2 ((4 32 31) 30) (KIND (EQV NIL 360)))
       )))))
  QUANTIFICATION)
 ((* DEFINITION 2.7: TRANSITIV) COMMENT COMMENT) ((TYPE TRANSITIV (REL)) (+ 0 NIL) TYPE)
 ((* DEFINITION 2.8: AEQUIVALENZRELATION) COMMENT COMMENT)
 ((SORT EQU.RELATION:REL) (+ 0 NIL) TYPE)
 ((ALL RHO:EQU.RELATION REFLEXIV (RHO) AND SYMMETRISCH (RHO) AND TRANSITIV (RHO))
  (ALL 34
   (AND (+ 27 (34) NIL)
        (AND (+ 29 (34) NIL)
             (+ 33 (34) NIL))))
  QUANTIFICATION)))(COND (ST*LOAD.FLAG (PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(39714 39729 39734 39739 39744 39759 39764 39779 39784 39789 39794 39799 39814 39819
          39824 39829 39843 39848 39853 39858 39863 39868 39873 39878 39883 39888 39893 39898
          39912 39926 39940 39954 39969 39984 39999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "EL" NIL NIL (ELEMENT SET) NIL NIL
          ((- 2 (POSITIVE))) ((+ 2 (NEGATIVE))) ((+ 2 (NIL))) ((- 2 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 14 "CARTES" NIL SET (SET SET) NIL NIL NIL NIL NIL 2 SET
          (DT*ST-KIND NIL) FUNCTION 14 "PAIR" NIL ELEMENT (ELEMENT ELEMENT) NIL NIL NIL NIL
          NIL 2 ELEMENT (DT*ST-KIND NIL) FUNCTION 14 "UNIONDREI" NIL REL (REL REL REL) NIL NIL
          NIL NIL NIL 3 REL (DT*ST-KIND NIL) FUNCTION 14 "UNIONZWEI" NIL REL (REL REL) NIL NIL
          NIL NIL NIL 2 REL (DT*ST-KIND NIL) FUNCTION 5 REL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 EL.OF.S NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 SET "S" (DT*ST-KIND NIL) CONSTANT 5 REL
          "IDENTITY" (DT*ST-KIND NIL) CONSTANT 14 "CONVERSE" NIL REL (REL) NIL NIL NIL NIL NIL
          1 REL (DT*ST-KIND NIL) FUNCTION 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE
          5 EL.OF.S NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "SUBSET" NIL NIL (REL REL) NIL NIL
          ((- 22 (POSITIVE))) ((+ 22 (NEGATIVE))) ((+ 22 (NIL))) ((- 22 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 REL
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "REFLEXIV" NIL NIL (REL) NIL NIL
          ((- 27 (POSITIVE))) ((+ 27 (NEGATIVE))) ((+ 27 (NIL))) ((- 27 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 REL NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15
          "SYMMETRISCH" NIL NIL (REL) NIL NIL ((- 29 (POSITIVE))) ((+ 29 (NEGATIVE)))
          ((+ 29 (NIL))) ((- 29 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 REL NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 EL.OF.S NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 15 "TRANSITIV" NIL NIL (REL) NIL NIL
          ((- 33 (POSITIVE))) ((+ 33 (NEGATIVE))) ((+ 33 (NIL))) ((- 33 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 EQU.RELATION NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE))
       (INCREMENT (- MEM*SIZE 40000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 40000)
             (SETQ COUNTER1 35)
             (MAPC #'(LAMBDA (RADDR)
                       (SETF (AREF MEM*MEMORY (DECF COUNTER1))
                              (COND ((EQL RADDR 'ATP.MEMORY.NIL) RADDR)
                                    ((OR (EQL RADDR 0)
                                         (MINUSP RADDR))
                                     RADDR)
                                    (T (+ RADDR INCREMENT)))))
                   ADDRLIST)
             (SETQ COUNTER1 MEM*SIZE)
             (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            (T (UNLESS (= MEM*SIZE 40000)
                 (MEM-INITIALIZE 40000))
               (SETQ COUNTER1 35)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (COND ((> MEM*SIZE 40000) (SETQ MEM*NEXT.RADR (+ 39709 INCREMENT)))
            (T (SETQ MEM*NEXT.RADR 39709)))
      (SETQ MEM*NEXT.VADR 35)
      (SETQ MEM*REST (1+ (- MEM*NEXT.RADR MEM*NEXT.VADR)))
      (SETQ MEM*FIRST.REUSABLE.VADR NIL)
      (SETQ MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL '(EQU.RELATION EL.OF.S REL ELEMENT SET ANY))
       (SETQ DT*SORT.NR '7)
       (SETQ DT*SORT.PROPERTIES
              '(DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE
                DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS
                DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))
       (SETQ DT*SORT.COMMON.COMPUTE.FLAG 'NIL)
       (SETQ DT*VARIABLE.COUNTER '0)
       (SETQ DT*CONSTANT.COUNTER '0)
       (SETQ DT*CONSTANT.ALL '(17 16))
       (SETQ DT*ABBREVIATIONS 'NIL)
       (SETQ DT*FUNCTION.COUNTER '0)
       (SETQ DT*FUNCTION.ALL '(18 6 5 4 3))
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
       (SETQ DT*NONEQUALITY.PREDICATES '(33 29 27 22 2 1 0))
       (SETQ DT*PREDICATE.ALL '(33 29 27 22 2 1 0))
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
(PROGN (PROGN (SETF (GET 'EQU.RELATION 'DT*SORT.NUMBER) '6)
              (SETF (GET 'EQU.RELATION 'DT*DIRECT.SUBSORTS) 'NIL)
              (SETF (GET 'EQU.RELATION 'DT*TRANSITIVE.CLOSURE) '(EQU.RELATION))
              (SETF (GET 'EQU.RELATION 'DT*MAX.SUBSORTS)
                     '((EQU.RELATION EQU.RELATION) (EL.OF.S) (REL EQU.RELATION) (ELEMENT)
                       (SET EQU.RELATION) (ANY EQU.RELATION)))
              (SETF (GET 'EQU.RELATION 'DT*MINIMAL.SUBSORTS) '(EQU.RELATION))
              (SETF (GET 'EQU.RELATION 'DT*DIRECT.SUPERSORTS) '(REL))
              (SETF (GET 'EQU.RELATION 'DT*INVERSE.TRANSITIVE.CLOSURE)
                     '(ANY SET REL EQU.RELATION)))
       (PROGN (SETF (GET 'EL.OF.S 'DT*SORT.NUMBER) '5)
              (SETF (GET 'EL.OF.S 'DT*DIRECT.SUBSORTS) 'NIL)
              (SETF (GET 'EL.OF.S 'DT*TRANSITIVE.CLOSURE) '(EL.OF.S))
              (SETF (GET 'EL.OF.S 'DT*MAX.SUBSORTS)
                     '((EQU.RELATION) (EL.OF.S EL.OF.S) (REL) (ELEMENT EL.OF.S) (SET EL.OF.S)
                       (ANY EL.OF.S)))
              (SETF (GET 'EL.OF.S 'DT*MINIMAL.SUBSORTS) '(EL.OF.S))
              (SETF (GET 'EL.OF.S 'DT*DIRECT.SUPERSORTS) '(ELEMENT))
              (SETF (GET 'EL.OF.S 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY SET ELEMENT EL.OF.S)))
       (PROGN (SETF (GET 'REL 'DT*SORT.NUMBER) '4)
              (SETF (GET 'REL 'DT*DIRECT.SUBSORTS) '(EQU.RELATION))
              (SETF (GET 'REL 'DT*TRANSITIVE.CLOSURE) '(REL EQU.RELATION))
              (SETF (GET 'REL 'DT*MAX.SUBSORTS)
                     '((EQU.RELATION EQU.RELATION) (EL.OF.S) (REL REL) (ELEMENT) (SET REL)
                       (ANY REL)))
              (SETF (GET 'REL 'DT*MINIMAL.SUBSORTS) '(REL))
              (SETF (GET 'REL 'DT*DIRECT.SUPERSORTS) '(SET))
              (SETF (GET 'REL 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY SET REL)))
       (PROGN (SETF (GET 'ELEMENT 'DT*SORT.NUMBER) '3)
              (SETF (GET 'ELEMENT 'DT*DIRECT.SUBSORTS) '(EL.OF.S))
              (SETF (GET 'ELEMENT 'DT*TRANSITIVE.CLOSURE) '(ELEMENT EL.OF.S))
              (SETF (GET 'ELEMENT 'DT*MAX.SUBSORTS)
                     '((EQU.RELATION) (EL.OF.S EL.OF.S) (REL) (ELEMENT ELEMENT) (SET ELEMENT)
                       (ANY ELEMENT)))
              (SETF (GET 'ELEMENT 'DT*MINIMAL.SUBSORTS) '(EL.OF.S))
              (SETF (GET 'ELEMENT 'DT*DIRECT.SUPERSORTS) '(SET))
              (SETF (GET 'ELEMENT 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY SET ELEMENT)))
       (PROGN (SETF (GET 'SET 'DT*SORT.NUMBER) '2)
              (SETF (GET 'SET 'DT*DIRECT.SUBSORTS) '(REL ELEMENT))
              (SETF (GET 'SET 'DT*TRANSITIVE.CLOSURE) '(EQU.RELATION SET REL ELEMENT EL.OF.S))
              (SETF (GET 'SET 'DT*MAX.SUBSORTS)
                     '((EQU.RELATION EQU.RELATION) (EL.OF.S EL.OF.S) (REL REL)
                       (ELEMENT ELEMENT) (SET SET) (ANY SET)))
              (SETF (GET 'SET 'DT*MINIMAL.SUBSORTS) '(REL EL.OF.S))
              (SETF (GET 'SET 'DT*DIRECT.SUPERSORTS) '(ANY))
              (SETF (GET 'SET 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY SET)))
       (PROGN (SETF (GET 'ANY 'DT*SORT.NUMBER) '1)
              (SETF (GET 'ANY 'DT*DIRECT.SUBSORTS) '(SET))
              (SETF (GET 'ANY 'DT*TRANSITIVE.CLOSURE)
                     '(EQU.RELATION ELEMENT REL ANY SET EL.OF.S))
              (SETF (GET 'ANY 'DT*MAX.SUBSORTS)
                     '((EQU.RELATION EQU.RELATION) (EL.OF.S EL.OF.S) (REL REL)
                       (ELEMENT ELEMENT) (SET SET) (ANY ANY)))
              (SETF (GET 'ANY 'DT*MINIMAL.SUBSORTS) '(EL.OF.S REL))
              (SETF (GET 'ANY 'DT*DIRECT.SUPERSORTS) 'NIL)
              (SETF (GET 'ANY 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY)))) (PROGN (SETQ ST*STACK1 ST*STACK1)
       (SETQ ST*SYMBOL.ADDRESSES
              '(NIL (ANY . T) (TRUE . 0) (FALSE . 1) (SET . T) (ELEMENT . T) (EL . 2)
                (CARTES . 3) (PAIR . 4) (REL . T) (UNIONDREI . 5) (UNIONZWEI . 6) (EL.OF.S . T)
                (X_7 . 7) (X_8 . 8) (X_9 . 9) (X_10 . 10) (X_11 . 11) (X_12 . 12) (X_13 . 13)
                (X_14 . 14) (X_15 . 15) (S . 16) (IDENTITY . 17) (CONVERSE . 18) (X_19 . 19)
                (X_20 . 20) (X_21 . 21) (SUBSET . 22) (X_23 . 23) (X_24 . 24) (X_25 . 25)
                (X_26 . 26) (REFLEXIV . 27) (X_28 . 28) (SYMMETRISCH . 29) (X_30 . 30)
                (X_31 . 31) (X_32 . 32) (TRANSITIV . 33) (EQU.RELATION . T) (X_34 . 34))))))))