;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION (QUOTE ((* IN SHAKESPEARE^S MERCHANT OF VENICE PORTIA HAD THREE CASKETS) (* HE WAS LUCKY ENOUGH (OR WISE ENOUGH) TO CHOOSE THE ONE WITH) (* THE PORTRAIT, THEN HE COULD CLAIM PORTIA AS HIS BRIDE.) (* ON THE LID OF EACH CASKET WAS AN INSCRIPTION TO HELP THE SUITOR) (* CHOOSE WISELY.) (* NOW SUPPOSE PORTIA WANTED TO CHOOSE HER HUSBAND NOT ON THE BASIS) (* OF VIRTUE, BUT SIMPLY ON THE BASIS OF INTELLIGENCE. SHE HAD THE) (* FOLLOWING INSCRIPTIONS PUT ON THE CASKETS :) (*) (* GOLD : THE PORTRAIT IS IN THIS CASKET.) (* SILVER : THE PORTRAIT IS NOT IN THIS CASKET.) (* LEAD : THE PORTRAIT IS NOT IN THE GOLD CASKET.) (*) (* PORTIA EXPLAINED TO THE SUITOR THAT OF THE THREE STATEMENTS AT) (* MOST ONE WAS TRUE.) (* WHICH CASKET SHOULD THE SUITOR CHOOSE ?) (*) (* WE DEFINE THE FOLLOWING PREDICATES :) (* PORTRAIT (X) --- THE PORTRAIT IS IN CASKET X) (* TRUTH (X) --- THE INSCRIPTION X IS TRUE) (* INSCR.PORTRAIT (X Y) --- THE INSCRIPTION X TELLS THAT) (* THE PORTRAIT IS IN CASKET Y) (* NEG.INSCR.PORTRAIT (X Y) --- THE INSCRIPTION X TELLS THAT) (* THE PORTRAIT IS NOT IN CASKET Y) (*) (* DECLARATIONS :) (TYPE GOLD,SILVER,LEAD : CASKET) (TYPE INSCRIPTION (CASKET) : INSCRIPT) (TYPE PORTRAIT (CASKET)) (TYPE TRUTH (INSCRIPT)) (TYPE INSCR.PORTRAIT (INSCRIPT CASKET)) (TYPE NEG.INSCR.PORTRAIT (INSCRIPT CASKET)) (*) (* THEN THE FOLLOWING PROPOSITIONS HOLD.) (*) (* THERE IS EXACTLY ONE PORTRAIT :) (PORTRAIT (GOLD) OR PORTRAIT (SILVER) OR PORTRAIT (LEAD)) (NOT (PORTRAIT (GOLD) AND PORTRAIT (SILVER) OR PORTRAIT (SILVER) AND PORTRAIT (LEAD) OR PORTRAIT (LEAD) AND PORTRAIT (GOLD))) (*) (* AT MOST ONE INSCRIPTION TELLS THE TRUTH :) (NOT (TRUTH (INSCRIPTION (GOLD)) AND TRUTH (INSCRIPTION (SILVER)) OR TRUTH (INSCRIPTION (SILVER)) AND TRUTH (INSCRIPTION (LEAD)) OR TRUTH (INSCRIPTION (LEAD)) AND TRUTH (INSCRIPTION (GOLD)))) (*) (* IMPLICATIONS TO BE DRAWN FROM INSCRIPTION :) (ALL X : INSCRIPT ALL Y : CASKET INSCR.PORTRAIT (X Y) AND TRUTH (X) IMPL PORTRAIT (Y)) (ALL X : INSCRIPT ALL Y : CASKET INSCR.PORTRAIT (X Y) AND NOT TRUTH (X) IMPL NOT PORTRAIT (Y)) (ALL X : INSCRIPT ALL Y : CASKET NEG.INSCR.PORTRAIT (X Y) AND TRUTH (X) IMPL NOT PORTRAIT (Y)) (ALL X : INSCRIPT ALL Y : CASKET NEG.INSCR.PORTRAIT (X Y) AND NOT TRUTH (X) IMPL PORTRAIT (Y)) (*) (* ACTUAL INSCRIPTIONS :) (INSCR.PORTRAIT (INSCRIPTION (GOLD) GOLD)) (NEG.INSCR.PORTRAIT (INSCRIPTION (SILVER) SILVER)) (NEG.INSCR.PORTRAIT (INSCRIPTION (LEAD) GOLD)))) (QUOTE (COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT COMMENT (+ 0 NIL NIL) (+ 0 NIL NIL) (+ 0 NIL NIL) (+ 0 NIL NIL) (+ 0 NIL NIL) (+ 0 NIL NIL) COMMENT COMMENT COMMENT COMMENT (OR (+ 7 (3) NIL) (OR (+ 7 (4) NIL) (+ 7 (5) NIL))) (NOT (OR (AND (+ 7 (3) NIL) (+ 7 (4) NIL)) (OR (AND (+ 7 (4) NIL) (+ 7 (5) NIL)) (AND (+ 7 (5) NIL) (+ 7 (3) NIL))))) COMMENT COMMENT (NOT (OR (AND (+ 8 ((6 3)) NIL) (+ 8 ((6 4)) NIL)) (OR (AND (+ 8 ((6 4)) NIL) (+ 8 ((6 5)) NIL)) (AND (+ 8 ((6 5)) NIL) (+ 8 ((6 3)) NIL))))) COMMENT COMMENT (ALL 11 (ALL 12 (IMPL (AND (+ 9 (11 12) NIL) (+ 8 (11) NIL)) (+ 7 (12) NIL)))) (ALL 13 (ALL 14 (IMPL (AND (+ 9 (13 14) NIL) (NOT (+ 8 (13) NIL))) (NOT (+ 7 (14) NIL))))) (ALL 15 (ALL 16 (IMPL (AND (+ 10 (15 16) NIL) (+ 8 (15) NIL)) (NOT (+ 7 (16) NIL))))) (ALL 17 (ALL 18 (IMPL (AND (+ 10 (17 18) NIL) (NOT (+ 8 (17) NIL))) (+ 7 (18) NIL)))) COMMENT COMMENT (+ 9 ((6 3) 3) NIL) (+ 10 ((6 4) 4) NIL) (+ 10 ((6 5) 3) NIL))) (QUOTE ((PORTRAIT (SILVER)))) (QUOTE ((+ 7 (4) NIL))) (QUOTE ("Edit:     Axioms and Theorems edited: 09-JUL,1991 10:53 ")) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9830 9835 9840 9845 9850 9855 9860 9865 9880 9895 9910 9925 9939 9944 9949 9954 9969
          9984 9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "=" NIL NIL (ANY ANY)
          (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) NIL ((- 2 (POSITIVE . SYMMETRIC)))
          ((+ 2 (NEGATIVE . SYMMETRIC))) ((+ 2 (NIL . SYMMETRIC))) ((- 2 (NIL . SYMMETRIC)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 CASKET "GOLD" (DT*ST-KIND NIL) CONSTANT 5
          CASKET "SILVER" (DT*ST-KIND NIL) CONSTANT 5 CASKET "LEAD" (DT*ST-KIND NIL) CONSTANT
          14 "INSCRIPTION" NIL INSCRIPT (CASKET) NIL NIL NIL NIL NIL 1 INSCRIPT
          (DT*ST-KIND NIL) FUNCTION 15 "PORTRAIT" NIL NIL (CASKET) NIL NIL ((- 7 (POSITIVE)))
          ((+ 7 (NEGATIVE))) ((+ 7 (NIL))) ((- 7 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15
          "TRUTH" NIL NIL (INSCRIPT) NIL NIL ((- 8 (POSITIVE))) ((+ 8 (NEGATIVE)))
          ((+ 8 (NIL))) ((- 8 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "INSCR.PORTRAIT"
          NIL NIL (INSCRIPT CASKET) NIL NIL ((- 9 (POSITIVE))) ((+ 9 (NEGATIVE))) ((+ 9 (NIL)))
          ((- 9 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "NEG.INSCR.PORTRAIT" NIL NIL
          (INSCRIPT CASKET) NIL NIL ((- 10 (POSITIVE))) ((+ 10 (NEGATIVE))) ((+ 10 (NIL)))
          ((- 10 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5 INSCRIPT NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 CASKET NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 INSCRIPT NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 CASKET NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 INSCRIPT NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 CASKET NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 INSCRIPT NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 CASKET NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE))
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
             (SETQ COUNTER1 19)
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
               (SETQ COUNTER1 19)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9825 INCREMENT) 9825))
      (SETQ MEM*NEXT.VADR 19
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL (QUOTE (INSCRIPT CASKET ANY))) (SETQ DT*SORT.NR (QUOTE 4)) (SETQ DT*SORT.PROPERTIES (QUOTE (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))) (SETQ DT*SORT.COMMON.COMPUTE.FLAG (QUOTE NIL)) (SETQ DT*ELEMENT.PREDICATE (QUOTE NIL)) (SETQ DT*OMEGA.CONSTANT (QUOTE NIL)) (SETQ DT*VARIABLE.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.COUNTER (QUOTE 0)) (SETQ DT*CONSTANT.ALL (QUOTE (5 4 3))) (SETQ DT*ABBREVIATIONS (QUOTE NIL)) (SETQ DT*FUNCTION.COUNTER (QUOTE 0)) (SETQ DT*FUNCTION.ALL (QUOTE (6))) (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES (QUOTE (ASSOCIATIVE))) (SETQ DT*FUNCTION.ACTUAL.THEORIES (QUOTE NIL)) (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES (QUOTE NIL)) (SETQ DT*FUNCTION.COMPONENTS (QUOTE (PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))) (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (QUOTE (SYMMETRIC DEFINED REFLEXIVE))) (SETQ DT*PREDICATE.COUNTER (QUOTE 0)) (SETQ DT*EQUALITY.SYMBOLS (QUOTE ("=" ":=" "=:" ":=:"))) (SETQ DT*EQUALITY.PREDICATES (QUOTE (2))) (SETQ DT*NONEQUALITY.PREDICATES (QUOTE (10 9 8 7 1 0))) (SETQ DT*PREDICATE.ALL (QUOTE (10 9 8 7 2 1 0))) (SETQ DT*PREDICATE.WITH.ATTRIBUTES (QUOTE NIL)) (SETQ DT*PREDICATE.COMPONENTS (QUOTE (PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES))) (SETQ DT*TRUE.PREDICATE (QUOTE 0)) (SETQ DT*FALSE.PREDICATE (QUOTE 1)) (SETQ DT*UNI.CREATES.VARIABLES (QUOTE NIL)) (SETQ DT*SIGN.MINUS.SYMBOLS (QUOTE (- --))) (SETQ DT*SIGN.PLUS.SYMBOLS (QUOTE (+ ++))) (SETQ DT*SYMBOL.KINDS (QUOTE (CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*SORT.NUMBER)) (QUOTE 3)) (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (INSCRIPT))) (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((INSCRIPT INSCRIPT) (CASKET) (ANY INSCRIPT)))) (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (INSCRIPT))) (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE INSCRIPT) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY INSCRIPT)))) (PROGN (SETF (GET (QUOTE CASKET) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE CASKET) (QUOTE DT*SORT.NUMBER)) (QUOTE 2)) (SETF (GET (QUOTE CASKET) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (CASKET))) (SETF (GET (QUOTE CASKET) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((INSCRIPT) (CASKET CASKET) (ANY CASKET)))) (SETF (GET (QUOTE CASKET) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (CASKET))) (SETF (GET (QUOTE CASKET) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE (ANY))) (SETF (GET (QUOTE CASKET) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY CASKET)))) (PROGN (SETF (GET (QUOTE ANY) (QUOTE DT*SORT.NUMBER)) (QUOTE 1)) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUBSORTS)) (QUOTE (INSCRIPT CASKET))) (SETF (GET (QUOTE ANY) (QUOTE DT*TRANSITIVE.CLOSURE)) (QUOTE (ANY INSCRIPT CASKET))) (SETF (GET (QUOTE ANY) (QUOTE DT*MAX.SUBSORTS)) (QUOTE ((INSCRIPT INSCRIPT) (CASKET CASKET) (ANY ANY)))) (SETF (GET (QUOTE ANY) (QUOTE DT*MINIMAL.SUBSORTS)) (QUOTE (CASKET))) (SETF (GET (QUOTE ANY) (QUOTE DT*DIRECT.SUPERSORTS)) (QUOTE NIL)) (SETF (GET (QUOTE ANY) (QUOTE DT*INVERSE.TRANSITIVE.CLOSURE)) (QUOTE (ANY))))) ))