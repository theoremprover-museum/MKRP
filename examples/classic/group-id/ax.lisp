(COND ((FMT-LOAD " 6-SEP-84 19:57:48" NIL (((TYPE SUBSET (SET SET)) (+ 1 NIL) TYPE) ((TYPE MEMBER (ELEMENT SET))
(+ 1 NIL) TYPE) ((TYPE GROUP (SET)) (+ 1 NIL) TYPE) ((TYPE ID (SET) :ELEMENT) (+ 1 NIL) TYPE) ((ALL X,Y:SET SUBSET
(X Y) IMPL (ALL Z:ELEMENT (MEMBER (Z X) IMPL MEMBER (Z Y)))) (ALL 8 (ALL 7 (IMPL (+ 3 (7 8) NIL) (ALL 9 (IMPL (+
4 (9 7) NIL) (+ 4 (9 8) NIL)))))) QUANTIFICATION) ((TYPE * (ELEMENT ELEMENT) :ELEMENT) (+ 1 NIL) TYPE) ((ASSOCIATIVE
(*)) (+ 1 NIL) PROPERTY) ((TYPE - (ELEMENT) :ELEMENT) (+ 1 NIL) TYPE) ((ALL G:SET ALL X:ELEMENT GROUP (G) AND MEMBER
(X G) IMPL * (- (X) X) = ID (G)) (ALL 12 (ALL 13 (IMPL (AND (+ 5 (12) NIL) (+ 4 (13 12) NIL)) (+ 14 ((10 (11 13)
13) (6 12)) NIL)))) QUANTIFICATION) ((ALL G:SET GROUP (G) IMPL MEMBER (ID (G) G)) (ALL 15 (IMPL (+ 5 (15) NIL)
(+ 4 ((6 15) 15) NIL))) QUANTIFICATION) ((TYPE A:SET) (+ 1 NIL) TYPE) ((TYPE B:SET) (+ 1 NIL) TYPE))) (COND (
ST*LOAD.FLAG (PROGN (PROGN (DT-RESET) (PROG ((ADDRLIST (QUOTE (10073 10078 10083 10098 10103 10108 10122 10136
10141 10146 10151 10165 10180 10195 10210 10225 10240))) (DATALIST (QUOTE (15 TRUE NIL NIL NIL (DEFINED) NIL ((-
1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 FALSE NIL NIL
NIL (DEFINED) NIL ((- 2 (POSITIVE))) ((+ 2 (NEGATIVE))) ((+ 2 (NIL))) ((- 2 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE
15 SUBSET NIL NIL (SET SET) NIL NIL ((- 3 (POSITIVE))) ((+ 3 (NEGATIVE))) ((+ 3 (NIL))) ((- 3 (NIL))) NIL NIL (
DT*ST-KIND NIL) PREDICATE 15 MEMBER NIL NIL (ELEMENT SET) NIL NIL ((- 4 (POSITIVE))) ((+ 4 (NEGATIVE))) ((+ 4 (NIL)))
((- 4 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 GROUP NIL NIL (SET) NIL NIL ((- 5 (POSITIVE))) ((+ 5 (NEGATIVE)))
((+ 5 (NIL))) ((- 5 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 14 ID NIL ELEMENT (SET) NIL NIL NIL NIL NIL 1 ELEMENT
(DT*ST-KIND NIL) FUNCTION 5 SET NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 SET NIL (ST*DATA NIL DT*ST-KIND
SYS-VAR) VARIABLE 5 ELEMENT NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 * (ASSOCIATIVE) ELEMENT (ELEMENT ELEMENT)
NIL NIL NIL NIL NIL 2 ELEMENT (DT*ST-KIND NIL) FUNCTION 14 - NIL ELEMENT (ELEMENT) NIL NIL NIL NIL NIL 1 ELEMENT
(DT*ST-KIND NIL) FUNCTION 5 SET NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ELEMENT NIL (ST*DATA NIL DT*ST-KIND
SYS-VAR) VARIABLE 15 = NIL NIL (ANY ANY) (DEFINED SYMMETRIC REFLEXIVE SYMMETRIC REFLEXIVE) NIL ((- 14 (POSITIVE
. SYMMETRIC))) ((+ 14 (NEGATIVE . SYMMETRIC))) ((+ 14 (NIL . SYMMETRIC))) ((- 14 (NIL . SYMMETRIC))) NIL NIL (DT*ST-KIND
NIL) PREDICATE 5 SET NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 A SET (DT*ST-KIND NIL) CONSTANT 5 B SET (DT*ST-KIND
NIL) CONSTANT))) (INCREMENT (IDIFFERENCE MEM*SIZE 10240)) COUNTER1) (MEM-RESET) (COND ((IGREATERP (IPLUS (IDIFFERENCE
18 1) (IDIFFERENCE 10240 10068)) MEM*SIZE) (MEM=SATURATION.ENTERED)) ((EQ MEM*SIZE 10240) (SETQ COUNTER1 18) (MAPC
ADDRLIST (FUNCTION (LAMBDA (RADDR) (SETA MEM*MEMORY (SETQ COUNTER1 (SUB1 COUNTER1)) RADDR)))) (SETQ COUNTER1 (ADD1
MEM*SIZE)) (MAPC DATALIST (FUNCTION (LAMBDA (DATA) (SETA MEM*MEMORY (SETQ COUNTER1 (SUB1 COUNTER1)) DATA))))) (T
(SETQ COUNTER1 18) (MAPC ADDRLIST (FUNCTION (LAMBDA (RADDR) (SETA MEM*MEMORY (SETQ COUNTER1 (SUB1 COUNTER1)) (COND
((EQ RADDR (QUOTE ATP.MEMORY.NIL)) RADDR) ((OR (EQ RADDR 0) (MINUSP RADDR)) RADDR) (T (IPLUS RADDR INCREMENT)))))))
(SETQ COUNTER1 (ADD1 MEM*SIZE)) (MAPC DATALIST (FUNCTION (LAMBDA (DATA) (SETA MEM*MEMORY (SETQ COUNTER1 (SUB1 COUNTER1))
DATA)))))) (RPAQ MEM*NEXT.RADR (IPLUS 10068 INCREMENT)) (RPAQ MEM*NEXT.VADR 18) (RPAQ MEM*REST (ADD1 (IDIFFERENCE
MEM*NEXT.RADR MEM*NEXT.VADR))) (RPAQ MEM*FIRST.REUSABLE.VADR NIL) (RPAQ MEM*LAST.REUSABLE.VADR NIL)) (PROGN (RPAQQ
DT*SORT.ALL (ELEMENT SET ANY)) (RPAQQ DT*SORT.NR 1) (RPAQQ DT*SORT.PROPERTIES (DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS
DT*TRANSITIVE.CLOSURE DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS DT*DIRECT.SUPERSORTS
DT*DISJOINT.SORTS DT*COMPLETION.SORT)) (RPAQQ DT*VARIABLE.COUNTER 0) (RPAQQ DT*CONSTANT.COUNTER 0) (RPAQQ 
DT*CONSTANT.ALL (17 16)) (RPAQQ DT*ABBREVIATIONS NIL) (RPAQQ DT*FUNCTION.COUNTER 0) (RPAQQ DT*FUNCTION.ALL (11
10 6)) (RPAQQ DT*FUNCTION.ADMISSIBLE.THEORIES (ASSOCIATIVE)) (RPAQQ DT*FUNCTION.ACTUAL.THEORIES (ASSOCIATIVE))
(RPAQQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES NIL) (RPAQQ DT*FUNCTION.COMPONENTS (PNAME ATTRIBUTES MAX.RANGE.SORT
MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))
(RPAQQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES (SYMMETRIC DEFINED REFLEXIVE)) (RPAQQ DT*PREDICATE.COUNTER 0) (RPAQQ
DT*EQUALITY.SYMBOLS (= := =: :=:)) (RPAQQ DT*EQUALITY.PREDICATES (14)) (RPAQQ DT*NONEQUALITY.PREDICATES (5 4 3
2 1)) (RPAQQ DT*PREDICATE.ALL (14 5 4 3 2 1)) (RPAQQ DT*PREDICATE.WITH.ATTRIBUTES NIL) (RPAQQ DT*PREDICATE.COMPONENTS
(PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES 
+SOTHERSIDES -SOTHERSIDES +TOTHERSIDES -TOTHERSIDES)) (RPAQQ DT*TRUE.PREDICATE 1) (RPAQQ DT*FALSE.PREDICATE 2)
(RPAQQ DT*UNI.CREATES.VARIABLES NIL) (RPAQQ DT*SIGN.MINUS.SYMBOLS (- --)) (RPAQQ DT*SIGN.PLUS.SYMBOLS (+ ++)))
(PROGN) (PROGN (SETPROPLIST (QUOTE DT*PREDICATE.WITH.ATTRIBUTES) (QUOTE (REFLEXIVE (14 14) SYMMETRIC (14 14) DEFINED
(14 2 1))))) (PROGN (SETPROPLIST (QUOTE ELEMENT) (QUOTE (DT*INVERSE.TRANSITIVE.CLOSURE (ELEMENT ANY) 
DT*DIRECT.SUPERSORTS (ANY) DT*MINIMAL.SUBSORTS (ELEMENT) DT*MAX.SUBSORTS ((ELEMENT ELEMENT) (SET) (ANY ELEMENT))
DT*TRANSITIVE.CLOSURE (ELEMENT) DT*DIRECT.SUBSORTS NIL))) (SETPROPLIST (QUOTE SET) (QUOTE (DT*INVERSE.TRANSITIVE.CLOSURE
(SET ANY) DT*DIRECT.SUPERSORTS (ANY) DT*MINIMAL.SUBSORTS (SET) DT*MAX.SUBSORTS ((ELEMENT) (SET SET) (ANY SET))
DT*TRANSITIVE.CLOSURE (SET) DT*DIRECT.SUBSORTS NIL))) (SETPROPLIST (QUOTE ANY) (QUOTE (DT*INVERSE.TRANSITIVE.CLOSURE
(ANY) DT*DIRECT.SUPERSORTS NIL DT*MINIMAL.SUBSORTS (SET) DT*MAX.SUBSORTS ((ELEMENT ELEMENT) (SET SET) (ANY ANY))
DT*TRANSITIVE.CLOSURE (ELEMENT SET ANY) DT*DIRECT.SUBSORTS (ELEMENT SET)))))) (RPAQQ ST*STACK1 (NIL ((B PROGN (
DT-CONSTANT.DELETE 17) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE B) ST*SYMBOL.ADDRESSES))) (SET 
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET PUTPROP (QUOTE SET) (QUOTE NIL))) ((A PROGN (
DT-CONSTANT.DELETE 16) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE A) ST*SYMBOL.ADDRESSES))) (SET 
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET PUTPROP (QUOTE SET) (QUOTE NIL))) ((X_15 PROGN
(DT-VARIABLE.DELETE 15) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE X_15) ST*SYMBOL.ADDRESSES))) (SET 
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET PUTPROP (QUOTE SET) (QUOTE NIL))) ((= PROGN (
DT-PREDICATE.DELETE 14) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE =) ST*SYMBOL.ADDRESSES))) (X_13 PROGN (
DT-VARIABLE.DELETE 13) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE X_13) ST*SYMBOL.ADDRESSES))) (ELEMENT 
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL)) (X_12
PROGN (DT-VARIABLE.DELETE 12) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE X_12) ST*SYMBOL.ADDRESSES))) (SET 
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET PUTPROP (QUOTE SET) (QUOTE NIL))) ((- PROGN (
DT-FUNCTION.DELETE 11) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE -) ST*SYMBOL.ADDRESSES))) (ELEMENT 
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL)) (ELEMENT
DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL)))
((* PROG1 NIL (DT-FUNCTION.CHANGE 10 (QUOTE ELEMENT) (QUOTE (ELEMENT ELEMENT)) NIL) (DT-FUNCTION.PUT.ATTRIBUTES
10 (QUOTE NIL))) (* PROG1 NIL (DT-PUTPROP 10 (QUOTE NIL)) (DT-PUTPROP 10 (QUOTE NIL)))) ((* PROGN (DT-FUNCTION.DELETE
10) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE *) ST*SYMBOL.ADDRESSES))) (ELEMENT DT-SORT.ST.PUT.DIRECT.SUPERSORTS
(QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL)) (ELEMENT DT-SORT.ST.PUT.DIRECT.SUPERSORTS
(QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL)) (ELEMENT DT-SORT.ST.PUT.DIRECT.SUPERSORTS
(QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL))) ((X_9 PROGN (DT-VARIABLE.DELETE 9)
(RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE X_9) ST*SYMBOL.ADDRESSES))) (ELEMENT DT-SORT.ST.PUT.DIRECT.SUPERSORTS
(QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT PUTPROP (QUOTE ELEMENT) (QUOTE NIL)) (X_8 PROGN (DT-VARIABLE.DELETE 8)
(RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE X_8) ST*SYMBOL.ADDRESSES))) (X_7 PROGN (DT-VARIABLE.DELETE 7) (RPAQ
ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE X_7) ST*SYMBOL.ADDRESSES))) (SET DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET)
(QUOTE (ANY))) (SET PUTPROP (QUOTE SET) (QUOTE NIL))) ((ID PROGN (DT-FUNCTION.DELETE 6) (RPAQ ST*SYMBOL.ADDRESSES
(REMASSOC (QUOTE ID) ST*SYMBOL.ADDRESSES))) (SET DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET
PUTPROP (QUOTE SET) (QUOTE NIL)) (ELEMENT DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE ELEMENT) (QUOTE (ANY))) (ELEMENT
PUTPROP (QUOTE ELEMENT) (QUOTE NIL))) ((GROUP PROGN (DT-PREDICATE.DELETE 5) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC
(QUOTE GROUP) ST*SYMBOL.ADDRESSES))) (SET DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET PUTPROP
(QUOTE SET) (QUOTE NIL))) ((MEMBER PROGN (DT-PREDICATE.DELETE 4) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE MEMBER)
ST*SYMBOL.ADDRESSES))) (SET DT-SORT.ST.PUT.DIRECT.SUPERSORTS (QUOTE SET) (QUOTE (ANY))) (SET PUTPROP (QUOTE SET)
(QUOTE NIL)) (ELEMENT PROGN (DT-SORT.ST.REMOVE (QUOTE ELEMENT)) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE ELEMENT)
ST*SYMBOL.ADDRESSES))) (ELEMENT REMPROP (QUOTE ELEMENT) (QUOTE ST*DATA))) ((SUBSET PROGN (DT-PREDICATE.DELETE 3)
(RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE SUBSET) ST*SYMBOL.ADDRESSES))) (SET DT-SORT.ST.PUT.DIRECT.SUPERSORTS
(QUOTE SET) (QUOTE (ANY))) (SET PUTPROP (QUOTE SET) (QUOTE NIL)) (SET PROGN (DT-SORT.ST.REMOVE (QUOTE SET)) (RPAQ
ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE SET) ST*SYMBOL.ADDRESSES))) (SET REMPROP (QUOTE SET) (QUOTE ST*DATA))) ((FALSE
PROGN (DT-PREDICATE.DELETE 2) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE FALSE) ST*SYMBOL.ADDRESSES))) (TRUE PROGN
(DT-PREDICATE.DELETE 1) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE TRUE) ST*SYMBOL.ADDRESSES))) (ANY PROGN (
DT-SORT.ST.REMOVE (QUOTE ANY)) (RPAQ ST*SYMBOL.ADDRESSES (REMASSOC (QUOTE ANY) ST*SYMBOL.ADDRESSES))) (ANY REMPROP
(QUOTE ANY) (QUOTE ST*DATA))))) (RPAQQ ST*SYMBOL.ADDRESSES (NIL (ANY . T) (TRUE . 1) (FALSE . 2) (SET . T) (SUBSET
. 3) (ELEMENT . T) (MEMBER . 4) (GROUP . 5) (ID . 6) (X_7 . 7) (X_8 . 8) (X_9 . 9) (* . 10) (- . 11) (X_12 . 12)
(X_13 . 13) (= . 14) (X_15 . 15) (A . 16) (B . 17))))))))STOP
 