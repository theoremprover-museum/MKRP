;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-  

#|
RED.I:CLAUSE.PURITY                   T
RED.I:CLAUSE.TAUTOLOGY                T
RED.I:CLAUSE.SUBSUMPTION              T
RED.I:CLAUSE.REPL.FACTORING           T
RED.I:CLAUSE.REPL.RESOLUTION          SIMPLE
RED.I:CLAUSE.REWRITING                T
RED.I:LINK.INCOMPATIBILITY            T
RED.I:LINK.TAUTOLOGY                  REMOVE-INHIBIT
RED.I:LINK.SUBSUMPTION                REMOVE-INHIBIT
 
 
RED.D:CLAUSE.PURITY                   REMOVE
RED.D:CLAUSE.TAUTOLOGY                REMOVE
RED.D:CLAUSE.TAUTOLOGY.RECHECK        T
RED.D:CLAUSE.SUBSUMPTION.FORWARD      REMOVE-INHIBIT
RED.D:CLAUSE.SUBSUMPTION.BACKWARD     REMOVE
RED.D:CLAUSE.SUBSUMPTION.RECHECK      T
RED.D:CLAUSE.REPL.FACTORING           T
RED.D:CLAUSE.REPL.FACTORING.RECHECK   T
RED.D:CLAUSE.REPL.RESOLUTION          SIMPLE
RED.D:CLAUSE.REPL.RESOLUTION.RECHECK  T
RED.D:LINK.INCOMPATIBILITY            REMOVE
RED.D:LINK.TAUTOLOGY                  REMOVE-INHIBIT
RED.D:LINK.TAUTOLOGY.RECHECK          T
RED.D:LINK.SUBSUMPTION                REMOVE-INHIBIT
RED.D:LINK.SUBSUMPTION.RECHECK        T
 
 
RED:SUB.CLAUSES.INITIAL               T
RED:SUB.CLAUSES.EACH.STEP             T
RED:SUB.INITIAL                       T
RED:SUB.EACH.STEP                     T
RED:PUR.CLAUSES.INITIAL               T
RED:PUR.CLAUSES.EACH.STEP             T
RED:PUR.INITIAL                       T
RED:PUR.EACH.STEP                     T
RED:TAU.CLAUSES.INITIAL               T
RED:TAU.CLAUSES.EACH.STEP             T
RED:TAU.INITIAL                       T
RED:TAU.EACH.STEP                     T
 
FAC:INITIAL                           NIL
FAC:EACH.STEP                         NIL
 
STR:RESOLUTION                        SET-OF-SUPPORT
STR:E-RESOLUTION                      NIL
STR:PARAMODULATION                    UNIT-ANCESTRY
STR:LINK.DEPTH                        NIL
STR:TERM.DEPTH                        NIL
STR:R.DEMODULATION                    T
STR:P.DEMODULATION                    T
 
 
TERM:UNITS                            T
TERM:ITERATIONS                       5
TERM:SET.OF.SUPPORT                   NIL
TERM:BREADTH.FIRST                    NIL
 
GEN:SPLITTING                         0
GEN:MANUAL.CONTROL                    NIL
GEN:SYSTEM.STRIP                      NIL
GEN:MAXIMUM.STEPS                     NIL
GEN:GRAPH.SAVING                      NIL
GEN:SAVE.FILE                         SAVE.DEFAULT
GEN:BATCH.ANSWER                      SC
 
 
TR:PREPROCESSING                      NIL
TR:STEP.MODE                          LR
TR:DUMP                               NIL
TR:CLAUSE.MODE                        I
TR:LINK.MODE                          I
TR:TRACE.FILE                         NIL
TR:TERMINAL                           NIL
 
 
PR:INFIX.FORM                         T
PR:PREFIX.FORM                        NIL
PR:OPTIONS                            T
PR:AXIOM.CLAUSES                      T
PR:SYMBOLS                            NIL
PR:STATISTICS                         NIL
PR:PROOF.FILE                         PR.DEU.1.12.INV.IS
PR:LEFT.MARGIN                        0
PR:LINELENGTH                         80
|# 



(SORT ELEMENT,MENGE,INDEX,INDEXMENGE,VERKN:ANY)
(TYPE S (INDEXMENGE) :MENGE)
(TYPE SM (MENGE MENGE) :MENGE)
(TYPE V (INDEXMENGE INDEX) : INDEXMENGE)
(TYPE U (INDEX) :MENGE)
(TYPE INVERS (ELEMENT VERKN) :ELEMENT)
(TYPE APPLY (VERKN ELEMENT ELEMENT) :ELEMENT)
(ALL ISTRICH,I:INDEXMENGE  ALL F:MENGE  ALL PHI:VERKN  ALL E:ELEMENT  
     ME (ISTRICH I) IMPL UG (S (ISTRICH) F PHI E))
(ALL K:INDEXMENGE ALL J:INDEX SM (S (K) U (J)) = S (V (K J)))
(ALL PHI:VERKN  ALL X,E:ELEMENT  ALL U1,U2,F:MENGE  
     UG (U1 F PHI E) AND UG (U2 F PHI E) AND EL (X SM (U1 U2))
     IMPL  
     GLEICH (APPLY (PHI X INVERS (X PHI)) E)
     AND  
     EL (INVERS (X PHI) SM (U1 U2)))

(ALL F:MENGE  ALL PHI:VERKN  ALL ISTRICH,I:INDEXMENGE  ALL J:INDEX  
     ALL X,E:ELEMENT  
     ME (ISTRICH I)
     AND  
     UG (U (J) F PHI E)
     AND  
     EL (X S (V (ISTRICH J)))
     IMPL  
     GLEICH (APPLY (PHI X INVERS (X PHI)) E)
     AND  
     EL (INVERS (X PHI) S (V (ISTRICH J))))