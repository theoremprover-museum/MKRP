;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*- 
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
TERM:ITERATIONS                       3
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
PR:PROOF.FILE                         PR.DEU.1.15.RL.IA
PR:LEFT.MARGIN                        0
PR:LINELENGTH                         80
 
 
|#
(SORT MENGE,VERKN,INDEX:ANY)
(TYPE 1:INDEX)
(TYPE V (MENGE MENGE) :MENGE)
(TYPE KLUHG (MENGE MENGE VERKN) :MENGE)
(TYPE EHG (MENGE VERKN INDEX) :MENGE)
(ALL A,B:MENGE ALL PHI:VERKN ME (V (A B) KLUHG (A B PHI)))
(ALL E:MENGE ALL PHI:VERKN EHG (E PHI 1) = E)

(ALL A,B:MENGE ALL PHI:VERKN ME (EHG (V (A B) PHI 1) KLUHG (A
							     B PHI)))