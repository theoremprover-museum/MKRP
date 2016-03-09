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
PR:PROOF.FILE                         PR.DEU.1.15.LEMMA.IS
PR:LEFT.MARGIN                        0
PR:LINELENGTH                         80
 
 
|#
(SORT ELEMENT,MENGE,VERKN,INDEX:ANY)
(TYPE EHG (MENGE VERKN INDEX) :MENGE)
(TYPE PLUS (INDEX INDEX) :INDEX)
(TYPE APPLY (VERKN ELEMENT ELEMENT) :ELEMENT)
(TYPE 1,M:INDEX)
(ALL X,Y:ELEMENT  ALL E:MENGE  ALL N,NSTRICH:INDEX  ALL PHI:VERKN  
     KLEINERGLEICH (NSTRICH N)
     AND  
     EL (X EHG (E PHI M))
     AND  
     EL (Y EHG (E PHI NSTRICH))
     IMPL  
     EL (APPLY (PHI X Y) EHG (E PHI PLUS (M NSTRICH))))
(ALL K:INDEX  ALL X,Y:ELEMENT  ALL PHI:VERKN  ALL E:MENGE  
     EL (X EHG (E PHI K)) AND EL (Y EHG (E PHI 1))
     IMPL  
     EL (APPLY (PHI X Y) EHG (E PHI PLUS (K 1))))
(ALL K:INDEX  ALL X,Y,Z:ELEMENT  ALL PHI:VERKN  ALL E:MENGE  
     EL (X EHG (E PHI PLUS (K 1))) IMPL X = APPLY (PHI Y Z))
(ALL K:INDEX  ALL X,Y,Z:ELEMENT  ALL PHI:VERKN  ALL E:MENGE  
     EL (X EHG (E PHI PLUS (K 1))) AND X = APPLY (PHI Y Z)
     IMPL  
     EL (Y EHG (E PHI K)) AND EL (Z EHG (E PHI 1)))
(ALL PHI:VERKN  ALL X,Y,Z:ELEMENT  
     APPLY (PHI X APPLY (PHI Y Z)) = APPLY (PHI APPLY (PHI X Y)
						Z))

(ALL E,F:MENGE  ALL X,Y:ELEMENT  ALL PHI:VERKN  ALL N,NSTRICH:INDEX  
     HG (F PHI)
     AND  
     ME (E F)
     AND  
     EL (X EHG (E PHI M))
     AND  
     EL (Y EHG (E PHI PLUS (NSTRICH 1)))
     AND  
     KLEINERGLEICH (NSTRICH N)
     IMPL  
     EL (APPLY (PHI X Y) EHG (E PHI PLUS (PLUS (M NSTRICH) 1))))