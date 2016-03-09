;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
#|


TWO:RULES                             NIL                   TWO:RULES.MAXLEVEL                    1.
TWO:SUPPRESS.NORULES                  NIL


RED.I:CLAUSE.MULTIPLE.LITERALS        T                     RED.I:CLAUSE.PURITY                   T
RED.I:CLAUSE.TAUTOLOGY                T                     RED.I:CLAUSE.TAUTOLOGY.RECHECK        PARTIAL
RED.I:CLAUSE.SUBSUMPTION              T                     RED.I:CLAUSE.SUBSUMPTION.RECHECK      PARTIAL
RED.I:CLAUSE.REPL.FACTORING           T                     RED.I:CLAUSE.REPL.FACTORING.RECHECK   T
RED.I:CLAUSE.REPL.RESOLUTION          SIMPLE                RED.I:CLAUSE.REPL.RESOLUTION.RECHECK  T
RED.I:CLAUSE.REWRITING                T                     RED.I:LINK.INCOMPATIBILITY            T
RED.I:LINK.TAUTOLOGY                  REMOVE-INHIBIT        RED.I:LINK.TAUTOLOGY.RECHECK          NIL
RED.I:LINK.SUBSUMPTION                REMOVE-INHIBIT        RED.I:LINK.SUBSUMPTION.RECHECK        NIL

RED.D:CLAUSE.MULTIPLE.LITERALS        T                     RED.D:CLAUSE.PURITY                   T
RED.D:CLAUSE.TAUTOLOGY                REMOVE-INHIBIT        RED.D:CLAUSE.TAUTOLOGY.RECHECK        NIL
RED.D:CLAUSE.SUBSUMPTION.FORWARD      REMOVE-INHIBIT        RED.D:CLAUSE.SUBSUMPTION.BACKWARD     REMOVE
RED.D:CLAUSE.SUBSUMPTION.RECHECK      NIL                   RED.D:CLAUSE.REPL.FACTORING           T
RED.D:CLAUSE.REPL.FACTORING.RECHECK   T                     RED.D:CLAUSE.REPL.RESOLUTION          SIMPLE
RED.D:CLAUSE.REPL.RESOLUTION.RECHECK  T                     RED.D:LINK.INCOMPATIBILITY            T
RED.D:LINK.TAUTOLOGY                  REMOVE-INHIBIT        RED.D:LINK.TAUTOLOGY.RECHECK          NIL
RED.D:LINK.SUBSUMPTION                REMOVE-INHIBIT        RED.D:LINK.SUBSUMPTION.RECHECK        NIL

FAC:INITIAL                           NIL                   FAC:EACH.STEP                         NIL

STR:RESOLUTION                        SET-OF-SUPPORT        STR:E-RESOLUTION                      NIL
STR:PARAMODULATION                    UNIT-ANCESTRY         STR:LINK.DEPTH                        NIL
STR:TERM.DEPTH                        NIL                   STR:R.DEMODULATION                    T
STR:P.DEMODULATION                    T                     STR:INDUCTION                         NIL

TERM:UNITS                            T                     TERM:ITERATIONS                       0.
TERM:SET.OF.SUPPORT                   NIL                   TERM:BREADTH.FIRST                    NIL

GEN:SPLITTING                         0.                    GEN:MANUAL.CONTROL                    NIL
GEN:MAXIMUM.STEPS                     NIL                   GEN:GRAPH.SAVING                      NIL
GEN:SAVE.FILE                         SAVE.DEFAULT


TR:PREPROCESSING                      NIL                   TR:STEP.MODE                          LR
TR:DUMP                               NIL                   TR:CLAUSE.MODE                        I
TR:LINK.MODE                          I                     TR:TRACE.FILE                         NIL
TR:TERMINAL                           T


PR:INFIX.FORM                         T                     PR:PREFIX.FORM                        NIL
PR:OPTIONS                            T                     PR:AXIOM.CLAUSES                      NIL
PR:SYMBOLS                            NIL                   PR:STATISTICS                         NIL
PR:PROTOCOL                           T                     PR:LEFT.MARGIN                        0.
PR:LINELENGTH                         120.

|#

(SORT MENGE,VERKN,ELEMENT,INDEX,INDEXMENGE:ANY)
(TYPE LIDEAL (MENGE MENGE VERKN))
(TYPE RIDEAL (MENGE MENGE VERKN))
(TYPE UHG (MENGE MENGE VERKN))
(TYPE ME (MENGE MENGE))
(TYPE NICHTLEER (MENGE))
(TYPE HG (MENGE VERKN))
(TYPE EL (ELEMENT MENGE))
(TYPE MINIMAL (MENGE MENGE VERKN))
(TYPE MPROD (MENGE MENGE VERKN) :MENGE)
(TYPE APPLYV (VERKN ELEMENT ELEMENT) :ELEMENT)
(TYPE F*X (MENGE ELEMENT VERKN) :MENGE)
(TYPE SM (MENGE MENGE) :MENGE)
(TYPE SET (ELEMENT) :MENGE)
(TYPE VER (MENGE MENGE) :MENGE)
(TYPE GR (MENGE VERKN ELEMENT))
(TYPE INVERS (VERKN ELEMENT) :ELEMENT)
(TYPE MINIMAL-BED-L (MENGE VERKN))
(TYPE MINIMAL-BED-R (MENGE VERKN))
(TYPE EL-IND (INDEX INDEXMENGE))
(TYPE U (INDEX) : MENGE)
(TYPE E (INDEX INDEX) : ELEMENT)
(TYPE NICHTLEER-IND (INDEXMENGE))
(TYPE GLEICH (MENGE MENGE))
(TYPE VER-IND (INDEXMENGE) : MENGE)
(TYPE SM-IND (INDEXMENGE) : MENGE)
(TYPE IDEAL (MENGE MENGE VERKN))
(* RECHENREGEL FUER SM *)
(ALL L1,L2,R1,R2:MENGE SM (SM (R1 L1) SM (R2 L2)) = SM (SM (L1 L2) SM (R1 R2)))
(* LEER + SCHNITTMENGE *)
(ALL M1,M2:MENGE LEER (M1) IMPL LEER (SM (M1 M2)) AND LEER (SM (M2 M1)))
(* EIGENSCHAFT VON SCHNITTMENGEN *)
(ALL M1,M2:MENGE ME (SM (M1 M2) M1) AND ME (SM (M1 M2) M2))
(* DEFINITION VON MINIMAL *)
(ALL L1,L,F:MENGE  ALL MAL:VERKN  
     LIDEAL (L F MAL) AND MINIMAL (L F MAL) AND ME (L1 L) AND LIDEAL (L1 F MAL) IMPL L1 = L)
(ALL A,B:MENGE SM (A B) = A IMPL ME (A B))
(* SATZ 332. *)
(ALL L1,L2,F:MENGE  ALL MAL:VERKN  
     LIDEAL (L2 F MAL) AND LIDEAL (L1 F MAL) IMPL (LIDEAL (SM (L1 L2) F MAL) OR LEER (SM (L1 L2))))

(ALL L1,L2,R1,R2,F:MENGE  ALL MAL:VERKN  
     NOT (L1 = L2)
     AND  
     HG (F MAL)
     AND  
     LIDEAL (L1 F MAL)
     AND  
     MINIMAL (L1 F MAL)
     AND  
     LIDEAL (L2 F MAL)
     AND  
     MINIMAL (L2 F MAL)
     AND  
     RIDEAL (R1 F MAL)
     AND  
     MINIMAL (R1 F MAL)
     AND  
     RIDEAL (R2 F MAL)
     AND  
     MINIMAL (R2 F MAL)
     IMPL  
     LEER (SM (SM (R1 L1) SM (R2 L2))))