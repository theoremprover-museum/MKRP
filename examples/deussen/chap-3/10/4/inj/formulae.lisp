;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*-
#|

TWO:RULES                             NIL                   TWO:RULES.MAXLEVEL                    1.
TWO:SUPPRESS.NORULES                  NIL


RED.I:CLAUSE.MULTIPLE.LITERALS        T                     RED.I:CLAUSE.PURITY                   T
RED.I:CLAUSE.TAUTOLOGY                T                     RED.I:CLAUSE.TAUTOLOGY.RECHECK        PARTIAL
RED.I:CLAUSE.SUBSUMPTION              T                     RED.I:CLAUSE.SUBSUMPTION.RECHECK      PARTIAL
RED.I:CLAUSE.REPL.FACTORING           T                     RED.I:CLAUSE.REPL.FACTORING.RECHECK   T
RED.I:CLAUSE.REPL.RESOLUTION          SIMPLE                RED.I:CLAUSE.REPL.RESOLUTION.RECHECK  T
RED.I:CLAUSE.REWRITING                T                     RED.I:LINK.INCOMPATIBILITY            NIL
RED.I:LINK.TAUTOLOGY                  REMOVE-INHIBIT        RED.I:LINK.TAUTOLOGY.RECHECK          NIL
RED.I:LINK.SUBSUMPTION                REMOVE-INHIBIT        RED.I:LINK.SUBSUMPTION.RECHECK        NIL

RED.D:CLAUSE.MULTIPLE.LITERALS        T                     RED.D:CLAUSE.PURITY                   T
RED.D:CLAUSE.TAUTOLOGY                REMOVE-INHIBIT        RED.D:CLAUSE.TAUTOLOGY.RECHECK        NIL
RED.D:CLAUSE.SUBSUMPTION.FORWARD      NIL                   RED.D:CLAUSE.SUBSUMPTION.BACKWARD     NIL
RED.D:CLAUSE.SUBSUMPTION.RECHECK      NIL                   RED.D:CLAUSE.REPL.FACTORING           NIL
RED.D:CLAUSE.REPL.FACTORING.RECHECK   NIL                   RED.D:CLAUSE.REPL.RESOLUTION          NIL
RED.D:CLAUSE.REPL.RESOLUTION.RECHECK  NIL                   RED.D:LINK.INCOMPATIBILITY            NIL
RED.D:LINK.TAUTOLOGY                  NIL                   RED.D:LINK.TAUTOLOGY.RECHECK          NIL
RED.D:LINK.SUBSUMPTION                NIL                   RED.D:LINK.SUBSUMPTION.RECHECK        NIL

FAC:INITIAL                           NIL                   FAC:EACH.STEP                         NIL

STR:RESOLUTION                        SET-OF-SUPPORT        STR:E-RESOLUTION                      NIL
STR:PARAMODULATION                    UNIT-ANCESTRY         STR:LINK.DEPTH                        NIL
STR:TERM.DEPTH                        6.                    STR:R.DEMODULATION                    T
STR:P.DEMODULATION                    T                     STR:INDUCTION                         NIL

TERM:UNITS                            T                     TERM:ITERATIONS                       1.
TERM:SET.OF.SUPPORT                   T                     TERM:BREADTH.FIRST                    T

GEN:SPLITTING                         0.                    GEN:MANUAL.CONTROL                    NIL
GEN:MAXIMUM.STEPS                     75.                   GEN:GRAPH.SAVING                      NIL
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
           (TYPE G (INDEX INDEX) :MENGE)
           (TYPE GLEICH-E (ELEMENT ELEMENT))
           (TYPE INV-A (ABB) :ABB)
           (TYPE APPLYA (ABB ELEMENT) :ELEMENT)
           (TYPE ABBILDUNG (ABB MENGE MENGE))
           (TYPE ABBVOR (ABB ELEMENT ELEMENT))
           (* DEFINITION VON INJEKTIV *)
           (ALL A,B:MENGE  ALL PHI:ABB  
               (ABBILDUNG (PHI A B) IMPL (ALL X:ELEMENT EL (X A) IMPL APPLYA (INV-A (PHI) APPLYA (PHI X)) = X))
               IMPL  
               INJEKTIV (PHI A B))
           (* ASSOZIATIVGESETZ FUER ELEMENTE EINER HALBGRUPPE *)
           (ALL F:MENGE ALL MAL:VERKN ALL X,Y,Z:ELEMENT APPLYV (MAL APPLYV (MAL X Y) Z) = APPLYV (MAL X APPLYV (MAL Y Z)))
           (* UMKEHRABBILDUNG *)
           (ALL PHI:ABB  ALL A,B:MENGE  ALL X,Y,E1,E2:ELEMENT  ALL MAL:VERKN  
                 GR (A MAL E1)
                 AND  
                 GR (B MAL E2)
                 AND  
                 (ABBILDUNG (PHI A B) IMPL (ALL X:ELEMENT EL (X A) IMPL APPLYA (PHI X) = APPLYV (MAL X E2)))
               IMPL  
                  ABBILDUNG (INV-A (PHI) B A)
                  AND  
                  (ALL Y:ELEMENT EL (Y B) IMPL APPLYA (INV-A (PHI) Y) = APPLYV (MAL Y E1)))
           (* SATZ 3.12 *)
           (ALL MAL:VERKN  ALL J1,J2,J3:INDEX  ALL E1,E2:ELEMENT  ALL PHI:ABB  
               GR (G (J1 J2) MAL E1) AND GR (G (J1 J3) MAL E2) AND HOMOMORPH (PHI G (J1 J2) G (J1 J3))
               IMPL  
               APPLYV (MAL E2 E1) := E1)
           (* EIGENSCHAFT DES EINSELEMENTES *)
           (ALL A:MENGE ALL MAL:VERKN ALL E,X:ELEMENT GR (A MAL E) AND EL (X A) IMPL APPLYV (MAL X E) := X)

(ALL E1,E2:ELEMENT  ALL PHI:ABB  ALL H:MENGE  ALL MAL:VERKN  ALL I1,I2:INDEXMENGE  ALL J1,J2,J3:INDEX  )
           (ALL F:ELEMENT  
                 HG (H MAL)
                 AND  
                 MINIMAL-BED-R (H MAL)
                 AND  
                 MINIMAL-BED-L (H MAL)
                 AND  
                 (ALL J1:INDEX EL-IND (J1 I1) IMPL RIDEAL (U (J1) H MAL) AND MINIMAL (U (J1) H MAL))
                 AND  
                 (ALL J2:INDEX EL-IND (J2 I2) IMPL LIDEAL (U (J2) H MAL) AND MINIMAL (U (J2) H MAL))
                 AND  
                 EL-IND (J1 I1)
                 AND  
                 EL-IND (J2 I2)
                 AND  
                 (ALL J1,J2:INDEX EL-IND (J1 I1) AND EL-IND (J2 I2) IMPL MPROD (U (J1) U (J2) MAL) = SM (U (J1) U (J2)))
                 AND  
                 EL (F G (J1 J2))
                 AND  
                 EL-IND (J3 I2)
                 AND  
                 GR (G (J1 J2) MAL E1)
                 AND  
                 GR (G (J1 J3) MAL E2)
                 AND  
                 ABBILDUNG (PHI G (J1 J2) G (J1 J3))
                 AND  
                 HOMOMORPH (PHI G (J1 J2) G (J1 J3))
                 AND  
                  ALL X:ELEMENT  
                    EL (X G (J1 J2)) IMPL APPLYA (PHI X) = APPLYV (MAL X E2) AND EL (APPLYA (PHI X) G (J1 J3))
               IMPL  
               INJEKTIV (PHI G (J1 J2) G (J1 J3)))