;;; -*- Package: MKRP; Base: 10.; Mode: LISP; Syntax: Common-lisp -*- 

#| 
TWO:RULES                             NIL                   TWO:RULES.MAXLEVEL                    1
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
 
TERM:UNITS                            T                     TERM:ITERATIONS                       0
TERM:SET.OF.SUPPORT                   NIL                   TERM:BREADTH.FIRST                    NIL
 
GEN:SPLITTING                         0                     GEN:MANUAL.CONTROL                    NIL
GEN:MAXIMUM.STEPS                     NIL                   GEN:GRAPH.SAVING                      NIL
GEN:SAVE.FILE                         SAVE.DEFAULT
 
 
TR:PREPROCESSING                      NIL                   TR:STEP.MODE                          LR
TR:DUMP                               NIL                   TR:CLAUSE.MODE                        I
TR:LINK.MODE                          I                     TR:TRACE.FILE                         NIL
TR:TERMINAL                           NIL
 
 
PR:INFIX.FORM                         T                     PR:PREFIX.FORM                        NIL
PR:OPTIONS                            T                     PR:AXIOM.CLAUSES                      NIL
PR:SYMBOLS                            NIL                   PR:STATISTICS                         NIL
PR:PROTOCOL                           T                     PR:LEFT.MARGIN                        0
PR:LINELENGTH                         120
|#

(SORT MENGE,ABB,VERKN:ANY)
           (TYPE W(MENGE) :MENGE)
           (TYPE CHI:ABB)
           (* ISOMORPHIEAXIOM *)
           (ALL PHI:ABB ALL A,B:MENGE ISOMORPH(PHI A B) EQV(HOMOMORPH(PHI A B) AND BIJEKTIV(PHI A B)))
           (* LEMMA 2.7.LR.HOM UND LEMMA 2.7.LR.BIJ *)
           (ALL F:MENGE  ALL MAL:VERKN  
             HG(F MAL) AND FREI(F MAL) IMPL(EX X:MENGE HOMOMORPH(CHI F W(X)) AND BIJEKTIV(CHI F W(X))))
 
(ALL F:MENGE ALL MAL:VERKN HG(F MAL) AND FREI(F MAL) IMPL(EX X:MENGE EX PHI:ABB ISOMORPH(PHI F W(X))))
 
 