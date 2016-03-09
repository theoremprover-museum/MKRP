;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* SMULLYAN, KAP 6, AUFGABE 69)
(* DREI KRIMINELLE A, B, C WURDEN ZUM VERHOER ZU SCOTLAND)
(* YARD GEBRACHT. FOLGENED FAKTEN WURDEN ERMITTELT:)
(* (1) AUSSER A, B, C WAR NIEMAND AM RAUB
BETEILIGT.)
(* (2) C DREHT KEIN DING, OHNE A (UND MOEGLICHERWEISE ANDERE))
(* ALS KOMPLIZEN ZU HABEN.)
(* (3) B KANN NICHT AUTO FAHREN.)
(* IST A SCHULDIG ODER UNSCHULDIG?)
(* ERKLAERUNG DER PRAEDIKATE:)
(* AUTO (X) IFF X KANN AUTO FAHREN)
(* DIEB (X) IFF X IST AM DIEBSTAHL BETEILIGT)
(*)
(NOT AUTO (A) IMPL DIEB (B) OR DIEB
(C))
(NOT AUTO (B) IMPL DIEB (A) OR DIEB (C))
(NOT AUTO (C) IMPL DIEB (A) OR DIEB (B))
(DIEB (A) OR DIEB (B) OR DIEB (C))
(DIEB (C) IMPL DIEB (A))
(NOT AUTO (B))

(* OFFENSICHTLICH IST A SCHULDIG!)
(DIEB (A))