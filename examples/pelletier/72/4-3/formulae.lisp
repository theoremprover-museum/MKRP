;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-



(* Axioms *)

(p1a or p1b or p1c)
(p2a or p2b or p2c)
(p3a or p3b or p3c)
(p4a or p4b or p4c)

(not p1a or not p2a)
(not p1a or not p3a)
(not p1a or not p4a)
(not p2a or not p3a)
(not p2a or not p4a)
(not p3a or not p4a)

(not p1b or not p2b)
(not p1b or not p3b)
(not p1b or not p4b)
(not p2b or not p3b)
(not p2b or not p4b)
(not p3b or not p4b)

(not p1c or not p2c)
(not p1c or not p3c)
(not p1c or not p4c)
(not p2c or not p3c)
(not p2c or not p4c)
(not p3c or not p4c)

(* No Theorems *)

