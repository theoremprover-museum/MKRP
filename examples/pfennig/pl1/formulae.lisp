;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-
(ALL F,G THM (F) AND THM (IMP (F G)) IMPL THM (G))
(ALL P,Q,R,S THM (IMP (IMP (IMP (P Q) R) IMP (IMP (R P) IMP (S P)))))
(ALL P,Q THM (IMP (P IMP (Q P))))




(* theorem)

(ALL P THM (IMP (P P)))