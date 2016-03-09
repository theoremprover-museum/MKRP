;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-
(ALL F,G THM (F) AND THM (IMP (F G)) IMPL THM (G))
(ALL P,Q,R,S THM (IMP (IMP (IMP (P Q) R) IMP (IMP (R P) IMP (S P)))))



(* theorem)

(ALL P,Q,R THM (IMP (P IMP (Q P))) AND THM (IMP (IMP (IMP (P Q) P) Q)) AND THM (IMP (IMP (P Q) IMP (IMP (Q R) IMP (P R)))))