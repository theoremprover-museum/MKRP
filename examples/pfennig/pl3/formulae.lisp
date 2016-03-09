;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-
(ALL F,G THM (F) AND THM (IMP (F G)) IMPL THM (G))
(ALL P,Q,R,S THM (IMP (IMP (IMP (P Q) R) IMP (IMP (R P) IMP (S P)))))
(ALL P THM (IMP (P P)))
(ALL P,Q THM (IMP (IMP (P Q) P)) IMPL THM (P))

(* THEOREMS Pearce'e law)

(ALL P,Q,R THM (IMP (IMP (IMP (P Q) P) P)))