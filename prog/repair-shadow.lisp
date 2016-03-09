;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

(IN-PACKAGE "USER")
;;; Only for Coral where SUBST on symbols does not work

(SHADOW '(SUBST)
	(FIND-PACKAGE "MKRP"))
