;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP -*-

(IN-PACKAGE "MKRP")
;;; Only for Coral where SUBST on symbols does not work

(defun subst (new old tree &rest args &KEY (TEST (FUNCTION EQL)) TEST-NOT (KEY (FUNCTION IDENTITY)))
  (if (symbolp tree)
      (if (funcall test old tree)
	  new
	  tree)
      (apply #'cl:subst new old tree args)))
