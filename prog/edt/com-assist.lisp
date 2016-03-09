;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

#+(or symbolics explorer)
(defmacro com-trace (&REST FUNCTIONS)
  `(com=trace ,(copy-list functions)))


#+(or symbolics explorer)
(DEFUN COM=TRACE (FUNCTIONS)			; EDITED  9-MAR-81 16:18:34
  (MAPC #'(LAMBDA (FOO)
	    (eval `(global:ADVISE ,FOO BEFORE foo 0
		     (PROGN (PRINC "(NULL COM*ERROR)= " T) (PRINC (NULL COM*ERROR) T)
			    (TERPRI T) (PRINC "SYMBOL= " T) (PRINC SYMBOL T) (TERPRI T)
			    (PRINC "SYMBOL.LIST= " T) (PRINC SYMBOL.LIST T) (TERPRI T)))))
	FUNCTIONS)
  (EVAL (CONS 'TRACE FUNCTIONS)))