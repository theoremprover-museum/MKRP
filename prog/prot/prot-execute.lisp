;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

;;; the protocol (sub-)system needs the memory and dataterm modules
;;; the memory has to be newly initialized !


(DEFUN PRO-LIST.PROTOCOL (CODE.FILE LIST.FILE)
						; edited: 31-aug-84 14:48:24  by cl
						; input : a file containing the proof code and a file
						;         to write the protocol on (the latter open).
						; effect: prints the protocol on list file.
						; value : fileversion of list file
  (PPR-CODE (READFILE CODE.FILE) code.file)
  (PROG1 (PPP-PRINT.PROTOCOL LIST.FILE)	;
	      (PRO=RESET)))

(DEFUN PRO=VERSIONS ()				; edited: 22-jun-84 13:56:41 by cl
						; input : a list of dotted pairs (the options !) symbols!
						; effect: extracts the parts needed and transforms them into a useful format
						; value : transformed lidt of dooted pairs
  (MAPCAN #'(LAMBDA (PAIR)
	      (let ((OPTION.NAME (CAR PAIR)) (option.value (cdr pair)))
		(when (search "PR_" (symbol-name OPTION.NAME))	; i.e. the protocol options
		  (CASE OPTION.NAME
		    (PR_Variable.print.names (when (listp option.value) (setq option.value (mapcar #'string option.value))))
		    (PR_INFIX.FORM  (SETQ OPTION.NAME 'INFIX))
		    (PR_PREFIX.FORM (SETQ OPTION.NAME 'PREFIX))
		    (OTHERWISE      (SETQ OPTION.NAME (INTERN (SUBSEQ (symbol-name OPTION.NAME) 3) (find-package "MKRP")))))
		  (LIST (CONS OPTION.NAME option.value)))))
	  (opt-get.list.options)))


(DEFUN PRO=RESET NIL				; edited: 28-aug-84 15:55:06  by cl
						; input :  none
						; effect:  sets the common.variables used to nil, and
						;          resets the memory, in order to give room
						;          for the garbage collector.
  (PDS-RESET))					; value :  undefined

