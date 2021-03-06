(in-package "MKRP")

(DEFUN OS=PROTOCOL.PROC (CODE.FILE LIST.FILE)	; INPUT:  LIST OF 0-2 ATOMS:
						;         [<CODE.FILE> [<LIST.FILE>]]
						; EFFECT: PREPARES A PROOF PROTOCOL FROM THE RAW DATA
						;         ON CODE.FILE.
						; VALUE:  UNDEFINED.
  (COND
    ((OS=SYSTEM.IS.READY.FOR 'PROTOCOL)
     (OS=OPEN.LIST.FILE LIST.FILE)
     (PRO-LIST.PROTOCOL (if (symbolp code.file)
			    (MKRP-MAKE.PATHNAME T "code" "lisp" CODE.FILE)
			     CODE.FILE)
			OS*ACTUAL.LIST.FILE)
     (OS=CLOSE.LIST.FILE)
     (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'PROTOCOL.PROC_LIST.FILE.MESSAGE OS*ACTUAL.LIST.FILE) *standard-output*))))