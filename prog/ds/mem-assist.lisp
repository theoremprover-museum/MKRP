;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: lisp -*-

(IN-PACKAGE "MKRP" :use '("CL"))


(defvar MEM*ALLOC.FILE NIL)

(defmacro mem-alloc.dump (sw fi)
  `(mem=alloc.dump ',sw ',fi))

(DEFUN MEM=ALLOC.DUMP (SWITCH FILE)
  ;; EDITED: 20. 9. 1984
  ;; INPUT:  ONE OF THE ATOMS ON OR OFF AND A FILENAME.
  ;; EFFECT: MEM-ALLOC DUMP IS SWITCHED ON OR OFF.
  ;;         THE OUTPUT WILL GO TO FILE (WHICH IS OPENED
  ;;         WITH SWITCH = ON AND CLOSED WITH
  ;;         SWITCH = OFF).
  ;; VALUE:  UNDEFINED. 
  (CASE SWITCH
    (ON (SETQ MEM*ALLOC.FILE FILE)
	(when FILE (setq file (open file :direction :output)))
	(GLOBAL:ADVISE MEM=ALLOC.alloc AFTER  mem 0 (MEM=ALLOC.ALLOC TYPE CELLS //VALUE))
	(GLOBAL:ADVISE MEM=ALLOC.ERASE BEFORE mem 0 (MEM=ALLOC.ERASE ADR COLLECTFLAG))
	'(DUMP SWITCHED ON))
    (OFF (COND (MEM*ALLOC.FILE (CLOSEFILE MEM*ALLOC.FILE))) (GLOBAL:UNADVISE MEM=ALLOC MEM-ERASE) '(DUMP SWITCHED OFF))
    (OTHERWISE '(ILLEGAL SWITCH! TRY ON OR OFF))))

(DEFUN MEM=ALLOC.ALLOC (TYPE CELLS ADR)		; edited: 20. 9. 1984
						; input:  a type specifier, the number of new cells
						;         and the new allocated address.
						; effect: information about the new object and a part
						;         of the stack is printed onto mem*alloc.file.
						; value:  undefined
  (format MEM*alloc.file "++ ~A~vT~A~vT~A cells~vT~%" type 12 adr 20 cells 27))

(DEFUN MEM=ALLOC.ERASE (ADR COLLECTFLAG) ; edited: 20. 9. 1984
						; input:  a memory address and the collectflag
						; effect: information about the erased object and
						;         a part of the stack is printed onto mem*alloc.file
  (format MEM*Alloc.file "-- ~A~vT~A~vT~A~vT~%" (MEM-TYPE ADR) 12 adr 20 collectflag 27)
  #|(MAPDL #'(LAMBDA (FCT)			; mapped ueber die Funktionen des Stacks
        (when (AND (NEQ FCT 'MEM=ALLOC.ERASE) (NEQ FCT 'MEM-ERASE) (NEQ FCT 'ADV-PROG)
		   (OR (POSITION "=" (string FCT)) (POSITION "-" (string FCT))))
	  (PRINC FCT MEM*ALLOC.FILE) (PRINC " " MEM*ALLOC.FILE))))
  |#
  (TERPRI MEM*ALLOC.FILE))

(DEFUN MEM-DUMP (FROM TO FILENAME)		; input:  two integer and a file name.
						; effect: maintenance and debugging aid
  (unwind-protect (let ((IND1 FROM) IND2 IND3)
		    (unless (MEMBER FILENAME '(T NIL))
		      (setq filename (if (streamp filename)
					 filename
					 (open FILENAME :direction :output))))
		    (SETQ IND2 0 IND3 0)
		    (format filename "~%Memory System: dump from ~D to ~D~2%" from to)
		    (WHILE (<= IND1 TO)
		      (COND ((EQL (AREF MEM*MEMORY IND1) 'ATP.MEMORY.NOBIND)
			     (SETQ IND3 IND1)
			     (WHILE (EQL (AREF MEM*MEMORY IND3) 'ATP.MEMORY.NOBIND)
			       (incf ind2) (incf ind3))
			     (format filename "MEM*MEMORY (~D (~D) ) =~vT~A~%" ind1 ind2 30 (AREF MEM*MEMORY IND1))
			     (SETQ IND1 IND3 IND2 0))
			    (T (format filename "MEM*MEMORY (~D) =~vT~A~%" ind1 30 (AREF MEM*MEMORY IND1))
			       (incf IND1)))))
    (unless (MEMBER FILENAME '(T NIL))
      (close FILENAME)))
  '(END OF MEM-DUMP))

(DEFUN MEM-PR (FILENAME)			; input:  a file name
						; effect: displaying the most important states
						;         on the file.
  (unwind-protect (progn (unless (MEMBER FILENAME '(T NIL))
			   (setq filename (open filename :direction :OUTPUT)))
			 (format filename "~%Status of Memory System 2:~2%~
                                             MEM*SIZE = ~D~vTMEM*RESERVE = ~D~2%~
                                             MEM*NEXT.VADR                 = ~D~%~
                                             MEM*NEXT.RADR                 = ~D~2%~
                                             MEM*REST                      = ~D~%~
                                             MEM*COLLECTABLE               = ~D~%~
                                             MEM*FIRST.REUSABLE.VADR       = ~D~%~
                                             MEM*LAST.REUSABLE.VADR        = ~D~2%~
                                             MEM*MEMORY(MEM*NEXT.VADR - 1) = ~A~%"
				 MEM*SIZE 18 MEM*RESERVE MEM*NEXT.VADR MEM*NEXT.RADR MEM*REST MEM*COLLECTABLE
				 MEM*FIRST.REUSABLE.VADR MEM*LAST.REUSABLE.VADR (AREF MEM*MEMORY (1- MEM*NEXT.VADR))))
    (unless (MEMBER FILENAME '(T NIL))
      (close filename)))
  '(END OF MEM-PR))

(defmacro mem-check (switch)
  `(mem=check ',switch))

(DEFUN MEM=CHECK (SWITCH)			; input:  an atp.memory2-function which is
						;         working with the type- or property-cell.
						; effect: maintenance and debugging aid
						; value:  undefined
  (CASE SWITCH
    (ON (SCL:ADVISE MEM-NEW         BEFORE mem 0 (MEM=CHECK_SIZE (second scl:arglist)))
	(SCL:ADVISE MEM-GET         BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_N   (first scl:arglist) (second scl:arglist))))
	(SCL:ADVISE MEM-PUT         BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_N   (first scl:arglist) (second scl:arglist))))
	(SCL:ADVISE MEM-SHORTEN     BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_N   (first scl:arglist) (second scl:arglist))))
	(SCL:ADVISE MEM-SIZE        BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))))
	(SCL:ADVISE MEM-ERASE       BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))))
	(SCL:ADVISE MEM-PUTPROP     BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_INDICATOR IND)))
	(SCL:ADVISE MEM-GETPROP     BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_INDICATOR IND)))
	(SCL:ADVISE MEM-REMPROP     BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_INDICATOR IND)))
	(SCL:ADVISE MEM-REMPROPS    BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(EVERY #'MEM=CHECK_INDICATOR INDS)))
	(SCL:ADVISE MEM-GETPROPLIST BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))))
	(SCL:ADVISE MEM-SETPROPLIST BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))))
	(SCL:ADVISE MEM-ADDPROP     BEFORE mem 0 (PROGN (MEM=CHECK_ADR (first scl:arglist))
							(MEM=CHECK_INDICATOR IND)))
	'(MEM-CHECK SWITCHED ON))
    (OFF (EVAL (CONS 'SCL:UNADVISE '(MEM-ADDPROP MEM-SETPROPLIST MEM-GETPROPLIST MEM-REMPROPS MEM-REMPROP
						 MEM-GETPROP MEM-PUTPROP MEM-ERASE MEM-SIZE MEM-SHORTEN MEM-PUT
						 MEM-GET MEM-NEW)))
	 '(MEM-CHECK SWITCHED OFF))
    (OTHERWISE '(INVALID ARGUMENT - TRY (MEM-CHECK ON) OR (MEM-CHECK OFF)))))

(DEFUN MEM=CHECK_INDICATOR (IND) (COND ((ATOM IND)) (T (ERROR "(MEM-CHECK) - INDICATOR IS NO ATOM!: ~A" IND))))

(DEFUN MEM=CHECK_SIZE (size)
  (when (OR (NOT (INTEGERP SIZE)) (MINUSP SIZE))
    (ERROR "(MEM-CHECK) - INVALID SIZE: ~A" SIZE)))

(DEFUN MEM=CHECK_ADR (adr)
  (unless (MEM-ADDRESS ADR)
    (ERROR "(MEM-CHECK) - Invalid address: ~S" ADR)))

(DEFUN MEM=CHECK_N (adr n)
  (when (OR (NOT (INTEGERP N)) (< N 1) (> N (- (MEM=GETSIZE ADR) 3)))
    (ERROR "(MEM-CHECK) - INVALID INDEX: ~S" N)))


