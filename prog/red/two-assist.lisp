;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

#+(or symbolics explorer)
(defmacro two-trace (SWITCH DUMP.OPTION)
  `(two==trace ',switch ',dump.option))

#+(or symbolics explorer)
(DEFUN TWO==TRACE (SWITCH DUMP.OPTION)
						; EDITED: 21-FEB-84 23:26:59        MW
						; INPUT:  EITHER OF THE ATOMS 'ON', T, 'OFF' AND THE
						;         DUMP.OPTION DETERMINING THE FORMAT FOR THE
						;         CG-DUMP CALLED. THE LATTER IS EVALUATED]
						; EFFECT: CAUSES THE OPERATIONS OF THE TWO MODULE
						;         TO BE TRACED.
						; NOTE:   FOR A DETAILED TRACE SWITCH ON THE CG-TRACE
						;         AS WELL]
  (CASE SWITCH
    ((ON T)
     (let ((TERMINALFLAG (EQL SWITCH T)))
       (eval `(progn (GLOBAL:ADVISE TWO=COMPLETE.RULES BEFORE two 0
		       (TWO=TRACE_BEFORE_COMPLETE.RULES ,TERMINALFLAG (first ARGLIST)))
		     (GLOBAL:ADVISE TWO=RESOLVE.ALL AFTER two 0
		       (TWO=TRACE_RESOLVE.ALL ,TERMINALFLAG (first values) (first arglist) (second arglist)))
		     (GLOBAL:ADVISE TWO=ADD.AND.LEVEL.SATURATE AFTER two 0
		       (TWO=TRACE_ADD.AND.LEVEL.SATURATE ,TERMINALFLAG (first ARGLIST)))
		     (GLOBAL:ADVISE TWO=COMPLETE.RULES AFTER two 0
		       (TWO=TRACE_AFTEr_COMPLETE.RULES ,TERMINALFLAG ,dump.option (first values) (first ARGLIST)))
		     (GLOBAL:ADVISE TWO=UPDATE.GRAPH BEFORE two 0
		       (TWO=TRACE_UPDATE.GRAPH ,TERMINALFLAG (first arglist) (second arglist) (third arglist)))))
       (APPEND '(TWO-TRACE SWITCHED ON. OUTPUT WILL GO TO) (COND ((EQL SWITCH T) '(TERMINAL)) (T '(TRACE FILE))))))
    (OFF (global:UNADVISE (TWO=COMPLETE.RULES TWO=RESOLVE.ALL TWO=ADD.AND.LEVEL.SATURATE TWO=UPDATE.GRAPH))
	 '(Two-TRACE SWITCHED OFF.))
    (OTHERWISE '(INVALID ARGUMENT - TRY (TWO-TRACE ON) OR (TWO-TRACE OFF)))))

(DEFUN TWO-DUMP (FILE &optional NICEFLG)
						; EDITED: 21-FEB-84 23:02:38        MW
						; INPUT: FILE OPEN FOR OUTPUT (OR T), NICEFLG =// NIL
						;        INDICATING THE FORMAT FOR A CG-DUMP.
						; VALUE: UNDEFINED.
  (TERPRI FILE) (TERPRI FILE)
  (PRINC "***** TWO-DUMP (RULES) " FILE) (DODOWN (RPTN (- (LINELENGTH NIL FILE) 24)) (PRINC "2" FILE)) (TERPRI FILE)
  (COND ((NOT TWO*RULES) (PRINC "NONE." FILE) (TERPRI FILE))
	(TWO*EMPTY.CLAUSE.FOUND
	 (CG-DUMP FILE (list '(MESSAGE "EMPTY CLAUSE DETECTED BY TWO: ") `(CLAUSES (LIST ,TWO*EMPTY.CLAUSE.FOUND) NICEFLG)))
	 (TERPRI FILE))
	(T
	 (PROG ((LEVEL# 0))
	       (MAPC
		 (FUNCTION
		   (LAMBDA (LEVEL) (PRINC "   AT LEVEL " FILE) (PRINC LEVEL# FILE) (PRINC ":   " FILE)
			   (COND (NICEFLG (TERPRI FILE) (CG-DUMP FILE `((CLAUSES ,LEVEL ,NICEFLG))))
				 (T (PRINC (DS-PNAME LEVEL) FILE) (TERPRI FILE)))
			   (SETQ LEVEL# (1+ LEVEL#))))
		 TWO*RULES)))))

(DEFUN TWO=TRACE_BEFORE_COMPLETE.RULES (TERMINALFLAG NEW.RULE)
						; EDITED: 22-FEB-84 10:37:25        MW
  (PROG ((FILE (COND (TERMINALFLAG T) (T (OPT-GET.OPTION TR_TRACE.FILE))))) (TERPRI FILE)
	(PRINC "22222 COMPLETION OF RULES BEGINS, MAX LEVEL = " FILE) (PRINC TWO*RULES.MAXLEVEL FILE) (TERPRI FILE)
	(PRINC "22222 CURRENT RULE CLAUSES: " FILE) (PRINC (DS-PNAME TWO*RULES) FILE) (TERPRI FILE)
	(PRINC "22222 NEW RULE ACCEPTED: " FILE) (PRINC (DS-PNAME NEW.RULE) FILE) (TERPRI FILE) (TERPRI FILE)))

(DEFUN TWO=TRACE_AFTER_COMPLETE.RULES (TERMINALFLAG DUMP.OPTION value NEW.RULE)
  (declare (ignore new.rule))
						; EDITED: 22-FEB-84 10:45:34        MW
  (let ((FILE (COND (TERMINALFLAG T) (T (OPT-GET.OPTION TR_TRACE.FILE)))))
    (PRINC "22222 NEW RULE CLAUSES ACCEPTED OR GENERATED: " FILE) (PRINC (DS-PNAME TWO*NEW.RULES) FILE) (TERPRI FILE)
    (PRINC "22222 NEW NON RULE CLAUSES GENERATED: " FILE) (PRINC (DS-PNAME VALUE) FILE) (TERPRI FILE) (TERPRI FILE)
    (COND
      (DUMP.OPTION
       (CG-DUMP FILE `((MESSAGE "22222 RULEGRAPH")
		       (CLAUSES ALL ,DUMP.OPTION)
		       (LINKS ALL (DS-LINK.COLOURS.FOR 'ALL) ,DUMP.OPTION)))
       (TERPRI FILE)))))

(DEFUN TWO=TRACE_RESOLVE.ALL (TERMINALFLAG value CLIST1 CLIST2)
  (COND
    ((AND CLIST1 CLIST2)
      (PROG ((FILE (COND (TERMINALFLAG T) (T (OPT-GET.OPTION TR_TRACE.FILE))))) (PRINC "22222 ...ALL RESOLVENTS OF: " FILE)
        (PRINC (DS-PNAME CLIST1) FILE) (TERPRI FILE) (PRINC "22222 WITH " FILE) (PRINC (DS-PNAME CLIST2) FILE) (TERPRI FILE)
        (PRINC "22222 RESULT = " FILE) (PRINC (DS-PNAME VALUE) FILE) (TERPRI FILE)))))

(DEFUN TWO=TRACE_UPDATE.GRAPH (TERMINALFLAG CLAUSES READY.CLAUSES RULES)
  (declare (ignore ready.clauses))
						; EDITED: 22-FEB-84 10:52:44        MW
  (PROG ((FILE (COND (TERMINALFLAG T) (T (OPT-GET.OPTION TR_TRACE.FILE)))))
	(PRINC "22222 NOW INSERTING LINKS INDUCED BY THE NEW RULES: " FILE) (PRINC (DS-PNAME RULES) FILE) (TERPRI FILE)
	(PRINC "22222 FOR THE CLAUSES " FILE) (PRINC (DS-PNAME CLAUSES) FILE) (TERPRI FILE)))

(DEFUN TWO=TRACE_ADD.AND.LEVEL.SATURATE (TERMINALFLAG LEVEL#)
  (PROG ((FILE (COND (TERMINALFLAG T) (T (OPT-GET.OPTION TR_TRACE.FILE))))) (PRINC "22222 SATURATION OF LEVEL " FILE)
    (PRINC LEVEL# FILE) (PRINC " COMPLETED." FILE) (TERPRI FILE) (TERPRI FILE)))