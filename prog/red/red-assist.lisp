;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: lisp -*-

(IN-PACKAGE "MKRP" :use '("CL"))

(DEFVAR RED*TRACE_TRACES '(TIME TIMES SPACE))

(DEFVAR RED*TRACE_ALL.STEPS.ENTRIES '(TOT.TIME TOT.TIMES TOT.SPACE))

(DEFVAR RED*TRACE_PRINTS
  '((22 CONCAT RED*TRACE_TIME " MSEC") (22 RDS-RULE 'TIMES RULE) (22 CONCAT RED*TRACE_SPACE " BYTES")))

(defvar RED*TRACE_BEGIN.POS 0)

(defvar RED*TRACE_PREFIX "!!!!!")

(defvar RED*TRACE_TIME NIL)

(defvar RED*TRACE_SPACE NIL)

(defvar RED*TRACE_END.POS NIL)

(DEFmacro RED-TRACE (SWITCH &REST TRACE.KINDS)	; edited:  3-apr-84 19:49:28
						; input:   'switch' is one of the atoms 'ON', 'OFF', and 'T'.
						;          'trace.kinds' is nil or a list only with
						;          one element: the atom 'CONTROL'.
						; effect:  reduction trace is switched on or off.
						;          if 'switch' = 't' trace will be printed
						;          onto terminal, if 'switch' = 'on' onto trace file.
						;          'control' switches on a trace for insertions of objects
						;          to be reduced into agenda of reduction module.
						; value:   error- or readymessage for the human user of this function.
  (CASE SWITCH
    (OFF `(progn ,@(mapcar #'(lambda (function) `(scl:unadvise ,function))
			   `(RED=CTL_APPLY.RULES
			      RED=CTL_AGENDA.INSERT.RECHECK
			      RED=CTL_AGENDA.INSERT.LINK
			      RED=CTL_AGENDA.INSERT.CLAUSE 
			      RED=CTL_APPLY.RULES RED=RESET RED-END
			      ,@(mapcar #'(lambda (rule)
					    (MAPC #'(LAMBDA (OBJECT.TYPE)
						      (PROG (FUNCTION.INDICATOR)
							    (SETQ FUNCTION.INDICATOR
								  (INTERN (format nil "APPLY.TO.~A.FUNCTION" OBJECT.TYPE)
									  (find-package "MKRP")))
							    (RDS-RULE FUNCTION.INDICATOR RULE)))
						  '(CLAUSE LINK RECHECK)))
					(RDS-RULES 'ALL))))
		 '(RED-TRACE SWITCHED OFF.)))	; (RED-TRACE ON)
    ((ON T) `(let ((CONTROL.FLAG NIL)
		   (VALUE NIL)
		   (FILE ',(CASE SWITCH (ON (OPT-GET.OPTION TR_TRACE.FILE)) (OTHERWISE T))))
	       (RED=RESET 'NEUTRAL)
	       (MAPC #'(LAMBDA (KIND)
			 (CASE KIND
			   (CONTROL   (SETQ CONTROL.FLAG T))
			   (OTHERWISE (SETQ VALUE '(INVALID ARGUMENTS. TRY (RED-TRACE ON) (RED-TRACE ON CONTROL)
							    (RED-TRACE T) (RED-TRACE T CONTROL) OR (RED-TRACE OFF))))))
		     ',TRACE.KINDS)
	       (unless VALUE
		 (if CONTROL.FLAG (RED=TRACE_CTL FILE) (RED=TRACE_RULES FILE))
		 (SETQ FILE (RED=TRACE_INIT FILE))
		 (RED=RESET 'NEUTRAL)
		 (SETQ VALUE (APPEND '(RED-TRACE) (COND (CONTROL.FLAG '(WITH CONTROL TRACE))) '(SWITCHED ON. OUTPUT WILL GO TO)
				     ,(CASE SWITCH (ON '(TRACE FILE.)) (OTHERWISE '(TERMINAL.))))))
	       VALUE))				; (RED-TRACE ON control) (red-trace T)
    (OTHERWISE					; (RED-TRACE FOO)
      ''(INVALID ARGUMENTS. TRY (RED-TRACE ON) (RED-TRACE ON CONTROL) (RED-TRACE T) (RED-TRACE T CONTROL) OR (RED-TRACE OFF)))))

(DEFUN RED=TRACE_RULES (FILE)
  ;; EDITED: 24-MAR-84 01:42:57
  ;; INPUT:  AN ATOM, DEFINING TRACE FILE.
  ;; EFFECT: PERFORMS THE ADVISES OF REDUCTION TRACE
  ;;         CONCERNING STATISTICS OF RULE APPLICATIONS.
  ;; VALUE:  UNDEFINED. 
  (eval `(global:ADVISE RED=CTL_APPLY.RULES BEFORE red 0 (RED=TRACE_APPLY.RULES.BEFORE ',FILE)))
  (eval `(global:ADVISE RED=CTL_APPLY.RULES AFTER red 0 (RED=TRACE_APPLY.RULES.AFTER ',FILE)))
  (global:ADVISE RED=RESET BEFORE red 0 (RED=TRACE_RESET))
  (eval `(global:ADVISE RED-END BEFORE red 0 (RED=TRACE_END ',FILE)))
  (MAPC #'(LAMBDA (WHERE)
	    (MAPC #'(LAMBDA (RULE)
		      (MAPC #'(LAMBDA (OBJECT.TYPE)
				(let (FUNCTION.TO.BE.ADVISED FUNCTION.INDICATOR)
				  (SETQ FUNCTION.INDICATOR
					(INTERN (format nil "APPLY.TO.~A.FUNCTION" OBJECT.TYPE)
						(find-package "MKRP")))
				  (SETQ FUNCTION.TO.BE.ADVISED (RDS-RULE FUNCTION.INDICATOR RULE))
				  (COND
				    ((functionp FUNCTION.TO.BE.ADVISED)
				     (eval `(global:ADVISE ,FUNCTION.TO.BE.ADVISED ,WHERE red 0
					      ,(nconc1
						 (LIST (INTERN (format nil "RED=TRACE_RULE.~A" WHERE)
							       (find-package "MKRP"))
						       (KWOTE FILE)
						       (KWOTE OBJECT.TYPE)
						       (KWOTE RULE))
						 'scl:arglist)))))))
			    '(CLAUSE LINK RECHECK)))
		  (RDS-RULES 'ALL)))
	'(BEFORE AFTER)))

(DEFUN RED=TRACE_CTL (FILE)
  ;; EDITED: 24-MAR-84 01:52:15
  ;; INPUT:  AN ATOM, DEFINING TRACE FILE.
  ;; EFFECT: PERFORMS THE ADVISES OF REDUCTION TRACE
  ;;         CONCERNING CONTROL OF RULE APPLICATIONS,
  ;;         E.G. INSERTION INTO AGENDAS.
  ;; VALUE:  UNDEFINED. 
  (eval `(global:ADVISE RED=CTL_AGENDA.INSERT.CLAUSE AFTER red 0
	   (RED=TRACE_AGENDA.INSERT.CLAUSE ,(KWOTE FILE) (first scl:arglist) (second scl:arglist))))
  (eval `(global:ADVISE RED=CTL_AGENDA.INSERT.LINK AFTER red 0
	   (RED=TRACE_AGENDA.INSERT.LINK ,(KWOTE FILE) (first scl:arglist) (second scl:arglist))))
  (eval `(global:ADVISE RED=CTL_AGENDA.INSERT.RECHECK AFTER red 0
	   (RED=TRACE_AGENDA.INSERT.RECHECK ,(KWOTE FILE) (first scl:arglist) (second scl:arglist)))))

(DEFUN RED=TRACE_APPLY.RULES.BEFORE (FILE)
  ;; EDITED: 29-MAR-84 15:14:11
  ;; INPUT:  AN ATOM.
  ;; EFFECT: PRINTS THE 'BEGIN OF REDUCTION' LINE ON
  ;;         TERMINAL FOR 'FILE' T OR ON CURRENT TRACE
  ;;         FILE IF 'FILE' IS NOT T.
  ;;         IF TRACE FILE IS NIL, REDUCTION TRACE WILL
  ;;         BE SWITCHED OFF.
  ;;         ALL STATISTIC COMPONENTS OF RULES FOR ONE
  ;;         STEP ARE RESET TO ZERO.
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF
  ;;         'RED=CTL_APPLY.RULES' SET BY
  ;;         'RED=TRACE_RULES'.
  (SETQ FILE (RED=TRACE_INIT FILE))
  ;; TRACE FILE HANDLING.
  (MAPC
    (FUNCTION
      (LAMBDA (RULE)
	;; RESET TO ZERO.
        (MAPC (FUNCTION (LAMBDA (KIND) (RDS-RULE.PUT KIND RULE 0))) RED*TRACE_TRACES)))
    (RDS-RULES 'ALL))
  ;; PRINTING BEGIN-LINE.
  (PROG (NUMBER)
	(SETQ NUMBER (+ RED*TRACE_END.POS (- RED*TRACE_BEGIN.POS) -23 (- (PRINT-LENGTH RED*TRACE_PREFIX))))
	(FORMAT FILE "~vT" (1+ RED*TRACE_BEGIN.POS)) (PRINC RED*TRACE_PREFIX FILE) (PRINC " BEGIN OF REDUCTION. " FILE)
	(DODOWN (RPTN (TRUNCATE NUMBER (PRINT-LENGTH RED*TRACE_PREFIX))) (PRINC RED*TRACE_PREFIX FILE))
	(FORMAT FILE "~vT" (1+ RED*TRACE_END.POS))))

(DEFUN RED=TRACE_APPLY.RULES.AFTER (FILE)
  ;; EDITED: 29-MAR-84 15:20:35
  ;; INPUT:  AN ATOM.
  ;; EFFECT: PRINTS A TABLE OF REDUCTION STATISTICS FOR
  ;;         THIS CALL OF REDUCTION FOR THE REDUCTION
  ;;         RULES SWITCHED ON AND THE
  ;;         'END OF REDUCTION' LINE ON
  ;;         TERMINAL FOR 'FILE' T OR ON CURRENT TRACE
  ;;         FILE ELSE. 
  ;;         IF TRACE FILE IS NIL, REDUCTION TRACE WILL
  ;;         BE SWITCHED OFF.
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF
  ;;         'RED=CTL_APPLY.RULES' SET BY
  ;;         'RED=TRACE_RULES'.
  ;; TRACE FILE HANDLING.
  (SETQ FILE (RED=TRACE_INIT FILE))
  (COND
    ((SYMBOL-VALUE 'RED*RULES.SWITCHED.ON)
     ;; PRINTING TABLE.
     (RED=TRACE_WRITELN (LIST 50 "REDUCTION STATISTICS AFTER ONE REDUCTION STEP: ") FILE)
     (RED=TRACE_PRINTTAB RED*TRACE_TRACES FILE RED*RULES.SWITCHED.ON)))
  ;; PRINTING END-LINE. 
  (PROG (NUMBER)
	(SETQ NUMBER
	      (+ RED*TRACE_END.POS (- RED*TRACE_BEGIN.POS) -23 (- (PRINT-LENGTH RED*TRACE_PREFIX NIL))))
	(FORMAT FILE "~T" (1+ RED*TRACE_BEGIN.POS)) (PRINC RED*TRACE_PREFIX FILE) (PRINC " END OF REDUCTION.   " FILE)
	(DODOWN (RPTN (TRUNCATE NUMBER (PRINT-LENGTH RED*TRACE_PREFIX NIL))) (PRINC RED*TRACE_PREFIX FILE))
	(FORMAT FILE "~T" (1+ RED*TRACE_END.POS))))

(DEFUN RED=TRACE_AGENDA.INSERT.CLAUSE (FILE RULE CLAUSE)
  ;; EDITED:  5-JUN-84 20:25:43
  ;; INPUT:  AN ATOM 'FILE', A REDUCTION RULE 'RULE', AND
  ;;         A CLAUSE 'CLAUSE'.
  ;; EFFECT: IF REDUCTION RULE IS SWITCHED ON, AGENDA,
  ;;         WHERE THE CLAUSE WILL BE INSERTED, IS
  ;;         PRINTED ON TERMINAL, IF 'FILE' IS 'T', ON
  ;;         CURRENT TRACE FILE ELSE.
  ;;         IF TRACE FILE IS NIL, REDUCTION TRACE WILL
  ;;         BE SWITCHED OFF.
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF
  ;;         'RED=CTL_AGENDA.INSERT.CLAUSE' SET BY
  ;;         'RED=TRACE_CTL'.
   (SETQ FILE (RED=TRACE_INIT FILE))
  (COND
    ((RDS-RULE 'CONDITION RULE)
      (RED=TRACE_WRITELN
        (NCONC (LIST 10 "CLAUSE" 10 (DS-PNAME CLAUSE) 16 "INTO AGENDA FOR" (PRINT-LENGTH RULE) RULE 2 ":")
          (MAPCAN (FUNCTION (LAMBDA (CLAUSE) (LIST (1+ (PRINT-LENGTH (DS-PNAME CLAUSE))) (DS-PNAME CLAUSE))))
            (RDS-RULE 'CLAUSE.AGENDA RULE)))
        FILE))))

(DEFUN RED=TRACE_AGENDA.INSERT.LINK (FILE RULE LINK)
  ;; EDITED:  5-JUN-84 20:30:43
  ;; INPUT:  AN ATOM 'FILE', A REDUCTION RULE 'RULE', AND
  ;;         A LINK 'LINK'.
  ;; EFFECT: IF REDUCTION RULE IS SWITCHED ON
  ;;         FOR LINK INSERTION, AGENDA,
  ;;         WHERE THE CLAUSE WILL BE INSERTED, IS
  ;;         PRINTED ON TERMINAL, IF 'FILE' IS 'T', ON
  ;;         CURRENT TRACE FILE ELSE.
  ;;         IF TRACE FILE IS NIL, REDUCTION TRACE WILL
  ;;         BE SWITCHED OFF.
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF
  ;;         'RED=CTL_AGENDA.INSERT.LINK' SET BY
  ;;         'RED=TRACE_CTL'.
   (SETQ FILE (RED=TRACE_INIT FILE))
  (COND
    ((RDS-RULE 'LINK RULE)
      (RED=TRACE_WRITELN
        (NCONC (LIST 10 "LINK" 10 (DS-PNAME LINK) 16 "INTO AGENDA FOR" (PRINT-LENGTH RULE) RULE 2 ":")
          (MAPCAN (FUNCTION (LAMBDA (LINK) (LIST (1+ (PRINT-LENGTH (DS-PNAME LINK))) (DS-PNAME LINK)))) (RDS-RULE 'LINK.AGENDA RULE)))
        FILE))))

(DEFUN RED=TRACE_AGENDA.INSERT.RECHECK (FILE RULE RECHECK)
  ;; EDITED:  5-JUN-84 20:30:43
  ;; INPUT:  AN ATOM 'FILE', A REDUCTION RULE 'RULE', AND
  ;;         A LINK 'RECHECK'.
  ;; EFFECT: IF RECHECK OF REDUCTION RULE IS
  ;;         SWITCHED ON TOTAL, AGENDA,
  ;;         WHERE THE LINK WILL BE INSERTED, IS
  ;;         PRINTED ON TERMINAL, IF 'FILE' IS 'T', ON
  ;;         CURRENT TRACE FILE ELSE.
  ;;         IF TRACE FILE IS NIL, REDUCTION TRACE WILL
  ;;         BE SWITCHED OFF.
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF
  ;;         'RED=CTL_AGENDA.INSERT.RECHECK' SET BY
  ;;         'RED=TRACE_CTL'.
   (SETQ FILE (RED=TRACE_INIT FILE))
  (COND
    ((EQL T (RDS-RULE 'RECHECK RULE))
      (RED=TRACE_WRITELN
        (NCONC (LIST 10 "RECHECK" 10 (DS-PNAME RECHECK) 16 "INTO AGENDA FOR" (PRINT-LENGTH RULE) RULE 2 ":")
          (MAPCAN
            (FUNCTION (LAMBDA (RECHECK) (LIST (1+ (PRINT-LENGTH (DS-PNAME RECHECK))) (DS-PNAME RECHECK))))
            (RDS-RULE 'RECHECK.AGENDA RULE)))
        FILE))))

(DEFUN RED=TRACE_RULE.BEFORE (FILE OBJECT.NAME RULE OBJECT)
  ;; EDITED:  5-JUN-84 20:37:31
  ;; INPUT:  'FILE' IS AN ATOM, 'OBJECT.NAME' ONE OF THE
  ;;         ATOMS 'CLAUSE', 'LINK', AND 'RECHECK'.
  ;;         'RULE' IS A REDUCTION RULE, MEMBER OF
  ;;         '(RDS-RULES 'ALL)'.
  ;;         'OBJECT' IS A CLAUSE OR A LINK, ACCORDING
  ;;         TO 'OBJECT.NAME'.
  ;;         IF 'OBJECT.NAME' IS 'RECHECK', LINK
  ;;         'RECHECK' HAS BEEN REMOVED FROM THE
  ;;         CONNECTIONGRAPH, BUT IS NOT DELETED IN THE
  ;;         MEMORY.
  ;; EFFECT: 'APPLY RULE TO OBJECT LINE' OF REDUCTION
  ;;         TRACE IS PRINTED ON THE TERMINAL IF 'FILE'
  ;;         IS 'T', ON CURRENT TRACE FILE ELSE.
  ;;         IF TRACE FILE IS NIL, REDUCTION
  ;;         TRACE WILL BE SWITCHED OFF.
  ;;         CURRENT VALUES OF USED TIME AND STORAGE ARE
  ;;         STORED INTO TWO COMMON VARIABLES FOR
  ;;         COMPUTATION OF USED TIME AND STORAGE, DURING
  ;;         THIS CALL OF REDUCTION RULE 'RULE' IN
  ;;         'RED=TRACE_RULE.AFTER'
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF
  ;;         'RED=APPLY:'RULE'' SET BY 'RED=TRACE_RULES'.
  (SETQ FILE (RED=TRACE_INIT FILE)) (TERPRI FILE)
  (RED=TRACE_WRITELN (LIST 6 "APPLY" 40 RULE 3 "TO" 10 OBJECT.NAME 1 "" 10 (DS-PNAME OBJECT)) FILE)
  (SETQ RED*TRACE_SPACE (RED=TRACE_STORAGE)) (SETQ RED*TRACE_TIME (GET-INTERNAL-RUN-TIME)))

(DEFUN RED=TRACE_RULE.AFTER (FILE OBJECT.NAME RULE OBJECT)
  (declare (ignore object.name object))
						; edited:  5-jun-84 20:56:45
						; input:  see 'RED=TRACE_RULE.BEFORE'.
						; effect: time and storage used during this call of
						;         reduction rule 'rule' are computed with
						;         the help of the stored values of time and
						;         storage at begin of the call.
						;         with this time and storage the statistic
						;         components of the rule are updated.
						;         the statistic line of reduction trace is
						;         printed for the call of rule 'rule'.
						; value:  undefined. 
						; remark: only used in the advise of
						;         'RED=APPLY_'rule'' set by 'RED=TRACE_RULES'.
  (SETQ FILE (RED=TRACE_INIT FILE))
  (let (OBJECTS)
    (declare (ignore objects))
    (SETQ RED*TRACE_TIME  (- (GET-INTERNAL-RUN-TIME) RED*TRACE_TIME))
    (SETQ RED*TRACE_SPACE (- (RED=TRACE_STORAGE) RED*TRACE_SPACE))
    (RDS-RULE.PUT 'SPACE RULE     (+ (RDS-RULE 'SPACE RULE) RED*TRACE_SPACE))
    (RDS-RULE.PUT 'TOT.SPACE RULE (+ (RDS-RULE 'TOT.SPACE RULE) RED*TRACE_SPACE))
    (RDS-RULE.PUT 'TOT.TIME RULE  (+ (RDS-RULE 'TOT.TIME RULE) RED*TRACE_TIME))
    (RDS-RULE.PUT 'TOT.TIMES RULE (+ (RDS-RULE 'TOT.TIMES RULE) 1))
    (RDS-RULE.PUT 'TIMES RULE     (+ (RDS-RULE 'TIMES RULE) 1))
    (RDS-RULE.PUT 'TIME RULE      (+ (RDS-RULE 'TIME RULE) RED*TRACE_TIME))
    (RED=TRACE_WRITELN (let ((STR "") (STRINGS NIL))
			 (MAPC #'(LAMBDA (KIND PRINT.INFO)
				   (SETQ STR (format nil "~A: ~A." kind (EVAL (CDR PRINT.INFO))))
				   (SETQ STRINGS (CONS (CAR PRINT.INFO) (CONS STR STRINGS))))
			       RED*TRACE_TRACES RED*TRACE_PRINTS)
			 (NCONC (LIST 2 "") STRINGS))
		       FILE)))

(DEFUN RED=TRACE_RESET NIL
  ;; EDITED: 24-MAR-84 01:28:17
  ;; INPUT:  -
  ;; EFFECT: ALL RULE COMPONENTS IN
  ;;         'RED*TRACE_ALL.STEPS.ENTRIES' OF ALL RULES
  ;;         ARE SET TO ZERO (STATISTICS INITIALIZATION
  ;;         FOR A WHOLE REFUTATION).
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF 'RED=RESET' SET
  ;;         BY 'RED=TRACE_RULES'.
  (MAPC
    (FUNCTION
      (LAMBDA (RULE)
        (MAPC (FUNCTION (LAMBDA (KIND) (RDS-RULE.PUT KIND RULE 0))) RED*TRACE_ALL.STEPS.ENTRIES)))
    (RDS-RULES 'ALL)))

(DEFUN RED=TRACE_END (FILE)
  ;; EDITED: 24-MAR-84 01:33:31
  ;; INPUT:  AN ATOM.
  ;; EFFECT: PRINTS A TABLE OF REDUCTION STATISTICS FOR
  ;;         THE CURRENTLY TERMINATING REFUTATION ON
  ;;         TERMINAL IF 'FILE' IS T OR ON CURRENT TRACE
  ;;         FILE ELSE. 
  ;;         IF TRACE FILE IS NIL, REDUCTION TRACE IS
  ;;         SWITCHED OFF.
  ;; VALUE:  UNDEFINED. 
  ;; REMARK: ONLY USED IN THE ADVISE OF 'RED-END' SET BY
  ;;         'RED=TRACE_RULES'.
   (SETQ FILE (RED=TRACE_INIT FILE))
  (RED=TRACE_WRITELN (LIST 65 "REDUCTION STATISTICS AFTER WHOLE REFUTATION: ") FILE)
  (RED=TRACE_PRINTTAB RED*TRACE_ALL.STEPS.ENTRIES FILE (RDS-RULES 'ALL)))

(DEFUN RED=TRACE_WRITELN (PRINT.DESCRIPTOR FILE)
  ;; EDITED:  5-JUN-84 21:02:27
  ;; INPUT:  'PRINT.DESCRIPTOR' IS A LIST WITH AN EVEN
  ;;         NUMBER OF ELEMENTS. THE ELEMENTS 2I+1 ARE
  ;;         NATURAL NUMBERS. 'FILE' IS THE CURRENT
  ;;         TRACE FILE OR T FOR TERMINAL.
  ;; EFFECT: FUNCTION IS FOR TABULATING, BEGINNING WITH
  ;;         THE NEXT LINE.
  ;;         AT FRONT OF EACH PRINTED LINE IS THE
  ;;         REDUCTION TRACE LINE PREFIX.
  ;;         THE ELEMENTS WITH ODD INDEX OF
  ;;         'PRINT.DESCRIPTOR' ARE PRINTED ONTO 'FILE'
  ;;         IN THEIR ORDER. FOR THE ELEMENT 2I 2I+1
  ;;         CHARACTERS ARE USED. IF THE ELEMENT 2I HAS
  ;;         LESS CHARACTERS, IT IS FILLED BY BLANKS.
  ;;         THE LAST CHARACTER OF EACH LINE MUST BE
  ;;         PRINTED ON POSITION 'RED*TRACE_END.POS'-1.
  ;; VALUE:  UNDEFINED. 
  (let ((W.POS (+ RED*TRACE_BEGIN.POS (PRINT-LENGTH RED*TRACE_PREFIX) 2)) LENGTH NEXTPR)
    (FORMAT FILE "~T" (1+ RED*TRACE_BEGIN.POS)) (PRINC RED*TRACE_PREFIX FILE) (PRINC " " FILE)
    (SMAPL
      (FUNCTION
        (LAMBDA (PRTAIL)
	  (SETQ W.POS 0)
	  (SETQ NEXTPR (SECOND PRTAIL)) (SETQ LENGTH (CAR PRTAIL))
          (COND
            ((< (1+ (- RED*TRACE_END.POS W.POS)) LENGTH) (FORMAT FILE "~T" (1+ RED*TRACE_END.POS))
	     (FORMAT FILE "~T" (1+ RED*TRACE_BEGIN.POS)) (PRINC RED*TRACE_PREFIX FILE) (PRINC "   " FILE)
	     (SETQ W.POS 0)))
          (PRINC NEXTPR FILE) (FORMAT FILE "~T" (+ W.POS LENGTH))))
      (FUNCTION CDDR) PRINT.DESCRIPTOR)
    (FORMAT FILE "~T" (1+ RED*TRACE_END.POS))))

(DEFUN RED=TRACE_PRINTTAB (INDICATORS FILE RULES)
  ;; EDITED: 24-MAR-84 02:03:47
  ;; INPUT:  'INDICATORS' IS AN ASSOCIATIONLIST, WHICH
  ;;         CARS ARE ELEMENTS OF 'RED*TRACE_TRACES',
  ;;         CDRS ARE STATISTIC INDICATORS OF REDUCTION
  ;;         RULES.
  ;;         'FILE' IS THE CURRENT TRACE FILE, OPEN FOR
  ;;         OUTPUT.
  ;;         'RULES' IS A LIST OF REDUCTION RULES.
  ;; EFFECT: PRINTS A TABLE.
  ;;         COLUMNS ARE DETERMINED BY THE STATISTIC
  ;;         KINDS ('RED*TRACE_TRACES'), ROWS BY THE
  ;;         RULES 'RULES', EACH ROW BEGINS WITH THE
  ;;         REDUCTION PREFIX.
  ;;         IF ALL ENTRIES OF THE TABLE BECOME ZERO IT
  ;;         WILL BE PRINTED NOTHING BUT AN INFORMATION
  ;;         LINE. ALSO ROWS WITH ONLY ZERO ENTRIES ARE
  ;;         OMITTED.
  ;; VALUE:  UNDEFINED. 
  ;; EXAMPLE:'INDICATORS' = (TOT.TIME TOT.TIMES
  ;;                         TOT.SPACE)
  ;;         'RULES'      = (RED*CLAUSE.PURITY)
  ;;         TABLE (WITHOUT PREFIX):
  ;;          RULES           !TIME!TIMES!SPACE
  ;;         -----------------+----+-----+------
  ;;         RED*CLAUSE.PURITY!33  !2    !40
  (COND
    ((NOT
       (EVERY RULES
         (FUNCTION
           (LAMBDA (RULE)
             (EVERY (FUNCTION (LAMBDA (INDICATOR) (ZEROP (RDS-RULE INDICATOR RULE)))) INDICATORS)))))
      ;; NOT ALL ENTRIES ARE ZERO.
      (PROG
        ((BLANK (CHARACTER 64)) (NT (LIST-LENGTH RED*TRACE_TRACES)) (SEP '!)
          (* "COMPUTING POSITIONS FOR TABLE SLASHES.              ")
          (TABULATOR
            (RED=TRACE_COMPUTE.TABULATOR (+ RED*TRACE_BEGIN.POS 1 (PRINT-LENGTH RED*TRACE_PREFIX NIL))
              RED*TRACE_END.POS
              (CONS (CONS 'RULE RULES)
                (MAPCAR
                  (FUNCTION
                    (LAMBDA (KIND) (CONS KIND (MAPCAR (FUNCTION (LAMBDA (RULE) (RDS-RULE KIND RULE))) RULES))))
                  INDICATORS))))
          POS)
        ;; PRINTING HEADING LINE OF TABLE.
   (SETQ POS 2)
        (FORMAT FILE "~T" (1+ RED*TRACE_BEGIN.POS)) (PRINC RED*TRACE_PREFIX FILE)
        (PRINTTAB
          (NCONC (LIST (AREF TABULATOR 1) BLANK (LIST 'RULE) (AREF TABULATOR 2) SEP)
            (MAPCON
              (FUNCTION
                (LAMBDA (TRACE.KINDS)
                  (LIST (LIST (CAR TRACE.KINDS)) (AREF TABULATOR (SETQ POS (1+ POS))) (COND ((CDR TRACE.KINDS) SEP) (T BLANK)))))
              RED*TRACE_TRACES))
          FILE)
        (FORMAT FILE "~T" (1+ RED*TRACE_END.POS))
  ;; PRINTING DASH ROW. 
        (PROG
          ((STR "-")
            (DASH.STRING "-------------------------------------------------------------------------------"))
          (DODOWN (RPTN (1+ NT))
            (SETQ STR
              (CONCATENATE 'STRING (PRINC-TO-STRING (COND ((EQL (1+ RPTN) 1) "-") (T "+")))
                (PRINC-TO-STRING
                  (SUBSEQ DASH.STRING (1- 1) (1- (- (AREF TABULATOR (1+ (1+ RPTN))) (AREF TABULATOR (1+ RPTN))))))
                (PRINC-TO-STRING STR))))
          (RED=TRACE_WRITELN (LIST (1+ (PRINT-LENGTH STR NIL)) STR) FILE))
        ;; PRINTING TABLE LINES FOR THE RULES, WITH NOT ALL
        ;; ENTRIES ZERO.
        (MAPC
          (FUNCTION
            (LAMBDA (RULE)
              (COND
                ((NOT (EVERY INDICATORS (FUNCTION (LAMBDA (INDICATOR) (ZEROP (RDS-RULE INDICATOR RULE)))))) (SETQ POS 2)
                  (FORMAT FILE "~T" (1+ RED*TRACE_BEGIN.POS)) (PRINC RED*TRACE_PREFIX FILE)
                  (PRINTTAB
                    (NCONC (LIST (AREF TABULATOR 1) BLANK (LIST RULE) (AREF TABULATOR 2) SEP)
                      (MAPCON
                        (FUNCTION
                          (LAMBDA (TRACE.KINDS)
                            (LIST (LIST (RDS-RULE (CAR TRACE.KINDS) RULE)) (AREF TABULATOR (SETQ POS (1+ POS)))
                              (COND ((CDR TRACE.KINDS) SEP) (T BLANK)))))
                        INDICATORS))
                    FILE)
                  (FORMAT FILE "~T" (1+ RED*TRACE_END.POS))))))
          RULES)
        (TERPRI FILE)))
    (T ;; PRINTING INFORMATION LINE.
      (RED=TRACE_WRITELN (LIST 60 "  NO OBJECT TO BE REDUCED GIVEN TO REDUCTION MODULE.") FILE))))

(DEFUN RED=TRACE_COMPUTE.TABULATOR (BEGIN.POS END.POS ENTRIES.LISTS)
  ;; EDITED: 23-MAR-84 13:37:01
  ;; INPUT:  'ENTRIES.LISTS' IS A LIST OF LISTS OF
  ;;         S-EXPRESSIONS,
  ;;         'BEGIN.POS' < 'END.POS' - LENGTH OF
  ;;         'ENTRIES.LISTS' ARE TWO INTEGER NUMBERS.
  ;;         'BEGIN.POS' AND 'END.POS' WILL BE THE
  ;;         LEFT AND RIGHT MARGIN OF A TABLE WHICH
  ;;         COLUMNS ARE THE LISTS OF 'ENTRIES.LISTS'.
  ;; EFFECT: -
  ;; VALUE:  AN ARRAY WITH LENGTH OF 'ENTRIES.LISTS' + 1
  ;;         CELLS, WHERE ARRAY(1) = 'BEGIN.POS'.
  ;;         IF POSSIBLE:
  ;;         CELL I CONTAINS AN INTEGER (POSITION OF
  ;;         TABLE SLASH) WITH ARRAY(I+1) - ARRAY(I) =
  ;;         LENGTH ('NCHARS') OF LONGEST ELEMENT IN I-TH
  ;;         LIST OF 'ENTRIES.LISTS' (I = 1 ... !ARRAY! -
  ;;         1), AND ARRAY(!ARRAY!) <= 'END.POS'.
  ;;         ELSE:
  ;;         THE DIFFERENCE OF CONTENTS OF ARRAY ARE
  ;;         EQUAL FOR I <> 1,REMAINDER IS ADDED TO THE
  ;;         DIFFERENCE (ARRAY(2) - ARRAY(1)). HERE
  ;;         ARRAY(!ARRAY!) = 'END.POS'.
  ;; EXAMPLE:'BEGIN.POS'       = 2
  ;;         'END.POS'         = 12
  ;;         'ENTRIES.LISTS'   = ((A AA)(AAA A))
  ;;         'VALUE'          := !2!5!9!
  (PROG ((TABLENGTH (LIST-LENGTH ENTRIES.LISTS)) (NUSED 0) NALL TABULATOR NUMBER ENTRY.LENGTH)
    (SETQ NALL (+ END.POS (- BEGIN.POS) (- TABLENGTH))) (SETQ TABULATOR (MAKE-ARRAY (LIST (1+ TABLENGTH)))) (SETQ NUMBER 1)
    ;; AFTER THIS MAPC ARRAY(I) = LENGTH OF LONGEST
    ;; ELEMENT IN I-TH LIST OF 'ENTRIES.LISTS'.
    (MAPC
      (FUNCTION
        (LAMBDA (ENTRIES.LIST)
          (MAPC
            (FUNCTION
              (LAMBDA (ENTRY)
                (COND
                  ((< (AREF TABULATOR NUMBER) (SETQ ENTRY.LENGTH (PRINT-LENGTH ENTRY NIL)))
                    (SETF (AREF TABULATOR NUMBER) ENTRY.LENGTH)))))
            ENTRIES.LIST)
          (SETQ NUMBER (1+ NUMBER))))
      ENTRIES.LISTS)
    (DODOWN (RPTN TABLENGTH) (SETQ NUSED (+ NUSED (AREF TABULATOR (1+ RPTN)))))
    ;; ELSE-CASE OF 'VALUE': NOT ENOUGH POSITIONS, ALL
    ;; LENGTHS ARE SET EQUAL, BUT THE FIRST.
    (COND
      ((< NALL NUSED)
        (PROG ((WIDTH (TRUNCATE NALL TABLENGTH)) (REST (REM NALL TABLENGTH)))
          (DODOWN (RPTN TABLENGTH) (SETF (AREF TABULATOR (1+ RPTN)) WIDTH)) (SETF (AREF TABULATOR 1) (+ REST WIDTH)))))
    ;; COMPUTING POSITIONS FROM LENGTHS, LAST POSITION
    ;; DEFINED BY 'BEGIN.POS' AND NUMBER OF USED POSITIONS.
    (SETF (AREF TABULATOR (1+ TABLENGTH)) (+ TABLENGTH 1 NUSED BEGIN.POS))
    (DODOWN (RPTN TABLENGTH)
        (SETF (AREF TABULATOR (1+ RPTN)) (+ (AREF TABULATOR (1+ (1+ RPTN))) (- (AREF TABULATOR (1+ RPTN))) -1)))
    (RETURN TABULATOR)))

(DEFUN RED=TRACE_STORAGE NIL
  ;; EDITED: 23-MAR-84 13:35:30
  ;; INPUT:  -
  ;; EFFECT: -
  ;; VALUE:  STORAGE IN BYTES USED FOR CONSCELLS AND
  ;;         ARRAYS.
  (+ (* 4 (remaining-memory)) (* 10 0)))

(DEFUN RED=TRACE_INIT (FILE)
  ;; EDITED: 21-MAR-84 20:47:56
  ;; INPUT:  AN ATOM.
  ;; EFFECT: 'RED*TRACE_END.POS' IS SET TO
  ;;         76                 IF 'FILE' IS 'T'
  ;;                            (TERMINAL),
  ;;         LINELENGTH-1       IF CURRENT TRACE FILE IS
  ;;                            OPEN FOR OUTPUT OR NIL,
  ;;         125                IF TRACE FILE IS CLOSED.
  ;;         REDUCTION TRACE IS SWITCHED OFF IF TRACE
  ;;         FILE IS NIL.
  ;; VALUE:  T                  IF 'FILE' IS 'T'
  ;;                            (TERMINAL),
  ;;         CURRENT TRACE FILE ELSE.
  (SETQ FILE (CASE FILE
	       ((T) FILE)
	       (OTHERWISE (OPT-GET.OPTION TR_TRACE.FILE))))
  (CASE FILE
    ((NIL) (mapcar #'(lambda (f) (EVAL `(scl:UNADVISE ,f)))
		   `(RED=CTL_AGENDA.INSERT.RECHECK
		      RED=CTL_AGENDA.INSERT.LINK RED=CTL_AGENDA.INSERT.CLAUSE
		      RED-END  RED=RESET RED=CTL_APPLY.RULES 
		      ,@(mapcar #'(lambda (rule)
				    (MAPC #'(LAMBDA (OBJECT.TYPE)
					      (PROG (FUNCTION.INDICATOR)
						    (SETQ FUNCTION.INDICATOR
							  (INTERN (format nil "APPLY.TO.~A.FUNCTION" OBJECT.TYPE)
								  (find-package "MKRP")))
						    (RDS-RULE FUNCTION.INDICATOR RULE)))
					  '(CLAUSE LINK RECHECK)))
				(rds-rules 'all)))))
    (OTHERWISE NIL))
  (SETQ RED*TRACE_END.POS
    (COND ((EQL FILE T) 76)
	  (T 125)))
  FILE)

(defmacro red-check (switch)
  `(red==check ',switch))

(DEFUN RED==CHECK (SWITCH)
  ;; EDITED: 21-MAR-84 20:29:31
  ;; INPUT:  'SWITCH' IS ONE OF THE ATOMS 'ON', 'T', AND
  ;;         'OFF'.
  ;; EFFECT: SWITCHS ON('ON', 'T') OR OFF PLAUSIBILITY
  ;;         CHECK FOR SOME IMPORTANT AND FREQUENTLY
  ;;         USED FUNCTIONS OF REDUCTION MODULE.
  ;; VALUE:  READY- OR ERRORMESSAGE FOR THE HUMAN USER
  ;;         OF THIS SWITCHFUNCTION.
  (CASE SWITCH
    ((ON T)
     (global:ADVISE RED=CTL_REDUCE BEFORE red 0 (RED=CHECK_CTL.REDUCE scl:ARGLIST))
     (global:ADVISE RED.DS==RULE BEFORE red 0 (RED=CHECK_DS.RULE scl:ARGLIST))
     (global:ADVISE RED.DS==RULE.PUT BEFORE red 0 (RED=CHECK_DS.RULE.PUT scl:ARGLIST))
     (global:ADVISE RED.DS==MARK BEFORE red 0 (RED=CHECK_DS.MARK scl:ARGLIST))
     (global:ADVISE RED.DS==MARK.PUT BEFORE red 0 (RED=CHECK_DS.MARK.PUT scl:ARGLIST))
     '(RED==CHECK SWITCHED ON.))
    (OFF (global:unadvise RED.DS==RULE)
	 (global:unadvise RED.DS==RULE.PUT)
	 (global:unadvise RED.DS==MARK)
	 (global:unadvise RED.DS==MARK.PUT)
	 (global:unadvise RED=CTL_REDUCE)
	 '(RED-CHECK SWITCHED OFF.))
    (OTHERWISE '(ILLEGAL ARGUMENT. TRY (RED-CHECK ON) OR (RED-CHECK OFF)))))

(DEFUN RED=CHECK_CTL.REDUCE
  (PREDICATE.UPDATE.FLAG STATE REMOVE.CLAUSES REMOVE.LITERALS REMOVE.UNIS INHIBIT.UNIS REDUCE.CLAUSES
    REDUCE.LINKS RULES.ON RULES.SUPPRESSED)
  ;; EDITED: 21-MAR-84 20:33:26
  ;; INPUT:  SAME AS 'RED=CTL_REDUCE'.
  ;; EFFECT: ERRORBREAK IF THIS FUNCTION AS ADVISE OF
  ;;         'RED=CTL_REDUCE' FINDS A FAILURE IN THE
  ;;         ARGUMENTS OF THIS CENTRAL REDUCTION FUNCTION
  ;;         USED BY MOST OF REDUCTION INTERFACES.
  ;; REMARK: USED ONLY IN THE ADVISE OF 'RED=CTL_REDUCE'
  ;;         BY 'RED-CHECK'.
  ;; VALUE:  UNDEFINED.
  (declare (ignore predicate.update.flag))
  (MAPC
    (FUNCTION
      (LAMBDA (RULE)
        (COND
          ((NOT (MEMBER RULE (RDS-RULES 'ALL))) (ERROR "RED-CHECK - ILLEGAL RULE IN RED=CTL_REDUCE: : ~A" RULE)))))
    (APPEND RULES.ON RULES.SUPPRESSED))
  (MAPC
    (FUNCTION
      (LAMBDA (REDUCE.CLAUSE)
        (COND
          ((NOT (DS-CLAUSE.IS REDUCE.CLAUSE))
            (ERROR "RED-CHECK - CLAUSE TO BE REDUCED IS NO CLAUSE IN RED=CTL_REDUCE: : ~A" REDUCE.CLAUSE))
          ((NOT (MEMBER REDUCE.CLAUSE (CG-CLAUSES ALL)))
            (ERROR "RED-CHECK - CLAUSE TO BE REDUCED IS NOT IN CONNECTIONGRAPH IN RED=CTL_REDUCE: : ~A"
              REDUCE.CLAUSE)))))
    REDUCE.CLAUSES)
  (MAPC
    (FUNCTION
      (LAMBDA (REDUCE.LINK)
        (COND
          ((NOT (DS-LINK.IS REDUCE.LINK))
            (ERROR "RED-CHECK - LINK TO BE REDUCED IS NO LINK IN RED=CTL_REDUCE: : ~A" REDUCE.LINK))
          ((NOT (MEMBER REDUCE.LINK (CG-LINKS (DS-LINK.COLOUR REDUCE.LINK) ALL)))
            (ERROR "RED-CHECK - LINK TO BE REDUCED IS NOT IN CONNECTIONGRAPH IN RED=CTL_REDUCE: : ~A"
              REDUCE.LINK)))))
    REDUCE.LINKS)
  (MAPC
    (FUNCTION
      (LAMBDA (REMOVE.CLAUSE.DESCRIPTOR)
        (COND
          ((CONSP REMOVE.CLAUSE.DESCRIPTOR)
            (COND
              ((NOT (DS-CLAUSE.IS (CAR REMOVE.CLAUSE.DESCRIPTOR)))
                (ERROR "RED-CHECK - CLAUSE TO BE REMOVED IS NO CLAUSE IN RED=CTL_REDUCE: : ~A"
                  (CAR REMOVE.CLAUSE.DESCRIPTOR)))
              ((NOT (MEMBER (CAR REMOVE.CLAUSE.DESCRIPTOR) (CG-CLAUSES ALL)))
                (ERROR "RED-CHECK - CLAUSE TO BE REMOVED IS NOT IN CONNECTIONGRAPH IN RED=CTL_REDUCE: : ~A"
                  (CAR REMOVE.CLAUSE.DESCRIPTOR)))))
          (T
            (ERROR "RED-CHECK - BAD FORMAT OF DESCRIPTOR OF CLAUSE TO BE REMOVED IN RED=CTL_REDUCE: : ~A"
              REMOVE.CLAUSE.DESCRIPTOR)))))
    REMOVE.CLAUSES)
  (MAPC
    (FUNCTION
      (LAMBDA (REMOVE.LITERAL.DESCRIPTOR)
        (COND
          ((CONSP REMOVE.LITERAL.DESCRIPTOR)
            (COND
              ((NOT (DS-CLAUSE.IS (CAR REMOVE.LITERAL.DESCRIPTOR)))
                (ERROR "RED-CHECK - CLAUSE OF LITERAL TO BE REMOVED IS NO CLAUSE IN RED=CTL_REDUCE: : ~A"
                  (CAR REMOVE.LITERAL.DESCRIPTOR)))
              ((NOT (MEMBER (CAR REMOVE.LITERAL.DESCRIPTOR) (CG-CLAUSES ALL)))
                (ERROR
                  "RED-CHECK - CLAUSE OF LITERAL TO BE REMOVED IS NOT IN CONNECTIONGRAPH IN RED=CTL_REDUCE: ~A"
                  (CAR REMOVE.LITERAL.DESCRIPTOR)))))
          (T
            (ERROR "RED-CHECK - BAD FORMT OF DESCRIPTOR OF LITERAL TO BE REMOVED IN RED=CTL_REDUCE: : ~A"
              REMOVE.LITERAL.DESCRIPTOR)))))
    REMOVE.LITERALS)
  (MAPC
    (FUNCTION
      (LAMBDA (REMOVE.UNI.DESCRIPTOR)
        (COND
          ((CONSP REMOVE.UNI.DESCRIPTOR)
            (COND
              ((NOT (DS-LINK.IS (CAR REMOVE.UNI.DESCRIPTOR)))
                (ERROR "RED-CHECK - LINK TO BE REMOVED IS NO LINK IN RED=CTL_REDUCE: : ~A"
                  (CAR REMOVE.UNI.DESCRIPTOR)))
              ((NOT
                 (MEMBER (CAR REMOVE.UNI.DESCRIPTOR) (CG-LINKS (DS-LINK.COLOUR (CAR REMOVE.UNI.DESCRIPTOR)) ALL)))
                (ERROR "RED-CHECK - LINK TO BE REMOVED IS NOT IN CONNECTIONGRAPH IN RED=CTL_REDUCE: : ~A"
                  (CAR REMOVE.UNI.DESCRIPTOR)))))
          (T
            (ERROR "RED-CHECK - BAD FORMAT OF DESCRIPTOR OF LINK TO BE REMOVED IN RED=CTL_REDUCE: : ~A"
              REMOVE.UNI.DESCRIPTOR)))))
    REMOVE.UNIS)
  (MAPC
    (FUNCTION
      (LAMBDA (INHIBIT.UNI.DESCRIPTOR)
        (COND
          ((CONSP INHIBIT.UNI.DESCRIPTOR)
            (COND
              ((NOT (DS-LINK.IS (CAR INHIBIT.UNI.DESCRIPTOR)))
                (ERROR "RED-CHECK - LINK TO BE INHIBITED IS NO LINK IN RED=CTL_REDUCE: : ~A"
                  (CAR INHIBIT.UNI.DESCRIPTOR)))
              ((NOT
                 (MEMBER (CAR INHIBIT.UNI.DESCRIPTOR)
                   (CG-LINKS (DS-LINK.COLOUR (CAR INHIBIT.UNI.DESCRIPTOR)) ALL)))
                (ERROR "RED-CHECK - LINK TO BE INHIBITED IS NOT IN CONNECTIONGRAPH IN RED=CTL_REDUCE: : ~A"
                  (CAR INHIBIT.UNI.DESCRIPTOR)))))
          (T
            (ERROR "RED-CHECK - BAD FORMAT OF DESCRIPTOR OF LINK TO BE INHIBITED IN RED=CTL_REDUCE: : ~A"
              INHIBIT.UNI.DESCRIPTOR)))))
    INHIBIT.UNIS)
  (COND
    ((NOT (MEMBER STATE '(INITIAL DEDUCED NEUTRAL))) (ERROR "RED-CHECK - ILLEGAL STATE IN RED=CTL_REDUCE: : ~A" STATE))))

(DEFUN RED=CHECK_DS.RULE (INDICATOR RULE)
  ;; EDITED: 21-MAR-84 20:37:21
  ;; INPUT:  SAME AS 'RDS-RULE'.
  ;; EFFECT: CHECKS INPUT OF 'RDS-RULE' IN AN
  ;;         ADVISE OF THIS FUNCTION,
  ;;         SOME CHECKS ARE DONE IMLICITELY IN
  ;;         'RDS-RULE',
  ;;         ERROR LEADS TO AN ERRORBREAK.
  ;; REMARK: ONLY USED IN THE ADVISE OF 'RDS-RULE'
  ;;         SET BY 'RED-CHECK'.
  ;; VALUE:  UNDEFINED.
  (declare (ignore indicator))
  (COND ((NOT (MEMBER RULE (RDS-RULES 'ALL))) (ERROR "RED-CHECK - ILLEGAL RULE IN RDS-RULE: : ~A" RULE))))

(DEFUN RED=CHECK_DS.RULE.PUT (INDICATOR RULE VALUE)
  ;; EDITED: 21-MAR-84 20:44:26
  ;; INPUT:  SAME AS 'RDS-RULE.PUT'.
  ;; EFFECT: CHECKS INPUT OF 'RDS-RULE.PUT' IN AN
  ;;         ADVISE OF THIS FUNCTION,
  ;;         SOME CHECKS ARE DONE IMLICITELY IN
  ;;         'RDS-RULE.PUT',
  ;;         ERROR LEADS TO AN ERRORBREAK.
  ;; REMARK: ONLY USED IN THE ADVISE OF 'RDS-RULE.PUT'
  ;;         SET BY 'RED-CHECK'.
  ;; VALUE:  UNDEFINED. 
  (COND
    ((NOT (MEMBER RULE (RDS-RULES 'ALL))) (ERROR "RED-CHECK - ILLEGAL RULE IN RDS-RULE.PUT: : ~A" RULE)))
  (COND
    ((AND (MEMBER INDICATOR '(TIME TOT.TIME TIMES TOT.TIMES SPACE TOT.SPACE)) (NOT (INTEGERP VALUE)))
      (ERROR "RED-CHECK - ILLEGAL VALUE IN RDS-RULE.PUT, VALUE MUST BE INTEGER IN ARRAY: : ~A" VALUE))))

(DEFUN RED=CHECK_DS.MARK (MARK.ARRAY INDEX VALUE)
  ;; EDITED: 21-MAR-84 20:43:48
  ;; INPUT:  SAME AS 'RDS-MARK'.
  ;; EFFECT: ERRORBREAK IF ARGUMENTS OF 'RDS-MARK'
  ;;         DO NOT HANDLE CORRECTLY WITH THE ARRAY
  ;;         'MARK.ARRAY'.
  ;; REMARK: USED ONLY IN THE ADVISE OF 'RDS-MARK'
  ;;         SET BY 'RED-CHECK'.
  ;; VALUE:  UNDEFINED.
  (declare (ignore value))
  (COND
    ((NOT (ARRAYP MARK.ARRAY)) (ERROR "RED-CHECK - ILLEGAL ARRAY ARGUMENT IN RDS-MARK: : ~A" MARK.ARRAY)))
  (COND ((< INDEX 1) (ERROR "RED-CHECK - ARRAY INDEX TO LESS IN RDS-MARK: : ~A" INDEX)))
  (COND
    ((< (ARRAY-DIMENSION MARK.ARRAY 1) INDEX) (ERROR "RED-CHECK - ARRAY INDEX TO GREAT IN RDS-MARK: : ~A" INDEX))))

(DEFUN RED=CHECK_DS.MARK.PUT (MARK.ARRAY INDEX VALUE)
  ;; EDITED: 21-MAR-84 20:49:48
  ;; INPUT:  SAME AS 'RDS-MARK.PUT'.
  ;; EFFECT: ERRORBREAK IF ARGUMENTS OF 'RDS-MARK.PUT'
  ;;         DO NOT HANDLE CORRECTLY WITH THE ARRAY
  ;;         'MARK.ARRAY'.
  ;; REMARK: USED ONLY IN THE ADVISE OF 'RDS-MARK.PUT'
  ;;         SET BY 'RED-CHECK'.
  ;; VALUE:  UNDEFINED.
  (declare (ignore value))
  (COND
    ((NOT (ARRAYP MARK.ARRAY)) (ERROR "RED-CHECK - ILLEGAL ARRAY ARGUMENT IN RDS-MARK.PUT: : ~A" MARK.ARRAY)))
  (COND ((< INDEX 1) (ERROR "RED-CHECK - ARRAY INDEX TO LESS IN RDS-MARK.PUT: : ~A" INDEX)))
  (COND
    ((< (ARRAY-DIMENSION MARK.ARRAY 1) INDEX)
      (ERROR "RED-CHECK - ARRAY INDEX TO GREAT IN RDS-MARK.PUT: : ~A" INDEX))))

