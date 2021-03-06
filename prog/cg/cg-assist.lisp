;; -*- Package: MKRP; Syntax: Common-lisp; Mode: lisp -*-

(in-package "MKRP")

(defmacro cg-check (switch)
  `(cg=check ',switch))

(DEFUN CG=CHECK (SWITCH)
						; EDITED:  3. 3. 1982   HJO
						; INPUT:   ONE OF THE ATOMS ON OR OFF.
						; EFFECT:  CAUSES ALL FUNCTIONS TO PERFORM
						;          PLAUSIBILITY CHECKS ON THEIR ARGUMENTS.
						; VALUE:   MESSAGE ABOUT KIND OF SWITCH PERFORMED.
  (CASE SWITCH
    (ON (GLOBAL:ADVISE CG-CLAUSES BEFORE cg 0 (CG=CHECK_CLAUSES CG.INDICATOR))
	(GLOBAL:ADVISE CG-#CLAUSES BEFORE cg 0 (CG=CHECK_#CLAUSES CG.INDICATOR))
	(GLOBAL:ADVISE CG-LINKS BEFORE cg 0 (CG=CHECK_LINKS CG.COLOUR CG.INDICATOR))
	(GLOBAL:ADVISE CG-#LINKS BEFORE cg 0 (CG=CHECK_#LINKS CG.COLOUR CG.INDICATOR))
	(GLOBAL:ADVISE CG-INSERT.CLAUSE BEFORE cg 0 (CG=CHECK_INSERT.CLAUSE CLAUSE CREATOR.UNIFIER LIST.OF.ANCESTORS))
	(GLOBAL:ADVISE CG=INSERT.LINK BEFORE cg 0 (CG=CHECK_INSERT.LINK LINK ANCESTORLINKS))
	(GLOBAL:ADVISE CG-REMOVE.CLAUSE BEFORE cg 0 (CG=CHECK_REMOVE.CLAUSE CLAUSE))
	(GLOBAL:ADVISE CG=REMOVE.LINK BEFORE cg 0 (CG=CHECK_REMOVE.LINK LINK))
	(GLOBAL:ADVISE CG-REMOVE.UNIFIER BEFORE cg 0 (CG=CHECK_REMOVE.UNIFIER LINK UNIFIER))
	(GLOBAL:ADVISE CG=INSERT.UNIFIER BEFORE cg 0 (CG=CHECK_INSERT.UNIFIER LINK UNIFIER))
	(GLOBAL:ADVISE CG-REMOVE.LITERAL BEFORE cg 0 (CG=CHECK_REMOVE.LITERAL CLAUSE LITNO SIBLINGLITNO))
	(eval `(scl:ADVISE CG-REPLACE.LITERAL BEFORE cg 0 ,(CONS 'CG=CHECK_REPLACE.LITERAL (scl:ARGLIST 'CG-REPLACE.LITERAL))))
	(GLOBAL:ADVISE CG-CLAUSE_ANCESTOR.LITERALS BEFORE cg 0 CG=CHECK_CLAUSE_ANCESTOR.LITERALS)
	(GLOBAL:ADVISE CG-CLAUSE_DESCENDANT.LITERALS BEFORE cg 0 CG=CHECK_CLAUSE_DESCENDANT.LITERALS)
	(GLOBAL:ADVISE CG-LINK_ANCESTOR.LINKS BEFORE cg 0 CG=CHECK_LINK_ANCESTOR.LINKS)
	(GLOBAL:ADVISE CG-LINK_DESCENDANT.LINKS BEFORE cg 0 CG=CHECK_LINK_DESCENDANT.LINKS)
	'(CG-CHECK SWITCHED ON))
    (OFF (EVAL (CONS 'GLOBAL:UNADVISE '(CG-CLAUSES
					 CG-#CLAUSES
					 CG-LINKS
					 CG-#LINKS
					 CG-INSERT.CLAUSE
					 CG=INSERT.LINK
					 CG-REMOVE.CLAUSE
					 CG=REMOVE.LINK
        				 CG-REMOVE.UNIFIER
					 CG=INSERT.UNIFIER
					 CG-REMOVE.LITERAL
					 CG-REPLACE.LITERAL
					 CG-CLAUSE_ANCESTOR.LITERALS
					 CG-CLAUSE_DESCENDANT.LITERALS
					 CG-LINK_ANCESTOR.LINKS
					 CG-LINK_DESCENDANT.LINKS)))
	 '(CG-CHECK SWITCHED OFF))
    (OTHERWISE '(ILLEGAL ARGUMENT! TRY (CG-CHECK ON) OR (CG-CHECK OFF)))))

(DEFUN CG=CHECK_CLAUSES (INDICATOR)
						; EDITED:  2. 3. 1982  HJO
						; EFFECT:  CHECKS IF INDICATOR IS ONE OF THE
						;          FOLLOWING ATOMS: ALL, REMOVED, INSERTED
						;          OR CHANGED.
  (COND ((MEMBER INDICATOR '(ALL REMOVED INSERTED CHANGED)))
	(T (ERROR "CG-CHECK - ARGUMENT IN CG-CLAUSES NO LEGAL INDICATOR:: ~A" INDICATOR))))

(DEFUN CG=CHECK_LINKS (COLOUR* INDICATOR)
						; EDITED:  2. 3. 1982  HJO
						; EFFECT:  CHECKS IF INDICATOR AND COLOUR ARE
						;          PERMISSIBLE ATOMS.
  (COND ((MEMBER INDICATOR '(ALL REMOVED INSERTED CHANGED)))
	(T (ERROR "CG-CHECK - ARGUMENT IN CG-LINKS NO LEGAL INDICATOR:: ~A" INDICATOR)))
  (COND
    ((OR
       (AND (ATOM COLOUR*)
	    (OR (MEMBER COLOUR* (DS-LINK.COLOURS.FOR 'ALL)) (MEMBER (TESTEVAL (EVAL COLOUR*)) (DS-LINK.COLOURS.FOR 'ALL))
		(AND (CONSP (TESTEVAL (EVAL COLOUR*)))
		     (EVERY (FUNCTION (LAMBDA (COL) (MEMBER COL (DS-LINK.COLOURS.FOR 'ALL)))) (TESTEVAL (EVAL COLOUR*))))))
       (AND (CONSP COLOUR*)
	    (OR (EVERY (FUNCTION (LAMBDA (COL) (MEMBER COL (DS-LINK.COLOURS.FOR 'ALL)))) COLOUR*)
		(MEMBER (TESTEVAL (EVAL COLOUR*)) (DS-LINK.COLOURS.FOR 'ALL))
		(AND (CONSP (TESTEVAL (EVAL COLOUR*)))
		     (EVERY (FUNCTION (LAMBDA (COL) (MEMBER COL (DS-LINK.COLOURS.FOR 'ALL)))) (TESTEVAL (EVAL COLOUR*))))))))
    (T (ERROR "CG-CHECK - ARGUMENT IN CG-LINKS NO LEGAL COLOUR(S):: ~A" COLOUR*))))

(DEFUN CG=CHECK_#CLAUSES (INDICATOR)
						; EDITED:  2. 3. 1982  HJO
						; EFFECT:  CHECKS IF INDICATOR IS ONE OF THE
						;          FOLLOWING ATOMS: ALL, REMOVED, INSERTED
						;          OR CHANGED.
  (COND ((MEMBER INDICATOR '(ALL REMOVED INSERTED CHANGED)))
	(T (ERROR "CG-CHECK - ARGUMENT IN CG-#CLAUSES NO LEGAL INDICATOR:: ~A" INDICATOR))))

(DEFUN CG=CHECK_#LINKS (COLOUR* INDICATOR)
						; EDITED:  2. 3. 1982  HJO
						; EFFECT:  CHECKS IF INDICATOR AND COLOUR ARE
						;          PERMISSIBLE ATOMS.
  (COND ((MEMBER INDICATOR '(ALL REMOVED INSERTED CHANGED)))
	(T (ERROR "CG-CHECK - ARGUMENT IN CG-#LINKS NO LEGAL INDICATOR:: ~A" INDICATOR)))
  (COND
    ((OR
       (AND (ATOM COLOUR*)
	    (OR (MEMBER COLOUR* (DS-LINK.COLOURS.FOR 'ALL)) (MEMBER (TESTEVAL (EVAL COLOUR*)) (DS-LINK.COLOURS.FOR 'ALL))
		(AND (CONSP (TESTEVAL (EVAL COLOUR*)))
		     (EVERY (FUNCTION (LAMBDA (COL) (MEMBER COL (DS-LINK.COLOURS.FOR 'ALL)))) (TESTEVAL (EVAL COLOUR*))))))
       (AND (CONSP COLOUR*)
	    (OR (EVERY (FUNCTION (LAMBDA (COL) (MEMBER COL (DS-LINK.COLOURS.FOR 'ALL)))) COLOUR*)
		(MEMBER (TESTEVAL (EVAL COLOUR*)) (DS-LINK.COLOURS.FOR 'ALL))
		(AND (CONSP (TESTEVAL (EVAL COLOUR*)))
		     (EVERY (FUNCTION (LAMBDA (COL) (MEMBER COL (DS-LINK.COLOURS.FOR 'ALL)))) (TESTEVAL (EVAL COLOUR*))))))))
    (T (ERROR "CG-CHECK - ARGUMENT IN CG-#LINKS NO LEGAL COLOUR(S):: ~A" COLOUR*))))

(DEFUN CG=CHECK_INSERT.CLAUSE (CLAUSE CREATOR.UNIFIER LIST.OF.ANCESTORS)
						; EDITED:  2. 3. 1982   HJO
						; EFFECT:  CHECKS  IF CLAUSE IS A VALID CLAUSE ADDRESS
						;          AND IF CLAUSE IS NEW TO THE ACTUAL GRAPH;
						;          FOR LIST.OF.ANCESTORS <>NIL CHECKS IF IT IS
						;          OF AN ILLEGAL FORM OR PLAUSIBLY DOES NOT
						;          BELONG TO CLAUSE.
  (COND ((DS-CLAUSE.IS CLAUSE)) (T (ERROR "CG-CHECK - ARGUMENT IN CG-INSERT.CLAUSE IS NO CLAUSE_: ~A" CLAUSE)))
  (COND ((NOT (OR (NULL CREATOR.UNIFIER) (DS-LINK.IS (CAR CREATOR.UNIFIER))))
	 (ERROR "CG-CHECK - ILLEGAL CREATOR UNIFIER IN CG-INSERT.CLAUSE_ : ~A" CREATOR.UNIFIER)))
  (COND ((MEMBER CLAUSE (CG=REPR_LIST 'CLAUSES 'ALL))
	 (ERROR "CG-CHECK - ATTEMPT TO REINSERT CLAUSE WITH CG-INSERT.CLAUSE_: ~A" CLAUSE)))
  (COND ((NULL LIST.OF.ANCESTORS)		; OK
	 )
	(T (COND ((AND (CONSP LIST.OF.ANCESTORS) (EVERY (FUNCTION LISTP) LIST.OF.ANCESTORS))	; OK
		  )
		 (T (ERROR "CG-CHECK - ILLEGAL LIST.OF.ANCESTORS FORMAT IN CG-INSERT.CLAUSE_: ~A" LIST.OF.ANCESTORS)))
	   (COND ((EQL (DS-CLAUSE.NOLIT CLAUSE) (LIST-LENGTH LIST.OF.ANCESTORS))	; OK
		  )
		 (T (ERROR "CG-CHECK - ILLEGAL NUMBER OF LIST.OF.ANCESTORS IN CG-INSERT.CLAUSE_: ~A" LIST.OF.ANCESTORS))))))

(DEFUN CG=CHECK_INSERT.LINK (LINK ANCESTORLINKS)
						; EDITED:  3. 3. 1982  HJO
						; EFFECT:  TESTS IF LINK IS REALLY A NEW LINK.
						;          IF ANCESTORLINKS <>NIL, TESTS IF ANCESTOR-
						;          LINKS ARE PLAUSIBLE ANCESTORLINKS.
  (COND ((DS-LINK.IS LINK)) (T (ERROR "CG-CHECK - ARGUMENT IN CG-INSERT.LINK IS NO LINK_: ~A" LINK)))
  (COND ((MEMBER (DS-LINK.COLOUR LINK) (DS-LINK.COLOURS.FOR 'ALL)))
	(T (ERROR "CG-CHECK - ILLEGAL LINK COLOUR IN CG-INSERT.LINK_: ~A" (DS-LINK.COLOUR LINK))))
  (COND ((MEMBER LINK (CG=REPR_LIST (DS-LINK.COLOUR LINK) 'ALL))
	 (ERROR "CG-CHECK - ATTEMPT TO REINSERT LINK WITH CG-INSERT.LINK_: ~A" LINK)))
  (COND
    ((CONSP ANCESTORLINKS)
     (COND
       ((EVERY
	  (FUNCTION
	    (LAMBDA (ANCESTORLINK) (MEMBER ANCESTORLINK (CG=REPR_LIST (DS-LINK.COLOUR ANCESTORLINK) 'ALL))))
	  ANCESTORLINKS)			; OK
	)
       (T (ERROR "CG-CHECK - ILLEGAL ANCESTORLINKS IN CG-INSERT.LINK_: ~A" ANCESTORLINKS))))))

(DEFUN CG=CHECK_INSERT.UNIFIER (LINK UNIFIER)
						; EDITED: 29-FEB-84 19:57:54
						; EFFECT: CHECKS IF LINK IS A PERMISSABLE ATOM.
						; REMARK: USED BY ADVISE IN CG-CHECK ONLY.
  (declare (ignore unifier))
  (COND ((NOT (DS-LINK.IS LINK)) (ERROR "CG-CHECK - ARGUMENT IN CG-INSERT.UNIFIER IS NOT LINK_ : ~A" LINK))))

(DEFUN CG=CHECK_REMOVE.CLAUSE (CLAUSE)
						; EDITED:  3. 2. 1982  HJO
						; EFFECT:  TESTS IF CLAUSE IS A CLAUSE OF THE
						;          ACTUAL GRAPH.
  (COND ((DS-CLAUSE.IS CLAUSE)) (T (ERROR "CG-CHECK - ARGUMENT IN CG-REMOVE.CLAUSE IS NO CLAUSE_: ~A" CLAUSE)))
  (COND ((MEMBER CLAUSE (CG=REPR_LIST 'CLAUSES 'ALL)))
	(T (ERROR "CG-CHECK - ATTEMPT TO REMOVE CLAUSE NOT BELONGING TO THE ACTUAL GRAPH:: ~A" CLAUSE))))

(DEFUN CG=CHECK_REMOVE.LINK (LINK)
						; EDITED:  3. 3. 1982  HJO
						; EFFECT:  CHECKS IF LINK IS A LINK OF THE ACTUAL
						;          GRAPH.
  (COND ((DS-LINK.IS LINK)) (T (ERROR "CG-CHECK - ARGUMENT IN CG-REMOVE.LINK IS NO LINK_: ~A" LINK)))
  (COND
    ((MEMBER LINK (CG=REPR_LIST (DS-LINK.COLOUR LINK) 'REMOVED))
     (ERROR "CG-CHECK - ATTEMPT TO REMOVE LINK ALREADY REMOVED:: ~A" LINK)))
  (COND ((MEMBER LINK (CG=REPR_LIST (DS-LINK.COLOUR LINK) 'ALL)))
	(T (ERROR "CG-CHECK - ATTEMPT TO REMOVE LINK NOT BELONGING TO THE ACTUAL GRAPH:: ~A" LINK))))

(DEFUN CG=CHECK_REMOVE.UNIFIER (LINK UNIFIER)
						; EDITED:  3. 3. 1982  HJO
						; EFFECT:  TESTS IF LINK IS AN R, P OR F LINK OF THE
						;          ACTUAL GRAPH AND IF UNIFIER IS A UNIFIER
						;          BELONGING TO LINK.
  (COND ((DS-LINK.IS LINK))
	(T (ERROR "CG-CHECK - ARGUMENT IN CG-REMOVE.UNIFIER IS NO LINK_: ~A" LINK)))
  (COND ((MEMBER (DS-LINK.COLOUR LINK) (DS-LINK.COLOURS.WITH 'UNIFIERS)))
	(T (ERROR "CG-CHECK - ILLEGAL LINK COLOUR IN CG-REMOVE.UNIFIER:: ~A" (DS-LINK.COLOUR LINK))))
  (COND ((MEMBER UNIFIER (DS-LINK.UNIFIERS LINK) :TEST (FUNCTION EQUAL)))
	(T
	 (ERROR "CG-CHECK - ATTEMPT TO REMOVE UNIFIER NOT BELONGING TO LINK (CG-REMOVE.UNIFIER):: ~A"
		(DS-PNAME UNIFIER)))))

(DEFUN CG=CHECK_REMOVE.LITERAL (CLAUSE LITNO SIBLINGLITNO)
						; EDITED:  3. 3. 1982  HJO
						; EFFECT:  TESTS IF CLAUSE IS A CLAUSE OF THE
						;          ACTUAL GRAPH AND IF LITNO IS AN INTEGER
						;          LESS THAN DS-CLAUSE.NOLIT.
  (COND ((DS-CLAUSE.IS CLAUSE)) (T (ERROR "CG-CHECK - ARGUMENT IN CG-REMOVE.LITERAL IS NO CLAUSE_: ~A" CLAUSE)))
  (COND ((MEMBER CLAUSE (CG=REPR_LIST 'CLAUSES 'ALL)))
	(T
	 (ERROR "CG-CHECK - ATTEMPT TO MODIFY CLAUSE NOT BELONGING TO THE ACTUAL GRAPH (CG-REMOVE.LITERAL):: ~A"
		CLAUSE)))
  (COND ((AND (INTEGERP LITNO) (< 0 LITNO) (NOT (> LITNO (DS-CLAUSE.NOLIT CLAUSE)))))
	(T (ERROR "CG-CHECK - ILLEGAL LITERAL NUMBER IN CG-REMOVE.LITERAL:: ~A" LITNO)))
  (COND
    (SIBLINGLITNO
     (COND ((NEQ SIBLINGLITNO LITNO)
						; OK
	    )
	   (T (ERROR "CG-CHECK - BAD SIBLINGLITNO = LITNO IN CG-REMOVE.LITERAL:: ~A" SIBLINGLITNO)))
     (COND
       ((AND (INTEGERP SIBLINGLITNO) (< 0 SIBLINGLITNO) (NOT (> SIBLINGLITNO (DS-CLAUSE.NOLIT CLAUSE))))
						; OK
	)
       (T (ERROR "CG-CHECK - BAD SIBLINGLITNO IN CG-REMOVE.LITERAL:: ~A" SIBLINGLITNO))))))

(DEFUN CG=CHECK_REPLACE.LITERAL (CLAUSE LITNO)
						; EDITED:  3. 3. 1982  HJO
						; EFFECT:  TESTS IF CLAUSE IS A CLAUSE OF THE
						;          ACTUAL GRAPH AND IF LITNO IS AN INTEGER
						;          LESS THAN DS-CLAUSE.NOLIT.
  (COND ((DS-CLAUSE.IS CLAUSE)) (T (ERROR "CG-CHECK - ARGUMENT IN CG-REPLACE.TERMLIST IS NO CLAUSE_: ~A" CLAUSE)))
  (COND ((MEMBER CLAUSE (CG=REPR_LIST 'CLAUSES 'ALL)))
	(T
	 (ERROR
	   "CG-CHECK - ATTEMPT TO MODIFY CLAUSE NOT BELONGING TO THE ACTUAL GRAPH (CG-REPLACE.TERMLIST):: ~A"
	   CLAUSE)))
  (COND ((AND (INTEGERP LITNO) (< 0 LITNO) (NOT (> LITNO (DS-CLAUSE.NOLIT CLAUSE)))))
	(T (ERROR "CG-CHECK - ILLEGAL LITERAL NUMBER IN CG-REPLACE.TERMLIST:: ~A" LITNO))))

(defmacro cg-trace (switch)
  `(cg=trace ',switch))

(DEFUN CG=TRACE (SWITCH)
						; INPUT:  EITHER OF THE ATOMS ON, OFF,T.
						; VALUE:  MESSAGE ABOUT KIND OF SWITCH PERFORMED.
						; EFFECT: WHEN SWITCHED ON CAUSES ALL FUNCTIONS
						;         REMOVING OR INSERTING OBJECTS OF THE ACTUAL
						;         GRAPH TO PROTOCOL THE REASON OF THIS ACTION.
						;         OUTPUT FILE IS T IF SWITCH IS T, ELSE THE
						;         PROTOCOL FILE.
  (CASE SWITCH
    ((ON T)
     (CG=TRACE.CLAUSES (EQL SWITCH T))
     (CG=TRACE.LINKS (EQL SWITCH T))
     (APPEND '(CG-TRACE SWITCHED ON. OUTPUT WILL GO TO) (COND ((EQL SWITCH T) '(TERMINAL.)) (T '(TRACE FILE.)))))
    (OFF (EVAL (cons 'progn (mapcar #'(lambda (fun) (list 'global:UNADVISE fun))
				  '(CG-REPLACE.LITERAL CG-REMOVE.LITERAL CG-REMOVE.CLAUSE CG-INSERT.CLAUSE CG=REMOVE.LINK
						       CG=INSERT.UNIFIER CG=REMOVE.UNIFIER CG=INSERT.LINK CG-INHIBIT.UNIFIER))))
	 '(CG-TRACE SWITCHED OFF.))
    (OTHERWISE '(INVALID ARGUMENT - TRY (CG-TRACE ON) (CG-TRACE T) OR (CG-TRACE OFF)))))

(DEFUN CG=TRACE.CLAUSES (TERMINALFLAG)
						; EDITED: 13-APR-83 14:17:01        NE
						; INPUT:  A BOOLEAN VALUE.
						; EFFECT: PERFORMS THE ADVISES OF CG-TRACE
						;         CONCERNING CLAUSES
						; VALUE:  UNDEFINED. 
  (PROGN
    (eval `(GLOBAL:ADVISE CG-INSERT.CLAUSE BEFORE CG nil
			  (apply #'CG=TRACE_INSERT.CLAUSE ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG-REMOVE.CLAUSE BEFORE CG nil
			  (apply #'CG=TRACE_REMOVE.CLAUSE ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG-REMOVE.LITERAL BEFORE CG nil
			  (apply #'CG=TRACE_REMOVE.LITERAL ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG-REPLACE.LITERAL BEFORE CG nil
			  (apply #'CG=TRACE_REPLACE.LITERAL ',TERMINALFLAG SCL:ARGLIST)))))

(DEFUN CG=TRACE.LINKS (TERMINALFLAG)
						; EDITED: 13-APR-83 14:17:01        NE
						; INPUT:  A BOOLEAN VALUE.
						; EFFECT: PERFORMS THE GLOBAL:ADVISES OF CG-TRACE
						;         CONCERNING LINKS.
						; VALUE:  UNDEFINED. 
  (PROGN
    (eval `(GLOBAL:ADVISE CG-INHIBIT.UNIFIER BEFORE CG nil
	     (apply #'CG=TRACE_INHIBIT.UNIFIER ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG=INSERT.LINK BEFORE CG nil
	     (apply #'CG=TRACE_INSERT.LINK ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG=REMOVE.UNIFIER BEFORE CG nil
	     (apply #'CG=TRACE_REMOVE.UNIFIER ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG=INSERT.UNIFIER BEFORE CG nil
	     (apply #'CG=TRACE_INSERT.UNIFIER ',TERMINALFLAG SCL:ARGLIST)))
    (eval `(GLOBAL:ADVISE CG=REMOVE.LINK BEFORE  CG nil
	     (apply #'CG=TRACE_REMOVE.LINK ',TERMINALFLAG SCL:ARGLIST)))))

(DEFUN CG=TRACE_INSERT.CLAUSE (TERMINALFLAG CLAUSE CREATOR.UNIFIER ANCESTORS &optional REASON INFO)
  (declare (ignore ANCESTORS CREATOR.UNIFIER))
						; EFFECT: PROTCOLS REASON FOR CLAUSE INSERTION
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.PNAME 24)
	(POS.REASON 60))
    (format file "+++++ Inserting clause~vT~A" pos.pname (ds-pname clause))	; removed object
    (format file "~vTReason: " pos.reason)	; reason
    (PROGN ; REASON 
	   (CASE REASON (INITIAL (PRINC "Initial clause" FILE))
		 (RESOLUTION (PRINC "Resolvent created by link " FILE) (PRINC INFO FILE))
		 (PARAMODULATION (PRINC "Paramodulant created by link " FILE) (PRINC INFO FILE))
		 (FACTORIZATION (PRINC "Factor created by link " FILE) (PRINC INFO FILE))
		 (OTHERWISE (PRINC (COND (REASON) (T "UNKNOWN")) FILE))))
    (TERPRI FILE)))

(DEFUN CG=TRACE_REMOVE.CLAUSE (TERMINALFLAG CLAUSE REASON INFO)
						; EFFECT: PROTCOLS REASON FOR CLAUSE REMOVAL
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.PNAME 24)
	(POS.SUBST 110)
	(POS.REASON 60))
    (format file "----- Removing clause~vT~A" pos.pname (DS-PNAME CLAUSE))	; removed object
    (format file "~vTReason: " pos.reason)	; reason
    (CASE REASON
      (PURITY (PRINC "Purity" FILE))
      (TAUTOLOGY (PRINC "Tautology" FILE))
      (SUBSUMPTION (format file "Subsumption by ~A with:" (DS-PNAME (CAR INFO)))
		   (MAPC #'(LAMBDA (SUBST) (FORMAT FILE "~vT~A" POS.SUBST (DS-PNAME SUBST)))
			 (CDR INFO)))
      (OTHERWISE (PRINC (or REASON "UNKNOWN") FILE)))
    (TERPRI FILE)))

(defun cg=trace_print.rw.rule (info file)
  (princ (DS-CLAUSE.PNAME INFO) file)
  (PRINC ": " FILE)
  (PRINC (DS-PNAME (CAR (DS-CLAUSE.TERMLIST INFO 1))) FILE) (PRINC " --> " FILE)
  (PRINC (DS-PNAME (SECOND (DS-CLAUSE.TERMLIST INFO 1))) FILE)
  (princ "  " file))


(DEFUN CG=TRACE_REPLACE.LITERAL (TERMINALFLAG CLAUSE LITNO NEWSIGN NEWPREDICATE NEWTERMLIST REASON INFO)
						; EFFECT: PROTCOLS REASON FOR TERMLIST REPLACEMENT
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.PNAME 24)
	(POS.TERMLIST 45)
	(POS.SUBST 110)
	(POS.REASON 60))
    (format file "+-+-+ Changing literal ~vT~A of clause ~A;~vT~ANew literal: ~A ~A ~A"
	    pos.pname litno (DS-PNAME CLAUSE) pos.termlist
	    (DS-PNAME NEWSIGN) (DS-PNAME NEWPREDICATE) (DS-PNAME NEWTERMLIST))	; changed object
    (format file "~vTReason: " pos.reason)	; reason
    (CASE REASON
      (SYMMETRY (PRINC "Applying symmetry of " FILE)
		(PRINC (DT-PREDICATE.PNAME (DS-CLAUSE.PREDICATE CLAUSE LITNO)) FILE))
      (REWRITE (PRINC "Rewrite with " FILE)
	       (if (listp info)
		   (mapcar #'(lambda (clause) (cg=trace_print.rw.rule clause file)) info)
		   (cg=trace_print.rw.rule info file)))
      (GENERALIZING (PRINC "Generalizing replacement resolution" FILE) (FORMAT FILE "~T" POS.REASON)
		    (PRINC "on link " FILE) (PRINC (DS-PNAME (CAR INFO)) FILE) (PRINC " with: " FILE)
		    (FORMAT FILE "~T" POS.SUBST)
		    (PRINC (DS-PNAME (CDR INFO)) FILE))
      (OTHERWISE (PRINC (COND (REASON) (T "UNKNOWN")) FILE)))
    (TERPRI FILE)))

(DEFUN CG=TRACE_REMOVE.LITERAL (TERMINALFLAG CLAUSE LITNO SIBLING.LITNO REASON INFO)
						; EFFECT: PROTCOLS REASON FOR LITERAL REMOVAL
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.REASON 60))
    (format file "----- Removing literal ~A from clause ~A" LITNO (DS-PNAME CLAUSE))	; REMOVED OBJECT
    (format file "~vTReason: " pos.reason)	; reason
    (CASE REASON
      (REPL.RESOLUTION (format file "Replacement resolution on link ~A with: ~A" (DS-PNAME (CAR INFO)) (DS-PNAME (CDR INFO))))
      (double.literal (format file "Double literal ~A" sibling.litno))
      (false.litno (format file "False literal, predicate ~A reflexive or irreflexive." (ds-clause.predicate clause litno)))
      (OTHERWISE (PRINC (COND (REASON) (T "UNKNOWN")) FILE)))
    (TERPRI FILE)))

(DEFUN CG=TRACE_INSERT.LINK (TERMINALFLAG LINK ANCESTORS REASON INFO)
						; EFFECT: PROTCOLS REASON FOR LINK INSERTION
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (declare (ignore ancestors))
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.PNAME 24)
	(POS.REASON 60))
    (format file "+++++ Inserting ~A-Link~vT~A" (DS-LINK.COLOUR LINK) pos.pname link)	; removed object
    (format file "~vTReason: " pos.reason)	; reason
    (CASE REASON (INITIAL (PRINC "Initial link" FILE)) (PARENTCONNECTOR (PRINC "Connection to parent clause" FILE))
	  (PARAMODULATION (PRINC "Connection of paramodulation literal" FILE))
	  (INHERITED (PRINC "INHERITED FROM " FILE)
		     (MAPC
		       (FUNCTION
			 (LAMBDA (LINK)
			   (PRINC
			     (CONCATENATE 'STRING (PRINC-TO-STRING (DS-LINK.COLOUR LINK))
					  "-LINK " (PRINC-TO-STRING LINK) " ")
			     FILE)))
		       INFO))
	  (GENERALIZING.INHERITANCE (PRINC "GENERALIZING INHERITANCE FROM " FILE)
				    (MAPC
				      (FUNCTION
					(LAMBDA (LINK)
					  (PRINC
					    (CONCATENATE 'STRING (PRINC-TO-STRING (DS-LINK.COLOUR LINK))
							 "-LINK " (PRINC-TO-STRING LINK))
					    FILE)))
				      INFO))
	  (INHIBITION (PRINC "INHIBITED DUE TO " FILE) (PRINC INFO FILE))
	  (OTHERWISE (PRINC (COND (REASON) (T "UNKNOWN")) FILE)))
    (TERPRI FILE)))

(DEFUN CG=TRACE_REMOVE.LINK (TERMINALFLAG LINK REASON INFO)
						; EFFECT: PROTCOLS REASON FOR LINK REMOVAL
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.PNAME 24)
	(POS.REASON 60))
    (format file "----- Removing ~A-Link~vT~A" (DS-LINK.COLOUR LINK) pos.pname link)	; removed object
    (format file "~vTReason: " pos.reason)	; reason
    (CASE REASON
      (PARENTREMOVAL    (PRINC "Removal of parent clause " FILE) (PRINC (DS-PNAME INFO) FILE))
      (UNIFIERREMOVAL   (PRINC "Removal of last unifier" FILE))
      (PARENTLITREMOVAL (PRINC "Removal of parent literal " FILE) (PRINC (DS-PNAME (CAR INFO)) FILE) (PRINC " , " FILE)
			(PRINC (CDR INFO) FILE))
      (:subsumed.p      (format file "Subsumed by ~A" (DS-PNAME INFO)))
      (OTHERWISE (PRINC (COND (REASON) (T "Unknown")) FILE)))
    (TERPRI FILE)))

(DEFUN CG=TRACE_INSERT.UNIFIER (TERMINALFLAG LINK UNIFIER REASON INFO)
						; EDITED: 29-FEB-84 19:54:54
						; EFFECT: PROTOCOLS REASON FOR  UNIFIER INSERTION
						;         ON TERMINAL OR TRACEFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (if TERMINALFLAG *terminal-io* (OPT-GET.OPTION TR_TRACE.FILE)))
	(POS.PNAME 24) (POS.REASON 60) (POS.SUBST 110))
    (declare (ignore pos.subst))
    (format file "+++++ Inserting unifier~vT~A into ~A-Link ~A" pos.pname (DS-PNAME UNIFIER) (DS-LINK.COLOUR LINK) link)
    (FORMAT FILE "~vTReason: " POS.REASON)
    (CASE REASON
      (INHIBITION (PRINC " Inhibited due to " FILE) (PRINC INFO FILE))
      (OTHERWISE  (PRINC (or REASON "Unknown") FILE)))
    (TERPRI FILE)))

(DEFUN CG=TRACE_REMOVE.UNIFIER (TERMINALFLAG LINK UNIFIER RECOLOURFLAG REASON INFO)
						; EFFECT: PROTCOLS REASON FOR UNIFIER REMOVAL
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (declare (ignore recolourflag))
  (let ((FILE (if TERMINALFLAG *terminal-io* (or (OPT-GET.OPTION TR_TRACE.FILE) t)))
	(POS.PNAME 24)
	(POS.REASON 60)
	(POS.SUBST 110))
    (format file "----- Removing unifier~vT~A from ~A-Link ~A"
	    pos.pname (DS-PNAME UNIFIER) (DS-LINK.COLOUR LINK) link)	; removed object
    (format file "~vTReason: " pos.reason)	; reason
    (CASE REASON
      (PURITY (PRINC "PURITY" FILE))
      (SUBSUMPTION (format file "Subsumption by ~A with: " (DS-PNAME (CAR INFO)))
		   (MAPC #'(LAMBDA (SUBST)
			     (FORMAT FILE "~T" POS.SUBST)
			     (PRINC (DS-PNAME SUBST) FILE))
			 (CDR INFO)))
      (INHIBITION (PRINC "Inhibited due to " FILE) (PRINC INFO FILE))
      (ACTIVE (PRINC "Operation on it" FILE))
      (OTHERWISE (PRINC (COND (REASON) (T "UNKNOWN")) FILE)))
    (TERPRI FILE)))

(DEFUN CG=TRACE_INHIBIT.UNIFIER (TERMINALFLAG LINK UNIFIER REASON INFO)
						; EFFECT: PROTCOLS REASON FOR UNIFIER INHIBITION
						;         ON TERMINAL OR PROTOCOLFILE DEPENDING ON
						;         TERMINALFLAG.
						; REMARK: USED BY ADVISE IN CG-TRACE ONLY.
  (let ((FILE (COND (TERMINALFLAG T) (T (OPT-GET.OPTION TR_TRACE.FILE))))
	(POS.PNAME 24)
	(POS.REASON 60)
	(POS.SUBST 110))
    (format file "----- Inhibit unifier~vT~A from ~A-Link ~A"	; Inhibited object.
	    pos.pname (DS-PNAME UNIFIER) (DS-LINK.COLOUR LINK) link)
    (format file "~vTReason: " pos.reason)	; REASON. 
    (CASE REASON
      (SUBSUMPTION (format file "Subsumption by ~A with: " (DS-PNAME (CAR INFO)))
		   (MAPC (FUNCTION (LAMBDA (SUBST) (FORMAT FILE "~T" POS.SUBST) (PRINC (DS-PNAME SUBST) FILE))) (CDR INFO)))
      (OTHERWISE (PRINC (COND (REASON) (T "Unknown")) FILE)))
    (TERPRI FILE)))

