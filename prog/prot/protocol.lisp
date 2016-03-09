;;; -*- mode: lisp; syntax: common-lisp; package: mkrp -*-

#| Copyright (C) 1991 AG Siekmann, 
                      Fachbereich Informatik, Universitaet des Saarlandes, 
                      Saarbruecken, Germany

This file is part of Markgraf Karl Refutation Procedure (MKRP).

MKRP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  

Everyone is granted permission to copy, modify and redistribute
MKRP, but only if the it is not used for military purposes or any
military research. It is also forbidden to use MKRP in nuclear plants
or nuclear research, and for verifying programs in military 
and nuclear research.  A copy of this license is
supposed to have been given to you along with MKRP so you
can know your rights and responsibilities.  
Among other things, the copyright notice
must be preserved on all copies.  |#

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(defvar pr*indices nil "To record old literals")
(DEFVAR PR*FILE NIL)
(defvar PR*INDENTATION 0
  "an integer, describing the number of blanks needed at the beginning of a line for formatted output")



(DEFUN PR=CONSTRUCT.START (FILE SYSTEM.VERSION COMMENT)
  (when (SETQ PR*FILE FILE)    
    (setq pr*indices nil)
    (setq comment (mapcar #'(lambda (c.line) (if (stringp c.line) c.line (princ-to-string c.line))) comment))
    (format PR*FILE "~%(CONSTRUCTION  ~S ~S ~%~14T(~{~S~%~15T~}))"
	    system.version (date) comment)))


(DEFUN PR=INFIX.FORM (AXIOMS.INFIX THEOREMS.INFIX)
						; edited:  5-sep-84 16:13:46  by cl
						; input : two lists
						; effect: prints <AXIOMS.INFIX   (...) ... (...)>
						;                <THEOREMS.INFIX (...) ... (...)>
						; value : undefined
  (when PR*FILE
    (format  PR*FILE "~&~%(AXIOMS.INFIX    (~S~{~%~18T~S~}))~%~%(THEOREMS.INFIX (~S~{~%~18T~S~}))"
	     (first AXIOMS.INFIX) (rest axioms.infix) (first THEOREMS.INFIX) (rest theorems.infix))))

					
(DEFUN PR=PREFIX.FORM (AXIOMS.PREFIX THEOREMS.PREFIX)
						; edited: 21-apr-83 17:27:16  by cl
						; input : two lists
						; effect: prints <'axioms.prefix   (...) ... (...)>
						;                <'theorems.prefix (...) ... (...)>
						; value : undefined
  (WHEN PR*FILE
    (format  PR*FILE "~&~%(AXIOMS.PREFIX   (~S~{~%~18T~S~}))~%~%(THEOREMS.PREFIX (~S~{~%~18T~S~}))"
	     (first AXIOMS.PREFIX) (rest axioms.prefix) (first THEOREMS.PREFIX) (rest theorems.prefix))))

					
(DEFUN PR=OPTIONS.top NIL				; edited: 22-apr-83 08:05:46  by cl
						; input : none
						; effect: prints <OPTIONS ... all options and their
						;                      values as dotted pairs ... >
						; value : undefined
  (WHEN PR*FILE (PR=OPTIONS PR*FILE)))


(DEFUN PR=AXIOMS.START NIL			; edited: 22-apr-83 11:15:44  by cl
						; input : none
						; effect: prints <LINK.COLOURS R ... PER>
						;                <AXIOMS (START.TIME time in sec/977)
  (when PR*FILE
    (PR=LINK.COLOURS PR*FILE)
    (setq PR*INDENTATION 8)
    (format PR*file "~&~%(AXIOMS (START.TIME ~A)" (GET-INTERNAL-RUN-TIME))))

					
(DEFUN PR=AXIOMS.END (RESULT)			; edited: 23-mar-84 10:05:44  by cl
						; input : a list (SUCCESS AXIOMS.UNSATISFIABLE)
						;             or (SUCCESS   ..empty clause..)  or nil
						; effect: prints (END.TIME  time in sec/997 )
						;                (FINAL     actual clauses  )
						;                (RESULT    result reason   )>
						; value : undefined
  (when PR*FILE
    (PR=AXIOMS.OR.THEOREMS.END RESULT PR*FILE)
    (PRINC ")" PR*FILE)				; closes the bracket opened in pr-axioms.start
    (setq PR*INDENTATION 0)))


(DEFUN PR=THEOREMS.START (SPLITPART.IDENTIFIER)
						      ; edited: 23-mar-84 10:23:02             by cl
						      ; input : a list of integers and a boolean value
						      ; effect: prints <THEOREMS (SPLITPART.IDENTIFIER ..)
						      ;                          (START.TIME    ..time.. )
						      ; value : undefined
  (WHEN PR*FILE
    (setq PR*INDENTATION 10)
    (format pr*file "~&~%(THEOREMS (SPLITPART.IDENTIFIER~{ ~A~})~%~vT(START.TIME ~D)"
	    SPLITPART.IDENTIFIER PR*INDENTATION (GET-INTERNAL-RUN-TIME))))

					
(DEFUN PR=THEOREMS.END (RESULT)		
						      ; edited: 23-mar-84 10:32:37  by cl
						      ; input : one of the following lists:
						      ;          (SUCCESS THEOREMS.VALID)
						      ;          (SUCCESS ..empty.clause..)
						      ;          (FAILURE GRAPH.COLLAPSED)
						      ;          (FAILURE GRAPH.SATISFIABLE ..model..)
						      ;          (SPLIT   ..file.. ..splitpart.indicators..)
						      ; effect: prints (END.TIME time in sec/997)
						      ;                (FINAL    actual clauses )
						      ;                (RESULT   result reason  )>
						      ; value : undefined
  (when PR*FILE
    (PR=AXIOMS.OR.THEOREMS.END RESULT PR*FILE)
    (PRINC ")" PR*FILE)				      ; closes the bracket opened in pr-theorems.start    
    (setq PR*INDENTATION 0)))


(DEFun PR=CONSTRUCT.END ()
  (testeval (format pr*file "~%(indices ~A)" pr*indices)))


(DEFUN PR=SPLITPARTS.START (FILE SYSTEM.VERSION COMMENT)
						      ; edited:  5-sep-84 16:16:43  by cl
						      ; input : code file and version of system
						      ; effect: sets linelength of code file to 120 and
						      ;         prints <SPLITPARTS system.version date>
						      ; value : undefined
  (SETQ PR*FILE FILE)
  (when pr*file
    (linelength 117 pr*file)			      ; length suitable for lineprinter
    (format PR*file "~&~%(SPLITPARTS ~S ~S ~S)" SYSTEM.VERSION (DATE) comment)))

					
(DEFUN PR=REFUTATION.START ()
						      ; edited: 28-aug-84 10:50:39  by cl
						      ; input : a list of lists of integers, a flag indica-
						      ;         ting if splitpart is continued, and an in-
						      ;         teger (or nil, if new splitpart begins)
						      ; effect: prints    <'refutation
						      ;                       ('start.time ..in msec..)
						      ; value : undefined
  (when PR*FILE
    (format PR*FILE "~&~%(REFUTATION (START.TIME ~D)" (GET-INTERNAL-RUN-TIME))
    (setq PR*INDENTATION 12)
    (PR=OPTIONS PR*FILE)))

					
(DEFUN PR=PARTIAL.GRAPH (PARTIAL.CLAUSE.LIST)
						      ; edited: 22-apr-83 08:10:18  by cl
						      ; input : list of clauses
						      ; effect: prints  <PARTIAL ... list of clauses as
						      ;                          described in pr=clauses ...>
						      ; value : undefined
  (when (AND PR*FILE PARTIAL.CLAUSE.LIST)
    (format PR*FILE "~&~vT(PARTIAL   " PR*INDENTATION)
    (setq PR*INDENTATION (+ PR*indentation 9))
    (MAPC (FUNCTION (LAMBDA (CLAUSE) (PR=CLAUSE CLAUSE PR*FILE t))) PARTIAL.CLAUSE.LIST)
    (PRINC ")" PR*FILE)
    (setq PR*INDENTATION (- PR*indentation 9))))


(DEFUN PR=INITIAL.GRAPH ()			; edited: 10-jul-84 16:00:47  by cl
						      ; input : none
						      ; effect: prints  <INITIAL ... list of clauses as
						      ;                          described in pr=clauses ...>
						      ; value : undefined
  (when PR*FILE
    (format pr*file "~&~vT(INITIAL   " PR*INDENTATION)
    (setq PR*INDENTATION (+ PR*indentation 9))
    (MAPC #'(LAMBDA (CLAUSE)
	      (PR=CLAUSE CLAUSE PR*FILE t)
	      (format pr*file "~%~vT" pr*indentation))
	  (DS-RULES))
    (MAPL #'(LAMBDA (rest.CLAUSEs)
	      (PR=CLAUSE (first rest.CLAUSEs) PR*FILE)
	      (format pr*file "~:[~%~;~vT~]" (endp (cdr rest.clauses)) pr*indentation))
	  (CG-CLAUSES ALL))
    (PRINC ")" PR*FILE)
    (setq PR*INDENTATION (- PR*indentation 9))))

(defun pr=operation (operation.type arguments)
  (when PR*FILE
    (CASE OPERATION.TYPE
      (R.CHAIN   (PR=R.CHAIN ARGUMENTS PR*FILE))
      (OTHERWISE (format PR*FILE "~&~vT(OPERATION " pr*indentation)
		 (setq PR*INDENTATION (+ PR*indentation 11))
		 (CASE OPERATION.TYPE		      ; in any case PR=CLAUSE is called first for the resulting clause
		   (RESOLUTION             (PR=RESOLUTION             ARGUMENTS PR*FILE))
		   (PARAMODULATION         (PR=PARAMODULATION         ARGUMENTS PR*FILE))
		   (FACTORIZATION          (PR=FACTORIZATION          ARGUMENTS PR*FILE))
		   (instantiate            (PR=instantiate            ARGUMENTS PR*FILE))
		   (REPLACEMENT.OPERATION  (PR=REPLACEMENT.OPERATION  ARGUMENTS PR*FILE))
		   (DOUBLE.LITERAL         (PR=DOUBLE.LITERAL         ARGUMENTS PR*FILE))
		   (REWRITE                (PR=REWRITE                ARGUMENTS PR*FILE))
		   (REWRITE.SYMMETRY       (PR=REWRITE.SYMMETRY       ARGUMENTS PR*FILE))
		   (REPLACEMENT.RESOLUTION (PR=REPLACEMENT.RESOLUTION ARGUMENTS PR*FILE))
		   (OTHERWISE              (ERROR "Non-existing operation type: ~S" OPERATION.TYPE)))
		 (PRINC ")" PR*FILE)
		 (setq PR*INDENTATION (- PR*indentation 11))))))


(DEFUN PR=REFUTATION.END (SPLITPART.IDENTIFIER RESULT)
						      ; edited: 28-aug-84 11:43:46         by cl
						      ; input : a list of lists of integers and a list:
						      ;            (SUCCESS ..empty.clause..)
						      ;         OR (FAILURE GRAPH.SATISFIABLE ..model..),
						      ;            (FAILURE LINKS.INOPERABLE),
						      ;            (FAILURE ABORTED.MAXSTEPS),
						      ;            (FAILURE ABORTED.MANUALLY),
						      ;            (SPLIT ..file.. ..splitpart.indicators..)
						      ; effect: prints  (END.TIME ..in sec/977..)
						      ;                 (SYMBOLS  ....)
						      ;                 (SPLITPART.IDENTIFIER  ....)
						      ;                 (RESULT result reason)   >
						      ; value : undefined
  (when PR*FILE
    (format pr*file "~&~vT(END.TIME ~D)" PR*INDENTATION (GET-INTERNAL-RUN-TIME))
    (PR=SYMBOLS PR*FILE)
    (format pr*file "~&~vT(SPLITPART.IDENTIFIER~{ ~A~})" PR*INDENTATION SPLITPART.IDENTIFIER)
    (format pr*file "~&~vT(RESULT~{ ~S~})~%" pr*indentation RESULT)
    (PRINC ")" PR*FILE)				      ; closes bracket opened in pr-refutation.start
    (setq PR*INDENTATION 0)))


(DEFUN PR=SPLITPARTS.END () nil)


(DEFUN PR=AXIOMS.OR.THEOREMS.END (RESULT FILE)	; edited: 28-aug-84 11:49:47  by cl
						; input : a list, see pr-axioms.end or pr-theorems.end
						;         and a file open for output
						; effect: prints (END.TIME ..time in sec/977..)
						;                (FINAL    ..actual.clauses.. )
						;                (SYMBOLS  .....)
						;                (RESULT   result reason)
  (FORMAT PR*FILE "~&~vT(END.TIME ~D)~%~vT(FINAL~{ ~A~})"
	  PR*INDENTATION (GET-INTERNAL-RUN-TIME) PR*INDENTATION (CG-CLAUSES ALL))
  (PR=SYMBOLS FILE)
  (format pr*file "~&~vT(RESULT~{ ~A~})" PR*INDENTATION RESULT))


(DEFUN PR=OPTIONS (FILE)
  (format file "~&~%(OPTIONS~{ ~A~%~9T~})" (OPT-GET.LIST.OPTIONS)))


(DEFUN PR=SYMBOLS (FILE)			; edited: 28-aug-84 10:16:45  by cl
						; input : a file name (open for output)
						; effect: prints the info about all constants,
						;         functions, and predicates to the code file.
						;         <SYMBOLS (LET (NEW.ADDRESS)
						;                    (LIST .....))>
						; value : undefined
  (format file "~&~vT(SYMBOLS " PR*INDENTATION)
  (DT-SAVE.SYMBOLS FILE (+ PR*INDENTATION 9))
  (PRINC ")" FILE))


(DEFUN PR=RESOLUTION (LINK.UNI.CLAUSE FILE)	; edited: 22-apr-83 08:10:18  by cl
						; input : a list (link unifier clause) and a file
						; effect: prints information as described in PR=CLAUSE and PR=LINK.RES
						; value : undefined
  (PR=CLAUSE (THIRD LINK.UNI.CLAUSE) FILE)
  (progn (format FILE "~&~vT(RESOLUTION " PR*indentation)
	 (PR=LINK.RES (first LINK.UNI.CLAUSE) FILE)
	 (format file " ~A ~A)" (SECOND LINK.UNI.CLAUSE) (THIRD LINK.UNI.CLAUSE))))


(DEFUN PR=R.CHAIN (ARGUMENTS FILE)		; EDITED:  5-SEP-84 16:23:08
						; INPUT:  A LIST (RESOLVENT TERMLISTS CHAIN-ELEMENT)
						;         RESOLVENT IS THE NEW CLAUSE,
						;         CHAIN-ELEMENT IS A LIST
						;         (UNIFIER CLAUSE LINKS POINTERS LITNOS)
						;         WHERE CLAUSE IS THE PARENTCLAUSE OF
						;         RESOLVENT (WHICH IS CREATED BY SIMULTANEUS
						;         RESOLUTION UPON LINKS WITH 'UNIFIER')
						;         POINTERS AND LITNOS IS IRRELEVANT.
						;         TERMLISTS IS THE LIST OF TERMLISTS OF
						;         THE INTERMEDIATE CLAUSE WHICH WOULD BE
						;         CREATED BY RESOLUTION UPON THE FIRST ELEMENT
						;         OF LINKS.
						; EFFECT: THIS OPERATION IS PROTOCOLLED AS A
						;         RESOLUTION UPON THE FIRST ELEMENT OF LINKS
						;         FOLLOWED BY A REPLACEMENT RESOLUTION STEP.
						; VALUE:  UNDEFINED.
  (PROG ((RESOLVENT (CAR ARGUMENTS))
	 (TERMLISTS (SECOND ARGUMENTS))
	 (UNIFIER (CAR (THIRD ARGUMENTS)))
	 (CLAUSE (SECOND (THIRD ARGUMENTS)))
	 (LINKS (THIRD (THIRD ARGUMENTS)))	;; (LITNOS (CAR (CDDDR (THIRD ARGUMENTS))))
	 (LITNO 0)
	 FIRST COLOUR RLINKS DOUBLE.LITS)
	(SETQ COLOUR (DS-LINK.COLOUR (CAR LINKS)))
	(SETQ FIRST  (CASE COLOUR
		       (R  (DS-LINK.THISLITNO (CAR LINKS) CLAUSE))
		       (SI (DS-LINK.POSLITNO  (CAR LINKS)))
		       (OTHERWISE (ERROR "ILLEGAL COLOUR IN PR=R.CHAIN: ~A" COLOUR))))
	(format file "(OPERATION (CLAUSE ~A ~A ~A "
		RESOLVENT (DS-PNAME RESOLVENT)
		(CASE COLOUR (R (LIST CLAUSE (DS-LINK.OTHERPAR (CAR LINKS) CLAUSE)))
		      (OTHERWISE (LIST CLAUSE))))
						; variables.sort
	(PROGN (PRINC "(" FILE)
	       (MAPC (FUNCTION (LAMBDA (VAR)
				       (PRINC "(" FILE) (PRINC VAR FILE) (PRINC " . " FILE)
				       (PRINC (DT-VARIABLE.SORT VAR) FILE) (PRINC ")" FILE)))
		     (DT-TERMLIST.VARIABLES TERMLISTS))
	       (PRINC ")" FILE))
	
	(progn (PRINC "(" FILE)
	       (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		 (PROGN (SETQ LITNO (1+ LITNO))
			(COND ((NEQ LITNO FIRST)
			       (PRINC "(" FILE)
			       (PRINC (DS-CLAUSE.SIGN CLAUSE LITNO) FILE) (PRINC " " FILE)
			       (PRINC (DS-CLAUSE.PREDICATE CLAUSE LITNO) FILE) (PRINC " " FILE)
			       (PRINC (CAR TERMLISTS) FILE)
			       (SETQ TERMLISTS (CDR TERMLISTS)) (PRINC ")" FILE)))))
	       (PRINC "))(" FILE))
	
	(CASE COLOUR (SI (PRINC "FACTORIZATION " FILE) (PR=LINK.FAC (CAR LINKS) FILE))
	      (OTHERWISE (PROGN (PRINC "RESOLUTION " FILE) (PR=LINK.RES (CAR LINKS) FILE))))
	(PRINC " " FILE) (PRINC UNIFIER FILE) (PRINC " " FILE) (PRINC RESOLVENT FILE) (PRINC "))" FILE)
	(MAPC
	  (FUNCTION
	    (LAMBDA (LINK)
	      (COND ((EQL 'R (DS-LINK.COLOUR LINK)) (SETQ RLINKS (NCONC1 RLINKS LINK)))
		    (T
		     (SETQ DOUBLE.LITS
			   (NCONC1 DOUBLE.LITS
				   (LIST
				     (CONS RESOLVENT
					   (COND ((< (DS-LINK.NEGLITNO LINK) FIRST) (DS-LINK.NEGLITNO LINK))
						 (T (1- (DS-LINK.NEGLITNO LINK)))))
				     (CONS RESOLVENT
					   (COND ((< (DS-LINK.POSLITNO LINK) FIRST) (DS-LINK.POSLITNO LINK))
						 (T (1- (DS-LINK.POSLITNO LINK)))))
				     (DS-LINK.RULE LINK))))))))
	  (CDR LINKS))
	(COND
	  ((OR RLINKS DOUBLE.LITS)
	   (PRINC "(OPERATION " FILE) (PR=CLAUSE RESOLVENT FILE)
	   (PRINC "(REPLACEMENT.OPERATION NIL (" FILE)
	   (PRINC RESOLVENT FILE) (PRINC " " FILE)
	   (MAPC (FUNCTION (LAMBDA (LINK) (PRINC (DS-LINK.OTHERPAR LINK CLAUSE) FILE)
				   (PRINC " " FILE))) RLINKS) (PRINC ")" FILE)
	   (COND
	     (RLINKS (PRINC "(" FILE)
		     (MAPC
		       (FUNCTION
			 (LAMBDA (LINK) (PRINC "((" FILE) (PRINC RESOLVENT FILE) (PRINC " . " FILE)
				 (SETQ LITNO (DS-LINK.THISLITNO LINK CLAUSE))
				 (PRINC (COND ((< LITNO FIRST) LITNO) (T (1- LITNO))) FILE) (PRINC ")(" FILE)
				 (PRINC (DS-LINK.OTHERPAR LINK CLAUSE) FILE) (PRINC " . " FILE)
				 (PRINC (DS-LINK.OTHERLITNO LINK CLAUSE litno) FILE) (PRINC ")" FILE)
				 (PRINC (DS-LINK.RULE LINK) FILE)
				 (PRINC ")" FILE)))
		       RLINKS)
		     (PRINC ")" FILE))
	     (T (PRINC " NIL " FILE)))
	   (PRINC "(" FILE) (PRINC (OR DOUBLE.LITS " NIL ") FILE)
	   (MAPC (FUNCTION (LAMBDA (LINK) (PRINC " NIL " FILE))) (CDR RLINKS)) (PRINC ")" FILE)
	   (PRINC (PR=VARIABLES.SORTS.OF.CODOMAIN NIL) FILE) (PRINC "))" FILE)))))

(DEFUN PR=LINK.RES (LINK FILE)			; edited:  3-jul-84 10:58:56               by cl
						; input : link address and a file opened for output
						; effect: prints :  pospar poslitno negpar neglitno rule
						; value : undefined
  (format file "~A ~D ~A ~D ~A"
	  (DS-LINK.POSPAR LINK) (DS-LINK.POSLITNO LINK)
	  (if (member (ds-link.colour link) (ds-link.colours.for 'autolinks))
	      (DS-LINK.POSPAR LINK)
	      (DS-LINK.NEGPAR LINK))
	  (DS-LINK.NEGLITNO LINK) (DS-LINK.RULE LINK)))


(DEFUN PR=REPLACEMENT.RESOLUTION (ARGUMENTS FILE)
					; edited: 29-oct-83 14:03:08
					; input:  a list  (replaced.parent litno other.parent
					;         litno unifier) and the code file
					; effect: the information is printed to the code file in
					;         the same way as PR=RESOLUTION would.
					; value:  undefined.
  (PR=CLAUSE (first ARGUMENTS) FILE)
  (format file "~&~vT(RESOLUTION ~{~A ~}~A)" PR*indentation arguments (first arguments)))


(DEFUN PR=PARAMODULATION (LINK.UNI.CLAUSE FILE)	; edited: 22-apr-83 08:10:18  by cl
						; input : a list (link unifier clause)  and a file
						; effect: prints information as described in pr=clause
						;                   and pr=link.par respectively
						; value : undefined
  (PR=CLAUSE (THIRD LINK.UNI.CLAUSE) FILE)
  (format file "~&~vT(PARAMODULATION " pr*indentation)
  (PR=LINK.PAR (first LINK.UNI.CLAUSE) FILE)
  (format file " ~A ~A)" (SECOND LINK.UNI.CLAUSE) (THIRD LINK.UNI.CLAUSE)))


(DEFUN PR=LINK.PAR (LINK FILE)			; edited:  3-jul-84 11:00:09  by cl
						; input : link# and a file opened for output
						; effect: prints :  eqpar eqlitno eqfct par litno fct
						; value : undefined
  (format file "~A ~D ~S ~A ~D ~S ~A"
	  (DS-LINK.POSPAR LINK) (DS-LINK.POSLITNO LINK) (DS-LINK.POSFCT LINK)
	  (DS-LINK.NEGPAR LINK) (DS-LINK.NEGLITNO LINK) (DS-LINK.NEGFCT LINK) (DS-LINK.RULE LINK)))


(DEFUN PR=FACTORIZATION (LINK.UNI.CLAUSE FILE)	; edited: 22-apr-83 08:10:18  by cl
						; input : a list (link unifier clause)  and a file
						; effect: prints information as described in pr=clause
						;                   and pr=link.fac respectively
						; value : undefined
  (PR=CLAUSE (THIRD LINK.UNI.CLAUSE) FILE)
  (format FILE "~&~vT(FACTORIZATION " pr*indentation)
  (PR=LINK.FAC (CAR LINK.UNI.CLAUSE) FILE)
  (format file " ~A ~A)" (SECOND LINK.UNI.CLAUSE) (THIRD LINK.UNI.CLAUSE)))

(DEFUN PR=instantiate (UNI.CLAUSE.old FILE)	; edited: 22-apr-83 08:10:18  by cl
						; input : a list (link unifier clause)  and a file
						; effect: prints information as described in pr=clause
						;                   and pr=link.fac respectively
						; value : undefined
  (PR=CLAUSE (third UNI.CLAUSE.old) FILE)
  (format FILE "~&~vT(instantiate " pr*indentation)
  (format file " ~A ~A)" (first UNI.CLAUSE.old) (second UNI.CLAUSE.old)))

(DEFUN PR=LINK.FAC (LINK FILE)			; edited:  3-jul-84 11:01:00  by cl
						; input : link# and a file opened for output
						; effect: prints par
						; value : undefined
  (format file "~A ~A"
	  (DS-LINK.POSPAR LINK) (DS-LINK.RULE LINK)))


(DEFUN PR=REPLACEMENT.OPERATION (CLAUSE.DESCRIPTION FILE)
					; edited: 24-mar-84 20:00:16  by cl
					; input : a list of two elements, an integer (the clause)
					;         and a list (unifier clauses resolutions multiples),
					;         and a file open for output.
					; effect: prints the info to the code file
					; value : undefined
  (PR=CLAUSE (CAR CLAUSE.DESCRIPTION) FILE)
  (let ((UNIFIER (CAADR CLAUSE.DESCRIPTION)))
    (format file "~&~vT(REPLACEMENT.OPERATION ~A ~{~A~%  ~}~A)"
	    pr*indentation unifier (rest (SECOND CLAUSE.DESCRIPTION))
	    (PR=VARIABLES.SORTS.OF.CODOMAIN UNIFIER))))

(DEFUN PR=VARIABLES.SORTS.OF.CODOMAIN (UNIFIER)	; edited: 15-may-84 09:18:06  by cl
						; input :  a list with an even number of elements
						; effect:  finds all the variables appearing in the
						;          codomain of the unifier
						; value :  a list of dotted pairs (var . sort)
  (let (CODOMAIN VARIABLES)
    (SETQ CODOMAIN  (SMAPCAR #'identity #'CDDR (CDR UNIFIER))
	  VARIABLES (DT-TERMLIST.VARIABLES CODOMAIN))
    (MAPCAR #'(LAMBDA (VAR) (CONS VAR (DT-VARIABLE.SORT VAR))) VARIABLES)))


(DEFUN PR=DOUBLE.LITERAL (CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE FILE)
  (PR=CLAUSE (CAR CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE) FILE)
  (format file "~&~vT(DOUBLE.LITERAL ~{~A ~}~A)"
	  pr*indentation (REST CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE)
	  (FIRST CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE)))


(DEFUN PR=REWRITE (RULE.CLAUSE.LITNO FILE)	; edited:  6-jul-83 15:47:07  by cl
						; input :  a list and a file open for output
						; effect:  prints clause as described in pr=clause,
						;          prints (REWRITE rule litno clause)
						; value :  undefined
  (PR=CLAUSE (SECOND RULE.CLAUSE.LITNO) FILE)
  (format file "~&~vT(REWRITE ~A ~A ~A)" pr*indentation
	  (CAR RULE.CLAUSE.LITNO) (THIRD RULE.CLAUSE.LITNO) (SECOND RULE.CLAUSE.LITNO)))


(DEFUN PR=REWRITE.SYMMETRY (CLAUSE. FILE)	; edited:  9-jul-84 10:24:06  by cl
						; input :  a list and a file open for output
						; effect:  prints clause as described in PR=CLAUSE,
						;          prints (REWRITE.SYMMETRY clause)
						; value :  undefined
  (PR=CLAUSE (CAR CLAUSE.) FILE)
  (format file "~&~vT(REWRITE.SYMMETRY ~A)" pr*indentation (CAR CLAUSE.)))


(DEFUN PR=CLAUSE (CLAUSE FILE &optional record.flag)			; edited: 31-mar-83 15:40:23  ne
						; input:  a clause, and a file name.
						; effect: prints the given information in the format:
						;         (CLAUSE address pname parents variables.sorts literals)
						; remark: it is assumed that the position is correct before.
						; value:  undefined.
  (format file "(CLAUSE ~A ~A ~A "
	  CLAUSE (DS-CLAUSE.PNAME CLAUSE) (DS-CLAUSE.PARENTS CLAUSE))
  (PROGN (PRINC "(" FILE)			; variables.sorts
	 (MAPC #'(LAMBDA (VAR)
		   (format file "(~A . ~A)" var (DT-VARIABLE.SORT VAR)))
	       (DS-CLAUSE.VARIABLES CLAUSE))
	 (PRINC ")" FILE))
  (let ((indices nil))
    (format FILE "~:[ ~;~%~vT~](" (> (DS-CLAUSE.NOLIT CLAUSE) 1) (+ pr*indentation 8))	; litlist, new line if > 1
    (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
      (push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
      (format file "(~A ~A ~A) "
	      (DS-CLAUSE.SIGN CLAUSE     (1+ LITNO))
	      (DS-CLAUSE.PREDICATE CLAUSE (1+ LITNO))
	      (DS-CLAUSE.TERMLIST CLAUSE (1+ LITNO))))    
    (PRINC ")" FILE)
    (when record.flag (push (cons clause (nreverse indices)) pr*indices)))
  (PRINC ")" FILE))



(DEFUN PR=LINK.COLOURS (FILE)
  (format file "~&~%(LINK.COLOURS ~A)" (DS-LINK.COLOURS.FOR 'ALL)))

