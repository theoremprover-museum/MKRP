;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.6
;;; Reason: Function MKRP::PPR=SPLIT.REPLACEMENT.OPERATION:  Unifier can be nil
;;; protocol interface: selection post/ standard
;;; internal call for standard
;;; Written by prckln, 4/08/92 16:22:56
;;; while running on JS-SFBUX2 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.5, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.19DEC91) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.6:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 505.

;;; Patch file for MKRP version 53.6
;;; Written by mkrp, 4/08/92 18:18:43
;;; while running on JS-SFBUX2 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.5, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.19DEC91) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.6:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 505.




(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MKRP:PROG;PROT;PROTOCOL.LISP.NEWEST"
  "MKRP:PROG;PROT;INTERFACE.LISP.NEWEST"
  "MKRP:PROG;PROT;PROT-PREPARE.LISP.NEWEST")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;PROT;PROT-PREPARE.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-")

(DEFUN PPR=SPLIT.REPLACEMENT.OPERATION (OP.CODE)
						; EDITED: 13-SEP-84 13:47:03            BY CL
						; INPUT : A LIST, CODE AS PUT OUT BY PR-OPERATION:
						;         (<UNIFIER> <CLAUSES.INVOLVED> <RESOLUTIONS>
						;          <DOUBLE.LIT.LISTS> <VARS.SORTS OF CODOMAIN>)
						; EFFECT: CREATES A (SERIES OF) OPERATION(S) CORRES-
						;         PONDING TO THE REPLACEMENT.OPERATION
						; VALUE : LIST OF OPERATION ADDRESSES
  (let ((UNIFIER                (first  OP.CODE))
	(CLAUSES.INVOLVED       (SECOND OP.CODE))
	(RESOLUTIONS            (THIRD  OP.CODE))
	(DOUBLE.LIT.LISTS       (FOURTH OP.CODE))
	(VARS.SORTS.OF.CODOMAIN (fifth  OP.CODE))
	CLAUSE.CHANGED
	AFFECTED.VARS
	(OPERATIONS (LIST NIL)))
    (declare (ignore affected.vars))
    (SETQ CLAUSE.CHANGED               (CAR CLAUSES.INVOLVED)
	  ppr*clause.changed           (PPR=GET.NEW.CLAUSE.ADDRESS clause.changed)
	  PPR*LENGTH.OF.CLAUSE.CHANGED (LIST-LENGTH (PDS-CLAUSE.LITERALS (PPR=GET.NEW.CLAUSE.ADDRESS CLAUSE.CHANGED))))
    (multiple-value-setq (unifier vars.sorts.of.codomain) (ppr=change.unifier unifier vars.sorts.of.codomain ppr*clause.changed))
    (WHEN (MEMBER CLAUSE.CHANGED (CDR CLAUSES.INVOLVED))
      (setq ppr*clause.symbols nil)
      (SETQ DOUBLE.LIT.LISTS
	    (CONS (FIRST DOUBLE.LIT.LISTS)
		  (MAPCAR #'(LAMBDA (RES DBL.LITS)
			      (IF (AND (OR (= CLAUSE.CHANGED (CAR (FIRST RES)))
					   (= CLAUSE.CHANGED (CAR (SECOND RES))))
				       (OR (= (SECOND CLAUSES.INVOLVED) (CAR (FIRST RES)))
					   (= (SECOND CLAUSES.INVOLVED) (CAR (SECOND RES)))))
				  (let ((new.symbol (gentemp (format nil "PPR-~A-" clause.changed))))
				    (setq ppr*clause.symbols (nconc1 ppr*clause.symbols new.symbol))
				    (PUTASSOC new.symbol
					      (cassoc clause.changed PPR*OLD.NEW.CLAUSE.ADDRESSES)
					      PPR*OLD.NEW.CLAUSE.ADDRESSES)
				    (dotimes (litno PPR*LENGTH.OF.CLAUSE.CHANGED)
				      (unless (= (1+ litno) (if (= CLAUSE.CHANGED (CAR (FIRST RES)))
								(cdr (first res))
								(cdr (second res))))
					(setq dbl.lits (NCONC1 DBL.LITS (list (cons clause.changed (1+ litno))
									      (cons new.symbol (1+ litno))
									      nil)))))
				    dbl.lits)
				  (progn (setq ppr*clause.symbols (nconc1 ppr*clause.symbols nil))
					 DBL.LITS)))
			  (CDR RESOLUTIONS)
			  (CDR DOUBLE.LIT.LISTS)))))
    (PPR=BUILD.OLDLITS.NEWLITS resolutions DOUBLE.LIT.LISTS (remove nil ppr*clause.symbols))
    (COND
      ((null resolutions) ;(AND UNIFIER (NULL RESOLUTIONS)) Sometimes there are pure double lit removeals here ! Axel
						; I.E. PURE REPLACEMENT FACTORIZATION
       (NCONC OPERATIONS
	      (PPR=OPERATION
		(LIST (PPR=SIMULATE.INSTANTIATION CLAUSE.CHANGED UNIFIER VARS.SORTS.OF.CODOMAIN)
		      (LIST 'INSTANTIATION UNIFIER CLAUSE.CHANGED)))
	      (PPR=DOUBLE.LITERAL.OPERATIONS (CAR DOUBLE.LIT.LISTS) CLAUSE.CHANGED)))
      (T (let ((ppr*clause.symbols nil))
	   (NCONC OPERATIONS
		  (PPR=RESOLUTION.OPERATION      (first RESOLUTIONs)      CLAUSE.CHANGED UNIFIER VARS.SORTS.OF.CODOMAIN)
		  (PPR=DOUBLE.LITERAL.OPERATIONS (first DOUBLE.LIT.lists) CLAUSE.CHANGED)
		  (pop ppr*clause.symbols)))
	 (MAPC #'(LAMBDA (RESOLUTION DOUBLE.LITERALS)
		   (NCONC OPERATIONS
			  (PPR=RESOLUTION.OPERATION      RESOLUTION      CLAUSE.CHANGED UNIFIER VARS.SORTS.OF.CODOMAIN)
			  (PPR=DOUBLE.LITERAL.OPERATIONS DOUBLE.LITERALS CLAUSE.CHANGED))
		   (pop ppr*clause.symbols))
	       (rest RESOLUTIONS) (rest DOUBLE.LIT.LISTS))))
    (setq ppr*clause.changed nil)
    (CDR OPERATIONS)))




;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;PROT;INTERFACE.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")

(DEFUN PR-CONSTRUCT.START (FILE SYSTEM.VERSION &optional COMMENT)
						      ; Edited:  08-APR-1992 19:11
						      ; Authors: MKRP CL
						      ; input : code file, version of system, a string comment
						      ; effect: sets linelength of code file to 120 and
						      ;         prints <CONSTRUCTION system.version date (C comment)>
						      ;         top-level elements of comment will be prin-
						      ;         in separate lines.
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=CONSTRUCT.START file SYSTEM.VERSION COMMENT))
    (post (po-CONSTRUCT.START file SYSTEM.VERSION COMMENT))))


(DEFUN PR-INFIX.FORM (AXIOMS.INFIX THEOREMS.INFIX)
						      ; Edited:  08-APR-1992 19:22
						      ; Authors: MKRP CL
						      ; input : two lists
						      ; effect: prints <AXIOMS.INFIX   (...) ... (...)>
						      ;                <THEOREMS.INFIX (...) ... (...)>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=INFIX.FORM AXIOMS.INFIX THEOREMS.INFIX))
    (post (po-INFIX.FORM AXIOMS.INFIX THEOREMS.INFIX))))

					
(DEFUN PR-PREFIX.FORM (AXIOMS.PREFIX THEOREMS.PREFIX)
						      ; Edited:  08-APR-1992 19:22
						      ; Authors: MKRP CL
						      ; input : two lists
						      ; effect: prints <'axioms.prefix   (...) ... (...)>
						      ;                <'theorems.prefix (...) ... (...)>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=PREFIX.FORM AXIOMS.PREFIX THEOREMS.PREFIX))
    (post (po-PREFIX.FORM AXIOMS.PREFIX THEOREMS.PREFIX))))

					
(DEFUN PR-OPTIONS NIL	
						      ; Edited:  08-APR-1992 19:23			
						      ; Authors: MKRP CL
						      ; input : none
						      ; effect: prints <OPTIONS ... all options and their
						      ;                      values as dotted pairs ... >
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=OPTIONS.TOP))
    (post (po-OPTIONS))))


(DEFUN PR-AXIOMS.START NIL			
						      ; Edited:  08-APR-1992 19:26
						      ; Authors: MKRP CL
						      ; input : none
						      ; effect: prints <LINK.COLOURS R ... PER>
						      ;                <AXIOMS (START.TIME time in sec/977)
  (case (opt-get.option pr_protocol)
    (standard (PR=AXIOMS.START))
    (post (po-AXIOMS.START))))

					
(DEFUN PR-AXIOMS.END (RESULT)	
						      ; Edited:  08-APR-1992 19:27
						      ; Authors: MKRP CL
						      ; input : a list (SUCCESS AXIOMS.UNSATISFIABLE)
						      ;             or (SUCCESS   ..empty clause..)  or nil
						      ; effect: prints (END.TIME  time in sec/997 )
						      ;                (FINAL     actual clauses  )
						      ;                (RESULT    result reason   )>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=AXIOMS.end result))
    (post (po-AXIOMS.end result))))


(DEFUN PR-THEOREMS.START (SPLITPART.IDENTIFIER &optional SPLITFLAG)
  (declare (ignore splitflag))		
						      ; Edited:  08-APR-1992 19:39
						      ; Authors: MKRP CL
						      ; input : a list of integers and a boolean value
						      ; effect: prints <THEOREMS (SPLITPART.IDENTIFIER ..)
						      ;                          (START.TIME    ..time.. )
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=THEOREMS.START SPLITPART.IDENTIFIER))
    (post (po-THEOREMS.START SPLITPART.IDENTIFIER))))

					
(DEFUN PR-THEOREMS.END (RESULT)	
						      ; Edited:  08-APR-1992 19:30
						      ; Authors: MKRP CL
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
  (case (opt-get.option pr_protocol)
    (standard (PR=THEOREMS.END result))
    (post (po-THEOREMS.END result))))


(DEFun PR-CONSTRUCT.END ()
  (case (opt-get.option pr_protocol)
    (standard (PR=CONSTRUCT.END))
    (post (po-CONSTRUCT.END))))


(DEFUN PR-SPLITPARTS.START (FILE SYSTEM.VERSION &optional COMMENT)
						      ; Edited:  08-APR-1992 19:31
						      ; Authors: MKRP CL
						      ; input : code file and version of system
						      ; effect: sets linelength of code file to 120 and
						      ;         prints <SPLITPARTS system.version date>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=SPLITPARTS.START FILE SYSTEM.VERSION COMMENT))
    (post (po-SPLITPARTS.START FILE SYSTEM.VERSION COMMENT))))

					
(DEFUN PR-REFUTATION.START (SPLITPART.INDICATOR RESUMPTION.FLAG STEP.NUMBER)
  (declare (ignore splitpart.indicator resumption.flag step.number))
						      ; Edited:  08-APR-1992 19:32
						      ; Authors: MKRP CL
						      ; input : a list of lists of integers, a flag indica-
						      ;         ting if splitpart is continued, and an in-
						      ;         teger (or nil, if new splitpart begins)
						      ; effect: prints    <'refutation
						      ;                       ('start.time ..in msec..)
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=REFUTATION.START))
    (post (po-REFUTATION.START))))

					
(DEFUN PR-PARTIAL.GRAPH (PARTIAL.CLAUSE.LIST)	
						      ; Edited:  08-APR-1992 19:34; edited: 22-apr-83 08:10:18  by cl
						      ; Authors: MKRP CL
						      ; input : list of clauses
						      ; effect: prints  <PARTIAL ... list of clauses as
						      ;                          described in pr=clauses ...>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=PARTIAL.GRAPH PARTIAL.CLAUSE.LIST))
    (post (po-PARTIAL.GRAPH PARTIAL.CLAUSE.LIST))))


(DEFUN PR-INITIAL.GRAPH ()	
						      ; Edited:  08-APR-1992 19:35
						      ; Authors: MKRP CL		
						      ; input : none
						      ; effect: prints  <INITIAL ... list of clauses as
						      ;                          described in pr=clauses ...>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=INITIAL.GRAPH))
    (post (po-INITIAL.GRAPH))))


(DEFUN PR-OPERATION (OPERATION.TYPE &REST ARGUMENTS)
						      ; edited:  3-jul-84 10:45:06  by cl
						      ; input : an atom, and arguments according to the type of operation
						      ; effect: prints <OPERATION <CLAUSE  ..see PR=CLAUSE..>
						      ;                           <op.type ..see PR=op.type..>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (pr=operation OPERATION.TYPE ARGUMENTS))
    (post (po-operation operation.type arguments))))

(DEFUN PR-STATISTICS (TIME)
  (declare (ignore time))			; edited:  3-jul-84 17:19:28  by cl
						; input : an integer (time in sec/997)
						; effect: prints the following (example) :
						;   (STATISTICS (CLAUSES (ALL . 44) (INSERTED . 2) (REMOVED . 1) (CHANGED . 0))
						;               (LINKS (ALL . (223 12 ... 0 2))   (INSERTED . (12 0 ... 0 1))
						;               (REMOVED . (20 2 ... 1 1)) (CHANGED . (1 0 ... 0 0))))
						;         numbers separated for different link colours.
						; value : undefined
  nil)


(DEFUN PR-REFUTATION.END (SPLITPART.IDENTIFIER RESULT)
						      ; Edited:  08-APR-1992 19:36
						      ; Authors: MKRP CL
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
  (case (opt-get.option pr_protocol)
    (standard (pr=REFUTATION.END SPLITPART.IDENTIFIER RESULT))
    (post (po-REFUTATION.END SPLITPART.IDENTIFIER RESULT))))


(DEFUN PR-SPLITPARTS.END () 
  (case (opt-get.option pr_protocol)
    (standard (pr=SPLITPARTS.END))
    (post (po-SPLITPARTS.END))))



;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;PROT;PROTOCOL.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")

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



