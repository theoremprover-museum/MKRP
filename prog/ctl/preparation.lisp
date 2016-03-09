;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-
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


(Defun PREP-INITIAL.GRAPH (PROBLEM.FILE GRAPH.FILE CODE.FILE COMMENT virtual.initial.graph.flag)
					; input   names of three files and a comment for the
					;         proof protocol.
					; effect: for the axioms and theorems resp. for all
					;         splitparts specified on problem.file the
					;         initial graphs are constructed and saved
					;         on the graph.file. the raw data for the
					;         protocol is written on code.file
					;         VIRTUAL.INITIAL.GRAPH.FLAG is true iff a code file
					;         must be constructed.
					; value:  a legal total.result.
					; remark: code.file and graph.file are respected to
					;         be open and remain so
  (let (RESULT)
    (PREP=INIT PROBLEM.FILE GRAPH.FILE)
    (PREP=CREATE.EMPTY.GRAPH)
    (unwind-protect (progn (PREP=READ.PROBLEM.FILE)
			   (PREP=PR_OPEN CODE.FILE (NCONC PREP*COMMENT COMMENT))
			   (PREP=PR_INFIX.FORM PREP*AXIOMS.INFIX PREP*THEOREMS.INFIX)
			   (PREP=PR_PREFIX.FORM PREP*AXIOMS.PREFIX PREP*THEOREMS.PREFIX)
			   (PREP=PR_OPTIONS)
			   (SETF PREP*AXIOMS.PREFIX   (PREP=REMOVE.COMMENTS PREP*AXIOMS.PREFIX)
				 PREP*THEOREMS.PREFIX (PREP=REMOVE.COMMENTS PREP*THEOREMS.PREFIX)
				 RESULT               (PSIM-APPLY.DEFINITIONS PREP*AXIOMS.PREFIX PREP*THEOREMS.PREFIX)
				 PREP*AXIOMS.PREFIX   (CAR RESULT)
				 PREP*THEOREMS.PREFIX (SECOND RESULT)
				 RESULT               (PREP=CREATE.AXIOMGRAPH PREP*AXIOMS.PREFIX virtual.initial.graph.flag))
			   (unless RESULT
				   (SETQ RESULT (PREP=CREATE.INITIAL.GRAPHS PREP*THEOREMS.PREFIX virtual.initial.graph.flag))))
      (PREP=PR_CLOSE RESULT)
      (PREP=END))
    RESULT))

(DEFUN PREP-PROBLEM.SPECIFICATION (AXIOMS.INFIX AXIOMS.PREFIX THEOREMS.INFIX THEOREMS.PREFIX COMMENT EXPRESSION)
						; remark: this function can be used to define a
						;         problem specification. the calls can but
						;         don't have to come from a file
						; input:  expression is a dummy argument. any actual
						;         argument is expected to create the dataterm-
						;         objects by side effect during its evaluation
						;         the value to which the actual argument evaluates is never used
						;         axioms.infix and theorems.infix   are formulas in infix-form
						;         axioms.prefix and theorems.prefix are formulas in prefix-form
						;         comment is a proof-comment
						; effect: the given values are assigned to the module
						;         variables
						; value:  undefined
  (declare (ignore expression))
  (SETF PREP*AXIOMS.INFIX      AXIOMS.INFIX)
  (SETF PREP*AXIOMS.PREFIX     AXIOMS.PREFIX)
  (SETF PREP*THEOREMS.INFIX    THEOREMS.INFIX)
  (SETF PREP*THEOREMS.PREFIX   THEOREMS.PREFIX)
  (SETF PREP*COMMENT           COMMENT))

(DEFUN PREP-INIT (PROBLEM.FILE GRAPH.FILE)
						; input:  two names of files which are expected to
						;         be open
						; effect: initializes all necessary variables
						; value:  undefined
						; remark: the files remain open
  (PREP=INIT PROBLEM.FILE GRAPH.FILE))

(DEFUN PREP-END NIL			       	; input:  none
						; effect: ends all necessary submodules
						; value:  undefined
  (PREP=END))

(DEFUN PREP-OPEN.PROTOCOL (CODE.FILE COMMENT)	; input:  a name of a file or nil and a comment
						; effect: gives the name of the codefile to the
						;         protocol,prints a header and the options
						;        on this file
						; remark: the file is expected to be open and remains
						;         so
						; value:  undefined.
  (PREP=PR_OPEN CODE.FILE COMMENT)
  (PREP=PR_OPTIONS)
  (PREP=PR_AXIOMS.START))

(DEFUN PREP-CLOSE.PROTOCOL (PROVEDFLAG)		; input:  a boolean value.
						; effect: prints a final message on the protocol file
						;         and closes that file.
						; value:  the protocol code file name
  (PREP=PR_AXIOMS.END NIL)
  (PREP=PR_CLOSE PROVEDFLAG))

(DEFUN PREP-VIRTUAL.PARTIAL.GRAPH (FILE)	; input:   either nil or the name of a file which is
						;          open for output.
						; effect:  computes an s-expression representing the
						;          actual graph such that (eval s-expression)
						;          creates the very same partial graph as it
						;          is during invocation of this function.
						;          if file <> nil, the s-expression is written
						;          on file, otherwise the s-expression is the
						;          value of this function.
						; value:   if file = nil then the computed
						;          s-expression, else nil.
						; remark:  partial graphs are total graphs,i.e. new
						;          clauses can be added to this graph
  (PREP=VIRTUAL.PARTIAL.GRAPH FILE))

(DEFUN PREP-VIRTUAL.INITIAL.GRAPH (RESULT)
						; edited:  6-mar-84 19:04:23
						; input:  a refutation result
						; effect: writes the s-expression on the initial graph
						;         file,which has to be open for output:
						;         (ctl-initial.graph splitpart.identifier
						;                            splitpart.flag
						;                            initial.result
						;                            reduced.flag
						;                            selection.flag
						;                            expression>
						;        where splitpart.identifier is a list of
						;        integers indicating the splitpart,
						;        splitpart.flag a boolean value indicating if
						;        there exists several splitparts,
						;        initial.result a legal refutation result
						;        reduced.flag a boolean value indicating if
						;        the initial.reduction has been performed
						;        selection.flag a boolean value indicating if
						;        the graph has already undergone the initial
						;        selection
						;        expression is a dummy argument to create
						;        the very same initial graph as it is during
						;        invocation of this function by side effect.
						; remark:initial graphs are already initially reduced
						;        i.e. they can only be refuted.
						; value: undefined
  (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT))

(DEFUN PREP-CREATE.EMPTY.GRAPH NIL
						; input:  none
						; effect: all components of the actual graph are
						;         destroyed and the graph is reset to the
						;         empty graph.
						; value:  undefined.
  (PREP=CREATE.EMPTY.GRAPH))

(DEFUN PREP-EXTEND.GRAPH (PREFIXFORMULA CLAUSETYPE)
						; INPUT:  FOPC EXPRESSION IN PREFIX FORM AND AN
						;         ARBITRARY STRING.
						; EFFECT: PREFIXFORMULA IS CONVERTED TO CLAUSAL FORM
						;         AND THE RESULTING CLAUSES BECOME PART OF
						;         THE ACTUAL GRAPH.
						;         ALL LINKS CONNECTED TO THE NEW CLAUSES ARE
						;         CREATED AND BECOME PART OF THE ACTUAL GRAPH.
						;         THE PRINT NAMES OF THE CLAUSES ARE CLAUSE-
						;         NAMESTRING CONCATENATED WITH INTEGERS.
						; VALUE:  A LEGAL REFUTATION.RESULT OR NIL
  (unless (EQ 'COMMENT PREFIXFORMULA)
    (let (RESULT)
      (COND ((STRING= CLAUSETYPE "THM") (SETQ CLAUSETYPE 'THEOREM)) (T (SETQ CLAUSETYPE 'AXIOM)))
      (SETQ RESULT (PREP=CREATE.CLAUSES PREFIXFORMULA CLAUSETYPE))
      RESULT)))

(DEFUN PREP-REDUCE.INITIAL NIL
						; EDITED:  6-MAR-84 21:08:41
						; EFFECT: THE REWRITING-PART IS COMPLETED AND THE
						;         INITIAL REDUCTIONS PERFORMED.THE INITIAL
						;         VARIABLES ARE DELETED.THE ER-PATHS ARE
						;         CONSTRUCTED.THE PREDICATE-OCCURRENCE-LISTS
						;         ARE DELETED.
						; VALUE:  A LEGAL REFUTATION.RESULT OR NIL
  (PREP=REDUCE.INITIAL))

(DEFUN PREP-SUBSUMPTION.TEST (CLAUSELIST)
						; EDITED: 15-JUN-83 18:38:36
						; INPUT:  A LIST OF CLAUSES WITH LITERALS
						;         IN LISP FORMAT: (SIGN PREDICATE TERMLIST)
						; EFFECT: THE ACTUAL GRAPH IS PUSHED, THE CLAUSES
						;         ARE CREATED AS DATASTRUCTRE OBJECTS,
						;         ALL CLAUSES WHICH
						;         ARE SUBSUMED BY OTHER CLAUSES ARE REMOVED.
						;         THE ACTUAL GRAPH IS POPPED AGAIN.
						; VALUE:  A LIST WITH THE REMAINING
						;         CLAUSES IN LISP FORMAT.
  (CG-PUSH)
  (let ((CLAUSES (OP-CONSTRUCT.CLAUSES CLAUSELIST 'AXIOM)) LITNO LITERALS)
    (MAPC #'(LAMBDA (CLAUSE LITLIST)
	      (SETQ LITNO 1)
	      (MAPC #'(LAMBDA (LITERAL)
			(DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO 'CTL*OLD.LIT LITERAL) 
			(incf LITNO))
		    LITLIST))
	  CLAUSES CLAUSELIST)
    (CONS-CONSTRUCT.LINKS CLAUSES 'S)
    (MAPC #'(LAMBDA (CLAUSE) (COND ((DS-CLAUSE.IS CLAUSE) (RED-CLAUSE.SUBSUMPTION) (CG-FIX))))
	  (SORT CLAUSES	#'(LAMBDA (CLAUSE1 CLAUSE2) (< (DS-CLAUSE.NOLIT CLAUSE1) (DS-CLAUSE.NOLIT CLAUSE2)))))
    (SETQ CLAUSELIST
	  (MAPCAR #'(LAMBDA (CLAUSE)
		      (SETQ LITERALS NIL)
		      (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
			(SETQ LITERALS (CONS (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'CTL*OLD.LIT) LITERALS)))
		      (CG-REMOVE.CLAUSE CLAUSE)
		      LITERALS)
		  (REMOVE-IF-NOT #'DS-CLAUSE.IS CLAUSES)))
    (CG-FIX)
    (CG-POP)
    CLAUSELIST))

(DEFUN PREP=INIT (PROBLEM.FILE GRAPH.FILE)
						; INPUT : TWO FILENAMES
						; REMARK: FILES ARE EXPECTED TO BE OPEN
						; EFFECT: INITIALIZES ALL NECESSARY VARIABLES
						; VALUE:  UNDEFINED
  (SETQ PREP*PROBLEM.FILE PROBLEM.FILE)
  (SETQ PREP*GRAPH.FILE GRAPH.FILE)
  (SETQ PREP*AXIOM.GRAPH.FILE (mkrp-make.pathname t (mkrp-default.tempprepaxioms) (mkrp-default.lisp) nil))
  (SETQ PREP*AXIOM.GRAPH.FILE.## NIL)
  (SETQ PREP*SEVERAL.SPLITPARTS NIL)
  (SETQ PREP*STEPNUMBER 0)
  (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES 0)
  (SETQ PREP*AXIOMS.INFIX NIL)
  (SETQ PREP*AXIOMS.PREFIX NIL)
  (SETQ PREP*THEOREMS.INFIX NIL)
  (SETQ PREP*THEOREMS.PREFIX NIL)
  (SETQ PREP*COMMENT NIL))

(DEFUN PREP=END NIL
						; INPUT:  NONE
						; EFFECT: ENDS ALL NECESSARY SUBMODULES
						; VALUE:  UNDEFINED
  (RED-END))

(DEFUN PREP=READ.PROBLEM.FILE NIL
						; INPUT:  NO ARGUMENTS
						; EFFECT: OPENS THE FILE PREP*PROBLEM.FILE,READS THE
						;         FIRST EXPRESSION ON FILE,EVALUATES IT AND
						;         CLOSES THE FILE
						; VALUE:  UNDEFINED
  (let (INPUTSTREAM EXPRESSION (*READTABLE* *STANDARD-READTABLE*))
    (SETQ INPUTSTREAM (mkrp-OPENIN (mkrp-make.pathname t #+symbolics "PROBLEM" #-symbolics "problem" (mkrp-default.lisp) PREP*PROBLEM.FILE)))
    (SETQ EXPRESSION (READ INPUTSTREAM))
    (eval EXPRESSION)
    (CLOSEFILE INPUTSTREAM)))

(DEFUN PREP=CREATE.AXIOMGRAPH (FORMULALIST virtual.initial.graph.flag)
						; EDITED:  6-MAR-84 19:46:34
						; INPUT:  A LIST OF FOPC-EXPRESSIONS PREFIX-FORM
						; EFFECT: THE FORMULAS ARE PRESIMPLIFIED AND NORMA-
						;         LIZED. THE ATTRIBUTE CLAUSES AND AXIOM
						;         CLAUSES ARE CONSTRUCTUED AND ADDED TO THE
						;         AXIOMGRAPH
						; VALUE:  A LEGAL TOTAL.RESULT
  (PROG (CLAUSELIST PRESIMPLIFIED.FORMULA NORMALIZED.FORMULA RESULT TIME) (PREP=PR_AXIOMS.START)
	(SETQ TIME (GET-INTERNAL-RUN-TIME))
	(SETQ CLAUSELIST
	      (SMAPCAR #'(LAMBDA (FORMULA)
			   (SETQ PRESIMPLIFIED.FORMULA (PSIM-PRESIMPLIFICATION FORMULA))
			   (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'PRESIMPLIFIED PRESIMPLIFIED.FORMULA)
			   (COND ((MEMBER PRESIMPLIFIED.FORMULA '(TRUE FALSE)) PRESIMPLIFIED.FORMULA)
				 (T (SETQ NORMALIZED.FORMULA (CAR (NORM-NORMALIZATION PRESIMPLIFIED.FORMULA NIL)))
				    (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'NORMALIZED NORMALIZED.FORMULA) NORMALIZED.FORMULA)))
		       #'(LAMBDA (TAIL) (COND ((NEQ PRESIMPLIFIED.FORMULA 'FALSE) (CDR TAIL))))
		       FORMULALIST))
	(COND
	  ((MEMBER 'FALSE CLAUSELIST) (SETQ RESULT (LIST 'SUCCESS 'AXIOMS.UNSATISFIABLE))
	   (PREP=PR_STATISTICS 'AXIOM (mkrp-TIME TIME))))
	(COND
	  ((NOT RESULT)
	   (SETQ CLAUSELIST (DELETE 'TRUE CLAUSELIST))
	   (SETQ CLAUSELIST (APPLY (FUNCTION NCONC) CLAUSELIST))
	   (SETQ CLAUSELIST
		 (SORT CLAUSELIST #'(LAMBDA (CLAUSE1 CLAUSE2) (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
						;    CONSTRUCTION OF THE ATTRIBUTE CLAUSES
						;    THERE IS NO CG-FIX ALLOWED UNTIL THE FIRST
						;    CALL OF PREP=ADD.CLAUSE
	   (some #'(lambda (clause)
		     (when (and (opt-get.option str_finite.domain) (ds-lit.finite.domain clause))
		       (ds-finite.domain.set clause (ds-lit.finite.domain clause))))
		 clauselist)
	   (cons-CONSTRUCT.ATTRIBUTE.CLAUSES)
						;   ADDING THE AXIOM-CLAUSES TO THE GRAPH
	   (SMAPC #'(LAMBDA (CLAUSE)
		      (SETQ PREP*STEPNUMBER (1+ PREP*STEPNUMBER))
		      (SETQ RESULT (PREP=ADD.CLAUSE CLAUSE 'AXIOM))
		      (PREP=PR_GRAPH.DUMP PREP*STEPNUMBER))
		  #'(LAMBDA (TAIL) (COND ((NEQ (CAR RESULT) 'SUCCESS) (CDR TAIL))))
		  CLAUSELIST)
	   (COND ((EQL PREP*STEPNUMBER 0) (PREP=PR_STATISTICS 'AXIOM (mkrp-TIME TIME))))))
	(PREP=PR_AXIOMS.END RESULT)
	(COND
	  ((EQL (CAR RESULT) 'SUCCESS)
	   (PREP=PR_PRINT.MESSAGE "Axioms unsatisfiable. Any theorem follows trivially.")
	   (when virtual.initial.graph.flag (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT))
	   (SETQ RESULT (CAR RESULT))))
	(RETURN RESULT)))

(DEFUN PREP=CREATE.INITIAL.GRAPHS (THEOREMS.PREFIX virtual.initial.graph.flag)
						; EDITED:  6-MAR-84 20:36:11
						; INPUT:  A LIST OF FOPC EXPRESSIONS IN PREFIXFORM
						; EFFECT: THE EXPRESSIONS ARE PREPROCESSED.IF THERE
						;         EXISTS SEVERAL SPLITPARTS THE AXIOM GRAPH IS
						;         SAVED ON FILE IF NON-EMPTY.IF THE LIST IS
						;         EMPTY OR THE NEGATED CONJUNCTION IS
						;         RECOGNIZED AS VALID THEN THE INITIAL
						;         REDUCTIONS ARE PERFORMED AND THE GRAPH IS
						;         SAVED ON THE INITIAL.GRAPH FILE.
						; VALUE:  A LEGAL TOTAL.RESULT
  (PROG (RESULT)
	(SETQ RESULT (PREP=THEOREMS.CLAUSE.LISTS THEOREMS.PREFIX))
	(COND
	  ((EQL RESULT 'FALSE)
	   (SETQ RESULT '(SUCCESS THEOREMS.VALID))
	   (PREP=PR_THEOREMS.START '(1) NIL)
	   (PREP=PR_STATISTICS 'THEOREM 0)
	   (PREP=PR_THEOREMS.END '(1) NIL RESULT)
	   (PREP=PR_PRINT.MESSAGE "Negated theorem unsatisfiable, i.e. theorem valid."
				  "Theorem follows from any axioms.")
	   (when virtual.initial.graph.flag (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT))
	   (SETQ RESULT 'SUCCESS))
	  ((EQL RESULT 'TRUE)
	   (SETQ PREP*SEVERAL.SPLITPARTS NIL)
	   (PREP=PR_THEOREMS.START '(1) NIL)
	   (PREP=PR_STATISTICS 'THEOREM 0) (SETQ RESULT (PREP=REDUCE.INITIAL))
	   (COND ((NULL RESULT) (PREP=PR_INITIAL.GRAPH)))
	   (PREP=PR_THEOREMS.END '(1) NIL RESULT)
	   (when virtual.initial.graph.flag
	     (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT))
	   (SETQ RESULT (COND (RESULT (CAR RESULT))))
	   (COND ((EQL RESULT 'SUCCESS) (PREP=PR_PRINT.MESSAGE "REFUTATION DETECTED"))
		 ((EQL RESULT 'FAILURE) (PREP=PR_PRINT.MESSAGE "GRAPH COLLAPSED"))))
	  (T (SETQ PREP*SEVERAL.SPLITPARTS (NORM-SEVERAL.SPLITPARTS?))
	     (SETQ RESULT
		   (COND
		     ((OR (NOT PREP*SEVERAL.SPLITPARTS) (NOT (CG-CLAUSES ALL)))
		      (setq os*opened.p t)
		      (setq prep*graph.file os*actual.graph.file)
		      (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES PREP*STEPNUMBER)
		      (PREP=CREATE.SPLITGRAPHS virtual.initial.graph.flag))
		     ((or PREP*SEVERAL.SPLITPARTS (not virtual.initial.graph.flag))
		      #|(unless os*opened.p (OS=OPEN.GRAPH.FILE prep*GRAPH.FILE))|#
			(setq os*opened.p t)
			(setq prep*graph.file os*actual.graph.file)
			(SETQ PREP*AXIOM.GRAPH.FILE (mkrp-OPENOUT PREP*AXIOM.GRAPH.FILE nil))
			(SETQ PREP*AXIOM.GRAPH.FILE.## (pathname PREP*AXIOM.GRAPH.FILE))
			(PREP=VIRTUAL.PARTIAL.GRAPH PREP*AXIOM.GRAPH.FILE)
			(CLOSEFILE PREP*AXIOM.GRAPH.FILE)
			(SETQ PREP*NUMBER.OF.AXIOM.CLAUSES PREP*STEPNUMBER)
			(PREP=CREATE.SPLITGRAPHS virtual.initial.graph.flag)
			#|(OS=close.GRAPH.FILE)|#)
		     (virtual.initial.graph.flag (SETQ PREP*AXIOM.GRAPH.FILE (mkrp-OPENOUT PREP*AXIOM.GRAPH.FILE nil))
						 (SETQ PREP*AXIOM.GRAPH.FILE.## (pathname PREP*AXIOM.GRAPH.FILE))
						 (PREP=VIRTUAL.PARTIAL.GRAPH PREP*AXIOM.GRAPH.FILE)
						 (CLOSEFILE PREP*AXIOM.GRAPH.FILE)
						 (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES PREP*STEPNUMBER)
						 (PREP=CREATE.SPLITGRAPHS virtual.initial.graph.flag))))))
	(RETURN RESULT)))

(defun prep=add.theorem (clause)
  (prep=add.clause clause 'theorem))

(DEFUN PREP=CREATE.SPLITGRAPHS (virtual)		; EDITED:  6-MAR-84 20:59:34
						; EFFECT: FOR ALL SPLITPARTS THE INITIAL GRAPHS ARE
						;         CONSTRUCTED AND SAVED ON THE INITIAL.GRAPH
						;         FILE
						; VALUE:  A LEGAL TOTAL.RESULT
  (let ((RESULT 'SUCCESS)
	SPLITPART.RESULT
	(SPLITPART.IDENTIFIER (LIST 0))
	NORMALIZED.THEOREMS)
    (WHILE (NORM-more.SPLITPARTS?)
      (incf (CAR SPLITPART.IDENTIFIER))
      (PREP=PR_THEOREMS.START SPLITPART.IDENTIFIER PREP*SEVERAL.SPLITPARTS)
      (COND
	((AND (NEQ (CAR SPLITPART.IDENTIFIER) 1) PREP*AXIOM.GRAPH.FILE.##)
	 (LOAD PREP*AXIOM.GRAPH.FILE.##))
	((AND PREP*SEVERAL.SPLITPARTS (NOT PREP*AXIOM.GRAPH.FILE.##)) (PREP=CREATE.EMPTY.PARTIAL.GRAPH)))
      (SETQ NORMALIZED.THEOREMS (SORT (NORM-NEXT.SPLITPART)
				      #'(LAMBDA (CLAUSE1 CLAUSE2)
					  (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
      (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'NORMALIZED NORMALIZED.THEOREMS)
      (SETQ PREP*STEPNUMBER PREP*NUMBER.OF.AXIOM.CLAUSES)
      (SMAPC #'(LAMBDA (CLAUSE)
		 (incf PREP*STEPNUMBER)
		 (SETQ SPLITPART.RESULT (prep=add.theorem CLAUSE))
		 (PREP=PR_GRAPH.DUMP PREP*STEPNUMBER))
	     #'(LAMBDA (TAIL) (COND ((NEQ (CAR SPLITPART.RESULT) 'SUCCESS) (CDR TAIL))))
	     NORMALIZED.THEOREMS)      
      (when (opt-get.option sort_literals) (cons-construct.links (cg-clauses all) nil))
      (COND ((NEQ (CAR SPLITPART.RESULT) 'SUCCESS)
	     (SETQ SPLITPART.RESULT (PREP=REDUCE.INITIAL))))
      (when (or  PREP*SEVERAL.SPLITPARTS virtual) (PREP=VIRTUAL.INITIAL.GRAPH SPLITPART.IDENTIFIER PREP*SEVERAL.SPLITPARTS SPLITPART.RESULT))
      (COND ((NULL SPLITPART.RESULT) (SETQ RESULT NIL) (PREP=PR_INITIAL.GRAPH))
	    ((EQL (CAR SPLITPART.RESULT) 'SUCCESS) (PREP=PR_PRINT.MESSAGE "Refutation detected"))
	    ((EQL (CAR SPLITPART.RESULT) 'FAILURE) (PREP=PR_PRINT.MESSAGE "Graph collapsed")
	     (SETQ RESULT
		   (COND ((EQL (CAR SPLITPART.IDENTIFIER) 1) 'FAILURE)
			 ((EQL RESULT 'SUCCESS) 'FAILURE)
			 ((EQL RESULT 'FAILURE) 'FAILURE)
			 (T NIL)))))
      (PREP=PR_THEOREMS.END SPLITPART.IDENTIFIER PREP*SEVERAL.SPLITPARTS SPLITPART.RESULT))
    RESULT))

(DEFUN PREP=REDUCE.INITIAL NIL			; EDITED:  6-MAR-84 21:08:41
						; EFFECT: THE INITIAL REDUCTIONS ARE PERFORMED.
						; VALUE:  A LEGAL REFUTATION.RESULT OR NIL
  (let ((TIME (GET-INTERNAL-RUN-TIME)))
    (prog1 (if (opt-get.option sort_literals)
	       (RED-REDUCE.GRAPH)
	       (RED-REDUCE.INITIAL.GRAPH))
	   (PREP=PR_STATISTICS "RED.INIT" (mkrp-TIME TIME))
	   (PREP=PR_CHANGES PREP*STEPNUMBER "INITIAL.REDUCTIONS") (CG-FIX NIL))))

(defun prep=get.inserted ()
  (CG-LINKS (DS-LINK.COLOURS.FOR 'INITIAL) INSERTED))

(DEFUN PREP=ADD.CLAUSE (CLAUSE CLAUSETYPE)	; INPUT:  A CLAUSE IN LISP FORMAT AND ONE OF THE ATOMS
						;         AXIOM OR THEOREM
						; EFFECT: THE CLAUSES ARE CONSTRUCTED AND ADDED TO THE
						;         GRAPH.THE INITIAL LINKS ARE CREATED AND
						;         ADDED TO THE GRAPH.PARTIAL REDUCTIONS ARE
						;         PERFORMED. TRIVIAL REFUTATIONS ARE
						;         SEARCHED.INITIAL REWRITING IS PERFORMED
						;         THE NEW AND CHANGED CLAUSES ARE GIVEN TO THE
						;         TWO-MODULE.
						; VALUE:  A LEGAL REFUTATION RESULT OR NIL
  (PROG
    (RESULT INSERTED.CLAUSES CHANGED.CLAUSES TWO.CLAUSE TWO.INSERTED.LINKS TWO.INSERTED.CLAUSES FACTOR.CLAUSES
     (TIME (GET-INTERNAL-RUN-TIME)))
    (OP-CONSTRUCT.CLAUSES  (CONS CLAUSE NIL) CLAUSETYPE)
    (PREP=PR_PARTIAL.GRAPH (CG-CLAUSES INSERTED))    
    (mapc #'(lambda (clause1)
	      (red-rewrite clause1)
	      (mapc #'(lambda (litno)
			(ds-clause.rewrite.rule.set clause1 litno))
		    (ds-clause.compute.max.litno clause1)))
	  (copy-list (CG-CLAUSES INSERTED)))
    (let ((domain (ds-finite.domain.domain)))
      (when (and domain (eq 'theorem clausetype))
	(mapc #'(lambda (clause)
		      (let ((variables (remove-if-not #'(lambda (var) (eql (dt-variable.sort var) (dt-constant.sort (first domain))))
						      (ds-clause.variables clause))))
			(CARTESIAN.LOOP (make-list (length variables) :initial-element (copy-list domain))
					#'(lambda (lists)
					    (let ((inst.uni (zip variables (mapcar #'first lists) t)))
					      (op-create.instance inst.uni clause))))
			(when variables (red-remove.clause clause t "Instantiation with all instances." nil))))
	     (copy-list (cg-clauses inserted)))))
    (cg-disjointify)    
    (unless (opt-get.option sort_literals) (CONS-CONSTRUCT.LINKS (CG-CLAUSES INSERTED) NIL))
    (PREP=PR_CHANGES PREP*STEPNUMBER "NEW.CLAUSES")
    (let ((RESULT (PREP=REDUCE.PARTIAL (CG-CLAUSES INSERTED) NIL)))
      (when result (return RESULT)))
    (CG-DISJOINTIFY)
    (SETQ INSERTED.CLAUSES (COPY-list (CG-CLAUSES INSERTED))
	  CHANGED.CLAUSES  (COPY-list (CG-CLAUSES CHANGED)))
    (PREP=PR_STATISTICS CLAUSETYPE (mkrp-TIME TIME))
    (PREP=PR_CHANGES PREP*STEPNUMBER "INSERTION AND PARTIAL REDUCTION")
    (CG-FIX NIL)
    (SETQ TIME (GET-INTERNAL-RUN-TIME))
    (PREP=CREATE.UNIT.FACTORS PREP*STEPNUMBER INSERTED.CLAUSES)
    (when (CG-CLAUSES INSERTED)
      (when (SETQ RESULT (PREP=REDUCE.PARTIAL (CG-CLAUSES INSERTED) NIL)) (return RESULT))
      (CG-DISJOINTIFY)
      (SETQ INSERTED.CLAUSES (REMOVE-DUPLICATES (APPEND (CG-CLAUSES INSERTED) (copy-list INSERTED.CLAUSES)))
	    CHANGED.CLAUSES  (REMOVE-DUPLICATES (APPEND (CG-CLAUSES CHANGED) (copy-list CHANGED.CLAUSES))))
      (PREP=PR_STATISTICS "UNIT.FAC" (mkrp-TIME TIME))
      (PREP=PR_CHANGES PREP*STEPNUMBER "PARTIAL REDUCTIONS WITH UNIT.FACTORS")
      (CG-FIX NIL)
      (SETQ TIME (GET-INTERNAL-RUN-TIME)))
    (WHILE (AND (OPT-GET.OPTION TWO_RULES) (OR INSERTED.CLAUSES CHANGED.CLAUSES))
      (SETQ TWO.CLAUSE           (if INSERTED.CLAUSES (POP INSERTED.CLAUSES) (POP CHANGED.CLAUSES))
	    TWO.INSERTED.CLAUSES (TWO-UPDATE.RULES TWO.CLAUSE))
      (when (AND TWO.INSERTED.CLAUSES (ATOM TWO.INSERTED.CLAUSES))
	(PREP=PR_STATISTICS "TWO.SUC" (mkrp-TIME TIME))
	(PREP=PR_CHANGES PREP*STEPNUMBER "EMPTY CLAUSE FOUND IN TWO-LITERAL RULES")
	(CG-FIX NIL)
	(RETURN (LIST 'SUCCESS TWO.INSERTED.CLAUSES)))
      (SETQ TWO.INSERTED.LINKS (prep=get.inserted))
      (when (CG-LINKS SI INSERTED)
	(SETQ FACTOR.CLAUSES NIL)
	(MAPC #'(LAMBDA (LINK) (SETQ FACTOR.CLAUSES (INSERT (DS-LINK.POSPAR LINK) FACTOR.CLAUSES)))
	      (CG-LINKS SI INSERTED))
	(PREP=CREATE.UNIT.FACTORS PREP*STEPNUMBER FACTOR.CLAUSES))
      (PREP=PR_STATISTICS "TWO.INS" (mkrp-TIME TIME))
      (when (OR (CG-CLAUSES REMOVED) TWO.INSERTED.LINKS)
	(PREP=PR_CHANGES PREP*STEPNUMBER "RESULTS OF TWO-LITERAL TREATMENT"))
      (CG-FIX T) 
      (SETQ TIME (GET-INTERNAL-RUN-TIME))
      (MAPC #'(LAMBDA (CLAUSE) (CG-INSERT.CLAUSE CLAUSE NIL NIL "CLAUSE CREATED BY TWO-LITERAL-RULE" NIL))
	    TWO.INSERTED.CLAUSES)
      (CONS-CONSTRUCT.LINKS TWO.INSERTED.CLAUSES NIL)
      (when (OR (CG-CLAUSES INSERTED) (prep=get.inserted))
	(PREP=PR_CHANGES PREP*STEPNUMBER "CHANGES BY TWO-LITERAL-RULE"))
      (when (SETQ RESULT (PREP=REDUCE.PARTIAL (CG-CLAUSES INSERTED) TWO.INSERTED.LINKS))
	(RETURN RESULT))
      (CG-DISJOINTIFY)
      (SETQ CHANGED.CLAUSES (REMOVE-DUPLICATES (APPEND CHANGED.CLAUSES (copy-list (CG-CLAUSES CHANGED)))))
      (PREP=PR_STATISTICS "TWO.RED" (mkrp-TIME TIME))
      (PREP=PR_CHANGES PREP*STEPNUMBER "PARTIAL REDUCTIONS AFTER TWO-LITERAL.RULE") (CG-FIX NIL)
      (SETQ TIME (GET-INTERNAL-RUN-TIME)))
    (RETURN NIL)))

(DEFUN PREP=CREATE.UNIT.FACTORS (STEPNUMBER CLAUSES.DEDUCED)
						; EDITED: 25-FEB-84 17:25:03        NE
						; INPUT:  THE CURRENT STEP NUMBER AND A LIST OF
						;         CLAUSES (NOT EQ TO ANY (CG-CLAUSES ...))
						; EFFECT: DEDUCES ALL POSSIBLE UNIT FACTORS OF THE
						;         GIVEN CLAUSES AND PROOCOTLS THE CHANGES.
						; VALUE:  NIL
						; REMARK: PRESENTLY NO REFUTATION RESULT POSSIBLE.
  (PROG (NEW.CLAUSES) (MAPC (FUNCTION (LAMBDA (CLAUSE) (OP-CREATE.UNIT.FACTORS CLAUSE))) CLAUSES.DEDUCED)
	(SETQ NEW.CLAUSES (SET-DIFFERENCE (CG-CLAUSES INSERTED) CLAUSES.DEDUCED))
	(COND (NEW.CLAUSES (PREP=PR_CHANGES STEPNUMBER "UNIT FACTORING")))))

(DEFUN PREP=REDUCE.PARTIAL (CLAUSES LINKS)
						; EDITED: 20-MAR-84 17:26:52
						; INPUT:  A LIST OF CLAUSES AND A LIST OF LINKS
						; EFFECT: THE CLAUSES AND LINKS ARE PARTIALLY REDUCED
						; VALUE:  A LEGAL REFUTATION.RESULT OR NIL
  (let (RESULT
	(TIME (GET-INTERNAL-RUN-TIME)))
    (SETQ RESULT (RED-REDUCE.INITIAL.GRAPH.PARTIAL CLAUSES LINKS))
    (COND
      (RESULT (PREP=PR_CHANGES PREP*STEPNUMBER "PARTIAL REDUCTIONS SUCCEEDEED")
	      (PREP=PR_STATISTICS "RED.SUC" (mkrp-TIME TIME)) (CG-FIX NIL)))
    RESULT))

(DEFUN PREP=THEOREMS.CLAUSE.LISTS (FORMULALIST)
						; INPUT:  A LIST OF FOPC EXPRESSIONS IN PREFIX FORM.
						; EFFECT: THE NEGATION OF THE CONJUNCTION OF THE
						;         FORMULAE IS NORMALIZED.
						;         AN OR SPLIT OF THIS NEGATION (I.E. AN AND
						;         SPLIT OF THE UNNEGATED CONJUNCTION) IS
						;         PERFORMED IF DESIRED, OTHERWISE THE FORMULA
						;         IS TREATED AS IF ONLY ONE SPLIT PART WAS
						;         OBTAINED.
						; VALUE:  IF THE NEGATED CONJUNCTION OF FORMULALIST
						;         IS RECOGNIZED AS UNSATISFIABLE, THEN THE
						;         ATOM FALSE.
						;         IF IT IS RECOGNIZED AS VALID, THEN THE ATOM
						;         TRUE, OTHERWISE NIL.
						; REMARK: THE EMPTY FORMULALIST AS THEOREM IS CONSI-
						;         DERED AS FALSE, I.E. ITS NEGATION IS VALID.
  (COND
    (FORMULALIST
     (PROG (NEGATED.FORMULA PRESIMPLIFIED.FORMULA NORMALIZED.SPLITPARTS )
	   (SETQ NEGATED.FORMULA (PREP=NEGATION.OF.CONJUNCTION FORMULALIST))
	   (SETQ PRESIMPLIFIED.FORMULA (PSIM-PRESIMPLIFICATION NEGATED.FORMULA))
	   (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'PRESIMPLIFIED PRESIMPLIFIED.FORMULA)
	   (COND ((MEMBER PRESIMPLIFIED.FORMULA '(TRUE FALSE)) (RETURN PRESIMPLIFIED.FORMULA)))
	   (SETQ NORMALIZED.SPLITPARTS (NORM-NEW.FORMULA PRESIMPLIFIED.FORMULA (OPT-GET.OPTION GEN_SPLITTING)))
	   (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'NORMALIZED NORMALIZED.SPLITPARTS) (RETURN NIL)))
    (T 'TRUE)))

(DEFUN PREP=NEGATION.OF.CONJUNCTION (EXPRESSIONS)
						; INPUT: LIST OF ONE OR MORE FOPC EXPRESSIONS.
						; VALUE: ONE FOPC EXPRESSION REPRESENTING THE
						;        NEGATION OF THE CONJUNCTION OF THE GIVEN
						;        EXPRESSIONS.
  (LIST 'NOT
	(PROG ((CONJUNCTION (POP EXPRESSIONS)))
	      (WHILE EXPRESSIONS (SETQ CONJUNCTION (LIST 'AND CONJUNCTION (POP EXPRESSIONS)))) (RETURN CONJUNCTION))))

(DEFUN PREP=REMOVE.COMMENTS (FORMULAS)
						; EDITED: 21-OCT-83 12:56:10
						; INPUT:  AN LIST OF PREFIXFORMULAS WITH COMMENTS
						; VALUE:  THE LIST WITHOUT COMMENTS
  (REMOVE-IF #'(LAMBDA (FORMULA) (member FORMULA '(COMMENT)))
	     FORMULAS))

(DEFUN PREP=ELIMINATE.EQUALITY (AXIOMS THEOREMS)
						; INPUT:  TWO FORMULAS IN PREFIX-FORM
						; EFFECT: DELETES THE EQUALITY AS A DATATERM OBJECT
						;          IF IT DOES NOT OCCUR IN THE GIVEN FORMULAS
						; VALUE:  UNDEFINED
  (COND
    ((NOT
       (MEMBER-IF
         (FUNCTION
           (LAMBDA (PREDICATE)
             (OR
               (MEMBER-IF (FUNCTION (LAMBDA (EXPRESSION) (PREP=PREDICATE.OCCURS.IN PREDICATE EXPRESSION))) AXIOMS)
               (MEMBER-IF (FUNCTION (LAMBDA (EXPRESSION) (PREP=PREDICATE.OCCURS.IN PREDICATE EXPRESSION))) THEOREMS))))
         (DT-PREDICATE.EQUALITIES)))
     (MAPC (FUNCTION (LAMBDA (PREDICATE) (DT-PREDICATE.DELETE PREDICATE))) (DT-PREDICATE.EQUALITIES)))))

(DEFUN PREP=PREDICATE.OCCURS.IN (PREDICATE EXPRESSION)
						; INPUT:  AN ADDRESS OF AN PREDICATE SYMBOL AND
						;         A FORMULA IN PREFIX-FORM
						; EFFECT: RETURNS VALUE
						; VALUE:  T IF PREDICATE OCCURS IN THE FORMULA
						;         ELSE NIL
  (CASE (CAR EXPRESSION) ((ALL EX) (PREP=PREDICATE.OCCURS.IN PREDICATE (THIRD EXPRESSION)))
	(NOT (PREP=PREDICATE.OCCURS.IN PREDICATE (SECOND EXPRESSION)))
	((AND OR IMPL EQV)
	 (OR (PREP=PREDICATE.OCCURS.IN PREDICATE (SECOND EXPRESSION))
	     (PREP=PREDICATE.OCCURS.IN PREDICATE (THIRD EXPRESSION))))
	((+ -) (EQL PREDICATE (SECOND EXPRESSION))) (OTHERWISE NIL)))

(DEFUN PREP=CREATE.EMPTY.GRAPH NIL
						; INPUT:  NONE
						; EFFECT: ALL COMPONENTS OF THE ACTUAL GRAPH ARE
						;         DESTROYED AND THE GRAPH IS RESET TO THE
						;         EMPTY GRAPH.
						; VALUE:  UNDEFINED.
  (DT-RESET)
  (NORM-RESET)
  (OP-RESET)
  (nar-reset)
  (UNI-RESET)
  (RED-RESET 'INITIAL)
  (CONS-RESET)
  (TWO-RESET))

(DEFUN PREP=CREATE.EMPTY.PARTIAL.GRAPH NIL
						; INPUT:  NONE
						; EFFECT: ALL COMPONENTS OF THE ACTUAL GRAPH ARE
						;         DESTROYED AND THE GRAPH IS RESET TO THE
						;         EMPTY GRAPH.THE NORMALIZATION PARTS ARE NOT
						;         CHANGEDED.
						; VALUE:  UNDEFINED.
  (OP-RESET)
  (nar-reset)
  (RED-RESET 'INITIAL)
  (UNI-RESET)
  (CONS-RESET)
  (TWO-RESET))

(DEFUN PREP=VIRTUAL.PARTIAL.GRAPH (FILE)
						; INPUT:   EITHER NIL OR THE NAME OF A FILE WHICH IS
						;          OPEN FOR OUTPUT.
						; EFFECT:  COMPUTES AN S-EXPRESSION REPRESENTING THE
						;          ACTUAL GRAPH SUCH THAT (EVAL S-EXPRESSION)
						;          CREATES THE VERY SAME GRAPH AS IT IS
						;          DURING INVOCATION OF THIS FUNCTION.
						;          IF FILE <> NIL, THE S-EXPRESSION IS WRITTEN
						;          ON FILE, OTHERWISE THE S-EXPRESSION IS THE
						;          VALUE OF THIS FUNCTION.
						; VALUE:   IF FILE = NIL THEN THE COMPUTED
						;          S-EXPRESSION, ELSE NIL.
  (PROG ((EXPRESSION (reverse `(,(CONS-VIRTUAL.GRAPH nil)
				,(RED-SAVE nil)
				,(copy-tree (TWO-SAVE nil))
				(PREP=CREATE.EMPTY.PARTIAL.GRAPH)	; Order of evaluation --> reverse
				progn))))
	(COND
	  (FILE (PRIN1 EXPRESSION file))
	  (T (RETURN expression)))))

(DEFUN PREP=VIRTUAL.INITIAL.GRAPH (SPLITPART.IDENTIFIER SPLITPARTFLAG INITIAL.RESULT)
						; EDITED:  7-MAR-84 09:35:57
						; INPUT:  A LIST OF INTEGERS,A BOOLEAN VALUE AND A
						;         LEGAL REFUTATION RESULT
						; EFFECT: WRITES THE S-EXPRESSION ON THE INITIAL GRAPH
						;         FILE,WHICH HAS TO BE OPEN FOR OUTPUT :
						;         (CTL-INITIAL.GRAPH SPLITPART.IDENTIFIER
						;                            SPLITPART.FLAG
						;                            INITIAL.RESULT
						;                            REDUCED.FLAG
						;                            SELECTION.FLAG
						;                            EXPRESSION>
						;         WHERE EXPRESSION IS EITHER NIL IF
						;         INITIAL.RESULT IS A LEGAL REFUTATION.RESULT
						;         OR AN S-EXPRESSION WHICH WHEN EVALUATED
						;         CREATES THE SAME STATE AS AT INVOCATION
						;         EXCEPT OF THE STATE OF THE PREPROCESSING
						;         MODULES
						; VALUE:  UNDEFINED
  (LET ((FILE PREP*GRAPH.FILE))
    (FORMAT FILE "(CTL-INITIAL.GRAPH '~A ~A '~A T NIL " SPLITPART.IDENTIFIER SPLITPARTFLAG  INITIAL.RESULT)
    (red-save.reset)
    (sel-save.reset)
    (IF  INITIAL.RESULT
	 (FORMAT FILE "NIL)")
	 (progn (FORMAT FILE "(PROGN ")
		(OP-SAVE FILE)
		(RED-SAVE FILE)
		(PRINC "))" FILE)))))

(DEFUN PREP=CREATE.CLAUSES (PREFIXFORMULA CLAUSETYPE)
						; INPUT:  A FOPC EXPRESSION IN PREFIX FORM AND A
						;         CLAUSETYPE, WHICH MAY BE 'AXIOM OR 'THEOREM.
						; EFFECT: PREFIXFORMULA IS PRESIMPLIFIED, NORMALIZED
						;         AND CONVERTED TO CLAUSAL FORM AND ADDED TO
						;         THE GRAPH.
						;         THE ATTRIBUTE-CLAUSES ARE CONSTRUCTED AND
						;         ADDED TO THE GRAPH
						;         THE CLAUSE NAMES ARE CLAUSENAMESTRING
						;         CONCATENATED WITH INTEGERS.
						;         THE INTERMEDIATE RESULTS ARE PROTOCOLLED ON
						;         THE PROTOCOL FILE AND THE TRACE FILE
						; VALUE:  A LEGAL REFUTATION.RESULT OR NIL
  (PROG (PRESIMPLIFIED.FORMULA NORMALIZED.FORMULA RESULT)
	(SETQ PRESIMPLIFIED.FORMULA (PSIM-PRESIMPLIFICATION PREFIXFORMULA))
	(PREP=PR_PREPROCESSED.PREFIX.FORMULA 'PRESIMPLIFIED PRESIMPLIFIED.FORMULA)
	(COND ((EQUAL PRESIMPLIFIED.FORMULA 'TRUE)
	       (RETURN NIL))
	      ((EQUAL PRESIMPLIFIED.FORMULA 'FALSE)
	       (return (list 'success (if (EQL CLAUSETYPE 'AXIOM)
					  'AXIOMS.UNSATISFIABLE
					  'THEOREMS.VALID)))))
	(SETQ NORMALIZED.FORMULA (CAR (NORM-NORMALIZATION PRESIMPLIFIED.FORMULA NIL)))
	(PREP=PR_PREPROCESSED.PREFIX.FORMULA
	  'NORMALIZED NORMALIZED.FORMULA)	;    CONSTRUCTION OF THE ATTRIBUTE-CLAUSES
						;    THERE IS NO CG-FIX ALLOWED UNTIL THE FIRST CALL
						;    OF PREP=ADD.CLAUSE
	(cons-CONSTRUCT.ATTRIBUTE.CLAUSES)	;    CONSTRUCTING THE CLAUSES AND ADDING THEM TO THE
						;    ACTUAL GRAPH
	(SMAPC #'(LAMBDA (CLAUSE)
		   (incf PREP*STEPNUMBER)
		   (SETQ RESULT (PREP=ADD.CLAUSE CLAUSE CLAUSETYPE))
		   (PREP=PR_GRAPH.DUMP PREP*STEPNUMBER))
	       #'(LAMBDA (TAIL) (unless (EQ (CAR RESULT) 'SUCCESS) (CDR TAIL)))
	       NORMALIZED.FORMULA)
	(RETURN RESULT)))

(DEFUN PREP=PR_OPEN (CODE.FILE.NAME COMMENT name)	; edited: 25-feb-84 16:13:09
						; input:  name of a file and a comment
						; effect: gives the name of the code.file to the
						;         protocol,opens the trace file, prints a header on trace file.
						; value:  undefined.
  (PR-CONSTRUCT.START CODE.FILE.NAME
		      #-symbolics (software-type)
		      #+symbolics (sct:system-version-info T)
		      COMMENT)
  (PROG
    ((FILE (OPT-GET.OPTION TR_TRACE.FILE))
     STREAM
     (HEADLINES
       (LIST "***************************************************************************"
	     ""
	     "   Markgraf Karl system, Uni Kaiserslautern,                version: "
	     (FORMAT NIL "   Trace file                                   date: ~A" (DATE))
	     ""
	     "***************************************************************************")))
    (COND ((NULL FILE) (SETQ PREP*TRACE.OUTPUTSTREAM NIL))
	  ((EQL FILE T)
	   (SETQ prep*TRACE.OUTPUTSTREAM T)
	   (LINELENGTH PREP*TRACE.OUTPUTSTREAM.LINELENGTH T))
	  (T (SETQ PREP*TRACE.OUTPUTSTREAM (SETQ STREAM (mkrp-OPENOUT (mkrp-make.pathname nil nil "text" FILE) nil)))
	     (LINELENGTH PREP*TRACE.OUTPUTSTREAM.LINELENGTH STREAM) (TERPRI STREAM)
	     (MAPC (FUNCTION (LAMBDA (LINE) (PRINC LINE STREAM) (TERPRI STREAM))) HEADLINES)
	     (TERPRI STREAM) (TERPRI STREAM)))))

(DEFUN PREP=PR_CLOSE (PROVED.FLAG)
						; EDITED: 25-FEB-84 16:34:45        NE
						; INPUT:  A BOOLEAN VALUE.
						; EFFECT: PRINTS A FINAL MESSAGE ON TRACE FILE AND
						;         CLOSES TRACE AND PROTOCOL FILE.
						; VALUE:  UNDEFINED
  (let ((STREAM PREP*TRACE.OUTPUTSTREAM)
	(MESSAGE (format nil "Proof ~:[failed~;succeeded~].  End: ~A" PROVED.FLAG (DATE))))
    (COND ((or (NULL STREAM) (EQL STREAM T)) 
	   (PR-CONSTRUCT.END)
	   NIL)
	  (T (format stream "~%~A~2%" MESSAGE)	     
	     (PR-CONSTRUCT.END)			; PR-CONSTRUCT.END writes on stream closed by CLOSE
	     (CLOSEfile STREAM)))
    (SETQ PREP*TRACE.OUTPUTSTREAM NIL)))

(DEFUN PREP=PR_INFIX.FORM (AXIOMS THEOREMS)
						; EDITED: 13-APR-83 08:33:25
						; INPUT:  TWO S-EXPRESSIONS
						; EFFECT: THE S-EXPRESSIONS ARE PRINTED TO THE PROOF
						;         AND TRACE PROTOCOL FILES, IF THE RESP.
						;         OPTIONS ARE SET.
						; VALUE:  UNDEFINED
  (PR-INFIX.FORM AXIOMS THEOREMS)
  (PROG ((STREAM PREP*TRACE.OUTPUTSTREAM))
	(COND
	  ((AND STREAM (OPT-GET.OPTION TR_PREPROCESSING) (NEQ STREAM T)) (TERPRI STREAM)
	   (PRINC "FORMULAE GIVEN TO THE THEOREM PROVER:" STREAM) (TERPRI STREAM) (TERPRI STREAM) (FORMAT STREAM "~T" 11)
	   (PRINC "Axioms:" STREAM)
	   (MAPC (FUNCTION (LAMBDA (AXIOM) (FORMAT STREAM "~T" 25) (PREP=PR_PRETTY.LIST.EXPRESSION AXIOM))) AXIOMS)
	   (TERPRI STREAM) (FORMAT STREAM "~T" 11) (PRINC "Theorem:" STREAM)
	   (MAPC
	     (FUNCTION (LAMBDA (THEOREM) (FORMAT STREAM "~T" 25) (PREP=PR_PRETTY.LIST.EXPRESSION THEOREM))) THEOREMS)
	   (TERPRI STREAM) (TERPRI STREAM)))))

(DEFUN PREP=PR_PREFIX.FORM (AXIOMS THEOREMS)
						; INPUT:  TWO S-EXPRESSIONS
						; EFFECT: THE S-EXPRESSIONS ARE PRINTED ON THE PROOF
						;         AND TRACE PROTOCOL FILES, IF THE RESP.
						;         OPTIONS ARE SET.
						; VALUE:  UNDEFINED.
  (PR-PREFIX.FORM AXIOMS THEOREMS)
  (PROG ((STREAM PREP*TRACE.OUTPUTSTREAM))
	(COND
	  ((AND STREAM (OPT-GET.OPTION TR_PREPROCESSING))
	   (PREP=PR_PREFIX.EXPRESSION "INITIAL PREFIX FORM OF AXIOMS:" AXIOMS)
	   (PREP=PR_PREFIX.EXPRESSION "INITIAL PREFIX FORM OF THEOREMS:" THEOREMS)))))

(DEFUN PREP=PR_OPTIONS NIL
						; EDITED: 25-FEB-84 16:55:44        NE
						; EFFECT: PRINTS THE OPTION SETTING ON PROTOCOL AND
						;         TRRACE FILE.
						; VALUE: UNDEFINED.
  (PR-OPTIONS)
  (PROG ((STREAM PREP*TRACE.OUTPUTSTREAM))
	(COND
	  ((AND STREAM (NEQ STREAM T)) (PRINC "Adjustment of the options:" STREAM) (TERPRI STREAM) (TERPRI STREAM)
	   (MAPC
	     (FUNCTION
	       (LAMBDA (OPTION.VALUE) (FORMAT STREAM "~T" 6) (PRINC (CAR OPTION.VALUE) STREAM) (FORMAT STREAM "~T" 50)
		       (PRINC (CDR OPTION.VALUE) STREAM) (TERPRI STREAM)))
	     (OPT-GET.LIST.OPTIONS))
	   (TERPRI STREAM) (TERPRI STREAM)))))

(DEFUN PREP=PR_AXIOMS.START NIL
						; EDITED: 11-APR-83 16:15:52
						; INPUT:  NONE
						; EFFECT: OPENS THE AXIOM BLOCK OF THE PROOF PROTOCOL
						;         FILE, IF THE RESP. OPTION IS SET.
						; VALUE:  UNDEFINED.
  (PR-AXIOMS.START))

(DEFUN PREP=PR_AXIOMS.END (RESULT)
						; EDITED: 11-APR-83 16:15:52
						; INPUT:  A LEGAL REFUTATION RESULT OR NIL
						; EFFECT: CLOSES THE AXIOM BLOCK OF THE PROOF PROTOCOL
						;         FILE, IF THE RESP. OPTION IS SET.
						; VALUE:  UNDEFINED.
  (PR-AXIOMS.END RESULT))

(DEFUN PREP=PR_THEOREMS.START (SPLITPART.IDENTIFIER SPLITFLAG)
						; INPUT:  A LIST OF INTEGERS AND A BOOLEAN VALUE
						; EFFECT: REPORTS THE BEGINNING OF THE THEOREM-PART
						;         TO THE PR-MODULE AND PRINTS A MESSAGE ON THE
						;         TRACE FILE IF SPLITFLAG IS NOT NIL
						; VALUE:  UNDEFINED
  (when SPLITFLAG
    (PREP=PR_PRINT.MESSAGE.FULL.LINE #\/ (format nil "Construction of the initial graph of split part ~a initiated."
						 SPLITPART.IDENTIFIER)))
  (PR-THEOREMS.START SPLITPART.IDENTIFIER SPLITFLAG))

(DEFUN PREP=PR_THEOREMS.END (SPLITPART.IDENTIFIER SPLITFLAG RESULT)
						; edited:  5-mar-84 17:59:36
						; input:  a list of integers and a boolean value
						;         and a refutation result or nil
						; effect: reports the end of the theorem-part to the
						;         pr-module and prints a message on the
						;         trace-file,if splitflag is not nil
						; value:  undefined
  (PR-THEOREMS.END RESULT)
  (WHEN SPLITFLAG
    (PREP=PR_PRINT.MESSAGE.FULL.LINE #\/ (FORMAT NIL "Construction of the initial graph of split part ~a ~a."
						 SPLITPART.IDENTIFIER
						 (CASE (CAR RESULT)
						   (SUCCESS "succeeded")
						   (FAILURE "failed")
						   (OTHERWISE "terminated"))))))

(DEFUN PREP=PR_PREPROCESSED.PREFIX.FORMULA (HEADERCODE EXPRESSION)
						; INPUT:  A HEADERCODE FROM (PRESIMPLIFIED
						;         NORMALIZED) AND AN S-EXPRESSION
						; EFFECT: DEPENDEND ON THE HEADERCODE A
						;         HEADER IS PRINTED, EXPRESSION PRETTYPRINTED
						;         ON THE TRACE FILES.
						; VALUE:  UNDEFINED.
  (COND
    ((AND (OPT-GET.OPTION TR_PREPROCESSING) PREP*TRACE.OUTPUTSTREAM)
     (CASE HEADERCODE
       (PRESIMPLIFIED (PREP=PR_PREFIX.EXPRESSION "PRESIMPLIFIED FORMULA" EXPRESSION))
       (NORMALIZED (PREP=PR_PREFIX.EXPRESSION "NORMALIZED FORMULA" EXPRESSION))
       (OTHERWISE (ERROR "ILLEGAL HEADERCODE IN PREP=PR_PREPROCESSED.PREFIX.FORM : : ~A" HEADERCODE))))))

(DEFUN PREP=PR_PARTIAL.GRAPH (CLAUSES)
						; EDITED: 13-APR-83 16:38:31
						; INPUT:  A LIST OF CLAUSE ADRESSES.
						; EFFECT: REPORTS THE CLAUSES TO THE PR-MODULE
						; VALUE:  UNDEFINED
  (PR-PARTIAL.GRAPH CLAUSES))

(DEFUN PREP=PR_INITIAL.GRAPH NIL
						; EDITED: 16-OCT-84 14:17:17
						; EFFECT: THE INITIAL GRAPH IS REPORTED TO THE
						;         PR-MODULE. FURTHER THE GRAPH IS
						;         PRINTED TO THE TRACE FILE.
						; VALUE: UNDEFINED
  (PR-INITIAL.GRAPH)
  (COND
    ((AND PREP*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_STEP.MODE))
     (PROG (STEP.MODE STEP.MESSAGE STEP.CLAUSES STEP.LINKS) (SETQ STEP.MODE (OPT-GET.OPTION TR_STEP.MODE))
	   (SETQ STEP.MESSAGE (LIST 'MESSAGE "INITIAL GRAPH AFTER SAVING ON FILE: "))
	   (SETQ STEP.CLAUSES (LIST 'CLAUSES 'ALL STEP.MODE))
	   (SETQ STEP.LINKS (COND ((EQL STEP.MODE 'I) '(LINKS ALL NIL I)) (T NIL)))
	   (CG-DUMP PREP*TRACE.OUTPUTSTREAM (list STEP.MESSAGE STEP.CLAUSES STEP.LINKS)))))
  (COND ((AND PREP*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_DUMP)) (PREP=PR_GRAPH.DUMP 'INITIAL)))
  (PROG ((STREAM PREP*TRACE.OUTPUTSTREAM))
	(COND
	  (STREAM (PRINC "TABLE OF DATATERM-OBJECTS: " STREAM)
		  (DT-PRINT.SYMBOLS '(CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL) STREAM 7 (LINELENGTH NIL STREAM)))))
  (COND ((OPT-GET.OPTION TR_TERMINAL) (CG-DUMP.SHORT *standard-output* NIL NIL NIL L))))

(DEFUN PREP=PR_CHANGES (STEPNUMBER EXPLANATION)
  (declare (SPECIAL DUMP.MESSAGE DUMP.MESSAGE.RULES DUMP.MESSAGE.NORULES DUMP.RULES DUMP.NORULES DUMP.CLAUSES.INSERTED
		    DUMP.CLAUSES.CHANGED DUMP.CLAUSES.REMOVED DUMP.LINKS.INSERTED DUMP.LINKS.CHANGED DUMP.LINKS.REMOVED))
						; EDITED: 26-FEB-84 16:28:52        NE
						; INPUT:  THE CURRENT STEP NUMBER, A STRING
						; EFFECT: PRINTS INFORMATION ABOUT CHANGED OBJECTS OF
						;         THE CURRENT GRAPH ON TRACE FILE.
						; VALUE:  UNDEFINED.
  (COND
    ((AND PREP*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_STEP.MODE))
     (PROG ((MODE (OPT-GET.OPTION TR_STEP.MODE)))
	   (SETQ DUMP.MESSAGE (LIST 'MESSAGE (format nil " Preparation step: ~A - ~A" STEPNUMBER EXPLANATION)))
	   (SETQ DUMP.RULES
		 (COND ((SETQ DUMP.RULES (TWO-LAST.CALL.NEW.RULES)) (LIST 'CLAUSES (LIST 'QUOTE DUMP.RULES) MODE))))
	   (SETQ DUMP.MESSAGE.RULES
		 (COND (DUMP.RULES (LIST 'MESSAGE "   NEW (TWO-LITERAL) RULES ACCEPTED//GENERATED:"))))
	   (SETQ DUMP.NORULES
		 (COND
		   ((SETQ DUMP.NORULES (TWO-LAST.CALL.NEW.NORULES)) (LIST 'CLAUSES (LIST 'QUOTE DUMP.NORULES) MODE))))
	   (SETQ DUMP.MESSAGE.NORULES
		 (COND
		   (DUMP.NORULES (LIST 'MESSAGE "   NEW NORULE CLAUSES GENERATED BY RULE COMPLETION ALGORITHM:"))))
	   (SETQ DUMP.CLAUSES.INSERTED
		 (COND ((NEQ 0 (CG-#CLAUSES INSERTED)) (LIST 'CLAUSES 'INSERTED MODE)) (T NIL)))
	   (SETQ DUMP.CLAUSES.CHANGED
		 (COND ((NEQ 0 (CG-#CLAUSES CHANGED)) (LIST 'CLAUSES 'CHANGED MODE)) (T NIL)))
	   (SETQ DUMP.CLAUSES.REMOVED
		 (COND ((NEQ 0 (CG-#CLAUSES REMOVED)) (LIST 'CLAUSES 'REMOVED 'N)) (T NIL)))
	   (SETQ DUMP.LINKS.INSERTED
		 (COND
		   ((AND (EQL MODE 'I) (NOT (EVERY #'ZEROP (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) INSERTED))))
		    (LIST 'LINKS 'INSERTED NIL MODE))
		   (T NIL)))
	   (SETQ DUMP.LINKS.CHANGED
		 (COND
		   ((AND (EQL MODE 'I) (NOT (EVERY #'ZEROP (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) CHANGED))))
		    (LIST 'LINKS 'CHANGED NIL MODE))
		   (T NIL)))
	   (SETQ DUMP.LINKS.REMOVED
		 (COND
		   ((AND (EQL MODE 'I) (NOT (EVERY #'ZEROP (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) REMOVED))))
		    (LIST 'LINKS 'REMOVED NIL 'A))
		   (T NIL)))
	   (CG-DUMP PREP*TRACE.OUTPUTSTREAM (list '(MESSAGE "") DUMP.MESSAGE DUMP.MESSAGE.RULES DUMP.RULES
						  DUMP.MESSAGE.NORULES DUMP.NORULES DUMP.CLAUSES.INSERTED
						  DUMP.CLAUSES.CHANGED DUMP.CLAUSES.REMOVED
						  DUMP.LINKS.INSERTED DUMP.LINKS.CHANGED DUMP.LINKS.REMOVED)))))
  (COND ((OPT-GET.OPTION TR_TERMINAL) (CG-DUMP.SHORT *standard-output* NIL L L NIL))))

(DEFUN PREP=PR_GRAPH.DUMP (STEPNUMBER)
						; EDITED: 25-FEB-84 19:37:49        NE
						; INPUT:  AN INTEGER DENOTING THE NUMBER OF PREPARATIO
						;         STEPS THAT HAVE BEEN PERFORMED ON THE
						;         CURRENT GRAPH WHICH IS REDUCED AND FIXED.
						;         OR THE ATOM INITIAL.
						; EFFECT: THE CURRENT GRAPH IS DUMPED ON THE TRACE
						;         FILE ACCORDING TO THE TRACE OPTIONS.
						; VALUE: UNDEFINED.
  (COND
    ((AND PREP*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_DUMP)
	  (OR (EQL STEPNUMBER 'INITIAL) (ZEROP (REM STEPNUMBER (OPT-GET.OPTION TR_DUMP)))))
     (PROG (DUMP.MESSAGE DUMP.CLAUSES DUMP.LINKS)	
	   (declare (SPECIAL DUMP.MESSAGE))
	   (SETQ DUMP.MESSAGE
		 (COND ((EQL STEPNUMBER 'INITIAL) (LIST 'MESSAGE "CURRENT GRAPH AFTER INITIAL REDUCTIONS:"))
		       (T
			(LIST 'MESSAGE
			      (CONCATENATE 'STRING "CURRENT GRAPH AFTER " (PRINC-TO-STRING STEPNUMBER) " PREPARATION STEPS:")))))
	   (SETQ DUMP.CLAUSES
		 (COND ((OPT-GET.OPTION TR_CLAUSE.MODE) (LIST 'CLAUSES 'ALL (OPT-GET.OPTION TR_CLAUSE.MODE))) (T NIL)))
	   (SETQ DUMP.LINKS
		 (COND ((OPT-GET.OPTION TR_LINK.MODE) (LIST 'LINKS 'ALL NIL (OPT-GET.OPTION TR_LINK.MODE))) (T NIL)))
	   (COND
	     ((OPT-GET.OPTION TWO_RULES) (TWO-DUMP PREP*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_CLAUSE.MODE))))
	   (CG-DUMP PREP*TRACE.OUTPUTSTREAM (list '(SEPARATOR) DUMP.MESSAGE DUMP.CLAUSES DUMP.LINKS)))))
  (COND ((AND (OPT-GET.OPTION TR_TERMINAL) (NEQ STEPNUMBER 0)) (CG-DUMP.SHORT *standard-output* NIL NIL NIL N))))

(DEFUN PREP=PR_STATISTICS (PREFIX TIME)		; EDITED: 25-FEB-84 14:34:11        NE
						; INPUT:  A STRING AND AN INTEGER.
						; EFFECT: REPORTS CHANGES OF THE CURRENT GRAPH SINCE
						;         THE LAST FIX TO THE PROTOCOL MODULE.
						;         PRINTS A LINE OF STATISTICAL INFORMATION
						;         ON THE TERMINAL.
						; VALUE:  UNDEFINED.
  (PR-STATISTICS TIME)				; positive: position before printing, negative: position after printing
  
  (LET (( LINKS   (APPLY #'+ (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) ALL)))
	(+LINKS   (APPLY #'+ (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) INSERTED)))
	(-LINKS   (APPLY #'+ (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) REMOVED)))
	( CLAUSES (CG-#CLAUSES ALL))
	(+CLAUSES (CG-#CLAUSES INSERTED))
	(-CLAUSES (CG-#CLAUSES REMOVED))
	(STORE (REMAINING-MEMORY)))
    (mkrp-gc.start (opt-get.option gen_lisp.garbage.collection))
    (let ((PREF PREFIX)
	  (TIM TIME)
	  (MEMORY (TRUNCATE (MEM-MEMORY) 1024)))
      (FORMAT T "~va~v@a Msec Links: ~v@a~v@a~v@a Clauses: ~v@a~v@a~v@a Store: ~v@a~v@a~%"
	      15 PREF 7 TIM 5 LINKS
	      5 (FORMAT NIL "+~a" +LINKS) 6 (FORMAT NIL "-~a" -LINKS) 
	      4 CLAUSES 4 (FORMAT NIL "+~a" +CLAUSES) 4 (FORMAT NIL "-~a" -CLAUSES)
	      6 STORE 5 MEMORY))))

(DEFUN PREP=PR_PRINT.MESSAGE (&REST STRINGS)	; INPUT:  ARBITRARILY MANY STRINGS
						; EFFECT: PRINTS THE STRINGS TO THE TRACE FILE AND
						;         THE TERMINAL, EACH INTO A SEPERATE LINE.
						; VALUE:  UNDEFINED.
  (PROG ((STREAM PREP*TRACE.OUTPUTSTREAM))
	(COND
	  ((AND STREAM (NEQ STREAM T)) (MAPC (FUNCTION (LAMBDA (STRING) (PRINC STRING STREAM) (TERPRI STREAM))) STRINGS)))
	(PROGN (MAPC (FUNCTION (LAMBDA (STRING) (PRINC STRING T) (TERPRI T))) STRINGS))))

(DEFUN PREP=PR_PRINT.MESSAGE.FULL.LINE (FILL.CHAR STRING)
						; INPUT:  TWO STRINGS (THE FIRST OF LENGTH ONE)
						; EFFECT: STRING IS PRINTED ON THE TERMINAL AND ON THE
						;         TRACE PROTOCOL FILE, THE LINE IS FILLED WITH
						;         FILL.CHAR
						; VALUE:  UNDEFINED.
  (let ((STREAM PREP*TRACE.OUTPUTSTREAM)
	(output (replace (make-string 117 :initial-element fill.char) (format nil " ~A " STRING) :start1 5)))
    (when (AND STREAM (NEQ STREAM T))
      (format stream "~%~A~%~%" output))
    (format *standard-output* "~%~A~%~%" output)))

(DEFUN PREP=PR_PRETTY.LIST.EXPRESSION (EXPRESSION)
						; EDITED:  5-MAR-84 18:31:11
						; INPUT:  A S-EXPRESSION
						; EFFECT: PRETTY PRINTS THE S-EXPRESSION ONTO THE
						;         TRACE-FILE
						; VALUE:  UNDEFINED
  (PROG ((FLAG NIL) (STREAM PREP*TRACE.OUTPUTSTREAM))
	(COND
	  ((MEMBER (CAR EXPRESSION) '(FUNCTION PREDICATE RELATION))
	   (MAPC (FUNCTION (LAMBDA (X) (SETQ FLAG (PREP=PR_PRETTY.EXPRESSION X FLAG STREAM)))) EXPRESSION))
	  (T (MAPPRINT EXPRESSION NIL NIL " " 'PRINC STREAM)))
	(TERPRI STREAM)))

(DEFUN PREP=PR_PRETTY.EXPRESSION (EXPRESSION AND.SWITCH STREAM)
						; EDITED:  5-MAR-84 18:36:30
						; INPUT:  AN S-EXPRESSION,A FLAG AND A FILE
						; EFFECT: PRETTY PRINTS THE S-EXPRESSION ON THE
						;         TRACE-FILE.IF FLAG IS NOT NIL A TERPRI IS
						;         PRINTED BEFRORE PRINTING AN AND
						; VALUE:  T IF A TERPRI SHOULD BE PRINTED BEFORE
						;         PRINTING AN AND ELSE NIL
  (PROG ((FLAG AND.SWITCH))
	(COND
	  ((ATOM EXPRESSION)
	   (CASE EXPRESSION (IF (TERPRI STREAM) (PRINC "           " STREAM) (SETQ FLAG T))
		 (AND (COND (FLAG (TERPRI STREAM) (PRINC "             " STREAM))))
		 (THEN (TERPRI STREAM) (PRINC "               " STREAM) (SETQ FLAG NIL)) (OTHERWISE NIL))
	   (PRINC EXPRESSION STREAM) (PRINC " " STREAM))
	  (T (PRINC "(" STREAM)
	     (MAPC (FUNCTION (LAMBDA (SUBLIST) (SETQ FLAG (PREP=PR_PRETTY.EXPRESSION SUBLIST FLAG STREAM)))) EXPRESSION)
	     (PRINC ")" STREAM)))
	(RETURN FLAG)))

(DEFUN PREP=PR_PREFIX.EXPRESSION (HEADER EXPRESSION)
						; INPUT:  TWO S-EXPRESSIONS.
						; EFFECT: HEADER IS PRINTED, EXPRESSION PRETTYPRINTED
						;         ON THE TRACE FILE   .
						; VALUE:  UNDEFINED.
  (PROG ((STREAM (SYMBOL-VALUE 'PREP*TRACE.OUTPUTSTREAM))) (PRINC HEADER STREAM) (TERPRI STREAM)
	(PPRINT EXPRESSION STREAM) (TERPRI STREAM)))

(DEFVAR PREP*AXIOM.GRAPH.FILE NIL)

(DEFVAR PREP*AXIOM.GRAPH.FILE.## NIL)

(DEFVAR PREP*PROBLEM.FILE NIL)

(DEFVAR PREP*GRAPH.FILE NIL)

(DEFVAR PREP*SEVERAL.SPLITPARTS NIL)

(DEFVAR PREP*STEPNUMBER NIL)

(DEFVAR PREP*NUMBER.OF.AXIOM.CLAUSES NIL)

(DEFVAR PREP*AXIOMS.INFIX NIL)

(DEFVAR PREP*AXIOMS.PREFIX NIL)

(DEFVAR PREP*THEOREMS.INFIX NIL)

(DEFVAR PREP*THEOREMS.PREFIX NIL)

(DEFVAR PREP*COMMENT NIL)

(DEFVAR PREP*TRACE.OUTPUTSTREAM NIL)

(DEFVAR PREP*TRACE.OUTPUTSTREAM.LINELENGTH 130)

