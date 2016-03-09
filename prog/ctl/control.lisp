;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))


;;; THESE VARIABLES ARE NON-LOCAL IN ORDER TO ENABLE THE INTERPLAY BETWEEN CTL=SAVE AND CTL=CONTINUE.
;;; THEIR PURPOSE IS DESCRIBED BY THE FOLLOWING INVARIANTS:
;;;
;;; INVARIANT.SPLITPARTS
;;;    CTL*SPLITPARTS.REMAINING.FILES IS A LIST OF THE NOT YET COMPLETELY PROCESSED FILES CONTAINING SPLITPARTS
;;;    TO BE REFUTED. NO OTHER THAN THE FIRST ONE IS OPEN AND THIS IS THE CASE IFF CTL*SPLITPARTS.INPUTSTREAM
;;;    IS NON-NIL.
;;;    CTL*SPLITPARTS.INPUTSTREAM IS NIL OR CONNECTED TO THE OPEN FILE IN CTL*SPLITPARTS.REMAINING.FILES.
;;;    IN THE LATTER CASE CTL*SPLITPARTS.INPUTSTREAM.COUNTER IS THE NUMBER OF EXPRESSIONS ALREADY READ FROM THE
;;;    OPEN FILE.
;;;    CTL*SPLITPARTS.RESULTS IS A LIST OF ALL REFUTATION RESULTS OBTAINED SO FAR, LATEST RESULT LAST.
;;;
;;; INVARIANT.CURRENT.SPLITPART:
;;;    CTL*SPLITPART.FLAG IS NIL AS LONG AS THERE IS ONLY A SINGLE GRAPH TO BE REFUTED, OTHERWISE T.
;;;    CTL*CURRENT.SPLITPART.IDENTIFIER IS A LIST OF INTEGERS UNIQUELY IDENTIFYING THE CURRENT GRAPH.
;;;    CTL*CURRENT.SPLITPART.INITIAL.REDUCTION.FLAG IS NIL AS LONG AS THE CURRENT GRAPH HAS NOT UNDERGONE THE
;;;    INITIAL REDUCTION, OTHERWISE T.
;;;    CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG IS NIL AS LONG AS THE CURRENT GRAPH HAS NOT UNDERGONE THE
;;;    INITIAL SELECTION, OTHERWISE T.
;;;    CTL*CURRENT.SPLITPART.RESULT IS NIL AS LONG AS THE CURRENT GRAPH HAS STILL TO BE PROCESSED, OTHERWISE A
;;;    LEGAL REFUTATION RESULT. THESE ARE:
;;;          (SUCCESS  AXIOMS.UNSATISFIABLE)
;;;          (SUCCESS  THEOREMS.VALID)
;;;          (SUCCESS  <EMPTY CLAUSE>)
;;;          (FAILURE  GRAPH.COLLAPSED)
;;;          (FAILURE  GRAPH.SATISFIABLE  <MODEL>)
;;;          (FAILURE  LINKS.INOPERABLE)
;;;          (FAILURE  ABORTED.MAXSTEPS)
;;;          (failure  aborted.maxtime)
;;;          (FAILURE  ABORTED.MANUALLY)
;;;          (SPLIT    <FILE>  <SPLITPART INDICATORS>)
;;;
;;; INVARIANT.INFERENCE:
;;;    CTL*STEPCOUNTER IS THE NUMBER OF INFERENCE STEPS PERFORMED ON THE CURRENT GRAPH.
;;;    CTL*ABORT.FLAG IS NON-NIL IFF AN ABORTION OF THE CURRENT REFUTATION IS REQUIRED.
;;;

(DEFUN CTL-REFUTE.GRAPHS.ON.FILE (FILE CODEFILENAME)
  ;; EDITED: 25-FEB-84 12:31:44        NE
  ;; INPUT:  NAME OF A FILE CONTAINING AS MANY CALLS OF
  ;;        CTL-INITIAL.GRAPH AS THERE ARE SPLIT PARTS
  ;;        TO BE REFUTED FOLLOWED BY THE ATOM STOP
  ;;        AND SECOND A FILE NAME WHERE THE RAW DATAS
  ;;        FOR THE PROTOCOL ARE WRITTEN ON.
  ;; EFFECT: THE SPLIT PARTS ARE SUCCESSIVELY REFUTED AND
  ;;        SO ARE ANY ADDITIONAL DYNAMICALLY CREATED
  ;;        ONES.
  ;; VALUE:  'SUCCESS OR 'FAILURE INDICATING
  ;;        THE TOTAL RESULT OF ALL REFUTATIONS.
  ;; REMARK: THE SECOND FILE IS EXPECTED TO BE OPEN.
  (CTL=START CODEFILENAME)
  (PROGN (SETQ CTL*SPLITPARTS.REMAINING.FILES (LIST FILE))
	 (SETQ CTL*SPLITPARTS.INPUTSTREAM NIL)
	 (SETQ CTL*SPLITPARTS.RESULTS NIL)
	 ;; INVARIANT.SPLITPARTS HOLDS 
	 (CTL=REFUTE.SPLITPARTS NIL))
  (CTL=END))




(DEFUN CTL-REFUTE.GRAPHS.ON.FILE1 (FILE CODEFILENAME)
  ;; EDITED: 25-FEB-84 12:31:44        NE AP
  ;; INPUT:  NAME OF A FILE CONTAINING AS MANY CALLS OF
  ;;        CTL-INITIAL.GRAPH AS THERE ARE SPLIT PARTS
  ;;        TO BE REFUTED FOLLOWED BY THE ATOM STOP
  ;;        AND SECOND A FILE NAME WHERE THE RAW DATAS
  ;;        FOR THE PROTOCOL ARE WRITTEN ON.
  ;; EFFECT: THE SPLIT PARTS ARE SUCCESSIVELY REFUTED AND
  ;;        SO ARE ANY ADDITIONAL DYNAMICALLY CREATED
  ;;        ONES.
  ;; VALUE:  'SUCCESS OR 'FAILURE INDICATING
  ;;        THE TOTAL RESULT OF ALL REFUTATIONS.
  ;; REMARK: THE SECOND FILE IS EXPECTED TO BE OPEN.
  (RED-RESET 'DEDUCED :options)
  (UNI-RESET)
  ;(when (opt-get.option sort_literals)   Jetzt in UNI-RESET
  ;  (upp-epsilon.literals.insert (DT-PREDICATE.POSITIVE.OCCURRENCES (dt-predicate.element))))
  (CTL=PR_OPEN CODEFILENAME)
  (CTL=PR_OPTIONS)
  (PROGN (SETQ CTL*SPLITPARTS.REMAINING.FILES (LIST FILE))
	 (SETQ CTL*SPLITPARTS.INPUTSTREAM NIL)
	 (setq CTL*CURRENT.SPLITPART.IDENTIFIER '(1))
	 (setq CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG nil)
	 (SETQ CTL*SPLITPARTS.RESULTS NIL)
	 (setq ctl*splitpart.flag nil)
	 (setq CTL*CURRENT.SPLITPART.RESULT nil)
	 ;; INVARIANT.SPLITPARTS HOLDS 
	 (CTL=REFUTE.SPLITPARTS1 t))
  (CTL=END))

(DEFUN CTL-REFUTE.GRAPH.ON.FILE (FILE N CODEFILENAME)
  ;; EDITED: 25-FEB-84 12:31:44        NE
  ;; INPUT:  NAME OF A FILE CONTAINING CALLS OF
  ;;         CTL-INITIAL.GRAPH, A POSITIVE INTEGER
  ;;         -THERE ARE AT LEAST N EXPRESSIONS ON FILE-
  ;;         AND A SECOND FILE NAME WHERE THE RAW DATAS
  ;;         FOR THE PROTOCOL ARE WRITTEN ON.
  ;; EFFECT: THE N-TH GRAPH ON FILE IS REFUTED AND SO
  ;;         ARE ANY ADDITIONAL DYNAMICALLY CREATED ONES.
  ;; VALUE:  'SUCCESS OR 'FAILURE INDICATING
  ;;         THE TOTAL RESULT OF ALL REFUTATIONS.
  ;; REMARK: THE SECOND FILE IS EXPECTED TO BE OPEN.
  (CTL=START CODEFILENAME)
  (PROG (INPUTSTREAM)
	(COND
	  ((SETQ INPUTSTREAM (CTL=OPEN.AND.ADVANCE.FILE FILE (1- N)))
	   (EVAL (READ INPUTSTREAM *STANDARD-READTABLE*))
	   (CLOSEFILE INPUTSTREAM)
	   (SETQ CTL*SPLITPART.FLAG NIL)
	   ;; INVARIANT.CURRENT.SPLITPART HOLDS
	   (SETQ CTL*SPLITPARTS.REMAINING.FILES NIL)
	   (SETQ CTL*SPLITPARTS.INPUTSTREAM NIL)
	   (SETQ CTL*SPLITPARTS.RESULTS NIL)
	   ;; INVARIANT.SPLITPARTS HOLDS
	   (CTL=REFUTE.SPLITPARTS T))))
  (CTL=END))

(DEFUN CTL-REFUTE.GRAPH (SPLITPART.IDENTIFIER CODEFILENAME)
  ;; EDITED: 25-FEB-84 12:31:44        NE
  ;; INPUT:  A LIST OF INTEGERS IDENTIFYING THE CURRENT
  ;;        GRAPH.
  ;;        THE CURRENT GRAPH HAS BEEN FULLY REDUCED,
  ;;        NO INITIAL SELECTION AND NO INFERENCE
  ;;        STEP HAS PERFORMED ON IT,
  ;;        NO REFUTATION RESULT WAS OBTAINABLE SO FAR.
  ;; EFFECT: THE CURRENT GRAPH IS REFUTED AND
  ;;        SO ARE ANY ADDITIONAL DYNAMICALLY CREATED
  ;;        ONES.
  ;; VALUE:  'SUCCESS OR 'FAILURE INDICATING
  ;;        THE TOTAL RESULT OF ALL REFUTATIONS.
  ;; REMARK: THE FILE IS EXPECTED TO BE OPEN.
  (CTL=START CODEFILENAME)
  (PROGN (SETQ CTL*SPLITPART.FLAG               NIL
	       CTL*CURRENT.SPLITPART.IDENTIFIER SPLITPART.IDENTIFIER)
	 (SETQ CTL*CURRENT.SPLITPART.INITIAL.REDUCTION.FLAG T
	       CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG NIL)
	 (SETQ CTL*CURRENT.SPLITPART.RESULT NIL)
						; invariant.current.splitpart holds
	 (SETQ CTL*SPLITPARTS.REMAINING.FILES NIL
	       CTL*SPLITPARTS.INPUTSTREAM     NIL
	       CTL*SPLITPARTS.RESULTS         NIL)
	 (C INVARIANT.SPLITPARTS HOLDS *)
	 (CTL=REFUTE.SPLITPARTS T))
  (CTL=END))

(DEFUN CTL-INITIAL.GRAPH (SPLITPART.IDENTIFIER SPLITPART.FLAG INITIAL.RESULT REDUCED.FLAG SELECTION.FLAG EXPRESSION)
  (DECLARE (IGNORE EXPRESSION))
						; edited: 25-feb-84 13:25:19        ne
						; remark: this function can be used to create a
						;         clause.graph and to define it as initial.
						;         the calls may, but don't have to come from a file.
						; input:  EXPRESSION is a dummy argument. any actual
						;         argument is expected to create a clause
						;         graph by side effect during its evaluation.
						;         the value to which the actual argument
						;         evaluates is never used.
						;         SPLITPART.IDENTIFIER is a list of integers
						;         representing a unique identification for the graph.
						;         SPLITPART.FLAG is nil iff the given graph is
						;         supposed to be the only one to be processed.
						;         REDUCED.FLAG is non-nil iff the graph has
						;         already undergone the initial reduction.
						;         SELECTION.FLAG is non-nil iff the graph has
						;         already undergone the initial selection.
						;         initial result is nil if the graph still has
						;         to be processed, otherwise one of the
						;         legal refutation results.
						; effect: the given values are assigned to the module
						;         variables such that invariant.current.splitpart holds.
						; value:  undefined.
  (SETQ CTL*SPLITPART.FLAG SPLITPART.FLAG)
  (SETQ CTL*CURRENT.SPLITPART.IDENTIFIER SPLITPART.IDENTIFIER)
  (SETQ CTL*CURRENT.SPLITPART.INITIAL.REDUCTION.FLAG REDUCED.FLAG)
  (SETQ CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG SELECTION.FLAG)
  (SETQ CTL*CURRENT.SPLITPART.RESULT INITIAL.RESULT) NIL)

(DEFVAR CTL*SPLITPARTS.REMAINING.FILES NIL)

(DEFVAR CTL*SPLITPARTS.INPUTSTREAM NIL)

(DEFVAR CTL*SPLITPARTS.INPUTSTREAM.COUNTER NIL)

(DEFVAR CTL*SPLITPARTS.RESULTS NIL)

(DEFVAR CTL*SPLITPART.FLAG NIL)

(DEFVAR CTL*CURRENT.SPLITPART.IDENTIFIER NIL)

(DEFVAR CTL*CURRENT.SPLITPART.INITIAL.REDUCTION.FLAG t)

(DEFVAR CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG NIL)

(DEFVAR CTL*CURRENT.SPLITPART.RESULT NIL)

(DEFVAR CTL*STEPCOUNTER NIL)

(defvar ctl*prooftime 0)

(DEFVAR CTL*ABORT.FLAG NIL)

(DEFVAR CTL*ABORT.MACRO '(ABORT (SETQ CTL*ABORT.FLAG T) OK))

(DEFUN CTL=OPEN.AND.ADVANCE.FILE (FILE N)
						; input:  a file name and an integer.
						; effect: Opens file and overreads the first
						;         n expressions, which are not equal 'stop
						;         on the resulting inputstream and sends
						;         an error message in case of reading 'stop.
						; value:  the new inputstream or nil by error.
  (LET (INPUTSTREAM (COUNTER 0) EXPRESSION (*READTABLE* *STANDARD-READTABLE*))
    (SETQ INPUTSTREAM (mkrp-OPENIN (mkrp-make.pathname t nil "lisp" FILE)))
    (WHILE (AND (NEQ COUNTER N) (NEQ EXPRESSION 'STOP))
      (SETQ EXPRESSION (READ INPUTSTREAM NIL 'STOP))
      (incf COUNTER))
    (IF (EQL EXPRESSION 'STOP)
	(FORMAT T "ERROR IN CTL=OPEN.AND.ADVANCE.FILE.~%Only ~D EXPRESSIONS ON FILE ~A.~%" COUNTER INPUTSTREAM)
	INPUTSTREAM)))

(DEFUN CTL=START (CODEFILENAME)
  ;; INPUT:  A FILE NAME.
  ;; EFFECT: INITIAL SUBMODULE RESET.
  ;; VALUE:  UNDEFINED.
  ;; REMARK: THE FILE IS EXPECTED TO BE OPEN.
  (DT-RESET)
  (OP-RESET)
  (cons-reset)
  (RED-RESET 'DEDUCED)
  (UNI-RESET)
  (when (opt-get.option sort_literals)
    (upp-epsilon.literals.insert (DT-PREDICATE.POSITIVE.OCCURRENCES (dt-predicate.element))))
  (CTL=PR_OPEN CODEFILENAME)
  (CTL=PR_OPTIONS))

(DEFUN CTL=END NIL
  ;; INPUT:  NO ARGUMENT.
  ;;        CTL*SPLITPARTS.RESULTS IS A LIST OF ALL
  ;;        REFUTATION RESULTS OBTAINED.
  ;; EFFECT: CLOSES TRACE FILE AND ENDS
  ;;        ALL NECESSARY SUBMODULES.
  ;; VALUE:  'SUCCESS OR 'FAILURE INDICATING
  ;;        THE TOTAL RESULT OF ALL REFUTATIONS.
  (let (REFUTATIONS.RESULT)
    (RED-END)
    (SETQ REFUTATIONS.RESULT
	  (if (MEMBER-IF #'(LAMBDA (RESULT) (EQ (CAR RESULT) 'FAILURE)) CTL*SPLITPARTS.RESULTS)
	      'FAILURE
	      'SUCCESS))
    (CTL=PR_CLOSE REFUTATIONS.RESULT)
    REFUTATIONS.RESULT))

(DEFUN CTL=REFUTE.SPLITPARTS (GRAPH.FLAG)	; edited: 25-feb-84 15:00:58        ne
						; input:  a boolean value.
						;         invariant.splitparts holds.
						;         graph.flag is non-nil iff currently a graph
						;         exists that has to be refuted. in this case
						;         invariant.current.splitpart holds also.
						; effect: all graphs are refuted including any
						;         dynamically created new ones. the input
						;         specification remains invariant.
						; value:  undefined.
  (WHILE (OR GRAPH.FLAG CTL*SPLITPARTS.INPUTSTREAM CTL*SPLITPARTS.REMAINING.FILES)
    (COND (GRAPH.FLAG		; a current graph exists for which invariant.current.splitparts holds
	   (LET ((REFUTATION.RESULT (CTL=REFUTE)))
	     (SETQ CTL*SPLITPARTS.RESULTS (NCONC1 CTL*SPLITPARTS.RESULTS REFUTATION.RESULT))
	     (WHEN (EQL (CAR REFUTATION.RESULT) 'SPLIT)
	       (SETQ CTL*SPLITPARTS.REMAINING.FILES (NCONC1 CTL*SPLITPARTS.REMAINING.FILES (SECOND REFUTATION.RESULT))))
	     (SETQ GRAPH.FLAG NIL)))
	  ((NOT (NULL CTL*SPLITPARTS.INPUTSTREAM))	; check if further graph can be read
	   (LET (EXPRESSION (*READTABLE* *STANDARD-READTABLE*) (*package* (find-package "MKRP")))
	     (SETQ EXPRESSION (READ CTL*SPLITPARTS.INPUTSTREAM NIL 'STOP))
	     (COND ((NEQ EXPRESSION 'STOP)	; a graph can be constructed and processed. evaluation
						; involves an implicit call of ctl-initial.graph
						; which ensures invariant.current.splitpart.
		    (EVAL EXPRESSION)
		    (SETQ EXPRESSION NIL)
		    (incf CTL*SPLITPARTS.INPUTSTREAM.COUNTER)
		    (SETQ GRAPH.FLAG T)
						; invariant.splitparts holds
		    )
		   (T				; eof reached, discard the first file
		    (CLOSEFILE CTL*SPLITPARTS.INPUTSTREAM)
		    (SETQ CTL*SPLITPARTS.INPUTSTREAM NIL)
		    (SETQ CTL*SPLITPARTS.REMAINING.FILES (CDR CTL*SPLITPARTS.REMAINING.FILES))
						; invariant.splitparts holds
		    ))))
	  (T					; at least one file remains and can be opened
	   (SETQ CTL*SPLITPARTS.INPUTSTREAM.COUNTER 0)
	   (SETQ CTL*SPLITPARTS.INPUTSTREAM (CTL=OPEN.AND.ADVANCE.FILE (CAR CTL*SPLITPARTS.REMAINING.FILES) 0))
						; invariant.splitparts holds
	   ))))



(DEFUN CTL=REFUTE.SPLITPARTS1 (GRAPH.FLAG)	; edited: 25-feb-84 15:00:58        ne
						; input:  a boolean value.
						;         invariant.splitparts holds.
						;         graph.flag is non-nil iff currently a graph
						;         exists that has to be refuted. in this case
						;         invariant.current.splitpart holds also.
						; effect: all graphs are refuted including any
						;         dynamically created new ones. the input
						;         specification remains invariant.
						; value:  undefined.
  (when GRAPH.FLAG				; a current graph exists for which invariant.current.splitparts holds
    (let ((result (CTL=REFUTE)))
      (SETQ CTL*SPLITPARTS.RESULTS (NCONC1 CTL*SPLITPARTS.RESULTS RESULT))
      result)))


(DEFUN CTL=REFUTE NIL
  ;; INPUT:  NONE.
  ;; EFFECT: A REFUTATION OF THE CURRENT GRAPH IS
  ;;        EXECUTED SUCH THAT THE INPUT SPECIFICATION
  ;;        REMAINS INVARIANT. UPON TERMINATION
  ;;        CTL*CURRENT.SPLITPART.RESULT IS EQ TO THE
  ;;        FUNCTION VALUE.
  ;; VALUE:  A LEGAL REFUTATION RESULT. (CF. FILECOMS)
  (CTL=PR_REFUTATION.START CTL*CURRENT.SPLITPART.IDENTIFIER CTL*SPLITPART.FLAG
			   CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG CTL*STEPCOUNTER)
  (UNLESS CTL*CURRENT.SPLITPART.RESULT
    (CTL=PR_CURRENT.GRAPH T)
    (UNLESS CTL*CURRENT.SPLITPART.INITIAL.REDUCTION.FLAG
      (SETQ CTL*CURRENT.SPLITPART.RESULT (CTL=REDUCE.INITIAL))
      (SETQ CTL*CURRENT.SPLITPART.INITIAL.REDUCTION.FLAG T)))
  (UNLESS CTL*CURRENT.SPLITPART.RESULT
    (SETQ CTL*STEPCOUNTER 0 CTL*ABORT.FLAG NIL ctl*prooftime (GET-INTERNAL-RUN-TIME))
    (unless CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG
      (LET ((TIME (GET-INTERNAL-RUN-TIME)))
	(SEL-INITIALIZE CTL*CURRENT.SPLITPART.IDENTIFIER)
	(CTL=PR_STATISTICS "Marked" (mkrp-TIME TIME))
	(SETQ CTL*CURRENT.SPLITPART.INITIAL.SELECTION.FLAG T)))
						; invariant.inference holds
    (SETQ BREAKMACROS (ADJOIN CTL*ABORT.MACRO BREAKMACROS :TEST #'EQUAL))
    (SETQ CTL*CURRENT.SPLITPART.RESULT (CTL=INFERENCE.LOOP))
    (SETQ BREAKMACROS (REMOVE CTL*ABORT.MACRO BREAKMACROS)) (SEL-END))
  (CTL=PR_REFUTATION.END CTL*CURRENT.SPLITPART.IDENTIFIER CTL*SPLITPART.FLAG CTL*CURRENT.SPLITPART.RESULT)
  CTL*CURRENT.SPLITPART.RESULT)

(DEFUN CTL=REDUCE.INITIAL NIL
  ;; EDITED: 25-FEB-84 15:47:46        NE
  ;; INPUT:  NO ARGUMENTS.
  ;;        THE CURRENT GRAPH IS FIXED AND NO INFERENCE
  ;;        STEP HAS BEEN PERFORMED ON IT.
  ;; EFFECT: THE ENTIRE GRAPH IS REDUCED, CHANGES ARE
  ;;        PROTOCOLLED, THE GRAPH IS FIXED.
  ;; VALUE:  NIL OR ONE OF THE LEGAL REFUTATION RESULTS.
  (let* ((TIME   (GET-INTERNAL-RUN-TIME))
	 (RESULT (RED-REDUCE.GRAPH)))
    (CTL=PR_CHANGES 0 "INITIAL REDUCTION" NIL NIL)
    (CTL=PR_STATISTICS "REDUCED" (mkrp-TIME TIME))
    (CG-FIX NIL)
    (CTL=PR_CURRENT.GRAPH NIL)
    RESULT))

(DEFUN CTL=INFERENCE.LOOP NIL
  ;; EDITED: 25-FEB-84 15:58:34        NE
  ;; INPUT:  NO ARGUMENTS.
  ;;        INVARIANT.INFERENCE HOLDS.
  ;; EFFECT: INFERENCE STEPS ARE SUCCESSIVELY PERFORMED
  ;;        UNTIL A REFUTATION RESULT IS OBTAINED.
  ;; VALUE:  ONE OF THE LEGAL REFUTATION RESULTS.
  ;; REMARK: TERMINATION IS NOT GUARANTEED. THIS IS
  ;;        WHERE THE UNDECIDABILITY COMES IN.
  (let (RESULT)
    (WHILE (NULL RESULT)						      ; INVARIANT.INFERENCE HOLDS.
      (COND ((SETQ RESULT (SEL-REFUTATION.RESULT)) NIL)
	    ((AND (OPT-GET.OPTION GEN_MAXIMUM.STEPS)
		  (NOT (< CTL*STEPCOUNTER (OPT-GET.OPTION GEN_MAXIMUM.STEPS))))
	     (SETQ RESULT '(FAILURE ABORTED.MAXSTEPS)))
	    ((AND (OPT-GET.OPTION GEN_MAXIMUM.time)
		  (NOT (< (/ (mkrp-time CTL*prooftime) 1000) (OPT-GET.OPTION GEN_MAXIMUM.time))))
	     (SETQ RESULT '(FAILURE ABORTED.MAXtime)))
	    (CTL*ABORT.FLAG (SETQ RESULT '(FAILURE ABORTED.MANUALLY)))
	    (T (incf CTL*STEPCOUNTER)
	       (SETQ RESULT (CTL=INFERENCE.STEP CTL*STEPCOUNTER))
	       (CTL=PR_GRAPH.DUMP CTL*STEPCOUNTER)
	       (COND
		 ((AND (NULL RESULT) (OPT-GET.OPTION GEN_GRAPH.SAVING)
		       (ZEROP (REM CTL*STEPCOUNTER (OPT-GET.OPTION GEN_GRAPH.SAVING))))
		  (CTL=SAVE))))))
    RESULT))

(DEFUN CTL=INFERENCE.STEP (STEPNUMBER)
  ;; EDITED: 25-FEB-84 16:16:48        NE
  ;; INPUT:  AN INTEGER.
  ;;        THE CURRENT GRAPH IS REDUCED AND FIXED.
  ;;        STEPNUMBER-1 INFERENCE STEPS HAVE BEEN DONE.
  ;;        NO REFUTATION RESULT HAS BEEN OBTAINED YET.
  ;;        THE SEL-MODULE WAS UPDATED WITH THE CURRENT
  ;;        STATE.
  ;; EFFECT: PERFORMS ONE DEDUCTION STEP AS DETERMINED
  ;;        BY SEL, FOLLOWED BY APPROPRIATE REDUCTIONS.
  ;;        PROTOCOLS AND UPDATES ARE PERFORMED AND THE
  ;;        GRAPH IS FIXED.
  ;; VALUE:  NIL OR A LEGAL REFUTATION RESULT.
  (let ((LINK.OR.PATH (SEL-DEDUCTION.CODE.LINK))
	(UNIFIER (SEL-DEDUCTION.CODE.UNIFIER))
	RESULT TIME CLAUSES.DEDUCED
	CLAUSES.UNITFACTORS)
    (PROGN					; DEDUCTION
      (SETQ TIME (GET-INTERNAL-RUN-TIME))
      (PROGN					; OPERATION WITH PARTIAL REDUCTION
	(SETQ RESULT (CTL=DEDUCE STEPNUMBER LINK.OR.PATH UNIFIER))
	(WHEN (AND (SEL-REDUCTION.FLAG) (NULL RESULT))
	  (SETQ RESULT (CTL=REDUCE.PARTIAL STEPNUMBER (CG-CLAUSES INSERTED))))    
	(let ((domain (ds-finite.domain.domain)))
	  (CG-DISJOINTIFY)
	  (when domain
	    (mapc #'(lambda (clause)
		      (let ((variables (remove-if-not #'(lambda (var) (eql (dt-variable.sort var) (dt-constant.sort (first domain))))
						      (ds-clause.variables clause))))
			(CARTESIAN.LOOP (make-list (length variables) :initial-element (copy-list domain))
					#'(lambda (lists)
					    (let ((inst.uni (zip variables (mapcar #'first lists) t)))
					      (op-create.instance inst.uni clause))))
			(when variables (red-remove.clause clause t "Instantiation with all instances." nil))))
		  (copy-list (cg-clauses inserted)))
	    (CG-DISJOINTIFY)
	    (WHEN (AND (SEL-REDUCTION.FLAG) (NULL RESULT))
	      (SETQ RESULT (CTL=REDUCE.PARTIAL STEPNUMBER (CG-CLAUSES INSERTED))))))
	(CG-DISJOINTIFY)
	(SETQ CLAUSES.DEDUCED (COPY-TREE (CG-CLAUSES INSERTED))))
      (PROGN					; UNITFACTORING WITH PARTIAL REDUCTION
	(UNLESS RESULT (SETQ RESULT (CTL=DEDUCE.UNITFACTORS STEPNUMBER CLAUSES.DEDUCED)))
	(SETQ CLAUSES.UNITFACTORS (SET-DIFFERENCE (CG-CLAUSES INSERTED) CLAUSES.DEDUCED))
	(when (AND (SEL-REDUCTION.FLAG) (NULL RESULT))
	  (SETQ RESULT (CTL=REDUCE.PARTIAL STEPNUMBER CLAUSES.UNITFACTORS)))
	(CG-DISJOINTIFY))
      (CTL=PR_STATISTICS (format nil "Step ~A" CTL*STEPCOUNTER) (mkrp-TIME TIME))
      (SETQ TIME (GET-INTERNAL-RUN-TIME))
      (COND ((NULL RESULT) (SEL-update.DEDUCE STEPNUMBER)))
      (CG-FIX NIL)
      ;; END OF DEDUCTION PHASE. ALL CLAUSES THAT ARE NOW IN THE CURRENT GRAPH BUT WHERE NOT WHEN THIS
      ;;  FUNCTION WAS INVOKED HAVE BEEN GIVEN TO THE PARTIAL REDUCTION EXACTLY ONCE.
      )
    (PROGN ;; REDUCTION
      (unless RESULT
	(SETQ RESULT
	      (CTL=REDUCE STEPNUMBER
			  (SEL-REDUCTION.CODE.LITERALS.TO.BE.REMOVED)
			  (append (SEL-REDUCTION.CODE.CLAUSES.TO.BE.REMOVED T))
			  (SEL-REDUCTION.CODE.UNIFIERS.TO.BE.REMOVED ALL)
			  (SEL-REDUCTION.CODE.CLAUSES.TO.BE.REDUCED))))
      (CG-DISJOINTIFY)
      (COND ((NULL RESULT) (SEL-UPDATE.REDUCE STEPNUMBER)))
      (CTL=PR_STATISTICS "" (mkrp-TIME TIME))
      (CG-FIX NIL))
    RESULT))

(DEFUN CTL=DEDUCE (STEPNUMBER LINK.OR.PATH UNIFIER)
  ;; EDITED: 25-FEB-84 16:43:40        NE
  ;; INPUT:  THE CURRENT STEP NUMBER AND THE OBJECTS TO
  ;;        BE OPERATED UPON.
  ;; EFFECT: PERFORMS THE RESPECTIVE OPERATION AND
  ;;        PROTOCOLS THE CHANGES.
  ;; VALUE:  NIL OR A LEGAL REFUTATION RESULT.
  (if LINK.OR.PATH
      (let (EMPTYCLAUSE)
	(COND ((DS-LINK.IS LINK.OR.PATH)
	       ;(if (ds-link.result link.or.path)
		   ;(op-rp.chain link (ds-link.result link.or.path) (ds-link.protocol.info link.or.path))
		   (CASE (DS-LINK.COLOUR LINK.OR.PATH)
		     ((R riw) (OP-RESOLVE LINK.OR.PATH UNIFIER))
		     (SI (OP-FACTORIZE LINK.OR.PATH UNIFIER))
		     ((P piw) (OP-PARAMODULATE LINK.OR.PATH UNIFIER))
		     (OTHERWISE (ERROR "ILLEGAL LINK COLOUR IN CTL=DEDUCE. LINK = : ~A" LINK.OR.PATH))));)
	      ((CONSP LINK.OR.PATH)
	       (CASE (CAR LINK.OR.PATH)
		 (R.CHAIN (OP-R.CHAIN (CDR LINK.OR.PATH)))
		 (OTHERWISE (ERROR "ILLEGAL KEYWORD IN CTL=DEDUCE: ~A" (CAR LINK.OR.PATH)))))
	      (T (ERROR "CTL=DEDUCE TO OPERATE ON OBJECT OTHER THAN LINK OR PATH: : ~A" LINK.OR.PATH)))
	(CTL=PR_CHANGES STEPNUMBER NIL LINK.OR.PATH UNIFIER)
	(SETQ EMPTYCLAUSE
	      (CAR (MEMBER-IF #'(LAMBDA (CLAUSE) (ZEROP (DS-CLAUSE.NOLIT CLAUSE))) (CG-CLAUSES INSERTED))))
	(if EMPTYCLAUSE
	    (LIST 'SUCCESS EMPTYCLAUSE)
	    NIL))
      nil))

(DEFUN CTL=DEDUCE.UNITFACTORS (STEPNUMBER CLAUSES.DEDUCED)
  ;; EDITED: 25-FEB-84 17:25:03        NE
  ;; INPUT:  THE CURRENT STEP NUMBER AND A LIST OF
  ;;        CLAUSES (NOT EQ TO ANY (CG-CLAUSES ...))
  ;; EFFECT: DEDUCES ALL POSSIBLE UNIT FACTORS OF THE
  ;;        GIVEN CLAUSES AND PROTOCOLS THE CHANGES.
  ;; VALUE:  NIL
  ;; REMARK: PRESENTLY NO REFUTATION RESULT POSSIBLE.
  (MAPC #'(LAMBDA (CLAUSE) (OP-CREATE.UNIT.FACTORS CLAUSE)) CLAUSES.DEDUCED)
  (when (SET-DIFFERENCE (CG-CLAUSES INSERTED) CLAUSES.DEDUCED)
    (CTL=PR_CHANGES STEPNUMBER "UNIT FACTORING" NIL NIL))
  NIL)

(DEFUN CTL=REDUCE.PARTIAL (STEPNUMBER CLAUSES)
  ;; EDITED: 25-FEB-84 13:26:16        NE
  ;; INPUT:  THE CURRENT STEP NUMBER AND A SUBSET OF
  ;;        (CG-CLAUSES INSERTED).
  ;; EFFECT: PERFORMS A REDUCTION AFFECTING AT MOST THE
  ;;        GIVEN CLAUSES AND THEIR INCIDENT LINKS
  ;;        AND PROTOCOLS THE CHANGES.
  ;; VALUE:  NIL OR A LEGAL REFUTATION RESULT.
  (when CLAUSES
    (prog1 (RED-REDUCE.DEDUCED.GRAPH.PARTIAL CLAUSES)
	   (CTL=PR_CHANGES STEPNUMBER "PARTIAL REDUCTION" NIL NIL))))

(DEFUN CTL=REDUCE (STEPNUMBER LITERALS.TO.BE.REMOVED CLAUSES.TO.BE.REMOVED UNIFIERS.TO.BE.REMOVED CLAUSES.TO.BE.REDUCED)
  ;; EDITED: 25-FEB-84 13:31:52        NE
  ;; INPUT:  THE CURRENT STEP NUMBER AND FOUR LISTS.
  ;;        FOR THE FIRST THREE OF THEM SEE SEL AND RED.
  ;;        THE LAST ONE IS A LIST OF CLAUSES OF THE
  ;;        CURRENT GRAPH THAT HAVE BEEN PARTIALLY BUT
  ;;        NOT YET FULLY REDUCED.
  ;; EFFECT: PERFORMS THE FULL REDUCTION AND PROTOCOLS
  ;;        THE CHANGES.
  ;; VALUE:  NIL OR ONE OF THE LEGAL REFUTATION RESULTS.
  (let ((LINKS.TO.BE.REDUCED NIL))
    (prog1 (RED-REDUCE.DEDUCED.GRAPH LITERALS.TO.BE.REMOVED CLAUSES.TO.BE.REMOVED UNIFIERS.TO.BE.REMOVED
				     CLAUSES.TO.BE.REDUCED LINKS.TO.BE.REDUCED)
	   (CTL=PR_CHANGES STEPNUMBER "FULL REDUCTION" NIL NIL))))

(DEFUN CTL=SAVE NIL				; edited: 25-feb-84 13:52:39        ne
						; input:  no arguments.
						;        invarinat.operation,
						;        invariant.current.splitpart,
						;        invriant.splitparts hold.
						;        the current graph is reduced and fixed.
						; effect: opens a file for graph-saving.
						;        writes the s-expression
						;        (ctl-initial.graph  splitpart.identifier splitpart.flag
						;                            initial.result reduced.flag selection.flag
						;                            expression)
						;        on savefile where expression is an s-expression which when evaluetd
						;        restores the same state as at invocation time, except of the state
						;        of open files. This expression is written on a file.
						; value:  name of the file.
  (LET ((OUTPUTSTREAM (mkrp-OPENOUT (mkrp-make.pathname t
							(CTL=SAVE.FILENAME
							  (pathname-name
							    (if (symbolp (OPT-GET.OPTION GEN_SAVE.FILE))
								(pathname (symbol-name (OPT-GET.OPTION GEN_SAVE.FILE)))
								(pathname (OPT-GET.OPTION GEN_SAVE.FILE))))
							  CTL*CURRENT.SPLITPART.IDENTIFIER CTL*STEPCOUNTER)
							"lisp"
							(if (symbolp (OPT-GET.OPTION GEN_SAVE.FILE))
							    (symbol-name (OPT-GET.OPTION GEN_SAVE.FILE))
							    (OPT-GET.OPTION GEN_SAVE.FILE)))
				    nil))
	MESSAGE FILENAME)
    (SETQ FILENAME (PATHNAME OUTPUTSTREAM))
    (red-save.reset)
    (PROGN (FORMAT OUTPUTSTREAM "(CTL-INITIAL.GRAPH '~S nil nil T T (PROGN " CTL*CURRENT.SPLITPART.IDENTIFIER)
	   (OP-SAVE OUTPUTSTREAM)
	   (RED-SAVE OUTPUTSTREAM)
	   (SEL-SAVE OUTPUTSTREAM)
	   (PRINC "))" OUTPUTSTREAM)
	   (CLOSEFILE OUTPUTSTREAM))
    (SETQ MESSAGE (format nil "$$$$$ Current graph saved on file ~A" FILENAME))
    (CTL=PR_MESSAGE MESSAGE)
    FILENAME))

(DEFUN CTL=SAVE.FILENAME (PREFIX SPLITPART.IDENTIFIER STEPCOUNTER)
  ;; EDITED: 25-FEB-84 14:09:22        NE
  ;; INPUT:  A SYMBOL OR STRING, A LIST OF INTEGERS,
  ;;        AND AN INTEGER.
  ;; VALUE:  A SYMBOL CONTAINING ALL THE ARGUMENTS SUCH
  ;;        THAT IT MAY BE USED AS A FILE NAME.
  (format nil "~A-split~A-step-~A"
	  prefix
	  (ZIP (DUPL "-" (LIST-LENGTH SPLITPART.IDENTIFIER))
	       (MAPCAR #'PRINC-TO-STRING SPLITPART.IDENTIFIER) NIL)
	  STEPCOUNTER))

(DEFVAR CTL*TRACE.OPEN.FLAG NIL)

(DEFVAR CTL*TRACE.OUTPUTSTREAM NIL)

(DEFVAR CTL*TRACE.OUTPUTSTREAM.LINELENGTH 130)

(DEFUN CTL=PR_OPEN (CODEFILENAME)
  ;; EDITED: 25-FEB-84 16:13:09        NE
  ;; EFFECT: OPENS PROTOCOL AND TRACE FILE, PRINTS A
  ;;        HEADER ON TRACE FILE.
  ;; VALUE:  UNDEFINED.
  (PR-SPLITPARTS.START CODEFILENAME
		       #-symbolics (software-type)
		       #+symbolics (sct:system-version-info T))
  (let ((FILE (OPT-GET.OPTION TR_TRACE.FILE))
	STREAM
	(HEADLINES
	  (LIST "***************************************************************************"
		" "
		(format nil "   Markgraf Karl system, Uni Kaiserslautern, version: " )
		(format nil "   Trace file                                   date: ~A" (DATE))
		" "
		"***************************************************************************")))
    (COND ((NULL FILE) (SETQ CTL*TRACE.OPEN.FLAG T) (SETQ CTL*TRACE.OUTPUTSTREAM NIL))
	  ((EQL FILE T)
	   (SETQ CTL*TRACE.OPEN.FLAG T)
	   (SETQ CTL*TRACE.OUTPUTSTREAM T)
	   (LINELENGTH CTL*TRACE.OUTPUTSTREAM.LINELENGTH T))
	  ((mkrp-outstreamp FILE) (SETQ CTL*TRACE.OPEN.FLAG T) (SETQ CTL*TRACE.OUTPUTSTREAM FILE))
	  (T (SETQ CTL*TRACE.OPEN.FLAG NIL)
	     (SETQ CTL*TRACE.OUTPUTSTREAM (SETQ STREAM (mkrp-OPENOUT (mkrp-make.pathname nil "trace" "text" file) nil)))
	     (LINELENGTH CTL*TRACE.OUTPUTSTREAM.LINELENGTH STREAM) (TERPRI STREAM)
	     (MAPC #'(LAMBDA (LINE) (PRINC LINE STREAM) (TERPRI STREAM)) HEADLINES)
	     (TERPRI STREAM) (TERPRI STREAM)))))

(DEFUN CTL=PR_CLOSE (PROVED.FLAG)
  ;; EDITED: 25-FEB-84 16:34:45        NE
  ;; INPUT:  A BOOLEAN VALUE.
  ;; EFFECT: PRINTS A FINAL MESSAGE ON TRACE FILE AND
  ;;        CLOSES TRACE AND PROTOCOL FILE.
  ;; VALUE:  EXTERNAL PROTOCOL CODE FILE NAME.
  (PROG
    ((STREAM CTL*TRACE.OUTPUTSTREAM)
     (MESSAGE (format nil "PROOF ~A.   End: ~A" (COND (PROVED.FLAG "SUCCEEDED") (T "FAILED")) (DATE))))
    (COND ((NULL STREAM) NIL)
	  ((EQL STREAM T) NIL)
	  (CTL*TRACE.OPEN.FLAG NIL)
	  (T (format stream "~%~A~2%" MESSAGE)
	     (CLOSEFILE STREAM)))
    (SETQ CTL*TRACE.OUTPUTSTREAM NIL)
    (PR-SPLITPARTS.END)))

(DEFUN CTL=PR_OPTIONS NIL
  ;; EDITED: 25-FEB-84 16:55:44        NE
  ;; EFFECT: PRINTS THE OPTION SETTING ON PROTOCOL AND
  ;;        TRRACE FILE.
  ;; VALUE: UNDEFINED.
  (PR-OPTIONS)
  (PROG ((STREAM CTL*TRACE.OUTPUTSTREAM))
	(COND
	  ((AND STREAM (NEQ STREAM T)) (PRINC "ADJUSTMENT OF THE OPTIONS:" STREAM) (TERPRI STREAM) (TERPRI STREAM)
	   (MAPC
	     (FUNCTION
	       (LAMBDA (OPTION.VALUE) (FORMAT STREAM "~T" 6) (PRINC (CAR OPTION.VALUE) STREAM) (FORMAT STREAM "~T" 50)
		       (PRINC (CDR OPTION.VALUE) STREAM) (TERPRI STREAM)))
	     (OPT-GET.LIST.OPTIONS))
	   (TERPRI STREAM) (TERPRI STREAM)))))

(DEFUN CTL=PR_REFUTATION.START (SPLITPART.IDENTIFIER SPLITPART.FLAG RESUMPTION.FLAG STEPNUMBER)
  ;; EDITED: 25-FEB-84 17:04:43        NE
  ;; INPUT:  A LIST OF INTEGERS, TWO BOOLEAN VALUES AND
  ;;        AN INTEGER DENOTING THE NUMBER OF INFERENCE
  ;;        STEPS PERFORMED ON THE CURRENT GRAPH SO FAR.
  ;; EFFECT: REPORTS THE BEGINNING REFUTATION TO THE
  ;;        PR-MODULE AND PRINTS A CORRESPONDING
  ;;        MESSAGE ON TRACE FILE.
  ;; VALUE:  UNDEFINED.
  (PR-REFUTATION.START SPLITPART.IDENTIFIER RESUMPTION.FLAG STEPNUMBER)
  (when SPLITpart.FLAG
    (ctl=PR_MESSAGE.FULL.LINE #\/ (format nil "Refutation of splitpart ~a ~A"
						 SPLITPART.IDENTIFIER
						 (COND ((NOT RESUMPTION.FLAG) "initiated.")
						       (T (format nil "resumed after step ~a." STEPNUMBER)))))))

(DEFUN CTL=PR_REFUTATION.END (SPLITPART.IDENTIFIER SPLITPART.FLAG RESULT)
						; edited: 25-feb-84 17:15:36  ne
						; input:  a list of integers, a boolean value, and
						;         a legal refutation result, whatever that is ?!
						; effect: reports the refutation end to the pr-module
						;         and prints a corresponding message on the trace file.
						; value:  undefined.
  (PR-REFUTATION.END SPLITPART.IDENTIFIER RESULT)
  (CTL=PR_MESSAGE (format nil "===== Result: ~A" RESULT))
  (WHEN SPLITpart.FLAG
    (CTL=PR_MESSAGE.FULL.LINE #\/ (FORMAT NIL "Refutation of split part ~a ~a."
					  (first SPLITPART.IDENTIFIER)
					  (CASE (CAR RESULT)
					    (SUCCESS   "succeeded")
					    (FAILURE   "failed")
					    (SPLIT     "split and deferred")
					    (OTHERWISE "terminated"))))))

(DEFUN CTL=PR_CURRENT.GRAPH (FIRST.CALL.FLAG)
  ;; EDITED: 25-FEB-84 18:52:42        NE
  ;; INPUT:  A BOOLEAN VALUE WHICH FOR THE CURRENT GRAPH
  ;;        MAY BE NON-NIL ONLY DURING THE FIRST CALL
  ;;        OF THIS FUNCTION.
  ;; EFFECT: IF FIRST.CALL.FLAG IS NON-NIL, THE CURRENT
  ;;        GRAPH IS REPORTED TO THE PR-MODULE AS
  ;;        INITIAL. FURTHER THE GRAPH IS PRINTED
  ;;        TO THE TRACE FILE.
  ;; VALUE:  UNDEFINED.
  (DECLARE (SPECIAL STEP.MESSAGE STEP.CLAUSES STEP.LINKS))
  (WHEN FIRST.CALL.FLAG
    (PR-INITIAL.GRAPH)
    (LET ((STREAM CTL*TRACE.OUTPUTSTREAM))
      (WHEN STREAM
	(PRINC "TABLE OF DATATERM-OBJECTS: " STREAM)
	(DT-PRINT.SYMBOLS '(CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL) STREAM 7 (LINELENGTH NIL STREAM)))))
  (WHEN (AND CTL*TRACE.OUTPUTSTREAM FIRST.CALL.FLAG (DS-RULES))
    (LET (RULE.MODE RULE.MESSAGE)
      (SETQ RULE.MODE    (OPT-GET.OPTION TR_STEP.MODE))
      (SETQ RULE.MESSAGE (LIST 'MESSAGE "TWO-LITERAL RULES OF THE INITIAL GRAPH:"))
      (CG-DUMP CTL*TRACE.OUTPUTSTREAM `(,RULE.MESSAGE (CLAUSES ,(DS-RULES) ,RULE.MODE)))))
  (WHEN (AND CTL*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_STEP.MODE))
    (LET (STEP.MODE)
      (SETQ STEP.MODE    (OPT-GET.OPTION TR_STEP.MODE))
      (SETQ STEP.MESSAGE (LIST 'MESSAGE (CONCATENATE 'STRING (IF FIRST.CALL.FLAG "INITIAL" "CURRENT")
						     " GRAPH BEFORE ANY INFERENCE STEP:")))
      (SETQ STEP.CLAUSES (LIST 'CLAUSES 'ALL STEP.MODE))
      (SETQ STEP.LINKS   (COND ((EQL STEP.MODE 'I) '((LINKS ALL NIL I)))
			       (T NIL)))
      (CG-DUMP CTL*TRACE.OUTPUTSTREAM `(,STEP.MESSAGE ,STEP.CLAUSES ,@STEP.LINKS))))
  (WHEN (AND CTL*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_dump)) (CTL=PR_GRAPH.DUMP 0))
  (WHEN (OPT-GET.OPTION TR_TERMINAL) (CG-DUMP.SHORT *standard-output* NIL NIL NIL L)))


(DEFUN CTL=PR_CHANGES (STEPNUMBER EXPLANATION LINK UNIFIER)
  (DECLARE
    (SPECIAL DUMP.MESSAGE DUMP.CLAUSES.INSERTED DUMP.CLAUSES.CHANGED DUMP.CLAUSES.REMOVED DUMP.LINKS.INSERTED
	     DUMP.LINKS.CHANGED DUMP.LINKS.REMOVED DUMP.CLAUSES DUMP.LINKS))
  ;; EDITED: 26-FEB-84 16:28:52        NE
  ;; INPUT:  THE CURRENT STEP NUMBER, A STRING AND TWO
  ;;        OBJECTS OPERATED UPON, OR NIL.
  ;; EFFECT: PRINTS INFORMATION ABOUT CHANGED OBJECTS OF
  ;;        THE CURRENT GRAPH ON TRACE FILE.
  ;; VALUE:  UNDEFINED.
  (COND
    ((AND CTL*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_STEP.MODE))
     (PROG ((MODE (OPT-GET.OPTION TR_STEP.MODE)))
	   (SETQ DUMP.MESSAGE
		 (COND
		   ((DS-LINK.IS LINK)
		    (LIST 'MESSAGE
			  (format nil "Step ~A - Operation on unifier ~A of ~A-Link ~A~A~A"
				  STEPNUMBER (DS-PNAME UNIFIER) (DS-LINK.COLOUR LINK) LINK
				  (let ((RULE (DS-LINK.RULE LINK)))
				    (if RULE
					(format nil " using rule ~A"
						(COND ((ATOM RULE) RULE)
						      (T (format nil "~A c~Ae" (DS-PNAME (CAR RULE)) (CDR RULE)))))
					""))
				  (let ((COLOUR (DS-LINK.COLOUR LINK)))
				    (format nil ", Parent(s) ~A~A ~A~A "
					    (COND ((MEMBER COLOUR (DS-LINK.COLOURS.WITH 'POSPAR))
						   (DS-CLAUSE.PNAME (DS-LINK.POSPAR LINK)))
						  (T ""))
					    (COND ((MEMBER COLOUR (DS-LINK.COLOURS.WITH 'POSLITNO))
						   (format nil "[~A]" (DS-LINK.POSLITNO LINK)))
						  (T ""))
					    (COND ((MEMBER COLOUR (DS-LINK.COLOURS.WITH 'NEGPAR))
						   (DS-CLAUSE.PNAME (DS-LINK.NEGPAR LINK)))
						  (T ""))
					    (COND ((MEMBER COLOUR (DS-LINK.COLOURS.WITH 'NEGLITNO))
						   (format nil "[~A]" (DS-LINK.NEGLITNO LINK)))
						  (T "")))))))
		   (T
		    (LIST 'MESSAGE
			  (CONCATENATE 'STRING "STEP " (PRINC-TO-STRING STEPNUMBER) " - " (PRINC-TO-STRING EXPLANATION))))))
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
	   (CG-DUMP CTL*TRACE.OUTPUTSTREAM `((MESSAGE "") ,DUMP.MESSAGE ,DUMP.CLAUSES.INSERTED ,DUMP.CLAUSES.CHANGED
		    ,DUMP.CLAUSES.REMOVED ,DUMP.LINKS.INSERTED ,DUMP.LINKS.CHANGED ,DUMP.LINKS.REMOVED)))))
  (COND ((OPT-GET.OPTION TR_TERMINAL) (CG-DUMP.SHORT *standard-output* NIL L L NIL))))

(DEFUN CTL=PR_GRAPH.DUMP (STEPNUMBER)
  (DECLARE
    (SPECIAL DUMP.CLAUSES.INSERTED DUMP.CLAUSES.CHANGED DUMP.CLAUSES.REMOVED DUMP.LINKS.INSERTED
	     DUMP.LINKS.CHANGED DUMP.LINKS.REMOVED))
  ;; EDITED: 25-FEB-84 19:37:49        NE
  ;; INPUT:  AN INTEGER DENOTING THE NUMBER OF INFERENCE
  ;;        STEPS THAT HAVE BEEN PERFORMED ON THE
  ;;        CURRENT GRAPH WHICH IS REDUCED AND FIXED.
  ;; EFFECT: THE CURRENT GRAPH IS DUMPED ON THE TRACE
  ;;        FILE ACCORDING TO THE TRACE OPTIONS.
  ;; VALUE: UNDEFINED.
  (COND
    ((AND CTL*TRACE.OUTPUTSTREAM (OPT-GET.OPTION TR_DUMP) (ZEROP (REM STEPNUMBER (OPT-GET.OPTION TR_DUMP))))
     (PROG (DUMP.MESSAGE DUMP.CLAUSES DUMP.LINKS)
	   (DECLARE (SPECIAL DUMP.MESSAGE DUMP.CLAUSES DUMP.LINKS))
	   (SETQ DUMP.MESSAGE
		 (LIST 'MESSAGE
		       (CONCATENATE 'STRING "CURRENT GRAPH AFTER " (PRINC-TO-STRING STEPNUMBER) " INFERENCE STEPS:")))
	   (SETQ DUMP.CLAUSES
		 (COND ((OPT-GET.OPTION TR_CLAUSE.MODE) (LIST 'CLAUSES 'ALL (OPT-GET.OPTION TR_CLAUSE.MODE))) (T NIL)))
	   (SETQ DUMP.LINKS
		 (COND ((OPT-GET.OPTION TR_LINK.MODE) (LIST 'LINKS 'ALL NIL (OPT-GET.OPTION TR_LINK.MODE))) (T NIL)))
	   (CG-DUMP CTL*TRACE.OUTPUTSTREAM `((SEPARATOR) ,DUMP.MESSAGE ,DUMP.CLAUSES ,DUMP.LINKS)))))
  (COND ((AND (OPT-GET.OPTION TR_TERMINAL) (NEQ STEPNUMBER 0)) (CG-DUMP.SHORT *STANDARD-OUTPUT* NIL NIL NIL N))))

(DEFUN CTL=PR_STATISTICS (PREFIX TIME)
  ;; EDITED: 25-FEB-84 14:34:11        NE
  ;; INPUT:  A STRING AND AN INTEGER.
  ;; EFFECT: REPORTS CHANGES OF THE CURRENT GRAPH SINCE
  ;;        THE LAST FIX TO THE PROTOCOL MODULE.
  ;;        PRINTS A LINE OF STATISTICAL INFORMATION
  ;;        ON THE TERMINAL.
  ;; VALUE:  UNDEFINED.
  (LET (LINKS +LINKS -LINKS CLAUSES +CLAUSES -CLAUSES STORE MEMORY PREF TIM)
    (PR-STATISTICS TIME)
    ;; POSITIVE: POSITION BEFORE PRINTING, NEGATIVE: POSITION AFTER PRINTING
    (SETQ LINKS (APPLY #'+ (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) ALL)))
    (SETQ +LINKS (APPLY #'+ (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) INSERTED)))
    (SETQ -LINKS (APPLY #'+ (CG-#LINKS (DS-LINK.COLOURS.FOR 'ALL) REMOVED)))
    (SETQ CLAUSES (CG-#CLAUSES ALL))
    (SETQ +CLAUSES (CG-#CLAUSES INSERTED))
    (SETQ -CLAUSES (CG-#CLAUSES REMOVED))
    (SETQ STORE (REMAINING-MEMORY))
    (mkrp-gc.start (opt-get.option gen_lisp.garbage.collection))
    (SETQ MEMORY (TRUNCATE (MEM-MEMORY) 1024))
    (SETQ PREF PREFIX)
    (SETQ TIM TIME)
    (FORMAT T "~va~v@a Msec Links: ~v@a~v@a~v@a Clauses: ~v@a~v@a~v@a Store: ~v@a~v@a~%"
	    15 PREF 7 TIM 5 LINKS
	    5 (FORMAT NIL "+~a" +LINKS) 6 (FORMAT NIL "-~a" -LINKS) 
	    4 CLAUSES 4 (FORMAT NIL "+~a" +CLAUSES) 4 (FORMAT NIL "-~a" -CLAUSES)
	    6 STORE 5 MEMORY)))


(DEFUN CTL=PR_MESSAGE (&REST STRINGS)		; edited: 25-feb-84 15:50:47  by NE
						; input:  arbitrarily many strings.
						; effect: prints the strings on trace file and
						;         terminal, each into a separate line.
						; value:  undefined
  (when (AND CTL*TRACE.OUTPUTSTREAM (NEQ CTL*TRACE.OUTPUTSTREAM T))
    (format CTL*TRACE.OUTPUTSTREAM "~{~A~%~}" strings))
  (format *standard-output* "~{~A~%~}" strings))


(DEFUN CTL=PR_MESSAGE.FULL.LINE (CHAR &REST STRINGS)
  ;; EDITED: 25-FEB-84 15:50:47        NE
  ;; INPUT:  A CHARACTER AND ARBITRARILY MANY STRINGS.
  ;; EFFECT: PRINTS THE STRINGS ON TRACE FILE AND
  ;;        TERMINAL, EACH INTO A SEPARATE LINE,
  ;;        WHICH IS PADDED TO THE RIGHT WITH CHAR.
  ;; VALUE:  UNDEFINED
  (mapc #'(lambda (string)
	    (let ((STREAM ctl*TRACE.OUTPUTSTREAM)
		  (output (replace (make-string 117 :initial-element char) (format nil " ~A " STRING) :start1 5)))
	      (when (AND STREAM (NEQ STREAM T))
		(format stream "~%~A~%~%" output))
	      (format *standard-output* "~%~A~%~%" output)))
	strings))

