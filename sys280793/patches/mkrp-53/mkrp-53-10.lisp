;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.10
;;; Reason: Function MKRP::OS=CONSTRUCT.PROC:  c prover
;;; Function MKRP::OS=OPEN.GRAPH.FILE:  c without ;;;
;;; Written by kasper, 5/06/92 17:38:46
;;; while running on Magic File Server from FEP0:>GENERA-8-0-INC-KKL-BEWEISER.LOAD.1
;;; with Genera 8.0.1, Logical Pathnames Translation Files NEWEST, IP-TCP 422.2,
;;; RPC 415.0, Embedding Support 407.0, UX Support 416.0,
;;; Experimental Network RPC 415.0, Experimental NFS Client 415.0, CLX 419.0,
;;; C Runtime 416.0, Compiler Tools Package 411.0, Compiler Tools Runtime 411.0,
;;; C Packages 413.0, Minimal Lexer Runtime 416.0, Lexer Package 415.0,
;;; Syntax Editor Runtime 411.0, Experimental X Server 409.0, X Remote Screen 418.1,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.9, microcode 3640-MIC 430,
;;; FEP 127, Fep0:>v127-lisp.flod(64), Fep0:>v127-loaders.flod(64),
;;; Fep0:>v127-info.flod(64), Fep0:>v127-debug.flod(38), 1067x748 B&W Screen,
;;; Machine serial number 5603.



(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MKRP:PROG;C;PREPARATION.LISP.NEWEST"
  "MKRP:PROG;OS;OPERATINGSYSTEM.LISP.NEWEST")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OS;OPERATINGSYSTEM.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-")

(DEFUN OS=CONSTRUCT.PROC (PROBLEM.FILE GRAPH.FILE CODE.FILE PROOF.COMMENT cr)
						; input:  list of 0-4 atoms:
						;         [<problem.file> [<graph.file>
						;         [<code.file> [<proof.comment> ]]]]
						;         CR is a flag that is true iff construct and refute is combined
						; effect: opens the graph file, prepares the initial
						;         graph and closes the graph file.
						; value:  undefined.
						; remark: code.file is open and remains so.
  (LET (RESULT)
    (when (OS=SYSTEM.IS.READY.FOR 'CONSTRUCT)
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'CONSTRUCT.PROC_START.MESSAGE) *standard-output*)
      (SETQ PROOF.COMMENT
	    (COND (PROOF.COMMENT (OS.E-GET.EXPLANATION 'CONSTRUCT.PROC_PROOF.COMMENT PROOF.COMMENT)) (T NIL)))
      (OS=OPEN.GRAPH.FILE GRAPH.FILE)
      (SETQ RESULT (case (opt-get.option gen_other.prover)
		     (ER (PREP.er-INITIAL.GRAPH PROBLEM.FILE os*actual.graph.file CODE.FILE PROOF.COMMENT))
		     (C (prep.c-initial.graph PROBLEM.FILE os*actual.graph.file CODE.FILE PROOF.COMMENT))
		     (mkrp (PREP-INITIAL.GRAPH PROBLEM.FILE OS*ACTUAL.GRAPH.FILE CODE.FILE PROOF.COMMENT (not cr)))))
      (OS=CLOSE.GRAPH.FILE)
      (COND ((EQ RESULT 'SUCCESS) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'CONSTRUCT.PROC_THEOREM.PROVED) *standard-output*))))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OS;OPERATINGSYSTEM.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-")

(DEFUN OS=OPEN.GRAPH.FILE (GRAPHFILENAME)
						; INPUT:  A GRAPH FILENAME.
						; EFFECT: OPENS THE FILE. IF THE FILENAME IS NIL,
						;         A STANDARD FILENAME IS USED.
						; VALUE:  UNDEFINED.
  (SETQ OS*ACTUAL.GRAPH.FILE (MKRP-OPENOUT (MKRP-MAKE.PATHNAME T (mkrp-default.graph) (mkrp-default.lisp) GRAPHFILENAME)
					   nil "LISP" (eq (opt-get.option gen_other.prover) 'c))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;C;PREPARATION.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-")


(DEFUN PREP.C=INIT (PROBLEM.FILE GRAPH.FILE)
						; INPUT : TWO FILENAMES
						; REMARK: FILES ARE EXPECTED TO BE OPEN
						; EFFECT: INITIALIZES ALL NECESSARY VARIABLES
						; VALUE:  UNDEFINED
  (SETQ PREP*PROBLEM.FILE PROBLEM.FILE)
  (SETQ PREP*GRAPH.FILE GRAPH.FILE)
  (SETQ PREP*SEVERAL.SPLITPARTS NIL)
  (SETQ PREP*STEPNUMBER 0
	prep*objects nil
	prep*sps nil)  
  (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES 0)
  (SETQ PREP*AXIOMS.INFIX NIL)
  (SETQ PREP*AXIOMS.PREFIX NIL)
  (SETQ PREP*THEOREMS.INFIX NIL)
  (SETQ PREP*THEOREMS.PREFIX NIL)
  (SETQ PREP*COMMENT NIL))

(DEFUN PREP.C=END NIL
						; INPUT:  NONE
						; EFFECT: ENDS ALL NECESSARY SUBMODULES
						; VALUE:  UNDEFINED
  (format prep*graph.file "~2%KBS(Axioms, Theorems);~%}"))

(DEFUN PREP.C=CREATE.AXIOMGRAPH (FORMULALIST)
						      ; EDITED:  6-MAR-84 19:46:34
						      ; INPUT:  A LIST OF FOPC-EXPRESSIONS PREFIX-FORM
						      ; EFFECT: THE FORMULAS ARE PRESIMPLIFIED AND NORMA-
						      ;         LIZED. THE ATTRIBUTE CLAUSES AND AXIOM
						      ;         CLAUSES ARE CONSTRUCTUED AND ADDED TO THE
						      ;         AXIOMGRAPH
						      ; VALUE:  A LEGAL TOTAL.RESULT
  (PROG (CLAUSELIST PRESIMPLIFIED.FORMULA NORMALIZED.FORMULA RESULT)
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
	  ((MEMBER 'FALSE CLAUSELIST) (SETQ RESULT (LIST 'SUCCESS 'AXIOMS.UNSATISFIABLE))))
	(COND
	  ((NOT RESULT)
	   (SETQ CLAUSELIST (DELETE 'TRUE CLAUSELIST))
	   (SETQ CLAUSELIST (APPLY (FUNCTION NCONC) CLAUSELIST))
	   (SETQ CLAUSELIST
		 (SORT CLAUSELIST #'(LAMBDA (CLAUSE1 CLAUSE2) (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
						      ;(cons-CONSTRUCT.ATTRIBUTE.CLAUSES)
						      ;   ADDING THE AXIOM-CLAUSES TO THE GRAPH
	   (format prep*graph.file "#include \"kbs.h\"~2%main()~%{ ")
	   (MAPC #'(LAMBDA (CLAUSE)
		     (prep.c=print.clause.objects clause prep*graph.file))
		 CLAUSELIST)
	   (format prep*graph.file "  clauselist Axioms = ")
	   (prep.c=print.clauselist clauselist prep*graph.file)))
	(format prep*graph.file ";~2%")
	(RETURN RESULT)))

(DEFUN PREP.C=CREATE.INITIAL.GRAPHS (THEOREMS.PREFIX)
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
	   (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT)
	   (SETQ RESULT 'SUCCESS))
	  ((EQL RESULT 'TRUE)
	   (SETQ PREP*SEVERAL.SPLITPARTS NIL)
	   (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT)
	   (SETQ RESULT (COND (RESULT (CAR RESULT)))))
	  (T (SETQ PREP*SEVERAL.SPLITPARTS (NORM-SEVERAL.SPLITPARTS?))
	     (SETQ RESULT (PREP.C=CREATE.SPLITGRAPHS))))
	(RETURN RESULT)))

(defvar prep*sps)

(defun prep.c=print.splitparts.objects (st)
  (if (NORM-more.SPLITPARTS?)
      (let ((NORMALIZED.THEOREMS (SORT (NORM-NEXT.SPLITPART)
				       #'(LAMBDA (CLAUSE1 CLAUSE2)
					   (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2))))))
	(push NORMALIZED.THEOREMS prep*sps)
	(mapc #'(lambda (clause) 
		  (prep.c=print.clause.objects clause st))
	      NORMALIZED.THEOREMS)
	(prep.c=print.splitparts.objects st))))

(defun prep.c=print.splitparts (st)
  (if prep*sps
      (let ((NORMALIZED.THEOREMS (pop prep*sps)))
	(princ "SplCons(SpCreate(" st)
	(prep.c=print.clauselist NORMALIZED.THEOREMS st)
	(format st "),~%     ")
	(prep.c=print.splitparts st)
	(princ ")" st))
      (princ "SplEmpty()" st)))

(DEFUN PREP.C=CREATE.SPLITGRAPHS NIL
						; Edited:  06-MAY-1992 18:04
						; Authors: prckln
						; EFFECT: FOR ALL SPLITPARTS THE INITIAL GRAPHS ARE
						;         CONSTRUCTED AND SAVED ON THE INITIAL.GRAPH
						;         FILE
						; VALUE:  A LEGAL TOTAL.RESULT
  (prep.c=print.splitparts.objects prep*graph.file)
  (format prep*graph.file "~2%  splitpartlist Theorems = ")
  (prep.c=print.splitparts prep*graph.file)
  (format prep*graph.file ";~%"))

(defun prep.c=print.clauselist (clauselist st)
  (if (null clauselist)
      (princ "ClEmpty()" st)
      (progn (princ "ClCons(CCreate(" st)
	     (prep.c=print.clause (first clauselist) st)
	     (format st "),~%     ")
	     (prep.c=print.clauselist (rest clauselist) st)
	     (princ ")" st))))

(defun prep.c=print.litlist (litlist st)
  (if (null litlist)
      (princ "LlEmpty()" st)
      (progn (princ "LlCons(LCreate(" st)
	     (prep.c=print.term (first litlist) st)
	     (format st "), ")
	     (prep.c=print.litlist (rest litlist) st)
	     (princ ")" st))))

(defun prep.c=print.clause (clause st)
  (prep.c=print.litlist 
    (mapcar #'(lambda (lit)
		(if (ds-sign.is.negative (ds-lit.sign lit))
		    (list (first (dt-predicate.equalities))
			  (cons (ds-lit.predicate lit) (ds-lit.termlist lit))
			  (dt-predicate.false))
		    (cons (ds-lit.predicate lit) (ds-lit.termlist lit))))
	    clause)
    st))


(defun prep.c=print.termlist (termlist st)
  (if (null termlist)
      (princ "TlEmpty()" st)
      (progn (princ "TlCons(" st)
	     (prep.c=print.term (first termlist) st)
	     (format st ", ")
	     (prep.c=print.termlist (rest termlist) st)
	     (princ ")" st))))

(defun prep.c=print.term (term st)
  (if (consp term)
      (progn (princ "TCreate(" st)
	     (prep.c=print.term (first term) st)
	     (princ ", 1, " st)
	     (prep.c=print.termlist (rest term) st)
	     (princ ")" st))
      (format st "term~A" term)))

(defun prep.c=print.clause.objects (clause st)
  (mapc #'(lambda (lit)
	    (prep.c=print.term.objects (if (ds-sign.is.negative (ds-lit.sign lit))
					   (list (first (dt-predicate.equalities))
						 (cons (ds-lit.predicate lit) (ds-lit.termlist lit))
						 (dt-predicate.false))
					   (cons (ds-lit.predicate lit) (ds-lit.termlist lit)))
				       st))
	clause))

(defun prep.c=print.term.objects (term st)
  (if (consp term)
      (progn (prep.c=print.term.objects (first term) st)
	     (mapc #'(lambda (subterm) (prep.c=print.term.objects subterm st))
		   (rest term)))
      (if (numberp term)
	  (if (member term prep*objects)
	      nil
	      (progn (push term prep*objects)
		     (cond ((dt-variable.is term)
			    (format st "  term term~A = TCreate(~S,0,TlEmpty());~%" term term))
			   ((dt-function.is term)
			    (format st "  int term~A = ~A;~%" term term))
			   ((dt-constant.is term)
			    (format st "  term term~A = TCreate(~S,1,TlEmpty());~%" term term))
			   ((dt-predicate.is term)
			    (if (rest (dt-predicate.domainsorts term))
				(format st "  int term~A = ~A;~%" term term)
				(format st "  term term~A = TCreate(~S,1,TlEmpty());~%" term term))))))
	  (print term st))))
