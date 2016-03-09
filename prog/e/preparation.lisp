;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

(defvar prep*objects nil)

(DEFUN PREP.Er-INITIAL.GRAPH (PROBLEM.FILE GRAPH.FILE CODE.FILE COMMENT)
						; input   names of three files and a comment for the
						;         proof protocol.
						; effect: for the axioms and theorems resp. for all
						;         splitparts specified on problem.file the
						;         initial graphs are constructed and saved
						;         on the graph.file. the raw data for the
						;         protocol is written on code.file
						; value:  a legal total.result.
						; remark: code.file and graph.file are respected to
						;         be open and remain so
  (declare (ignore code.file comment))
  (let (RESULT)
    (PREP.ER=INIT PROBLEM.FILE GRAPH.FILE)
    (unwind-protect (progn (PREP=READ.PROBLEM.FILE)
			   (SETF PREP*AXIOMS.PREFIX   (PREP=REMOVE.COMMENTS PREP*AXIOMS.PREFIX)
				 PREP*THEOREMS.PREFIX (PREP=REMOVE.COMMENTS PREP*THEOREMS.PREFIX)
				 RESULT               (PSIM-APPLY.DEFINITIONS PREP*AXIOMS.PREFIX PREP*THEOREMS.PREFIX)
				 PREP*AXIOMS.PREFIX   (CAR RESULT)
				 PREP*THEOREMS.PREFIX (SECOND RESULT)
				 RESULT               (PREP.ER=CREATE.AXIOMGRAPH PREP*AXIOMS.PREFIX))
			   (unless RESULT (SETQ RESULT (PREP.ER=CREATE.INITIAL.GRAPHS PREP*THEOREMS.PREFIX))))
      (PREP.ER=END))
    RESULT))

(DEFUN PREP.ER=INIT (PROBLEM.FILE GRAPH.FILE)
						; INPUT : TWO FILENAMES
						; REMARK: FILES ARE EXPECTED TO BE OPEN
						; EFFECT: INITIALIZES ALL NECESSARY VARIABLES
						; VALUE:  UNDEFINED
  (SETQ PREP*PROBLEM.FILE PROBLEM.FILE)
  (SETQ PREP*GRAPH.FILE GRAPH.FILE)
  (SETQ PREP*SEVERAL.SPLITPARTS NIL)
  (SETQ PREP*STEPNUMBER 0
	prep*objects nil)  
  (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES 0)
  (SETQ PREP*AXIOMS.INFIX NIL)
  (SETQ PREP*AXIOMS.PREFIX NIL)
  (SETQ PREP*THEOREMS.INFIX NIL)
  (SETQ PREP*THEOREMS.PREFIX NIL)
  (SETQ PREP*COMMENT NIL))

(DEFUN PREP.ER=END NIL
						; INPUT:  NONE
						; EFFECT: ENDS ALL NECESSARY SUBMODULES
						; VALUE:  UNDEFINED
  )

(DEFUN PREP.ER=CREATE.AXIOMGRAPH (FORMULALIST)
						; EDITED:  6-MAR-84 19:46:34
						; INPUT:  A LIST OF FOPC-EXPRESSIONS PREFIX-FORM
						; EFFECT: THE FORMULAS ARE PRESIMPLIFIED AND NORMA-
						;         LIZED. THE ATTRIBUTE CLAUSES AND AXIOM
						;         CLAUSES ARE CONSTRUCTUED AND ADDED TO THE
						;         AXIOMGRAPH
						; VALUE:  A LEGAL TOTAL.RESULT
  (format prep*graph.file "(setq ectl*false ")
  (prep.er=print.term (dt-predicate.false) prep*graph.file)
  (format prep*graph.file "ectl*true ")
  (prep.er=print.term (dt-predicate.true) prep*graph.file)
  (format prep*graph.file "ectl*equality ")
  (prep.er=print.term (first (dt-predicate.equalities)) prep*graph.file)
  (format prep*graph.file "ectl*AXIOMS (list ")
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
						;    CONSTRUCTION OF THE ATTRIBUTE CLAUSES
						;    THERE IS NO CG-FIX ALLOWED UNTIL THE FIRST
						;    CALL OF PREP.ER=ADD.CLAUSE
	   (some #'(lambda (clause)
		     (when (and (opt-get.option str_finite.domain) (ds-lit.finite.domain clause))
		       (ds-finite.domain.set clause (ds-lit.finite.domain clause))))
		 clauselist)
	   ;(cons-CONSTRUCT.ATTRIBUTE.CLAUSES)
						;   ADDING THE AXIOM-CLAUSES TO THE GRAPH
	   (MAPC #'(LAMBDA (CLAUSE)
		     (prep.er=print.clause clause prep*graph.file))
		 CLAUSELIST)))
	(format prep*graph.file ")")
	(RETURN RESULT)))

(DEFUN PREP.ER=CREATE.INITIAL.GRAPHS (THEOREMS.PREFIX)
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
	     (SETQ RESULT (PREP.ER=CREATE.SPLITGRAPHS))))
	(RETURN RESULT)))

(DEFUN PREP.ER=CREATE.SPLITGRAPHS NIL		; EDITED:  6-MAR-84 20:59:34
						; EFFECT: FOR ALL SPLITPARTS THE INITIAL GRAPHS ARE
						;         CONSTRUCTED AND SAVED ON THE INITIAL.GRAPH
						;         FILE
						; VALUE:  A LEGAL TOTAL.RESULT
  (let ((RESULT nil)
	NORMALIZED.THEOREMS)
    (format prep*graph.file "~2% ectl*THEOREMS (list ")
    (WHILE (NORM-more.SPLITPARTS?)
      (princ "(list " prep*graph.file)
      (SETQ NORMALIZED.THEOREMS (SORT (NORM-NEXT.SPLITPART)
				      #'(LAMBDA (CLAUSE1 CLAUSE2)
					  (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
      (MAPC #'(LAMBDA (CLAUSE)
		(prep.er=print.clause clause prep*graph.file))
	    NORMALIZED.THEOREMS)
      (princ ")" prep*graph.file))
    (format prep*graph.file "))")
    RESULT))

(defun prep.er=print.clause (clause st)
  (princ "(list " st)
  (mapc #'(lambda (lit)
	    (prep.er=print.term (if (ds-sign.is.negative (ds-lit.sign lit))
				 (list (first (dt-predicate.equalities))
				       (cons (ds-lit.predicate lit) (ds-lit.termlist lit))
				       (dt-predicate.false))
				 (cons (ds-lit.predicate lit) (ds-lit.termlist lit)))
			     st))
	clause)
  (format st ")~%"))

(defun prep.er=print.term (term st)
  (if (consp term)
      (progn (princ "(eds-term_create " st)
	     (prep.er=print.term (first term) st)
	     (princ "(list " st)
	     (mapc #'(lambda (subterm) (prep.er=print.term subterm st))
		   (rest term))
	     (princ "))" st))
      (if (numberp term)
	  (if (member term prep*objects)
	      (format st "'#~A#" term)
	      (progn (push term prep*objects)
		     (format st "'#~A=#.~A" term (cond ((dt-variable.is term)
						       (format nil "(eds-var_create ~S)" (dt-pname term)))
						      ((dt-function.is term)
						       (format nil "(eds-fct_create ~S ~A ~A)"
							       (dt-pname term) (dt-function.arity term) (ord=symbol.weight term)))
						      ((dt-constant.is term)
						       (format nil "(eds-const_create ~S ~A)" (dt-pname term)
							       (ord=symbol.weight term)))
						      ((dt-predicate.is term)
						       (format nil "(eds-fct_create ~S ~A ~A)"
							       (dt-pname term) (length (DT-PREDICATE.DOMAINSORTS term))
							       (ord=symbol.weight term)))))))
	  (print term st))))
