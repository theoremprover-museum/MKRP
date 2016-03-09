;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

(DEFUN PREP.c-INITIAL.GRAPH (PROBLEM.FILE GRAPH.FILE CODE.FILE COMMENT)
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
    (PREP.C=INIT PROBLEM.FILE GRAPH.FILE)
    (unwind-protect (progn (PREP=READ.PROBLEM.FILE)
			   (SETF PREP*AXIOMS.PREFIX   (PREP=REMOVE.COMMENTS PREP*AXIOMS.PREFIX)
				 PREP*THEOREMS.PREFIX (PREP=REMOVE.COMMENTS PREP*THEOREMS.PREFIX)
				 RESULT               (PSIM-APPLY.DEFINITIONS PREP*AXIOMS.PREFIX PREP*THEOREMS.PREFIX)
				 PREP*AXIOMS.PREFIX   (CAR RESULT)
				 PREP*THEOREMS.PREFIX (SECOND RESULT)
				 RESULT               (PREP.C=CREATE.AXIOMGRAPH PREP*AXIOMS.PREFIX))
			   (unless RESULT (SETQ RESULT (PREP.C=CREATE.INITIAL.GRAPHS PREP*THEOREMS.PREFIX))))
      (PREP.C=END))
    RESULT))

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
	prep*funs nil
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
  (format prep*graph.file ";~2%  funlist Functions = ")
  (prep.c=print.fun.list prep*funs prep*graph.file)  
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
			(push term prep*funs)
			(format st "  int term~A = FCreate(~S,~A,~A);~%"
				term (dt-pname term) (dt-function.arity term) (ord=symbol.weight term)))
		       ((dt-constant.is term)
			(push term prep*funs)
			(format st "  int term~A = FCreate(~S,~A,~A);~%"
				term (dt-pname term) 0 (ord=symbol.weight term)))
		       ((dt-predicate.is term)
			(push term prep*funs)
			(format st "  int term~A = FCreate(~S,~A,~A);~%"
				term (dt-pname term) (length (DT-PREDICATE.DOMAINSORTS term)) (ord=symbol.weight term))))))
    (print term st))))

(defun prep.c=print.fun.list (funs st)
  (cond (funs (format st "FlCons(term~A, " (first funs))
	      (prep.c=print.fun.list (rest funs) st)
	      (format st ")"))
	(t    (format st "FlEmpty()"))))