;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP; Patch-File: Yes -*-

(in-package "MKRP")



(defvar po*rescounter)
(defvar po*faccounter)
(defvar po*paracounter)
(defvar po*instcounter)
(defvar po*rencounter)
(defvar po*indices nil "To record old literals")
(DEFVAR PO*FILE NIL)
(defvar po*all.formulas)
(defvar PO*INDENTATION 0
  "an integer, describing the number of blanks needed at the beginning of a line for formatted output")

(defvar po*operation.stack t
  "A stack indicating whether operations are collected or printed immediately, T means initial, ~
   NIL after end of insertion, a list of operations in beteeen")


(defun PO=INIT.COUNTERS ()  
  (declare (edited  "06-MAY-1993 12:30")
	   (authors KROENER PRCKLN)
	   (input   "")
	   (effect  "resets the counters for resolution, factoring and paramodulation")
	   (value   "undefined"))
  #+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
  (setq po*literal.index 0)
  (setq po*all.formulas nil)
  (setq po*operation.stack t)
  (setq po*rencounter -1)
  (setq po*rescounter -1)
  (setq po*faccounter -1)
  (setq po*paracounter -1)
  (setq po*instcounter -1))
 

(defun PO=PRINT.CONSTANTS (CONSTANTS)
  (declare (edited  "19-APR-1993 17:58")
	   (authors KROENER)
	   (input   "list of constants")
	   (effect  "prints name and sort")
	   (value   "undefined"))
  (when PO*FILE
    (cond ((consp CONSTANTS)
	   (format PO*FILE " (~A ~A)" (first (DS-PNAME CONSTANTS)) (DT-CONSTANT.SORT (first CONSTANTS)))
	   (PO=PRINT.CONSTANTS (rest CONSTANTS))))))


(defun po=print.predicates (predicates)
  (declare (edited  "19-APR-1993 17:58")
	   (authors KROENER)
	   (input   "list of predicates")
	   (effect  "prints name and sorts")
	   (value   "undefined"))
  (when PO*FILE
    (cond ((consp predicates)
	   (let ((name (dt-predicate.PNAME (first predicates)))
		 (types (cons 'o (reverse (dt-predicate.DOMAINSORTS (first predicates))))))
	     (when (not (or (equal '"=" name) (equal '"FALSE" name) (equal '"TRUE" name)))
	       (format PO*FILE " (~A ~A)" name types))
	       (po=print.predicates (rest predicates)))))))

(defun po=print.functions (functions)
  (declare (edited  "19-APR-1993 17:58")
	   (authors KROENER)
	   (input   "list of functions")
	   (effect  "prints name and sorts")
	   (value   "undefined"))  
  (when PO*FILE
    (cond ((consp functions)
	   (format PO*FILE " (~A (~A ~{ ~A~}))"
		     (dt-function.PNAME (first functions))
		     (dt-function.max.range.sort (first functions))
		     (reverse (dt-function.domainsorts (first functions))))
	   (po=print.functions (rest functions))))))

(defun PO-CONSTRUCT.START (FILE comment)  
  (declare (edited  "19-APR-1993 18:06")
	   (authors KROENER)
	   (input   "code file")
	   (effect  "prints problem, <name>, <status>, parts of <problem>")
	   (value   "undefined"))
  (when (SETQ PO*FILE FILE)  
	(PO=INIT.COUNTERS)
	(setq po*indices nil)
	(format PO*FILE "~%(problem ~A proved" comment)
	(format po*file "~%~4T((type-variables aa)~%~4T(type-variables bb)~%~4T~
                         (type-constants o)~%~4T(type-constants i)~%~4T(constants (= (o aa aa)))~%~4T~
	                 (constants (false o))~%~4T(constants (true o))~%~4T~
                         (constants (and (o o o)))~%~4T(constants (or (o o o)))~%~4T~
                         (constants (implies (o o o)))~%~4T(constants (equiv (o o o)))~%~4T~
                         (constants (not (o o)))~%~4T (constants (forall (o (o aa))))~%~4T~
	                 (constants (exists (o (o aa))))")
	(format PO*FILE "~%~4T(type-constants")
	(mapcar #'(lambda (constant)
		    (format PO*FILE " ~A" constant))
		(DT-SORT.ALL))
	(PRINC ")" PO*FILE)
	(format  PO*FILE "~%~4T(constants")
	(PO=PRINT.CONSTANTS (DT-CONSTANT.ALL))
	(po=print.predicates (DT-PREDICATE.ALL))
	(po=print.functions  (DT-FUNCTION.ALL))
	(princ ")" PO*FILE)))

#-:mkrp-basic
(defun po=treat.formula.internal (formula name)  
  (declare (edited  "20-AUG-1993 20:11")
	   (authors KROENER PRCKLN)
	   (input   "formula (quantifier variable rest)")
	   (effect  "changes mkrp-formula to post-formula")
	   (value   "formula (quantifier (lam (variable-declaration) rest))"))  
  (CASE (first formula)
	((AND OR)
	 (list (first formula)
	       (po=treat.formula.internal (second formula) name)
	       (po=treat.formula.internal (third formula) name)))
	(NOT (list (first formula)
		   (po=treat.formula.internal (second formula) name)))
	(impl (list 'implies
		    (po=treat.formula.internal (second formula) name)
		    (po=treat.formula.internal (third formula) name)))
	(EQv (list 'EQUIV
		   (po=treat.formula.internal (second formula) name)
		   (po=treat.formula.internal (third formula) name)))
	(ALL (list 'forall
		   (list 'lam
			 (list (second formula) (DT-VARIABLE.SORT (second formula)))
			 (po=treat.formula.internal (third formula) name))))
	(ex (list 'exists
		  (list 'lam
			(list (second formula) (DT-VARIABLE.SORT (second formula)))
			(po=treat.formula.internal (third formula) name))))
	(+ (dt-pname (cons (second formula) (third formula))))
	(- (list 'not (dt-pname (cons (second formula) (third formula)))))))

#-:mkrp-basic
(defun po=treat.formula (formula name)  
  (declare (edited  "20-AUG-1993 20:11")
	   (authors KROENER PRCKLN)
	   (input   "formula (quantifier variable rest)")
	   (effect  "changes mkrp-formula to post-formula")
	   (value   "formula (quantifier (lam (variable-declaration) rest))"))  
  (let ((f (po=treat.formula.internal formula name)))
    (push (cons name f) po*all.formulas)
    f))

#+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
(defun po=put.index (literal index)
  (setf (getf (fourth literal) 'Index) index))

#+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
(defvar po*literal.index)

#+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
(defun po=list.clause.cons.quantors (vars formula)
  (if vars
      `(forall (lam (,(first vars) ,(DT-VARIABLE.SORT (first vars)))
		    ,(po=list.clause.cons.quantors (rest vars) formula)))
    formula))

#+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
(defun po=lit (lit)
  (po=put.index lit (incf po*literal.index))
  (if (ds-sign.is.negative (ds-lit.sign lit))
      `(not (,(ds-lit.predicate lit) ,@(ds-lit.termlist lit)))
    `(,(ds-lit.predicate lit) ,@(ds-lit.termlist lit))))

#+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
(defun po=list.clause.make.formula (list.clause)
  (if (rest list.clause)
      `(or ,(po=lit (first list.clause))
	   ,(po=list.clause.make.formula (rest list.clause)))
    (po=lit (first list.clause))))

#+mkrp-self-problem ; Only if assumptions are from AX and TH and not from the original problem
(defun po=treat.formula (list.clause name)
  (let ((f (po=list.clause.cons.quantors (ds-lits.vars list.clause)
					 (po=list.clause.make.formula list.clause))))
    (push (cons name f) po*all.formulas)
    f))

(defun po-axioms.start ()
  (when PO*FILE
	(format PO*FILE "~%~4T(resolution-proof~%~8T(cnf (")))

(DEFUN Po-AXIOMS.END (RESULT) (declare (ignore result)))

(defun po-theorems.start (SPLITPART.IDENTIFIER) (declare (ignore SPLITPART.IDENTIFIER)))

#-:mkrp-basic
(defvar po*act.num)

#-:mkrp-basic
(defun po=get.formula.position (pos num formula)
  (declare (edited  "19-AUG-1993 17:58")
	   (authors PRCKLN)
	   (input  "POS is the position of FORMULA in a superformula in reverse direction."
		   "NUM is the number of an atom in the whole formula."
		   "FORMULA is a formula.")
	   (effect "None.")
	   (value  "Computes the position of the NUMth atom in the formula FORMULA and"
		   "appends it to the beginning position POS."))
  (CASE (first formula)
	((AND OR IMPLIES EQUIV)
	 (or (po=get.formula.position (cons 1 pos) num (second formula))
	     (po=get.formula.position (cons 2 pos) num (third formula))))
	(NOT (po=get.formula.position (cons 1 pos) num (second formula)))
	((forALL EXists)
	 (po=get.formula.position (cons 1 pos) num (second formula)))
	((lam)
	 (po=get.formula.position (cons 0 pos) num (third formula)))
	(OTHERWISE
	 (if (= (incf po*act.num) num)
	     (list pos)))))

#-:mkrp-basic
(defun po=get.formulas.position (num formulas)
  (declare (edited  "19-AUG-1993 17:58")
	   (authors PRCKLN)
	   (input  "The number of an atom NUM in a list of formulas FORMULAS.")
	   (effect "None.")
	   (value  "1. The name of the NUMth formula in FORMULAS."
		   "2. The position of the NUMth formula in FORMULAS."))
  (when formulas
	(let ((pos (po=get.formula.position nil num (rest (first formulas)))))
	  (if pos (values (first (first formulas)) (first pos))
	    (po=get.formulas.position num (rest formulas))))))

#-:mkrp-basic
(defun po=convert.indices (inds formulas)
  (declare (edited  "19-AUG-1993 17:58")
	   (authors PRCKLN)
	   (input  "A list of indices IND that are lists (CLAUSE NUM1 .. NUMn),"
		   "where NUMi specifies the number of the atom in FORMULAS, where the"
		   "ith literal in CLAUSE stems from.")
	   (effect "None.")
	   (value  "The corresponding Post-delta relation."))
  (mapcan #'(lambda (cl.litnos)
	      (let ((litno.cl -1))
		(mapcar-not-nil #'(lambda (litno)
				    (when litno
				      (setq po*act.num 0)
				      (multiple-value-bind
					  (name pos)
					  (po=get.formulas.position litno formulas)
					(list name
					      (list (cons 'position (nreverse pos))
						    (list (ds-pname (first cl.litnos))
							  (list 'position (incf litno.cl)))))
					#|(list (list name (cons 'position (nreverse pos)))
						 (list (ds-pname (first cl.litnos))
						       (list 'position (incf litno.cl))))|#)))
				(rest cl.litnos))))
	  inds))
 
(DEFUN po-theoremS.END (RESULT)
  (declare (edited  "19-AUG-1993 17:58")
	   (authors PRCKLN)
	   (input  "RESULT is a symbol.")
	   (effect "Prints the delta relation."
		   "If there are initial operations on axioms and theorems, these are stored"
		   "in PO*OPERATION.STACK, and are printed now."
		   "If RESULT is SUCCESS the prover information is printed next.")
	   (value  "Undefined."))
  (when po*file
    (let (indices)
      #-:mkrp-basic(setq indices (po=convert.indices po*indices (nreverse po*all.formulas)))
      (format po*FILE ")~%~12T(delta-relation ~{~%~10T~A~})" indices)
      (setq po*all.formulas nil);; Clean storage
      (PRINC ")" Po*FILE)
      (let ((ops po*operation.stack))
	(setq po*operation.stack nil)
	(when (consp ops)
	  (mapc #'(lambda (str) (princ str po*file))
		(nreverse ops))))
      (when (EQL (CAR RESULT) 'SUCCESS) (po=prover po*file)))))

(DEFUN Po-INITIAL.GRAPH ()
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")))

(DEFun Po-CONSTRUCT.END ()
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")))

(DEFUN Po-SPLITPARTS.START (FILE SYSTEM.VERSION COMMENT)
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")
	   (ignore FILE SYSTEM.VERSION COMMENT)))

(defun PO=PRINT.MKRP.FORMULAS (formulas.all file type name counter)  
  (declare (edited  "21-JUL-1993 17:58")
	   (authors KROENER PRCKLN)
	   (input  "list of formulas")
	   (effect "prints for example assumption ass1 ...")
	   (value  "undefined"))
  (WHEN FILE
	(let* ((formulas (set-difference formulas.all '(+ nil comment))))
	  (mapc #'(lambda (formula)
		    (unless (equal formula '(0))
			    (let ((name (format nil "~A~A" name (incf counter))))
			      (format FILE "~%~4T(~A ~A ~A)"
				      type name
				      (ds-pname (po=treat.formula formula name))))))
		formulas))))

(defun PO=PRINT.AXIOMS (axioms.all counter file)  
  (declare (edited  "21-JUL-1993 17:58")
	   (authors KROENER PRCKLN)
	   (input  "list of axioms")
	   (effect "prints assumption ass1 ...")
	   (value  "undefined"))
  (PO=PRINT.MKRP.FORMULAS axioms.all file "assumption" "ass" counter))


(DEFUN PO=PRINT.THEOREMS (theorems.all counter file)  
  (declare (edited  "21-JUL-1993 17:58")
	   (authors KROENER PRCKLN)
	   (input  "list of theorems, normaly 1 theorem")
	   (effect "prints conclusion  th1 ...")
	   (value  "undefined"))
  (PO=PRINT.MKRP.FORMULAS theorems.all file "conclusion" "th" counter))


(DEFUN PO-PREFIX.FORM (AXIOMS THEOREMS)  
  (declare (edited  "19-APR-1993 18:21")
	   (authors KROENER)
	   (input  "two lists")
	   (effect "Prints assumptions and conclusions if MKRP is used"
		   "without OMEGA, othewise OMEGA knows assumptions and conclusions.")
	   (value  "undefined"))
  #+:mkrp-basic(setq axioms nil)
  #+:mkrp-basic(setq theorems nil)
  (WHEN PO*FILE
	(PO=PRINT.AXIOMS AXIOMS 0 po*file)
	(PO=PRINT.THEOREMS THEOREMS 0 po*file)
	(PRINC ")" PO*FILE)))


(DEFUN PO-REFUTATION.START ()
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")))


(DEFUN PO=PCLAUSE (CLAUSE FILE)
  (declare (edited  "02-MAY-1993 22:09")
	   (authors KROENER)
	   (input   "a clause and a filename")
	   (effect  "prints the clause in the format: (clause address"
		    "pname parents variables.sorts literals")
	   (value   "undefined")
	   (ignore record.flag))
  (when file
    (format FILE "~%~12T")
    (PO=CLAUSE CLAUSE FILE nil nil t)))

(DEFUN PO-PARTIAL.GRAPH (PARTIAL.CLAUSE.LIST)  
  (declare (edited  "22-JUL-1993 21:29")
	   (authors KROENER PRCKLN)
	   (input   "list of clauses")
	   (effect  "prints clauses and delte relation")
	   (value   "undefined"))
  (when (AND PO*FILE PARTIAL.CLAUSE.LIST)
	(setq PO*INDENTATION (+ PO*indentation 9))
	(MAPC (FUNCTION (LAMBDA (CLAUSE) (PO=PCLAUSE CLAUSE PO*FILE))) PARTIAL.CLAUSE.LIST)
	(setq PO*INDENTATION (- PO*indentation 9))))

; ;;;;;;;;;;;;;;;;;;;;;
;;; Printing operations
; ;;;;;;;;;;;;;;;;;;;;;

(defun po=print.op.header (op counter file)
  (declare (edited  "19-AUG-1993 19:36")
	   (authors PRCKLN)
	   (input   "OP is one of \"resolution\", \"paramodulation\", \"instance\","
		    "and \factoring\". COUNTER is the actual value of the corresponding"
		    "counter. FILE is the code file.")
	   (effect  "Prints the operation header, i.e. type, step-number, on FILE.")
	   (value   "Undefined."))
  (format FILE "~%~8T(~A step-~D ~%~10T" op counter))

(defmacro po=print.operation.frame (operationtype counter file cl.expr par.expr subst.expr ren.expr)
  (declare (edited  "23-JUL-1993 19:36")
	   (authors PRCKLN)
	   (input   "OPERATIONTYPE is a string to be printed on POST outpt file as"
		    "operation."
		    "COUNTER is the corresponding step counter."
		    "FILE is the codefile stream."
		    "CL.EXPR is an expression to print a clause in form of a literal list."
		    "PAR.EXPR is an expression to print the paramodulation literals in form of positions."
		    "SUBST.EXPR is an expression to print the unifying substitution."
		    "REN.EXPR is an expression to print the renaming substitution.")
	   (effect  "Prints a paramodulation step using the input expressions.")
	   (value   "Undefined."))
  `(progn (po=print.op.header ,operationtype ,counter ,file)
	  ,cl.expr
	  ,par.expr
	  ,subst.expr
	  ,ren.expr
	  (princ ")" ,FILE)))

(DEFUN Po-OPERATION (OPERATION.TYPE ARGUMENTS)  
  (declare (edited  "23-JUL-1993 23:19")
	   (authors PRCKLN)
	   (input   "an atom, and arguments according to the type of operation")
	   (effect  "Prints a complete operation in post syntax, decompose composed operations."
		    "If the operation occurs before the insertion of the last theorem the "
		    "output is put in a list PO*OPERATION.STACK. We print it on strings"
		    "because a delay of printing is not possible due to deletion of used objects."
		    "All collected operations in PO*OPERATION.STACK are printed on the file in PO-THEOREMS.END.")
	   (value   "undefined"))
  (cond (po*operation.stack
	 (when (eq t po*operation.stack) (setq po*operation.stack nil))
	 (push (with-output-to-string (stream)
				      (po=operation opeRATION.TYPE ARGUMENTS stream))
	       po*operation.stack))
	(po*file (po=operation opeRATION.TYPE ARGUMENTS po*file))))

(defun po=operation (opeRATION.TYPE ARGUMENTS file)   
  (declare (edited  "23-JUL-1993 23:19")
	   (authors PRCKLN KROENER)
	   (input   "an atom, and arguments according to the type of operation."
		    "FILE is the output stream.")
	   (effect  "Prints a complete operation in post syntax on FILE, decomposes composed operations.")
	   (value   "undefined"))
  (CASE OPERATION.TYPE
	(r.chain                (PO=R.CHAIN ARGUMENTS FILE))
	(RESOLUTION             (PO=RESOLUTION             ARGUMENTS FILE (incf po*rescounter)))
	(PARAMODULATION         (PO=PARAMODULATION         ARGUMENTS FILE (incf po*paracounter)))
	(FACTORIZATION          (PO=FACTORIZATION          ARGUMENTS FILE (incf po*faccounter)))
	(instantiate            (PO=instantiate            ARGUMENTS FILE))
	(REPLACEMENT.OPERATION  (PO=REPLACEMENT.OPERATION  ARGUMENTS FILE))
	(DOUBLE.LITERAL         (PO=DOUBLE.LITERAL         ARGUMENTS FILE))
	(REWRITE                (PO=REWRITE                ARGUMENTS FILE (incf po*paracounter)))
	(REWRITE.SYMMETRY       (PO=REWRITE.SYMMETRY       ARGUMENTS FILE))
	(REPLACEMENT.RESOLUTION (PO=REPLACEMENT.RESOLUTION ARGUMENTS FILE))
	(OTHERWISE              (ERROR "Non-existing operation type: ~S" OPERATION.TYPE))))

(defun po=prover (file)
  (declare (edited  "06-MAY-1993 12:39")
	   (authors KROENER)
	   (input   "The code file for Post output.")
	   (effect  "Prints '(prover <prover-name> <options>)")
	   (value   "undefined"))  
  (format FILE ")~%~4T(prover mkrp ")
  (PO=OPTIONS FILE)
  (princ "))" FILE))

(DEFUN PO-REFUTATION.END (SPLITPART.IDENTIFIER RESULT)
  (declare (edited  "06-MAY-1993 12:39")
	   (authors KROENER)
	   (input   "")
	   (effect  "prints '(prover <prover-name> <options>)")
	   (value   "undefined")
	   (ignore SPLITPART.IDENTIFIER RESULT))  
  (when PO*FILE (po=prover po*file)))

(DEFUN Po-SPLITPARTS.END ()
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")))

(Defun PO=OPTIONS (FILE)
  (declare (edited  "06-MAY-1993 12:15")
	   (authors KROENER PRCKLN)
	   (input   "FILE is the output code file for POST expressions.")
	   (effect  "Prints the values of MKRP-options on FILE.")
	   (value   "Undefined."))
  (format file "~%~8T(options~{ ~A~%~8T~})" (OPT-GET.LIST.OPTIONS)))

(DEFUN PO-OPTIONS ()
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")))

(defun po=print.resolution (link uni clause renaming file counter)
  (declare (edited  "20-AUG-1993 12:15")
	   (authors KROENER MIKO PRCKLN)
	   (input   "UNIFIER is the unifier of the r-link LINK resulting in the"
		    "resolvent CLAUSE. RENAMING is the renaming substitution for the"
		    "resolvent."
		    "FILE is the output code file for POST expressions."
		    "COUNTER is the actual value of the resolution step counter.")
	   (effect  "Prints the information of the resolution step on FILE.")
	   (value   "Undefined."))
  (po=print.operation.frame "resolution" counter file
			    (PO=CLAUSE clause FILE (uni-unifier.codomain renaming) nil)
			    (PO=LINK.PARENTS link FILE)
			    (po=print.subst uni file)
			    (po=print.subst renaming file)))

(DEFUN PO=RESOLUTION (LINK.UNI.CLAUSE.ren FILE counter &REST args)  
  (declare (edited  "06-MAY-1993 12:15")
	   (authors KROENER MIKO PRCKLN)
	   (input   "a list (link unifier clause), a file and a counter")
	   (effect  "prints the information to FILE")
	   (value   "undefined"))
  (po=print.resolution (first link.uni.clause.ren)
		       (second link.uni.clause.ren)
		       (third link.uni.clause.ren)
		       (fourth link.uni.clause.ren)
		       file counter))

(DEFUN PO=LINK.PARENTS (LINK FILE)  
  (declare (edited  "06-MAY-1993 10:37")
	   (authors KROENER MIKO)
	   (input   "a link and a file")
	   (effect  "writes the parents of the link to FILE")
	   (value   "undefined"))  
  (po=print.parents (DS-LINK.POSPAR LINK)
		    (DS-LINK.POSLITNO LINK)
		    (if (member (ds-link.colour link) (ds-link.colours.for 'autolinks))
			(DS-LINK.POSPAR LINK)
		      (DS-LINK.NEGPAR LINK))
		    (DS-LINK.NEGLITNO LINK)
		    file))

(defun po=print.parents (pospar poslitno negpar neglitno file)
  (po=print.position (po=clause.pname pospar) (1- poslitno) nil
		     (po=clause.pname negpar) (1- neglitno) nil file))


(defun po=print.instance (clause newcounter file)
  (let ((uni (ds-clause.renaming clause))
	(renaming (po=clause.renaming clause)))
    ;(print (dt-pname uni) file) (print (dt-pname renaming) file) 
    (po=print.operation.frame "instance" newcounter file
			      (progn (format file "(clause ~A-~A ("  (po=clause.pname clause) newcounter)
				     (po=print.vars (uni-apply.substitution uni (ds-clause.variables clause) t)
						    file)
				     (princ ")" FILE)
				     (let ((indices nil))
				       (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
						(push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
						(po=print.lit (DS-CLAUSE.SIGN CLAUSE (1+ LITNO))
							      (ds-clause.predicate  clause (1+ litno))
							      (uni-apply.substitution
							       uni 
							       (ds-clause.termlist clause (1+ litno))
							       t)
							      file)
						(when nil (push (cons (ds-clause.pname clause) (nreverse indices))
								po*indices)))
				       (PRINC ") " FILE)))
			      (format file "(~A)" (po=clause.pname clause))
			      (po=print.subst nil file)
			      (po=print.subst (uni-apply.substitution renaming uni t) file))))





(defun po=print.vars (vars file)
  (MAPC #'(LAMBDA (VAR)			; Variables and sorts
		  (format file "(~A ~A)" (ds-pname var) (DT-VARIABLE.SORT VAR)))
	vars))

(defun po=print.subst (subst file)
  (format file "~%~10T(substitution ~:A ~:A)"
	  (ds-pname (uni-unifier.domain subst))
	  (ds-pname (uni-unifier.codomain subst))))

(defun po=print.lit (sign pred tl file)
  (format file " (~A ~A)"   (if (ds-sign.is.negative sign) '- '+) (ds-pname (cons pred tl))))

(defmacro po=print.clause.frame (clause vars file &rest body)
  `(progn  (format file "(clause ~A (" (po=clause.pname ,clause))
	   (po=print.vars ,vars ,file)
	   (princ ")" ,FILE)
	   ,@body
	   (princ ")" ,FILE)))

(DEFUN PO=REPLACEMENT.RESOLUTION (ARGUMENTS FILE)
  (declare (ignore ARGUMENTS FILE)))

(DEFUN PO=FACTORIZATION (LINK.UNI.CLAUSE FILE counter)
  (declare (edited  "23-JUL-1993 19:36")
	   (authors KROENER PRCKLN)
	   (input   "a list (link unifier clause) and a file")
	   (effect  "prints a factorization step in post syntax")
	   (value   "undefined"))
  (let ((link (first link.uni.clause))
	(uni (second link.uni.clause))
	(clause (third link.uni.clause))
	(renaming (fourth link.uni.clause)))
    (po=print.operation.frame "factoring" counter
			      file
			      (PO=CLAUSE clause FILE (uni-unifier.codomain renaming) nil)
			      (PO=LINK.FAC link FILE)
			      (po=print.subst uni file)
			      (po=print.subst renaming file))))

(DEFUN PO=instantiate (UNI.CLAUSE.old FILE)
  (declare (ignore UNI.CLAUSE.old FILE)))

(DEFUN PO=LINK.FAC (LINK FILE)		
  (declare (edited  "23-JUL-1993 19:36")
	   (authors PRCKLN)
	   (input   "An SI-link and a file.")
	   (effect  "Prints the parent literals of a factorization step in POST syntax.")
	   (value   "Undefined."))
  (po=print.fac.position file
			 (po=clause.pname (DS-LINK.POSPAR LINK))
			 (ds-link.poslitno link)
			 (ds-link.neglitno link)))

(defun po=print.fac.position (file clause litno1 litno2)
  (declare (edited  "19-AUG-1993 19:36")
	   (authors PRCKLN)
	   (input   "FILE is the outputfile for POST expressions."
		    "CLAUSE is a factorized clause."
		    "LITNO1 and LITNO2 are the numbers of the literals of CLAUSE that are factorized.")
	   (effect  "Prints the parent positions of the factoring step in POST syntax.")
	   (value   "Undefined."))
  (format file "~%~10T(~A (position ~D) (position ~D))"
	  clause
	  (1- litno1)
	  (1- litno2)))

;;; Composed Operations
; ;;;;;;;;;;;;;;;;;;;;;

(defun po=r.chain.first.res (file unifier clause resolvent first links)
  (declare (edited  "19-AUG-1993 19:36")
	   (authors PRCKLN)
	   (input   "FILE is the outputfile for POST expressions."
		    "UNIFIER is the combined unifier for a sequence of resolution and factoring"
		    "steps computed by the terminator."
		    "CLAUSE is the nucleus clause for the sequence of steps."
		    "RESOLVENT is the new clause."
		    "FIRST is the number of a literal."
		    "LINKS is the list of links used for the compound operation.")
	   (effect  "Prints resolution step according to the first link in LINKS in POST syntax.")
	   (value   "Undefined."))
  (setq unifier (first (UNI-MERGE.SUBSTITUTIONS (first (ds-link.unifiers (first links))) unifier)))
  (let ((lits nil))
    (po=print.operation.frame "resolution" (incf po*rescounter) file
			      (po=print.clause.frame resolvent
						     nil
						     file
						     (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
						       (unless (= (1+ litno) first) 
							 (setq lits (nconc1 lits (cons clause (1+ litno))))
							 (po=print.lit (ds-clause.sign clause (1+ litno))
								       (ds-clause.predicate  clause (1+ litno))
								       (uni-apply.substitution
									unifier
									(ds-clause.termlist clause (1+ litno)) t)
								       file))))
			      (PO=print.parents clause first (DS-LINK.OTHERPAR (CAR LINKS) CLAUSE)
						(ds-link.otherlitno (car links) clause first) file)
			      (po=print.subst unifier file)
			      (po=print.subst nil file))
    lits))

(defun po=r.chain.rest.res (resolvent clause lits rlinks double.lits unifier file)
  (mapc #'(lambda (rlink)
	    (setq unifier (first (UNI-MERGE.SUBSTITUTIONS (first (ds-link.unifiers rlink)) unifier))))
	rlinks)
  (po=replacement.operation;; a list of two elements (CLAUSE DESC)
   ;; where DESC is a list (UNIFIER CLAUSES RESOLUTIONS MULTIPLES).
   (list (cons resolvent lits)
	 (list unifier
	       (mapcar #'(lambda (link) (DS-LINK.OTHERPAR LINK CLAUSE))
		       rlinks)
	       (mapcar #'(lambda (link)
			   (let ((litno (DS-LINK.THISLITNO LINK CLAUSE)))
			     (list (cons clause litno)
				   (cons (DS-LINK.OTHERPAR LINK CLAUSE)
					 (DS-LINK.OTHERLITNO LINK CLAUSE litno))
				   (DS-LINK.RULE LINK))))
		       rlinks)
	       (cons double.lits (mapcar #'(lambda (link) (declare (ignore link)) nil)
					 rlinks))
	       nil
	       t))
   file))

(DEFUN PO=R.CHAIN (ARGUMENTS FILE)	 
  (declare (edited  "24-JUL-1993 12:15")
	   (authors HJO PRCKLN)
	   (input   "A list (RESOLVENT TERMLISTS CHAIN-ELEMENT)."
		    "RESOLVENT is the new clause,"
		    "CHAIN-ELEMENT is a list (UNIFIER CLAUSE LINKS POINTERS LITNOS)"
		    "where clause is the parentclause of RESOLVENT"
		    "(which is created by simultaneus resolution upon LINKS with 'UNIFIER')"
		    "POINTERS and LITNOS is irrelevant."
		    "TERMLISTS is the list of termlists of the intermediate clause which would be"
		    "created by resolution upon the first element of LINKS."
		    "This is the protocol information of the TERMINATOR module.")
	   (effect  "Prints the successive resolution and factoring steps in POST syntax.")
	   (value   "Undefined."))
  (let* ((RESOLVENT (CAR ARGUMENTS))
	 (UNIFIER (first (THIRD ARGUMENTS)))
	 (CLAUSE (SECOND (THIRD ARGUMENTS)))
	 (LINKS (THIRD (THIRD ARGUMENTS)));; (LITNOS (CAR (CDDDR (THIRD ARGUMENTS))))
	 (COLOUR (DS-LINK.COLOUR (CAR LINKS)))
	 (FIRST  (CASE COLOUR
		       (R  (DS-LINK.THISLITNO (CAR LINKS) CLAUSE))
		       (SI (DS-LINK.POSLITNO  (CAR LINKS)))
		       (OTHERWISE (ERROR "ILLEGAL COLOUR IN PO=R.CHAIN: ~A" COLOUR))))
	 RLINKS DOUBLE.LITS lits)
    (CASE COLOUR
	  (si (error "si link first in terminator not yet impl for POST"))
	  (otherwise (setq lits (po=r.chain.first.res file unifier clause resolvent first links))))
    (MAPC #'(lambda (LINK)		; Generate double literals
	      (cond ((EQL 'R (DS-LINK.COLOUR LINK)) (SETQ RLINKS (NCONC1 RLINKS LINK)))
		    (T (SETQ DOUBLE.LITS
			     (NCONC1 DOUBLE.LITS
				     (LIST (CONS clause
						 (COND ((< (DS-LINK.NEGLITNO LINK) FIRST)
							(DS-LINK.NEGLITNO LINK))
						       (T (1- (DS-LINK.NEGLITNO LINK)))))
					   (CONS clause
						 (COND ((< (DS-LINK.POSLITNO LINK) FIRST)
							(DS-LINK.POSLITNO LINK))
						       (T (1- (DS-LINK.POSLITNO LINK)))))
					   (DS-LINK.RULE LINK)))))))
	  (CDR LINKS))
    (po=r.chain.rest.res resolvent clause lits rlinks double.lits unifier file)))

(defun po=get.all.lits (clause)
  (declare (edited  "23-JUL-1993 19:36")
	   (authors PRCKLN)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of dotted pairs (CLAUSE . LITNO), one for each literal in CLAUSE."
		    "The list is used to simulate operations for composed MKRP-operations."))
  (let ((lits nil))
    (dotimes (litno (ds-clause.nolit clause))
	     (push (cons clause (1+ litno)) lits))
    (nreverse lits)))

(defun po=sim.resolution (res lits)
  (declare (edited  "23-JUL-1993 19:36")
	   (authors PRCKLN)
	   (input   "RES is a list of two dotted pairs (CLAUSE . LITNO),"
		    "the first one is also contained in LITS."
		    "LITS is a list of such pairs of arbitrary length.")
	   (effect  "See value.")
	   (value   "A resolution is simulated: The first literal in RES is deleted from LITS,"
		    "all literals of the second clause in RES but the specified one are added"
		    "to LITS."))
  (let ((lit1 (first res))
	(lit2 (second res)))
    (nconc (delete lit1 lits :test #'equal)
	   (delete lit2 (po=get.all.lits (first lit2)) :test #'equal))))

(defun po=sim.factoring (fac lits &optional (test #'equal))
  (declare (edited  "23-JUL-1993 19:36")
	   (authors PRCKLN)
	   (input   "FAC is a list of two dotted pairs (CLAUSE . LITNO),"
		    "both are also contained in LITS."
		    "LITS is a list of such pairs of arbitrary length.")
	   (effect  "See value.")
	   (value   "A factorization is simulated: The second literal in FAC is deleted from LITS."))
  (let ((lit2 (second fac)))
    (delete lit2 lits :test test)))

(defun po=search.act.litno (lit lits)
  (declare (edited  "24-Jul-1993 20:46")
	   (authors PRCKLN)
	   (input   "LIT is a dotted pair, LITS a list of dotted pairs (CLAUSE . LITNO).")
	   (effect  "None." )
	   (value   "The position of LIT in LITS, beginning count with 1."))
  (cond ((null lits) (error "~A not found in virtual list of lits" lit))
	((equal lit (first lits)) 1)
	(t (1+ (po=search.act.litno lit (rest lits))))))

(defun po=repl.clause (file resolvent act.lits oldrenaming newrenaming variables oldunifier)
  ;(print (dt-pname newrenaming) file) (print (dt-pname oldunifier) file)
  (po=print.clause.frame resolvent
			 (uni-apply.substitution oldrenaming variables t)
			 file
			 (mapc #'(lambda (lit)
				   (let ((cl (first lit))
					 (ln (rest lit)))
				     ;(print (dt-pname (ds-clause.termlist cl ln)) file)
				     (po=print.lit (ds-clause.sign cl ln)
						   (ds-clause.predicate cl ln)
						   (uni-apply.substitution
						    newrenaming
						    (uni-apply.substitution oldunifier
									    (ds-clause.termlist cl ln) t)
						    t)
						   file)))
			       act.lits)))

(defun po=merge.unifier (uni ren)
  (append ren uni))

(defun po=generate.ren.ren (ren)
  (smapcon #'(lambda (restren)
	       (DT-VARIABLE.RENAMING.SUBSTITUTION (list (first restren))))
	   #'cddr
	   ren))

(defun PO=COMPOSITION.OF.SUBSTITUTIONS (subst1 subst2)
  (NCONC (UNI-APPLY.SUBSTITUTION subst2 SUBST1 t) SUBST2))

(defun po=compute.unifier (subjectclause resolvent resolution oldrenaming originalrenaming newunifier)
  (when (= subjectclause resolvent)
    (PO=COMPOSITION.OF.SUBSTITUTIONS
     newunifier
     (first (uni-unify.termlists
	     (uni-apply.substitution originalrenaming
				     (ds-clause.termlist subjectclause
							 (rest (second resolution)))
				     t)
	     (uni-apply.substitution oldrenaming
				     (uni-apply.substitution newunifier
							     (ds-clause.termlist (first (first resolution))
										 (rest (first resolution)))
							     t)
				     t))))))

(defun po=repl.op.subst (file resolution unifier unifier.flag)
  (po=print.subst (if unifier.flag
		      (uni-restrict unifier
				    (ds-clause.lit.variables (first (second resolution))
							     (rest (second resolution))))
		      unifier)
		  file))

(defun po=repl.op.one.res (file resolvent resolution unifier act.lits act.litno
				assocs originalname originalrenaming unifier.flag)
  (declare (edited  "24-Jul-1993 20:46")
	   (authors PRCKLN)
	   (input   "FILE is the output codefile stream."
		    "RESOLVENT is the resulting, i.e. reduced, clause of the replacement operation."
		    "RESOLUTION is a list of two dotted pairs (CLAUSE . LITNO) representing"
		    "one resolution step."
		    "UNIFIER is the merged substitution of all involved unifiers."
		    "ACT.LITS is a list of dotted pairs (CLAUSE . LITNO) describing the actual"
		    "state of the reduced clause."
		    "ACT.LITNO is number of the resolution literal in the reduced clause in the"
		    "actual state after RESOLUTION is done."
		    "ASSOCS is an associationlist (REALCLAUSE . VIRTUALCLAUSE). This is necessary"
		    "because the terminator produces a clause which is changed when this print"
		    "is induced, and hence all literals must be got from other clauses.")
	   (effect  "prints one resolution step according to RESOLUTION in post syntax on FILE." )
	   (value   "Undefined"))
  (let* ((subjectclause (first (second resolution)))
	 (origoldrenaming (po=clause.renaming resolvent))
	 (oldrenaming (if (= resolvent subjectclause)
			  (append originalrenaming origoldrenaming)
			(append origoldrenaming (po=clause.renaming (first (second resolution))))))
	 (newrenaming (nconc (if origoldrenaming
				 (po=generate.ren.ren origoldrenaming)
			       (po=generate.renaming resolvent))
			     (if (= resolvent subjectclause)
				 nil
			       (po=generate.renaming subjectclause))))
	 #|Old version, maybe better than the new one  Axel 31/10/93
         (oldrenaming (append origoldrenaming
			      (if (= resolvent subjectclause)
				  nil
				(po=clause.renaming (first (second resolution))))))
	 (newrenaming (nconc (if origoldrenaming (po=generate.ren.ren origoldrenaming)
			       (po=generate.renaming resolvent))
			     (if (= subjectclause resolvent)
				 nil
			       (po=generate.renaming subjectclause))))|#
	 (mergedrenaming (po=merge.renaming oldrenaming newrenaming))
	 (oldname (po=clause.pname (or (cassoc resolvent assocs)
				       resolvent)))
	 (newunifier (or (po=compute.unifier subjectclause resolvent resolution oldrenaming
					     originalrenaming unifier)
			 unifier)))
    (setq newunifier (uni-apply.substitution oldrenaming newunifier t))
    (po=clause.putpname resolvent)
    (po=clause.putrenaming resolvent newrenaming)
    ;;(print (dt-pname oldrenaming) t) (print (dt-pname newrenaming) t) (print (dt-pname mergedrenaming) t)
    ;;(print (dt-pname newunifier) t) (print (dt-pname unifier) t)
    (po=print.operation.frame "resolution" (incf po*rescounter)
			      file
			      (po=repl.clause file resolvent act.lits oldrenaming newrenaming
					      (uni-unifier.codomain mergedrenaming)
					      unifier)
			      (PO=print.position oldname (1- act.litno) nil
						 (if (= subjectclause resolvent)
						     originalname
						   (po=clause.pname subjectclause))
						 (1- (rest (second resolution))) nil
						 file)
			      (po=repl.op.subst file resolution newunifier unifier.flag)
			      (po=print.subst mergedrenaming file))))

(defun po=repl.op.one.fac (file resolvent factoring unifier act.lits act.litno1 act.litno2 assocs unifier.flag)
  (declare (edited  "24-Jul-1993 20:46")
	   (authors PRCKLN)
	   (input   "FILE is the output codefile stream."
		    "RESOLVENT is the resulting, i.e. reduced, clause of the replacement operation."
		    "FACTORING is a list of two dotted pairs (CLAUSE . LITNO) representing"
		    "one FACTORING step."
		    "UNIFIER is the merged substitution of all involved unifiers."
		    "ACT.LITS is a list of dotted pairs (CLAUSE . LITNO) describing the actual"
		    "state of the reduced clause."
		    "ACT.LITNO is number of the resolution literal in the reduced clause in the"
		    "actual state after RESOLUTION is done."
		    "ASSOCS is an associationlist (REALCLAUSE . VIRTUALCLAUSE). This is necessary"
		    "because the terminator produces a clause which is changed when this print"
		    "is induced, and hence all literals must be got from other clauses.")
	   (effect  "prints one resolution step according to RESOLUTION in post syntax on FILE." )
	   (value   "Undefined"))
  (let* ((oldrenaming (po=clause.renaming resolvent))
	 (newrenaming (if oldrenaming (po=generate.ren.ren oldrenaming) (po=generate.renaming resolvent)))
	 (mergedrenaming (po=merge.renaming oldrenaming newrenaming))
	 (oldname (po=clause.pname (or (cassoc (first (first factoring)) assocs)
				       (first (first factoring)))))
	 newunifier)
    (po=clause.putpname resolvent)
    (po=clause.putrenaming resolvent newrenaming)
    (setq newunifier (uni-apply.substitution oldrenaming unifier t))
    ;(print (dt-pname oldrenaming) file) (print (dt-pname newrenaming) file) (print (dt-pname mergedrenaming) file)
    ;(print (dt-pname unifier) file)
    (po=print.operation.frame "factoring" (incf po*faccounter) file
			      (po=repl.clause file resolvent act.lits oldrenaming newrenaming
					      (uni-unifier.codomain mergedrenaming)
					      unifier)
			      (PO=print.fac.position file oldname
						     act.litno1 act.litno2)
			      (po=repl.op.subst file factoring newunifier unifier.flag)
			      (po=print.subst mergedrenaming file))))

(defun po=repl.op.implicit.multiple (file resolvent unifier act.lits assocs)
  (let (cand dlit (act.litno1 0) act.litno2)
    (somel #'(lambda (restlits)
	       (incf act.litno1)
	       (setq dlit (first restlits))
	       (if (setq cand (member (first restlits) (rest restlits) :test #'equal))
		   (progn (setq act.litno2 (+ 1 act.litno1 (position (first cand) (rest restlits) :test #'equal)))
			  (setq cand (first cand)))))
	   act.lits)
    (if cand
	(let ()
	  (setq act.lits (po=sim.factoring (list dlit cand) act.lits #'eql))
	  (po=repl.op.one.fac file resolvent (list dlit cand) unifier act.lits act.litno1 act.litno2 assocs nil)
	  (po=repl.op.implicit.multiple file resolvent unifier act.lits assocs))
      act.lits)))

(DEFUN PO=REPLACEMENT.OPERATION (CLAUSE.DESCRIPTION FILE)  
  (declare (edited  "24-Jul-1993 20:46")
	   (authors PRCKLN)
	   (input   "CLAUSE.DESCRIPTION is a list of two elements, a CLAUSE"
		    "and a list (UNIFIER CLAUSES RESOLUTIONS MULTIPLES)"
		    "and a file open for output.")
	   (effect  "prints resolution and factoring-steps in post syntax" )
	   (value   "Undefined"))
  ;;(format t "~%~a~%" CLAUSE.DESCRIPTION)
  (let* ((resolvent (first CLAUSE.DESCRIPTION))
	 (unifier (first (second CLAUSE.DESCRIPTION)))
	 (clauses (second (second CLAUSE.DESCRIPTION)))
	 (resolutions (third (second CLAUSE.DESCRIPTION)))
	 (multiples (fourth (second CLAUSE.DESCRIPTION)))
	 (unifier.flag (sixth (second CLAUSE.DESCRIPTION)))
	 act.lits assocs
	 originalname originalrenaming)
    (mapc #'(lambda (cl) (po=restrict.renaming cl)) clauses)
    (if (consp resolvent)
	(setq act.lits (rest resolvent)
	      resolvent (first resolvent)
	      assocs (list (cons (first (first act.lits)) resolvent)))
      (setq act.lits (po=get.all.lits resolvent)))
    (setq originalname (po=clause.pname resolvent))
    (setq originalrenaming (po=clause.renaming resolvent))
    (unless (= (length multiples) (length resolutions)) ; If there are more multiple literal deletions
					; than resolutions, the sequence begins with a factorization
      (mapc #'(lambda (multiple)		
		(let ((act.litno1 (po=search.act.litno (first multiple) act.lits))
		      (act.litno2 (po=search.act.litno (second multiple) act.lits)))
		  (setq act.lits (po=sim.factoring multiple act.lits))
		  (po=repl.op.one.fac file resolvent multiple unifier act.lits
				      act.litno1 act.litno2 assocs unifier.flag)))
	    (first multiples))
      (setq multiples (rest multiples))) ; The remaining multiples belong to the resolutions
    (if resolutions
	(mapc #'(lambda (resolution multiple)
		  (let ((act.litno (po=search.act.litno (first resolution) act.lits)))
		    (setq act.lits (po=sim.resolution resolution act.lits))
		    (po=repl.op.one.res file resolvent resolution unifier act.lits act.litno assocs
					originalname originalrenaming unifier.flag)
		    (mapc #'(lambda (dlit)	
			      (let ((act.litno1 (po=search.act.litno (first dlit) act.lits))
				    (act.litno2 (po=search.act.litno (second dlit) act.lits)))
				(setq act.lits (po=sim.factoring dlit act.lits))
				(po=repl.op.one.fac file resolvent dlit unifier act.lits act.litno1
						    act.litno2 assocs unifier.flag)))
			  multiple)
		    (setq act.lits (po=repl.op.implicit.multiple file resolvent unifier act.lits assocs))))
	      resolutions
	      multiples))))

(defun po=restrict.renaming (clause)
  (let* ((vars (ds-clause.variables clause))
	 (ren (smapcon #'(lambda (restren)
			   (if (member (first restren) vars)
			       (list (first restren) (second restren))))
		       #'cddr
		       (po=clause.renaming clause))))
    (po=clause.putrenaming clause ren)))

(DEFUN PO=VARIABLES.SORTS.OF.CODOMAIN (UNIFIER)	; edited: 15-may-84 09:18:06  by cl
						; input :  a list with an even number of elements
						; effect:  finds all the variables appearing in the
						;          codomain of the unifier
						; value :  a list of dotted pairs (var . sort)
  (let (CODOMAIN VARIABLES)
    (SETQ CODOMAIN  (SMAPCAR #'identity #'CDDR (CDR UNIFIER))
	  VARIABLES (DT-TERMLIST.VARIABLES CODOMAIN))
    (MAPCAR #'(LAMBDA (VAR) (CONS VAR (DT-VARIABLE.SORT VAR))) VARIABLES)))


(DEFUN PO=DOUBLE.LITERAL (CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE FILE)
  (declare (ignore CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE FILE))
  (error "Multiple literal removal must be switched off for POST"))

(defun po=compute.rw.match (match leftcopy rule)
  (let* ((left (dt-access (list (ds-clause.lit.rewrite.rule rule 1)) (ds-clause.termlist rule 1)))
	 (copymatch (first (uni-unify1.terms leftcopy left t))))
    (UNI-apply.substitution copymatch match t)))

(defun po=taf.to.position (taf)
  (cons 1 taf))

(defun po=print.position (clause1 litno1 taf1 clause2 litno2 taf2 file)
  (format file
	  "~%~10T(~A (position ~D~{ ~D~})) (~A (position ~D)~@[ ~A~])"
	  clause1 litno1 taf1
	  clause2 litno2 taf2))

(defmacro po=generate.renaming (clause)
  `(DT-VARIABLE.RENAMING.SUBSTITUTION (ds-clause.variables ,clause)))

(defun po=merge.renaming (oldren newren)
  (if oldren
      (smapcon #'(lambda (newrest)
		   (let ((other (member (first newrest) oldren)))
		    (list (if other (second other) (first newrest))
			  (second newrest))))
	       #'cddr
	       newren)
    newren))

(defun po=clause.pname (clause)
  (or (dt-getprop clause `po*pname) (ds-clause.pname clause)))

(defmacro po=clause.renaming (clause)
  `(dt-getprop ,clause 'po*renaming))

(defun po=clause.putpname (clause)  
  (dt-putprop clause 'po*pname (format nil "~A.Ren~D"
				       (ds-clause.pname clause)
				       (incf po*rencounter))))

(defmacro po=clause.putrenaming (clause renaming)  
  `(dt-putprop ,clause 'po*renaming ,renaming))

(DEFUN PO=PARAMODULATION (LINK.UNI.CLAUSE FILE counter)	
					; Edited:  08-APR-1992 19:06
					; Authors: PRCKLN CL
					; input : a list (link unifier clause)  and a file
					; effect: prints information as described in po=clause
					;                   and po=link.par respectively
					; value : undefined
  (when file
	(let ((link (first link.uni.clause))
	      (uni (second link.uni.clause))
	      (clause (third link.uni.clause))
	      (renaming (fourth link.uni.clause)))
	  (setq uni (uni-apply.substitution (po=clause.renaming (ds-link.pospar link)) uni t))
	  (setq uni (uni-apply.substitution (po=clause.renaming (ds-link.negpar link)) uni t))
	  ;(print uni)
	  (setq renaming (uni-apply.substitution (po=clause.renaming (ds-link.pospar link)) renaming t))
	  (setq renaming (uni-apply.substitution (po=clause.renaming (ds-link.negpar link)) renaming t))
	  ;(print renaming)
	  (if (member (ds-link.colour link) (DS-LINK.COLOURS.FOR 'autolinks))
	      (po=i.paramodulation link uni clause renaming file counter)
	    (po=e.paramodulation link uni clause renaming file counter)))))

(defun po=i.paramodulation (link uni clause renaming file counter)
  (declare (Edited "19-AUG-1993 20:07")
	   (Authors PRCKLN)
	   (input  "An internnal paramodulation link LINK, the used unifier UNI, the resulting"
		   "clause CLAUSE, the renaming RENAMING, the paramodulation counter COUNTER,"
		   "and file opened for output.")
	   (effect "Prints the copy operation as instance and the"
		   "paramodulation operation in Post syntax on FILE.")
	   (value  "Undefined."))  
  (let ((newcounter (incf po*instcounter)))
    (po=print.instance (ds-link.negpar link) newcounter file)
    (po=print.operation.frame "paramodulation" counter file
			      (PO=CLAUSE clause FILE (DT-TERMLIST.VARIABLES (uni-unifier.codomain renaming)) nil)
			      (PO=LINK.par.inst link newcounter FILE)
			      (po=print.subst uni file)
			      (po=print.subst renaming file))))

(defun po=e.paramodulation (link uni clause renaming file counter)
  (declare (Edited "19-AUG-1993 20:07")
	   (Authors PRCKLN)
	   (input  "An external paramodulation link LINK, the used unifier UNI, the resulting"
		   "clause CLAUSE, the renaming RENAMING, the paramodulation counter COUNTER,"
		   "and file opened for output.")
	   (effect "Prints the paramodulation operation in Post syntax on FILE.")
	   (value  "Undefined."))  
  (po=print.operation.frame "paramodulation" counter file
			    (PO=CLAUSE clause FILE (DT-TERMLIST.VARIABLES (uni-unifier.codomain renaming)) nil)
			    (PO=LINK.par link FILE)
			    (po=print.subst uni file)
			    (po=print.subst renaming file)))


(DEFUN PO=LINK.PAR (LINK FILE)
  (declare (Edited "30-OKT-1992 20:07")
	   (Authors PRCKLN MKRP CL)
	   (input  "An external paramodulation link and a file opened for output.")
	   (effect "Prints the operation literals with positions in Post syntax on FILE.")
	   (value  "Undefined."))
  (po=print.position (po=clause.pname (ds-link.negpar link))
		     (- (DS-LINK.NEGLITNO LINK) 1)
		     (po=taf.to.position (ds-link.negfct link))
		     (po=clause.pname (DS-LINK.POSPAR LINK))
		     (- (DS-LINK.POSLITNO LINK) 1)
		     (if (DT-TAF.IS.LEFT (ds-link.posfct link)) 'lr 'rl)
		     file))

(DEFUN PO=LINK.PAR.inst (LINK counter FILE)
  (declare (Edited "25-JUL-1992 20:07")
	   (Authors PRCKLN MKRP CL)
	   (input  "An internal paramodulation link, an operation counter and a file opened for output.")
	   (effect "Prints the operation literals with positions in Post syntax on FILE.")
	   (value  "Undefined."))
  (format file "~%~10T(~A (position ~D~{ ~D~})) (~A-~A (position ~D) ~A)"
	  (po=clause.pname (DS-LINK.NEGPAR LINK))
	  (- (DS-LINK.NEGLITNO LINK) 1)
	  (ds-link.negfct link)
	  (po=clause.pname (DS-LINK.POSPAR LINK)) counter
	  (- (DS-LINK.POSLITNO LINK) 1)
	  (if (DT-TAF.IS.LEFT (ds-link.posfct link)) 'lr 'rl)))

(DEFUN PO=REWRITE (RULE.CLAUSE.LITNO.pos FILE counter)
  (when file
    (let* ((rule (first rule.clause.litno.pos))
	   (clause (second rule.clause.litno.pos))
	   (litno (third rule.clause.litno.pos))
	   (pos (first (fourth rule.clause.litno.pos)))
	   (match (second (fourth rule.clause.litno.pos)))
	   (leftcopy (third (fourth rule.clause.litno.pos)))
	   (name (po=clause.pname clause))
	   (oldrenaming (po=clause.renaming clause))
	   (newrenaming (po=generate.renaming clause))
	   (mergedrenaming (po=merge.renaming oldrenaming newrenaming)))
      (setq match (uni-apply.substitution oldrenaming
					  (po=compute.rw.match match leftcopy rule)
					  t))
      (setq match (uni-apply.substitution (po=clause.renaming rule) match t))
      (po=clause.putpname clause)
      (po=clause.putrenaming clause newrenaming)
      ;(print (dt-pname oldrenaming) file)(print (dt-pname newrenaming) file)(print (dt-pname mergedrenaming) file)
      (po=print.operation.frame "paramodulation" counter file
				(PO=CLAUSE clause FILE
					   (DT-TERMLIST.VARIABLES (uni-unifier.codomain mergedrenaming))
					   newrenaming)
				(po=print.position name
						   (1- litno)
						   (po=taf.to.position pos)
						   (po=clause.pname rule)
						   0
						   (if (dt-taf.is.left (list (ds-clause.lit.rewrite.rule rule 1))) 'lr 'rl)
						   file)
				(po=print.subst match file)
				(po=print.subst mergedrenaming file)))))

(DEFUN PO=REWRITE.SYMMETRY (CLAUSE FILE) ; To Do
  (declare (ignore CLAUSE FILE)))


(DEFUN PO=CLAUSE (CLAUSE FILE vars renaming &optional record.flag)  
  (declare (edited  "19-AUG-1993 19:36")
	   (authors PRCKLN)
	   (input   "CLAUSE is a clause."
		    "VARS are additional variables to be declared for the operation resulting"
		    "in CLAUSE."
		    "RENAMING is the renaming used after resolution."
		    "FILE is the outputfile for POST expressions.")
	   (effect  "Prints the clause in POST syntax.")
	   (value   "Undefined."))
  (let ((variables (uni-apply.substitution renaming (DS-CLAUSE.VARIABLES CLAUSE) t))
	(additional.vars (uni-apply.substitution renaming vars t)))
    (po=print.clause.frame clause
			   (union additional.vars variables)
			   file
			   (let ((indices nil))
			     (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
			       (unless (eq (1+ litno) nil)
				 (push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
				 (po=print.lit (ds-clause.sign clause (1+ litno))
					       (ds-clause.predicate  clause (1+ litno))
					       (uni-apply.substitution renaming
								       (ds-clause.termlist clause (1+ litno))
								       t)
					       file)))
			     (when record.flag
			       (push (cons (ds-clause.pname clause) (nreverse indices)) po*indices))))))


(DEFUN po-INFIX.FORM (AXIOMS.INFIX THEOREMS.INFIX)
  (declare (edited  "20-AUG-1993 12:15")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "None.")
	   (value   "Undefined.")
	   (remark  "Only to keep the interface of PR and PO the same.")
	   (ignore AXIOMS.INFIX THEOREMS.INFIX)))
