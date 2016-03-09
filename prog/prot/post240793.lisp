;;; -*- mode: lisp; syntax: common-lisp; package: mkrp -*-

;;; Copyright (C) 1991 AG Siekmann, 
;;;                       Fachbereich Informatik, Universitaet des Saarlandes, 
;;;                       Saarbruecken, Germany
;;; 
;;; This file is part of Markgraf Karl Refutation Procedure (MKRP).
;;; 
;;; MKRP is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  
;;; 
;;; Everyone is granted permission to copy, modify and redistribute
;;; MKRP, but only if it is not used for military purposes or any
;;; military research. It is also forbidden to use MKRP in nuclear plants
;;; or nuclear research, and for verifying programs in military 
;;; and nuclear research.  A copy of this license is
;;; supposed to have been given to you along with MKRP so you
;;; can know your rights and responsibilities.  
;;; Among other things, the copyright notice
;;; must be preserved on all copies.  

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))



(defvar po*rescounter)
(defvar po*faccounter)
(defvar po*paracounter)
(defvar po*instcounter)
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
  #+:mkrp-basic (setq po*literal.index 0)
  (setq po*all.formulas nil)
  (setq po*operation.stack t)
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
	   (format PO*FILE " (~A ~A)" (first (DT-PNAME CONSTANTS)) (DT-CONSTANT.SORT (first CONSTANTS)))
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
	(format PO*FILE "~%~4T((type-constants")
	(mapcar #'(lambda (constant)
		    (format PO*FILE " ~A" constant))
		(DT-SORT.ALL))
	(PRINC ")" PO*FILE)
	(format  PO*FILE "~%~4T(constants")
	(PO=PRINT.CONSTANTS (DT-CONSTANT.ALL))
	(po=print.predicates (DT-PREDICATE.ALL))
	(po=print.functions  (DT-FUNCTION.ALL))
	(princ ")" PO*FILE)))

(defun po=modify.quantor (element)  
  (declare (edited  "17-MAY-1993 18:41")
	   (authors KROENER)
	   (input   "first element of a clause")
	   (effect  "changes all->forall and ex->exists")
	   (value   "modified element"))  
  (let ((foo1 (subst 'exists 'ex element)))
    (let ((foo2 (subst 'forall 'all foo1)))
      foo2)))


(defun po-rearrange (obj)
  (declare (edited  "03-JUN-1993 19:02")
	   (authors KROENER)
	   (input   "a predicate in mkrp-notation")
	   (effect  "changes obj to post-notation")
	   (value   "modified predicate"))
  (if (listp obj)
      (let ((foo1 (first obj))
	    (foo2 (second obj)))
	(if (atom foo1)
	    (cons (dt-pname foo1)
		  (mapcar #'(lambda (foo2.part)
			      (dt-pname foo2.part))
			  foo2))
	  (mapcar #'(lambda (obj.part) (po-rearrange obj.part)) obj)))))


(defun po-modify-pred (term)  
  (declare (edited  "03-JUN-1993 19:05")
	   (authors KROENER)
	   (input   "a term")
	   (effect  "modifies the predicates of the term")
	   (value   "modified term"))
  (cond ((listp term)
	 (let* ((op1 (first term))
		(op (if (listp op1)
			(first op1)
		      op1)))
	   (cond ((equal op 'NOT)
		  (list op (po-modify-pred (second term))))
		 ((member op '(IMPL AND OR))
		  (list op
			(po-modify-pred (second term))
			(po-modify-pred (third term))))
		 (t (po-rearrange term)))))))

#-:mkrp-basic
(defun po=treat.formula (formula)  
  (declare (edited  "29-APR-1993 20:11")
	   (authors KROENER)
	   (input   "formula (quantifier variable rest)")
	   (effect  "changes mkrp-formula to post-formula")
	   (value   "formula (quantifier (lam (variable-declaration) rest))"))  
  (if (listp formula)
      (let ((foo (po=modify.quantor (first formula)))
	    (f (if (member foo '(forall exists))
		   (let ((full-var (list (second formula) (DT-VARIABLE.SORT (second formula))))
			 (body (third formula)))
		     (list foo (list 'lam full-var (po=treat.formula body))))
		 (if (listp formula)
		     (po-modify-pred formula)))))
	(push f po*all.formulas)
	f)))

#+:mkrp-basic
(defun po=put.index (literal index)
  (setf (getf (fourth literal) 'Index) index))

#+:mkrp-basic
(defvar po*literal.index)

#+:mkrp-basic
(defun po=list.clause.cons.quantors (vars formula)
  (if vars
      `(forall (lam (,(first vars) ,(DT-VARIABLE.SORT (first vars)))
		    ,(po=list.clause.cons.quantors (rest vars) formula)))
    formula))

#+:mkrp-basic
(defun po=lit (lit)
  (po=put.index lit (incf po*literal.index))
  (if (ds-sign.is.negative (ds-lit.sign lit))
      `(not (,(ds-lit.predicate lit) ,@(ds-lit.termlist lit)))
    `(,(ds-lit.predicate lit) ,@(ds-lit.termlist lit))))

#+:mkrp-basic
(defun po=list.clause.make.formula (list.clause)
  (if (rest list.clause)
      `(or ,(po=lit (first list.clause))
	   ,(po=list.clause.make.formula (rest list.clause)))
    (po=lit (first list.clause))))

#+:mkrp-basic
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

(defvar po*act.num)

(defun po=get.formula.position (pos num formula)
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

(defun po=get.formulas.position (num formulas)
  (when formulas
	(let ((pos (po=get.formula.position nil num (rest (first formulas)))))
	  (if pos (values (first (first formulas)) (first pos))
	    (po=get.formulas.position num (rest formulas))))))

(defun po=convert.indices (inds formulas)
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
						       (list (format nil "c~A" (dt-pname (first cl.litnos)))
							     (list 'position (incf litno.cl)))))
					   #|(list (list name (cons 'position (nreverse pos)))
						 (list (format nil "c~A" (dt-pname (first cl.litnos)))
						       (list 'position (incf litno.cl))))|#)))
				(rest cl.litnos))))
	  inds))

(DEFUN po-theoremS.END (RESULT)
  (when po*file
	(format po*FILE ")~%~12T(delta-relation ~{~%~10T~A~})"
		(po=convert.indices po*indices (nreverse po*all.formulas)))
	(setq po*all.formulas nil) ;; Clean storage
	(PRINC ")" Po*FILE)
	(let ((ops po*operation.stack))
	  (setq po*operation.stack nil)
	  (when (consp ops)
		(mapc #'(lambda (str) (princ str po*file))
		      (nreverse ops))))
	(when (EQL (CAR RESULT) 'SUCCESS) (po=prover po*file))))

(DEFUN Po-INITIAL.GRAPH ())

(DEFun Po-CONSTRUCT.END ())

(DEFUN Po-SPLITPARTS.START (FILE SYSTEM.VERSION COMMENT)
  (declare (ignore FILE SYSTEM.VERSION COMMENT)))

(defun PO=PRINT.MKRP.FORMULAS (formulas.all file type name counter)  
  (declare (edited  "21-JUL-1993 17:58")
	   (authors KROENER PRCKLN)
	   (input  "list of formulas")
	   (effect "prints for example assumption ass1 ...")
	   (value  "undefined"))
  (WHEN FILE
	(let* ((formulas1 (remove '+ formulas.all))
	       (formulas (remove nil formulas1)))
	  (mapc #'(lambda (formula)
		    (unless (equal formula '(0))
			    (let ((name (format nil "~A~A" name (incf counter))))
			      (format FILE "~%~4T(~A ~A ~A)"
				      type name
				      (dt-pname (po=treat.formula formula name))))))
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
	   (effect "prints assumptions and conclusions")
	   (value  "undefined"))
  
  (WHEN PO*FILE
	(PO=PRINT.AXIOMS AXIOMS 0 po*file)
	(PO=PRINT.THEOREMS THEOREMS 0 po*file)
	(PRINC ")" PO*FILE)))


(DEFUN PO-REFUTATION.START ())


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
    (PO=CLAUSE CLAUSE FILE nil t)))

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

(DEFUN Po-OPERATION (OPERATION.TYPE ARGUMENTS)  
  (declare (edited  "23-JUL-1993 23:19")
	   (authors PRCKLN)
	   (input   "an atom, and arguments according to the type of operation")
	   (effect  "Prints a complete operation in post syntax, decompose composed operations."
		    "If the operation occurs before the insertion of the last theorem the "
		    "output is put in a list PO*OPERATION.STACK. We print it on strings"
		    "because a delay of printing is not possible due to deletion of used objects."
		    "All collected operations are printed on the file in PO-THEOREMS.END.")
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
	   (input   "an atom, and arguments according to the type of operation")
	   (effect  "Prints a complete operation in post syntax on FILE, decomposes composed operations.")
	   (value   "undefined"))
  (CASE OPERATION.TYPE
	(R.CHAIN   (PO=R.CHAIN ARGUMENTS FILE))
	(OTHERWISE (setq PO*INDENTATION (+ PO*indentation 11))
		   (CASE OPERATION.TYPE
			 ;; in any case PO=CLAUSE is called first for the resulting clause
			 (RESOLUTION             (PO=RESOLUTION             ARGUMENTS FILE
									    (incf po*rescounter)))
			 (PARAMODULATION         (PO=PARAMODULATION         ARGUMENTS FILE
									    (incf po*paracounter)))
			 (FACTORIZATION          (PO=FACTORIZATION          ARGUMENTS FILE
									    (incf po*faccounter)))
			 (instantiate            (PO=instantiate            ARGUMENTS FILE))
			 (REPLACEMENT.OPERATION  (PO=REPLACEMENT.OPERATION  ARGUMENTS FILE))
			 (DOUBLE.LITERAL         (PO=DOUBLE.LITERAL         ARGUMENTS FILE))
			 (REWRITE                (PO=REWRITE                ARGUMENTS FILE
									    (incf po*paracounter)))
			 (REWRITE.SYMMETRY       (PO=REWRITE.SYMMETRY       ARGUMENTS FILE))
			 (REPLACEMENT.RESOLUTION (PO=REPLACEMENT.RESOLUTION ARGUMENTS FILE))
			 (OTHERWISE              (ERROR "Non-existing operation type: ~S"
							OPERATION.TYPE)))
		   (setq PO*INDENTATION (- PO*indentation 11)))))

(defun po=prover (file)
  (format FILE ")~%~4T(prover mkrp ")
  (PO=OPTIONS FILE)
  (princ "))" FILE))

(DEFUN PO-REFUTATION.END (SPLITPART.IDENTIFIER RESULT)
  (declare (edited  "06-MAY-1993 12:39")
	   (authors KROENER)
	   (input   "")
	   (effect  "prints '(prover <prover-name> <options>")
	   (value   "undefined")
	   (ignore SPLITPART.IDENTIFIER RESULT))  
  (when PO*FILE (po=prover po*file)))

(DEFUN Po-SPLITPARTS.END () nil)

(DEFUN PO=OPTIONS (FILE)
  (when FILE
    (format file "~%~8T(options~{ ~A~%~8T~})" (OPT-GET.LIST.OPTIONS))))

(DEFUN PO-OPTIONS ())

(defun po=print.resolution (link uni clause file counter args)
  (format FILE "~%~8T(resolution step-~D " counter)
  (if args
      (PO-PART.CLAUSE args FILE)
    (PO=CLAUSE clause FILE nil))
  (PO=LINK.PARENTS link FILE)
  (po=print.subst uni file)
  (princ ")" FILE))

(DEFUN PO=RESOLUTION (LINK.UNI.CLAUSE FILE counter &REST args)
  
  
  (declare (edited  "06-MAY-1993 12:15")
	   (authors KROENER MIKO)
	   (input   "a list (link unifier clause), a file and a counter")
	   (effect  "prints the information to FILE")
	   (value   "undefined"))

  (when FILE
	(let ((link (first link.uni.clause))
	      (uni (second link.uni.clause))
	      (clause (third link.uni.clause)))
	  (po=print.resolution link uni clause file counter args))))


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
  (po=print.position pospar (1- poslitno) nil negpar (1- neglitno) nil file))

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
	      (clause (third link.uni.clause)))	  
	  (if (member (ds-link.colour link) (DS-LINK.COLOURS.FOR 'autolinks))
	      (po=i.paramodulation link uni clause file counter)
	    (po=e.paramodulation link uni clause file counter)))))

(defun po=print.instance (clause newcounter file)
  (let ((uni (ds-clause.renaming clause)))
    (when file	
	  (format FILE "~%~8T(instance step-~D ~%~10T" newcounter)
	  (format file "(clause c~A-~A ("  (dt-pname clause) newcounter)
	  (MAPC #'(LAMBDA (VAR)		; Variables and sorts
			  (format file "(~A ~A)" (dt-pname var) (DT-VARIABLE.SORT VAR)))
		(uni-apply.substitution (ds-clause.renaming clause)
					(ds-clause.variables clause)))
	  (princ ")" FILE)
	  (let ((indices nil))
	    (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
		     (push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
		     (po=print.lit (DS-CLAUSE.SIGN CLAUSE (1+ LITNO))
				   (ds-clause.predicate  clause (1+ litno))
				   (uni-apply.substitution
				    (ds-clause.renaming clause)
				    (ds-clause.termlist clause (1+ litno)))
				   file)
		     (when nil (push (cons clause (nreverse indices)) po*indices)))
	    (PRINC ") " FILE))
	  (format file "(c~A)" (dt-pname clause))
	  (po=print.subst uni file)
	  (princ ")" FILE))))

(defun po=i.paramodulation (link uni clause file counter)
  (let ((newcounter (incf po*instcounter)))
    (po=print.instance (ds-link.negpar link) newcounter file)
    (format FILE "~%~8T(paramodulation step-~D ~%~10T" counter)
    (PO=CLAUSE clause FILE (DT-TERMLIST.VARIABLES (uni-unifier.codomain uni)))
    (format file "~%~10T")
    (PO=LINK.par.inst link newcounter FILE)
    (po=print.subst uni file)
    (princ ")" FILE)))

(defun po=e.paramodulation (link uni clause file counter)
  (format FILE "~%~8T(paramodulation step-~D ~%~10T" counter)
  (PO=CLAUSE clause FILE (DT-TERMLIST.VARIABLES (uni-unifier.codomain uni)))
  (format file "~%~10T")
  (PO=LINK.par link FILE)
  (po=print.subst uni file)
  (princ ")" FILE))


(DEFUN PO=LINK.PAR (LINK FILE)			
					; Edited:  08-APR-1992 20:07;
					; Authors: MKRP CL
					; input : link# and a file opened for output
					; effect: prints :  eqpar eqlitno eqfct par litno fct
					; value : undefined
  (po=print.position (ds-link.negpar link)
		     (- (DS-LINK.NEGLITNO LINK) 1)
		     (ds-link.negfct link)
		     (DS-LINK.POSPAR LINK)
		     (- (DS-LINK.POSLITNO LINK) 1)
		     (if (DT-TAF.IS.LEFT (ds-link.posfct link)) 'lr 'rl)
		     file))

(DEFUN PO=LINK.PAR.inst (LINK counter FILE)			
					; Edited:  08-APR-1992 20:07;
					; Authors: MKRP CL
					; input : link# and a file opened for output
					; effect: prints :  eqpar eqlitno eqfct par litno fct
					; value : undefined
  (format file "(c~A (position ~D~{ ~D~})) (c~A-~A (position ~D) ~A)"
	  (dt-pname (DS-LINK.NEGPAR LINK))
	  (- (DS-LINK.NEGLITNO LINK) 1)
	  (ds-link.negfct link)
	  (dt-pname (DS-LINK.POSPAR LINK)) counter
	  (- (DS-LINK.POSLITNO LINK) 1)
	  (if (DT-TAF.IS.LEFT (ds-link.posfct link)) 'lr 'rl)))



(defun po=print.vars (vars file)
  (MAPC #'(LAMBDA (VAR)			; Variables and sorts
		  (format file "(~A ~A)" (dt-pname var) (DT-VARIABLE.SORT VAR)))
	vars))

(defun po=print.subst (subst file)
  (format file "~%~10T(substitution ~A ~A)"
	  (dt-pname (uni-unifier.domain subst))
	  (dt-pname (uni-unifier.codomain subst))))

(defun po=print.lit (sign pred tl file)
  (format file " ~A"   (list (if (ds-sign.is.negative sign) '- '+) (dt-pname (cons pred tl)))))

(defmacro po=print.clause.frame (clause vars file &rest body)
  `(progn  (format file "(clause c~A (" (dt-pname ,clause))
	   (po=print.vars ,vars ,file)
	   (princ ")" ,FILE)
	   ,@body
	   (princ ")" ,FILE)))

(defun po=print.clause (clause variables additional.vars
			       tl.access used.litno file record.flag)
  (po=print.clause.frame clause
			 (union additional.vars variables)
			 file
			 (let ((indices nil))
			   (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
				    (unless (eq (1+ litno) used.litno)
					    (push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
					    (po=print.lit (ds-clause.sign clause (1+ litno))
							  (ds-clause.predicate  clause (1+ litno))
							  (funcall tl.access litno)
							  file)))
			   (when record.flag
				 (push (cons clause (nreverse indices)) po*indices)))))

(defun po=r.chain.first.res (file unifier clause first links)
  (let ((lits nil))
    (po=print.resolution.frame (incf po*rescounter) file
			       (po=print.clause.frame clause
						      (DT-TERMLIST.VARIABLES unifier)
						      file
						      (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
							       (unless (= (1+ litno) first) 
								       (setq lits (nconc1 lits (cons clause (1+ litno))))
								       (po=print.lit (ds-clause.sign clause (1+ litno))
										     (ds-clause.predicate  clause (1+ litno))
										     (uni-apply.substitution
										      unifier
										      (ds-clause.termlist clause (1+ litno)))
										     file))))
			       (PO=print.PARENTS clause first (DS-LINK.OTHERPAR (CAR LINKS) CLAUSE)
						 (ds-link.otherlitno (car links) clause first) file)
			       (po=print.subst unifier file))
    lits))

(defun po=r.chain.rest.res (resolvent clause lits rlinks file)
  (po=replacement.operation;; a list of two elements (CLAUSE DESC)
   ;; where DESC is a list (UNIFIER CLAUSES RESOLUTIONS MULTIPLES).
   (list (cons resolvent lits)
	 (list nil
	       (mapcar #'(lambda (link) (DS-LINK.OTHERPAR LINK CLAUSE))
		       rlinks)
	       (mapcar #'(lambda (link)
			   (let ((litno (DS-LINK.THISLITNO LINK CLAUSE)))
			     (list (cons clause litno)
				   (cons (DS-LINK.OTHERPAR LINK CLAUSE)
					 (DS-LINK.OTHERLITNO LINK CLAUSE litno))
				   (DS-LINK.RULE LINK))))
		       rlinks)
	       (mapcar #'(lambda (link) (declare (ignore link)) nil)
		       rlinks)
	       nil))
   file))

(DEFUN PO=R.CHAIN (ARGUMENTS FILE)	; EDITED:  5-SEP-84 16:23:08
					; INPUT:  A list (RESOLVENT TERMLISTS CHAIN-ELEMENT)
					;         RESOLVENT is the new clause,
					;         CHAIN-ELEMENT is a list
					;         (UNIFIER CLAUSE LINKS POINTERS LITNOS)
					;         where clause is the parentclause of
					;         RESOLVENT (which is created by simultaneus
					;         resolution upon LINKS with 'UNIFIER')
					;         POINTERS and LITNOS is irrelevant.
					;         TERMLISTS is the list of termlists of
					;         the intermediate clause which would be
					;         created by resolution upon the first element
					;         of LINKS. This is the protocol information
					;         of the TERMINATOR module.
					; EFFECT: This operation is protocolled as a
					;         resolution upon the first element of links
					;         followed by a replacement resolution step.
					; VALUE:  Undefined.
  (let* ((RESOLVENT (CAR ARGUMENTS))
	 (TERMLISTS (SECOND ARGUMENTS))
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
	  (R (setq lits (po=r.chain.first.res file unifier clause first links)))
	  (si (error "not yet impl")))
    (MAPC #'(lambda (LINK)		; Generate double literals
	      (cond ((EQL 'R (DS-LINK.COLOUR LINK)) (SETQ RLINKS (NCONC1 RLINKS LINK)))
		    (T (SETQ DOUBLE.LITS
			     (NCONC1 DOUBLE.LITS
				     (LIST (CONS RESOLVENT
						 (COND ((< (DS-LINK.NEGLITNO LINK) FIRST)
							(DS-LINK.NEGLITNO LINK))
						       (T (1- (DS-LINK.NEGLITNO LINK)))))
					   (CONS RESOLVENT
						 (COND ((< (DS-LINK.POSLITNO LINK) FIRST)
							(DS-LINK.POSLITNO LINK))
						       (T (1- (DS-LINK.POSLITNO LINK)))))
					   (DS-LINK.RULE LINK)))))))
	  (CDR LINKS))
    (po=r.chain.rest.res resolvent clause lits rlinks file)))



(DEFUN PO=REPLACEMENT.RESOLUTION (ARGUMENTS FILE)
  (declare (ignore ARGUMENTS FILE)))

(defun po=print.op.header (op counter file)
  (format FILE "~%~8T(~A step-~D " op counter))  

(defmacro po=print.factorization.frame (counter file cl.expr par.expr subst.expr)
  `(progn (po=print.op.header "factoring" ,counter ,file)
	  ,cl.expr
	  ,par.expr
	  ,subst.expr
	  (princ ")" ,FILE)))


(DEFUN PO=FACTORIZATION (LINK.UNI.CLAUSE FILE counter)
  (declare (edited  "23-JUL-1993 19:36")
	   (authors KROENER PRCKLN)
	   (input   "a list (link unifier clause) and a file")
	   (effect  "prints a factorization step in post syntax")
	   (value   "undefined"))
  (let ((link (first link.uni.clause))
	(uni (second link.uni.clause))
	(clause (third link.uni.clause)))
    (po=print.factorization.frame counter
				  file
				  (PO=CLAUSE clause FILE nil)
				  (PO=LINK.FAC link FILE)
				  (po=print.subst uni file))))

(DEFUN PO=instantiate (UNI.CLAUSE.old FILE)
  (declare (ignore UNI.CLAUSE.old FILE)))

(DEFUN PO=LINK.FAC (LINK FILE)		; edited:  3-jul-84 11:01:00  by cl
					; input : link# and a file opened for output
					; effect: prints par
					; value : undefined
  (format file "(c~A (position ~D) (position ~D))"
	  (dt-pname (DS-LINK.POSPAR LINK))
	  (1- (ds-link.poslitno link))
	  (1- (ds-link.neglitno link))))

(defun po=get.all.lits (clause)
  (let ((lits nil))
    (dotimes (litno (ds-clause.nolit clause))
	     (push (cons clause (1+ litno)) lits))
    (nreverse lits)))

(defun po=sim.resolution (res lits)
  (let ((lit1 (first res))
	(lit2 (second res)))
    (nconc (delete lit1 lits :test #'equal)
	   (delete lit2 (po=get.all.lits (first lit2)) :test #'equal))))

(defun po=sim.factoring (fac lits)
  (let ((lit1 (first fac))
	(lit2 (second fac)))
    (delete lit2 lits :test #'equal)))

(defmacro po=print.resolution.frame (counter file cl.expr par.expr subst.expr)
  `(progn (format ,FILE "~%~8T(resolution step-~D " ,counter)
	  ,cl.expr
	  ,par.expr
	  ,subst.expr
	  (princ ")" ,FILE)))

(defun po=search.act.litno (lit lits)
  (cond ((null lits) (error "~A not found in virtual list of lits" lit))
	((equal lit (first lits)) 1)
	(t (1+ (po=search.act.litno lit (rest lits))))))

(DEFUN PO=REPLACEMENT.OPERATION (CLAUSE.DESCRIPTION FILE)  
  (declare (edited  "24-Jul-1993 20:46")
	   (authors PRCKLN)
	   (input   "CLAUSE.DESCRIPTION is a list of two elements, a CLAUSE"
		    "and a list (UNIFIER CLAUSES RESOLUTIONS MULTIPLES)"
		    "and a file open for output.")
	   (effect  "prints resolution and factoring-steps in post syntax" )
	   (value   "Undefined"))
  (let* ((resolvent (first CLAUSE.DESCRIPTION))
	 (unifier (first (second CLAUSE.DESCRIPTION)))
	 (clauses (second (second CLAUSE.DESCRIPTION)))
	 (resolutions (third (second CLAUSE.DESCRIPTION)))
	 (multiples (fourth (second CLAUSE.DESCRIPTION)))
	 (VARS.SORTS.OF.CODOMAIN (fifth (second CLAUSE.DESCRIPTION)))
	 act.lits)
    (if (consp resolvent)
	(setq act.lits (rest resolvent) resolvent (first resolvent))
      (setq act.lits (po=get.all.lits resolvent)))    
    (if resolutions
	(mapc #'(lambda (resolution multiple)
		  (let ((act.litno (po=search.act.litno (first resolution) act.lits)))
		    (setq act.lits (po=sim.resolution resolution act.lits))
		    (po=print.resolution.frame (incf po*rescounter)
					       file
					       (po=print.clause.frame resolvent
								      (ds-clause.variables resolvent) file
								      (mapc #'(lambda (lit)
										(let ((cl (first lit))
										      (ln (rest lit)))
										  (po=print.lit (ds-clause.sign cl ln)
												(ds-clause.predicate cl ln)
												(ds-clause.termlist cl ln)
												file)))
									    act.lits))
					       (PO=print.PARENTS (first (first resolution)) act.litno
								 (first (second resolution)) (rest (second resolution))
								 file)
					       (po=print.subst unifier file))))
	      resolutions
	      multiples)
      (mapc #'(lambda (multiple)		
		(let ((act.litno (po=search.act.litno (first multiple) act.lits)))
		  (setq act.lits (po=sim.factoring multiple act.lits))
		  (po=print.factorization.frame (incf po*faccounter) file
						(po=print.clause.frame resolvent
								       (ds-clause.variables resolvent) file
								       (mapc #'(lambda (lit)
										 (let ((cl (first lit))
										       (ln (rest lit)))
										   (po=print.lit (ds-clause.sign cl ln)
												 (ds-clause.predicate cl ln)
												 (ds-clause.termlist cl ln)
												 file)))
									     act.lits))
						(PO=print.PARENTS (first (first multiple)) act.litno
								  (first (second multiple)) (rest (second multiple))
								  file)
						(po=print.subst unifier file))))
	    (first multiples)))))

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
  (declare (ignore CLAUSE.REMAININGLITNO.DELETEDLITNO.RULE FILE)))

(defun po=compute.rw.match (match leftcopy rule)
  (let* ((left (dt-access (list (ds-clause.lit.rewrite.rule rule 1)) (ds-clause.termlist rule 1)))
	 (copymatch (first (uni-unify1.terms leftcopy left t))))
    (UNI-apply.substitution copymatch match)))

(defun po=print.position (clause1 litno1 taf1 clause2 litno2 taf2 file)
  (format file
	  "~%~10T(c~A (position ~D~{ ~D~})) (c~A (position ~D) ~A)"
	  (dt-pname clause1) litno1 taf1 (dt-pname clause2) litno2 taf2))

(DEFUN PO=REWRITE (RULE.CLAUSE.LITNO.pos FILE counter)
  (when file
	(let* ((rule (first rule.clause.litno.pos))
	       (clause (second rule.clause.litno.pos))
	       (litno (third rule.clause.litno.pos))
	       (pos (first (fourth rule.clause.litno.pos)))
	       (match (second (fourth rule.clause.litno.pos)))
	       (leftcopy (third (fourth rule.clause.litno.pos))))
	  (setq match (po=compute.rw.match match leftcopy rule))
	  (format FILE "~%~8T(paramodulation step-~D~%~10T" counter)
	  (PO=CLAUSE clause FILE (DT-TERMLIST.VARIABLES (uni-unifier.codomain match)))
	  (po=print.position (dt-pname clause)
			     (1- litno)
			     pos
			     (dt-pname rule)
			     0
			     (if (dt-taf.is.left (list (ds-clause.lit.rewrite.rule rule 1))) 'lr 'rl)
			     file)
	  (po=print.subst match file)
	  (princ ")" FILE))))


(DEFUN PO=REWRITE.SYMMETRY (CLAUSE FILE)
  (declare (ignore CLAUSE FILE)))


(DEFUN PO=CLAUSE (CLAUSE FILE vars &optional record.flag)  
  (declare (edited  "23-JUL-1993 19:40")
	   (authors KROENER PRCKLN)
	   (input   "a clause and a filename")
	   (effect  "prints the clause")
	   (value   "undefined"))
  (po=print.clause clause (DS-CLAUSE.VARIABLES CLAUSE) vars
		   #'(lambda (litno) (ds-clause.termlist clause (1+ litno)))
		   nil file record.flag))


(DEFUN PO=LINK.COLOURS (FILE) (declare (ignore file)))



(DEFUN po-INFIX.FORM (AXIOMS.INFIX THEOREMS.INFIX)
  (declare (ignore AXIOMS.INFIX THEOREMS.INFIX)))
