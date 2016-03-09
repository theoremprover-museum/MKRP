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
;;; Everyone is granted permission to copy, modify and redle.clauistribute
;;; MKRP, but only if the it is not used for military purposes or any
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
(defvar PO*INDENTATION 0 "an integer, describing the number of blanks needed at the beginning of a line for formatted output")


(defun PO-INIT.COUNTERS ()
  
  (declare (edited  "06-MAY-1993 12:30")
	   (authors KROENER)
	   (input   "")
	   (effect  "resets the counters for resolution, factoring and paramodulation")
	   (value   "undefined"))

  (setq po*rescounter -1)
  (setq po*faccounter -1)
  (setq po*paracounter -1)
  (setq po*instcounter -1))
 

(defun PO-PCONSTANTS (CONSTANTS)

  (declare (edited  "19-APR-1993 17:58")
	   (authors KROENER)
	   (input   "list of constants")
	   (effect  "prints name and sort")
	   (value   "undefined"))

  (when PO*FILE
    (cond ((consp CONSTANTS)
	   (format PO*FILE " (~A ~A)" (first (DT-PNAME CONSTANTS)) (DT-CONSTANT.SORT (first CONSTANTS)))
	   (PO-PCONSTANTS (rest CONSTANTS))))))


(defun PO-PPREDICATES (predicates)

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
	       (PO-PPREDICATES (rest predicates)))))))


#+comment(defun PO-PFUNCTIONS (functions)

  (declare (edited  "19-APR-1993 17:58")
	   (authors KROENER)
	   (input   "list of functions")
	   (effect  "prints name and sorts")
	   (value   "undefined"))
  
  (when PO*FILE
    (cond ((consp functions)
	   (format PO*FILE " (~A (~A ~{~A~}))"
		     (dt-function.PNAME (first functions))
		     (dt-function.max.range.sort (first functions))
		     (reverse (dt-function.domainsorts (first functions))))
	   (PO-PFUNCTIONS (rest functions))))))

(defun PO-PFUNCTIONS (functions)

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
	   (PO-PFUNCTIONS (rest functions))))))



(defun PO-CONSTRUCT.START (FILE comment)
  
  (declare (edited  "19-APR-1993 18:06")
	   (authors KROENER)
	   (input   "code file")
	   (effect  "prints problem, <name>, <status>, parts of <problem>")
	   (value   "undefined"))
  
  (PO-INIT.COUNTERS)
  (when (SETQ PO*FILE FILE)
    (setq po*indices nil)
    (format PO*FILE "~%(problem ~A proved" comment)
    (format PO*FILE "~%~4T((type-constants")
    (mapcar #'(lambda (constant)
		(format PO*FILE " ~A" constant))
	    (DT-SORT.ALL))
    (PRINC ")" PO*FILE)
    (format  PO*FILE "~%~4T(constants")
    (PO-PCONSTANTS (DT-CONSTANT.ALL))
    (PO-PPREDICATES (DT-PREDICATE.ALL))
    (PO-PFUNCTIONS  (DT-FUNCTION.ALL))
    (princ ")" PO*FILE)))

(defun po-modify-quantor (element)
  
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


(defun po-treat-formula (formula)
  
  (declare (edited  "29-APR-1993 20:11")
	   (authors KROENER)
	   (input   "formula (quantifier variable rest)")
	   (effect  "changes mkrp-formula to post-formula")
	   (value   "formula (quantifier (lam (variable-declaration) rest))"))
  
  (if (listp formula)
      (let ((foo (po-modify-quantor (first formula))))
	(if (member foo '(forall exists))
	    (let ((full-var (list (second formula) (DT-VARIABLE.SORT (second formula))))
		  (body (third formula)))
	      (list foo (list 'lam full-var (po-treat-formula body))))
	  (if (listp formula)
	      (po-modify-pred formula))) )))

(defun po-axioms.start ()
  (when PO*FILE
	(format PO*FILE "~%~4T(resolution-proof~%~8T(cnf (")))

(DEFUN Po-AXIOMS.END (RESULT) (declare (ignore result)))

(defun po-theorems.start (SPLITPART.IDENTIFIER) (declare (ignore SPLITPART.IDENTIFIER)))

(DEFUN po-theoremS.END (RESULT)
  (declare (ignore result))
  (when PO*FILE
	(format po*FILE ")~%~12T(delta-relation)")
	(PRINC ")" Po*FILE)))

(DEFUN Po-INITIAL.GRAPH ())

(DEFun Po-CONSTRUCT.END ())

(DEFUN Po-SPLITPARTS.START (FILE SYSTEM.VERSION COMMENT)
  (declare (ignore FILE SYSTEM.VERSION COMMENT)))

(defun PO-PAXIOMS (axioms.all counter)
  
  (declare (edited  "21-APR-1993 17:58")
	   (authors KROENER)
	   (input  "list of axioms")
	   (effect "prints assumption ass1 ...")
	   (value  "undefined"))

  (WHEN PO*FILE
	(let* ((axioms1 (remove '+ axioms.all))
	       (axioms (remove nil axioms1)))
	  (mapcar #'(lambda (axiom)
		      (if (NOT (equal axiom '(0)))
			  (format PO*FILE "~%~4T(assumption ass~D ~A)"
				  (incf counter) (dt-pname (po-treat-formula axiom)))))
		  axioms))))


(DEFUN PO-PTHEOREMS (theorems.all counter)
  
  (declare (edited  "21-APR-1993 17:58")
	   (authors KROENER)
	   (input  "list of theorems, normaly 1 theorem")
	   (effect "prints conclusion  th1 ...")
	   (value  "undefined"))
  (let* ((theorems1 (remove '+ theorems.all))
	 (theorems (remove nil theorems1)))
    (WHEN PO*FILE
	  (mapcar #'(lambda (theorem)
		      (if (NOT (equal theorem '(0)))
			  (format PO*FILE "~%~4T(conclusion th~D ~A)"
				  (incf counter)
				  (dt-pname (po-treat-formula theorem)))))
		  theorems))))


(DEFUN PO-PREFIX.FORM (AXIOMS THEOREMS)
  
  (declare (edited  "19-APR-1993 18:21")
	   (authors KROENER)
	   (input  "two lists")
	   (effect "prints assumptions and conclusions")
	   (value  "undefined"))
  
  (WHEN PO*FILE
	(PO-PAXIOMS AXIOMS 0)
	(PO-PTHEOREMS THEOREMS 0)
	(PRINC ")" PO*FILE)))


(DEFUN PO-REFUTATION.START ())


(DEFUN PO-PCLAUSE (CLAUSE FILE &optional record.flag)
  
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
  
  (declare (edited  "02-MAY-1993 21:29")
	   (authors KROENER)
	   (input   "list of clauses")
	   (effect  "prints first part of the <proof-result>")
	   (value   "undefined"))

  (when (AND PO*FILE PARTIAL.CLAUSE.LIST)
    (setq PO*INDENTATION (+ PO*indentation 9))
    (MAPC (FUNCTION (LAMBDA (CLAUSE) (PO-PCLAUSE CLAUSE PO*FILE nil))) PARTIAL.CLAUSE.LIST)
    (setq PO*INDENTATION (- PO*indentation 9))))



(DEFUN Po-OPERATION (OPERATION.TYPE ARGUMENTS)

  
  (declare (edited  "03-JUN-1993 19:19")
	   (authors PRCKLN KROENER)
	   (input   "an atom, and arguments according to the type of operation")
	   (effect  "")
	   (value   "undefined"))

					; Edited:  02-APO-1992 20:58
					; Authors: PRCKLN
					; input : an atom, and arguments according to the type of operation
					; effect: prints <OPERATION <CLAUSE  ..see PO=CLAUSE..>
					;                           <op.type ..see PO=op.type..>
					; value : undefined
  (when PO*FILE
	(CASE OPERATION.TYPE
	      (R.CHAIN   (PO=R.CHAIN ARGUMENTS PO*FILE))
	      (OTHERWISE (setq PO*INDENTATION (+ PO*indentation 11))
			 (CASE OPERATION.TYPE
			       ;; in any case PO=CLAUSE is called first for the resulting clause
			       (RESOLUTION             (PO=RESOLUTION             ARGUMENTS PO*FILE
										  (incf po*rescounter)))
			       (PARAMODULATION         (PO=PARAMODULATION         ARGUMENTS PO*FILE
										  (incf po*paracounter)))
			       (FACTORIZATION          (PO=FACTORIZATION          ARGUMENTS PO*FILE
										  (incf po*faccounter)))
			       (instantiate            (PO=instantiate            ARGUMENTS PO*FILE))
			       (REPLACEMENT.OPERATION  (PO=REPLACEMENT.OPERATION  ARGUMENTS PO*FILE
										  (incf po*rescounter)))
			       (DOUBLE.LITERAL         (PO=DOUBLE.LITERAL         ARGUMENTS PO*FILE))
			       (REWRITE                (PO=REWRITE                ARGUMENTS PO*FILE
										  (incf po*paracounter)))
			       (REWRITE.SYMMETRY       (PO=REWRITE.SYMMETRY       ARGUMENTS PO*FILE))
			       (REPLACEMENT.RESOLUTION (PO=REPLACEMENT.RESOLUTION ARGUMENTS PO*FILE))
			       (OTHERWISE              (ERROR "Non-existing operation type: ~S"
							      OPERATION.TYPE)))
			 (setq PO*INDENTATION (- PO*indentation 11))))))

(DEFUN PO-REFUTATION.END (SPLITPART.IDENTIFIER RESULT)
  
  (declare (edited  "06-MAY-1993 12:39")
	   (authors KROENER)
	   (input   "")
	   (effect  "prints '(prover <prover-name> <options>")
	   (value   "undefined")
	   (ignore SPLITPART.IDENTIFIER RESULT))
  
  (when PO*FILE
    (format PO*FILE ")~%~4T(prover mkrp ")
    (PO=OPTIONS PO*FILE)
    (princ "))" PO*FILE)))

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
  (format file "~%~10T(c~A (position ~D)) (c~A (position ~D)) "
	  (dt-pname pospar)
	  (- poslitno 1) 
	  (dt-pname negpar)
	  (- neglitno 1)))

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
	  (PROGN (MAPC #'(LAMBDA (VAR)	; Variables and sorts
				 (format file "(~A ~A)" (dt-pname var) (DT-VARIABLE.SORT VAR)))
		       (union nil(uni-apply.substitution uni
							 (DS-CLAUSE.VARIABLES CLAUSE)))))
	  (princ ")" FILE)
	  (let ((indices nil))
	    (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
		     (push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
		     (let ((pname (dt-pname (cons (ds-clause.predicate  clause (1+ litno))
						  (uni-apply.substitution
						   (ds-clause.renaming clause)
						   (ds-clause.termlist clause (1+ litno)))))))
		       (if (ds-sign.is.negative (DS-CLAUSE.SIGN CLAUSE (1+ LITNO)))
			   (setq pname (list '- pname))
			 (setq pname (list '+ pname)))
		       (format file " ~A" pname))
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
  (when file
    (format file "(c~A (position ~D~{ ~D~})) (c~A (position ~D) ~A)"
	    (dt-pname (DS-LINK.NEGPAR LINK))
	    (- (DS-LINK.NEGLITNO LINK) 1)
	    (mapcar #'1- (ds-link.negfct link))
	    (dt-pname (DS-LINK.POSPAR LINK))
	    (- (DS-LINK.POSLITNO LINK) 1)
	    (if (DT-TAF.IS.LEFT (ds-link.posfct link)) 'lr 'rl))))

(DEFUN PO=LINK.PAR.inst (LINK counter FILE)			
					; Edited:  08-APR-1992 20:07;
					; Authors: MKRP CL
					; input : link# and a file opened for output
					; effect: prints :  eqpar eqlitno eqfct par litno fct
					; value : undefined
  (when file
    (format file "(c~A (position ~D~{ ~D~})) (c~A-~A (position ~D) ~A)"
	    (dt-pname (DS-LINK.NEGPAR LINK))
	    (- (DS-LINK.NEGLITNO LINK) 1)
	    (mapcar #'1- (ds-link.negfct link))
	    (dt-pname (DS-LINK.POSPAR LINK)) counter
	    (- (DS-LINK.POSLITNO LINK) 1)
	    (if (DT-TAF.IS.LEFT (ds-link.posfct link)) 'lr 'rl))))



(defun po=print.vars (vars file)
  (MAPC #'(LAMBDA (VAR)			; Variables and sorts
		  (format file "(~A ~A)" (dt-pname var) (DT-VARIABLE.SORT VAR)))
	vars))

(defun po=print.subst (subst file)
  (format file "~%~10T(substitution ~A ~A)"
	  (dt-pname (uni-unifier.domain subst))
	  (dt-pname (uni-unifier.codomain subst))))

(defun po=print.clause (clause variables additional.vars
			       tl.access used.litno file record.flag)
  (format file "(clause c~A (" (dt-pname clause))
  (po=print.vars (union additional.vars variables) file)
  (princ ")" FILE)
  (let ((indices nil))
    (dotimes (litno (DS-CLAUSE.NOLIT CLAUSE))
	     (unless (eq (1+ litno) used.litno)
		     (push (ds-clause.lit.getprop clause (1+ litno) 'index) indices)
		     (let ((pname (dt-pname (cons (ds-clause.predicate  clause (1+ litno))
						  (funcall tl.access litno)))))
		       (if (ds-sign.is.negative (DS-CLAUSE.SIGN CLAUSE (1+ LITNO)))
			   (setq pname (list '- pname))
			 (setq pname (list '+ pname)))
		       (format file " ~A" pname))
		     (when record.flag (push (cons clause (nreverse indices)) po*indices))))
    (PRINC ") " FILE)))

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
	 (LITNO 0)
	 (COLOUR (DS-LINK.COLOUR (CAR LINKS)))
	 (FIRST  (CASE COLOUR
		       (R  (DS-LINK.THISLITNO (CAR LINKS) CLAUSE))
		       (SI (DS-LINK.POSLITNO  (CAR LINKS)))
		       (OTHERWISE (ERROR "ILLEGAL COLOUR IN PO=R.CHAIN: ~A" COLOUR))))
	 RLINKS
	 DOUBLE.LITS
	 counter)
    (CASE COLOUR
	  (R (setq counter (incf po*rescounter))
	     (format FILE "~%~8T(resolution step-~D " counter)
	     (po=print.clause clause (DT-TERMLIST.VARIABLES TERMLISTS) nil
			      #'(lambda (litno) (declare (ignore litno)) (pop termlists))
			      first file nil)
	     (PO=print.PARENTS clause first (DS-LINK.OTHERPAR (CAR LINKS) CLAUSE)
			       (ds-link.otherlitno (car links) clause first) file)
	     (po=print.subst unifier file)
	     (princ ")" FILE))
	  (si (error "not yet impl")))
    (MAPC #'(LAMBDA (LINK)   ;; Generate double literals
	      (COND ((EQL 'R (DS-LINK.COLOUR LINK)) (SETQ RLINKS (NCONC1 RLINKS LINK)))
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
					   (DS-LINK.RULE LINK))))))))
     (CDR LINKS))
  (po=replacement.operation
   ;; a list of two elements (CLAUSE DESC)
   ;; where DESC is a list (UNIFIER CLAUSES RESOLUTIONS MULTIPLES).
   (list resolvent
	 (list nil
	       (mapcar #'(lambda (link) (DS-LINK.OTHERPAR LINK CLAUSE))
		       rlinks)
	       (mapcar #'(lambda (link)
			   (list (cons resolvent
				       (let ((litno (DS-LINK.THISLITNO LINK CLAUSE)))
					 (if (< litno first) litno (1- litno))))
				 (cons (DS-LINK.OTHERPAR LINK CLAUSE)
				       (DS-LINK.OTHERLITNO LINK CLAUSE litno))
				 (DS-LINK.RULE LINK)))
		       rlinks)
	       (mapcar #'(lambda (link) (declare (ignore link)) nil)
		       rlinks)
	       nil))
   file))



(DEFUN PO=REPLACEMENT.RESOLUTION (ARGUMENTS FILE)
  (declare (ignore ARGUMENTS FILE)))


(DEFUN PO=FACTORIZATION (LINK.UNI.CLAUSE FILE counter)

  (declare (edited  "03-JUN-1993 19:36")
	   (authors KROENER)
	   (input   "a list (link unifier clause) and a file")
	   (effect  "prints a factorization step in post syntax")
	   (value   "undefined"))


  (when file
    (let ((link (first link.uni.clause))
	  (uni (second link.uni.clause))
	  (clause (third link.uni.clause)))
      (format FILE "~%~8T(factoring step-~D " counter)
      (PO=CLAUSE clause FILE nil)
      (PO=LINK.FAC link FILE)
      (po=print.subst uni file))
    (princ ")" FILE)))

(DEFUN PO=instantiate (UNI.CLAUSE.old FILE)
  (declare (ignore UNI.CLAUSE.old FILE)))


(DEFUN PO=LINK.FAC (LINK FILE)		; edited:  3-jul-84 11:01:00  by cl
					; input : link# and a file opened for output
					; effect: prints par
					; value : undefined
  (when file
    (format file "(~A ~D ~D)"
	    (DS-LINK.POSPAR LINK)
	    (ds-link.poslitno link)
	    (ds-link.neglitno link))))

(defun po=get.all.lits (clause)
  (let ((lits nil))
    (dotimes (litno (ds-clause.nolit clause))
	     (push (cons clause (1+ litno)) lits))))


(DEFUN PO=REPLACEMENT.OPERATION (CLAUSE.DESCRIPTION FILE)
					; edited: 24-mar-84 20:00:16  by cl
					; input : a list of two elements, an integer (the clause)
					;         and a list (unifier clauses resolutions multiples),
					;         and a file open for output.
					; effect: prints the info to the code file
					; value : undefined(link.uni.clause  FILE counter)

  
  (declare (edited  "24-Jul-1993 20:46")
	   (authors PRCKLN)
	   (input   "CLAUSE.DESCRIPTION is a list of two elements, a CLAUSE"
		    "and a list (UNIFIER CLAUSES RESOLUTIONS MULTIPLES)"
		    "and a file open for output.")
	   (effect  "prints resolution and factoring-steps in post syntax" )
	   (value   "Undefined"))
  (let ((resolvent (first CLAUSE.DESCRIPTION))
	(unifier (first (second CLAUSE.DESCRIPTION)))
	(clauses (second (second CLAUSE.DESCRIPTION)))
	(resolutions (third (second CLAUSE.DESCRIPTION)))
	(multiples (fourth (second CLAUSE.DESCRIPTION)))
	(VARS.SORTS.OF.CODOMAIN (fifth (second CLAUSE.DESCRIPTION)))
	counter)
    (mapc #'(lambda (resolution multiple)
	      (setq counter (incf po*rescounter))
	      (format FILE "~%~8T(resolution step-~D " counter)
	      (po=print.clause clause (DT-TERMLIST.VARIABLES TERMLISTS) nil
			       #'(lambda (litno) (declare (ignore litno)) (pop termlists))
			       first file nil)
	      (PO=print.PARENTS clause first (DS-LINK.OTHERPAR (CAR LINKS) CLAUSE)
				(ds-link.otherlitno (car links) clause first) file)
	      (po=print.subst unifier file)
	      (princ ")" FILE))
	  resolutions
	  multiples)))

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
	  (format file "~%~10T(c~A (position ~D~{ ~D~})) (c~A (position ~D) ~A)"
		  (dt-pname clause)
		  (1- litno)
		  (mapcar #'1- pos)
		  (dt-pname rule)
		  0
		  (if (dt-taf.is.left (list (ds-clause.lit.rewrite.rule rule 1))) 'lr 'rl))
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