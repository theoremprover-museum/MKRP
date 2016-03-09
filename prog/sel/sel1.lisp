;;; -*- Package: mkrp; Syntax: common-lisp; Mode: lisp -*-
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

(DEFVAR SEL=MANUAL)

(DEFVAR SEL=FACTORIZE)

(DEFVAR SEL=REDUCTIONS)

(DEFVAR SEL=ELIMINATE)

(DEFVAR SEL=MERGING)

(DEFVAR SEL=INDUCTION)

(DEFVAR SEL=TERMINATOR)

(DEFVAR SEL=TERMINATOR.UNITS)

(DEFVAR SEL=STRATEGIES)

(DEFVAR SEL=CHAIN)

(DEFVAR SEL*OPERATION.STACK NIL)

(DEFVAR SEL*ACTUAL.OPERATION NIL)

(DEFVAR SEL*REDUCE.DEDUCE 'DEDUCE)

(DEFVAR SEL*INITIAL.FLAG NIL)

(DEFVAR SEL*FINAL.ACTIONS NIL)

(DEFparameter SEL*OPERATIONS
	      '(SEL=MANUAL SEL=FACTORIZE SEL=REDUCTIONS SEL=ELIMINATE SEL=MERGING 
			   SEL=TERMINATOR SEL=TERMINATOR.UNITS SEL=STRATEGIES))

(DEFVAR SEL*LINK.CLASSES
	'(((ACTIVE) . R) ((PASSIVE) . R) ((INHIBITED) . R) ((ACTIVE) . P) ((ACTIVE) . PIW)
	  ((PASSIVE) . P) ((INHIBITED) . P) ((REDUCTIONS))))

(DEFVAR SEL*CLAUSE.CLASSES
	'((SUPPORTED) (INDUCTION) (EQUIVALENCE) (EQUALITY) (IMPLICATION) (DEDUCTION-RULES) (ELIMINATING EQUATIONS)))

(DEFVAR SEL*CLAUSELISTS '(SEL*TERMINATOR.NEW.CLAUSES SEL*TERMINATOR.LOOK.NEW.CLAUSES))

(DEFVAR SEL*LINKLISTS NIL)

(DEFVAR SEL*TERMINATOR.NEW.CLAUSES NIL)


(DEFVAR SEL*REDUCTION.FLAG NIL)


(DEFUN SEL-INITIALIZE (SPLITPART.IDENTIFIER)	; edited:  25. 5. 1982   hjo
						; input:   a list of the theoremclauses of the actual
						;          graph.
						; effect:  initializes the selection module and
						;          determines the first operation code.
						; value:   the first deduction code itself.
  (DECLARE (IGNORE SPLITPART.IDENTIFIER))
  (CATCH 'SEL-INITIALIZE
    (let ((THEOREMCLAUSES (REMOVE-IF-NOT #'(LAMBDA (CLAUSE)
					     (EQL 'THEOREM (DS-CLAUSE.PARENTS CLAUSE)))
					 (CG-CLAUSES ALL))))
      (SETQ SEL*STR_PARAMODULATION.STRATEGY (OPT-GET.OPTION er_PARAMODULATION))
      (SETQ SEL*STEPCOUNTER 0)
      (SEL=CLEAR.LINK.CLASSES)
      (SEL=CLEAR.CLAUSE.CLASSES)
      (SETQ SEL*INITIAL.FLAG T)
      (SETQ SEL*FINAL.ACTIONS NIL)
      (SETQ SEL*REDUCE.DEDUCE 'DEDUCE)
      (SEL=NO.REFUTATION.POSSIBLE?)
      (SETQ SEL*TERMINATOR.PROVED.FLAG NIL)
      (SETQ SEL*TERMINATOR.LOOK NIL)
      (MAKEEMPTYSTACK SEL*OPERATION.STACK)
      (SETQ SEL*ACTUAL.DEDUCTION.CODE NIL)
      (SETQ SEL*ACTUAL.REDUCTION.CODE NIL)
      (SETQ SEL*REDUCTION.FLAG T)
      (SEL=REFUTATION.FOUND (CG-LINKS R ALL) (CG-LINKS P ALL))
      (SETQ SEL*ACTUAL.CLAUSES NIL)
      (SETQ SEL*END.OF.INDUCTION NIL)
      (SETQ SEL*IND.OLD.CLAUSES NIL)
      (SETQ SEL*IND.NEW.CLAUSES NIL)
      (SETQ SEL*IND.OLD.LINKS NIL)
      (SETQ SEL*IND.CHAIN NIL)
      (SETQ SEL*IND.OPTIONS (LIST 0 '(IND.EQUIVALENCE IND.EQUALITY IND.INDUCTION) NIL 0 (SEL=CLAUSES SUPPORTED)))
      (SETQ SEL*IND.CASE.COUNTER 0) (SETQ SEL*IND.SINGLE.LINK NIL)
      (MAPC #'(LAMBDA (OPERATION) (SETF (SYMBOL-VALUE OPERATION) NIL) (REMPROPS OPERATION)) SEL*OPERATIONS)
      (SETQ SEL=CHAIN NIL)
      (REMPROPS 'SEL=CHAIN)
      ;; the value cells of the function names will be used as an initialization flag and the propertylists may
      ;;  be used as global values.
      (SETF (GET 'SEL=TERMINATOR 'SEL*TRY.FULL) T) (SEL=CLASSIFY.CLAUSES THEOREMCLAUSES)
      (SETF (GET 'SEL=STRATEGIES 'SEL*STATE) nil)
      (MAPC #'(LAMBDA (PLINK)
		(COND ((OR (DS-LINK.IS.MARKED INHIBITED PLINK)
			   #|(and (not (opt-is.completion))
				(DT-VARIABLE.IS
				  (DT-ACCESS (DS-LINK.POSFCT PLINK)
					     (DS-CLAUSE.TERMLIST (DS-LINK.POSPAR PLINK) (DS-LINK.POSLITNO PLINK)))))|#
			   (DT-VARIABLE.IS
			     (DT-ACCESS (DS-LINK.NEGFCT PLINK)
					(DS-CLAUSE.TERMLIST (DS-LINK.NEGPAR PLINK) (DS-LINK.NEGLITNO PLINK)))))
		       (SEL=INHIBIT PLINK))))
	    (CG-LINKS P ALL))
      (MAPC #'(LAMBDA (LINK) (COND ((DS-LINK.IS.MARKED INHIBITED LINK) (SEL=INHIBIT LINK))))
	    (CG-LINKS R ALL))
      (SEL=LINK.&.TERM.DEPTH.TREATMENT (CG-LINKS R ALL) (CG-LINKS P ALL))
      (MAPC #'(LAMBDA (OPERATION)
		(FUNCALL (SYMBOL-FUNCTION (INTERN (CONCATENATE 'STRING (SYMBOL-NAME OPERATION) ".UPDATE")
						  (find-package "MKRP")))
			 T))
	    SEL*OPERATIONS)
      (SETQ SEL*ACTUAL.OPERATION (SEL=NEXT.OPERATION))
      (PROG1 (IF (FUNCTIONP SEL*ACTUAL.OPERATION)
		 (APPLY SEL*ACTUAL.OPERATION NIL)
		 (CERROR "continue without execution"
			 "sel*actual.operation ~a is not a function"  SEL*ACTUAL.OPERATION))
	     (SETQ SEL*INITIAL.FLAG NIL)))))

(DEFUN SEL-END NIL 
  (SETQ SEL*ACTUAL.CLAUSES NIL) (SETQ SEL*IND.CHAIN NIL) (SETQ SEL*IND.OPTIONS NIL))

(DEFUN SEL-UPDATE.REDUCE (STEPCOUNTER)
						; edited:  25. 5. 1982   hjo
						; effect:  the next deduction code is computed and
						;          may be accessed by:
						;          sel-deduction.code.link
						;          sel-deduction.code.unifier
						;          sel-deduction.code.linklist .
						; value:   the actual deduction code itself.
  (CATCH 'SEL-UPDATE.REDUCE
    (SETQ SEL*STEPCOUNTER   STEPCOUNTER
	  SEL*REDUCE.DEDUCE 'DEDUCE)
    (when (CG-CLAUSES REMOVED) (SEL=NO.REFUTATION.POSSIBLE?))
    (when (EQL (CAR SEL*ACTUAL.DEDUCTION.CODE) 'EMPTY)
      (SEL=MAKE.DEDUCTION.CODE (SECOND SEL*ACTUAL.DEDUCTION.CODE) (THIRD SEL*ACTUAL.DEDUCTION.CODE))
      (SEL=RETURN))
    (SETQ SEL*ACTUAL.DEDUCTION.CODE NIL) 
    (MAPC #'(LAMBDA (LINK) (SEL=REMOVE.LINK LINK p)) (CG-LINKS P REMOVED))
    (MAPC #'(LAMBDA (LINK) (SEL=REMOVE.LINK LINK piw)) (CG-LINKS Piw REMOVED))
    (MAPC #'(LAMBDA (LINK) (SEL=REMOVE.LINK LINK reductions) (SEL=REMOVE.LINK LINK r)) (CG-LINKS R REMOVED))
    (MAPC #'(LAMBDA (CLAUSE) (SEL=REMOVE.CLAUSE CLAUSE)) (CG-CLAUSES REMOVED))
    (SEL=UPDATE.CLAUSELISTS)
    (SEL=UPDATE.LINKLISTS)
    (unless SEL*TERMINATOR.PROVED.FLAG (SEL=UPDATE))
    (IF (FUNCTIONP SEL*ACTUAL.OPERATION)
	(APPLY SEL*ACTUAL.OPERATION NIL)
	(CERROR "continue without execution"
		"SEL*ACTUAL.OPERATION ~a is not bound to a function"  SEL*ACTUAL.OPERATION))))

(DEFUN SEL-UPDATE.DEDUCE (STEPCOUNTER)		; edited: 25. 5. 1982   hjo
						; effect: the next reduction code is computed and may
						;         be accessed by:
						;         sel-reduction.code.clauses.to.be.reduced
						;         sel-reduction.code.clauses.to.be.removed
						;         sel-reduction.code.links.to.be.removed
						; value:  the actual reduction code itself.
  (CATCH 'SEL-UPDATE.DEDUCE
    (SETQ SEL*STEPCOUNTER           STEPCOUNTER
	  SEL*REDUCE.DEDUCE         'REDUCE
	  SEL*ACTUAL.REDUCTION.CODE NIL)
    (COND
      ((SEL-DEDUCTION.CODE.LINK)
       (SEL=REFUTATION.FOUND (CG-LINKS R INSERTED) (CG-LINKS P INSERTED))
       (MAPC #'(LAMBDA (LINK) (SEL=REMOVE.LINK LINK p)) (CG-LINKS P REMOVED))
       (MAPC #'(LAMBDA (LINK) (SEL=REMOVE.LINK LINK piw)) (CG-LINKS Piw REMOVED))
       (MAPC #'(LAMBDA (LINK) (SEL=REMOVE.LINK LINK reductions) (SEL=REMOVE.LINK LINK r)) (CG-LINKS R REMOVED))
       (COND ((AND (DS-LINK.IS (SEL-DEDUCTION.CODE.LINK))
		   (DS-LINK.IS.MARKED INHIBITED (SEL-DEDUCTION.CODE.LINK)))
	      (SEL=REMOVE.LINK (SEL-DEDUCTION.CODE.LINK))))
       (MAPC #'(LAMBDA (CLAUSE) (SEL=REMOVE.CLAUSE CLAUSE)) (CG-CLAUSES REMOVED))
       (SEL=UPDATE.CLAUSELISTS)
       (SEL=UPDATE.LINKLISTS)
       (UNLESS SEL*TERMINATOR.PROVED.FLAG (SEL=UPDATE))))
    (IF (FUNCTIONP SEL*ACTUAL.OPERATION)
	(funcall SEL*ACTUAL.OPERATION)
	(CERROR "Continue without execution."
		"SEL*ACTUAL.OPERATION ~a is not a function."  SEL*ACTUAL.OPERATION))))



(DEFUN SEL-MARKED.CLAUSES NIL
						; edited:  25. 5. 1982    hjo
						; value:   a list of all marked clauses.
						;          every toplevel element of this list is of
						;          the following structure:
						;          ((indicator1 .. indicatorn) . (link1 ..)).
						;          the indicators are those defined in
						;          sel*clause.classes.
  (MAPCAR
    #'(LAMBDA (CLASS) (CONS CLASS (CAR (SYMBOL-VALUE (INTERN (FORMAT NIL "SEL*CLAUSES:~{~A~}" CLASS)
							     (find-package "MKRP"))))))
    SEL*CLAUSE.CLASSES))

(DEFUN SEL-DEDUCTION.CODE.LINK NIL
						; edited:  3. 6. 1982   hjo
						; value:   the link of the last deduction code.
  (CAR SEL*ACTUAL.DEDUCTION.CODE))

(DEFUN SEL-DEDUCTION.CODE.UNIFIER NIL
						; edited:  3. 6. 1982   hjo
						; value:   the unifier of the last deduction code.
  (CDR SEL*ACTUAL.DEDUCTION.CODE))

(DEFUN SEL-REDUCTION.CODE.CLAUSES.TO.BE.REDUCED NIL
						; edited: 3. 6. 1982   hjo
						; value:  the list of clauses which shall be examined
						;         for reductions.
  (CAR SEL*ACTUAL.REDUCTION.CODE))

(DEFUN SEL-REDUCTION.CODE.CLAUSES.TO.BE.REMOVED (REASONFLAG)
						; edited: 3. 6. 1982   hjo
						; input:  a boolean value.
						; value:  the list of clauses which shall be removed.
						;         reasonflag = t : the value is a list of
						;         dotted pairs (clause . reason)
						;         reasonflag = nil: the value is a flattened
						;         list of clauses.
  (COND (REASONFLAG (SECOND SEL*ACTUAL.REDUCTION.CODE))
	(T (MAPCAR #'CAR (SECOND SEL*ACTUAL.REDUCTION.CODE)))))

(DEFMACRO SEL-REDUCTION.CODE.UNIFIERS.TO.BE.REMOVED (FORM)
						; edited: 3. 6. 1982   hjo
						; input:  form  is one of the following atoms:
						;         links   links.unifiers    all
						; value:  form = links: :
						;         a list of links of which unifiers shall be
						;         removed.
						;         form = links.unifiers
						;         a list of dotted pairs (link . unifier).
						;         form = all
						;         a list with elements (link unifier . reason)
  (CASE FORM
    (LINKS `(MAPCAR #'CAR (CDDR SEL*ACTUAL.REDUCTION.CODE)))
    (LINKS.UNIFIERS
      `(MAPCAR #'(LAMBDA (ELEMENT) (CONS (CAR ELEMENT) (SECOND ELEMENT)))
	       (CDDR SEL*ACTUAL.REDUCTION.CODE)))
    (ALL `(CDDR SEL*ACTUAL.REDUCTION.CODE))
    (OTHERWISE (ERROR "illegal form in sel-reduction.code.links: ~a" FORM))))

(DEFUN SEL-REDUCTION.CODE.LITERALS.TO.BE.REMOVED NIL NIL)

(DEFUN SEL-REFUTATION.RESULT NIL
  (COND ((EQL (SEL-DEDUCTION.CODE.LINK) 'COLLAPSED) (LIST 'FAILURE 'GRAPH.COLLAPSED))))

(DEFUN SEL-REDUCTION.FLAG NIL SEL*REDUCTION.FLAG)


(DEFUN SEL=ELIMINATE NIL
						; edited: 28-jun-82 10:20:47
						; effect: eliminates constants and functions
						;         defined in sel=is.eliminating.equality
						;         from the actual graph by successive
						;         paramodulations.
						; value:  undfined.
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE (SETQ SEL*DUMMY NIL)
						; sel*dummy is used to store the other parent of the active plink
	    (COND ((AND (SYMBOL-VALUE 'SEL=ELIMINATE) (SEL=REDUCTIONS.ACTIVATE?))
		   (SETQ SEL=ELIMINATE NIL)
		   (SEL=PASS.CONTROL '(SEL=REDUCTIONS) NIL T))
		  (T (PROG ((CLAUSE.SIDE (CAR (SEL=CLAUSES ELIMINATING EQUATIONS))) CLAUSE SIDE ELIMINATING.LINK)
			   (COND (CLAUSE.SIDE
				  (SETQ CLAUSE (CAR CLAUSE.SIDE)) (SETQ SIDE (CDR CLAUSE.SIDE))
				  (SETQ ELIMINATING.LINK
					(CAR (MEMBER-IF #'(LAMBDA (PLINK)
							    (SETQ SEL*DUMMY (SEL=ELIMINATE.IS.ELIMINATING.LINK PLINK CLAUSE SIDE))
							    SEL*DUMMY)
							(DS-CLAUSE.LINKS 'P CLAUSE 1))))
				  (COND (ELIMINATING.LINK (SEL=MAKE.DEDUCTION.CODE ELIMINATING.LINK 'FIRST) (SETQ SEL=ELIMINATE T)))
				  (SEL=RETURN))
			     (T (SEL=PASS.CONTROL NIL NIL)))))))
    (REDUCE
      (PROG ((CLAUSE.SIDE (CAR (SEL=CLAUSES ELIMINATING EQUATIONS))) CLAUSE SIDE)
	    (SETQ CLAUSE (CAR CLAUSE.SIDE)
		  SIDE   (CDR CLAUSE.SIDE))
	    (COND (SEL*DUMMY (SETQ SEL*DUMMY (LIST SEL*DUMMY))
			     (COND ((NOTANY #'(LAMBDA (PLINK)
						(AND (NEQ PLINK (SEL-DEDUCTION.CODE.LINK))
						     (SEL=ELIMINATE.IS.ELIMINATING.LINK PLINK CLAUSE SIDE SEL*DUMMY)))
					    (DS-CLAUSE.LINKS 'P CLAUSE 1))
				    (SETQ SEL*DUMMY (CONS CLAUSE SEL*DUMMY)))))
		  (T (SETQ SEL*DUMMY (LIST (cons CLAUSE "Eliminating equality")))))
	    (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) SEL*DUMMY NIL))
      (SEL=RETURN))
    (OTHERWISE (ERROR "illegal reduce.deduce in sel=eliminate: ~a" SEL*REDUCE.DEDUCE))))

(DEFUN SEL=ELIMINATE.ACTIVATE? NIL
						; edited: 30-jun-82 15:27:06
						; value:  not nil if there is an equation which
						;         eliminates a function or a constant,
						;         else nil.
  (SEL=CLAUSES ELIMINATING EQUATIONS))

(DEFUN SEL=ELIMINATE.UPDATE (&OPTIONAL FLAG)
  (DECLARE (IGNORE FLAG))
						; edited: 30-jun-82 15:29:20
						; effect: examines the inserted clauses for
						;         equations which eliminates a constant or
						;         a function.
  (MAPC #'(LAMBDA (CLAUSE)
	    (let ((RESULT (SEL=IS.ELIMINATING.EQUALITY CLAUSE)))
	      (COND (RESULT (SEL=INSERT.CLAUSE RESULT NIL ELIMINATING EQUATIONS)))))
	(CG-CLAUSES INSERTED)))

(DEFUN SEL=ELIMINATE.IS.ELIMINATING.LINK (LINK CLAUSE SIDE &OPTIONAL PARENTS)
						; edited: 18-jan-83 12:43:03
						; input:  a p-link connected to clause
						;         side is one of the atoms left or right
						;         parents is a list of clauses.
						; value:  if the link is an eliminating link,
						;         the other parent, else nil.
  (if (DS-LINK.IS.MARKED INHERITANCE.ONLY LINK)
      nil
      (PROG (POSFCT NEGPAR NEGLITNO NEGFCT)
	    (COND
	      ((EQL CLAUSE (DS-LINK.POSPAR LINK))
	       (SETQ POSFCT (DS-LINK.POSFCT LINK))
	       (SETQ NEGPAR (DS-LINK.NEGPAR LINK))
	       (SETQ NEGLITNO (DS-LINK.NEGLITNO LINK))
	       (SETQ NEGFCT (DS-LINK.NEGFCT LINK)))
	      ((EQL CLAUSE (DS-LINK.NEGPAR LINK))
	       (SETQ POSFCT (DS-LINK.NEGFCT LINK))
	       (SETQ NEGPAR (DS-LINK.POSPAR LINK))
	       (SETQ NEGLITNO (DS-LINK.POSLITNO LINK))
	       (SETQ NEGFCT (DS-LINK.POSFCT LINK))))
	    (COND
	      ((AND (COND ((EQL SIDE 'LEFT) (DT-TAF.IS.LEFT POSFCT))
			  (T (DT-TAF.IS.RIGHT POSFCT)))
		    (NOT (MEMBER NEGPAR PARENTS))
		    (NOT (DT-VARIABLE.IS (DT-ACCESS NEGFCT (DS-CLAUSE.TERMLIST NEGPAR NEGLITNO)))))
	       (RETURN NEGPAR))))))

(DEFUN SEL=IS.ELIMINATING.EQUALITY (CLAUSE)
						; edited: 24-jun-82 17:59:48
						; input:  a clause address.
						; value:  nil or (clause . 'left) or (clause . 'right)
						;         if clause is a unit equality like
						;         f(x1 .. xn) = term
						;         where f does not occur in term
						;         or  constant = term
						;         and the constant does not occur in term.
						;         left and right denotes the side of the
						;         equality in which the function or constant
						;         occurs.
  (unless (opt-is.completion)
    (let (LEFT.TERM RIGHT.TERM SIDE)
      (COND
	((AND (EQL 1 (DS-CLAUSE.NOLIT CLAUSE))
	      (DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE 1))
	      (DS-SIGN.IS.POSITIVE (DS-CLAUSE.SIGN CLAUSE 1)))
	 (SETQ LEFT.TERM  (CAR (DS-CLAUSE.TERMLIST CLAUSE 1))
	       RIGHT.TERM (SECOND (DS-CLAUSE.TERMLIST CLAUSE 1)))
	 (COND ((SETQ SIDE (COND ((AND (CONSP LEFT.TERM)
				       (NOT (DT-VARIABLE.IS RIGHT.TERM))
				       (NOT (DT-VARIABLE.IS LEFT.TERM))
				       (NULL (DT-FUNCTION.ATTRIBUTES (CAR LEFT.TERM)))
				       (let (FAIL)
					 (MAPL #'(LAMBDA (TAIL)
						   (COND ((AND (DT-VARIABLE.IS (CAR TAIL))
							       (NOT (MEMBER (CAR TAIL) (CDR TAIL)))))
							 (T (SETQ FAIL T))))
					       (CDR LEFT.TERM))
					 (NOT FAIL))
				       (NOT (IN (CAR LEFT.TERM) RIGHT.TERM)))
				  'LEFT)
				 ((AND (CONSP RIGHT.TERM)
				       (NULL (DT-FUNCTION.ATTRIBUTES (CAR RIGHT.TERM)))
				       (LET (FAIL)
					 (MAPL #'(LAMBDA (TAIL)
						   (COND ((AND (DT-VARIABLE.IS (CAR TAIL))
							       (NOT (MEMBER (CAR TAIL) (CDR TAIL)))))
							 (T (SETQ FAIL T))))
					       (CDR RIGHT.TERM))
					 (NOT FAIL))
				       (NOT (INSIDE (CAR RIGHT.TERM) LEFT.TERM)))
				  'RIGHT)
				 ((AND (DT-CONSTANT.IS LEFT.TERM) (NULL (DS-CLAUSE.VARIABLES CLAUSE))
				       (NOT (INSIDE LEFT.TERM RIGHT.TERM)))
				  'LEFT)
				 ((AND (DT-CONSTANT.IS RIGHT.TERM) (NULL (DS-CLAUSE.VARIABLES CLAUSE))
				       (NOT (INSIDE RIGHT.TERM LEFT.TERM)))
				  'RIGHT)))
		(CONS CLAUSE SIDE))))))))

(DEFUN SEL=INDUCTION.ACTIVATE? NIL
						; edited: 10-feb-84 10:56:20
						; input:  none.
						; effect: this is the main function to compute the
						;         deduction-chains, which are worked off in
						;         sel=induction. the chains are stored in the
						;         common variable sel*ind.chain.
						; value:  a sexpression =// nil iff there is a chain
						;         to work off.
  (COND
    ((AND (NULL SEL*END.OF.INDUCTION) (NULL SEL*IND.CHAIN))
     (PROG (END.OF.INDUCTION RESULT)
	   (WHILE (AND (NOT END.OF.INDUCTION) (NULL RESULT))
	     (SETQ RESULT
		   (COND ((SEL=SHORTEN.TREE (SEL=ACTUAL.NODE))) ((SEL=GET.NEXT.EVAL.CLAUSE))
			 (T
			  (COND
			    ((< (SEL=IND.GET.OPTION DEPTH) SEL*IND.CASE.COUNTER)
			     (SEL=IND.PUT.OPTION DEPTH (1+ (SEL=IND.GET.OPTION DEPTH)))
			     (SEL=RESET.TREE))
			    ((NULL (SEL=IND.GET.OPTION IMPL))
			     (SEL=IND.PUT.OPTION DEPTH 0)
			     (SEL=IND.PUT.OPTION IMPL T)
			     (SEL=RESET.TREE))
			    (T (SETQ END.OF.INDUCTION T)))
			  NIL))))
	   (COND
	     (END.OF.INDUCTION (SEL=RESET.TREE)
			       (WHILE (AND (NULL RESULT) (SEL=ACTUAL.NODE))
				 (SETQ RESULT (SEL=SIMPLIFY.TREE (SEL=ACTUAL.NODE)))
				 (COND ((NULL RESULT) (SEL=NEXT.NODE))))))
	   (COND
	     ((NULL RESULT) (SETQ SEL*END.OF.INDUCTION T)
	      (PROG (CLAUSE THEOREMS)
		    (SMAPC
		      #'(LAMBDA (SUBNODE)
			  (COND ((NUMBERP SUBNODE))
				(T (SETQ CLAUSE (SEL=SUBNODE.GET CLAUSE SUBNODE)) (SETQ THEOREMS (CONS CLAUSE THEOREMS))
				   (DS-CLAUSE.ALL.LIT.REMPROPS CLAUSE '(IND.EQUALITY IND.EQUIVALENCE)))))
		      #'(LAMBDA (X) (SEL=NEXT.SUBNODE)) (SEL=ACTUAL.SUBNODE)))))
	   (RETURN RESULT)))))

(DEFUN SEL=SHORTEN.TREE (NODE)
						; edited:  9-feb-84 11:18:35
						; input:   node - a node in the actual trans.-tree.
						; effect:  tests whether there is a refutation-chain,
						;          which creates a clause, which subsumes
						;          all clauses in the subnodes of node
  (LET (CHAIN)
    (COND
      ((AND (EQL (SECOND NODE) 'CASE)
	    (EVERY
	      #'(LAMBDA (SUBNODE)
		  (COND ((CONSP SUBNODE) (NOT (CDR (ASSOC 0 (SEL=SUBNODE.GET LITERALS SUBNODE)))))))
	      (CDDR NODE))
	    (SETQ CHAIN
		  (SEL=IS.COMPLETE.REFUTATION
		    (MAPCAR
		      #'(LAMBDA (ENTRY)
			  (CONS (SEL=SUBNODE.GET CLAUSE ENTRY)
				(CAR (CDR (ASSOC (CAR NODE) (SEL=SUBNODE.GET LITERALS ENTRY))))))
		      (CDDR NODE))
		    NIL)))
       (SEL=REMOVE.SUBTREE (CAR NODE)) (SETQ SEL*IND.OLD.CLAUSES (MAPCAR #'CAR (CDDR NODE)))
       (SETQ SEL*IND.CHAIN (LIST (NCONC (LIST 'MERGE (CDR CHAIN) (CAR CHAIN) NIL) (CDDR NODE))))))))

(DEFUN SEL=SIMPLIFY.TREE (NODE)
						; edited: 10-feb-84 10:26:44
						; input:  node - a node in the actual trans.-tree.
						; effect: tests whether there is a refutation-chain
						;         which creates a clause, which subsumes one
						;         of the clauses in one of the subnodes of
						;         node.
						; value:  the refutation-chain if the test successes
						;         else nil.
  nil)

(DEFUN SEL=ACTIVATE.LITNO (CL.LITNOLIST DEPTH)
						; edited: 10-feb-84 10:29:51
						; input:  node - a node of the actual trans.-tree.
						;         depth - an integer
						; effect: all literals of the subnodes of node,
						;         which belong to the node with case-number
						;         depth are reorganized to the initial node 0
						; value : the changed node.
  (MAPC
    #'(LAMBDA (CL.LITNOS)
	(COND
	  ((CONSP CL.LITNOS)
	   (PROG ((ENTRY (ASSOC DEPTH (SEL=SUBNODE.GET LITERALS CL.LITNOS))))
		 (COND (ENTRY (RPLACA ENTRY 0) (SEL=MERGE.LITERALS CL.LITNOS)))))))
    CL.LITNOLIST)
  CL.LITNOLIST)

(DEFUN SEL=IND.ELIMINATE (CLAUSE LITNO)
  (PROG (OTHERCLAUSE SIDE ELIMINATION.LINK)
	(MEMBER-IF
	  #'(LAMBDA (CLAUSE.SIDE) (SETQ OTHERCLAUSE (CAR CLAUSE.SIDE)) (SETQ SIDE (CDR CLAUSE.SIDE))
		    (SETQ ELIMINATION.LINK
			  (CAR
			    (MEMBER-IF
			      #'(LAMBDA (PLINK) (SEL=ELIMINATE.IS.ELIMINATING.LINK PLINK OTHERCLAUSE SIDE))
			      (INTERSECTION (DS-CLAUSE.LINKS 'P OTHERCLAUSE 1) (DS-CLAUSE.LINKS 'P CLAUSE LITNO))))))
	  (SEL=CLAUSES ELIMINATING EQUATIONS))
	(COND
	  (ELIMINATION.LINK
	   (RETURN
	     (SETQ SEL*IND.CHAIN
		   (LIST (LIST 'DEDUCE (CONS ELIMINATION.LINK (CAR (DS-LINK.UNIFIERS ELIMINATION.LINK))) NIL NIL
			       (CAR (SEL=ACTUAL.SUBNODE)) (LIST OTHERCLAUSE)))))))))

(DEFUN SEL=IND.REDUCTION (CLAUSE LITNO)
  (PROG ((PRED.IS.NO.EQ (NOT (DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE LITNO)))) OTHERPAR)
	(MEMBER-IF
	  #'(LAMBDA (RLINK) (SETQ OTHERPAR (DS-LINK.OTHERPAR RLINK CLAUSE))
		    (COND
		      ((AND (OR PRED.IS.NO.EQ (MEMBER OTHERPAR (SEL=CLAUSES SUPPORTED)))
			    (DS-CLAUSE.ONE.LIT.UNIFIER CLAUSE LITNO (CAR (DS-LINK.UNIFIERS RLINK)))
			    (NOT (< (DS-CLAUSE.NOLIT CLAUSE) (DS-CLAUSE.NOLIT OTHERPAR))))
		       (COND
			 ((EQL (DS-CLAUSE.NOLIT OTHERPAR) 1)
			  (RETURN
			    (SETQ SEL*IND.CHAIN
				  (LIST (LIST 'DEDUCE (CONS RLINK (CAR (DS-LINK.UNIFIERS RLINK)))
					      NIL NIL (CAR (SEL=ACTUAL.SUBNODE))
					      (LIST OTHERPAR))))))
			 (T (let ((OTHERLITNO (DS-LINK.OTHERLITNO RLINK CLAUSE)) NEW.MATCHER
				  (MATCHER (CAR (DS-LINK.UNIFIERS RLINK))) UNIFIER LIT)
			      (DODOWN (RPTN (DS-CLAUSE.NOLIT OTHERPAR))
				(COND ((EQL (1+ RPTN) OTHERLITNO))
				      ((MEMBER-IF
					 #'(LAMBDA (SLINK) (SETQ LIT (DS-LINK.OTHERLITNO SLINK OTHERPAR))
						   (COND ((AND (NEQ LITNO LIT)
							       (SETQ UNIFIER
								     (UNI-UNIFIER.IS.MATCHER
								       (DS-LINK.UNIFIERS RLINK)
								       (DS-CLAUSE.PREDICATE OTHERPAR (1+ RPTN))
								       (DS-CLAUSE.TERMLIST OTHERPAR (1+ RPTN))
								       (DS-CLAUSE.PREDICATE CLAUSE LIT)
								       (DS-CLAUSE.TERMLIST CLAUSE LIT) (DS-CLAUSE.VARIABLES CLAUSE)
								       T))
							       (SETQ NEW.MATCHER (CAR (UNI-MERGE.SUBSTITUTIONS MATCHER UNIFIER))))
							  (SETQ MATCHER NEW.MATCHER))))
					 (INTERSECTION (DS-CLAUSE.ALL.LINKS 'S CLAUSE) (DS-CLAUSE.LINKS 'S OTHERPAR (1+ RPTN)))))
				      (T (SETQ RPTN 0) (SETQ MATCHER 'FAIL))))
			      (COND
				((NEQ MATCHER 'FAIL)
				 (SETQ SEL*IND.CHAIN
				       (LIST (LIST 'DEDUCE (CONS RLINK MATCHER) NIL NIL (CAR (SEL=ACTUAL.SUBNODE))
						   (LIST OTHERPAR))))))
			      SEL*IND.CHAIN))))))
	  (DS-CLAUSE.LINKS 'R CLAUSE LITNO))
	(RETURN SEL*IND.CHAIN)))

(DEFUN SEL=GET.NEXT.EVAL.CLAUSE NIL
  (PROG (CLAUSE)
	(RETURN
	  (SSOME
	    #'(LAMBDA (NODE)
		(COND
		  ((CONSP NODE) (SETQ CLAUSE (SEL=SUBNODE.GET CLAUSE NODE))
		   (MEMBER-IF
		     #'(LAMBDA (LIT)
			 (COND ((SEL=IND.ELIMINATE CLAUSE LIT)) ((SEL=IND.REDUCTION CLAUSE LIT))
			       (T
				(COND
				  ((NULL (DS-CLAUSE.LIT.GETPROP CLAUSE LIT 'IND.EVAL)) (SEL=INSERT.ALL.EVAL.LINKS CLAUSE LIT)))
				(SETQ SEL*IND.CHAIN (SEL=GET.NEXT.EVAL.LINKS CLAUSE LIT)))))
		     (CDR (ASSOC (SEL=IND.GET.OPTION DEPTH) (SEL=SUBNODE.GET LITERALS NODE)))))))
	    #'(LAMBDA (LITLIST) (SEL=NEXT.SUBNODE)) (SEL=ACTUAL.SUBNODE)))))

(DEFUN SEL=GET.NEXT.EVAL.LINKS (CLAUSE LITNO)
						; edited: 26-may-83 13:01:21
						; input:  clause and litno denote a literal in a
						;         clause which is to be rewritten.
						; effect & value:
						;         a list of lists of refutation-chains. each
						;         list of refutation-chains denote a
						;         equivalent transformation of clause. if
						;         there are more than one list of chains a
						;         distinction of different cases is simulated.
						; note:   if a eval-link is detected to generate a
						;         tautology, it is removed from the actual
						;         eval-link.list of clause.
  (PROG (APPLICABLE.LINKS FINAL.LINK FCT)
	(MAPC
	  #'(LAMBDA (KIND)
	      (SETQ APPLICABLE.LINKS
		    (NCONC (SEL=GET.APPLICABLE.LINKS (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO KIND) CLAUSE LITNO NIL)
			   APPLICABLE.LINKS)))
	  (SEL=IND.GET.OPTION KIND))
	(COND
	  (APPLICABLE.LINKS (SETQ FINAL.LINK (SEL=SELECT.EVAL.LINK APPLICABLE.LINKS)) (SETQ FCT (CAR FINAL.LINK))
			    (COND ((NOT (CDDR FINAL.LINK)) (SETQ SEL*IND.OLD.CLAUSES (LIST CLAUSE))))
			    (SETQ FINAL.LINK (SECOND FINAL.LINK))))
	(RETURN
	  (COND
	    (FINAL.LINK
	     (COND
	       ((MEMBER (CAR FINAL.LINK) (SEL=CLAUSES INDUCTION))
		(DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO 'IND.INHIBIT
				       (CONS
					 (CONS (SEL=IF.CLAUSE.GET NAME (CAR FINAL.LINK))
					       (CASE (CAR (SEL=IF.CLAUSE.GET SIDE (CAR FINAL.LINK)))
						 (LEFT 'RIGHT)
						 (RIGHT 'LEFT) (OTHERWISE NIL)))
					 (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO 'IND.INHIBIT)))))
	     (SEL=GET.OPTIMIZED.LINKS (SECOND FINAL.LINK) (CAR FINAL.LINK) (THIRD FINAL.LINK)
				      (PROG (PATTERN)
					    (DODOWN (RPTN (LIST-LENGTH (SEL=IF.CLAUSE.GET CONDITION (CAR FINAL.LINK))))
					      (SETQ PATTERN (CONS NIL PATTERN)))
					    (RETURN PATTERN))
				      CLAUSE LITNO FCT
				      (SEL=REMOVE.SUBTREE (CONS 'CASE (SEL=SUBTREE.OF (SEL=IND.GET.OPTION DEPTH) NIL CLAUSE)))
				      (SEL=REMOVE.SUBTREE (CONS 'CASE (SEL=IND.GET.OPTION DEPTH)))))))))

(DEFUN SEL=GET.APPLICABLE.LINKS (CANDIDATES CLAUSE LITNO PATTERN)
  (MAPCAN
    #'(LAMBDA (FCT.LINKS)
        (PROG (CILS.LIST CHAIN ENTRY)
	      (MAPC
		#'(LAMBDA (ENTRY.LINK.UNIFIER)
		    (SETQ CILS.LIST
			  (CONS (SEL=GET.CILS.LIST (CDR ENTRY.LINK.UNIFIER) (CAR ENTRY.LINK.UNIFIER) PATTERN) CILS.LIST)))
		(CDR FCT.LINKS))
	      (RETURN
		(COND
		  ((SETQ CHAIN (SEL=GET.REFUTATION CLAUSE LITNO (CG-CLAUSES ALL) CILS.LIST))
		   (SETQ CHAIN
			 (MAPCAN
			   #'(LAMBDA (CLS)
			       (MAPCAR
				 #'(LAMBDA (LINK.UNIFIER)
				     (MEMBER-IF
				       #'(LAMBDA (ENTRY.LINK.UNIFIER)
					   (COND
					     ((EQUAL LINK.UNIFIER (CDR ENTRY.LINK.UNIFIER))
					      (SETQ ENTRY (CAR ENTRY.LINK.UNIFIER)))))
				       (CDR FCT.LINKS))
				     (LIST ENTRY LINK.UNIFIER (CDDR CLS)))
				 (CAADR CLS)))
			   CHAIN))
		   (COND (CHAIN (LIST (CONS (CAR FCT.LINKS) CHAIN))) (T NIL)))))))
    CANDIDATES))

(DEFUN SEL=GET.OPTIMIZED.LINKS (SELECTED.LINK IF.CLAUSE OLD.TERM PATTERN CLAUSE* LITNO FCT SUBTREE TREE)
  (DECLARE (SPECIAL CLAUSE))
  (COND
    ((NEQ (DS-TYPE (SEL=IF.CLAUSE.GET NAME IF.CLAUSE)) 'CLAUSE)
     (PROG ((SUB.IF.CLAUSES (SEL=IF.CLAUSE.GET SUB.CASES IF.CLAUSE)) CANDIDATES CHAIN CLAUSE APPEND.CASES)
	   (DECLARE (SPECIAL CLAUSE APPEND.CASES))
	   (SETQ CLAUSE CLAUSE*)
	   (MAPC
	     #'(LAMBDA (IF.CLAUSE2)
		 (SETQ CANDIDATES (NCONC (SEL=GET.EVAL.LINK CLAUSE LITNO IF.CLAUSE2 FCT) CANDIDATES)))
	     SUB.IF.CLAUSES)
	   (SETQ CHAIN (SEL=GET.APPLICABLE.LINKS (LIST (CONS FCT CANDIDATES)) CLAUSE LITNO PATTERN))
	   (RETURN
	     (COND
	       (CHAIN
                (SEL=GET.OPTIMIZED.LINKS (SECOND (CADAR CHAIN)) (CAR (CADAR CHAIN)) (THIRD (CADAR CHAIN)) (CONS NIL PATTERN) CLAUSE
					 LITNO FCT SUBTREE TREE))
	       (T (SETQ SEL*IND.CASE.COUNTER (1+ SEL*IND.CASE.COUNTER))
		  (CONS
		    (CONS SEL*IND.CASE.COUNTER
			  (CONS 'CASE
				(MAPCAN
				  #'(LAMBDA (IF.CLAUSE2)
				      (SEL=FORMAT.EVAL.CHAIN
					(SETQ CHAIN
					      (SEL=GET.OPTIMIZED.LINKS
						(CDAR (SEL=GET.EVAL.LINK CLAUSE LITNO IF.CLAUSE2 FCT)) IF.CLAUSE2 OLD.TERM
						(CONS SEL*IND.CASE.COUNTER PATTERN) CLAUSE LITNO FCT SUBTREE TREE))))
				  SUB.IF.CLAUSES)))
		    APPEND.CASES))))))
    (T (let (SUB.IF.CLAUSES
	     (CHAIN (SEL=GET.ALL.RELEVANT.LINK.CHAINS SELECTED.LINK IF.CLAUSE OLD.TERM PATTERN CLAUSE LITNO FCT
						      SUBTREE TREE))
	     APPEND.CASES)
	 (COND ((SETQ SUB.IF.CLAUSES (SEL=IF.CLAUSE.GET SUB.CASES IF.CLAUSE))
		(SETQ SEL*IND.CASE.COUNTER (1+ SEL*IND.CASE.COUNTER))
		(SETQ CHAIN (CONS (SEL=FORMAT.EVAL.CHAIN CHAIN)
				  (MAPCAN #'(LAMBDA (SUB.IF.CLAUSE)
					      (SEL=FORMAT.EVAL.CHAIN
						(LIST (SEL=GET.ALL.RELEVANT.LINK.CHAINS
							(SEL=GET.EVAL.LINK CLAUSE LITNO SUB.IF.CLAUSE FCT)
							SUB.IF.CLAUSE NIL PATTERN CLAUSE
							LITNO FCT SUBTREE TREE))))
					  SUB.IF.CLAUSES)))
		(LIST (NCONC (LIST SEL*IND.CASE.COUNTER 'EQV CHAIN) APPEND.CASES)))
	       (T CHAIN))))))

(DEFUN SEL=SELECT.EVAL.LINK (LINKLIST)
						; edited: 26-may-83 17:16:15
						; input:  linklist - a list of links connected to
						;         clause.
						; effect & value:
						;         returns a list of selected links of linklist
						; remark: because of incompleteness of paramodulation
						;         in this version the innermost p-link of
						;         linklist connected to clause is selected.
						;         if there is no such p-link a list of r-links
						;         denoting an arbitrary equivalent transforma-
						;         tion is returned
  (PROG (FCT ENTRY.LINK.UNIFIERS)
	(MAPC
	  #'(LAMBDA (FCT.ENTRY.LINK.UNIFIERS)
	      (COND
		((< (LIST-LENGTH FCT) (LIST-LENGTH (CAR FCT.ENTRY.LINK.UNIFIERS)))
		 (SETQ FCT (CAR FCT.ENTRY.LINK.UNIFIERS))
		 (SETQ ENTRY.LINK.UNIFIERS (CDR FCT.ENTRY.LINK.UNIFIERS)))
		((EQUAL (CAR FCT.ENTRY.LINK.UNIFIERS) FCT)
		 (SETQ ENTRY.LINK.UNIFIERS (APPEND (CDR FCT.ENTRY.LINK.UNIFIERS) (COPY-LIST ENTRY.LINK.UNIFIERS))))))
	  LINKLIST)
	(RETURN (CONS FCT ENTRY.LINK.UNIFIERS))))

(DEFUN SEL=GET.ALL.RELEVANT.LINK.CHAINS (SELECTED.LINK IF.CLAUSE OLD.TERM PATTERN CLAUSE LITNO FCT SUBTREE TREE)
  (COND (TREE (SETQ SUBTREE (SEL=RENUMBER.TREE (COPY-TREE SUBTREE) (CAR (LAST TREE)))) (RPLACA (LAST TREE) (CDR SUBTREE))
	      (SETQ SUBTREE (CAR SUBTREE))
	      (MAPC #'(LAMBDA (NODE)
			(MAPL #'(LAMBDA (SUBNODE.LIST)
				  (COND ((EQL (DS-TYPE (CAAR SUBNODE.LIST)) CLAUSE)
					 (RPLACA SUBNODE.LIST
						 (let ((SUBNODE (CAR SUBNODE.LIST)) IF.CLAUSE RESULT PATTERN)
						   (COND ((EQL (SEL=SUBNODE.GET CLAUSE SUBNODE) CLAUSE)
							  (SEL=GET.EVAL.CHAIN SUBNODE LITNO IF.CLAUSE SELECTED.LINK NIL OLD.TERM))
							 (T (SETQ RESULT (SEL=GET.PARALLEL.LINK CLAUSE LITNO FCT SELECTED.LINK
												SUBNODE IF.CLAUSE))
							    (SEL=GET.EVAL.CHAIN (SEL=SUBNODE.GET CLAUSE SUBNODE)
										(CAR RESULT) IF.CLAUSE
										(CDR RESULT) PATTERN NIL))))))))
			      (CDR NODE)))
		    SUBTREE)
	      (NCONC SUBTREE TREE))
	(T (LIST (SEL=GET.EVAL.CHAIN (CAR (SEL=GET.SUBNODE CLAUSE NIL)) LITNO IF.CLAUSE SELECTED.LINK PATTERN OLD.TERM)))))

(DEFUN SEL=GET.EVAL.CHAIN (SUBNODE LITNO IF.CLAUSE SELECTED.LINK PATTERN OLD.TERM)
  (DECLARE (IGNORE LITNO))
  (PROG (TO.REFUTE REMAINING.LITS CONCLUSIO)
	(MAPC
	  #'(LAMBDA (FLG LIT)
	      (COND (FLG (SETQ REMAINING.LITS (CONS (LIST FLG LIT) REMAINING.LITS))) (T (SETQ TO.REFUTE T))))
	  PATTERN (SEL=IF.CLAUSE.GET CONDITION IF.CLAUSE))
	(COND
	  ((EQL (DS-LINK.COLOUR (CAR SELECTED.LINK)) 'R) (SETQ CONCLUSIO (COPY-TREE (SEL=IF.CLAUSE.GET CONCLUSIO IF.CLAUSE)))))
	(RETURN
	  (LIST 'DEDUCE SELECTED.LINK OLD.TERM (COND (TO.REFUTE (CONS IF.CLAUSE PATTERN))) (COPY-TREE SUBNODE)
		(CONS (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)
		      (COND (CONCLUSIO (CONS (CONS (SEL=IND.GET.OPTION DEPTH) CONCLUSIO) REMAINING.LITS)) (T REMAINING.LITS)))))))

(DEFUN SEL=FORMAT.EVAL.CHAIN (CHAIN)
  (DECLARE (SPECIAL APPEND.CASES))
  (COND ((OR (EQL (CAAR CHAIN) 'DEDUCE) (EQL (DS-TYPE (CAAR CHAIN)) 'CLAUSE)) CHAIN)
	(T (PROG1 (LIST (CAAR (LAST CHAIN))) (SETQ APPEND.CASES (NCONC CHAIN APPEND.CASES))))))

(DEFUN SEL=GET.EVAL.LINK (CLAUSE LITNO IF.CLAUSE FCT)
  (COND (FCT (SEL=INSERT.EVAL.PLINKS CLAUSE LITNO IF.CLAUSE 'IND.INTER))
	(T (SEL=INSERT.EVAL.RLINKS CLAUSE LITNO IF.CLAUSE 'IND.INTER)))
  (PROG1 (CDR (ASSOC FCT (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO 'IND.INTER) :TEST #'EQUAL))
	 (DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO 'IND.INTER NIL)))

(DEFUN SEL=GET.PARALLEL.LINK (CLAUSE LITNO FCT LINK.UNIFIER SUBNODE IF.CLAUSE)
  (DECLARE (IGNORE LINK.UNIFIER CLAUSE LITNO))
  (PROG
    ((NEW.LITNOS (CDR (ASSOC (SEL=IND.GET.OPTION DEPTH) (SEL=SUBNODE.GET LITERALS SUBNODE)))) NEW.LINK NEW.LITNO)
    (COND
      ((SETQ NEW.LITNO
	     (CAR
	       (MEMBER-IF
		 #'(LAMBDA (LIT)
		     (SETQ NEW.LINK (SEL=GET.EVAL.LINK (SEL=SUBNODE.GET CLAUSE SUBNODE) LIT IF.CLAUSE FCT)))
		 NEW.LITNOS)))
       (RETURN (CONS NEW.LITNO NEW.LINK))))))

(DEFUN SEL=INDUCTION.UPDATE (ALLFLAG)
						; edited:  9-oct-83 18:48:56
						; input:   allflag = t//nil .
						; effect:  updates all global informations for the
						;          eval-part of selection after a step.
						;          i.e. the eval-possibilities of the new
						;          generated clauses are inserted in special
						;          propertylists of this clauses and the
						;          entries of sel*ind.chain are updated.
						; value:   undefined
  (declare (ignore allflag)))

(DEFUN SEL=INHERIT.LINKS.AND.LITERALS NIL
						; edited:  9-oct-83 18:52:26
						; input:   nil.
						; effect:  updates the global eval.lists:
						;          in sel*ind.chain clauses and its
						;          literals are substituted by their
						;          descendants. if the descendants of two
						;          clauses merge, then their entries are con-
						;          catinated. in sel*ind.in.work the old
						;          link and its unifier is replaced by its
						;          descendant. if the eval-chain for the
						;          eval-possibility is worked up, the
						;          entries of the new clauses are added to
						;          sel*actual.clauses.
						; value :  undefined.
						; note:    sel*actual.entry is a part of sel*ind.
						;          chain. so every change of sel*actual.entry
						;          will cause a change of sel*ind.chain ]
  (PROG ((ENTRY (CAR SEL*IND.ACTUAL.ENTRY)))
	(MAPC
	  #'(LAMBDA (CL.LITLIST)
	      (PROG ((CLAUSE (SEL=SUBNODE.GET CLAUSE CL.LITLIST)) NEW.CLAUSE)
		    (COND
		      ((SETQ NEW.CLAUSE
			     (CAR
			       (MEMBER-IF
				 #'(LAMBDA (CL)
				     (PROG ((CL1.CL2 (DS-CLAUSE.PARENTS CL)))
					   (RETURN (OR (EQL (CAR CL1.CL2) CLAUSE) (EQL (SECOND CL1.CL2) CLAUSE)))))
				 (CG-CLAUSES INSERTED))))
		       (SETQ SEL*IND.OLD.CLAUSES (INS NEW.CLAUSE SEL*IND.OLD.CLAUSES))
		       (SEL=SUBNODE.PUT LITERALS CL.LITLIST (SEL=INHERIT.LITERALS NEW.CLAUSE CLAUSE (CDR CL.LITLIST)))
		       (SEL=SUBNODE.PUT CLAUSE CL.LITLIST NEW.CLAUSE)))))
	  ENTRY)
	(RPLACD ENTRY
		(DREMAP (CDR ENTRY) NIL
			#'(LAMBDA (CL.LITNO.LIST)
			    (COND
			      ((EQL (SEL=SUBNODE.GET CLAUSE (CAR CL.LITNO.LIST)) (SEL=SUBNODE.GET CLAUSE (CAR ENTRY)))
			       (SEL=SUBNODE.PUT LITERALS (CAR ENTRY)
						(NCONC (SEL=SUBNODE.GET LITERALS (CAR ENTRY))
						       (SEL=SUBNODE.GET LITERALS (CAR CL.LITNO.LIST)))))))))
	(COND
	  ((CDR ENTRY)
	   (PROG ((NEW.LINK (CAR (CG-LINK_DESCENDANT.LINKS (CAR SEL*IND.SINGLE.LINK)))))
		 (SETQ SEL*IND.SINGLE.LINK (CONS NEW.LINK (CAR (DS-LINK.UNIFIERS NEW.LINK)))) (RPLACA SEL*IND.ACTUAL.ENTRY ENTRY)))
	  (T (SETQ ENTRY (SEL=MERGE.LITERALS (CAR ENTRY))) (SETQ SEL*IND.NEW.CLAUSES (CONS (CAR ENTRY) SEL*IND.NEW.CLAUSES))
	     (SETQ SEL*IND.OLD.CLAUSES (SET-DIFFERENCE SEL*IND.OLD.CLAUSES SEL*IND.NEW.CLAUSES))
	     (RPLACA SEL*IND.ACTUAL.ENTRY ENTRY)
	     (COND
	       ((NEQ SEL*IND.CHAIN SEL*IND.ACTUAL.ENTRY)
		(COND
		  ((NULL (OR (SEL=GET.SUBNODE 'MERGE SEL*IND.CHAIN) (SEL=GET.SUBNODE 'DEDUCE SEL*IND.CHAIN)))
		   (SEL=REPLACE.NODE (CAAR (LAST SEL*IND.CHAIN)) SEL*IND.CHAIN) (SETQ SEL*IND.CHAIN NIL))))
	       (T (SEL=REPLACE.NODE (CAR SEL*IND.CHAIN) NIL) (SETQ SEL*IND.CHAIN NIL)))))))

(DEFUN SEL=INHERIT.LITERALS (CLAUSE OLDCLAUSE LITERAL.LIST)
						; edited:  9-oct-83 18:59:51
						; input:   oldclause and clause are two clauseadresses
						;          literals are a list of literalnumbers of
						;          oldclause.
						; effect & value:
						;          all literalnumbers of literals are replaced
						;          by their descendants in clause. not
						;          inherited literals are removed from the
						;          list.
						; note :   this function operates destructively on
						;          literals ]]]
  (MAPC
    #'(LAMBDA (CASE.LITNOS)
        (MAPL
          #'(LAMBDA (OLD.LITNOS)
              (COND
                ((NUMBERP (CAR OLD.LITNOS))
		 (PROG
		   ((NEW.LITNO
		      (COND
			((CAR
			   (CDR (ASSOC CLAUSE (CG-CLAUSE_DESCENDANT.LITERALS OLDCLAUSE (CAR OLD.LITNOS))))))
			((PROG (LIT)
			       (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
				 (COND
				   ((EQL
				      (CAR
					(CDR (ASSOC OLDCLAUSE (CG-CLAUSE_ANCESTOR.LITERALS CLAUSE (1+ RPTN)))))
				      (CAR OLD.LITNOS))
				    (SETQ LIT (1+ RPTN)) (SETQ RPTN 0))))
			       (RETURN LIT))))))
		   (COND
		     (NEW.LITNO
		      (DS-CLAUSE.LIT.PUTPROP CLAUSE NEW.LITNO 'IND.INHIBIT
					     (DS-CLAUSE.LIT.GETPROP OLDCLAUSE (CAR OLD.LITNOS) 'IND.INHIBIT))))
		   (RPLACA OLD.LITNOS NEW.LITNO)))))
          (CDR CASE.LITNOS)))
    LITERAL.LIST)
  LITERAL.LIST)

(DEFUN SEL=MERGE.LITERALS (CL.LITLIST)
						; edited: 10-feb-84 10:34:36
						; input:  cl.litlist - a association-list of
						;                      clause . literals.
						; effect: if there are two entries of one clause
						;         both entries are merged to one entry.
						; value:  the simplified cl.litlist.
  (PROG (ENTRY)
	(RPLACD CL.LITLIST
		(DREMAP (CDR CL.LITLIST) NIL
			#'(LAMBDA (CASE.LITNOS)
			    (COND
			      ((SETQ ENTRY (ASSOC (CAAR CASE.LITNOS) (CDR CASE.LITNOS)))
			       (RPLACD ENTRY (UNION (CDR (CAR CASE.LITNOS)) (CDR ENTRY))))
			      (T (RPLACD (CAR CASE.LITNOS) (DELETE NIL (CDAR CASE.LITNOS))) (NULL (CDAR CASE.LITNOS)))))))
	(RETURN CL.LITLIST)))

(DEFUN SEL=REMOVE.LINK.FROM.EVAL (LINK)
						; edited: 26-may-83 12:58:07
						; input:  link - a link of the actual graph.
						; effect: removes link from the propertylists of the
						;         resp. literals, which denote actual eval-
						;         possibilities.
  (PROG
    ((NEGPAR (DS-LINK.NEGPAR LINK)) (POSPAR (DS-LINK.POSPAR LINK)) (NEGLITNO (DS-LINK.NEGLITNO LINK))
     (POSLITNO (DS-LINK.POSLITNO LINK)))
    (MAPC
      #'(LAMBDA (TYPE)
          (DS-CLAUSE.LIT.PUTPROP NEGPAR NEGLITNO TYPE
				 (DREMAP (DS-CLAUSE.LIT.GETPROP NEGPAR NEGLITNO TYPE) NIL
					 #'(LAMBDA (FCT.LINK.UNIFIERS)
					     (PROG ((LINK.UNIFIERS (REMASSOC LINK (CDAR FCT.LINK.UNIFIERS))))
						   (RETURN
						     (COND ((NULL LINK.UNIFIERS))
							   (T (RPLACD (CAR FCT.LINK.UNIFIERS) LINK.UNIFIERS) NIL)))))))
          (DS-CLAUSE.LIT.PUTPROP POSPAR POSLITNO TYPE
				 (DREMAP (DS-CLAUSE.LIT.GETPROP POSPAR POSLITNO TYPE) NIL
					 #'(LAMBDA (FCT.LINK.UNIFIERS)
					     (PROG ((LINK.UNIFIERS (REMASSOC LINK (CDAR FCT.LINK.UNIFIERS))))
						   (RETURN
						     (COND ((NULL LINK.UNIFIERS))
							   (T (RPLACD (CAR FCT.LINK.UNIFIERS) LINK.UNIFIERS) NIL))))))))
      '(IND.EQUALITY IND.EQUIVALENCE))))

(DEFUN SEL=INSERT.ALL.EVAL.LINKS (CLAUSE LITNO)
						; edited: 10-feb-84 10:37:31
						; input:  clause,litno denotes a literal in clause.
						; effect: all possible evaluation links to the denoted
						;         literal in clause in are stored in a the
						;         property equality or equivalence of this
						;         literal. also a property 'eval is set to t.
						; value:  undefined.
  (MAPC #'(LAMBDA (IF.CLAUSE) (SEL=INSERT.EVAL.PLINKS CLAUSE LITNO IF.CLAUSE 'IND.EQUALITY)) (SEL=CLAUSES EQUALITY))
  (MAPC
    #'(LAMBDA (IF.CLAUSE)
        (COND
          ((NOT
             (MEMBER
               (CAR
                 (ASSOC (SEL=IF.CLAUSE.GET NAME IF.CLAUSE) (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO 'IND.INHIBIT)))
               (SEL=IF.CLAUSE.GET SIDE IF.CLAUSE)))
	   (SEL=INSERT.EVAL.PLINKS CLAUSE LITNO IF.CLAUSE 'IND.INDUCTION))))
    (SEL=CLAUSES INDUCTION))
  (MAPC #'(LAMBDA (IF.CLAUSE) (SEL=INSERT.EVAL.RLINKS CLAUSE LITNO IF.CLAUSE 'IND.EQUIVALENCE))
	(SEL=CLAUSES EQUIVALENCE))
  (DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO 'EVAL T))

(DEFUN SEL=INSERT.EVAL.PLINKS (CLAUSE LITNO IF.CLAUSE KIND)
						; edited: 31-may-83 18:12:10
						; input:  clause and litno denotes a literal in clause
						;         kind is one of the atoms equality or
						;         equivalnce.
						; effect: all plinks denoting eval-possibilities
						;         between the literal and the if.clause
						;         are stored in the propertylist with the
						;         indicator kind. each possibility is stored
						;         as a tuple (if.clause link . unifier) in
						;         a list of other possibilities with same
						;         access-function.
						; value : undefined.
  (PROG
    ((IF.CLAUSE.NO (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)) (IF.LITNO (SEL=IF.CLAUSE.GET LITNO IF.CLAUSE))
     (EVAL.SIDE (SEL=IF.CLAUSE.GET SIDE IF.CLAUSE)) UNIFIERS POSFCT NEGFCT)
    (COND
      ((NOT (EQL IF.CLAUSE.NO CLAUSE))
       (MAPC
	 #'(LAMBDA (PLINK)
	     (COND
	       ((MEMBER (DS-LINK.SIDE PLINK IF.CLAUSE.NO) EVAL.SIDE)
		(SETQ LITNO (DS-LINK.OTHERLITNO PLINK IF.CLAUSE.NO))
		(COND
		  ((EQL CLAUSE (DS-LINK.POSPAR PLINK)) (SETQ POSFCT (DS-LINK.POSFCT PLINK))
		   (SETQ NEGFCT (DS-LINK.NEGFCT PLINK)))
		  (T (SETQ NEGFCT (DS-LINK.POSFCT PLINK)) (SETQ POSFCT (DS-LINK.NEGFCT PLINK))))
		(SETQ UNIFIERS
		      (UNI-UNIFIER.IS.MATCHER (DS-LINK.UNIFIERS PLINK) (DS-CLAUSE.PREDICATE IF.CLAUSE.NO IF.LITNO)
					      (DS-CLAUSE.TERMLIST IF.CLAUSE.NO IF.LITNO) (DS-CLAUSE.PREDICATE CLAUSE LITNO)
					      (DS-CLAUSE.TERMLIST CLAUSE LITNO) (DS-CLAUSE.VARIABLES CLAUSE) T))
		(COND
		  ((AND (NULL (CDR NEGFCT))
			(SETQ UNIFIERS
			      (COND ((NOT (SEL=IF.CLAUSE.GET MATCH IF.CLAUSE)) UNIFIERS)
				    (T
				     (MAPCAN
				       #'(LAMBDA (UNI)
					   (UNI-MERGE1.SUBSTITUTIONS UNI
								     (DS-CLAUSE.LIT.GETPROP IF.CLAUSE.NO IF.LITNO 'IND.CHAIN)
								     (DS-CLAUSE.VARIABLES CLAUSE)))
				       UNIFIERS)))))
		   (DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO KIND
					  (PROG
					    ((ENTRY (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO KIND)) LINKS
					     (NEW.LINKS
					       (MAPCAR #'(LAMBDA (UNIFIER) (CONS IF.CLAUSE (CONS PLINK UNIFIER))) UNIFIERS)))
					    (SETQ LINKS (ASSOC POSFCT ENTRY :TEST #'EQUAL))
					    (COND (LINKS (RPLACD LINKS (APPEND NEW.LINKS (COPY-LIST (CDR LINKS)))))
						  (T (SETQ ENTRY (CONS (CONS POSFCT NEW.LINKS) ENTRY))))
					    (RETURN ENTRY))))))))
	 (INTERSECTION (DS-CLAUSE.LINKS 'P CLAUSE LITNO) (DS-CLAUSE.LINKS 'P IF.CLAUSE.NO IF.LITNO)))))))

(DEFUN SEL=INSERT.EVAL.RLINKS (CLAUSE LITNO IF.CLAUSE KIND)
						; edited: 31-may-83 18:12:10
						; input:  clause and litno denotes a literal in clause
						;         kind is one of the atoms equality or
						;         equivalnce.
						; effect: all rlinks denoting eval-possibilities
						;         between the literal and the if.clause
						;         are stored in the propertylist with the
						;         indicator kind. each possibility is stored
						;         as a tuple (if.clause link . unifier).
						; value : undefined.
  (PROG
    ((IF.CLAUSE.NO (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)) (IF.LITNO (SEL=IF.CLAUSE.GET LITNO IF.CLAUSE)) UNIFIERS)
    (COND
      ((NOT (EQL IF.CLAUSE.NO CLAUSE))
       (MAPC
	 #'(LAMBDA (RLINK) (SETQ LITNO (DS-LINK.OTHERLITNO RLINK IF.CLAUSE.NO))
		   (SETQ UNIFIERS
			 (UNI-UNIFIER.IS.MATCHER (DS-LINK.UNIFIERS RLINK) (DS-CLAUSE.PREDICATE IF.CLAUSE.NO IF.LITNO)
						 (DS-CLAUSE.TERMLIST IF.CLAUSE.NO IF.LITNO) (DS-CLAUSE.PREDICATE CLAUSE LITNO)
						 (DS-CLAUSE.TERMLIST CLAUSE LITNO) (DS-CLAUSE.VARIABLES CLAUSE) T))
		   (COND
		     ((SETQ UNIFIERS
			    (COND ((NOT (SEL=IF.CLAUSE.GET MATCH IF.CLAUSE)) UNIFIERS)
				  (T
				   (MAPCAN
				     #'(LAMBDA (UNI)
					 (UNI-MERGE1.SUBSTITUTIONS UNI
								   (DS-CLAUSE.LIT.GETPROP IF.CLAUSE.NO IF.LITNO 'IND.CHAIN)
								   (DS-CLAUSE.VARIABLES CLAUSE)))
				     UNIFIERS))))
		      (DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO KIND
					     (PROG
					       (LINKS (ENTRY (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO KIND))
						(NEW.LINKS
						  (MAPCAR #'(LAMBDA (UNI) (CONS IF.CLAUSE (CONS RLINK UNI))) UNIFIERS)))
					       (SETQ LINKS (ASSOC NIL ENTRY))
					       (COND (LINKS (RPLACD LINKS (APPEND NEW.LINKS (COPY-LIST (CDR LINKS)))))
						     (T (SETQ ENTRY (CONS (CONS NIL NEW.LINKS) ENTRY))))
					       (RETURN ENTRY))))))
	 (INTERSECTION (DS-CLAUSE.LINKS 'R CLAUSE LITNO) (DS-CLAUSE.LINKS 'R IF.CLAUSE.NO IF.LITNO)))))))

(DEFUN SEL=PUT.MATCH.CHAIN (CLAUSE LITNO MATCH.LITERALS)
						; edited: 26-may-83 17:23:48
						; input:  clause and litno denote a literal.
						;         matching.literals is a list of literalno.
						;         of the matching terms of clause.
						; effect: the unifiers of the links between the
						;         matching literals and the reflexivity-clause
						;         are merged and the result is stored in  the
						;         propertylist of the literal denoted by
						;         clause and litno.
						; value:  undefined.
  (PROG (LINK MERGE.UNIFIERS UNIFIED.TERMLIST)
	(MAPC
	  #'(LAMBDA (LIT)
	      (SETQ LINK
		    (CAR
		      (REMOVE-IF-NOT
			#'(LAMBDA (LINK) (MEMBER 'REFLEXIVITY (DS-CLAUSE.ATTRIBUTES (DS-LINK.OTHERPAR LINK CLAUSE))))
			(DS-CLAUSE.LINKS 'R CLAUSE LIT))))
	      (SETQ MERGE.UNIFIERS (CAR (UNI-MERGE.SUBSTITUTIONS (CAR (DS-LINK.UNIFIERS LINK)) MERGE.UNIFIERS))))
	  MATCH.LITERALS)
	(DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO 'IND.CHAIN MERGE.UNIFIERS)
	(SETQ UNIFIED.TERMLIST
	      (MAPCAR #'(LAMBDA (TERM) (UNI-APPLY.SUBSTITUTION MERGE.UNIFIERS TERM T)) (DS-CLAUSE.TERMLIST CLAUSE LITNO)))
	(DS-CLAUSE.LIT.PUTPROP CLAUSE LITNO 'IND.TERMLIST UNIFIED.TERMLIST)))

(DEFUN SEL=INDUCTION NIL
						; edited:  9-feb-84 10:29:54
						; input:   none.
						; effect:  this is the main function which works off
						;          the informations computed by
						;          sel=induction.activate?. the information is
						;          stored in a common variable called
						;          sel*ind.chain. its a list of following
						;          elements: (indicator link.unifier
						;          terminator.chain terminator.calls .
						;          subnodes). indicator is one of the atoms
						;          deduce or merge. link.unifier is the actual
						;          link with its unifier.terminator.chain is a
						;          deduction.chain which is used by sel=chain
						;          before link.unifier is used. calls is a
						;          dotted.pair of a if.clause and literals.
						;          a deduction.chain is computed, which
						;          removes literals from the if.clause and
						;          together with the terminator.chain this
						;          chain is used by sel=chain.
						; value:   undefined.
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE
      (COND
        ((NULL SEL*IND.CHAIN) (SEL=INDUCTION.ACTIVATE?)
	 (COND ((NULL SEL*IND.CHAIN) (SETQ SEL*TERMINATOR.LOOK NIL) (SEL=PASS.CONTROL NIL NIL)))))
      (COND
        ((NULL SEL*IND.SINGLE.LINK)
	 (SETQ SEL*IND.ACTUAL.ENTRY
	       (COND
		 ((NUMBERP (CAAR SEL*IND.CHAIN))
		  (OR (SEL=GET.SUBNODE 'MERGE SEL*IND.CHAIN) (SEL=GET.SUBNODE 'DEDUCE SEL*IND.CHAIN)))
		 (T SEL*IND.CHAIN)))
	 (PROG ((CHAIN.LINKS (SEL=PREPARE.EVAL.CHAINS SEL*IND.ACTUAL.ENTRY)))
	       (SETQ SEL*IND.SINGLE.LINK (CDR CHAIN.LINKS))
	       (SETQ SEL*IND.OLD.LINKS (CONS (CAR SEL*IND.SINGLE.LINK) SEL*IND.OLD.LINKS))
	       (COND
		 ((CAR CHAIN.LINKS) (SETQ SEL*CHAIN (CAR CHAIN.LINKS)) (SETQ SEL*CHAIN.DELETE.INTERMEDIATE.RESULTS T)
		  (SEL=PASS.CONTROL '(SEL=CHAIN) NIL T))))))
      (COND
        (SEL*IND.SINGLE.LINK (sEL=REMOVE.LINK.FROM.EVAL (CAR SEL*IND.SINGLE.LINK))
			     (SEL=MAKE.DEDUCTION.CODE (CAR SEL*IND.SINGLE.LINK) (CDR SEL*IND.SINGLE.LINK))
			     (SETQ SEL*IND.SINGLE.LINK NIL)
			     (SEL=RETURN))))
    (REDUCE
      (COND
        ((NULL SEL*IND.SINGLE.LINK)
	 (COND
	   ((NULL SEL*IND.CHAIN) (SEL=MAKE.REDUCTION.CODE SEL*IND.NEW.CLAUSES SEL*IND.OLD.CLAUSES NIL)
	    (SETQ SEL*IND.OLD.CLAUSES NIL) (SETQ SEL*IND.OLD.LINKS NIL)
	    (SETQ SEL*CHAIN.UNITS (SET-DIFFERENCE SEL*CHAIN.UNITS SEL*IND.OLD.CLAUSES)))
	   (T (SEL=MAKE.REDUCTION.CODE NIL NIL NIL)))
	 (SEL=RETURN))))
    (OTHERWISE (ERROR "illegal modus of sel*reduce.deduce in sel=induction : ~a" NIL))))

(DEFUN SEL=PREPARE.EVAL.CHAINS (ACTUAL.ENTRY)
						; edited:  9-oct-83 18:42:08
						; input:   actual.entry is a pointer to a top level
						;          element of sel*ind.chain:
						;          (indicator link.unifier terminator-chain
						;           terminator-calls . subnodes)
						; effect:  the pointed entry is replaced by the list
						;          of subnodes. the deduction-chain for
						;          terminator-calls is computed and merged
						;          with the terminator-chain to one chain.
						; value:   a dotted pair x, where (car x)is this chain
						;          and (cdr x) is the link to resolve, param.
						;          upon.
  (PROG (TERMS CALLS LINK.UNIFIER KIND (ENTRY (CDAR ACTUAL.ENTRY))) (SETQ LINK.UNIFIER (CAR ENTRY)) (SETQ TERMS (SECOND ENTRY))
	(SETQ CALLS (THIRD ENTRY)) (SETQ KIND (CAAR ACTUAL.ENTRY)) (RPLACA ACTUAL.ENTRY (CDDDR ENTRY))
	(COND
	  (CALLS
	   (PROG ((CHAIN (SEL=GET.REFUTATION.CHAIN (CAR ACTUAL.ENTRY) LINK.UNIFIER CALLS KIND)))
		 (COND ((CAR CHAIN) (SETQ TERMS (CONS (CAR CHAIN) TERMS))) (T (SETQ LINK.UNIFIER (CDR CHAIN)))))))
	(RETURN
	  (PROG (CHAIN PRE.CHAINS MASTER.CHAIN MERGE.UNIFIER CLAUSE)
		(MAPC
		  #'(LAMBDA (CHAINS)
		      (COND
			((CDR CHAINS) (SETQ CHAIN (LASTN CHAINS 1)) (SETQ PRE.CHAINS (NCONC PRE.CHAINS (CAR CHAIN)))
			 (SETQ CHAIN (SECOND CHAIN)))
			(T (SETQ CHAIN (CAR CHAINS))))
		      (SETQ MASTER.CHAIN
			    (LIST (CAR (UNI-MERGE.SUBSTITUTIONS (CAR CHAIN) (CAR MASTER.CHAIN))) (SECOND CHAIN)
				  (NCONC (THIRD CHAIN) (THIRD MASTER.CHAIN)))))
		  TERMS)
		(COND
		  ((EQL KIND 'DEDUCE) (SETQ CLAUSE (SEL=SUBNODE.GET CLAUSE (SECOND (CAR ACTUAL.ENTRY))))
		   (SETQ MERGE.UNIFIER
			 (DS-CLAUSE.LIT.GETPROP CLAUSE
						(DS-LINK.OTHERLITNO (CAR LINK.UNIFIER)
								    (DS-LINK.OTHERPAR (CAR LINK.UNIFIER) CLAUSE)) 'IND.CHAIN))
		   (COND
		     (MASTER.CHAIN
		      (RPLACA MASTER.CHAIN (CAR (UNI-MERGE.SUBSTITUTIONS (CAR MASTER.CHAIN) MERGE.UNIFIER)))))))
		(COND (MASTER.CHAIN (RETURN (CONS (NCONC1 PRE.CHAINS MASTER.CHAIN) LINK.UNIFIER)))
		      (T (RETURN (CONS NIL LINK.UNIFIER))))))))

(DEFUN SEL=GET.REFUTATION.CHAIN (NODE.LIST LINK.UNIFIER CALLS KIND)
						; edited:  9-feb-84 10:59:10
						; input:   node.list - a list of sub.nodes
						;          link.unifier - the actual link with its
						;                         unifier.
						;          calls - a terminator.call,s. sel=induction
						;          kind - one of the atoms deduce or merge.
						; effect & value:
						;          computes refutation-chain for the
						;          terminator-call according to the kind of
						;          refutation (deduce or merge).
  (PROG (IF.CLAUSE RESULT)
	(COND
	  ((EQL KIND 'DEDUCE) (SETQ IF.CLAUSE (CAR CALLS))
	   (SETQ RESULT
		 (CADAR
		   (SEL=GET.REFUTATION (DS-LINK.OTHERPAR (CAR LINK.UNIFIER) (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE))
				       (DS-LINK.OTHERLITNO (CAR LINK.UNIFIER) (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)) (CG-CLAUSES ALL)
				       (LIST (SEL=GET.CILS.LIST LINK.UNIFIER IF.CLAUSE (CDR CALLS))))))
	   (RETURN (CONS (CDR RESULT) (CAAR RESULT))))
	  (T
	   (RETURN
	     (SEL=IS.COMPLETE.REFUTATION
	       (CONS (CONS (CAAR NODE.LIST) (CAR CALLS))
		     (MAPCAR
		       #'(LAMBDA (CLAUSE.LITS)
			   (CONS (CAR CLAUSE.LITS) (CAR (CDR (ASSOC 0 (SEL=SUBNODE.GET LITERALS CLAUSE.LITS))))))
		       (CDR NODE.LIST)))
	       NIL))))))

(DEFUN SEL=GET.REFUTATION (CLAUSE LITNO CLAUSE.LIST CILS.LIST)
						; edited:  9-feb-84 11:03:17
						; input:   clause, litno denote a literal in clause.
						;          clause.list a list of clauses.
						;          cils.list is a list of elements:
						;           clause.litlist.link.unifiers described in
						;           term-1:terminator.
						; effect:  refutation-chains are computed for each
						;          cils such that all literals of clause exept
						;          the literals in litlist are removed from
						;          clause.
						; value :  a list of these refutation-chains.
  (PROG (CHAIN)
	(SETQ CILS.LIST
	      (DREMAP CILS.LIST NIL
		      #'(LAMBDA (CILS.LIST.TAIL)
			  (PROG ((CILS (CAR CILS.LIST.TAIL)))
				(RETURN
				  (COND
				    ((EQL (LIST-LENGTH (SECOND CILS)) (DS-CLAUSE.NOLIT (CAR CILS)))
				     (SETQ CHAIN (CONS (LIST (CAR CILS) (LIST (CDDR CILS))) CHAIN)))
				    (T
				     (DODOWN (RPTN (DS-CLAUSE.NOLIT (CAR CILS)))
				       (COND
					 ((AND (NOT (MEMBER (1+ RPTN) (SECOND CILS)))
					       (EQL 'MERGE
						    (SEL=GET.MERGING.LITERALS CLAUSE LITNO (CAR CILS) (1+ RPTN)
									      (CDR (THIRD CILS)))))
					  (RPLACA (CDR CILS) (CONS (1+ RPTN) (SECOND CILS))))))
				     (COND
				       ((EQL (LIST-LENGTH (SECOND CILS)) (DS-CLAUSE.NOLIT (CAR CILS)))
					(SETQ CHAIN (CONS (LIST (CAR CILS) (LIST (CDDR CILS))) CHAIN)))))))))))
	(RETURN
	  (APPEND CHAIN
		  (COPY-LIST (TERM-1_TERMINATOR CLAUSE.LIST (SEL=IND.GET.OPTION CLAUSES) CILS.LIST
						(SEL=IND.GET.OPTION ITERATION)))))))

(DEFUN SEL=IS.COMPLETE.REFUTATION (CASE UNIFIER)
						; edited:  9-feb-84 11:13:19
						; input:   case - a list of dotted.pairs (clause .
						;                 litno)
						;          unifier - a unifier
						; effect:  computes a refutation-chain which generates
						;          the empty-clause if the clauses in case
						;          are considered as units with litno as their
						;          only literal. the unifiers of this
						;          refutation-chain don't change the variables
						;          occuring in unifier.
						; value:   a refutation.chain .
  (SETQ CASE (DREMAP CASE NIL #'(LAMBDA (CASE.LIST) (NULL (CDAR CASE.LIST)))))
  (PROG (UNIFIERS LINK (CL.LIT1 (CAR CASE)))
	(RETURN
	  (COND
	    ((MEMBER-IF
	       #'(LAMBDA (CL.LIT2)
		   (PROG
		     ((CLAUSE1 (CAR CL.LIT1)) (CLAUSE2 (CAR CL.LIT2)) (LIT1 (CDR CL.LIT1)) (LIT2 (CDR CL.LIT2)))
		     (COND
		       ((SETQ LINK
			      (CAR (INTERSECTION (DS-CLAUSE.LINKS 'R CLAUSE1 LIT1) (DS-CLAUSE.LINKS 'R CLAUSE2 LIT2))))
			(SETQ UNIFIERS (DS-LINK.UNIFIERS LINK)))
		       ((SETQ LINK
			      (CAR
				(INTERSECTION (DS-CLAUSE.LINKS 'RD CLAUSE1 LIT1) (DS-CLAUSE.LINKS 'RD CLAUSE2 LIT2))))
			(SETQ UNIFIERS
			      (UNI-UNIFY.ATOMS (DS-CLAUSE.PREDICATE CLAUSE1 LIT1) (DS-CLAUSE.TERMLIST CLAUSE1 LIT1)
					       (DS-CLAUSE.PREDICATE CLAUSE2 LIT2) (DS-CLAUSE.TERMLIST CLAUSE2 LIT2)))))
		     (COND
		       (LINK
			(RETURN
			  (MEMBER-IF #'(LAMBDA (UNI) (UNI-MERGE1.SUBSTITUTIONS UNIFIER UNI T)) UNIFIERS))))))
	       (CDR CASE))
	     (RETURN (CONS NIL (CONS LINK (CAR (DS-LINK.UNIFIERS LINK))))))
	    ((CDR CASE) (LIST (TERM-3_TERMINATOR (CG-CLAUSES ALL) CASE UNIFIER 1)))))))

(DEFUN SEL=GET.CILS.LIST (LINK.UNIFIER IF.CLAUSE PATTERN)
						; edited:  9-oct-83 19:04:57
						; input:   link.unifier is a eval.link.
						;          if.clause is the entry of a rw.clause.
						;          literals are literalnumbers of rw.clause
						; effect & value:
						;          returns a list, which is compatible to the
						;          cils in term-1_terminator. ignored literals
						;          are match-literals, conclusio-literals and
						;          literals.
  (PROG (LITERALS)
	(COND
	  (PATTERN
	   (MAPC #'(LAMBDA (FLG LITNO) (COND ((NULL FLG) (SETQ LITERALS (CONS LITNO LITERALS))))) PATTERN
		 (SEL=IF.CLAUSE.GET CONDITION IF.CLAUSE)))
	  (T (SETQ LITERALS (SEL=IF.CLAUSE.GET CONDITION IF.CLAUSE))))
	(RETURN
	  (LIST (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)
		(PROG (LITS)
		      (DODOWN (RPTN (DS-CLAUSE.NOLIT (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)))
			(COND ((NOT (MEMBER (1+ RPTN) LITERALS)) (SETQ LITS (CONS (1+ RPTN) LITS)))))
		      (RETURN LITS))
		LINK.UNIFIER))))

(DEFUN SEL=IS.EQUALITY (CLAUSE)
						; edited: 24-jun-82 15:39:59
						; input:  a clause address
						; value:  a list:
						;         (clause eq.litno '(left) pref.litnos)
						;         or (clause eq.litno '(rigth) pref.litnos)
						;         or (clause eq.litno '(left right) pr.litnos
						;         if the following conditions are satisfied:
						;      1. the clause consists of at least two literals
						;      2. one literal is a := =: or :=: preferred
						;         equality.
						;      3. every other literal is preferred.
  (PROG (LITERALS SIDE MATCH.LITERALS COND.LITERALS NOT.NEEDED.LITERALS PREDICATE)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (PROG ((KIND (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'KIND)))
		(COND
		  ((AND (DS-SIGN.IS.POSITIVE (DS-CLAUSE.SIGN CLAUSE (1+ RPTN)))
			(DT-PREDICATE.IS.EQUALITY (SETQ PREDICATE (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))))
			(OR (MEMBER (CAR KIND) '(DEF IMPL INJECTIVITY))
			    (AND (NULL KIND) (string/= (DS-PNAME PREDICATE) "="))))
		   (SETQ LITERALS (CONS (1+ RPTN) LITERALS))
		   (CASE (INTERN (DS-PNAME PREDICATE) (find-package "MKRP"))
		     (\:= (SETQ SIDE '(LEFT)))
		     (=\: (SETQ SIDE '(RIGHT)))
		     (\:=\: (SETQ SIDE '(LEFT RIGHT)))
		     (OTHERWISE (ERROR "illegal predicate-symbol in sel=is.equality: ~a" NIL))))
		  ((MEMBER 'NOT.NEEDED KIND) (SETQ NOT.NEEDED.LITERALS (CONS (1+ RPTN) NOT.NEEDED.LITERALS))
		   (SETQ COND.LITERALS (CONS (1+ RPTN) COND.LITERALS)))
		  ((MEMBER 'MATCH KIND) (SETQ MATCH.LITERALS (CONS (1+ RPTN) MATCH.LITERALS)))
		  ((MEMBER 'CONDITION KIND) (SETQ COND.LITERALS (CONS (1+ RPTN) COND.LITERALS))))))
	(RETURN
	  (COND
	    ((EQL (LIST-LENGTH LITERALS) 1) (SEL=PUT.MATCH.CHAIN CLAUSE (CAR LITERALS) MATCH.LITERALS)
	     (LIST CLAUSE CLAUSE (CAR LITERALS) SIDE MATCH.LITERALS COND.LITERALS NOT.NEEDED.LITERALS NIL NIL))))))

(DEFUN SEL=IS.INDUCTION.HYPOTHESIS (CLAUSE)
						; edited: 24-jun-82 15:39:59
						; input:  a clause address
						; value:  a list:
						;         (clause eq.litno '(left) pref.litnos)
						;         or (clause eq.litno '(rigth) pref.litnos)
						;         or (clause eq.litno '(left right) pr.litnos
						;         if the following conditions are satisfied:
						;      1. the clause consists of at least two literals
						;      2. one literal is a := =: or :=: preferred
						;         equality.
						;      3. every other literal is preferred.
  (COND
    ((MEMBER 'IND (DS-CLAUSE.LIT.GETPROP CLAUSE 1 'KIND))
     (PROG (LITERALS SIDE COND.LITERALS ABORT)
	   (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	     (PROG
	       ((KIND (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'KIND)) (PREDICATE (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))))
	       (COND
		 ((DT-PREDICATE.IS.EQUALITY PREDICATE) (SETQ LITERALS (CONS (1+ RPTN) LITERALS))
		  (SETQ SIDE
			(CASE (INTERN (DS-PNAME PREDICATE) (find-package "MKRP"))
			  ((= \:=\:) '(LEFT RIGHT))
			  (\:= '(LEFT))
			  (=\: '(RIGHT))
			  (OTHERWISE (ERROR "illegal predicate-symbol in sel=is.induction.hypothesis: ~a" NIL)))))
		 ((NULL (CDR KIND)) (SETQ COND.LITERALS (CONS (1+ RPTN) COND.LITERALS))) (T (SETQ ABORT T)))))
	   (COND
	     ((AND (NULL ABORT) (EQL (LIST-LENGTH LITERALS) 1))
	      (RETURN
		(MAPCAR
		  #'(LAMBDA (SIDE)
		      (LIST CLAUSE CLAUSE (CAR LITERALS) (LIST SIDE) NIL COND.LITERALS (COPY-TREE COND.LITERALS) NIL NIL))
		  SIDE))))))))

(DEFUN SEL=IS.PART.OF.EQUIVALENCE (CLAUSE)
						; edited: 24-jun-82 15:39:59
						; input:  a clause address
						; value:  a list:
						;         (clause eq.litno '(left) pref.litnos)
						;         or (clause eq.litno '(rigth) pref.litnos)
						;         or (clause eq.litno '(left right) pr.litnos
						;         if the following conditions are satisfied:
						;      1. the clause consists of at least two literals
						;      2. one literal is a := =: or :=: preferred
						;         equality.
						;      3. every other literal is preferred.
  (PROG (LITERALS CONCLUSIO.LITERALS MATCH.LITERALS COND.LITERALS NOT.NEEDED.LITERALS)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (PROG ((KIND (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'KIND)))
		(COND ((MEMBER (CAR KIND) '(DEF EQV)) (SETQ LITERALS (CONS (1+ RPTN) LITERALS)))
		      ((MEMBER 'NOT.NEEDED KIND) (SETQ NOT.NEEDED.LITERALS (CONS (1+ RPTN) NOT.NEEDED.LITERALS))
		       (SETQ COND.LITERALS (CONS (1+ RPTN) COND.LITERALS)))
		      ((MEMBER 'MATCH KIND) (SETQ MATCH.LITERALS (CONS (1+ RPTN) MATCH.LITERALS)))
		      ((MEMBER 'CONDITION KIND) (SETQ COND.LITERALS (CONS (1+ RPTN) COND.LITERALS)))
		      (T (SETQ CONCLUSIO.LITERALS (CONS (1+ RPTN) CONCLUSIO.LITERALS))))))
	(RETURN
	  (COND
	    ((EQL (LIST-LENGTH LITERALS) 1) (SEL=PUT.MATCH.CHAIN CLAUSE (CAR LITERALS) MATCH.LITERALS)
	     (LIST CLAUSE CLAUSE (CAR LITERALS) CONCLUSIO.LITERALS MATCH.LITERALS COND.LITERALS NOT.NEEDED.LITERALS
		   (PROG ((PREDICATE (DS-CLAUSE.PREDICATE CLAUSE (CAR LITERALS))))
			 (RETURN
			   (NOT
			     (NULL
			       (MEMBER-IF #'(LAMBDA (LIT) (EQL PREDICATE (DS-CLAUSE.PREDICATE CLAUSE LIT))) CONCLUSIO.LITERALS)))))
		   NIL))))))

(DEFUN SEL=IS.TERMINAL.RULE (FUNCTION.SYMBOL TERM)
						; edited: 22-jul-83 10:10:48
						; input:  function.symbol is a address of a function-
						;         symbol.
						; effect & value:
						;         if function.symbol occurs in a subterm of
						;         term an other subterm of term else nil.
  (COND
    ((CONSP TERM)
     (OR (EQL (CAR TERM) FUNCTION.SYMBOL)
	 (NOT
	   (NULL
	     (MEMBER-IF #'(LAMBDA (SUBTERM) (SEL=IS.TERMINAL.RULE FUNCTION.SYMBOL SUBTERM)) (CDR TERM))))))
    (T NIL)))

(DEFUN SEL=SORT.EQUIVALENCES NIL
  (PROG (PRED.DEFS PRED.DEF RW.RULES P-PRED.DEFS M-PRED.DEFS)
	(MAPC
	  #'(LAMBDA (IF.CLAUSE)
	      (PROG
		((CLAUSE (SEL=IF.CLAUSE.GET CLAUSE IF.CLAUSE)) (LITNO (SEL=IF.CLAUSE.GET LITNO IF.CLAUSE)) PROPERTY SIGN KIND
		 DEF)
		(SETQ PROPERTY (DS-CLAUSE.LIT.GETPROP CLAUSE LITNO 'KIND)) (SETQ SIGN (DS-CLAUSE.SIGN CLAUSE LITNO))
		(SETQ KIND (SECOND PROPERTY)) (SETQ DEF (THIRD PROPERTY))
		(COND
		  ((SETQ PRED.DEF
			 (ASSOC DEF (SETQ PRED.DEFS (COND ((DS-SIGN.IS.POSITIVE SIGN) P-PRED.DEFS) (T M-PRED.DEFS)))))
		   (COND
		     ((SETQ RW.RULES (CDR (ASSOC KIND (CDR PRED.DEF))))
		      (PUTASSOC KIND (CONS IF.CLAUSE RW.RULES) (CDR PRED.DEF)))
		     (T (RPLACD PRED.DEF (CONS (LIST KIND IF.CLAUSE) (CDR PRED.DEF))))))
		  ((DS-SIGN.IS.POSITIVE SIGN) (SETQ P-PRED.DEFS (CONS (LIST DEF (LIST KIND IF.CLAUSE)) P-PRED.DEFS)))
		  (T (SETQ M-PRED.DEFS (CONS (LIST DEF (LIST KIND IF.CLAUSE)) M-PRED.DEFS))))))
	  (SEL=CLAUSES EQUIVALENCE))
	(SETQ PRED.DEFS (NCONC P-PRED.DEFS M-PRED.DEFS))
	(MAPC
	  #'(LAMBDA (PRED.DEF)
	      (MAPC #'(LAMBDA (CLAUSE.SET) (SEL=CONNECT.EQUIVALENCES (CDR CLAUSE.SET))) (CDR PRED.DEF))
	      (SEL=COMPUTE.ALL.NEW.IF.CLAUSES
		(MAPCAN
		  #'(LAMBDA (ENT.LIST)
		      (COND ((SEL=IF.CLAUSE.GET NOT.NEEDED (SECOND ENT.LIST)) (LIST (SECOND ENT.LIST)))))
		  (CDR PRED.DEF))
		T))
	  PRED.DEFS)))

(DEFUN SEL=SORT.EQUALITIES NIL
  (PROG (FUNC.DEFS FUNC.DEF PROPERTY FUNC.NAME)
	(MAPC
	  #'(LAMBDA (ENTRY)
	      (SETQ PROPERTY
		    (DS-CLAUSE.LIT.GETPROP (SEL=IF.CLAUSE.GET CLAUSE ENTRY) (SEL=IF.CLAUSE.GET LITNO ENTRY) 'KIND))
	      (SETQ FUNC.NAME (THIRD PROPERTY))
	      (COND
		((SETQ FUNC.DEF (ASSOC FUNC.NAME FUNC.DEFS)) (PUTASSOC FUNC.NAME (CONS ENTRY (CDR FUNC.DEF)) FUNC.DEFS))
		(T (SETQ FUNC.DEFS (CONS (LIST FUNC.NAME ENTRY) FUNC.DEFS)))))
	  (SEL=CLAUSES EQUALITY))
	(MAPC
	  #'(LAMBDA (FUNC.DEF)
	      (SEL=COMPUTE.ALL.NEW.IF.CLAUSES
		(REMOVE-IF-NOT #'(LAMBDA (ENT) (SEL=IF.CLAUSE.GET NOT.NEEDED ENT)) FUNC.DEF) NIL))
	  FUNC.DEFS)))

(DEFUN SEL=CONNECT.EQUIVALENCES (ENTRIES)
  (COND
    ((CDR ENTRIES)
     (PROG
       ((CLAUSE1 (SEL=IF.CLAUSE.GET CLAUSE (CAR ENTRIES))) (LITNO1 (SEL=IF.CLAUSE.GET LITNO (CAR ENTRIES)))
	(MATCH (SEL=IF.CLAUSE.GET MATCH (CAR ENTRIES))) (CONDITION (SEL=IF.CLAUSE.GET CONDITION (CAR ENTRIES))) RESULT CLAUSE2
	LITNO2 UNIFIER)
       (MAPC
	 #'(LAMBDA (OTHERENTRY) (SETQ CLAUSE2 (SEL=IF.CLAUSE.GET CLAUSE OTHERENTRY))
		   (SETQ LITNO2 (SEL=IF.CLAUSE.GET LITNO OTHERENTRY))
		   (SETQ UNIFIER (SEL=GET.VARIABLE.RENAMING CLAUSE1 LITNO1 NIL CLAUSE2 LITNO2 NIL))
		   (SETQ RESULT
			 (SEL=GET.EQUIVALENT.LITERALS CLAUSE1 MATCH CLAUSE2 (SEL=IF.CLAUSE.GET MATCH OTHERENTRY) UNIFIER))
		   (SETQ UNIFIER (CAR RESULT)) (SEL=IF.CLAUSE.PUT MATCH OTHERENTRY (SECOND RESULT))
		   (SEL=IF.CLAUSE.PUT CONDITION OTHERENTRY
				      (SECOND
					(SEL=GET.EQUIVALENT.LITERALS CLAUSE1 CONDITION CLAUSE2
								     (SEL=IF.CLAUSE.GET CONDITION OTHERENTRY) UNIFIER))))
	 (CDR ENTRIES))
       (SEL=IF.CLAUSE.PUT SUB.CASES (CAR ENTRIES) (CDR ENTRIES))
       (MAPC #'(LAMBDA (ENTRY) (SEL=REMOVE.CLAUSE (SEL=IF.CLAUSE.GET NAME ENTRY) EQUIVALENCE)) (CDR ENTRIES))))))

(DEFUN SEL=COMPUTE.ALL.NEW.IF.CLAUSES (ENTRIES EQVFLG)
  (PROG (NEW.ENTRYLIST LEN NEW.IF.CLAUSES)
	(MAPC
	  #'(LAMBDA (ENTRY)
	      (SETQ LEN
		    (+ (LIST-LENGTH (SEL=IF.CLAUSE.GET MATCH ENTRY)) (LIST-LENGTH (SEL=IF.CLAUSE.GET CONDITION ENTRY))))
	      (SETQ NEW.ENTRYLIST (INSASSOC (LIST LEN ENTRY) NEW.ENTRYLIST)))
	  ENTRIES)
	(SETQ NEW.ENTRYLIST (SORT NEW.ENTRYLIST #'(LAMBDA (X Y) (> (CAR X) (CAR Y)))))
	(SMAPC
	  #'(LAMBDA (LEN.ENTRIES) (SETQ NEW.IF.CLAUSES (SEL=COMPUTE.NEW.IF.CLAUSES (CDR LEN.ENTRIES) EQVFLG)))
	  #'(LAMBDA (ENTRY.TAIL)
	      (COND
		(NEW.IF.CLAUSES (SETQ LEN (1- (CAAR ENTRY.TAIL)))
				(SETQ ENTRY.TAIL (INSASSOC (CONS LEN NEW.IF.CLAUSES) ENTRY.TAIL)) (SETQ NEW.IF.CLAUSES NIL)))
	      (CDR ENTRY.TAIL))
	  NEW.ENTRYLIST)))

(DEFUN SEL=COMPUTE.NEW.IF.CLAUSES (ENTRIES EQVFLG)
  (PROG
    (CLAUSE LITNO SIDE MATCH CONDITION CLAUSE2 UNIFIER MATCH.RES COND.RES CASE.LIST ENTRIES.TO.REMOVE ENTRY)
    (SMAPL
      #'(LAMBDA (ENTRYLIST) (SETQ ENTRY (CAR ENTRYLIST)) (SETQ CLAUSE (SEL=IF.CLAUSE.GET CLAUSE ENTRY))
		(SETQ LITNO (SEL=IF.CLAUSE.GET LITNO ENTRY)) (SETQ SIDE (SEL=IF.CLAUSE.GET SIDE ENTRY))
		(SETQ MATCH (SEL=IF.CLAUSE.GET MATCH ENTRY)) (SETQ CONDITION (SEL=IF.CLAUSE.GET CONDITION ENTRY))
		(MAPC
		  #'(LAMBDA (ENTRY2) (SETQ CLAUSE2 (SEL=IF.CLAUSE.GET CLAUSE ENTRY2))
			    (SETQ UNIFIER
				  (SEL=GET.VARIABLE.RENAMING CLAUSE LITNO SIDE CLAUSE2 (SEL=IF.CLAUSE.GET LITNO ENTRY2)
							     (SEL=IF.CLAUSE.GET SIDE ENTRY2)))
			    (SETQ MATCH.RES
				  (SEL=GET.EQUIVALENT.LITERALS CLAUSE MATCH CLAUSE2 (SEL=IF.CLAUSE.GET MATCH ENTRY2) UNIFIER))
			    (COND
			      ((AND MATCH.RES (NULL (THIRD MATCH.RES)))
			       (SETQ COND.RES
				     (SEL=GET.EQUIVALENT.LITERALS CLAUSE CONDITION CLAUSE2
								  (SEL=IF.CLAUSE.GET CONDITION ENTRY2) (CAR MATCH.RES)))
			       (COND
				 (COND.RES
				  (SETQ CASE.LIST
					(INSASSOC
					  (LIST (THIRD COND.RES)
						(LIST (SECOND MATCH.RES) (SECOND COND.RES) (FOURTH COND.RES) ENTRY2 (CAR COND.RES)))
					  CASE.LIST)))))))
		  (CDR ENTRIES))
		(PROG (UNIFIER CLAUSE.LITS)
		      (MAPC
			#'(LAMBDA (CASE) (SETQ UNIFIER NIL) (SETQ CLAUSE.LITS (LIST (CONS CLAUSE (CAR CASE))))
				  (MAPC
				    #'(LAMBDA (IF.CLAUSE)
					(SETQ CLAUSE.LITS
					      (CONS (CONS (SEL=IF.CLAUSE.GET CLAUSE (FOURTH IF.CLAUSE)) (THIRD IF.CLAUSE))
						    CLAUSE.LITS))
					(SETQ UNIFIER (CAR (UNI-MERGE1.SUBSTITUTIONS UNIFIER (CAR (CDDDDR IF.CLAUSE)) T))))
				    (CDR CASE))
				  (COND
				    ((SEL=IS.COMPLETE.REFUTATION CLAUSE.LITS UNIFIER)
				     (SEL=INSERT.NEW.IF.CLAUSE
				       (CONS
					 (LIST (SEL=IF.CLAUSE.GET MATCH ENTRY)
					       (REMOVE (CAR CASE) (SEL=IF.CLAUSE.GET CONDITION ENTRY)) (CAR CASE) ENTRY)
					 (CDR CASE))
				       EQVFLG)
				     (MAPC
				       #'(LAMBDA (IF.CLAUSE)
					   (SETQ ENTRIES.TO.REMOVE (CONS (FOURTH IF.CLAUSE) ENTRIES.TO.REMOVE)))
				       (CDR CASE)))))
			CASE.LIST)))
      #'(LAMBDA (ENTRYLIST)
          (PROG1 (SET-DIFFERENCE (CDR ENTRYLIST) ENTRIES.TO.REMOVE) (SETQ ENTRIES.TO.REMOVE NIL)))
      ENTRIES)))

(DEFUN SEL=INSERT.NEW.IF.CLAUSE (ENTRYLIST EQVFLG)
  (PROG ((PMATCH (CAAR ENTRYLIST))
	 (PCONDITION (CADAR ENTRYLIST))
	 (ENTRY (FOURTH (CAR ENTRYLIST))) CONDITION RESULT)
	(SETQ CONDITION (SEL=IF.CLAUSE.GET CONDITION ENTRY))
	(MAPC
	  #'(LAMBDA (ENTR) (SEL=SUBST.LITLIST (CAR ENTR) (FOURTH ENTR) 'MATCH)
		    (SEL=SUBST.LITLIST (CONS (THIRD ENTR) (SECOND ENTR)) (FOURTH ENTR) CONDITION)
		    (COND (EQVFLG (SEL=REMOVE.CLAUSE (SEL=IF.CLAUSE.GET NAME (FOURTH ENTR)) EQUIVALENCE))
			  (T (SEL=REMOVE.CLAUSE (SEL=IF.CLAUSE.GET NAME (FOURTH ENTR)) EQUALITY))))
	  ENTRYLIST)
	(SETQ SEL*IND.CASE.COUNTER (1+ SEL*IND.CASE.COUNTER))
	(SETQ RESULT
	      (LIST (INTERN (COERCE (LIST 'C SEL*IND.CASE.COUNTER) 'STRING) (find-package "MKRP"))
		    (SEL=IF.CLAUSE.GET CLAUSE ENTRY)
		    (SEL=IF.CLAUSE.GET LITNO ENTRY) (SEL=IF.CLAUSE.GET SIDE ENTRY) PMATCH PCONDITION
		    (REMOVE (THIRD (CAR ENTRYLIST)) (SEL=IF.CLAUSE.GET NOT.NEEDED ENTRY)) T (MAPCAR #'CADDDR ENTRYLIST)))
	(COND (EQVFLG (SEL=INSERT.CLAUSE RESULT T EQUIVALENCE))
	      (T (SEL=INSERT.CLAUSE RESULT T EQUALITY)))))

(DEFUN SEL=GET.EQUIVALENT.LITERALS (CLAUSE1 LITLIST1 CLAUSE2 LITLIST2 UNIFIER)
						; edited: 26-jan-84 11:36:04
						; input:  clause1 and clause2 are two clauseaddresses.
						;         litlist1 and litlist2 are two literallists
						;         of clause1 resp. clause2. unifier is a
						;         renaming of variables. match? is a flag.
						; effect: if litlist1 = (l1,...,ln) then
						;         a permutation (k1,...,kn) is computed, such
						;         that li and ki are equivalent modulo
						;         unifier. if match? = t new renamings of
						;         variables are computed if needed.
						; value:  if no permutation exists nil, else a list
						;         of the new unifier, the permutation,
						;         a literal of litlist1, which has no
						;         corresponedent equivalent literal in lit-
						;         list2, and analoguos a literal in litlist2.
  (PROG (MATCHER NEW.LITLIST DIFFER.LIT ABORT)
	(SMAPC
	  #'(LAMBDA (LIT1)
	      (COND
		((MEMBER-IF
		   #'(LAMBDA (LIT2)
		       (COND
			 ((SETQ MATCHER (SEL=LITERALS.ARE.EQUIVALENT CLAUSE1 LIT1 CLAUSE2 LIT2 UNIFIER))
			  (SETQ UNIFIER (CAR MATCHER)) (SETQ NEW.LITLIST (NCONC1 NEW.LITLIST LIT2)))
			 (DIFFER.LIT (SETQ ABORT T))
			 (T (SETQ DIFFER.LIT LIT1) (SETQ NEW.LITLIST (NCONC NEW.LITLIST NIL)))))
		   LITLIST2))))
	  #'(LAMBDA (LITLIST) (COND (ABORT NIL) (T (CDR LITLIST)))) LITLIST1)
	(RETURN
	  (COND (ABORT NIL) (T (LIST UNIFIER NEW.LITLIST DIFFER.LIT (CAR (SET-DIFFERENCE LITLIST2 NEW.LITLIST))))))))

(DEFUN SEL=LITERALS.ARE.EQUIVALENT (CLAUSE1 LIT1 CLAUSE2 LIT2 UNIFIER)
  (PROG ((LINK (CAR (INTERSECTION (DS-CLAUSE.LINKS 'S CLAUSE1 LIT1) (DS-CLAUSE.LINKS 'S CLAUSE2 LIT2)))))
	(RETURN
	  (COND
	    ((AND (MEMBER 'MATCH (DS-CLAUSE.LIT.GETPROP CLAUSE1 LIT1 'KIND))
		  (MEMBER 'MATCH (DS-CLAUSE.LIT.GETPROP CLAUSE2 LIT2 'KIND)) LINK)
	     (MEMBER-IF #'(LAMBDA (UNI) (UNI-MERGE1.SUBSTITUTIONS UNIFIER UNI T)) (DS-LINK.UNIFIERS LINK)))
	    ((AND LINK
		  (MEMBER-IF #'(LAMBDA (UNI) (COND ((UNI-INSTANCE.IS UNIFIER UNI)))) (DS-LINK.UNIFIERS LINK)))
	     (LIST UNIFIER))))))

(DEFUN SEL=SUBST.LITLIST (PLIST2 ENTRY INDICATOR)
  (PROG (PLIST LIST2)
	(COND
	  ((EQL INDICATOR 'MATCH) (SETQ LIST2 (SEL=IF.CLAUSE.GET MATCH ENTRY)) (SEL=IF.CLAUSE.PUT MATCH ENTRY PLIST2))
	  (T (SETQ LIST2 (SEL=IF.CLAUSE.GET CONDITION ENTRY)) (SEL=IF.CLAUSE.PUT CONDITION ENTRY PLIST2)))
	(MAPC
	  #'(LAMBDA (SUB.ENTRY)
	      (SETQ PLIST
		    (SEL=PERMUTE.LITLIST LIST2 PLIST2
					 (COND ((EQL INDICATOR 'MATCH) (SEL=IF.CLAUSE.GET MATCH SUB.ENTRY))
					       (T (SEL=IF.CLAUSE.GET CONDITION SUB.ENTRY)))))
	      (SEL=SUBST.LITLIST PLIST SUB.ENTRY INDICATOR))
	  (SEL=IF.CLAUSE.GET SUB.CASES ENTRY))))

(DEFUN SEL=PERMUTE.LITLIST (LIST1 PLIST1 LIST2)
  (PROG (PLIST2 (LEN (LIST-LENGTH LIST2)))
	(MAPC
	  #'(LAMBDA (EL)
	      (DODOWN (RPTN LEN)
		(COND
		  ((EQL (CAR (NTHCDR (1- (1+ RPTN)) LIST1)) EL) (SETQ PLIST2 (NCONC1 PLIST2 (CAR (NTHCDR (1- (1+ RPTN)) LIST2))))
		   (SETQ RPTN 0)))))
	  PLIST1)
	(RETURN (NCONC (SET-DIFFERENCE LIST2 PLIST2) PLIST2))))

(DEFUN SEL=GET.VARIABLE.RENAMING (CLAUSE1 LIT1 SIDE1 CLAUSE2 LIT2 SIDE2)
  (let ((PRED1 (DS-CLAUSE.PREDICATE CLAUSE1 LIT1))
	(PRED2 (DS-CLAUSE.PREDICATE CLAUSE2 LIT2))
	(TERMLIST1 (DS-CLAUSE.TERMLIST CLAUSE1 LIT1))
	(TERMLIST2 (DS-CLAUSE.TERMLIST CLAUSE2 LIT2)))
    (CAR (COND ((DT-PREDICATE.IS.EQUALITY PRED1)
		(UNI-UNIFY1.TERMS (COND ((MEMBER 'LEFT SIDE1) (CAR TERMLIST1)) (T (SECOND TERMLIST1)))
				  (COND ((MEMBER 'LEFT SIDE2) (CAR TERMLIST2)) (T (SECOND TERMLIST2))) T))
	       (T (UNI-UNIFY1.ATOMS PRED1 TERMLIST1 PRED2 TERMLIST2 T))))))

(DEFUN SEL=GET.MERGING.LITERALS (CLAUSE LITNO RW.CLAUSE RW.LITNO UNIFIER)
						; edited: 23-jul-83 10:45:34
						; input:  clause litno and rw.clause rw.litno denote
						;         two literals. unifier is a unifier s of a
						;         link between clause and rw.clause.
						; effect: all literals of clause exept the litno'th
						;         are compared with the rw.litno'th literal
						;         of rw.clause, whether s(lit1) = s(lit2) .
						; value : 'merge if there exist such a literal and
						;         the literals have same signs; if they have
						;         opposite signs then 'tautology else nil.
  (PROG (RESULT LINK)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (COND
	    ((NEQ (1+ RPTN) LITNO)
	     (COND
	       ((AND
		  (SETQ LINK
			(CAR
			  (INTERSECTION (DS-CLAUSE.LINKS 'R CLAUSE (1+ RPTN)) (DS-CLAUSE.LINKS 'R RW.CLAUSE RW.LITNO))))
		  (MEMBER-IF #'(LAMBDA (UNI) (UNI-INSTANCE.IS UNIFIER UNI)) (DS-LINK.UNIFIERS LINK)))
		(SETQ RPTN 0) (SETQ RESULT 'TAUTOLOGY))
	       ((MEMBER-IF #'(LAMBDA (UNI) (UNI-INSTANCE.IS UNIFIER UNI))
			   (UNI-UNIFY.ATOMS (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)) (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN))
					    (DS-CLAUSE.PREDICATE RW.CLAUSE RW.LITNO) (DS-CLAUSE.TERMLIST RW.CLAUSE RW.LITNO)))
		(SETQ RESULT 'MERGE) (SETQ RPTN 0))))))
	(RETURN RESULT)))

(DEFMACRO SEL=IF.CLAUSE.GET (CLASSIFICATION IF.CLAUSE*)
  (CASE CLASSIFICATION
    (NAME `(CAR ,IF.CLAUSE*))
    (CLAUSE `(SECOND ,IF.CLAUSE*))
    (LITNO `(THIRD ,IF.CLAUSE*))
    ((SIDE CONCLUSIO) `(FOURTH ,IF.CLAUSE*))
    (MATCH `(SECOND (CDDDR ,IF.CLAUSE*)))
    (NOT.NEEDED `(THIRD (CDDDR ,IF.CLAUSE*)))
    (CONDITION `(FOURTH (CDDDR ,IF.CLAUSE*)))
    (RECURSION `(SECOND (CDDDR (CDDDR ,IF.CLAUSE*))))
    (SUB.CASES `(THIRD (CDDDR (CDDDR ,IF.CLAUSE*))))
    (OTHERWISE (ERROR "illegal classifiaction in sel=if.clause.get: ~a" CLASSIFICATION))))

(DEFMACRO SEL=IF.CLAUSE.PUT (CLASSIFICATION IF.CLAUSE* VALUE*)
  (CASE CLASSIFICATION
    (NAME `(SETF (FIRST ,IF.CLAUSE*) ,VALUE*))
    (CLAUSE `(SETF (SECOND ,IF.CLAUSE*) ,VALUE*))
    (LITNO `(SETF (THIRD ,IF.CLAUSE*) ,VALUE*))
    ((SIDE CONCLUSIO) `(SETF (FOURTH ,IF.CLAUSE*) ,VALUE*))
    (MATCH `(SETF (FIFTH ,IF.CLAUSE*) ,VALUE*))
    (NOT.NEEDED `(SETF (SIXTH ,IF.CLAUSE*) ,VALUE*))
    (CONDITION `(SETF (SEVENTH ,IF.CLAUSE*) ,VALUE*))
    (RECURSION `(SETF (EIGHTH ,IF.CLAUSE*) ,VALUE*))
    (SUB.CASES `(SETF (NINTH ,IF.CLAUSE*) ,VALUE*))
    (OTHERWISE (ERROR "illegal classifiaction in sel=if.clause.get: ~a" CLASSIFICATION))))

(DEFMACRO SEL=SUBNODE.GET (CLASSIFICATION SUBNODE*)
						; edited: 10-feb-84 10:51:40
						; input:  classification - an atom
						;         subnode - a subnode of the trans.-tree.
						; value:  the classification-entry of the subnode
  (CASE CLASSIFICATION
    (CLAUSE `(CAR ,SUBNODE*))
    (LITERALS `(CDR ,SUBNODE*))
    (OTHERWISE (ERROR "illegal classification in sel=subnode.get : ~a" CLASSIFICATION))))

(DEFMACRO SEL=SUBNODE.PUT (CLASSIFICATION SUBNODE* VALUE*)
						; edited: 10-feb-84 10:51:40
						; input:  classification - an atom
						;         subnode - a subnode of the trans.-tree.
						;         value - a sexpression.
						; effect: changes the classification-entry of subnode
						;         to value.
						; value:  undefined.
  (CASE CLASSIFICATION
    (CLAUSE `(RPLACA ,SUBNODE* ,VALUE*))
    (LITERALS `(RPLACD ,SUBNODE* ,VALUE*))
    (OTHERWISE (ERROR "illegal classification in sel=subnode.put : ~a" CLASSIFICATION))))

(DEFMACRO SEL=IND.GET.OPTION (INDICATOR)
						; edited:  9-feb-84 10:25:22
						; input:   indicator - one of the atoms depth, kind,
						;          impl, iteration or clauses.
						; value:   value of the denoted option.
  (CASE INDICATOR
    (DEPTH `(CAR SEL*IND.OPTIONS))
    (KIND `(SECOND SEL*IND.OPTIONS))
    (IMPL '(THIRD SEL*IND.OPTIONS))
    (ITERATION '(FOURTH SEL*IND.OPTIONS))
    (CLAUSES '(SECOND (CDDDR SEL*IND.OPTIONS)))
    (OTHERWISE (ERROR "illegal indicator in sel=ind.get.option: ~a" NIL))))

(DEFMACRO SEL=IND.PUT.OPTION (INDICATOR VALUE)
						; edited:  9-feb-84 10:26:59
						; input:   indicator - one of the atoms depth, kind,
						;          impl, iteration or clauses.
						;          value - a sexpression
						; effect:  replaces actual option of indicator by
						;          value.
  `(RPLACA
     ,(CASE INDICATOR
	(DEPTH `SEL*IND.OPTIONS)
	(KIND `(CDR SEL*IND.OPTIONS))
	(IMPL `(CDDR SEL*IND.OPTIONS))
	(ITERATION `(CDDDR SEL*IND.OPTIONS))
	(CLAUSES `(CDDDDR SEL*IND.OPTIONS))
	(OTHERWISE (ERROR "illegal indicator in sel=ind.get.option: ~a" NIL)))
     ,VALUE))

(DEFUN SEL=RESET.TREE NIL
						; edited:  7. 2. 1984    d.h.
						; input:   none.
						; effect:  resets pointer of the transformation-tree
						;          to the first node of sel*actual.clauses
						; value :  undefined.
  (SETQ SEL*TREE.POINTER.1 SEL*ACTUAL.CLAUSES)
  (SETQ SEL*TREE.POINTER.2 (CDDAR SEL*TREE.POINTER.1)))

(DEFUN SEL=NEXT.NODE NIL
						; edited:  7. 2. 1984    d.h.
						; input:   none.
						; effect:  moves node-pointer of the transformation-
						;          tree to the next node of sel*actual.clauses
						; value:   tail of the nodelist beginning at the
						;          actual pointer of node.
  (COND
    ((SETQ SEL*TREE.POINTER.1 (CDR SEL*TREE.POINTER.1)) (SETQ SEL*TREE.POINTER.2 (CDDAR SEL*TREE.POINTER.1))
     (CDDAR SEL*TREE.POINTER.1))
    (T 'NOT.DEFINED)))

(DEFUN SEL=NEXT.SUBNODE NIL
						; edited:  7. 2. 1984    d.h.
						; input:   none.
						; effect:  moves subnode-pointer of the transformation
						;          tree to the next subnode of the actual node
						;          if there are no more subnodes of the actual
						;          node the node-pointer is moved to the next
						;          node of sel*actual.clauses.
						; value:   tail of the subnodelist beginning at the
						;          actual pointer of subnode.
  (COND ((SETQ SEL*TREE.POINTER.2 (CDR SEL*TREE.POINTER.2)) SEL*TREE.POINTER.2)
	((SETQ SEL*TREE.POINTER.1 (CDR SEL*TREE.POINTER.1)) (SETQ SEL*TREE.POINTER.2 (CDDAR SEL*TREE.POINTER.1))
	 SEL*TREE.POINTER.2)))

(DEFUN SEL=ACTUAL.NODE NIL
						; edited:  9-feb-84 10:03:24
						; input:   none.
						; effect & value: returns actual node.
  (CAR SEL*TREE.POINTER.1))

(DEFUN SEL=ACTUAL.SUBNODE NIL
						; edited:  9-feb-84 10:03:24
						; input:   none.
						; effect & value: returns the list of the actual
						;          subnodes.
  SEL*TREE.POINTER.2)

(DEFUN SEL=GET.SUBNODE (CLAUSE NODE.LIST)
						; edited:  9-feb-84 10:22:28
						; input:   clause - a clause.address.
						;          node.list - a list of nodes.
						; effect:  if node.list is nil the actual trans.-tree
						;          is considered.
						; value :  a pointer to a subnode, which contains
						;          clause.
  (COND ((NULL NODE.LIST) (SETQ NODE.LIST SEL*ACTUAL.CLAUSES)))
  (PROG (POINTER)
	(MEMBER-IF
	  #'(LAMBDA (ENTRY)
	      (SETQ POINTER
		    (MEMBER-IF #'(LAMBDA (CL.LITS) (AND (CONSP CL.LITS) (EQL (CAR CL.LITS) CLAUSE))) (CDDR ENTRY))))
	  NODE.LIST)
	(RETURN POINTER)))

(DEFUN SEL=SUBTREE.OF (CASENO SUPERCASENO CLAUSE)
						; edited:  9-feb-84 10:18:44
						; input:   caseno and supercaseno are to case-numbers.
						;          clause - a clauseaddress.
						; effect & value:
						;          returns a case-number of a subtree of
						;          the tree denoting by caseno which
						;          contains clause.
  (PROG ((ENTRY (ASSOC CASENO SEL*ACTUAL.CLAUSES)) NO)
	(RETURN
	  (COND
	    ((MEMBER-IF
	       #'(LAMBDA (CL.LITS)
		   (COND ((CONSP CL.LITS) (EQL CLAUSE (CAR CL.LITS)))
			 (T (SETQ NO (SEL=SUBTREE.OF CL.LITS (COND (SUPERCASENO) (T CL.LITS)) CLAUSE)))))
	       (CDDR ENTRY))
	     (COND ((NULL SUPERCASENO) (SETQ SUPERCASENO NO))) SUPERCASENO)))))

(DEFUN SEL=REPLACE.NODE (SUBNODE NODE)
						; edited:  7. 2. 1984    d.h.
						; input:   subnode - a sexpression, denoting a subnode
						;          node - a sexpression, denoting a list of
						;                 nodes.
						; effect:  replaces the actual subnode of the trans.-
						;          tree by subnode. the list of nodes are
						;          inserted in the actual trans.-tree.
						;          also all nodes, which belong to the
						;          removed subnode are removed from the trans.
						;          tree. the actual node- and subnode-pointer
						;          are moved to the inserted node and subnodes
						; value :  undefined .
  (COND ((NUMBERP (CAR (SEL=ACTUAL.SUBNODE))) (SEL=REMOVE.SUBTREE (CAR (SEL=ACTUAL.SUBNODE))))) (RPLACA SEL*TREE.POINTER.2 SUBNODE)
  (COND
    (NODE (RPLACD SEL*TREE.POINTER.1 (NCONC (CDR NODE) (COPY-LIST SEL*TREE.POINTER.1)))
	  (RPLACA SEL*TREE.POINTER.1 (CAR NODE))))
  (SETQ SEL*TREE.POINTER.2 (CDDAR SEL*TREE.POINTER.1)))

(DEFUN SEL=REMOVE.SUBTREE (CASE)
						; edited:  9-feb-84 10:11:38
						; input:   case - a case-number.
						; effect:  the node with the cas-number case and all
						;          the nodes, which belong to this node, are
						;          removed from the transformation-tree.
						;          if the actual node or subnode is removed
						;          from the transformation-tree, the pointer
						;          to the actual subnode is set to the subnode
						;          which is equal to case-number. the pointer
						;          to the actual node is set to the above node
						; value :  a list of the removed nodes.
  (PROG (ENTRIES ENTRY NEW.POINT1 NEW.POINT2)
	(SETQ NEW.POINT1
	      (MEMBER-IF
		#'(LAMBDA (CASE2)
		    (SETQ NEW.POINT2 (MEMBER-IF #'(LAMBDA (CLAUSE) (EQL CASE CLAUSE)) (CDDR CASE2))))
		SEL*ACTUAL.CLAUSES))
	(SMAPC #'IGNORE
	       #'(LAMBDA (CASE.LIST) (SETQ ENTRY (ASSOC (CAR CASE.LIST) SEL*ACTUAL.CLAUSES))
			 (COND
			   (ENTRY (SETQ SEL*ACTUAL.CLAUSES (DELETE ENTRY SEL*ACTUAL.CLAUSES))
				  (COND
				    ((EQL ENTRY (CAR SEL*TREE.POINTER.1)) (SETQ SEL*TREE.POINTER.1 NEW.POINT1)
				     (SETQ SEL*TREE.POINTER.2 NEW.POINT2)))
				  (SETQ ENTRIES (CONS ENTRY ENTRIES))
				  (UNION (CDR CASE.LIST)
					 (MAPCAN #'(LAMBDA (CL.LITS) (COND ((NUMBERP CL.LITS) (LIST (CAR CL.LITS)))))
						 (CDDR ENTRY))))
			   (T (CDR CASE.LIST))))
	       (LIST CASE))
	(RETURN ENTRIES)))

(DEFUN SEL=RENUMBER.TREE (TREE UPPER.TREE)
						; edited:  9-feb-84 10:05:43  d.h.
						; input:   upper.tree - a node
						;          subtree - a list of nodes.
						; effect:  all case-numbers of subtree are changed
						;          to new, not yet used case-numbers.
						;          if some of these case-numbers occure
						;          in the subnodes of upper.tree, the
						;          subnodes of upper.tree are also changed.
						; value:   a dotted pair of the changed subtree
						;          and upper-tree.
  (PROG (CHANGES NO)
	(MAPC
	  #'(LAMBDA (NODE) (SETQ SEL*IND.CASE.COUNTER (1+ SEL*IND.CASE.COUNTER))
		    (SETQ CHANGES (CONS (CONS (CAR NODE) SEL*IND.CASE.COUNTER) CHANGES)) (RPLACA NODE SEL*IND.CASE.COUNTER)
		    (MAPL
		      #'(LAMBDA (CL.LIST)
			  (COND ((NUMBERP (CAR CL.LIST)) (RPLACA CL.LIST (CDR (ASSOC (CAR CL.LIST) CHANGES))))
				(T
				 (MAPC
				   #'(LAMBDA (LITLIST)
				       (COND ((SETQ NO (CDR (ASSOC (CAR LITLIST) CHANGES))) (RPLACA LITLIST NO))))
				   (CDAR CL.LIST)))))
		      (CDDR NODE)))
	  TREE)
	(MAPL
	  #'(LAMBDA (NODE.LIST)
	      (COND
		((AND (NUMBERP (CAR NODE.LIST)) (ASSOC (CAR NODE.LIST) CHANGES))
		 (RPLACA NODE.LIST (CDR (ASSOC (CAR NODE.LIST) CHANGES))))))
	  (CDDR UPPER.TREE))
	(SETQ SEL*IND.CASE.COUNTER (1+ SEL*IND.CASE.COUNTER)) (RPLACA UPPER.TREE SEL*IND.CASE.COUNTER)
	(RETURN (CONS TREE UPPER.TREE))))

(DEFVAR SEL*ACTUAL.CLAUSES NIL)

(DEFVAR SEL*STEPCOUNTER NIL)

(DEFVAR SEL*STEPS NIL)

(DEFVAR SEL*TREE.POINTER.1 NIL)

(DEFVAR SEL*TREE.POINTER.2 NIL)

(DEFVAR SEL*IND.CASE.COUNTER NIL)

(DEFVAR SEL*IND.CHAIN NIL)

(DEFVAR SEL*IND.ACTUAL.ENTRY NIL)

(DEFVAR SEL*IND.SINGLE.LINK NIL)

(DEFVAR SEL*END.OF.INDUCTION NIL)

(DEFVAR SEL*IND.OLD.CLAUSES NIL)

(DEFVAR SEL*IND.NEW.CLAUSES NIL)

(DEFVAR SEL*IND.OLD.LINKS NIL)

(DEFVAR SEL*IND.OPTIONS NIL)

(DEFUN SEL=CLASSIFY.CLAUSES (THEOREMCLAUSES)
						; edited: 25-jun-82 12:37:22
						; input:  a list of the theoremclauses.
						; effect: classifies the clauses (see code]).
						; value:  undefined.
  (let (RESULT)
    (SEL=RESET.TREE)
    (MAPC #'(LAMBDA (CLAUSE)
	      (COND ((SETQ RESULT (SEL=IS.ELIMINATING.EQUALITY CLAUSE))
		     (SEL=INSERT.CLAUSE RESULT NIL ELIMINATING EQUATIONS))
		    ((SETQ RESULT (SEL=IS.EQUALITY CLAUSE))
		     (SEL=INSERT.CLAUSE RESULT NIL EQUALITY))))
	  (CG-CLAUSES ALL))
    (SETQ SEL*ACTUAL.CLAUSES
	  (CONS (list* 0 'INITIAL
		       (MAPCAN #'(LAMBDA (CLAUSE)
				   (COND ((AND (NOT (ASSOC CLAUSE (SEL=CLAUSES EQUALITY)))
					       (NOT (ASSOC CLAUSE (SEL=CLAUSES INDUCTION)))
					       (NOT (ASSOC CLAUSE (SEL=CLAUSES EQUIVALENCE))))
					  (SEL=INSERT.CLAUSE CLAUSE NIL SUPPORTED)
					  (LIST (LIST CLAUSE (CONS 0 (let (X)
								       (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
									 (push (1+ RPTN) X))
								       X)))))
					 (T (LIST (LIST CLAUSE)))))
			       (OR THEOREMCLAUSES
				   (COND ((MEMBER-IF #'(LAMBDA (CLAUSE) (EQL 'THEOREM (DS-CLAUSE.PARENTS CLAUSE))) (DS-RULES))
					  NIL)
					 (T (CG-CLAUSES ALL))))))
		NIL))
    (SEL=SORT.EQUALITIES)
    (MAPC #'(LAMBDA (CLAUSE) (DS-CLAUSE.ALL.LIT.REMPROP CLAUSE 'KIND)) (CG-CLAUSES ALL)) (SETQ SEL*IND.CASE.COUNTER 0)))

(defun sel=weigth_set.foreign (symbol)
						; Edited:  01-FEB-1992 14:28
						; Authors: PRCKLN
						; Input:   SYMBOL is a function, predicate, or constant.
						; Effect:  Resets the Polynomial to weight function and predicate
						;          symbols
						; Value:   -
  (let ((entry (cassoc (intern (dt-pname symbol) (find-package "MKRP")) (opt-get.option str_weight.polynomials))))
    (dt-putprop symbol 'sel*weight
		(if entry
		    (compile nil `(lambda ,(first entry) ,@(rest entry)))
		    #'(lambda (&rest args) (apply #'+ 1 args))))
    (dt-getprop symbol 'sel*weight)))

(defun sel=weigth_symbol.weight (symbol)
						; Edited:  01-FEB-1992 14:41
						; Authors: PRCKLN
						; Input:   A constant, predicate, or function symbol
						; Effect:  Puts the property SEL*WEIGHT according to options
						;          if not set.
						; Value:   This weight      
  (or (dt-getprop symbol 'sel*weight)
      (sel=weigth_set.foreign symbol)))

(defun sel-weight (litlist)
						      ; Edited:  06-FEB-1992 20:32
						      ; Authors: MKRP
						      ; Input:   A literal list.
						      ; Effect:  -
						      ; Value:   The heuristic value according to option STR_POLYNOMIAL.WEIGHT.
  (let ((res 0))
    (mapc #'(lambda (lit)
	      (incf res (sel=weight_eval (dt-term_create (ds-lit.predicate lit) (ds-lit.termlist lit)))))
	  litlist)
    res))

(defun sel=weight_eval (term)
						; Edited:  01-FEB-1992 15:21
						; Authors: MKRP
						; Input:   A term.
						; Effect:  -
						; Value:   The heuristic value of TERM
  (cond ((dt-variable.is term) 1)
	((dt-constant.is term) (apply (sel=weigth_symbol.weight term) nil))
	(t (apply (sel=weigth_symbol.weight (dt-term_topsymbol term))
		  (mapcar #'sel=weight_eval (dt-term_arguments term))))))

(defun sel=strat_value (link func)
						      ; Edited:  06-FEB-1992 20:30
						      ; Authors: MKRP PRCKLN
						      ; Input:   A resolution link (R).
						      ; Effect:  -
						      ; Value:   A heuristic value for the largeness of the result
						      ;          (The number of cons cells in a list representation of the
						      ;          resulting resolvent).
  (or (ds-link.selection.info link)
      (let ((res (funcall func link (first (ds-link.unifiers link)) nil t)))
	(ds-link.put.selection.info link (sel-weight res))
	(dolist (var (dt-termlist.variables (mapcan #'(lambda (lit) (copy-list (ds-lit.termlist lit))) res)))
	  (dt-variable.delete var))
	(ds-link.selection.info link))))

(defun sel=strat_basic.value (link)
  (sel=strat_p.selection (if (eq (ds-link.colour link) 'r)
			     (sel=strat_value link #'op-resolve )
			     (sel=strat_value link #'op-paramodulate))
			 (DS-LINK.NOLIT LINK)
			 (ds-link.depth link)
			 1
			 (intersection (sel=clauses supported) (list (ds-link.pospar link) (ds-link.negpar link)))
			 (ds-clause.only.equations (ds-link.result link))))

(defun sel=strat_total.value (link)
						; Edited:  25-OCT-1991 17:58
						; Authors: PRCKLN
						; Input:   WEIGHT is the weight of the link, i.e. a value computed from the
						;          potential result.
						; Effect:  -
						; Value:   The heuristic value of the R-link to be weighed.
  (if (or (opt-is.kz.completion) (opt-is.with.residues) (eq sel*str_paramodulation.strategy 'clause-graph))
      (sel=strat_basic.value link)
      (sel=strat_r.selection 0 ;(sel=strat_value link #'op-resolve) To be compatible
			     (OR (DT-GETPROP LINK 'SEL*NOLIT)
				 (DS-LINK.NOLIT LINK))
			     (SEL=LINK.VARIABLES LINK)
			     (DS-LINK.DEPTH LINK)
			     (DS-LINK.IS.MARKED ACTIVE link)
			     nil)))

(defun sel=strat_active.plink (LINK)
						; Edited:  18-JAN-1990 02:47
						; Authors: PRCKLN
						; Input:   A link
						; Effect:  -
						; Value:   The heuristic value for this
						;          link iff strategy is HEURISTIC-PARAMODULATION
  (- (sel=strat_basic.value link)))

(defun sel=strat_active.rlink (link)
						; Edited:  18-JAN-1990 02:47
						; Authors: PRCKLN
						; Input:   A link
						; Effect:  -
						; Value:   The heuristic value of an active r-link in all strategies
  (- (sel=strat_total.value link)))

(DEFUN SEL=STRATEGIES NIL
						; Edited:  17-AUG-1989 18:58
						; Authors: HJO PRCKLN
						; effect: calculates deduction and reduction codes for
						;         the next link selected by the heuristics or
						;         strategies according to unit preference
						;         sub-strategy for r-links and the selected
						;         strategy for p-links:
						;         active p-links are preferred.
						; value:  undefined
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE (COND ((NULL SEL=STRATEGIES)
		   (SETQ SEL=STRATEGIES T)
		   (LET ((LINK (cCASE SEL*STR_RESOLUTION.STRATEGY
				 ((SET-OF-SUPPORT BASIC-RESOLUTION UNIT-REFUTATION ALL)
				  (case SEL*STR_paramodulation.STRATEGY
				    ((clause-graph heuristic-completion zhang-kapur bachmair-ganzinger snyder-lynch dershowitz)
				     (OR (let ((r.link (MAXELT (SEL=LINKS R ACTIVE) #'sel=strat_active.rlink))
					       (p.link (MAXELT (append (SEL=LINKS p ACTIVE) (SEL=LINKS piw ACTIVE))
							       #'sel=strat_active.plink)))
					   (if (not r.link)
					       p.link
					       (if (not p.link)
						   r.link
						   (if (> (sel=strat_active.rlink r.link)
							  (sel=strat_active.plink p.link))
						       r.link
						       p.link))))
					 (CAR (SEL=LINKS P PASSIVE))))
				    (otherwise (error "Undefined paramodulation strategy: ~A." SEL*STR_paramodulation.STRATEGY))))
				 (LINEAR
				   (OR (CAR (SEL=LINKS R ACTIVE))
				       (CAR (SEL=LINKS P ACTIVE))
				       (CAR (SEL=LINKS R PASSIVE))
				       (CAR (SEL=LINKS P PASSIVE))))
				 (INDUCTION-SPECIAL
				   (OR (CAR (SEL=LINKS R ACTIVE)) (CAR (SEL=LINKS P ACTIVE)) (CAR (SEL=LINKS P PASSIVE)))))))
		     (IF LINK
			 (SEL=MAKE.DEDUCTION.CODE LINK 'FIRST)
			 (SEL=MAKE.DEDUCTION.CODE 'COLLAPSED 'COLLAPSED))))
		  (T (SEL=PASS.CONTROL NIL NIL))))
    (REDUCE (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) NIL (LIST (SEL-DEDUCTION.CODE.LINK))))
    (OTHERWISE (ERROR "illegal reduce.deduce in sel=strategies: ~a" SEL*REDUCE.DEDUCE)))
  (SEL=RETURN))

(DEFUN SEL=LINK.VARIABLES (LINK)
  (+ (LENGTH (DS-CLAUSE.VARIABLES (DS-LINK.POSPAR LINK)))
     (LENGTH (DS-CLAUSE.VARIABLES (DS-LINK.NEGPAR LINK)))
     (- (/ (LENGTH (FIRST (DS-LINK.UNIFIERS LINK))) 2))))

(DEFUN SEL=STRATEGIES.ACTIVATE? NIL
						; edited:  5-jul-82 18:26:46
						; effect: the r- and p-links are initially marked
						;         according to selected strategies.
						;         the heuristics are initialized at the very
						;         first activation of the strategies.
						; value:  t
  (unless (OPT-GET.OPTION GEN_MANUAL.CONTROL)
    (SEL=STRATEGIES.ACTIVATE?.exec))
  T)

(defun SEL=STRATEGIES.ACTIVATE?.exec ()
  (unless (GET 'SEL=STRATEGIES 'SEL*STATE)
    (SETF (GET 'SEL=STRATEGIES 'SEL*STATE) 'ACTIVATED)
    (SEL=STR.DEFINE.STRATEGY)
    (SEL=STR.INIT.MARK.RLINKS (CG-LINKS R ALL))
    (SEL=STR.INIT.MARK.PLINKS (CG-LINKS P ALL))))

(DEFUN SEL=STRATEGIES.UPDATE (&OPTIONAL FLAG)
						; Edited:  12-MAR-1991 23:54
						; Authors: PRCKLN
						; effect: after the very first activation of
						;         sel=strategies this functions marks the
						;         new r- and p-links according to the selected
						;         resolution and paramodulation strategies
						;         the heuristics are updated at every step too
						; value:  undefined.
  (unless (OPT-GET.OPTION GEN_MANUAL.CONTROL)
    (SEL=STRATEGIES.UPDATE.exec flag)))

(DEFUN SEL=STRATEGIES.UPDATE.exec (&OPTIONAL FLAG)
						; edited:  5-jul-82 18:16:26
						; effect: after the very first activation of
						;         sel=strategies this functions marks the
						;         new r- and p-links according to the selected
						;         resolution and paramodulation strategies
						;         the heuristics are updated at every step too
						; value:  undefined.
  (DECLARE (IGNORE FLAG))
  (when (GET 'SEL=STRATEGIES 'SEL*STATE)
    (CCASE SEL*REDUCE.DEDUCE
      (REDUCE (SETF (GET 'SEL=STRATEGIES 'SEL*INSERTED.RLINKS)  (CG-LINKS R INSERTED)
		    (GET 'SEL=STRATEGIES 'SEL*INSERTED.PLINKS)  (CG-LINKS '(piw P) INSERTED)
		    (GET 'SEL=STRATEGIES 'SEL*INSERTED.CLAUSES) (CG-CLAUSES INSERTED))
	      (CASE SEL*STR_RESOLUTION.STRATEGY
		(LINEAR (COND ((EQL SEL*ACTUAL.OPERATION 'SEL=STRATEGIES)
			       (SETF (GET 'SEL=STRATEGIES 'SEL*LABEL)         'ACTIVE
				     (GET 'SEL=STRATEGIES 'SEL*ACTIVE.CLAUSE) (CAR (CG-CLAUSES INSERTED))))
			      (T (SETF (GET 'SEL=STRATEGIES 'SEL*LABEL)
				       (SEL=STR.LABEL (SEL-DEDUCTION.CODE.LINK) (GET 'SEL=STRATEGIES 'SEL*ACTIVE.CLAUSE))))))
		(OTHERWISE NIL)))
      (DEDUCE
	(SEL=STR.MARK.RLINKS (SET-DIFFERENCE (union (GET 'SEL=STRATEGIES 'SEL*INSERTED.RLINKS) (cg-links r inserted))
					     (CG-LINKS R REMOVED)))
	(SEL=STR.MARK.PLINKS (SET-DIFFERENCE (union (GET 'SEL=STRATEGIES 'SEL*INSERTED.PLINKS) (cg-links P inserted))
					     (CG-LINKS P REMOVED)))))))




(DEFUN SEL=STR.LABEL (LINK ACTIVE.CLAUSE)
						; input: an r- p- or f-link and a clause.
						; value: one of the atoms: active passive inhibited.
  (CASE (DS-LINK.COLOUR LINK)
    ((R P)
     (COND ((DS-LINK.IS.MARKED ACTIVE LINK) 'ACTIVE)
	   ((DS-LINK.IS.MARKED PASSIVE LINK) 'PASSIVE)
	   ((DS-LINK.IS.MARKED INHIBITED LINK) 'INHIBITED)
	   (T 'ACTIVE)))
    (SI (COND ((EQL ACTIVE.CLAUSE (DS-LINK.POSPAR LINK)) 'ACTIVE) (T 'PASSIVE)))
    (OTHERWISE (ERROR "illegal colour in sel=str.label: ~a" (DS-LINK.COLOUR LINK)))))

(DEFUN SEL=STR.DEFINE.STRATEGY NIL
						; edited:  5-aug-82 15:59:05
						; effect: the actual resolution strategy is determined
						;         if unit-refutation had been selected,
						;         a horn renamable test will be preformed
						;         and the strategy will be changed, if the
						;         test is negative and an alternative is
						;         available, otherwise a warning is printed.
						;         for linear strategy, the top clause is
						;         determined.
  (PROG ((STRATEGY (OPT-GET.OPTION STR_RESOLUTION)))
	(SETQ SEL*STR_LINEAR.TOP.CLAUSE NIL)
	(LET ((NUMBER.IN.STRAT.STRING (REMOVE-IF-NOT #'(LAMBDA (CHAR)
							 (MEMBER CHAR '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
						     (STRING STRATEGY))))
	  (UNLESS (STRING= "" NUMBER.IN.STRAT.STRING)
	    (SETQ SEL*STR_LINEAR.TOP.CLAUSE (INTERN (SUBSEQ STRATEGY (- (LENGTH STRATEGY) 2 (LENGTH NUMBER.IN.STRAT.STRING)))
						    (find-package "MKRP")))
	    (SETQ STRATEGY (INTERN (SUBSEQ STRATEGY  0 (- (LENGTH STRATEGY) 4 (LENGTH NUMBER.IN.STRAT.STRING)))
				   (find-package "MKRP")))))
	;; linear top-clause seperated from strategy declaration")
	(SETQ SEL*STR_RESOLUTION.STRATEGY
	      (CASE STRATEGY
		((BASIC-RESOLUTION SET-OF-SUPPORT INDUCTION-SPECIAL all) STRATEGY)
		(UNIT-REFUTATION
		  (COND
		    ((NOT (SEL=HORN.RENAMABLE.TEST (CG-CLAUSES ALL))) (TERPRI (OPT-GET.OPTION TR_TRACE.FILE))
		     (PROGN
		       (PRINC "**WARNING** Horn renamable test negative! Unit refutation might be incomplete."
			      (OPT-GET.OPTION TR_TRACE.FILE))
		       (TERPRI (OPT-GET.OPTION TR_TRACE.FILE)))
		     (TERPRI (OPT-GET.OPTION TR_TRACE.FILE))))
		  STRATEGY)
		((UNIT-REFUTATION.OR.BASIC-RESOLUTION UNIT-REFUTATION.OR.SET-OF-SUPPORT UNIT-REFUTATION.OR.LINEAR)
		 (COND
		   ((SEL=HORN.RENAMABLE.TEST (CG-CLAUSES ALL)) (TERPRI (OPT-GET.OPTION TR_TRACE.FILE))
		    (PROGN
		      (PRINC "Horn renamable test positive! Resolution strategy: Unit refutation."
			     (OPT-GET.OPTION TR_TRACE.FILE))
		      (TERPRI (OPT-GET.OPTION TR_TRACE.FILE)))
		    (TERPRI (OPT-GET.OPTION TR_TRACE.FILE)) 'UNIT-REFUTATION)
		   (T
		    (PROG ((NEWSTRATEGY (INTERN (SUBSEQ (STRING STRATEGY) 19)
						(find-package "MKRP"))))
			  (TERPRI (OPT-GET.OPTION TR_TRACE.FILE))
			  (PRINC "Horn renamable test negative! Resolution strategy: " (OPT-GET.OPTION TR_TRACE.FILE))
			  (PROGN (PRINC NEWSTRATEGY (OPT-GET.OPTION TR_TRACE.FILE)) (TERPRI (OPT-GET.OPTION TR_TRACE.FILE)))
			  (TERPRI (OPT-GET.OPTION TR_TRACE.FILE))
			  (COND ((EQL NEWSTRATEGY 'LINEAR) (SEL=STR.SET.LINEAR.TOP.CLAUSE))) (RETURN NEWSTRATEGY)))))
		(LINEAR (SEL=STR.SET.LINEAR.TOP.CLAUSE) STRATEGY)
		(OTHERWISE (ERROR "Illegal strategy in STR=DEFINE.STRATEGY: ~a" STRATEGY))))))



(DEFUN SEL=HORN.RENAMABLE.TEST (CLAUSESET)
						; edited: "29-oct-81 15:59:29"
						; input:   a set of clauses
						; value:   t if clauseset is horn renamable, nil if not.
  (PROG (PROP.CLAUSES)
     REDUCE.TO.PROPOSITIONAL.CALCULUS
	(SETQ PROP.CLAUSES
	      (MAPCAN #'(LAMBDA (CLAUSE)
			  (let (P.CLAUSE (NOLIT (DS-CLAUSE.NOLIT CLAUSE)))
			    (COND ((> NOLIT 1)
				   (DODOWN (RPTN NOLIT)
				     (PROG ((PREDICATE (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))))
					   (COND ((DS-SIGN.IS.NEGATIVE (DS-CLAUSE.SIGN CLAUSE (1+ RPTN)))
						  (SETQ PREDICATE (- PREDICATE))))
					   (SETQ P.CLAUSE (CONS PREDICATE P.CLAUSE))))))
			    (LIST P.CLAUSE)))
		      CLAUSESET))
     END.OF.REDUCTION
	(RETURN (SATISFIABLE (MAPCAN #'(LAMBDA (CLAUSE)
					 (MAPCON #'(LAMBDA (RESTCLAUSE)
						     (MAPCAR #'(LAMBDA (LITERAL)
								 (LIST (CAR RESTCLAUSE) LITERAL))
							     (CDR RESTCLAUSE)))
						 CLAUSE))
				     PROP.CLAUSES)))))

(DEFUN SEL=STR.SET.LINEAR.TOP.CLAUSE NIL
						; effect: determines the top clause for linear
						;         strategy.
						; value:  undefined.
  (SETQ SEL*STR_LINEAR.TOP.CLAUSE
	(PROG (TOP.CLAUSE)
	   ERROR
	      (WHEN (NULL SEL*STR_LINEAR.TOP.CLAUSE)
		(TERPRI T)
		(PROGN (PRINC "Linear strategy requires the definition of a top clause") (TERPRI))
		(PROGN (PRINC "Which one of the following clauses shall be the top clause?") (TERPRI)) (TERPRI T)	;
		(CG-DUMP NIL `((CLAUSES ALL LR)))
		(SETQ SEL*STR_LINEAR.TOP.CLAUSE (READ T)))
	      (COND
		((SETQ TOP.CLAUSE
		       (CAR
			 (MEMBER-IF
			   #'(LAMBDA (CLAUSE) (STRING= (STRING SEL*STR_LINEAR.TOP.CLAUSE) (DS-CLAUSE.PNAME CLAUSE)))
			   (CG-CLAUSES ALL))))
		 (RETURN TOP.CLAUSE))
		(T (TERPRI T) (PROGN (PRINC "Illegal top clause! The pname of an existing clause is required.") (TERPRI))
		   (TERPRI T) (SETQ SEL*STR_LINEAR.TOP.CLAUSE NIL) (GO ERROR))))))

(DEFUN SEL=STR.LINEAR.CONNECTED.TO.TOP.CLAUSE (LINK)
						; edited: "30-oct-81 09:07:16"
						; input: one p- or r-link
						; value: t if one of the parent clauses of link
						;        is the top clause (linear strategy)
						;        else nil
  (OR (EQL SEL*STR_LINEAR.TOP.CLAUSE (DS-LINK.POSPAR LINK)) (EQL SEL*STR_LINEAR.TOP.CLAUSE (DS-LINK.NEGPAR LINK))))



(DEFVAR SEL*STR_RESOLUTION.STRATEGY NIL)

(DEFVAR SEL*STR_PARAMODULATION.STRATEGY NIL)

(DEFVAR SEL*STR_LINEAR.TOP.CLAUSE NIL)



(DEFVAR SEL*LINKS_RPASSIVE NIL)

(DEFVAR SEL*LINKS_PACTIVE NIL)

(DEFVAR SEL*LINKS_PPASSIVE NIL)

(DEFVAR SEL*LINKS_RINHIBITED NIL)

(DEFVAR SEL*LINKS_PINHIBITED NIL)

(DEFUN SEL=REDUCTIONS NIL
						; edited: 27. 5. 1982   hjo
						; effect: calculates deduction and reduction codes for
						;         the first reduction link defined in
						;         sel=reductions.activate? .
						; value:  undefined
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE
      (COND
        ((NULL SEL=REDUCTIONS)
	 (SETQ SEL=REDUCTIONS T)
	 (SEL=MAKE.DEDUCTION.CODE
	   (FIRST (SEL=LINKS REDUCTIONS)) 'FIRST))
        (T (SEL=PASS.CONTROL NIL NIL))))
    (REDUCE (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) NIL (LIST (SEL-DEDUCTION.CODE.LINK))))
    (OTHERWISE (ERROR "illegal reduce.deduce in sel=reductions: ~a" SEL*REDUCE.DEDUCE)))
  (SEL=RETURN))

(DEFUN SEL=REDUCTIONS.ACTIVATE? NIL
						; edited:  5-jul-82 17:31:20
						; effect:  links which reduce the graph by resolution
						;          upon them are inserted into
						;          sel*links_reductions :
						;       1. both parents become pure.
						;       2. the resolvent subsumes the parent clause
						;       3. one parent clause becomes pure and the
						;          resolvent is shorter as this parent.
						;       4. the resolvent becomes a unit clause by
						;          merging of two literals.
						; value:   nil, if no reduction link has been found,
						;          else not nil.
  (unless (or (opt-is.kz.completion) (opt-is.with.residues))
    (let ((LINKS (COND ((GET 'SEL=REDUCTIONS 'SEL*STATE)
			(GET 'SEL=REDUCTIONS 'SEL*WAITING.RLINKS))
		       (T (SETF (GET 'SEL=REDUCTIONS 'SEL*STATE) 'ACTIVATED)
			  (CG-LINKS R ALL))))
	  EXAMINED.CLAUSES)
      (MAPC #'(LAMBDA (LINK)
		(WHEN (NOT (DS-LINK.IS.MARKED INHIBITED LINK))
		  (if (SEL=REDUCTIONS.BOTH.PARENTS.PURE LINK)
		      (SEL=INSERT.LINK LINK T REDUCTIONS)
		      (SETQ EXAMINED.CLAUSES (NCONC (SEL=REDUCTIONS.RESOLVENT.REPLACES.PARENT LINK) EXAMINED.CLAUSES)))))
	    LINKS)
      (REMPROP 'SEL=REDUCTIONS 'sel*WAITING.RLINKS)))
  (SEL=LINKS REDUCTIONS))

(DEFUN SEL=REDUCTIONS.UPDATE (&OPTIONAL FLAG)
  (DECLARE (IGNORE FLAG))
						; edited:  5-jul-82 17:22:35
						; effect:  after the very first activation of
						;          sel=reductions this function gathers all
						;          inserted r-links as a property
						;          'waiting.rlinks of 'sel=reductions.
						;          this property is deleted at the next
						;          activation of sel=reductions.
						; value:   undefined.
  (when (GET 'SEL=REDUCTIONS 'SEL*STATE)
    (SETF (GET 'SEL=REDUCTIONS 'SEL*WAITING.RLINKS)
	  (APPEND (CG-LINKS R INSERTED) (COPY-LIST (NSET-DIFFERENCE (GET 'SEL=REDUCTIONS 'SEL*WAITING.RLINKS)
								    (CG-LINKS R REMOVED)))))))

(DEFUN SEL=REDUCTIONS.BOTH.PARENTS.PURE (LINK)
						; edited: 2. 6. 1982  hjo
						; input:  an r-link
						; value:  t if resolution upon this link will cause
						;         purity of both parents, else nil.
  (let ((POSPAR (DS-LINK.POSPAR LINK))
	(NEGPAR (DS-LINK.NEGPAR LINK))
	(POSLITNO (DS-LINK.POSLITNO LINK))
	(NEGLITNO (DS-LINK.NEGLITNO LINK)))
    (AND (NULL (CDR (DS-CLAUSE.LINKS 'R POSPAR POSLITNO)))
	 (NULL (CDR (DS-CLAUSE.LINKS 'R NEGPAR NEGLITNO)))
	 (NULL (DS-CLAUSE.LINKS 'P POSPAR POSLITNO))
	 (NULL (DS-CLAUSE.LINKS 'P NEGPAR NEGLITNO))
	 (NULL (DS-CLAUSE.LINKS 'PIW POSPAR POSLITNO))
	 (NULL (DS-CLAUSE.LINKS 'PIW NEGPAR NEGLITNO))
	 (NULL (DS-CLAUSE.LINKS 'RIW POSPAR POSLITNO))
	 (NULL (DS-CLAUSE.LINKS 'RIW NEGPAR NEGLITNO)))))

(DEFUN SEL=REDUCTIONS.RESOLVENT.REPLACES.PARENT (LINK)
						; edited: 2. 6. 1982   hjo
						; input:  a r-link
						; value:  a list of clauses examined with
						;         ds-clause.one.lit.unifier.
						;         (this function creates a literal property
						;         variables, which has to be removed)
						; effect: if the link causes subsumption of one
						;         parent clause by its resolvent, or one
						;         parent clause is a unit clause and the
						;         other parent gets pure after resolution,
						;         the link will be inserted into
						;         sel*links_reductions
  (let ((POSPAR (DS-LINK.POSPAR LINK))
	(NEGPAR (DS-LINK.NEGPAR LINK))
	(POSLITNO (DS-LINK.POSLITNO LINK))
	(NEGLITNO (DS-LINK.NEGLITNO LINK))
	(UNIFIER (CAR (DS-LINK.UNIFIERS LINK))) EXAMINED.CLAUSES)
    (COND ((OR (AND (= 1 (DS-CLAUSE.NOLIT POSPAR))
		    (OR (NULL UNIFIER)
			(AND (NULL (CDR (DS-CLAUSE.LINKS 'R NEGPAR NEGLITNO)))
			     (NULL (DS-CLAUSE.LINKS 'P NEGPAR NEGLITNO))
			     (NULL (DS-CLAUSE.LINKS 'PIW NEGPAR NEGLITNO))
			     (NULL (DS-CLAUSE.LINKS 'RIW NEGPAR NEGLITNO)))
			(PROG2 (SETQ EXAMINED.CLAUSES (LIST NEGPAR))
			       NIL)
			(DS-CLAUSE.ONE.LIT.UNIFIER NEGPAR NEGLITNO UNIFIER)
			(UNI-UNIFIER.BECOMES.MATCHER UNIFIER (DS-CLAUSE.VARIABLES NEGPAR) T T)))
	       (AND (= 1 (DS-CLAUSE.NOLIT NEGPAR))
		    (OR (NULL UNIFIER)
			(AND (NULL (CDR (DS-CLAUSE.LINKS 'R POSPAR POSLITNO)))
			     (NULL (DS-CLAUSE.LINKS 'P POSPAR POSLITNO))
			     (NULL (DS-CLAUSE.LINKS 'PIW POSPAR POSLITNO))
			     (NULL (DS-CLAUSE.LINKS 'RIW POSPAR POSLITNO)))
			(PROG2 (SETQ EXAMINED.CLAUSES (NCONC1 EXAMINED.CLAUSES POSPAR))
			       NIL)
			(DS-CLAUSE.ONE.LIT.UNIFIER POSPAR POSLITNO UNIFIER)
			(UNI-UNIFIER.BECOMES.MATCHER UNIFIER (DS-CLAUSE.VARIABLES POSPAR) T T))))
	   (SEL=INSERT.LINK LINK T REDUCTIONS)))
    EXAMINED.CLAUSES))

(DEFUN SEL=MERGING NIL				; edited: 11. 7. 1984
						; effect: calculates the deduction and reduction codes
						;         for the link stored in the property
						;         mergelinks of sel=merging which generates
						;         the smallest resolvent.
						; value:  undefined.
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE (COND ((NULL (SYMBOL-VALUE 'SEL=MERGING))
		   (SETQ SEL=MERGING T)
		   (LET ((MAXELT (MAXELT (GET 'SEL=MERGING 'SEL*MERGELINKS) #'(LAMBDA (ELEMENT) (- (SECOND ELEMENT))))))
		     (SEL=MAKE.DEDUCTION.CODE (CAR MAXELT) (CDDR MAXELT))))
		  (T (SEL=PASS.CONTROL NIL NIL))))
    (REDUCE (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) NIL
				     (LIST (CONS (SEL-DEDUCTION.CODE.LINK) (SEL-DEDUCTION.CODE.UNIFIER)))))
    (OTHERWISE (ERROR "illegal reduce.deduce in sel=merging: ~a" SEL*REDUCE.DEDUCE)))
  (SEL=RETURN))

(DEFUN SEL=MERGING.ACTIVATE? NIL
						; edited: 11. 7. 1984
						; effect: the property mergelinks of sel=merging
						;         is calculated. it is a list with elements
						;         (rlink nolit . unifier) where nolit is
						;         an estimation about the number of literals
						;         in the resolvent and is smaller than the
						;         smallest parent clause (or nolit = 1 and
						;         at least one parent has length 3).
						;         the property waiting.rlinks is removed.
						;         if the resolvent is smaller then usually
						;         expected, a property sel*nolit is attatched
						;         at the link.
						; value:  t if at least one mergelink exists.
  (unless (or (opt-is.kz.completion) (opt-is.with.residues))
    (let ((WAITING.RLINKS (COND ((GET 'SEL=MERGING 'SEL*STATE) (GET 'SEL=MERGING 'SEL*WAITING.RLINKS))
				(T (SETF (GET 'SEL=MERGING 'SEL*STATE) 'ACTIVATED)
				   (CG-LINKS R ALL))))
	  (MERGELINKS (delete-IF #'(LAMBDA (DESCRIPTOR)
				     (let ((LINK (CAR DESCRIPTOR)))
				       (or (not (DS-LINK.IS LINK))
					   (member link (cg-links 'r removed))
					   (member link (cg-links 'p removed))
					   (DS-LINK.IS.MARKED INHIBITED LINK)
					   (not (MEMBER (CDDR DESCRIPTOR) (DS-LINK.UNIFIERS LINK))))))
				 (GET 'SEL=MERGING 'SEL*MERGELINKS))))
      (MAPC #'(LAMBDA (LINK)
		(unless (DS-LINK.IS.MARKED INHIBITED LINK)
		  (MAPC #'(LAMBDA (UNIFIER)
			    (let ((NOLIT (SEL=LINK.NOLIT LINK UNIFIER))
				  (NOLIT1 (DS-CLAUSE.NOLIT (DS-LINK.POSPAR LINK)))
				  (NOLIT2 (DS-CLAUSE.NOLIT (DS-LINK.NEGPAR LINK))))
			      (COND ((OR (< NOLIT (MIN NOLIT1 NOLIT2))
					 (AND (EQL 1 NOLIT)
					      (OR (> NOLIT2 2)
						  (> NOLIT1 2))))
				     (SETQ MERGELINKS (CONS (CONS LINK (CONS NOLIT UNIFIER)) MERGELINKS)))
				    ((< NOLIT (+ NOLIT1 NOLIT2 -2))
				     (DT-PUTPROP LINK 'SEL*NOLIT NOLIT)))))
			(DS-LINK.UNIFIERS LINK))))
	    WAITING.RLINKS)
      (SETF (GET 'SEL=MERGING 'SEL*MERGELINKS) MERGELINKS)
      (REMPROP 'SEL=MERGING 'SEL*WAITING.RLINKS)
      MERGELINKS)))

(DEFUN SEL=MERGING.UPDATE (&OPTIONAL FLAG)
  (DECLARE (IGNORE FLAG))
						; edited: 11. 7. 1984
						; effect: the property waiting.rlinks of sel=merging
						;         is updated. new links are inserted, removed
						;         links are removed and links connected to
						;         changed clauses are removed from the
						;         property mergelinks and inserted into
						;         waiting.rlinks.
						; value:  NIL
  (when (GET 'SEL=MERGING 'sel*STATE)
    (let ((WAITING.RLINKS (GET 'SEL=MERGING 'SEL*WAITING.RLINKS))
	  (MERGELINKS     (GET 'SEL=MERGING 'SEL*MERGELINKS)))
      (MAPC #'(LAMBDA (LINK) (SETQ MERGELINKS (REMASSOC LINK MERGELINKS))) (CG-LINKS R REMOVED))
      (MAPC #'(LAMBDA (CLAUSE)
		(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		  (MAPC #'(LAMBDA (LINK)
			    (SETQ WAITING.RLINKS (INS LINK WAITING.RLINKS)
				  MERGELINKS     (REMASSOC LINK MERGELINKS)))
			(DS-CLAUSE.LINKS 'R CLAUSE (1+ RPTN)))))
	    (CG-CLAUSES CHANGED))
      (SETF (GET 'SEL=MERGING 'SEL*MERGELINKS)
	    MERGELINKS
	    (GET 'SEL=MERGING 'SEL*WAITING.RLINKS)
	    (APPEND (CG-LINKS R INSERTED) (COPY-LIST (NSET-DIFFERENCE WAITING.RLINKS (CG-LINKS R REMOVED))))))
    nil))

(DEFUN SEL=FACTORIZE ()
						; edited: 26. 5. 1982   hjo
						; effect: works off all si-links in the actual graph.
						; value:  undefined.
  (let ((SI.LINK
	  (CAR (MEMBER-IF #'(LAMBDA (LINK)
			      (NOT (or (DS-LINK.IS.MARKED INHIBITED LINK)
				       (DS-LINK.IS.MARKED passive link))))
			  (CG-LINKS SI ALL)))))
    (CASE SEL*REDUCE.DEDUCE
      (DEDUCE (COND (SEL=FACTORIZE (SEL=PASS.CONTROL NIL NIL))
		    (t (SETQ SEL=FACTORIZE T)
		       (SEL=MAKE.DEDUCTION.CODE SI.LINK 'FIRST))))
      (REDUCE (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) NIL (LIST SI.LINK)))
      (OTHERWISE (ERROR "illegal reduce.deduce in sel=factorize: ~a" SEL*REDUCE.DEDUCE))))
  (SEL=RETURN))

(DEFUN SEL=FACTORIZE.ACTIVATE? NIL
						; edited:  5-jul-82 13:33:09
						; value:   depends on the options
  (AND (if SEL*INITIAL.FLAG
	   (OPT-GET.OPTION FAC_INITIAL)
	   (OPT-GET.OPTION FAC_EACH.STEP))
       (MEMBER-IF #'(LAMBDA (LINK) (NOT (or (DS-LINK.IS.MARKED INHIBITED LINK)
					    (DS-LINK.IS.MARKED passive link))))
		  (CG-LINKS SI ALL))))

(DEFUN SEL=FACTORIZE.UPDATE (&OPTIONAL FLAG)
						; Edited:  30-OCT-1991 00:42
						; Authors: PRCKLN
						; effect:  Labels SI-Links according to strategy as passive or active.
						;          Changes next operation if it is factorization
						;          and factorization is not longer applicable.
						; Value:   Undefined
  (SEL=STR.MARK.SILINKS (if flag
			    (SEL=STR.MARK.SILINKS (CG-LINKS SI ALL))
			    (SET-DIFFERENCE (cg-links SI inserted) (CG-LINKS SI REMOVED))))
  (when (and (eq sel*actual.operation 'sel=factorize)
	     (not (MEMBER-IF #'(LAMBDA (LINK) (NOT (or (DS-LINK.IS.MARKED INHIBITED LINK)
						       (DS-LINK.IS.MARKED passive link))))
			     (CG-LINKS SI ALL))))
    (setq sel*actual.operation (SEL=NEXT.OPERATION))))

(DEFUN SEL=TERMINATOR NIL
						; this is a dummy function and will never be executed.
  )

(DEFUN SEL=TERMINATOR.ACTIVATE? NIL
						; edited: 10. 8. 1984
						; effect: if term.iterations is > 0 and in
						;         sel*terminator.units are at least three
						;         clauses or one unitclause, the terminator
						;         is called. in case a refutation is found,
						;         the deduction code is created and the
						;         function is left with sel=return,
						;         else if term_units is activated
						;         and some units are deducible, the deduction
						;         chain is stored as a property 'chain at
						;         sel=terminator.
						; value:  nil if not left.
  (COND ((ZEROP (OPT-GET.OPTION TERM_ITERATIONS)) NIL)
	(T (LET ((RESULT (COND ((OR (CDDR SEL*TERMINATOR.NEW.CLAUSES)
				    (MEMBER-IF #'(LAMBDA (CLAUSE) (EQL 1 (DS-CLAUSE.NOLIT CLAUSE)))
					       SEL*TERMINATOR.NEW.CLAUSES))
				(COND ((OPT-GET.OPTION TERM_UNITS)
				       (TERM-TERMINATOR (CG-CLAUSES ALL) SEL*TERMINATOR.NEW.CLAUSES (SEL=CLAUSES SUPPORTED)))
				      ((not (or (opt-is.with.residues)
						(/= (opt-get.option er_narrow.depth) 0)))
				       (SETQ SEL*TERMINATOR.NEW.CLAUSES NIL)
				       (TERM-TERMINATOR NIL (CG-CLAUSES AlL) (SEL=CLAUSES SUPPORTED))))))))
	     (WHEN RESULT
	       (COND ((EQL 'PROVED (CAR RESULT))
		      (SEL=MAKE.DEDUCTION.CODE (CONS 'R.CHAIN (CDR RESULT)) NIL)
		      (SEL=RETURN))
		     ((OPT-GET.OPTION TERM_UNITS)
		      (SETF (GET 'SEL=TERMINATOR 'SEL*CHAIN) (CONS 'R.CHAIN (CDR RESULT)))
		      (SETQ SEL*FINAL.ACTIONS (CONS '(IF (GET 'SEL=TERMINATOR 'SEL*CHAIN)
							 (REMPROP 'SEL=TERMINATOR 'SEL*CHAIN)
							 (SETQ SEL*TERMINATOR.NEW.CLAUSES NIL))
						    SEL*FINAL.ACTIONS))
		      NIL)))))))

(DEFUN SEL=TERMINATOR.UPDATE (&OPTIONAL INITIALFLAG)
						; edited: 10. 8. 1984
						; input:  a flag indicating the status of the
						;         refutation
						; effect: the two lists sel*terminator.new.clauses
						;         and sel*terminator.look.new.clauses
						;         are updated.
						;         if the actual operation is not
						;         sel*terminator.units, the terminator is
						;         called in fastmode. if a refutation is
						;         found, this function is left with
						;         sel=return.
						; value:  undefined.
  (COND (INITIALFLAG (SETQ SEL*TERMINATOR.NEW.CLAUSES (COPY-LIST (CG-CLAUSES ALL)))
		     (SETQ SEL*TERMINATOR.LOOK.NEW.CLAUSES (COPY-LIST (CG-CLAUSES ALL))))
	((EQL SEL*ACTUAL.OPERATION 'SEL=TERMINATOR.UNITS)
	 (SETQ SEL*TERMINATOR.NEW.CLAUSES (NSET-DIFFERENCE SEL*TERMINATOR.NEW.CLAUSES (CG-CLAUSES REMOVED)))
	 (SETQ SEL*TERMINATOR.LOOK.NEW.CLAUSES
	       (NSET-DIFFERENCE SEL*TERMINATOR.LOOK.NEW.CLAUSES (CG-CLAUSES REMOVED))))
	(T (SETQ SEL*TERMINATOR.NEW.CLAUSES
		 (APPEND (CG-CLAUSES INSERTED) (CG-CLAUSES CHANGED)
			 (COPY-LIST (NSET-DIFFERENCE SEL*TERMINATOR.NEW.CLAUSES (CG-CLAUSES REMOVED)))))
	   (SETQ SEL*TERMINATOR.LOOK.NEW.CLAUSES
		 (APPEND (CG-CLAUSES INSERTED) (CG-CLAUSES CHANGED)
			 (COPY-LIST (NSET-DIFFERENCE SEL*TERMINATOR.LOOK.NEW.CLAUSES (CG-CLAUSES REMOVED)))))))
  (PROG (RESULT)
	(CASE SEL*REDUCE.DEDUCE
	  (REDUCE)
	  (DEDUCE
	    (when (not (or (opt-is.with.residues)
			   (/= (opt-get.option er_narrow.depth) 0)))
	      (SETQ RESULT
		    (TERM-TERMINATOR (CG-CLAUSES ALL) SEL*TERMINATOR.LOOK.NEW.CLAUSES (SEL=CLAUSES SUPPORTED) T)))
	    (SETQ SEL*TERMINATOR.LOOK.NEW.CLAUSES NIL)
	    (COND ((EQL 'PROVED (CAR RESULT))
		   (SEL=MAKE.DEDUCTION.CODE (CONS 'R.CHAIN (CDR RESULT)) NIL)
		   (SEL=RETURN))))
	  (OTHERWISE (ERROR "illegal reduce.deduce in sel=terminator.update: ~a" SEL*REDUCE.DEDUCE)))))

(DEFUN SEL=TERMINATOR.UNITS NIL
						; edited: 10. 8. 1984
						; effect: the deduction code for generating the
						;         unitclauses, found in
						;         sel=terminator.activate? is generated.
						;         the new unitclauses are reduced.
						;         single unifiers in the resolution chain
						;         are removed.
						; value:  undefined.
  (PROG ((CHAIN (GET 'SEL=TERMINATOR 'SEL*CHAIN)) REMOVE.UNIFIERS LINKS)
	(CASE SEL*REDUCE.DEDUCE
	  (DEDUCE
	    (COND
	      (CHAIN (REMPROP 'SEL=TERMINATOR 'SEL*CHAIN)
		     (SEL=MAKE.DEDUCTION.CODE CHAIN NIL)
		     (MAPC
		       #'(LAMBDA (ELEMENT) (SETQ LINKS (THIRD ELEMENT))
				 (COND
				   ((OR (CDR LINKS) (NEQ 'R (DS-LINK.COLOUR (CAR LINKS)))
					(NEQ 1 (DS-CLAUSE.NOLIT (DS-LINK.OTHERPAR (CAR LINKS) (SECOND ELEMENT))))))
				   ((CDR (DS-LINK.UNIFIERS (CAR LINKS)))
				    (MEMBER-IF
				      #'(LAMBDA (UNIFIER)
					  (COND
					    ((UNI-WEAK.INSTANCE UNIFIER (CAR ELEMENT)
								(DS-CLAUSE.VARIABLES (SECOND ELEMENT)) NIL T)
					     (SETQ REMOVE.UNIFIERS
						   (CONS (CONS (CAR LINKS) (CONS UNIFIER 'ACTIVE)) REMOVE.UNIFIERS)))))
				      (DS-LINK.UNIFIERS (CAR LINKS))))
				   (T
				    (SETQ REMOVE.UNIFIERS
					  (CONS (CONS (CAR LINKS) (CONS (CAR (DS-LINK.UNIFIERS (CAR LINKS))) 'ACTIVE))
						REMOVE.UNIFIERS)))))
		       (CDR CHAIN))
		     (SETF (GET 'SEL=TERMINATOR.UNITS 'SEL*REMOVE.UNIFIERS) REMOVE.UNIFIERS) (SEL=RETURN))
	      (T (SEL=PASS.CONTROL NIL NIL))))
	  (REDUCE (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) NIL (GET 'SEL=TERMINATOR.UNITS 'SEL*REMOVE.UNIFIERS))
		  (REMPROP 'SEL=TERMINATOR.UNITS 'SEL*REMOVE.UNIFIERS) (SEL=RETURN))
	  (OTHERWISE (ERROR "Illegal SEL*REDUCE.DEDUCE in SEL=TERMINATOR.UNITS: ~a" SEL*REDUCE.DEDUCE)))))

(DEFUN SEL=TERMINATOR.UNITS.ACTIVATE? NIL
						; edited: 20-jan-83 13:10:42
						; value:  t if unitclauses can be derived, else nil
  (GET 'SEL=TERMINATOR 'SEL*CHAIN))

(DEFUN SEL=TERMINATOR.UNITS.UPDATE (&OPTIONAL INITIALFLAG)
  (DECLARE (IGNORE INITIALFLAG))
						; edited: 20-jan-83 13:09:20
						;         *** dummy function ***
  NIL)

(DEFVAR SEL*TERMINATOR.PROVED.FLAG NIL)

(DEFVAR SEL*TERMINATOR.LOOK NIL)

(DEFUN SEL=LINK.NOLIT (LINK UNIFIER)
						; edited: 10. 7. 1984
						; input:  an r-link and one of its unifiers.
						; value:  an estimation about the length of the
						;         resolvent. (with merging and replacement
						;         resolution with units).
  (PROG ((CLAUSE1 (DS-LINK.POSPAR LINK))
	 (CLAUSE2 (DS-LINK.NEGPAR LINK))
	 (LIT1 (DS-LINK.POSLITNO LINK))
	 (LIT2 (DS-LINK.NEGLITNO LINK))
	 NOLIT1 NOLIT2 NOLIT CLAUSE LITS LIT)
    (UNI-SET.BINDINGS.OF.SUBSTITUTION UNIFIER)
    (SETQ NOLIT1 (DS-CLAUSE.NOLIT CLAUSE1))
    (SETQ NOLIT2 (DS-CLAUSE.NOLIT CLAUSE2))
    (SETQ LITS (+ NOLIT1 NOLIT2 -2))
    (SETQ CLAUSE CLAUSE1)
    (SETQ LIT LIT1)
    (SETQ NOLIT NOLIT1)
    (DODOWN (RPTN 2)
      (PROGN (UNI-CONSTANTIFY (DS-CLAUSE.VARIABLES CLAUSE))
	     (DODOWN (RPTN NOLIT)
	       (COND ((AND (NEQ (1+ RPTN) LIT)
			   (flet ((sel=member (links)
				    (MEMBER-IF #'(LAMBDA (RLINK)
						   (AND (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.OTHERPAR RLINK CLAUSE)))
							(MEMBER-IF #'(LAMBDA (UNIFIER)
								       (UNI-UNIFY.MIXED.TERMLIST UNIFIER T))
								   (DS-LINK.UNIFIERS RLINK))))
					       links)))
			     (or (sel=member (DS-CLAUSE.LINKS 'RD CLAUSE (1+ RPTN)))
				 (sel=member (DS-CLAUSE.LINKS 'R CLAUSE (1+ RPTN))))))
		      (SETQ LITS (1- LITS))
		      (DS-CLAUSE.LIT.PUTPROP CLAUSE (1+ RPTN) 'SEL*REMOVED T))))
	     (SETQ CLAUSE CLAUSE2)
	     (SETQ LIT LIT2)
	     (SETQ NOLIT NOLIT2)))
    (UNI-CLEAR.VARIABLES.AS.CONSTANTS)
    (DS-CLAUSE.LIT.PUTPROP CLAUSE1 LIT1 'SEL*REMOVED T)
    (DS-CLAUSE.LIT.PUTPROP CLAUSE2 LIT2 'SEL*REMOVED T)
    ;; MERGETEST 
    (DODOWN (RPTN NOLIT2)
      (COND
        ((NOT (DS-CLAUSE.LIT.GETPROP CLAUSE2 (1+ RPTN) 'SEL*REMOVED))
	 (MAPC #'(LAMBDA (SILINK)
		   (SETQ LIT (DS-LINK.OTHERLITNO SILINK CLAUSE2 (1+ RPTN)))
		   (WHEN (< LIT (1+ RPTN))
		     (SETQ LITS (SEL=LINK.NOLIT.MERGES CLAUSE1 LIT1 NOLIT1 CLAUSE2 LIT2 NOLIT2 CLAUSE2 LIT CLAUSE2
						       (1+ RPTN) (DS-LINK.UNIFIERS SILINK) LITS))))
	       (DS-CLAUSE.LINKS '(SID si) CLAUSE2 (1+ RPTN)))
	 (MAPC
	   #'(LAMBDA (SLINK)
	       (COND
		 ((EQL CLAUSE1 (DS-LINK.OTHERPAR SLINK CLAUSE2))
		  (SETQ LITS
			(SEL=LINK.NOLIT.MERGES CLAUSE1 LIT1 NOLIT1 CLAUSE2 LIT2 NOLIT2 CLAUSE1
					       (DS-LINK.OTHERLITNO SLINK CLAUSE2 (1+ RPTN)) CLAUSE2 (1+ RPTN)
					       (DS-LINK.UNIFIERS SLINK) LITS)))))
	   (DS-CLAUSE.LINKS 'S CLAUSE2 (1+ RPTN))))))
    (DODOWN (RPTN NOLIT1)
      (COND
        ((NOT (DS-CLAUSE.LIT.GETPROP CLAUSE1 (1+ RPTN) 'SEL*REMOVED))
	 (MAPC
	   #'(LAMBDA (SILINK) (SETQ LIT (DS-LINK.OTHERLITNO SILINK CLAUSE1 (1+ RPTN)))
		     (COND
		       ((< LIT (1+ RPTN))
			(SETQ LITS
			      (SEL=LINK.NOLIT.MERGES CLAUSE1 LIT1 NOLIT1 CLAUSE2 LIT2 NOLIT2 CLAUSE1 LIT CLAUSE1
						     (1+ RPTN) (DS-LINK.UNIFIERS SILINK) LITS)))))
	   (DS-CLAUSE.LINKS 'SI CLAUSE1 (1+ RPTN))))))
    (DS-CLAUSE.ALL.LIT.REMPROP CLAUSE1 'SEL*REMOVED)
    (DS-CLAUSE.ALL.LIT.REMPROP CLAUSE2 'SEL*REMOVED)
    (UNI-RESET.BINDINGS.OF.SUBSTITUTION UNIFIER)
    (RETURN LITS)))

(defun sel=link.nolit.unify.p (unifier vars)
  (SSOME #'(LAMBDA (VAR1)
	     (MEMBER-IF #'(LAMBDA (VAR2)
			    (OR (EQL VAR1 VAR2) (IN VAR1 (DT-VARIABLE.GET.BINDING VAR2))))
			VARS))
	 #'CDDR
	 UNIFIER))


(DEFUN SEL=LINK.NOLIT.MERGES
       (CLAUSE1 LIT1 NOLIT1 CLAUSE2 LIT2 NOLIT2 MCLAUSE1 MLIT1 MCLAUSE2 MLIT2 UNIFIERS LITS)
						; edited: 18. 7. 1984
						; input  the two parentclauses of the rlink given to
						;        sel=link.nolit and two literals connected
						;        by an slink or silink with its unifiers.
						;        lits is an estimation for the number of
						;        literals in the resolvent.
						; value: the new estimation for lits.
  (COND
    ((AND (NOT (DS-CLAUSE.LIT.GETPROP MCLAUSE1 MLIT1 'SEL*REMOVED))
	  (NOT (DS-CLAUSE.LIT.GETPROP MCLAUSE2 MLIT2 'SEL*REMOVED)))
     (PROG
       (AFFECTED1 AFFECTED2 NOT.AFFECTED1 NOT.AFFECTED2 VARS
	(VARIABLES (DS-CLAUSE.LIT.VARIABLES MCLAUSE2 MLIT2))
	(CLAUSE MCLAUSE1)
	(LIT MLIT1))
       (DODOWN (RPTN 2)
	 (MEMBER-IF #'(LAMBDA (UNIFIER)
			(AND (SETQ UNIFIER (UNI-UNIFIER.BECOMES.MATCHER UNIFIER VARIABLES NIL T))
			     (COND
			       ((NULL (CAR UNIFIER)) (DS-CLAUSE.LIT.PUTPROP MCLAUSE2 MLIT2 'SEL*REMOVED T) (SETQ LITS (1- LITS))
				(SETQ RPTN 0))
			       (T
				(MEMBER-IF
				  #'(LAMBDA (UNIFIER) (SETQ AFFECTED1 NIL) (SETQ AFFECTED2 NIL) (SETQ NOT.AFFECTED1 NIL)
					    (SETQ NOT.AFFECTED2 NIL)
					    (DODOWN (RPTN NOLIT1)
					      (COND
						((OR (EQL (1+ RPTN) LIT1) (AND (EQL CLAUSE1 CLAUSE) (EQL (1+ RPTN) LIT))))
						((AND (NOT (DS-CLAUSE.LIT.GETPROP CLAUSE1 (1+ RPTN) 'SEL*REMOVED))
						      (PROGN (SETQ VARS (DS-CLAUSE.LIT.VARIABLES CLAUSE1 (1+ RPTN))) T)
						      (sel=link.nolit.unify.p unifier vars))
						 (SETQ AFFECTED1 (CONS (1+ RPTN) AFFECTED1)))
						(T (SETQ NOT.AFFECTED1 (CONS (1+ RPTN) NOT.AFFECTED1)))))
					    (DODOWN (RPTN NOLIT2)
					      (COND
						((OR (EQL (1+ RPTN) LIT2) (AND (EQL CLAUSE2 CLAUSE) (EQL (1+ RPTN) LIT))))
						((AND (NOT (DS-CLAUSE.LIT.GETPROP CLAUSE2 (1+ RPTN) 'SEL*REMOVED))
						      (PROGN (SETQ VARS (DS-CLAUSE.LIT.VARIABLES CLAUSE2 (1+ RPTN))) T)
						      (SSOME
							#'(LAMBDA (VAR1)
							    (MEMBER-IF
							      #'(LAMBDA (VAR2)
								  (OR (EQL VAR1 VAR2)
								      (IN VAR1 (DT-VARIABLE.GET.BINDING VAR2))))
							      VARS))
							#'CDDR UNIFIER))
						 (SETQ AFFECTED2 (CONS (1+ RPTN) AFFECTED2)))
						(T (SETQ NOT.AFFECTED2 (CONS (1+ RPTN) NOT.AFFECTED2)))))
					    (COND
					      ((OR AFFECTED1 AFFECTED2) (UNI-SET.BINDINGS.OF.SUBSTITUTION UNIFIER)
					       (COND
						 ((AND
						    (SEL=LINK.NOLIT.AFFECTED.MERGE AFFECTED1 CLAUSE1 NOT.AFFECTED1 CLAUSE2
										   NOT.AFFECTED2)
						    (SEL=LINK.NOLIT.AFFECTED.MERGE AFFECTED2 CLAUSE2 NOT.AFFECTED2 CLAUSE1
										   NOT.AFFECTED1))
						  (MAPC
						    #'(LAMBDA (LITNO) (DS-CLAUSE.LIT.PUTPROP CLAUSE1 LITNO 'SEL*REMOVED T)
							      (SETQ LITS (1- LITS)))
						    AFFECTED1)
						  (MAPC
						    #'(LAMBDA (LITNO) (DS-CLAUSE.LIT.PUTPROP CLAUSE2 LITNO 'SEL*REMOVED T)
							      (SETQ LITS (1- LITS)))
						    AFFECTED2)
						  (DS-CLAUSE.LIT.PUTPROP CLAUSE LIT 'SEL*REMOVED T) (SETQ LITS (1- LITS))
						  (SETQ AFFECTED1 T) (SETQ RPTN 0))
						 (T (SETQ AFFECTED1 NIL)))
					       (UNI-RESET.BINDINGS.OF.SUBSTITUTION UNIFIER T) AFFECTED1)
					      (T (DS-CLAUSE.LIT.PUTPROP CLAUSE LIT 'SEL*REMOVED T) (SETQ LITS (1- LITS))
						 (SETQ RPTN 0))))
				  UNIFIER)))))
		    UNIFIERS)
	 (SETQ CLAUSE MCLAUSE2)
	 (SETQ LIT MLIT2)
	 (SETQ VARIABLES (DS-CLAUSE.LIT.VARIABLES MCLAUSE1 MLIT1))))))
  LITS)

(DEFUN SEL=LINK.NOLIT.AFFECTED.MERGE (AFFECTED CLAUSE NOT.AFFECTED CLAUSE2 NOT.AFFECTED2)
						; edited: 18. 7. 1984
						; input:  affected is the list of literal numbers
						;         (belonging to clause) which are changed
						;         by factoring after resolution upon the
						;         link given to sel=link.nolit.
						;         clause2 is the other parent clause of the
						;         rlink. not.affected and not.affected2 are
						;         the list of literals which are not changed
						;         by factoring.
						; value:  t if every affected literal is either
						;         resolved away by unit resolution or merges
						;         into a not affected literal.
  (EVERY
    #'(LAMBDA (LITNO)
        (OR
          (PROG2 (UNI-CONSTANTIFY (DS-CLAUSE.LIT.VARIABLES CLAUSE LITNO))
		 (MEMBER-IF
		   #'(LAMBDA (LINK)
		       (AND (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.OTHERPAR LINK CLAUSE)))
			    (MEMBER-IF #'(LAMBDA (UNIFIER) (UNI-UNIFY.MIXED.TERMLIST UNIFIER T)) (DS-LINK.UNIFIERS LINK))))
		   (APPEND (DS-CLAUSE.LINKS 'RD CLAUSE LITNO) (COPY-LIST (DS-CLAUSE.LINKS 'R CLAUSE LITNO))))
		 (UNI-CLEAR.VARIABLES.AS.CONSTANTS))
          (MEMBER-IF
            #'(LAMBDA (LINK)
                (AND (MEMBER (DS-LINK.OTHERLITNO LINK CLAUSE LITNO) NOT.AFFECTED)
		     (MEMBER-IF #'(LAMBDA (UNIFIER) (UNI-BINDING.IS.INSTANCE.OF UNIFIER)) (DS-LINK.UNIFIERS LINK))))
            (APPEND (DS-CLAUSE.LINKS 'SID CLAUSE LITNO) (COPY-LIST (DS-CLAUSE.LINKS 'SI CLAUSE LITNO))))
          (MEMBER-IF
            #'(LAMBDA (LINK)
                (AND (EQL CLAUSE2 (DS-LINK.OTHERPAR LINK CLAUSE))
		     (MEMBER (DS-LINK.OTHERLITNO LINK CLAUSE LITNO) NOT.AFFECTED2)
		     (MEMBER-IF #'(LAMBDA (UNIFIER) (UNI-BINDING.IS.INSTANCE.OF UNIFIER)) (DS-LINK.UNIFIERS LINK))))
            (DS-CLAUSE.LINKS 'S CLAUSE LITNO))))
    AFFECTED))

(DEFUN SEL=MAKE.DEDUCTION.CODE (LINK UNIFIER)
						; edited:  3. 6. 1982   hjo
						; input:   the link and unifier to be operated on in
						;          the next deduction step.
						;          linklist is a list of r- or p-links of
						;          the descedants shall be determined.
						; effect:  constructs the actual deduction code.
						;          if unifier is the atom first, the first
						;          unifier of link is used.
						; value:   the deduction code.
  (SETQ SEL*ACTUAL.DEDUCTION.CODE
	(COND (LINK (CONS LINK (if (EQL UNIFIER 'FIRST) (CAR (DS-LINK.UNIFIERS LINK)) UNIFIER))))))

(DEFUN SEL=MAKE.REDUCTION.CODE (CLAUSES.TO.BE.REDUCED CLAUSES.TO.BE.REMOVED LINKS.TO.BE.REMOVED)
						; edited: 3. 6. 1982   hjo
						; input:  clauses.to.be.reduced is a simple list of
						;         clauses.
						;         clauses.to.be.removed is a list of elements
						;         which might be clause addresses or
						;         dotted pairs (clause . reason).
						;         links.to.be.removed is a list of elements
						;         which may be link addresses or
						;         dotted pairs (link . unifier) or
						;         elements (link unifier . reason).
						;         the different possibilities may be used
						;         mixed in the same list.
						;         each list may be empty
						; effect: constructs the reduction code.
						;         if the reason in clauses.to.be.removed is
						;         not given, nil ('unknown') is assumed.
						;         if the unifier in links.to.be.removed is not
						;         given, the first unifier is used.
						;         if the reason is not given, 'active' is used
  (SETQ SEL*ACTUAL.REDUCTION.CODE
	`(,CLAUSES.TO.BE.REDUCED
	  ,(MAPCAR #'(LAMBDA (CLAUSE) (COND ((ATOM CLAUSE) (LIST CLAUSE)) (T CLAUSE))) CLAUSES.TO.BE.REMOVED)
	  ,@(MAPCAN #'(LAMBDA (LINK)
			(COND ((ATOM LINK)	; link = l
			       (IF (DS-LINK.IS.MARKED INHIBITED LINK)
				   NIL
				   (LIST (CONS LINK (CONS (CAR (DS-LINK.UNIFIERS LINK)) 'ACTIVE)))))
			      ((OR (NULL (CDR LINK)) (DT-VARIABLE.IS (SECOND LINK)))	; link = (l . u)
			       (COND
				 ((NOT (DS-LINK.IS.MARKED INHIBITED (CAR LINK)))
				  (LIST (CONS (CAR LINK) (CONS (CDR LINK) 'ACTIVE))))
				 (T NIL)))
			      (T		; link = (l u . r)
			       (COND ((NOT (DS-LINK.IS.MARKED INHIBITED (CAR LINK))) (LIST LINK))
				     (T NIL)))))
		    LINKS.TO.BE.REMOVED))))

(DEFMACRO SEL=CLEAR.LINK.CLASSES NIL
						; edited:  24. 5. 1982    hjo
						; effect:  for every toplevel element of
						;          sel*link.classes an empty tconc list is
						;          created.
						; example: sel*link.classes =
						;          (((favoured) . r)((pure and snowball))
						;          two lists are created:
						;          sel*links_rfavoured  and
						;          sel*links_pureandsnowball .
						;          additionally atoms are created to store
						;          if the link colour is part of the name.
						;          example:
						;          sel*links.colour.flag:favoured = r
						;          sel*links.colour.flag:pureandsnowball = nil
						;         the linklists are initiated with nil
  `(PROGN ,@(MAPCAN
	      #'(LAMBDA (CLASS)
		  (LIST `(SETQ ,(INTERN (APPLY #'CONCATENATE 'STRING
					       (MAPCAR #'SYMBOL-NAME (DELETE NIL (NCONC (LIST 'SEL*LINKS_ (CDR CLASS))
											(CAR CLASS)))))
					(find-package "MKRP"))
			       (LIST NIL))
			`(SETQ ,(INTERN (APPLY #'CONCATENATE 'STRING "SEL*LINKS.COLOUR.FLAG_"
					       (MAPCAR #'SYMBOL-NAME (CAR CLASS)))
					(find-package "MKRP"))
			       ',(CDR CLASS))))
	      SEL*LINK.CLASSES)
	  (MAPC #'(LAMBDA (LINKLIST) (SETF (SYMBOL-VALUE LINKLIST) NIL)) SEL*LINKLISTS)))

(DEFVAR SEL*LINKS_RACTIVE)

(DEFVAR SEL*LINKS_piwACTIVE)

(DEFVAR SEL*LINKS.COLOUR.FLAG_ACTIVE)







(DEFVAR SEL*LINKS.COLOUR.FLAG_PASSIVE)



(DEFVAR SEL*LINKS.COLOUR.FLAG_INHIBITED)

(DEFVAR SEL*LINKS_REDUCTIONS)

(DEFVAR SEL*LINKS.COLOUR.FLAG_REDUCTIONS)

(DEFMACRO SEL=CLEAR.CLAUSE.CLASSES NIL
						; edited:  24. 5. 1982   hjo
						; effect:  for every toplevel element x in
						;          sel*clause.classes this function creates
						;          an empty tconc list with the name
						;          (pack (cons 'sel*clauses: x))
						; example: sel*clause.classes = ((theoremclauses))
						;          a tconc list    sel*clauses:theoremclauses
						;          is created.
						;          the clauselists are initiated with nil
						; value:   undefined.
  `(PROGN ,@(MAPCAR
	      #'(LAMBDA (CLASS) `(SETQ ,(INTERN (APPLY #'CONCATENATE 'STRING "SEL*CLAUSES_"
						       (MAPCAR #'SYMBOL-NAME CLASS))
						(find-package "MKRP"))
				       (LIST NIL)))
	      SEL*CLAUSE.CLASSES)
	  (MAPC #'(LAMBDA (CLAUSELIST) (SETF (SYMBOL-VALUE CLAUSELIST) NIL)) SEL*CLAUSELISTS)))

(DEFVAR SEL*CLAUSES_SUPPORTED NIL)

(DEFVAR SEL*CLAUSES_INDUCTION NIL)

(DEFVAR SEL*CLAUSES_EQUIVALENCE NIL)

(DEFVAR SEL*CLAUSES_EQUALITY NIL)

(DEFVAR SEL*CLAUSES_IMPLICATION NIL)

(DEFVAR SEL*CLAUSES_DEDUCTION-RULES NIL)

(DEFVAR SEL*CLAUSES_ELIMINATINGEQUATIONS NIL)

(DEFVAR SEL*TERMINATOR.LOOK.NEW.CLAUSES NIL)

(DEFUN SEL=UPDATE.CLAUSELISTS NIL
						; edited: 13-jan-83 17:46:10
						; effect: removed clauses are removed from
						;         the lists defined by sel*clauselists
						; value:  undefined
  (MAPC #'(LAMBDA (CLAUSE)
	    (MAPC #'(LAMBDA (CLAUSELIST)
		      (let ((CLAUSES (EVAL CLAUSELIST)))
			(SETF (SYMBOL-VALUE CLAUSELIST)
			      (if (CONSP (CAR CLAUSES))
				  (REMASSOC CLAUSE CLAUSES)
				  (DELETE CLAUSE CLAUSES)))))
		  SEL*CLAUSELISTS))
	(CG-CLAUSES REMOVED)))

(DEFUN SEL=UPDATE.LINKLISTS NIL
						; edited: 14-jan-83 16:38:16
						; effect: removed r- and p-links are removed from
						;         the lists defined by sel*linklists.
						; value:  undefined.
  (MAPC #'(LAMBDA (LINK)
	    (setf (GET 'SEL=STRATEGIES 'SEL*INSERTED.PLINKS)
		  (delete link (GET 'SEL=STRATEGIES 'SEL*INSERTED.PLINKS))))
	(CG-LINKS '(P piw) REMOVED))
  (MAPC #'(LAMBDA (LINK)
	    (MAPC #'(LAMBDA (LINKLIST)
		      (LET ((LINKS (symbol-value LINKLIST)))
			(SETF (SYMBOL-VALUE LINKLIST)
			      (COND ((CONSP (CAR LINKS)) (REMASSOC LINK LINKS))
				    (T                   (DELETE LINK LINKS))))))
		  SEL*LINKLISTS))
	(CG-LINKS '(P piw r) REMOVED))
  (MAPC #'(LAMBDA (LINKLIST)
	    (MAPC #'(LAMBDA (LINK) (COND ((DS-LINK.IS.MARKED INHIBITED LINK) (SEL=INHIBIT LINK))))
		  LINKLIST))
	(LIST (CG-LINKS R INSERTED) (CG-LINKS P INSERTED) (CG-LINKS R CHANGED) (CG-LINKS P CHANGED))))

(DEFMACRO SEL=LINKS (&REST INDICATORS)
						; edited:  24. 5. 1982   hjo
						; input:   some atoms indicating a list created by
						;          sel=create.link.classes.
						;          if necessary, the link colour should be
						;          the first element of indicators.
						; value:   the list  given by indicators.
  `(CAR ,(INTERN (APPLY #'CONCATENATE 'STRING "SEL*LINKS_" (MAPCAR #'SYMBOL-NAME INDICATORS))
		 (find-package "MKRP"))))

(DEFMACRO SEL=CLAUSES (&REST INDICATORS)
						; edited:  24. 5. 1982   hjo
						; input:   some atoms indicating a list created by
						;          sel=create.clause.classes.
						; value:   the list given by indicators.
  `(CAR ,(INTERN (APPLY #'CONCATENATE 'STRING "SEL*CLAUSES_" (MAPCAR #'SYMBOL-NAME INDICATORS))
		 (find-package "MKRP"))))

(DEFMACRO SEL=INSERT.LINK (LINK FRONTFLAG &REST INDICATORS)
						; edited:  24. 5. 1982   hjo
						; input:   a link, a boolean value and some atoms.
						;          indicators should not contain the link
						;          colour.
						; effect:  inserts the link into the list given by
						;          indicators.
						;          if frontflag* = t , link is inserted in
						;          front of the list, else at the end.
						; value:   undefined.
  (LET (COLOURS CLASS)
    (COND
      ((MEMBER-IF #'(LAMBDA (CLASS) (EQUAL INDICATORS (CAR CLASS))) SEL*LINK.CLASSES)
       (MAPC #'(LAMBDA (CLASS)
		 (when (AND (EQUAL INDICATORS (CAR CLASS)) (CDR CLASS))
		   (push (CDR CLASS) COLOURS)))
	     SEL*LINK.CLASSES)
       (COND (COLOURS `(CASE (DS-LINK.COLOUR ,LINK)
			 ,@(MAPCAR #'(LAMBDA (COLOUR)
				       (SETQ CLASS
					     (INTERN (APPLY #'CONCATENATE 'STRING
							    (CONS "SEL*LINKS_" (CONS (SYMBOL-NAME COLOUR)
										     (MAPCAR #'SYMBOL-NAME INDICATORS))))
						     (find-package "MKRP")))
				       (LIST COLOUR
					     (COND ((EQL FRONTFLAG T) (LIST 'QCONS LINK CLASS))
						   ((NULL FRONTFLAG) (LIST 'QCONC1 CLASS LINK))
						   (T (LIST 'COND (LIST FRONTFLAG (LIST 'QCONS LINK CLASS))
							    (LIST T (LIST 'QCONC1 CLASS LINK)))))))
				   COLOURS)
			 (OTHERWISE (ERROR "illegal link colour in sel=insert.link" (DS-LINK.COLOUR ,LINK)))))
	     (T (SETQ CLASS (INTERN (APPLY #'CONCATENATE 'STRING
					   (CONS "SEL*LINKS_" (MAPCAR #'SYMBOL-NAME INDICATORS)))
				    (find-package "MKRP")))
		(COND ((EQ FRONTFLAG T) (LIST 'QCONS LINK CLASS))
		      ((NULL FRONTFLAG) (LIST 'QCONC1 CLASS LINK))
		      (T (LIST 'COND (LIST FRONTFLAG (LIST 'QCONS LINK CLASS)) (LIST T (LIST 'QCONC1 CLASS LINK))))))))
      (T (TERPRI)
	 (PROGN
	   (error "illegal indicators in sel=insert.link: ~A" INDICATORS)
	   (TERPRI))
	 (TERPRI) NIL))))

(DEFMACRO SEL=INSERT.CLAUSE (CLAUSE FRONTFLAG &REST INDICATORS)
						; edited:  24. 5. 1982   hjo
						; input:   a clause, a boolean value and some atoms
						; effect:  inserts the clause into the list given by
						;          indicators.
						;          if frontflag* = t clause* will be inserted
						;          in front of the list else at the end.
						; value:   undefined.
  (LET ((CLASS (LIST 'SYMBOL-VALUE (KWOTE (INTERN (APPLY #'CONCATENATE 'STRING
							 (CONS "SEL*CLAUSES_"
							       (MAPCAR #'SYMBOL-NAME INDICATORS)))
						  (find-package "MKRP"))))))
    (COND
      ((MEMBER-IF #'(LAMBDA (CLASS) (EQUAL CLASS INDICATORS)) SEL*CLAUSE.CLASSES)
       (COND ((EQL T FRONTFLAG) (LIST 'QCONS CLAUSE CLASS))
	     ((NULL FRONTFLAG) (LIST 'QCONC1 CLASS CLAUSE))
	     (T (LIST 'COND (LIST FRONTFLAG (LIST 'QCONS CLAUSE CLASS))
		      (LIST T (LIST 'QCONC1 CLASS CLAUSE))))))
      (T (TERPRI)
	 (PROGN
	   (PRINC
	     (CONCATENATE 'STRING "illegal indicators in sel=insert.clause: " (PRINC-TO-STRING INDICATORS)))
	   (TERPRI))))))


(DEFMACRO SEL=REMOVE.LINK (LINK &REST INDICATORS)
						; edited:  25. 5. 1982  hjo
						; input:   a link and some atoms (except  the link
						;          colour) or nil.
						; effect:  if indicators = nil the link is removed
						;          from every sel*links_ list.
						;          if indicators <> nil the link is removed
						;          from the list given by indicators.
  `(LET ((LINK ,LINK))
     ,(LET (COLOURS)
	(COND
	  (INDICATORS
	   (COND
	     ((MEMBER-IF #'(LAMBDA (CLASS) (EQUAL INDICATORS (CAR CLASS))) SEL*LINK.CLASSES)
	      (MAPC
		#'(LAMBDA (CLASS)
		    (COND ((AND (EQUAL INDICATORS (CAR CLASS)) (CDR CLASS)) (SETQ COLOURS (CONS (CDR CLASS) COLOURS)))))
		SEL*LINK.CLASSES)
	      (COND
		(COLOURS
		 (NCONC1
		   (NCONC (LIST 'CASE (LIST 'DS-LINK.COLOUR 'LINK))
			  (MAPCAR
			    #'(LAMBDA (COLOUR)
				(LIST COLOUR
				      (LIST 'QDELETE
					    (LIST 'SYMBOL-VALUE
						  (KWOTE
						    (INTERN (APPLY #'CONCATENATE 'STRING
								   (CONS "SEL*LINKS_"
									 (CONS (SYMBOL-NAME COLOUR)
									       (MAPCAR #'SYMBOL-NAME INDICATORS))))
							    (find-package "MKRP"))))
					    LINK)))
			    COLOURS))
		   `(OTHERWISE (ERROR "illegal link colour in sel=remove.link" (LIST 'DS-LINK.COLOUR ,LINK)))))
		(T
		 (LIST 'QDELETE
		       (LIST 'SYMBOL-VALUE (KWOTE (INTERN (APPLY #'CONCATENATE 'STRING
								 (CONS "SEL*LINKS_"
								       (MAPCAR #'SYMBOL-NAME INDICATORS)))
							  (find-package "MKRP"))))
		       LINK))))
	     ((member-if #'(lambda (class) (equal (first indicators) (cdr class))) sel*link.classes)
	      (CONS 'PROGN
		    (MAPCAR-not-nil
		      #'(LAMBDA (CLASS)
			  (if (equal (first indicators) (cdr class))
			      (LIST 'QDELETE
				    (INTERN
				      (APPLY #'CONCATENATE 'STRING
					     (CONS "SEL*LINKS_"
						   (MAPCAR #'SYMBOL-NAME (DELETE NIL (CONS (CDR CLASS) (CAR CLASS))))))
				      (find-package "MKRP"))
				    'LINK)
			      nil))
		      SEL*LINK.CLASSES)))
	     ((member-if #'(lambda (class) (equal (first indicators) class)) sel*link.classes)
	      (LIST 'QDELETE
		    (INTERN
		      (APPLY #'CONCATENATE 'STRING
			     (CONS "SEL*LINKS_"
				   (MAPCAR #'SYMBOL-NAME (DELETE NIL (CONS (CDR (first iNDICATORS))
									   (CAR (first iNDICATORS)))))))
		      (find-package "MKRP"))
		    'LINK))
	     (T (format *debug-io* "~%Illegal indicators in SEL=REMOVE.LINK: ~A~%" INDICATORS) NIL)))
	  (T
	   (CONS 'PROGN
		 (MAPCAR
		   #'(LAMBDA (CLASS)
		       (LIST 'QDELETE
			     (LIST 'SYMBOL-VALUE
				   (KWOTE
				     (INTERN
				       (APPLY #'CONCATENATE 'STRING
					      (CONS "SEL*LINKS_"
						    (MAPCAR #'SYMBOL-NAME (DELETE NIL (CONS (CDR CLASS) (CAR CLASS))))))
				       (find-package "MKRP"))))
			     'LINK))
		   SEL*LINK.CLASSES)))))))

(DEFMACRO SEL=REMOVE.CLAUSE (CLAUSE &REST INDICATORS)
						; edited:  25. 5. 1982 hjo
						; input:   a clause and some atoms or nil.
						; effet:   if indicators = nil clause* is removed
						;          from every sel*clauses_ list,
						;          else it is removed only from the list
						;          given by indicators.
  (LET (CLASS)
    (if INDICATORS
	(COND
	  ((MEMBER-IF #'(LAMBDA (CLASS) (EQUAL CLASS INDICATORS)) SEL*CLAUSE.CLASSES)
	   (SETQ CLASS
		 (LIST 'SYMBOL-VALUE (KWOTE (INTERN (APPLY #'CONCATENATE 'STRING
							   (CONS "SEL*CLAUSES_"
								 (MAPCAR #'SYMBOL-NAME INDICATORS)))
						    (find-package "MKRP")))))
	   (LIST 'QDELETE CLASS
		 (LIST 'COND (LIST (LIST 'LISTP (LIST 'CAAR CLASS)) (LIST 'ASSOC CLAUSE (LIST 'CAR CLASS))) (LIST T CLAUSE))))
	  (T (format *debug-io* "~%Illegal indicators in SEL=REMOVE.CLAUSE: ~A~%" INDICATORS)))
	`(PROGN ,@(MAPCAR #'(LAMBDA (CLASS)
			      (SETQ CLASS (INTERN (APPLY #'CONCATENATE 'STRING
							 (CONS "SEL*CLAUSES_" (MAPCAR #'SYMBOL-NAME CLASS)))
						  (find-package "MKRP")))
			      `(QDELETE ,CLASS (IF (LISTP (CAAR ,CLASS)) (ASSOC ,CLAUSE (CAR, CLASS)) ,CLAUSE)))
			  SEL*CLAUSE.CLASSES)))))

(DEFUN SEL=PASSIVATE (LINK &OPTIONAL FRONTFLAG)
						; edited: 30-jun-82 15:13:11
						; input:  an r- or p-link and a boolean value.
						; effect: passivates this link
						; value:  undefined.
  (COND
    ((AND (NOT (DS-LINK.IS.MARKED PASSIVE LINK))
	  (NOT (DS-LINK.IS.MARKED INHIBITED LINK))
	  (NOT (DS-LINK.IS.MARKED INHERITANCE.ONLY LINK)))
     (DS-LINK.MARK PASSIVE LINK)
     (CASE (DS-LINK.COLOUR LINK)
       (R (QDELETE SEL*LINKS_RACTIVE LINK)
          (COND (FRONTFLAG (QCONS LINK SEL*LINKS_RPASSIVE))
		(T (QCONC1 SEL*LINKS_RPASSIVE LINK))))
       (P (QDELETE SEL*LINKS_PACTIVE LINK)
          (COND (FRONTFLAG (QCONS LINK SEL*LINKS_PPASSIVE))
		(T (QCONC1 SEL*LINKS_PPASSIVE LINK))))
       (piw (QDELETE SEL*LINKS_PIWACTIVE LINK))
       (OTHERWISE (ERROR "illegal colour in sel=passivate: ~a" (DS-LINK.COLOUR LINK))))
     'passive)))

(DEFUN SEL=INHIBIT (LINK)			; edited: 30-jun-82 15:13:11
						; input:  an r- or p-link
						; effect: inhibits this link
						; value:  undefined.
  (COND
    ((AND (NOT (DS-LINK.IS.MARKED INHIBITED LINK)) (NOT (DS-LINK.IS.MARKED INHERITANCE.ONLY LINK)))
     (DS-LINK.MARK INHIBITED LINK)
     (CASE (DS-LINK.COLOUR LINK)
       (R (QDELETE SEL*LINKS_RACTIVE LINK) (QDELETE SEL*LINKS_RPASSIVE LINK) (QCONC1 SEL*LINKS_RINHIBITED LINK))
       (P (QDELETE SEL*LINKS_PACTIVE LINK) (QDELETE SEL*LINKS_PPASSIVE LINK) (QCONC1 SEL*LINKS_PINHIBITED LINK))
       (OTHERWISE (ERROR "illegal colour in sel=inhibit: ~a" (DS-LINK.COLOUR LINK)))))))

(DEFUN SEL=CHAIN NIL
						; edited:  15. 6. 1982   hjo
						;
						; this is the main control function to handle the
						; deduction chains calculated by the terminator etc.
						; deduction and reduction codes are determined
						; until sel*chain has been worked off.
						; intermediate results, except unit clauses are
						; deleted if sel*chain.delete.intermeditate.results
						; is set.  active links are deleted only if they are
						; connected to two-literal clauses.
						; (this is necessary because the intermediate results
						; are deleted).
						; sel*units is a list of the new unit clauses.
						;
						; value:  undefined
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE
      ;; structure of sel*chain: (chain1 chain2 "...") ".." each chain is a code determing how to reduce a
      ;;  clause to a unit clause by successive resolutions.
      ;; structure of a chain:  (unifier clause (l1 ... ln) (p1 ... pm))
      ;;  unifier is the merged unifier
      ;;  to be applied to clause. clause is the clause which shall be reduced to a unit clause. l1 "..." ln are
      ;;  either r-links or riw-links. p1 "..." pm are pointers to elements of linklists in chain occuring after
      ;;  the current chain.
      (COND
        ((NULL (SYMBOL-VALUE 'SEL=CHAIN))
	 ;; initialization: (car sel*chain.tail) points to a single deduction chain. sel*chain.links points to
	 ;;  the current chains linklist. 
	 (SETQ SEL=CHAIN T) (SETQ SEL*CHAIN.UNITS NIL) (SETQ SEL*CHAIN.TAIL SEL*CHAIN)
	 (SETQ SEL*CHAIN.LINKS (CADDAR SEL*CHAIN))))
      (SETQ SEL*CHAIN.LINKS (MEMBER-IF #'DS-LINK.IS SEL*CHAIN.LINKS))
      (COND (SEL*CHAIN.LINKS)
	    (T (SETQ SEL*CHAIN.TAIL (CDR SEL*CHAIN.TAIL))
	       (COND ((NULL SEL*CHAIN.TAIL) (SEL=PASS.CONTROL NIL NIL)) (T (SETQ SEL*CHAIN.LINKS (CADDAR SEL*CHAIN.TAIL))))))
      (COND
        (SEL*CHAIN.TAIL (SEL=MAKE.DEDUCTION.CODE (CAR SEL*CHAIN.LINKS) (CAAR SEL*CHAIN.TAIL))
			(RPLACA (CAR SEL*CHAIN.TAIL) NIL) (SETQ SEL*CHAIN.LINKS (CDR SEL*CHAIN.LINKS)))))
    (REDUCE
      (COND
        ((OR SEL*CHAIN.LINKS (CDR SEL*CHAIN.TAIL))
	 (COND ((NULL (CG-CLAUSES INSERTED)) (SETQ SEL*CHAIN.LINKS NIL) (SEL=RETURN)))
	 (C NOW THE REMAINING LINKS IN THE CURRENT CHAIN AND THE LINKS POINTED TO BY THE 4TH TOP-LEVEL
            ELEMENT IN THE CURRENT CHAIN ARE BEEING REPLACED BY THEIR DESCEDANTS *)
	 (MAPL #'(LAMBDA (TAIL) (RPLACA TAIL (CAR (CG-LINK_DESCENDANT.LINKS (CAR TAIL))))) SEL*CHAIN.LINKS)
	 (MAPC
	   #'(LAMBDA (TAIL)
	       (RPLACA TAIL
		       (CASE (DS-LINK.COLOUR (CAR TAIL)) (R (CAR (CG-LINK_DESCENDANT.LINKS (CAR TAIL))))
			     (RIW
			       (CAR
				 (MEMBER-IF
				   #'(LAMBDA (LINK)
				       (AND (EQL 'R (DS-LINK.COLOUR LINK))
					    (NOT
					      (MEMBER
						(COND
						  ((EQL (DS-LINK.POSPAR LINK) (CADAR SEL*CHAIN.TAIL)) (DS-LINK.POSLITNO LINK))
						  (T (DS-LINK.NEGLITNO LINK)))
						(SECOND (CDDDAR SEL*CHAIN.TAIL))))))
				   (CG-LINK_DESCENDANT.LINKS (CAR TAIL)))))
			     (OTHERWISE (ERROR "illegal link colour in sel=chain: ~a" (DS-LINK.COLOUR (CAR TAIL)))))))
	   (CAR (CDDDAR SEL*CHAIN.TAIL)))
	 (COND
	   ((CDR SEL*CHAIN.LINKS)
						; multiple occurences of a link caused by literal merging are removed
	    (RPLACD SEL*CHAIN.LINKS (CDR (INTERSECTION SEL*CHAIN.LINKS SEL*CHAIN.LINKS)))))
	 (COND
	   (SEL*CHAIN.LINKS
	    (SETF (GET 'SEL=CHAIN 'SEL*INTERMEDIATE.CLAUSES)
		  (APPEND (CG-CLAUSES INSERTED) (COPY-LIST (GET 'SEL=CHAIN 'SEL*INTERMEDIATE.CLAUSES)))))
	   (T (SETQ SEL*CHAIN.UNITS (NCONC SEL*CHAIN.UNITS (CG-CLAUSES INSERTED)))
						; the current deduction chain has been worked off
						; and is replaced by the deduced unit clause
              (RPLACA (CAR SEL*CHAIN.TAIL) (CAR (CG-CLAUSES INSERTED)))
						; intermediate results are removed
              (COND
                (SEL*CHAIN.DELETE.INTERMEDIATE.RESULTS
		 (SEL=MAKE.REDUCTION.CODE NIL
					  (MAPCAR #'(LAMBDA (CLAUSE) (CONS CLAUSE 'INTERMEDIATE.RESULT))
						  (GET 'SEL=CHAIN 'SEL*INTERMEDIATE.CLAUSES))
					  NIL))
                (T (SEL=MAKE.REDUCTION.CODE (GET 'SEL=CHAIN 'SEL*INTERMEDIATE.CLAUSES) NIL NIL)))
              (REMPROP 'SEL=CHAIN 'INTERMEDIATE.CLAUSES))))
        (T ;; the end of the chain code has been reached. new unit clauses will be examined for reductions.
	 ;; single.links will be removed if they have only one unifier, because the actually used unifier
	 ;; had been modified by the chain and so if there are more than one unifier, it is not possible to
	 ;; determine which one has been actually used. 
	 (SETQ SEL*CHAIN.UNITS (NCONC SEL*CHAIN.UNITS (CG-CLAUSES INSERTED)))
	 (SEL=MAKE.REDUCTION.CODE SEL*CHAIN.UNITS
				  (COND
				    (SEL*CHAIN.DELETE.INTERMEDIATE.RESULTS
				     (MAPCAR #'(LAMBDA (CLAUSE) (CONS CLAUSE 'INTERMEDIATE.RESULT))
					     (GET 'SEL=CHAIN 'SEL*INTERMEDIATE.CLAUSES))))
				  (REMOVE-DUPLICATES
				    (REMOVE-IF-NOT
				      #'(LAMBDA (LINK) (AND (DS-LINK.IS LINK) (NULL (CDR (DS-LINK.UNIFIERS LINK)))))
				      (COND (SEL*CHAIN.DELETE.INTERMEDIATE.RESULTS (GET 'SEL=CHAIN 'SEL*SINGLE.LINKS))
					    (T (MAPCAN #'CADDR SEL*CHAIN))))))
	 (REMPROPS 'SEL=CHAIN '(INTERMEDIATE.CLAUSES SINGLE.LINKS)))))
    (OTHERWISE (ERROR "illegal reduce.deduce in sel=chain : ~a" SEL*REDUCE.DEDUCE)))
  (SEL=RETURN))

(DEFVAR SEL*CHAIN NIL)

(DEFVAR SEL*CHAIN.DELETE.INTERMEDIATE.RESULTS NIL)

(DEFVAR SEL*CHAIN.UNITS NIL)

(DEFVAR SEL*CHAIN.LINKS NIL)

(DEFVAR SEL*CHAIN.TAIL NIL)

(DEFVAR SEL*ACTUAL.DEDUCTION.CODE NIL)

(DEFVAR SEL*ACTUAL.REDUCTION.CODE NIL)

(DEFVAR SEL*DUMMY NIL)

(DEFUN SEL=PASS.CONTROL (OPERATIONS VARIABLES.TO.BE.STORED &OPTIONAL RETURN.TO.OPERATION.FLAG)
						; edited: 26. 5. 1982  hjo
						; input:  operations is a list of operations to be
						;         performed before the next regular operation.
						;         variables.to.be.stored is a list of
						;         variables of which the top level value shall
						;         be saved if return.to.operation.flag = t .
						; effect: arranges sel*operation.stack in such a way
						;         that at first the operations in 'operations'
						;         are performed an after this the next
						;         regular operations.
						; value:  the current operation code.
  (COND
    (RETURN.TO.OPERATION.FLAG
     (MAPC #'(LAMBDA (VARIABLE) (PUSH (EVAL VARIABLE) SEL*OPERATION.STACK) (PUSH VARIABLE SEL*OPERATION.STACK))
	   VARIABLES.TO.BE.STORED)
     (PUSH (EVAL SEL*ACTUAL.OPERATION) SEL*OPERATION.STACK)
     (PUSH SEL*ACTUAL.OPERATION SEL*OPERATION.STACK)
     (PUSH (1+ (LIST-LENGTH VARIABLES.TO.BE.STORED)) SEL*OPERATION.STACK)
     (PUSH SEL*ACTUAL.OPERATION SEL*OPERATION.STACK)
						; the actual environment has been pushed onto the stack.
     )
    (T (SETF (SYMBOL-VALUE SEL*ACTUAL.OPERATION) NIL)))
  (when (AND (NULL OPERATIONS) (ISEMPTYSTACK SEL*OPERATION.STACK))
    ;; if the stack had been empty the next regular operation is pushed onto the stack 
    (PUSH 0 SEL*OPERATION.STACK)
    (PUSH (SEL=NEXT.OPERATION) SEL*OPERATION.STACK))
  (MAPC #'(LAMBDA (OPERATION) (PUSH 0 SEL*OPERATION.STACK) (PUSH OPERATION SEL*OPERATION.STACK))
	(REVERSE OPERATIONS))
  ;; now the stack contains at least one operation. the top operation will be performed as next one. 
  (SETQ SEL*ACTUAL.OPERATION (POP SEL*OPERATION.STACK))
  (DODOWN (RPTN (POP SEL*OPERATION.STACK)) (SETF (SYMBOL-VALUE (POP SEL*OPERATION.STACK)) (POP SEL*OPERATION.STACK)))
  (IF (FUNCTIONP SEL*ACTUAL.OPERATION)
      (APPLY SEL*ACTUAL.OPERATION NIL)
      (CERROR "continue without execution"
	      "sel*actual.operation ~a is not a function"  SEL*ACTUAL.OPERATION)))

(DEFUN SEL=NEXT.OPERATION NIL
						; edited:  6-jul-82 15:03:21
						; value:   the next operation to be performed.
						; remark:  the activate? functions of the operations
						;          are called in the order defined in
						;          sel*operations. the first activate?
						;          function which returns a value =//= nil
						;          determines the next operation.
  (FIRST (MEMBER-IF #'(LAMBDA (OPERATION)
			(funcall (SYMBOL-FUNCTION (INTERN (format nil "~A.ACTIVATE?" OPERATION) (find-package "MKRP")))))
		    SEL*OPERATIONS)))

(DEFUN SEL=RETURN NIL
						; edited:  2-jul-82 15:45:42
						; effect:  returns control to the control module.
  (MAPC #'EVAL SEL*FINAL.ACTIONS)
  (SETQ SEL*FINAL.ACTIONS NIL)
  (when SEL*INITIAL.FLAG
    (SETQ SEL*INITIAL.FLAG NIL)
    (THROW 'SEL-INITIALIZE SEL*ACTUAL.DEDUCTION.CODE))
  (CASE SEL*REDUCE.DEDUCE
    (DEDUCE (THROW 'SEL-UPDATE.REDUCE SEL*ACTUAL.REDUCTION.CODE))
    (REDUCE (THROW 'SEL-UPDATE.DEDUCE SEL*ACTUAL.DEDUCTION.CODE))
    (OTHERWISE (ERROR "illegal reduce.deduce in sel=return: ~a" SEL*REDUCE.DEDUCE))))

(DEFUN SEL=UPDATE NIL
						; edited: 7. 6. 1982  hjo
						; effect: updates the clause classe: supported
						;         and calls the operation-mark functions.
						; value:  undefined
  (MAPC #'(LAMBDA (CLAUSE)
	    (when (OR (MEMBER-IF #'(LAMBDA (PARENT) (MEMBER PARENT (SEL=CLAUSES SUPPORTED)))
				 (DS-CLAUSE.PARENTS CLAUSE))
		      (AND (DS-LINK.IS (CAR (CG-CLAUSE_CREATOR.UNIFIER CLAUSE)))
			   (DS-LINK.IS.MARKED ACTIVE (CAR (CG-CLAUSE_CREATOR.UNIFIER CLAUSE)))
			   (not (opt-is.completion))))
	      (SEL=INSERT.CLAUSE CLAUSE NIL SUPPORTED)))
	(CG-CLAUSES INSERTED))
  ;; Inserting all changed clauses because in deduce state only changed due to newly inserted clauses
  (MAPC #'(LAMBDA (CLAUSE)
	    (case SEL*STR_PARAMODULATION.STRATEGY
	      (bachmair-ganzinger (sel=mark.update.b.g clause))
	      (snyder-lynch (sel=mark.update.s.l clause))
	      (dershowitz (sel=mark.update.d clause))
	      (zhang-kapur (sel=mark.update.z.k clause))
	      ((clause-graph heuristic-completion) (sel=mark.update.h.c clause))
	      (otherwise (cerror "Do nothing" "Undefined paramodulation strategy: ~A" SEL*STR_PARAMODULATION.STRATEGY))))
	(cG-CLAUSES changed))
  (SEL=LINK.&.TERM.DEPTH.TREATMENT (CG-LINKS R INSERTED) (CG-LINKS P INSERTED))
  (MAPC #'(LAMBDA (OPERATION)
	    (funcall (SYMBOL-FUNCTION (INTERN (FORMAT NIL "~a.UPDATE" OPERATION) (find-package "MKRP")))))
	SEL*OPERATIONS))



(DEFUN SEL=LINK.&.TERM.DEPTH.TREATMENT (RLINKS PLINKS)
						; edited: 9. 6. 1982   hjo
						; input:  a list of rlinks and a list of plinks.
						; effect: a link will be marked inhibited if it's
						;         depth exceeds the boundary value or if
						;         the maximum term depth of its resolvent or
						;         paramodulant exceeds the boundary value.
						; value:  undefined.
  (PROG ((LINK.DEPTH (OPT-GET.OPTION STR_LINK.DEPTH))
	 (TERM.DEPTH (OPT-GET.OPTION STR_TERM.DEPTH)))
	(when LINK.DEPTH
	  (MAPC #'(LAMBDA (RLINK) (when (> (DS-LINK.DEPTH RLINK) LINK.DEPTH) (SEL=INHIBIT RLINK))) RLINKS)
	  (MAPC #'(LAMBDA (PLINK) (when (> (DS-LINK.DEPTH PLINK) LINK.DEPTH) (SEL=INHIBIT PLINK))) PLINKS))
	(when TERM.DEPTH
	  (MAPC #'(LAMBDA (LINK)
		    (let ((POSPAR (DS-LINK.POSPAR LINK))
			  (NEGPAR (DS-LINK.NEGPAR LINK))
			  (POSLITNO (DS-LINK.POSLITNO LINK))
			  (NEGLITNO (DS-LINK.NEGLITNO LINK))
			  (UNIFIER (CAR (DS-LINK.UNIFIERS LINK)))
			  (COLOUR (DS-LINK.COLOUR LINK))
			  (NOTINHIBITED T))
		      (DODOWN (RPTN (DS-CLAUSE.NOLIT POSPAR))
			(COND ((AND (NEQ (1+ RPTN) POSLITNO)
				    (> (DT-TERMLIST.MAXDEPTH
					 (UNI-APPLY.SUBSTITUTION UNIFIER (DS-CLAUSE.TERMLIST POSPAR (1+ RPTN)) T))
				       TERM.DEPTH))
			       (SEL=INHIBIT LINK)
			       (SETQ NOTINHIBITED NIL)
			       (SETQ RPTN 0))))
		      (COND (NOTINHIBITED (DODOWN (RPTN (DS-CLAUSE.NOLIT NEGPAR))
					    (COND
					      ((AND (NEQ (1+ RPTN) NEGLITNO)
						    (>
						      (DT-TERMLIST.MAXDEPTH
							(UNI-APPLY.SUBSTITUTION UNIFIER (DS-CLAUSE.TERMLIST NEGPAR (1+ RPTN)) T))
						      TERM.DEPTH))
					       (SEL=INHIBIT LINK) (SETQ NOTINHIBITED NIL) (SETQ RPTN 0))))))
		      (COND
			((AND NOTINHIBITED (EQL COLOUR 'P)
			      (> (DT-TERMLIST.MAXDEPTH
				   (DT-REPLACE.TERM.IN.TERMLIST
				     (UNI-APPLY.SUBSTITUTION UNIFIER
							     (DT-ACCESS (DT-TAF.OTHERSIDE (DS-LINK.POSFCT LINK))
									(DS-CLAUSE.TERMLIST POSPAR POSLITNO))
							     T)
				     (DS-LINK.NEGFCT LINK)
				     (COPY-TREE (DS-CLAUSE.TERMLIST NEGPAR NEGLITNO))))
				 TERM.DEPTH))
			 (SEL=INHIBIT LINK)))))
		(APPEND PLINKS RLINKS)))))

(DEFUN SEL=NO.REFUTATION.POSSIBLE? NIL
						; edited: 11-dec-83 16:23:34        ne
						; input:  the graph is in a consistent state.
						; effect: checks whether the current clause set is
						;         satisfiable if all term lists are ignored.
						;         if so, the refutation is aborted.
						; value:  undefined.
  (PROG (LITERALLISTS LITERALLIST LITERAL MODEL (FILE (OPT-GET.OPTION TR_TRACE.FILE)))
	(PROGN ;; CONVERT CLAUSES TO LITERAL LISTS WHERE LITERALS ARE POSITIVE OR NEGATIVE INTEGERS. *)
	       (SETQ LITERALLISTS
		     (MAPCAR #'(LAMBDA (CLAUSE)
				 (SETQ LITERALLIST NIL)
				 (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
				   (PROGN (SETQ LITERAL (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)))
					  (COND ((DT-PREDICATE.IS.EQUALITY LITERAL)
						 (SETQ LITERAL (CAR (DT-PREDICATE.EQUALITIES)))))
					  (COND ((DS-SIGN.IS.NEGATIVE (DS-CLAUSE.SIGN CLAUSE (1+ RPTN)))
						 (SETQ LITERAL (- LITERAL))))
					  (SETQ LITERALLIST (CONS LITERAL LITERALLIST))))
				 literallist)
			     (APPEND (CG-CLAUSES ALL) (COPY-LIST (DS-RULES))))))
	(PROGN ;; CHECK FOR PROPOSITIONAL SATISFIABILITY *)
	       (SETQ MODEL (SATISFIABLE LITERALLISTS NIL)))
	(COND ((NULL MODEL)
	       ;; NOT PROPOSITIONALLY SATISFIABLE *)
	       NIL)
	      (T (COND ((CONSP MODEL) (TERPRI FILE) (PRINC "current clause set satisfiable. model: " FILE)
			(MAPC #'(LAMBDA (LITERAL)
				  (COND ((MINUSP LITERAL) (SETQ LITERAL (- LITERAL)) (PRINC "-" FILE)) (T (PRINC "+" FILE)))
				  (PRINC (DT-PREDICATE.PNAME LITERAL) FILE) (PRINC "(...) " FILE))
			      MODEL)
			(TERPRI FILE))
		       (T (TERPRI FILE) (PRINC "current clause set valid." FILE) (TERPRI FILE)))
		 (SEL=MAKE.DEDUCTION.CODE 'COLLAPSED 'COLLAPSED) (SEL=RETURN)))))

(DEFUN SEL=REFUTATION.FOUND (RLINKS PLINKS)
						; edited: 21. 8. 1984
						; input:  a set of rlinks and a set of plinks
						; effect: if either an rlink or a plink generates
						;         the empty clause, the corresponding
						;         deduction code is formed and the selection
						;         module is left with sel=return
						; value:  undefined
  (unless (opt-is.with.residues)
    (let ((RLINK (CAR (MEMBER-IF #'(LAMBDA (LINK)
				     (AND (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.POSPAR LINK)))
					  (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.NEGPAR LINK)))))
				 RLINKS))))
      (COND (RLINK (COND (SEL*INITIAL.FLAG (SEL=MAKE.DEDUCTION.CODE RLINK 'FIRST))
			 (T (SETQ SEL*ACTUAL.DEDUCTION.CODE (LIST 'EMPTY RLINK (CAR (DS-LINK.UNIFIERS RLINK))))))
		   (SEL=RETURN))))
    (MAPC #'(LAMBDA (PLINK)
	      (let ((POSPAR (DS-LINK.POSPAR PLINK))
		    (NEGPAR (DS-LINK.NEGPAR PLINK)))
		(COND ((AND (EQL 1 (DS-CLAUSE.NOLIT POSPAR))
			    (EQL 1 (DS-CLAUSE.NOLIT NEGPAR)))
		       (let (OTHERCLAUSE CLAUSE UNIFIER TERM
			     (PREDICATE (DS-CLAUSE.PREDICATE NEGPAR 1))
			     (SIGN (DS-CLAUSE.SIGN NEGPAR 1))
			     (TAF (DS-LINK.POSFCT PLINK)))
			 (COND ((AND (DT-TAF.IS.LEFT TAF)
				     (MEMBER-IF #'(LAMBDA (LINK)
						    (SETQ CLAUSE (DS-LINK.NEGPAR LINK))
						    (AND (NEQ CLAUSE POSPAR)
							 (eql 1 (DS-CLAUSE.NOLIT CLAUSE))
							 (DT-TAF.IS.RIGHT (DS-LINK.POSFCT LINK))
							 (EQL PREDICATE (DS-CLAUSE.PREDICATE CLAUSE 1))
							 (EQL SIGN (DS-SIGN.OTHER.SIGN (DS-CLAUSE.SIGN CLAUSE 1)))
							 (SETQ OTHERCLAUSE CLAUSE)))
						(DS-CLAUSE.LINKS 'P POSPAR 1)))
				(SETQ TERM (SECOND (DS-CLAUSE.TERMLIST POSPAR 1))))
			       ((AND (DT-TAF.IS.RIGHT TAF)
				     (MEMBER-IF #'(LAMBDA (LINK)
						    (SETQ CLAUSE (DS-LINK.NEGPAR LINK))
						    (AND (NEQ CLAUSE POSPAR) (EQL 1 (DS-CLAUSE.NOLIT CLAUSE))
							 (DT-TAF.IS.LEFT (DS-LINK.POSFCT LINK))
							 (EQL PREDICATE (DS-CLAUSE.PREDICATE CLAUSE 1))
							 (EQL SIGN (DS-SIGN.OTHER.SIGN (DS-CLAUSE.SIGN CLAUSE 1)))
							 (SETQ OTHERCLAUSE CLAUSE)))
						(DS-CLAUSE.LINKS 'P POSPAR 1)))
				(SETQ TERM (CAR (DS-CLAUSE.TERMLIST POSPAR 1)))))
			 (COND ((AND OTHERCLAUSE
				     (MEMBER-IF #'(LAMBDA (UNIF)
						    (COND ((UNI-UNIFY.TERMLISTS
							     (DS-CLAUSE.TERMLIST OTHERCLAUSE 1)
							     (DT-REPLACE.TERM.IN.TERMLIST
							       (UNI-APPLY.SUBSTITUTION UNIF TERM T)
							       (DS-LINK.NEGFCT PLINK)
							       (UNI-APPLY.SUBSTITUTION UNIF (DS-CLAUSE.TERMLIST NEGPAR 1) T)))
							   (SETQ UNIFIER UNIF) T)))
						(DS-LINK.UNIFIERS PLINK)))
				(COND (SEL*INITIAL.FLAG (SEL=MAKE.DEDUCTION.CODE PLINK UNIFIER))
				      (T (SETQ SEL*ACTUAL.DEDUCTION.CODE (LIST 'EMPTY PLINK UNIFIER))))
				(SEL=RETURN))))))))
	  PLINKS)))


(DEFUN SEL-SAVE (FILE)
						; edited: 19-jul-82 09:55:28
						; input:  a filename. file is expected to be open and
						;         remains so.
						; effect: all global variables are saved onto file.
						; value:  undefined.
  (MAPC #'(LAMBDA (ATOM)
	    (PROGN (PRINC (LIST 'SETQ ATOM `',(EVAL ATOM)) FILE) (TERPRI FILE)))
	'(SEL*ACTUAL.CLAUSES
	   SEL*STEPCOUNTER
	   SEL*STEPS
	   SEL*TREE.POINTER.1
	   SEL*TREE.POINTER.2
	   SEL*IND.CASE.COUNTER
	   SEL*IND.CHAIN
	   SEL*IND.ACTUAL.ENTRY
	   SEL*IND.SINGLE.LINK
	   SEL*END.OF.INDUCTION
	   SEL*IND.OLD.CLAUSES SEL*IND.NEW.CLAUSES SEL*IND.OLD.LINKS SEL*IND.OPTIONS
	   SEL*STR_RESOLUTION.STRATEGY
	   SEL*STR_PARAMODULATION.STRATEGY
	   SEL*STR_LINEAR.TOP.CLAUSE
	   SEL*TERMINATOR.PROVED.FLAG SEL*TERMINATOR.LOOK
	   SEL*CHAIN.DELETE.INTERMEDIATE.RESULTS
	   SEL*CHAIN.UNITS           
	   SEL*ACTUAL.DEDUCTION.CODE
	   SEL*ACTUAL.REDUCTION.CODE
	   SEL*DUMMY
	   SEL*OPERATION.STACK
	   SEL*ACTUAL.OPERATION
	   SEL*REDUCE.DEDUCE
	   SEL*INITIAL.FLAG
	   SEL*FINAL.ACTIoNS
	   SEL*OPERATIONS
	   SEL*LINK.CLASSES
	   SEL*CLAUSE.CLASSES
	   SEL*CLAUSELISTS
	   SEL*LINKLISTS
	   SEL*TERMINATOR.NEW.CLAUSES
	   SEL*TERMINATOR.LOOK.NEW.CLAUSES
	   SEL*REDUCTION.FLAG))
  (mapc #'(lambda (op) (format file "(setf (symbol-plist '~A) nil)~%" op)) sel*operations)
  (MAPC #'(LAMBDA (ATOM)
	    (format file "(SETQ ~A '~A) ~A~%" atom (EVAL ATOM) (SAVE-PROPLIST ATOM '(SEL))))
	(CONS 'SEL=CHAIN SEL*OPERATIONS))
  (COND
    (SEL*CHAIN (PROGN (PRINC (LIST 'SETQ 'SEL*CHAIN (COPY-GRAPH SEL*CHAIN)) FILE) (TERPRI FILE))
	       (PROGN
		 (PRINC `(SETQ SEL*CHAIN.TAIL (NTHCDR (1- ,(LISTPOS (CAR SEL*CHAIN.TAIL) SEL*CHAIN)) SEL*CHAIN))
			FILE)
		 (TERPRI FILE))
	       (PROG ((POS (LISTPOS (CAR SEL*CHAIN.LINKS) (CADDAR SEL*CHAIN.TAIL))))
		     (COND
		       (POS
			(PROGN (PRINC (LIST 'SETQ 'SEL*CHAIN.LINKS (LIST 'FNTH '(CADDAR SEL*CHAIN.TAIL) POS)) FILE)
			       (TERPRI FILE)))
		       (T (PROGN (PriNC '(SETQ SEL*CHAIN.LINKS NIL) FILE) (TERPRI FILE)))))))
  (MAPC #'(LAMBDA (LIST) (PROGN (PRINC (LIST 'SETQ LIST `',(EVAL LIST)) FILE) (TERPRI FILE)))
	(APPEND SEL*LINKLISTS (COPY-LIST SEL*CLAUSELISTS)))
  (MAPC #'(LAMBDA (CLASS)
	    (SETQ CLASS (INTERN (APPLY #'CONCATENATE 'STRING "SEL*CLAUSES_" (MAPCAR #'STRING CLASS))
				(find-package "MKRP")))
	    (PROGN
	      (PRINC
		(LIST 'SETQ CLASS
		      (COND ((CAR (SYMBOL-VALUE CLASS)) (LIST 'QCONC NIL (KWOTE (CAR (SYMBOL-VALUE CLASS))))) (T '(LIST NIL))))
		FILE)
	      (TERPRI FILE)))
	SEL*CLAUSE.CLASSES)
  (MAPC #'(LAMBDA (CLASS)
	    (let ((CLASS1 (INTERN (APPLY #'CONCATENATE 'STRING
					 "SEL*LINKS_"
					 (MAPCAR #'SYMBOL-NAME (DELETE NIL (CONS (CDR CLASS) (CAR CLASS)))))
				  (find-package "MKRP")))
		  (CLASS2 (INTERN (APPLY #'CONCATENATE 'STRING
					 "SEL*LINKS.COLOUR.FLAG_"
					 (MAPCAR #'SYMBOL-NAME (CAR CLASS)))
				  (find-package "MKRP"))))
	      (PROGN (PRINC (LIST 'SETQ CLASS1
				  (COND ((CAR (SYMBOL-VALUE CLASS1)) (LIST 'QCONC NIL (KWOTE (CAR (SYMBOL-VALUE CLASS1)))))
					(T '(LIST NIL))))
			    FILE)
		     (TERPRI FILE))
	      (COND ((NEQ (SYMBOL-VALUE CLASS2) 'NIL)
		     (PROGN (PRINC (LIST 'SETQ CLASS2 `',(SYMBOL-VALUE CLASS2)) FILE) (TERPRI FILE))))))
	SEL*LINK.CLASSES))



