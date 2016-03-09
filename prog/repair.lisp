;;; -*- Package: mkrp; Syntax: common-lisp; Mode: lisp -*-

(defun sel=strat_basic.value (link)
  (sel=strat_p.selection (if (eq (ds-link.colour link) 'r)
			     (sel=strat_value link #'op-resolve )
			     (sel=strat_value link #'op-paramodulate))
			 (DS-LINK.NOLIT LINK)
			 1
			 (ds-link.depth link)
			 (intersection (sel=clauses supported) (list (ds-link.pospar link) (ds-link.negpar link)))
			 (ds-clause.only.equations (ds-link.result link))))

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

(defun sel-save.reset ()
  (mapc #'(lambda (fun) (dt-remprop fun 'sel*weight))
	(append (dt-constant.all) (dt-predicate.all) (dt-function.all))))

(DEFUN RED=CML_TO.DETECTION.LINK (CLAUSE LINK)
						; Authors: MKRP
						; edited:  13-MAR-1992 19:32
						; input:   'link' is a si- or sid-link incident with
						;          clause 'clause'.
						; effect:  if link 'link' has the identical
						;          substitution it indicates double literals.
						;          its positive parentliteral will be removed.
						;          information is given to protocol and
						;          connectiongraph module. agenda is updated.
						; value:   not nil iff a literal has been removed.
  
  (when (MEMBER NIL (DS-LINK.UNIFIERS LINK))
    (let* ((POSLITNO (DS-LINK.POSLITNO LINK))
	   (NEGLITNO (DS-LINK.NEGLITNO LINK))
	   (pos.r (length (ds-clause.links 'r clause poslitno)))
	   (neg.r (length (ds-clause.links 'r clause neglitno)))
	   remove.litno remain.litno)
      (if (or (> neg.r pos.r) (ds-link.rule link))
	  (setq remove.litno poslitno remain.litno neglitno)
	  (setq remove.litno neglitno remain.litno poslitno))
      (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.LITERAL CLAUSE remove.LITNO remain.LITNO 'DOUBLE.LITERAL NIL) NIL NIL)
      (CG-CHANGE.QUEUE_APPEND (LIST 'DOUBLE.LITERAL (CONS (CONS CLAUSE remain.LITNO) (CONS CLAUSE remove.LITNO))))
      (PR-OPERATION 'DOUBLE.LITERAL CLAUSE remain.LITNO remove.LITNO (DS-LINK.RULE LINK))
      t)))

(defun SEL=STR.INIT.MARK.RLINKS.sos (rlinks supported.clauses)
						; Edited:  31-OCT-1991 21:25
						; Authors: PRCKLN
						; Input:   A list of rlinks
						; Effect:  activates and passivates the rlinks
						;          according to SOS strategy.
						; Value:   Undefined
  (case (opt-get.option er_paramodulation)
    (zhang-kapur (MAPC #'(LAMBDA (LINK)
			   (COND ((or (and (ds-clause.lit.is.max (DS-LINK.NEGPAR LINK) (DS-LINK.NEGlitno LINK))
					   (ds-clause.lit.is.max (DS-LINK.POSPAR LINK) (DS-LINK.POSlitno LINK)))
						;(= 1 (ds-link.nolit link))
				      )
				  (SEL=INSERT.LINK LINK NIL ACTIVE)
				  (DS-LINK.MARK ACTIVE LINK))
				 (T (SEL=PASSIVATE LINK))))
		       RLINKS))
    ((clause-graph bachmair-ganzinger snyder-lynch) (MAPC #'SEL=STR.MARK.rLINK.b.g RLINKS))
    ((heuristic-completion) (MAPC #'(LAMBDA (LINK)
				      (COND ((OR (MEMBER (DS-LINK.NEGPAR LINK) SUPPORTED.CLAUSES)
						 (MEMBER (DS-LINK.POSPAR LINK) SUPPORTED.CLAUSES)
						 (AND (CONSP (DS-LINK.RULE LINK))
						      (MEMBER-IF #'(LAMBDA (CLAUSE) (EQ 'THEOREM (DS-CLAUSE.PARENTS CLAUSE)))
								 (DS-CLAUSE.ANCESTORS (CAR (DS-LINK.RULE LINK))))))
					     (SEL=INSERT.LINK LINK NIL ACTIVE)
					     (DS-LINK.MARK ACTIVE LINK))
					    (T (SEL=PASSIVATE LINK))))
				  RLINKS))))

(defun SEL=STR.MARK.RLINKS.sos (rlinks)
						; Edited:  31-OCT-1991 21:56
						; Authors: PRCKLN
						; Input:   A list of rlinks
						; Effect:  activates and passivates the rlinks
						;          according to SOS
						;          strategy.
						; Value:   Undefined
  (MAPC #'(LAMBDA (RLINK)
	    (case (opt-get.option er_paramodulation)
	      (zhang-kapur (COND ((or (and (ds-clause.lit.is.max (ds-link.pospar rlink) (ds-link.poslitno rlink))
					   (ds-clause.lit.is.max (ds-link.negpar rlink) (ds-link.neglitno rlink)))
						;(= 1 (ds-link.nolit rlink))
				      )
				  (SEL=INSERT.LINK RLINK NIL ACTIVE)
				  (DS-LINK.MARK ACTIVE RLINK))
				 (T (SEL=PASSIVATE RLINK))))
	      ((clause-graph snyder-lynch bachmair-ganzinger) (SEL=STR.MARK.rLINK.b.g rlink))
	      ((heuristic-completion) (COND ((OR (MEMBER (DS-LINK.NEGPAR RLINK) (SEL=CLAUSES SUPPORTED))
						 (MEMBER (DS-LINK.POSPAR RLINK) (SEL=CLAUSES SUPPORTED)))
					     (SEL=INSERT.LINK RLINK NIL ACTIVE)
					     (DS-LINK.MARK ACTIVE RLINK))
					    (T (SEL=PASSIVATE RLINK))))))
	RLINKS))

(DEFUN OP=FACTORIZE (LINK UNIFIER &optional PREDICATE.UPDATE.FLAG complexity list.result)
  (declare (ignore PREDICATE.UPDATE.FLAG))
						; EDITED:27-APR-82 17:35:41")
						;         25. 4. 1983  SB
						; INPUT:  LINK IS A FLINK OF THE ACTUAL GRAPH
						;         UNIFIER IS A UNIFIER OF LINK.
						;         AND A FLAG
						; EFFECT: THE FACTOR OF FLINK IS CREATED AND ADDED TO
						;         THE GRAPH. THE PREDICATE OCCURENCE LISTS ARE
						;         UPDATED IF THE FLAG IS T
						; VALUE:  NAME OF THE FACTOR (OR NIL).
  (let* ((CLAUSE (DS-LINK.POSPAR LINK))
	 (ORIGINS (LIST NIL)) 
	 (PNAME (DS-CLAUSE.PNAME CLAUSE))
	 (DEPTH (DS-CLAUSE.DEPTH CLAUSE))
	 (LITLIST (OP=TRANSMIT.LITERALS CLAUSE (DS-LINK.negLITNO LINK) UNIFIER NIL ORIGINS nil))
	 (REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION (DS-CLAUSE.VARIABLES CLAUSE))))
    (if complexity
	(conses (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
	(if list.result
	    (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)
	    (let* ((pname (OP=fac_NEWNAME pname))
		   (new.litlist (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
		   (factor (DS-CLAUSE.CREATE pname (LIST CLAUSE) DEPTH new.litlist))
		   (pars (list factor)))      
	      (CG-INSERT.CLAUSE FACTOR (CONS LINK UNIFIER) (CAR ORIGINS) 'FACTORIZATION LINK)
	      (setq origins nil)
	      (red-rewrite factor)
	      (ds-rewrite.update clause)
	      (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
		     (OP=POT.T.AND.F.LITNOS (list factor))
		     (cons-construct.links (list factor) nil))
		    ((NOT (ZEROP (DS-CLAUSE.NOLIT FACTOR)))
		     (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS FACTOR)
		     (OP=INHERIT.LINKS FACTOR LINK UNIFIER REN.SUBST REN.SUBST REN.SUBST)
		     (OP=create_P.PD.AND.PIW.LINKS factor unifier nil)
		     (OP=CONNECT.TO.PARENTS factor)))
	      (PR-OPERATION 'FACTORIZATION LINK UNIFIER FACTOR) (SETQ LITLIST (DS-CLAUSE.VARIABLES FACTOR))
	      (SMAPC #'(LAMBDA (VAR) (COND ((NOT (MEMBER VAR (DS-CLAUSE.VARIABLES FACTOR))) (DT-VARIABLE.DELETE VAR))))
		     #'CDDR (CDR REN.SUBST))
	      pars)))))

(defun red=ls_p.link.new (clause)
  (mapc #'(lambda (link)
	    (when (and (ds-link.is link)
		       (if (opt-is.kz.completion) (not (member link (cg-links 'r removed))))
		       (not (member link (cg-links 'p removed)))
		       (not (member link (cg-links 'piw removed))))
	      (let* ((TERMLIST (ds-link.result link)))
		(when (and termlist (or (some #'(lambda (literal)
						  (and (dt-predicate.is.equality (ds-lit.predicate literal))
						       (ds-sign.is.positive (ds-lit.sign literal))
						       (uni-equal (first (ds-lit.termlist literal))
								  (second (ds-lit.termlist literal)))))
					      termlist)
					(op-subsumed.p (dt-termlist.variables termlist) termlist)
					(red=ls_all.instances.true termlist)))
		  (red=ctl_remove.link link "Subsumed" nil)))))
	(prog1 (delete-duplicates red*info_links.changed) (setq red*info_links.changed nil)))
  (mapc #'(lambda (links)
	    (mapc #'(lambda (link)
		      (let* ((TERMLIST (ds-link.result link)))
			(when (and termlist
				   (or (op-subsumed.p (dt-termlist.variables termlist) termlist clause)
				       (red=ls_all.instances.true termlist)))
			  (red=ctl_remove.link link :Subsumed.p clause))))
		  links))
	(list (cg-links 'p all)
	      (if (or (opt-is.kz.completion) (opt-is.with.residues)) (cg-links 'r all) nil)
	      (cg-links 'piw all))))

(defun RED=LS_SUBSUME.P.LINK (LINK)
						; Edited:  18-AUG-1989 13:49
						; Authors: PRCKLN
						; Input:   LINK is an external operation link
						; Effect:  If LINK is a P-Link whose parents are
						;          units then if a unifier of link LINK is subsumed by
						;          an arbitrary clause, it will be removed.
						; Value:   Undefined.
  (let ((termlist (ds-link.result link)))
    (when (or (some #'(lambda (lit)
			(and (dt-predicate.is.equality (ds-lit.predicate lit))
			     (ds-sign.is.positive (ds-lit.sign lit))
			     (uni-equal (first (ds-lit.termlist lit)) (second (ds-lit.termlist lit)))))
		    termlist)
	      (op-subsumed.p (dt-termlist.variables termlist) termlist)
	      (red=ls_all.instances.true termlist))
      (red=ctl_remove.link link "Subsumed" nil))))


(defun red=ls_all.instances.true (litlist)
  (let ((domain (ds-finite.domain.domain)))
    (when domain
      (let ((variables (ds-lits.vars litlist))
	    (is.true t))
	(when variables
	  (CARTESIAN.LOOP (make-list (length variables) :initial-element (copy-list domain))
			  #'(lambda (lists)
			      (let* ((inst.uni (zip variables (mapcar #'first lists) t))
				     (instance (UNI-APPLY.SUBSTITUTION inst.uni LITLIST T)))
				(declare (special CARTESIAN.LOOP.END))
				(unless (or (some #'(lambda (lit)
						      (and (dt-predicate.is.equality (ds-lit.predicate lit))
							   (ds-sign.is.positive (ds-lit.sign lit))
							   (uni-equal (first (ds-lit.termlist lit))
								      (second (ds-lit.termlist lit)))))
						  instance)
					    (op-subsumed.p nil instance))
				  (setq cartesian.loop.end t is.true nil)))))
	  is.true)))))

(defun uni-equal (t1 t2)
  (if (dt-function.theories) (member nil (uni-unify.terms t1 t2)) (equal t1 t2)))


(DEFUN UNI-RESET NIL
						; input:  nil
						; value:  undefined
						; effect: creates two new buffers, the values for
						;         uni*buffer.stack and uni*constantify.buffer.
  (when (opt-get.option sort_literals)
    (upp-init)
    (upp-epsilon.literals.insert (if (dt-predicate.is (dt-predicate.element))
				     (DT-PREDICATE.POSITIVE.OCCURRENCES (dt-predicate.element))
				     nil)))
  (denz-init)
  (SETQ UNI*BUFFER.STACK (BUFFER.CREATE 0))
  (setq UNI*VARIABLES.REGARDED.AS.CONSTANTS nil)
  (SETQ UNI*CONSTANTIFY.BUFFER (BUFFER.CREATE 10)))

(DEFUN OP-create.instance (UNIFIER clause &optional list.result)
						; Edited:  19-AUG-1990 15:17
						; Authors: PRCKLN
						; INPUT:  UNIFIER IS A UNIFIER OF LINK.
						;         AND A clause
						; EFFECT: THE FACTOR OF FLINK IS CREATED AND ADDED TO
						;         THE GRAPH. THE PREDICATE OCCURENCE LISTS ARE
						;         UPDATED IF THE FLAG IS T
						; VALUE:  NAME OF THE FACTOR (OR NIL).
  (let* ((ORIGINS (LIST NIL)) 
	 (PNAME (DS-CLAUSE.PNAME CLAUSE))
	 (DEPTH (DS-CLAUSE.DEPTH CLAUSE))
	 (REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION (DS-CLAUSE.VARIABLES CLAUSE)))
	 (LITLIST (OP=TRANSMIT.LITERALS CLAUSE nil UNIFIER NIL ORIGINS nil)))
    (if list.result
	(UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)
	(let* ((pname (OP=inst_NEWNAME pname))
	       (new.litlist (UNI-APPLY.SUBSTITUTION unifier (UNI-APPLY.SUBSTITUTION ren.subst LITLIST T)))
	       (instance (DS-CLAUSE.CREATE pname (LIST CLAUSE) DEPTH new.litlist))
	       (pars (list instance)))      
	  (CG-INSERT.CLAUSE INSTANCE UNIFIER (CAR ORIGINS) "Instantiation" nil)
	  (setq origins nil)
	  (red-rewrite clause)
	  (ds-rewrite.update clause)
	  (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
		 (OP=POT.T.AND.F.LITNOS (list instance))
		 (cons-construct.links (list instance) nil))
		((NOT (ZEROP (DS-CLAUSE.NOLIT INSTANCE)))
		 (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS INSTANCE)
		 (OP=INHERIT.LINKS INSTANCE nil UNIFIER ren.subst ren.subst ren.subst)
		 (OP=create_P.PD.AND.PIW.LINKS instance unifier nil)
		 (OP=CONNECT.TO.PARENTS instance)))
	  (PR-OPERATION 'instantiate UNIFIER clause instance)
	  pars))))


(defun denz-init ()
  (setq e-compl*ac-functions (denz=trans.to.joerg
			       (delete nil (mapcar #'(lambda (func) (if (DT-FUNCTION.IS.MARKED ac func) func nil))
						   (dt-function.all))))))
(defun denz=trans.from.joerg (object)
  (let ((hd.object
	  (cond ((consp object)
		 (if (cdr object)
		     (dt-term_create (denz=trans.from.joerg (first object)) (mapcar #'denz=trans.from.joerg (rest object)))
		     (denz=trans.from.joerg (first object))))
		((get object 'mkrp*object))
		(t (error "No new creation of consts, vars, and funs allowed.")))))
    hd.object))

(defun denz=trans.subst.from.joerg (subst)
  (cond ((eq subst t) (list nil))
	(subst (list (mapcan #'(lambda (comp)
				 (list (denz=trans.from.joerg (first comp))
				       (denz=trans.from.joerg (second comp))))
			     subst)))
	(t nil)))

(defun denz=trans.to.joerg (object)
  (if (listp object)
      (mapcar #'denz=trans.to.joerg object)
      (or (let ((res (dt-getprop object 'mkrp*object)))
	    (when res
	      (if (symbolp res)
		  (unless (get res 'mkrp*object)
		    (setf (get res 'mkrp*object) object))
		  (unless (get (first res) 'mkrp*object)
		    (setf (get (first res) 'mkrp*object) object))))
	    res)		
	  (let ((joerg.object
		  (cond ((dt-function.is object)
			 (let ((new (mkrp-intern (dt-pname object))))
			   (dt-putprop object 'mkrp*object new)
			   (setf (get new 'mkrp*object) object)
			   new))
			((dt-constant.is object) 
			 (let ((new (mkrp-intern (dt-pname object))))
			   (dt-putprop object 'mkrp*object (list new))
			   (setf (get new 'mkrp*object) object)
			   (list new)))
			((dt-variable.is object)
			 (let ((new (mkrp-intern (dt-pname object))))
			   (dt-putprop object 'mkrp*object new)
			   (setf (get new 'mkrp*object) object)
			   new))
			(t
			 (let ((fun (denz=trans.to.joerg (dt-term_topsymbol object)))
			       (args (mapcar #'denz=trans.to.joerg (dt-term_arguments object))))
			   (cons fun args))))))
	    joerg.object))))

(defvar uni*th.term.top (dt-function.create "AUX" 'any '(any any)))

(defun denz=term.create (terml) (dt-term_create (if (dt-function.is uni*th.term.top)
						    uni*th.term.top
						    (setq uni*th.term.top (dt-function.create "AUX" 'any '(any any))))
						terml))

(defun denz=trans.remove.l (tl)
  (declare (ignore tl))
  nil)

(defun denz-match (term1 term2)
  (unwind-protect (denz=trans.subst.from.joerg (ac-match (denz=trans.to.joerg term1) (denz=trans.to.joerg term2)))
    (denz=trans.remove.l (list term1 term2))))

(defun denz-match.list (terml1 terml2)
  (unwind-protect (denz=trans.subst.from.joerg (ac-match (denz=trans.to.joerg (denz=term.create terml1))
							 (denz=trans.to.joerg (denz=term.create terml2))))
    (denz=trans.remove.l (append terml1 terml2))))