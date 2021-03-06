;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-

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




(DEFVAR RED*RW_RULES NIL)
(defvar red*rw_taf)
(defvar red*rw_rules.completion nil)
(defvar red*rw_rules.unfailing nil)


(DEFUN RED=APPLY_CLAUSE.REWRITING.TO.CLAUSE (CLAUSE)
						; edited: 22-sep-83 16:48:58
						; input:  a clause.
						; effect: clause 'clause' will be rewritten
						;         by the elements of red*rw_rules.
						;         furthermore, 'clause' is tested whether
						;         it is a rewrite rule or not.
						;         if this check is positive then:
						;         all clauses of 'red*rw_clauses' are checked
						;         wether they can be modified by the new
						;         rewrite rule 'clause'.
						;         the changed clauses are inserted into
						;         reduction agenda and information is given
						;         to the protocol module.
						;         the links of the changed literals are
						;         reconstructed.
						;         else:
						;         'clause' is inserted into 'red*rw_clauses'.
						;         'clause' is changed by rewrite rules
						;         'red*rw_rules'.
						; value: nil.
  (declare (special red*forward red*state))
  (let (CHANGED.CLAUSES RW_RULES NEW.CHANGES)
						; invariant:
						;         'changed.clauses' is a list of the form:
						;              (c1 l11 ... l1n1)
						;               :   :        :
						;               :   :        :
						;              (cm lm1 ... lmnm))
						;         c1 .. cm are the clauses rewritten during
						;         this call of the function and
						;         lj1 .. ljnj are the modified literals
						;         of cj  cj = 1...me.
    (when (member (RDS-RULE 'CONDITION 'RED*CLAUSE.rewriting) '(t def))
      (SETQ RED*RW_RULES (DELETE-IF-NOT #'(LAMBDA (CLAUSE) (DS-CLAUSE.IS CLAUSE)) RED*RW_RULES))
      (unless (member clause red*rw_rules)
	(MAPC #'(LAMBDA (RULE)
		  (when (CDR (SETQ NEW.CHANGES (RED=RW_REPLACE CLAUSE RULE)))	; If cdr then the literals in cdr are changed
		    (SETQ CHANGED.CLAUSES (INSASSOC NEW.CHANGES CHANGED.CLAUSES))))
	      RED*RW_RULES))
      (when (and (or (eq red*state 'initial) (not red*forward)) (RED=RW_REWRITE.CLAUSE CLAUSE))
						; definition for a rewrite rule:
						; a rw_rule must satisfy the following conditions :
						; - rw_rule must be a unit clause

						;   equation.
						; - the first or second term of literal must be a
						;   rewrite-constant or a rewrite-function.
						; the conditions for a rewrite-constant-rule are:
						; - o.b.d.a the first term of literal must be a
						;   constant 'c'.
						; - 'c' is not in the second term of literal
						; - sort ('c') >= sort (second.term)
						; - the second term is a ground term
						; the conditions for a rewrite-function-rule are:
						; - o.b.d.a the first term of literal must be a
						;   function f(x1 ,... ,xn) with n >= 1
						; - the functionsymbol 'f' is not in the second term
						; - the arguments of 'f' are different in pair
						; - sort ('f') >= sort (second.term)
						; - 'f' is not associative
						; - sort (xj) = domainsort ('f') with j = 1..n
						; - there are no variables in the second term, which
						;   are not in the set of arguments of 'f'.
	(SETQ RW_RULES (CONS CLAUSE RW_RULES))
	(SETQ RED*RW_RULES (NCONC1 RED*RW_RULES CLAUSE))
	(setq red*rw_rules.completion (delete clause red*rw_rules.completion :key #'first))
	(MAPC #'(LAMBDA (RW_CLAUSE)
		  (when (CDR (SETQ NEW.CHANGES (RED=RW_REPLACE RW_CLAUSE CLAUSE)))
		    (SETQ CHANGED.CLAUSES (INSASSOC NEW.CHANGES CHANGED.CLAUSES))))
	      (SET-DIFFERENCE (CG-CLAUSES ALL) RED*RW_RULES))
	(mapc #'(lambda (plink)
		  (let ((left (first (ds-clause.termlist clause 1)))
			(right (second (ds-clause.termlist clause 1)))
			(termlist (ds-link.result plink)))
		    (if (DT-CONSTANT.IS LEFT)
			(nsubst right left termlist)
			(mapc #'(lambda (lit) (RED=RW_FIND.MATCHES.IN.TERMLIST (ds-lit.termlist lit) left right))
			      termlist))))
	      (cg-links '(piw p) all)))
      (red=ctl_RECONSTRUCT RW_RULES CHANGED.CLAUSES))
    (when (member (RDS-RULE 'CONDITION 'RED*CLAUSE.rewriting) '(t dem))
      (red=rw_completion clause))
    nil))

(defun red=rw_add.completion.rule (clause litno)
						; Edited:  26-JUL-1990 23:06
						; Authors: PRCKLN
						; Input:   A clause and a literal number.
						; Effect:  Inserts a new rule for this literal into the properties of
						;          itself.
						; Value:   Undefined.
  (unless (dt-getprop clause 'cons*a)
    (let ((rule (or (ds-clause.lit.getprop clause litno 'red*rule)
		    (let* ((tl (ds-clause.termlist clause litno))
			   (taf (ds-clause.lit.rewrite.rule clause litno)))
		      (if (and taf (= 1 (ds-clause.nolit clause)))
			  (rds-rw_rule.create
			    :clause clause
			    :left (dt-access (list taf) tl)
			    :right (dt-access (dt-taf.otherside (list taf)) tl))
			  nil)))))
      (ds-clause.lit.putprop clause litno 'red*rule rule))))

(defun red=rw_reduce.cc (clause litno)
  (when (and (eq (opt-get.option er_completion) 'constant-congruence)
	     (ds-clause.lit.is.unfailing clause litno))
    (mapc #'(lambda (variable) (dt-putprop variable 'red*cc t))
	  (ds-clause.lit.variables clause litno))
    (WHILE (SOME #'(LAMBDA (RULES FLAG)
		     (SOME #'(LAMBDA (RULE)
			       (UNLESS (EQL CLAUSE (FIRST RULE))
				 (WHEN (RED=RW_APPLY.TO.TERMLIST
					 RULE (DS-CLAUSE.TERMLIST CLAUSE LITNO) 
					 (DS-CLAUSE.TERMLIST CLAUSE LITNO)
					 FLAG nil nil )
				   (cg-REPLACE.LITERAL CLAUSE LITNO (DS-CLAUSE.SIGN CLAUSE LITNO)
						       (DS-CLAUSE.PREDICATE CLAUSE LITNO)
						       (DS-CLAUSE.TERMLIST CLAUSE LITNO)
						       'REWRITE (FIRST RULE))
				   (PR-OPERATION 'REWRITE (FIRST RULE) CLAUSE LITNO red*rw_taf)
				   T)))
			   RULES))
		 (list (red-info_p.link.rules t)
		       (red-info_p.link.unfail.rules t))
		 (LIST NIL T)))
    (mapc #'(lambda (variable) (dt-putprop variable 'red*cc nil))
	  (ds-clause.lit.variables clause litno))))

(defun red=rw_add.completion.unfail (clause litno)
						; Edited:  26-JUL-1990 23:09
						; Authors: PRCKLN
						; Input:   A clause and a literal number.
						; Effect:  Inserts a new unfail rule for this literal into the properties of
						;          itself.
						; Value:   Undefined.
  (unless (dt-getprop clause 'cons*a)
    (let ((rule (or (ds-clause.lit.getprop clause litno 'red*unfail)
		    (if (and (= 1 (ds-clause.nolit clause))
			     (ds-clause.lit.is.unfailing clause 1))
			(let ((tl (ds-clause.termlist clause 1)))
			  (if (equal (first tl) (second tl))
			      nil
			      (if (and (consp (first tl))
				       (consp (second tl))
				       (eql (first (first tl)) (first (second tl)))
				       (= (length (first tl)) 3)
				       (equal (rest (first tl)) (reverse (rest (second tl)))))
				  (list (rds-rw_rule.create :clause clause :left (first tl) :right (second tl)))
				  (list (rds-rw_rule.create :clause clause :left (first tl) :right (second tl))
					(rds-rw_rule.create :clause clause :left (second tl) :right (first tl))))))
			nil))))
      (ds-clause.lit.putprop clause litno 'red*unfail rule))))

(defun red=rw_add.completion.tree (clause)
  (ds-clause.do #'(lambda (litno)
		      (cond ((ds-clause.lit.rewrite.rule clause litno)
			     (red=rw_add.completion.rule clause litno)
			     (red=rw_c.insert (ds-clause.lit.getprop clause litno 'red*rule)))
			    ((ds-clause.lit.is.unfailing clause litno)
			     (red=rw_add.completion.unfail clause litno)
			     (setq red*rw_rules.unfailing
				   (adjoin (cons clause litno) red*rw_rules.unfailing :test #'equal)))))
		  clause))

(defun red=rw_add.completion.single (clause)
  (unless (member clause red*rw_rules)
    (when (= 1 (ds-clause.nolit clause)) ; Preliminary
      (ds-clause.do #'(lambda (litno)
			(cond ((ds-clause.lit.rewrite.rule clause litno)
			       (red=rw_add.completion.rule clause litno)
			       (setq red*rw_rules.completion
				     (adjoin (cons clause litno) red*rw_rules.completion :test #'equal)))
			      ((ds-clause.lit.is.unfailing clause litno)
			       (red=rw_add.completion.unfail clause litno)
			       (setq red*rw_rules.unfailing
				     (adjoin (cons clause litno) red*rw_rules.unfailing :test #'equal)))))
		    clause))))

(defun red=rw_add.completion (clause)
  (red=rw_add.completion.single clause))


(defun red=rw_apply.to.termlist.internal (rule left right rest.tl unfailing Not.reduce.term depth link origterm)
					; Edited:  19-NOV-1991 20:49
					; Authors: PRCKLN
					; Input:   RULE is an RW_RULE object, LEFT and RIGHT are its terms.
					;          TERMLIST is a list of terms.
					;          UNFAILING should be true iff RULE is not directable
					;          LINK is a link or NIL.
					;          NOT.REDUCE.TERM is NIL or one of the terms in TERMLIST. 
					; Effect:  Recursively tries to apply RULE to all subterms and terms
					;          in TERMLIST, if once successful, it terminates, NOT.REDUCE.TERM is
					;          not reduced.
					;          Updates protocol info of LINK if given.
					; Value:   True iff TERMLIST is changed
  (if rest.tl
      (or (if (or (eq Not.reduce.term (first rest.tl)) (dt-variable.is (first rest.tl)))
	      nil
	    (if (dt-getprop (rds-rw_rule.clause rule) 'cons*ac1)
		(if (and (consp (first rest.tl))
			 (member (dt-getprop (dt-getprop (rds-rw_rule.clause rule) 'cons*ac1) 'dt*null)
				 (first rest.tl)))
		    (setf (first rest.tl)
			  (second (remove (dt-getprop (dt-getprop (rds-rw_rule.clause rule) 'cons*ac1) 'dt*null)
					  (first rest.tl) :count 1)))
		  (if (consp (first rest.tl))
		      (red=rw_apply.to.termlist rule (rest (first rest.tl)) origterm unfailing  nil
						(if (null depth) nil (1- depth)) link)
		    nil))
	      (let* ((matcher (uni-unify1.terms left (first rest.tl) t)))
		(if matcher
		    (let ((new.term (uni-apply.substitution (first matcher) right t)))
		      (if (or (not unfailing)
			      (and (member (opt-get.option er_completion) '(unfailing constant-congruence))
				   (ord-greater (uni-apply.substitution (first matcher) left t)
						new.term)))
			  (progn (setq red*rw_taf (list (dt-taf.create (first rest.tl) origterm)
							(first matcher)
							left))
				 (rplaca rest.tl new.term))  ; Destructive replacement of term
			(if (consp (first rest.tl))
			    (red=rw_apply.to.termlist rule (rest (first rest.tl)) origterm unfailing nil
						      (if (null depth) nil (1- depth)) link)
			  nil)))
		  (if (consp (first rest.tl))
		      (red=rw_apply.to.termlist rule (rest (first rest.tl)) origterm unfailing  nil
						(if (null depth) nil (1- depth)))
		    nil)))))
	  (red=rw_apply.to.termlist.internal rule left right (rest rest.tl)
					     unfailing Not.reduce.term depth link origterm))
    nil))

(defun red=rw_apply.to.termlist (rule termlist origterm &optional unfailing Not.reduce.term depth link)
					; Edited:  23-MAY-1990 13:57
					; Authors: PRCKLN
					; Input:   RULE is an RW_RULE object, TERMLIST is a list of terms
					;          UNFAILING should be true iff RULE is not directable
					;          LINK is a link or NIL.
					;          NOT.REDUCE.TERM is NIL or one of the terms in TERMLIST. 
					; Effect:  Recursively tries to apply RULE to all subterms and terms
					;          in TERMLIST, if once successful, it terminates, 
					;          NOT.REDUCE.TERM is not reduced.
					;          Updates protocol info of LINK if given.
					; Value:   True iff TERMLIST is changed
  (if (or (null depth) (> depth 0))
      (red=rw_apply.to.termlist.internal rule (rds-rw_rule.left rule) (rds-rw_rule.right rule)
					     termlist unfailing Not.reduce.term depth link origterm)
    nil))

(defun red=rw_contextual (plink clause)
						; Edited:  26-JUN-1990 21:07
						; Authors: PRCKLN
						; Input:   A P-link.
						; Effect:  Performs eventually contextual rewriting on PLINK
						; Value:   changes
  (when (ds-link.demodulation.is plink)
    (let* ((changes nil)
	   (eq.clause (ds-link.pospar plink))
	   (eq.litno (ds-link.poslitno plink))
	   (eq.termlist (ds-clause.termlist eq.clause eq.litno))
	   (object.clause (ds-link.negpar plink))
	   (object.litno (ds-link.neglitno plink))
	   (left.access (ds-clause.lit.rewrite.rule eq.clause eq.litno))
	   (matcher (first (ds-link.unifiers plink)))
	   (SMARKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT eq.CLAUSE)))
	   (OMARKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT OBJECT.CLAUSE)))
	   (SVSILINKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT eq.CLAUSE)))
	   (SRLINKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT eq.CLAUSE)))
	   (ORLINKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT OBJECT.CLAUSE)))
	   SIMPLE REMAINED.LITERALS)
      (declare (special red*forward))
      (when (and left.access
		 (or (not red*forward) (eq clause object.clause))
		 (or (> (ds-clause.nolit eq.clause) 1)
		     (> (ds-clause.nolit object.clause) 1)
		     (not (dt-predicate.is.equality (ds-clause.predicate object.clause object.litno)))))
	(RDS-BINDING.PUSH matcher)		; all literals are marked as not removed.
	(RDS-MARKS.RESET OMARKS NIL) 
	(RDS-MARKS.RESET SMARKS NIL) 
	(RDS-MARK.PUT SMARKS eq.LITNO T)	; marking the resolution literals as removed.
	(RDS-MARK.PUT OMARKS OBJECT.LITNO T)
	(COND ((RED=CRR_COMPUTE.S.AND.R.LINKS OBJECT.CLAUSE OBJECT.LITNO eq.clause SVSILINKS
					      SRLINKS ORLINKS eq.litno)
	       ;; computation of s-links from 'eq.clause' to
	       ;; 'object.clause', r- and rd-links to units, and
	       ;; r- and rd-links from 'object.clause' to
	       ;; literal 'eq.litno'. if there does not exist a
	       ;; link for each not potentially false literal of
	       ;; 'eq.clause' to 'object.clause', replacement
	       ;; resolution is not possible.
	       (RED=CRR_MARK.ALL.SUBJECT.LITERALS object.CLAUSE OBJECT.LITNO OMARKS eq.CLAUSE
						  SMARKS SRLINKS SVSILINKS t)
	       ;; attempt to remove (mark) all literals of
	       ;; 'subject.clause'.
	       (SETQ REMAINED.LITERALS NIL SIMPLE NIL)
	       (DODOWN (RPTN (DS-CLAUSE.NOLIT eq.CLAUSE))
		 (when (RED=CRR_MARKED.NOT.REMOVED SMARKS (1+ RPTN))
		   (SETQ REMAINED.LITERALS (CONS (1+ RPTN) REMAINED.LITERALS))))
	       (unless REMAINED.LITERALS	; If all literals are marked as deleted, it will be
						; checked wether the remaining literals of
						; OBJECT.CLAUSE replacement-subsume OBJECT.CLAUSE.
		 (SETQ SIMPLE (notany #'(lambda (variable) (dt-variable.get.binding variable))
				      (ds-clause.variables object.clause)))
		 (unless simple))
	       (COND (SIMPLE			; Replacement resolution is done and
						; bindings are reset, some is terminated.
		      (COND ((RED=CRR_PROTOCOL.RECONSTRUCT OBJECT.CLAUSE OBJECT.LITNO OMARKS
								      eq.CLAUSE eq.LITNO SMARKS
								      SVSILINKS pLINK)
			     (RDS-BINDING.RESET)
			     (red=rw_apply.to.termlist (rds-rw_rule.create
							 :clause eq.clause
							 :left (dt-access (list left.access) eq.termlist)
							 :right (dt-access (dt-taf.otherside (list left.access))
									   eq.termlist))
						       (ds-clause.termlist object.clause object.litno)
						       (ds-clause.termlist object.clause object.litno))
			     (red=ctL_REPLACE.LITERAL object.CLAUSE object.litno
						      (DS-CLAUSE.SIGN object.CLAUSE object.litno)
						      (DS-CLAUSE.PREDICATE object.CLAUSE object.litno)
						      (ds-clause.termlist object.clause object.litno)
						      'REWRITE eq.clause)
			     (RED=CTL_AGENDA.UPDATE NIL object.CLAUSE (RDS-RULES 'ALL))
			     (PR-OPERATION 'REWRITE eq.clause object.CLAUSE object.litno red*rw_taf)
			     (setq changes (insassoc (list object.clause object.litno) changes)))
			    (T (RDS-BINDING.RESET) NIL)))
		     (T				; no replacement resolution is possible, only bindings
						; are reset, value is nil, some not terminated.
		      (RDS-BINDING.RESET) NIL)))
	      (T (RDS-BINDING.reset) NIL))
	(RDS-MARKS.DESTROY 0)
	changes))))

;;; changed following because Lucid 4.1 can't compile these nested
;;; SOME statements.  DAN 16JUN94
#+original-mkrp
(defun red=rewrite.lit (all.rules clause litno &optional red.ctl)
  (WHILE (SOME #'(LAMBDA (RULES FLAG)
		   (SOME #'(LAMBDA (RULE) 
			     (UNLESS (EQL CLAUSE (FIRST RULE))
				     (WHEN (RED=RW_APPLY.TO.TERMLIST
					    RULE
					    (DS-CLAUSE.TERMLIST CLAUSE LITNO)
					    (DS-CLAUSE.TERMLIST CLAUSE LITNO)
					    FLAG
					    (IF (AND (DS-CLAUSE.LIT.REWRITE.RULE CLAUSE LITNO)
						     (DS-CLAUSE.IRREDUCIBLE.IS CLAUSE LITNO))
						(DT-ACCESS (LIST (DS-CLAUSE.LIT.REWRITE.RULE CLAUSE LITNO))
							   (DS-CLAUSE.TERMLIST CLAUSE LITNO))))
					   (if red.ctl
					       (red=ctl_REPLACE.LITERAL CLAUSE LITNO (DS-CLAUSE.SIGN CLAUSE LITNO)
									(DS-CLAUSE.PREDICATE CLAUSE LITNO)
									(DS-CLAUSE.TERMLIST CLAUSE LITNO)
									'REWRITE (FIRST RULE))
					     (cg-REPLACE.LITERAL CLAUSE LITNO (DS-CLAUSE.SIGN CLAUSE LITNO)
								 (DS-CLAUSE.PREDICATE CLAUSE LITNO)
								 (DS-CLAUSE.TERMLIST CLAUSE LITNO)
								 'REWRITE (FIRST RULE)))
					   (PR-OPERATION 'REWRITE (FIRST RULE) CLAUSE LITNO red*rw_taf)
					   T)))
			 RULES))
	       ALL.RULES (LIST NIL T)))
  (unless red.ctl (red=rw_reduce.cc clause litno)))
    
#-original-mkrp
(defun red=rewrite.lit (all.rules clause litno &optional red.ctl)
  (labels ((somepred1 
	    (rules flag)
	    (let ((my-flag flag))
	      (declare (special my-flag))
	      (some #'somepred2 rules)))
	   (somepred2 
	    (rule)
	    (declare (special my-flag))
	    (unless (eql clause (first rule))
	      (when (red=rw_apply.to.termlist
		     rule
		     (ds-clause.termlist clause litno)
		     (ds-clause.termlist clause litno)
		     my-flag
		     (if (and (ds-clause.lit.rewrite.rule clause litno)
			      (ds-clause.irreducible.is clause litno))
			 (dt-access (list (ds-clause.lit.rewrite.rule clause litno))
				    (ds-clause.termlist clause litno))))
		(if red.ctl
		    (red=ctl_replace.literal clause litno (ds-clause.sign clause litno)
					     (ds-clause.predicate clause litno)
					     (ds-clause.termlist clause litno)
					     'rewrite (first rule))
		    (cg-replace.literal clause litno (ds-clause.sign clause litno)
					(ds-clause.predicate clause litno)
					(ds-clause.termlist clause litno)
					'rewrite (first rule)))
		(pr-operation 'rewrite (first rule) clause litno red*rw_taf)
		t))))
    (while
	(some 
	 #'somepred1
	 all.rules 
	 (list nil t)))
  (unless red.ctl (red=rw_reduce.cc clause litno))))

(DEFUN RED-REWRITE (CLAUSE)
					; Edited:  13-JUL-1990 17:40
					; Authors: PRCKLN
					; Input:   A clause.
					; Effect:  Reduces CLAUSE by all unit directed and undirected
					;          equations.
					; Value:   Undefined.
  (LET ((ALL.RULES (LIST (RED-INFO_P.LINK.RULES T)
			 (RED-INFO_P.LINK.UNFAIL.RULES T))))
       (ds-clause.do #'(lambda (litno) (red=rewrite.lit all.rules clause litno))
		     clause)))

(defun red=rw_completion (clause)
  (declare (special red*forward))
  (let ((changes nil))
    (unless red*forward
      (let ((this.rules (list (red-info_p.link.rules (list clause))
			      (red-info_p.link.unfail.rules (list clause)))))
	(mapc #'(lambda (rules flag)
		  (mapc #'(lambda (rule)
			    (mapc #'(lambda (clause)
				      (unless (eql clause (rds-rw_rule.clause rule))
					(ds-clause.do #'(lambda (litno)
							  (when (red=rw_apply.to.termlist
								  rule (ds-clause.termlist clause litno)
								  (ds-clause.termlist clause litno) flag
								  (if (and (ds-clause.lit.rewrite.rule clause litno)
									   (ds-clause.irreducible.is clause litno))
								      (dt-access (list (ds-clause.lit.rewrite.rule
											 clause litno))
										 (ds-clause.termlist clause litno))))
							    (red=ctL_REPLACE.LITERAL CLAUSE litno (DS-CLAUSE.SIGN CLAUSE litno)
										     (DS-CLAUSE.PREDICATE CLAUSE litno)
										     (ds-clause.termlist clause litno)
										     'REWRITE (rds-rw_rule.clause RULE))
							    (RED=CTL_AGENDA.UPDATE NIL CLAUSE (RDS-RULES 'ALL))
							    (PR-OPERATION 'REWRITE (rds-rw_rule.clause RULE)
									  CLAUSE litno  red*rw_taf)
							    (red=rewrite.lit (list (red-info_p.link.rules t)
										   (red-info_p.link.unfail.rules t))
									     clause litno t)
							    (setq changes (insassoc (list clause litno) changes))))
						      clause)))
				  (set-difference (cg-clauses all) (list clause))))
			rules))
	      this.rules (list nil t))))
      (setq changes (delete-if #'(lambda (change)
				   (let ((clause (first change)))
				     (cond ((RED=CS_SUBSUME.TRUE.LITERAL clause)
					    t)
					   (t nil))))
			       changes))
      (red=ctl_RECONSTRUCT nil changes)
      (setq changes nil)
      (while (some #'(lambda (plink)
		       (cond ((setq changes (red=rw_contextual plink clause))
			      (red=ctl_reconstruct nil changes)
			      (setq changes nil)
			      t)))
		   (ds-clause.all.links 'p clause)))))

(DEFUN RED=RW_REPLACE (CLAUSE RULE)
						; edited:  8-jul-83 15:01:07       nb
						; input:   a clause and a rewrite_rule
						; effect:  clause 'clause' is rewritten by rule
						;          'rule'.
						; value:   if 'clause' is modified a list ('clause'
						;          l1 ... lk), where the li are the numbers of
						;          changed literals.
  (let ((RULE.LEFT (first (DS-CLAUSE.TERMLIST RULE 1)))
	(RULE.RIGHT (SECOND (DS-CLAUSE.TERMLIST RULE 1))) HELP.LITLIST)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
      (let ((TERMLIST (COPY-TREE (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN)))))
	(COND ((DT-CONSTANT.IS RULE.LEFT)
	       (COND ((IN RULE.LEFT TERMLIST)
		      (red=ctL_REPLACE.LITERAL CLAUSE (1+ RPTN) (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))
					       (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))
					       (NSUBST RULE.RIGHT RULE.LEFT TERMLIST) 'REWRITE RULE)
		      (when (eq (ds-clause.parents rule) 'theorem)
			(ds-clause.put.parents clause 'theorem))
		      (RED=CTL_AGENDA.UPDATE NIL CLAUSE (RDS-RULES 'ALL))
		      (PR-OPERATION 'REWRITE RULE CLAUSE (1+ RPTN))
		      (SETQ HELP.LITLIST (CONS (1+ RPTN) HELP.LITLIST)))))
	      ((RED=RW_FIND.MATCHES.IN.TERMLIST TERMLIST RULE.LEFT RULE.RIGHT)
	       (red=ctL_REPLACE.LITERAL CLAUSE (1+ RPTN) (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))
					(DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)) TERMLIST 'REWRITE RULE)
	       (RED=CTL_AGENDA.UPDATE NIL CLAUSE (RDS-RULES 'ALL))
	       (PR-OPERATION 'REWRITE RULE CLAUSE (1+ RPTN))
	       (SETQ HELP.LITLIST (CONS (1+ RPTN) HELP.LITLIST))))))
    (CONS CLAUSE HELP.LITLIST)))

(DEFUN RED=RW_FIND.MATCHES.IN.TERMLIST (TERMLIST RULE.LEFT RULE.RIGHT)
						; edited:  8-jul-83 15:19:31           nb
						; input:   a termlist, the first and second term of
						;          a unit clause, where the literal is a
						;          positive equation.
						; effect:  each term of termlist is tested if there
						;          are subterms of term which are one-sided
						;          -unifiable with rule.left .if this check
						;          is positive all these subterms are replaced
						;          by u(rule.right) at which u(rule.right)
						;          is arosen from rule.right by applying
						;          the one_sided_unifier u on rule.right.
						;          termlist is destructively changed.
						; value:   t if test is positive else nil
  (PROG (MATCHER CHFLAG TERM)
	(MAPL
	  #'(LAMBDA (TERMLISTTAIL) (SETQ TERM (CAR TERMLISTTAIL))
		    (COND
		      ((CONSP TERM)
						; term is functional.
		       (COND
			 ((EQL (CAR TERM) (CAR RULE.LEFT))
			  (COND
			    ((SETQ MATCHER (CAR (UNI-UNIFY1.TERMS RULE.LEFT TERM T)))
						; really more general.
			     (RPLACA TERMLISTTAIL (UNI-APPLY.SUBSTITUTION MATCHER RULE.RIGHT T)) (SETQ CHFLAG T)
			     (RED=RW_FIND.MATCHES.IN.TERMLIST (CDR TERM) RULE.LEFT RULE.RIGHT))))
			 (T
			  (SETQ CHFLAG (OR (RED=RW_FIND.MATCHES.IN.TERMLIST (CDR TERM) RULE.LEFT RULE.RIGHT) CHFLAG)))))))
	  TERMLIST)
	(RETURN CHFLAG)))

(DEFUN RED=RW_REWRITE.CLAUSE (CLAUSE)
						; edited:  8-jul-83 15:36:26      nb
						; input:   a clause
						; effect:  in some case the termlist of <clause> is
						;          destructively changed.
						; value:   t if clause is an unit clause, the literal
						;          is an equation and the first or second term
						;          of the literal must be a rewrite constant
						;          or a rewrite function,
						;          else nil.
  (when (AND (DS-CLAUSE.IS.EQUATION CLAUSE 1)
	     (EQL 1 (DS-CLAUSE.NOLIT CLAUSE)))
    (let ((LEFT  (first  (DS-CLAUSE.TERMLIST CLAUSE 1)))
	  (RIGHT (SECOND (DS-CLAUSE.TERMLIST CLAUSE 1))))
      (COND ((NULL (DS-CLAUSE.VARIABLES CLAUSE))
	     (COND ((RED=RW_REWRITE.CONSTANT.IS LEFT RIGHT)
		    (cond ((and (RED=RW_REWRITE.constant.IS RIGHT LEFT)
				(dt-constant.is.skolem right))
			   (red=ctL_REPLACE.LITERAL CLAUSE 1 (DS-CLAUSE.SIGN CLAUSE 1) (DS-CLAUSE.PREDICATE CLAUSE 1)
						    (nREVERSE (DS-CLAUSE.TERMLIST CLAUSE 1)) 'SYMMETRY NIL)
			   (PR-OPERATION 'REWRITE.SYMMETRY CLAUSE)
			   T)
			  (t)))
		   ((RED=RW_REWRITE.CONSTANT.IS RIGHT LEFT)
		    (red=ctL_REPLACE.LITERAL CLAUSE 1
					     (DS-CLAUSE.SIGN CLAUSE 1)
					     (DS-CLAUSE.PREDICATE CLAUSE 1)
					     (nREVERSE (DS-CLAUSE.TERMLIST CLAUSE 1)) 'SYMMETRY NIL)
		    (PR-OPERATION 'REWRITE.SYMMETRY CLAUSE)
		    T)))
	    ((RED=RW_REWRITE.FUNCTION.IS LEFT RIGHT)
	     (cond ((and (RED=RW_REWRITE.FUNCTION.IS RIGHT LEFT)
			 (dt-function.is.skolem (first right)))
		    (red=ctL_REPLACE.LITERAL CLAUSE 1 (DS-CLAUSE.SIGN CLAUSE 1) (DS-CLAUSE.PREDICATE CLAUSE 1)
					     (nREVERSE (DS-CLAUSE.TERMLIST CLAUSE 1)) 'SYMMETRY NIL)
		    (PR-OPERATION 'REWRITE.SYMMETRY CLAUSE)
		    T)
		   (t)))
	    ((RED=RW_REWRITE.FUNCTION.IS RIGHT LEFT)
	     (red=ctL_REPLACE.LITERAL CLAUSE 1 (DS-CLAUSE.SIGN CLAUSE 1) (DS-CLAUSE.PREDICATE CLAUSE 1)
				      (nREVERSE (DS-CLAUSE.TERMLIST CLAUSE 1)) 'SYMMETRY NIL)
	     (PR-OPERATION 'REWRITE.SYMMETRY CLAUSE) T)))))

(DEFUN RED=RW_REWRITE.CONSTANT.IS (TERM1 TERM2)
						; edited:  8-jul-83 15:43:00       nb
						; input:   two terms
						; effect:  -
						; value:   not nil iff
						;                     1:term1 is a constant
						;                     2:term1 is not an element of
						;                       term2
						;                     3:the sort of term2 is a
						;                       subsort of term1.
  (AND (DT-CONSTANT.IS TERM1)
       (NOT (IN TERM1 TERM2))
       (DT-SORT.IS.SUBSORT (DT-TERM.SORT TERM2)
			   (DT-CONSTANT.SORT TERM1))))

(DEFUN RED=RW_REWRITE.FUNCTION.IS (TERM1 TERM2)
						; edited:  8-jul-83 15:48:56          nb
						; input:   two terms
						; effect:  -
						; value:   not nil iff
						;                     1:term1 is functional.
						;                     2:function-name is not element
						;                       of term2
						;                     3:arguments of term1 are
						;                       pairwise disjoint variables.
						;                     4:sort of term2 is a subsort of
						;                       term1
						;                     5:term1 is not associative.
						;                     6:the sorts of the arguments are
						;                       the same as the domainsorts of
						;                       function of term1.
						;                     7:there are no variables in
						;                       term2, which aren't in term1.
  (AND (CONSP TERM1)
       (NOTANYL #'(LAMBDA (TERM.TAIL) (OR (MEMBER (CAR TERM.TAIL) (CDR TERM.TAIL))
					  (NOT (DT-VARIABLE.IS (CAR TERM.TAIL)))))
		(CDR TERM1))
       (EQUAL (MAPCAR #'(LAMBDA (VAR) (DT-VARIABLE.SORT VAR)) (CDR TERM1)) (DT-FUNCTION.DOMAINSORTS (CAR TERM1)))
       (DT-SORT.IS.SUBSORT (DT-TERM.SORT TERM2) (DT-FUNCTION.SORT (CAR TERM1) (DT-FUNCTION.DOMAINSORTS (CAR TERM1))))
       (NOT (INSIDE (CAR TERM1) TERM2)) (NOT (MEMBER 'ASSOCIATIVE (DT-FUNCTION.ATTRIBUTES (CAR TERM1))))
       (NULL (SET-DIFFERENCE (DT-TERMLIST.VARIABLES TERM2) (DT-TERMLIST.VARIABLES TERM1)))))

