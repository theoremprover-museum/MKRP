;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-

(IN-PACKAGE "MKRP" :USE '("CL"))

(DEFUN RED=APPLY_CLAUSE.REPL.RESOLUTION.SUBJECT.TO.CLAUSE (SUBJECT.CLAUSE)
						; edited:  5-jun-84 23:51:53
						; input:  'subject.clause' is a clause.
						; effect: if there exists a sequence of r-, rd-, si-,
						;         and sid-links incident with the
						;         resolvent, selected by the algorithms
						;         described in 'red=crr_mark.all.subject.-
						;         literals' and 'red=crr_mark.object.-
						;         literals'. for each r- and rd-link one
						;         sequence is checked.
						;         successive operations on the links
						;         produce a clause replacement-
						;         subsuming the other parent clause of the
						;         first link, this parent will be replaced by
						;         the inferred clause. this is done by
						;         replacing and deleting literals in the
						;         existing clause.
						;         if there can be made a replacement
						;         resolution, agendas are updated and
						;         information is given to connectiongraph
						;         and protocol module.
						; value:  in case of resolving the empty clause, a
						;         dotted pair: (atom_'success' .
						;         empty_clause) will be returned, else nil.
						; remark: 'replacement subsumption' and using of the
						;         option are explained in
						;         'red=crr_attempt.to.resolve'.
  (PROG (SUCCESS OTHERPAR (CONDITION (RDS-RULE 'CONDITION 'RED*CLAUSE.REPL.RESOLUTION.OBJECT)))
	(COND
	  ((OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT SUBJECT.CLAUSE)))
						; if 'unit' is set as option of replacement
						; resolution rule, 'subject.clause' must be a unit.
	   (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
	     (MEMBER-IF
	       #'(LAMBDA (R.LINKS)
		   (MEMBER-IF
		     #'(LAMBDA (R.LINK)
			 (unless (MEMBER R.LINK (CG-LINKS (DS-LINK.COLOUR R.LINK) REMOVED))
						; if the first and second link of some-area are
						; removed in the some-apply-function, the next link
						; in the some will be a deleted one.
			   (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK SUBJECT.CLAUSE))
			   (COND ((AND (RED=CRR_ATTEMPT.TO.RESOLVE
					 R.LINK SUBJECT.CLAUSE OTHERPAR (1+ RPTN)
					 (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK SUBJECT.CLAUSE) CONDITION)
				       (ZEROP (DS-CLAUSE.NOLIT OTHERPAR)))
						; if the empty clause is generated,the loop can be
						; terminated and value returned.
				  (SETQ SUCCESS (LIST 'SUCCESS OTHERPAR)) (SETQ RPTN 0)))))
		     R.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'REPL.RESOLUTION) SUBJECT.CLAUSE (1+ RPTN))))))
	(RETURN SUCCESS)))

(DEFUN RED=APPLY_CLAUSE.REPL.RESOLUTION.SUBJECT.TO.LINK (LINK)
						; edited:  6-jun-84 03:01:40
						; input:  'link' is a link.
						; effect: different for the colours
						;         r, rd: if there exists a sequence of r-,
						;         rd-, si-, and sid-links
						;         in the resolvent of 'link',
						;         selected by the algorithms described in
						;         'red=crr_mark.all.subject.literals' and
						;         'red=crr_mark.object.literals', so
						;         that successive operations on them produce
						;         a clause replacement-subsuming one of the
						;         parent clauses of 'link', it will be
						;         replaced by this clause. that will be done
						;         by removing or replacing literals of the
						;         parent clause.
						;         si, sid: if recheck option is set, all r-
						;         and rd-links of the parent clause are
						;         treated in the way described in the upper
						;         case.
						;         s: if recheck option is set, all r and rd-
						;         links between the parent clauses of 'link'
						;         are considered.
						;         else: no effect.
						;         if there can be made a replacement
						;         resolution, agendas are updated and
						;         information is given to connectiongraph and
						;         protocol module.
						; value:  if the empty clause is created, it will be
						;         returned a dotted pair: (atom_'success' .
						;         empty_clause), else nil.
						; remark: 'replacement subsumption' and using of the
						;         option are explained in
						;         'red=crr_attempt.to.resolve'.
  (PROG
    ((COLOUR (DS-LINK.COLOUR LINK)) (POSPAR (DS-LINK.POSPAR LINK)) NEGPAR (POSLITNO (DS-LINK.POSLITNO LINK))
     (NEGLITNO (DS-LINK.NEGLITNO LINK)) (CONDITION (RDS-RULE 'CONDITION 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT))
     (OBJECT.CLAUSE NIL))
    (COND
      ((MEMBER COLOUR (RDS-LINK.COLOURS 'REPL.RESOLUTION))
						; r or rd-link: check for unit-option and attempt to
						; resolve with both parents as objects.
       (SETQ NEGPAR (DS-LINK.NEGPAR LINK))
       (COND
	 ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT POSPAR)))
	       (SETQ OBJECT.CLAUSE
		     (RED=CRR_ATTEMPT.TO.RESOLVE LINK POSPAR NEGPAR POSLITNO NEGLITNO CONDITION))))
	 ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT NEGPAR)))
	       (SETQ OBJECT.CLAUSE
		     (RED=CRR_ATTEMPT.TO.RESOLVE LINK NEGPAR POSPAR NEGLITNO POSLITNO CONDITION)))
						; if a resolution has been made, the check of the
						; second case is assigned by snowball effect.
	  )))
      ((MEMBER COLOUR (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL))
						; s-link.
       (SETQ NEGPAR (DS-LINK.NEGPAR LINK))
       (COND
	 ((RDS-RULE 'RECHECK 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT)
	  (SETQ OBJECT.CLAUSE
		(RED=CRR_TO.EXT.SUBSUMPTION.LINK LINK CONDITION POSPAR NEGPAR POSLITNO NEGLITNO)))))
      ((MEMBER COLOUR (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL))
						; si- or sid-link.
       (COND
	 ((RDS-RULE 'RECHECK 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT)
	  (SETQ OBJECT.CLAUSE (RED=CRR_TO.INT.SUBSUMPTION.LINK LINK CONDITION POSPAR POSLITNO NEGLITNO))))))
    (RETURN
      (COND
        ((AND (DS-CLAUSE.IS OBJECT.CLAUSE) (ZEROP (DS-CLAUSE.NOLIT OBJECT.CLAUSE))) (list 'SUCCESS OBJECT.CLAUSE))))))

(DEFUN RED=APPLY_CLAUSE.REPL.RESOLUTION.OBJECT.TO.CLAUSE (OBJECT.CLAUSE)
						; edited:  5-jun-84 21:31:04
						; input:  'object.clause' is a clause.
						; effect: all false literals in clause 'object.clause'
						;         will be removed, by doing a replacement
						;         resolution with a unit-clause, reflexivity
						;         clause for example.
						;         if there exists a r or rd-link of 'object.-
						;         clause' and a serie of r-, rd-, si, and
						;         sid-links incident
						;         with the resolvent, selected by the
						;         algorithms described in
						;         'red=crr_mark.all.subject.literals' and
						;         'red=crr_mark.object.literals', so
						;         that successive operations on them produce
						;         a clause replacement-subsuming
						;         'object.clause', it will be replaced by this
						;         clause. that will be done by deleting or
						;         replacing literals of 'object.clause'
						;         according to the option.
						;         if there can be made a replacement
						;         resolution, agendas are updated and
						;         information is given to the connectiongraph
						;         and the protocol module.
						; value:  if the empty clause is created by deleting
						;         literals, it is returned a dotted pair:
						;         (atom_'success' . 'object.clause'), else
						;         nil.
						; remark: 'replacement subsumption' and using of the
						;         option are explained in
						;         'red=crr_attempt.to.resolve'.
  (let (OTHERPAR (CONDITION (RDS-RULE 'CONDITION 'RED*CLAUSE.REPL.RESOLUTION.OBJECT)))
    (RED=CRR_FALSE.LITERALS OBJECT.CLAUSE)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
      (MEMBER-IF
	#'(LAMBDA (R.LINKS)
	    (MEMBER-IF
	      #'(LAMBDA (R.LINK)
		  (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK OBJECT.CLAUSE))
		  (COND
		    ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT OTHERPAR)))
			  (RED=CRR_ATTEMPT.TO.RESOLVE
			    R.LINK OTHERPAR OBJECT.CLAUSE
			    (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK OBJECT.CLAUSE) (1+ RPTN) CONDITION))
						; if 'unit' is set as option, the other parent clause
						; of the r-link must be a unit clause. if in
						; 'red=crr_attempt.to.resolve' a replacement
						; resolution
						; took place, the loop will be terminated, because
						; the clause is considered again by the snowball
						; effect.
		     (SETQ RPTN 0))))
	      R.LINKS))
	(RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'REPL.RESOLUTION) OBJECT.CLAUSE (1+ RPTN))))
    (COND ((ZEROP (DS-CLAUSE.NOLIT OBJECT.CLAUSE)) (LIST 'SUCCESS OBJECT.CLAUSE)))))

(DEFUN RED=CRR_ATTEMPT.TO.RESOLVE (R.LINK SUBJECT.CLAUSE OBJECT.CLAUSE SUBJECT.LITNO OBJECT.LITNO CONDITION)
						; edited:  6-jun-84 17:17:55
						; input:  'r.link' is a r or rd-link, 'pospar',
						;         'negpar', 'poslitno', and 'neglitno'
						;         describe its parent literals. 
						;         'condition' is one of the atoms 'unit',
						;         'simple', and 'generalizing'.
						; effect: if there exists a sequence of r-, rd-, si,
						;         and sid-links in the resolvent of 'r.link',
						;         selected by the algorithm, described in
						;         'red=crr_mark.all.subject.literals' and
						;         'red=crr_mark.object.literals', so that
						;         successive operations on 'r.link' and this
						;         sequence create a clause replacement-
						;         subsuming 'object.clause', it will be
						;         replaced by this clause, according to
						;         'condition'. replacement is done by removing
						;         or replacing literals of 'object.clause'.
						;         if this replacement resolution can be done,
						;         'object.clause' and by link-removals
						;         affected clauses are inserted into the
						;         agenda as explained in 'red=ctl_agenda.-
						;         update'. all needed information is given to
						;         the protocol and connectiongraph module.
						;         meaning of replacement subsumption:
						;         -----------------------------------
						;         a clause c is replacement subsumed iff for
						;         each of its literals and the current
						;         binding (arosen from actual link sequence),
						;         one of the following conditions is met:
						;         (1) the variables of the literal are not
						;           attached by the binding.
						;         (2) the literals becomes false due to the
						;           binding.
						;         (3) the literal is a double literal.
						;         (4) there exists a s-link to another clause
						;           patizipated in the operation, as described
						;           in 'red=crr_generalizing.condition'.
						;         (2) or (3) must be fulfilled for other
						;         partizipated clauses.
						;         the actual binding can be changed, so that
						;         one of the conditions is fulfilled, if the
						;         change complies with the operation.
						;         meaning of condition:
						;         ---------------------
						;         'unit': 'subject.clause' is a unit. for
						;           the test of
						;           this condition the function calling
						;           'red=crr_attempt.to.resolve' is
						;           responsible. only (1) - (3) are allowed.
						;         'simple': only (1) - (3) are allowed.
						;         'generalizing': (1) - (4) are allowed.
						; value:  'object.clause' if replacement is made, nil
						;         else.
  (unless (ds-link.sort.inhibited r.link)
    (LET ((SMARKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBJECT.CLAUSE)))
	(OMARKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT OBJECT.CLAUSE)))
	(SVSILINKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBJECT.CLAUSE)))
	(SRLINKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBJECT.CLAUSE)))
	(ORLINKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT OBJECT.CLAUSE)))
	SIMPLE GEN REMAINED.LITERALS INFO)
    (PROG1 (COND ((MEMBER-IF
		    #'(LAMBDA (UNI)
			(RDS-BINDING.PUSH UNI) ; all literals are marked as not removed.
			(RDS-MARKS.RESET OMARKS NIL) 
			(RDS-MARKS.RESET SMARKS NIL) 
			(RDS-MARK.PUT SMARKS SUBJECT.LITNO T) ; marking the resolution literals as removed.
			(RDS-MARK.PUT OMARKS OBJECT.LITNO T)
			(COND ((RED=CRR_COMPUTE.S.AND.R.LINKS OBJECT.CLAUSE OBJECT.LITNO SUBJECT.CLAUSE SVSILINKS
							      SRLINKS ORLINKS SUBJECT.LITNO)
			       ;; computation of s-links from 'subject.clause' to
			       ;; 'object.clause', r- and rd-links to units, and
			       ;; r- and rd-links from 'object.clause' to
			       ;; literal 'subject.litno'. if there does not exist a
			       ;; link for each not potentially false literal of
			       ;; 'subject.clause' to 'object.clause', replacement
			       ;; resolution is not possible.
			       (RED=CRR_MARK.ALL.SUBJECT.LITERALS OBJECT.CLAUSE OBJECT.LITNO OMARKS SUBJECT.CLAUSE
								  SMARKS SRLINKS SVSILINKS nil)
			       ;; attempt to remove (mark) all literals of
			       ;; 'subject.clause'.
			       (SETQ REMAINED.LITERALS NIL GEN NIL SIMPLE NIL)
			       (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
				 (when (RED=CRR_MARKED.NOT.REMOVED SMARKS (1+ RPTN))
				   (SETQ REMAINED.LITERALS (CONS (1+ RPTN) REMAINED.LITERALS))))
			       (COND ((NULL REMAINED.LITERALS)	; if all literals are marked as deleted, it will be
						; checked wether the remaining literals of
						; 'object.clause' replacement-subsume 'object.clause'.
				      (SETQ SIMPLE (RED=CRR_SIMPLE.CONDITION OBJECT.CLAUSE OMARKS OBJECT.LITNO ORLINKS
									     SUBJECT.CLAUSE SUBJECT.LITNO)))
				     ((NOT (CDR REMAINED.LITERALS))
				      ;; if one literal of 'subject.clause' remains, it is
				      ;; checked wether resolution literal of 'object.clause'
				      ;; (object literal) can be generalized by this literal.
				      ;; 'condition' must be 'generalizing'.
				      (SETQ GEN (RED=CRR_GENERALIZING.CONDITION SUBJECT.CLAUSE OBJECT.CLAUSE
										(CAR REMAINED.LITERALS)
										OBJECT.LITNO CONDITION OMARKS
										SVSILINKS ORLINKS R.LINK))))
			       (COND (GEN	; generalizing replacement resolution is done and
						; bindings are reset
						; some is terminated.
				      (COND ((SETQ INFO (RED=CRR_PROTOCOL.RECONSTRUCT OBJECT.CLAUSE OBJECT.LITNO OMARKS
										      SUBJECT.CLAUSE SUBJECT.LITNO SMARKS
										      SVSILINKS R.LINK))
					     (SETQ UNI (RDS-BINDING.TOP))
					     (RDS-BINDING.RESET)
					     (RED=CRR_INSERT.ANCESTORS (FOURTH INFO) (THIRD INFO))
					     (PR-OPERATION 'REPLACEMENT.OPERATION OBJECT.CLAUSE INFO)
					     (RED=CTL_AGENDA.UPDATE
					       (UNION (RED=CRR_REMOVE.MARKED.LITS OBJECT.CLAUSE OMARKS R.LINK UNI)
						      (RED=CRR_REPLACE.LITERAL OBJECT.CLAUSE SUBJECT.CLAUSE OBJECT.LITNO
									       (CAR REMAINED.LITERALS) R.LINK UNI))
					       OBJECT.CLAUSE (RDS-RULES 'SUCCESSOR.GENERALIZING)) OBJECT.CLAUSE)
					    (T (RDS-BINDING.RESET) NIL)))
				     (SIMPLE	; replacement resolution is done and
						; bindings are reset;; some is terminated.
				      (COND ((SETQ INFO (RED=CRR_PROTOCOL.RECONSTRUCT OBJECT.CLAUSE OBJECT.LITNO OMARKS
										      SUBJECT.CLAUSE SUBJECT.LITNO SMARKS
										      SVSILINKS R.LINK))
					     (RDS-BINDING.RESET)
					     (RED=CRR_INSERT.ANCESTORS (CAR (FOURTH INFO)) (THIRD INFO))
					     (PR-OPERATION 'REPLACEMENT.OPERATION OBJECT.CLAUSE INFO)
					     (RED=CTL_AGENDA.UPDATE
					       (RED=CRR_REMOVE.MARKED.LITS OBJECT.CLAUSE OMARKS R.LINK UNI) OBJECT.CLAUSE
					       (RDS-RULES 'SUCCESSOR.LITERAL.REMOVAL)) OBJECT.CLAUSE)
					    (T (RDS-BINDING.RESET) NIL)))
				     (T		; no replacement resolution is possible, only bindings
						; are reset, value is nil, some not terminated.
				      (RDS-BINDING.RESET) NIL)))
			      (T (RDS-BINDING.POP) NIL)))
		    (DS-LINK.UNIFIERS R.LINK))	; if replacement took place, 'object.clause' is
						; returned as value.
		  OBJECT.CLAUSE))
	   (RDS-MARKS.DESTROY 0)))))

(DEFUN RED=CRR_TO.EXT.SUBSUMPTION.LINK (S.LINK CONDITION POSPAR NEGPAR POSLITNO NEGLITNO)
						; edited:  6-jun-84 18:55:48
						; input:  's.link' is a s-link between the parent
						;         literals described by the clauses 'pospar'
						;         and 'negpar' and the literal numbers
						;         'poslitno' and 'neglitno'.
						;         'condition' is one of the atoms 'unit',
						;         'simple', and 'generalizing'.
						; effect: for all r- and rd-links between 'pospar' and
						;         'negpar' it will be attempted to make
						;         replacement resolutions as described in
						;         'red=crr_attempt.to.resolve', one with
						;         'pospar' the other one with 'negpar' as
						;         object clause. after success new candidates
						;         for reductions are inserted into agendas, as
						;         described in 'red=ctl_agenda.update'.
						;         all necessary information is given to the
						;         protocol and connectiongraph module.
						;         'condition' is considered as described in
						;         'red=crr_attempt.to.resolve'.
						; value:  if a replacement resolution is done the
						;         changed clause else nil.
  (DECLARE (IGNORE S.LINK POSLITNO NEGLITNO))
  (PROG (RPOSPAR RNEGPAR RPOSLITNO RNEGLITNO OBJECT.CLAUSE)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT POSPAR))
	  (MEMBER-IF
	    #'(LAMBDA (R.LINKS)
		(MEMBER-IF
		  #'(LAMBDA (R.LINK)
		      (COND
			((EQL NEGPAR (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK POSPAR))
			 ;; only r and rd-links between 'pospar' and 'nepar' are
			 ;; considered.
			 (SETQ RPOSPAR (DS-LINK.POSPAR R.LINK)) (SETQ RNEGPAR (DS-LINK.NEGPAR R.LINK))
			 (SETQ RPOSLITNO (DS-LINK.POSLITNO R.LINK)) (SETQ RNEGLITNO (DS-LINK.NEGLITNO R.LINK))
			 (COND
			   ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT RPOSPAR)))
				 (SETQ OBJECT.CLAUSE
				       (RED=CRR_ATTEMPT.TO.RESOLVE R.LINK RPOSPAR RNEGPAR RPOSLITNO RNEGLITNO CONDITION)))
			    ;; the positive parent clause of the r- or rd-link is
			    ;; 'object.clause'. if 'condition is 'unit' subject
			    ;; clause, i.e. the negative parent clause must be a
			    ;; unit clause.
			    (SETQ RPTN 0))
			   ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT RNEGPAR)))
				 (SETQ OBJECT.CLAUSE
				       (RED=CRR_ATTEMPT.TO.RESOLVE R.LINK RNEGPAR RPOSPAR RNEGLITNO RPOSLITNO CONDITION)))
			    ;; the negitive parent clause of the r-link is
			    ;; 'object.clause'. in both cases loop is broken by
			    ;; success.
			    (SETQ RPTN 0))))))
		  R.LINKS))
	    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'REPL.RESOLUTION) POSPAR (1+ RPTN))))
	(RETURN OBJECT.CLAUSE)))

(DEFUN RED=CRR_TO.INT.SUBSUMPTION.LINK (SI.LINK CONDITION CLAUSE POSLITNO NEGLITNO)
						; edited:  6-jun-84 19:14:28
						; input:  'si.link' is a si- or sid-link of its parent
						;         clause 'clause'. its parent literals have
						;         the numbers 'poslitno' and 'neglitno'.
						;         'condition' is one of the atoms 'unit',
						;         'simple', and 'generalizing'.
						; effect: for all r- and rd-links of 'clause' without
						;         these of the parent
						;         literals of 'si.link', it will
						;         be attempted to make replacement resolutions
						;         one with clause and one with the other
						;         parent of the r-links as object clause.
						;         in case of success, new candidates for
						;         reductions are inserted into agendas, as
						;         described in 'red=ctl_agenda.update'.
						;         necessary information is given to
						;         connectiongraph and protocol module.
						;         'condition' is considered as explained
						;         in 'red=crr_attempt.to.resolve'.
						; value:  if a replacement resolution is done the
						;         changed clause is returned, else nil.
  (DECLARE (IGNORE SI.LINK))
  (PROG (RPOSPAR RNEGPAR RPOSLITNO RNEGLITNO OBJECT.CLAUSE)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (COND
	    ((AND (NEQ (1+ RPTN) POSLITNO) (NEQ (1+ RPTN) NEGLITNO))
						; only r- and rd-links of not-parent-literals of
						; 'si.link'.
	     (MEMBER-IF
	       #'(LAMBDA (R.LINKS)
		   (MEMBER-IF
		     #'(LAMBDA (R.LINK) (SETQ RPOSPAR (DS-LINK.POSPAR R.LINK)) (SETQ RNEGPAR (DS-LINK.NEGPAR R.LINK))
			       (SETQ RPOSLITNO (DS-LINK.POSLITNO R.LINK)) (SETQ RNEGLITNO (DS-LINK.NEGLITNO R.LINK))
			       (COND
				 ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT RPOSPAR)))
				       (SETQ OBJECT.CLAUSE
					     (RED=CRR_ATTEMPT.TO.RESOLVE R.LINK RPOSPAR RNEGPAR RPOSLITNO RNEGLITNO CONDITION)))
						; the positive parent clause of the r-link is object.
						; if 'condition' is 'unit' subject clause, i.e. the
						; negative parent clause must be a unit clause, if
						; rule will be applied.
				  (SETQ RPTN 0))
				 ((AND (OR (NEQ CONDITION 'UNIT) (EQL 1 (DS-CLAUSE.NOLIT RNEGPAR)))
				       (SETQ OBJECT.CLAUSE
					     (RED=CRR_ATTEMPT.TO.RESOLVE R.LINK RNEGPAR RPOSPAR RNEGLITNO RPOSLITNO CONDITION)))
						; the negative parent clause of the r-link is object.
						; 'condition' is considered as above.
						; in both cases success leads to termination of the
						; loop, other links are considered by the snowball
						; effect.
				  (SETQ RPTN 0))))
		     R.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'REPL.RESOLUTION) CLAUSE (1+ RPTN))))))
	(RETURN OBJECT.CLAUSE)))

(DEFUN RED=CRR_REMOVE.MARKED.LITS (OC OMARKS R.LINK UNI)
						; edited:  6-jun-84 21:03:40
						; input:  'object.clause' is a clause, 'object.marks'
						;         an array, denoting marks of the literals of
						;         'object.clause'. 'uni' is a substitution of
						;         r- or rd-link 'r.link', on which replacement
						;         resolution will be done. 'uni' and 'r.link'
						;         are only used as information for
						;         connectiongraph trace.
						; effect: if cell i of the array 'object.marks'
						;         contains one of the atoms 't', 'double', and
						;         'false', the literal i of 'object.clause'
						;         will be removed.
						; value:  list of all clauses, which are affected by
						;         removal of r- and p-links of the removed
						;         literal.
  (let (AFFECTED.CLAUSES)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT OC))
      (unless (RED=CRR_MARKED.NOT.REMOVED OMARKS (1+ RPTN))
	(SETQ AFFECTED.CLAUSES
	      (UNION (RED=CTL_REMOVE.LITERAL OC (1+ RPTN) NIL 'REPL.RESOLUTION (CONS R.LINK UNI)) AFFECTED.CLAUSES))))
    (when (and (eq red*state 'initial)
	       (some #'(lambda (clause) (eq (ds-clause.parents clause) 'theorem)) AFFECTED.CLAUSES))
      (DS-CLAUSE.put.PARENTS oc 'theorem))
    AFFECTED.CLAUSES))

(DEFUN RED=CRR_COMPUTE.S.AND.R.LINKS
       (OBJECT.CLAUSE OBJECT.LITNO SUBJECT.CLAUSE SVSILINKS SRLINKS ORLINKS SUBJECT.LITNO)
						; edited:  6-jun-84 21:19:52
						; input:  SUBJECT.CLAUSE and OBJECT.CLAUSE are
						;         two clauses. SUBJECT.LITNO and
						;         OBJECT.LITNO are two literal numbers
						;         describing literals of the clauses.
						;         SVSILINKS and SRLINKS are arrays with
						;         length =
						;         number of literals of OBJECT.CLAUSE.
						;         ORLINKS is an array with length =
						;         number of literals of 'subject.clause'.
						; effect: computes lists of s-links to 'object.clause'
						;         for each literal of 'subject.clause'
						;         and puts them into the fields of the array
						;         'svsilinks' with index = number of the
						;         literal.
						;         in the cells i of the arrays 'orlinks' and
						;         'srlinks' rd- and r-links of the literals
						;         with number i of clauses 'object.clause'
						;         and 'subject.clause' will be stored
						;         if they are incident on their other side
						;         with a unit clause or in case of 'orlinks'
						;         with literal 'subject.litno' of clause
						;         'subject.clause'.
						; value:  not nil iff for each literal of
						;         'subject.clause' not potentially false
						;         exists a link in one of the three
						;         link arrays, that is
						;         compatible with the current binding.
						;         'subject.litno' is not considered.
  (DECLARE (IGNORE CONDITION))
  (let ((POT.FALSE.OF.SUBJECT (DS-CLAUSE.POTENTIALLY.FALSE.LITNOS SUBJECT.CLAUSE)) (NOT.ENOUGH.LINKS NIL))
    ;; initializing the virtual si-link lists and the
    ;; r-link lists to unit clauses
    (RDS-MARKS.RESET SVSILINKS NIL)
    (RDS-MARKS.RESET ORLINKS NIL)
    (RDS-MARKS.RESET SRLINKS NIL)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
      (COND
	((NEQ (1+ RPTN) SUBJECT.LITNO)
	 ;; computing all s-links of 'subject.clause' to
	 ;; 'object.clause' without considering the orientation
	 ;; of the links.
	 (MAPC #'(LAMBDA (NC.LINKS)
		   (MAPC #'(LAMBDA (NC.LINK)
			     (COND
			       ((EQL OBJECT.CLAUSE (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBJECT.CLAUSE))
				(RDS-MARK.PUT SVSILINKS (1+ RPTN) (CONS NC.LINK (RDS-MARK SVSILINKS (1+ RPTN)))))))
			 NC.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBJECT.CLAUSE (1+ RPTN)))
	 (MAPC #'(LAMBDA (R.LINKS)
		   (MAPC #'(LAMBDA (R.LINK)
			     (COND
			       ((and (not (ds-link.sort.inhibited r.link))
				     (OR (= 1 (DS-CLAUSE.NOLIT (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK SUBJECT.CLAUSE)))
					 (AND (EQL OBJECT.CLAUSE (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK SUBJECT.CLAUSE))
					      (EQL OBJECT.LITNO (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK SUBJECT.CLAUSE)))))
				(RDS-MARK.PUT SRLINKS (1+ RPTN) (CONS R.LINK (RDS-MARK SRLINKS (1+ RPTN)))))))
			 R.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'REPL.RESOLUTION) SUBJECT.CLAUSE (1+ RPTN)))
	 ;; links exist or literal is potentially false.
	 (unless (OR (RDS-MARK SRLINKS (1+ RPTN)) (RDS-MARK SVSILINKS (1+ RPTN)) (MEMBER (1+ RPTN) POT.FALSE.OF.SUBJECT))
	   (SETQ NOT.ENOUGH.LINKS T)
	   (SETQ RPTN 0)))))
    (COND
      ((NOT NOT.ENOUGH.LINKS)
       ;; compatibility of existing links.
       (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
	 (COND
	   ((NOT
	      (OR (EQL SUBJECT.LITNO (1+ RPTN)) (MEMBER (1+ RPTN) POT.FALSE.OF.SUBJECT)
		  (RDS-MARK.PUT
		    SVSILINKS (1+ RPTN)
		    (DREMAP (RDS-MARK SVSILINKS (1+ RPTN)) NIL
			    #'(LAMBDA (VSI.LINKS.TAIL)
				(AND (NEQ (RED.SERVICE-OTHERLITNO.EXTERNAL (CAR VSI.LINKS.TAIL) SUBJECT.CLAUSE)
					  OBJECT.LITNO)
				     (NOTANY #'(LAMBDA (TL) (UNI-UNIFY.MIXED.TERMLIST TL))
					     (DS-LINK.UNIFIERS (CAR VSI.LINKS.TAIL)))))
			    NIL))
		  (RDS-MARK.PUT
		    SRLINKS (1+ RPTN)
		    (DREMAP (RDS-MARK SRLINKS (1+ RPTN)) NIL
			    #'(LAMBDA (R.LINKS.TAIL)
				(AND (NEQ (RED.SERVICE-OTHERPAR.EXTERNAL (CAR R.LINKS.TAIL) SUBJECT.CLAUSE) OBJECT.CLAUSE)
				     (NOTANY #'(LAMBDA (TL) (UNI-UNIFY.MIXED.TERMLIST TL))
					     (DS-LINK.UNIFIERS (CAR R.LINKS.TAIL)))))
			    NIL))))
	    (SETQ NOT.ENOUGH.LINKS T) (SETQ RPTN 0))))))
    (COND
      ((NOT NOT.ENOUGH.LINKS)
       (DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
	 (MAPC #'(LAMBDA (R.LINKS)
		   (MAPC #'(LAMBDA (R.LINK)
			     (COND
			       ((and (not (ds-link.sort.inhibited r.link))
				     (OR (EQL 1 (DS-CLAUSE.NOLIT (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK OBJECT.CLAUSE)))
					 (AND (EQL SUBJECT.CLAUSE (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK OBJECT.CLAUSE))
					      (EQL SUBJECT.LITNO (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK OBJECT.CLAUSE)))))
				(RDS-MARK.PUT ORLINKS (1+ RPTN) (CONS R.LINK (RDS-MARK ORLINKS (1+ RPTN)))))))
			 R.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'REPL.RESOLUTION) OBJECT.CLAUSE (1+ RPTN))))))
    (NOT NOT.ENOUGH.LINKS)))

(DEFUN RED=CRR_INSERT.ANCESTORS (MULTIPLES RESOLUTIONS)
						; edited:  6-jun-84 22:05:13
						; input:  'multiples' is a list of elements
						;         (literal_to_be_removed literal_to_remain
						;         rule) each literal being a dotted pair
						;         (clause . litno). 'resolutions' is a list
						;         of dotted pairs (literal_of_object_clause .
						;         other_literal).
						; effect: the list of elements (literal_to_be_removed
						;         . literal_to_remain) and
						;         (literal_of_object_clause . atom_'remove')
						;         is given to change queue of connectiongraph
						;         module.
						; value:  undefined.
  (CG-CHANGE.QUEUE_APPEND
    (CONS 'REPLACEMENT.RESOLUTION
	  (NCONC (MAPCAR #'(LAMBDA (DOUBLE) (CONS (CAR DOUBLE) (SECOND DOUBLE))) MULTIPLES)
		 (MAPCAR #'(LAMBDA (RESOLUTION) (CONS (CAR RESOLUTION) 'REMOVE)) RESOLUTIONS)))))

(DEFUN RED=CRR_FALSE.LITERALS (CLAUSE)
						; edited:  6-jun-84 22:19:03
						; input:  'clause' is a clause.
						; effect: all false literals are removed from 'clause'
						;         and potentially false literal numbers are
						;         updated.
						; value:  undefined.
						; criterion given for induction (structure).
  (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
    (COND
      ((AND (DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)))
	    (RED=CRR_FALSE.STRUCTURE (CAR (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN)))
				     (SECOND (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN)))))
       (RED=CRR_FALSE.REMOVE CLAUSE (1+ RPTN) 'STRUCTURE.AXIOM))))
						; literals in the list of potentially false literal
						; numbers.
  (WHILE
    (MEMBER-IF
      #'(LAMBDA (P)
          (CASE (RED.SERVICE-FALSE.LITERAL.IS CLAUSE P)
            (IMPOSSIBLE
              (DS-CLAUSE.PUT.POTENTIALLY.FALSE.LITNOS CLAUSE
						      (DELETE P (DS-CLAUSE.POTENTIALLY.FALSE.LITNOS CLAUSE))))
            ((T) (RED=CRR_FALSE.REMOVE CLAUSE P (RED.SERVICE-FALSE.CLAUSE CLAUSE P)))
	    (OTHERWISE NIL)))
      (DS-CLAUSE.POTENTIALLY.FALSE.LITNOS CLAUSE))))

(DEFUN RED=CRR_FALSE.STRUCTURE (TERM1 TERM2)
						; input:  term1 and term2 are two terms in standard
						;           representation.
						; effect: -
						; value:  a sexpression <> nil iff there are no
						;         models comparible with the structure -
						;         definitions which satisfies term1 = term2
  (COND ((AND (ATOM TERM1) (ATOM TERM2)) NIL) ((ATOM TERM1) (RED=CRR_FALSE.STRUCTURE TERM2 TERM1))
	(T
	 (PROG ((TERM1.IS.STRUCTURE (MEMBER 'STRUCTURE (DT-FUNCTION.ATTRIBUTES (CAR TERM1)))) NOT.EQUAL)
	       (RETURN
		 (COND
		   ((AND TERM1.IS.STRUCTURE (CONSP TERM2) (EQL (CAR TERM1) (CAR TERM2)))
		    (SMAPC #'(LAMBDA (SUBTERM1 SUBTERM2) (SETQ NOT.EQUAL (RED=CRR_FALSE.STRUCTURE SUBTERM1 SUBTERM2)))
			   #'(LAMBDA (T1.LIST) (COND (NOT.EQUAL NIL) (T (CDR T1.LIST)))) (CDR TERM1) (CDR TERM2))
		    NOT.EQUAL)
		   ((AND TERM1.IS.STRUCTURE (CONSP TERM2) (MEMBER 'STRUCTURE (DT-FUNCTION.ATTRIBUTES (CAR TERM2)))) NIL)
		   (TERM1.IS.STRUCTURE
		    (MEMBER-IF
		      #'(LAMBDA (SUBTERM1)
			  (OR (EQUAL SUBTERM1 TERM2)
			      (AND (CONSP SUBTERM1) (MEMBER 'STRUCTURE (DT-FUNCTION.ATTRIBUTES (CAR SUBTERM1)))
				   (RED=CRR_FALSE.STRUCTURE SUBTERM1 TERM2))))
		      (CDR TERM1)))
		   ((AND (NOT TERM1.IS.STRUCTURE) (CONSP TERM2) (MEMBER 'STRUCTURE (DT-FUNCTION.ATTRIBUTES (CAR TERM2))))
		    (RED=CRR_FALSE.STRUCTURE TERM2 TERM1))))))))

(DEFUN RED=CRR_FALSE.REMOVE (CLAUSE LITNO RESOLVE.CLAUSE)
						; edited:  6-jun-84 22:24:09
						; input:  'litno' is the number of a literal in clause
						;         'clause'. 'resolve.clause' is the unit
						;         clause with which the literal can be
						;         resolved away. 'resolve.clause' can be the
						;         atom 'structure.axiom'.
						; effect: 'litno' is removed, agendas updated,
						;         information given to the connectiongraph
						;         and protocol module.
						; value:  undefined.
  (PR-OPERATION 'REPLACEMENT.OPERATION CLAUSE
		(LIST (first (uni-unify.termlists (ds-clause.termlist clause litno)
						  (ds-clause.termlist resolve.clause 1)))
		      (LIST CLAUSE RESOLVE.CLAUSE)
		      (LIST (LIST (CONS CLAUSE LITNO) (CONS RESOLVE.CLAUSE 1) NIL)) (LIST NIL)))
  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.LITERAL CLAUSE LITNO NIL 'FALSE.LITNO RESOLVE.CLAUSE) NIL NIL))

(DEFUN RED=CRR_MARK.DOUBLE.LITERALS.INTERNAL (CLAUSE MARKS OBJECT.FLAG)
						; edited:  6-jun-84 22:57:12
						; input:  'marks' is a mark array for clause 'clause'.
						;         length of mark array = !'clause'!.
						;         'object.flag' is t, if 'clause' is used as
						;         object for replacement resolution, nil else.
						; effect: all double literals of clause 'clause',
						;         which are detected by si- and sid-links
						;         with nil-unifiers, can be marked with 't',
						;         'double', or 'false' as deleted.
						;         if 'object.flag' is nil ever the positive
						;         parent literal of the si- or sid-link will
						;         be marked with 'double'.
						;         if 'object.flag' is not nil and binding is
						;         a renaming on the variables for both
						;         parent literals, the remaining one is marked
						;         with 'permuteable' and not with 'nil'. if
						;         only one of them is renamed, the other one
						;         is taken for deletion marking. if both are
						;         not renamed or the link is oriented the
						;         positive parent literal is marked.
						; value:  undefined.
  (PROG (OTHERLITNO POSLITNO NEGLITNO)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (COND
	    ((RED=CRR_MARKED.NOT.REMOVED MARKS (1+ RPTN))
						; only literals not yet marked as removed are
						; considered.
	     (MEMBER-IF
	       #'(LAMBDA (NCI.LINKS)
		   (MEMBER-IF
		     #'(LAMBDA (NCI.LINK)
			 (COND
			   ((AND
			      (< (1+ RPTN) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL NCI.LINK (1+ RPTN))))
			      (RED=CRR_MARKED.NOT.REMOVED MARKS OTHERLITNO)
			      (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS NCI.LINK)))
						; avoiding two time consideration of si- or sid-link,
						; the other literal is not removed also, and link has
						; the identical substitution under the current
						; binding.
			    (SETQ POSLITNO (DS-LINK.POSLITNO NCI.LINK)) (SETQ NEGLITNO (DS-LINK.NEGLITNO NCI.LINK))
			    (COND
			      ((NOT OBJECT.FLAG)
						; positive parent literal is removed if not object.
			       (RDS-MARK.PUT MARKS POSLITNO 'DOUBLE) (EQL POSLITNO (1+ RPTN)))
			      ((RED.SERVICE-BINDING.RENAMES.VARIABLES (DS-CLAUSE.LIT.VARIABLES CLAUSE POSLITNO))
			       (COND
				 ((RED.SERVICE-BINDING.RENAMES.VARIABLES
				    (DS-CLAUSE.LIT.VARIABLES CLAUSE NEGLITNO))
						; both literals are renamed by binding. mark the
						; positive one as removed, the other one with
						; 'permuteable'.
                                  (RDS-MARK.PUT MARKS NEGLITNO 'PERMUTEABLE)
                                  (RDS-MARK.PUT MARKS POSLITNO 'DOUBLE) (EQL POSLITNO (1+ RPTN)))
				 ((NOT (RED.SERVICE-LINK.ORIENTED NCI.LINK))
						; the not renamed literal is remove marked.
                                  (RDS-MARK.PUT MARKS NEGLITNO 'DOUBLE) (EQL NEGLITNO (1+ RPTN)))
				 (T		; if link is oriented, the positive parent must be
						; removed. here another strategy can be used, to look
						; for a link in the other direction, so that the not
						; renamed literal can be removed.
                                  (RDS-MARK.PUT MARKS POSLITNO 'DOUBLE) (EQL POSLITNO (1+ RPTN)))))
			      (T		; if the positive parent literal is not renamed, it
						; can be removed without considereation of the other
						; one.
			       (RDS-MARK.PUT MARKS POSLITNO 'DOUBLE) (EQL POSLITNO (1+ RPTN)))))))
		     NCI.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL) CLAUSE (1+ RPTN))))))))

(DEFUN RED=CRR_MARK.DOUBLE.LITERALS.EXTERNAL
       (SUBJECT.CLAUSE SUBJECT.MARKS OBJECT.CLAUSE OBJECT.MARKS SUBJECT.VIRTUAL.SI.LINKS)
						; edited:  7-jun-84 16:24:26
						; input:  'subject.marks' and
						;         'subject.virtual.si.links' are mark arrays
						;         of clause 'subject.clause', 'object.marks'
						;         is a mark array for clause 'object.clause'.
						;         length of mark array = length of its clause.
						;         if cell i of mark array 'subject.marks' or
						;         'object.marks' contains 'double', 'false',
						;         or 't', the corresponding literal of
						;         'subject.clause' or 'object.clause' is
						;         marked as removed.
						;         the cell i of 'subject.virtual.si.links'
						;         contains the list of s-links incident with
						;         'object.clause' and literal i of clause
						;         'subject.clause', except the links not
						;         compatible with the resolution unifier or
						;         incident with the resolution literals.
						; efect_  if one of the s-links in
						;         'subject.virtual.si.links' has a nil-unifier
						;         and both parent literals are not marked as
						;         removed, one of its parent literals is
						;         marked with 'double'. the literal of clause
						;         'subject.clause' is prefered for this
						;         markment, if link is not oriented.
						; value:  undefined.
  (DECLARE (IGNORE OBJECT.CLAUSE))
  (PROG (POSPAR OTHERLITNO)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
	  (COND
	    ((RED=CRR_MARKED.NOT.REMOVED SUBJECT.MARKS (1+ RPTN))
						; the literal is not removed.
	     (MEMBER-IF
	       #'(LAMBDA (VSI.LINK) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL VSI.LINK SUBJECT.CLAUSE))
			 (COND
			   ((AND (RED=CRR_MARKED.NOT.REMOVED OBJECT.MARKS OTHERLITNO)
				 (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS VSI.LINK)))
						; the other literal is not removed and nil is one of
						; the unifiers of the s-link under binding.
			    (SETQ POSPAR (DS-LINK.POSPAR VSI.LINK))
			    (COND
			      ((OR (EQL POSPAR SUBJECT.CLAUSE) (NOT (RED.SERVICE-LINK.ORIENTED VSI.LINK)))
						; if s-link is oriented and 'object.clause' has the
						; positive parent literal of it, it must be marked
						; as removed.
						; other strategy: searching s-link with other
						; direction.
			       (RDS-MARK.PUT SUBJECT.MARKS (1+ RPTN) 'DOUBLE))
			      (T		; literal of 'subject.clause' is removed. values of
						; first and second case of this cond are for
						; termination of the somes, if one of the literals is
						; removed.
			       (RDS-MARK.PUT OBJECT.MARKS OTHERLITNO 'DOUBLE) NIL)))))
	       (RDS-MARK SUBJECT.VIRTUAL.SI.LINKS (1+ RPTN))))))))

(DEFUN RED=CRR_MARK.FALSE.LITERALS (CLAUSE MARKS)
						; edited:  7-jun-84 16:47:57
						; input:  'marks' is a mark array of clause 'clause',
						;         length of 'marks' = length of 'clause'.
						;         cell i of 'marks' in {'double', 'false',
						;         't'} means that literal i of clause 'clause'
						;         is marked as removed.
						; effect: all literals in potentially true literals
						;         not yet marked as removed are checked,
						;         wether they are false literals, considering
						;         the current binding.
						; value:  undefined.
  (MAPC #'(LAMBDA (LITNO)
	    (COND
	      ((AND (RED=CRR_MARKED.NOT.REMOVED MARKS LITNO) (EQL T (RED.SERVICE-FALSE.LITERAL.IS CLAUSE LITNO)))
	       (RDS-MARK.PUT MARKS LITNO 'FALSE))))
	(DS-CLAUSE.POTENTIALLY.FALSE.LITNOS CLAUSE)))

(DEFUN RED=CRR_MARK.ALL.SUBJECT.LITERALS
       (OBJECT.CLAUSE OBJECT.LITNO OBJECT.MARKS SUBJECT.CLAUSE SUBJECT.MARKS SRLINKS SUBJECT.VIRTUAL.SI.LINKS PARA.p)
						; edited:  7-jun-84 16:55:44
						; input:  'object.clause' and 'subject.clause' are
						;         two clauses, 'object.marks' and
						;         'subject.marks' two mark arrays of
						;         corresponding lengths, denoting literal
						;         labels. one literal in both clauses is
						;         marked as deleted with 't' in the mark
						;         array field with index 'number of literal'.
						;         the others are marked with 'nil' as not
						;         removed.
						;         one of the unifiers of a r- or rd-link
						;         connecting
						;         the two remove marked literals is set as
						;         binding, i.e. a virtual resolution is done.
						;         'subject.virtual.si.links' is an array with
						;         length = !'subject.clause'!. the s-links of
						;         literal i of 'subject.clause' to
						;         'object.clause', compatible with the
						;         binding are listed in the ith component of.
						;         the array.
						;         'srlinks' is another array of this type,
						;         but with r- and rd-links of literal i
						;         to unit clauses listed in cell i.
						;         PARA.P is true iff the operation is a paramodulation
						;         and the op literals are not necessarily opposite.
						; effect: operations on unifiers of the links in the
						;         arrays will be simulated to mark all
						;         literals of 'subject.clause' as removed.
						;         selection of operation unifiers is described
						;         in 'red=crr_mark.operating'.
						; value:  nil.
  (PROG (OTHERLITNO MERGE.UNIS) (RED=CRR_MARK.DOUBLE.LITERALS.INTERNAL SUBJECT.CLAUSE SUBJECT.MARKS NIL)
	;; marking double and false literals of resolution
	;; in clause 'subject.clause'.
	(RED=CRR_MARK.DOUBLE.LITERALS.EXTERNAL SUBJECT.CLAUSE SUBJECT.MARKS OBJECT.CLAUSE OBJECT.MARKS
					       SUBJECT.VIRTUAL.SI.LINKS)
	(RED=CRR_MARK.FALSE.LITERALS SUBJECT.CLAUSE SUBJECT.MARKS)
	;; considering not deleted literals.
	(DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
	  (COND
	    ((RED=CRR_MARKED.NOT.REMOVED SUBJECT.MARKS (1+ RPTN))
	     ;; internal links.
	     (MAPC #'(LAMBDA (NCI.LINKS)
		       (MAPC #'(LAMBDA (NCI.LINK) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL NCI.LINK (1+ RPTN)))
				       (COND
					 ((AND (RED=CRR_MARKED.NOT.REMOVED SUBJECT.MARKS OTHERLITNO) (< OTHERLITNO (1+ RPTN)))
					  (MAPC #'(LAMBDA (NCI.UNI)
						    (SETQ MERGE.UNIS
							  (NCONC (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) NCI.UNI)
								 MERGE.UNIS)))
						(DS-LINK.UNIFIERS NCI.LINK))
					  ;; both literals of link are not deleted, avoidance of
					  ;; two time test of compatibility with binding, and
					  ;; computing the merge unifiers with the binding.
					  )))
			     NCI.LINKS))
		   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL) SUBJECT.CLAUSE (1+ RPTN)))
	     ;; considering s-links between clauses 'subject.clause'
	     ;; and 'object.clause'.
	     (MAPC #'(LAMBDA (NC.LINK) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK SUBJECT.CLAUSE))
			     (COND
			       ((RED=CRR_MARKED.NOT.REMOVED OBJECT.MARKS OTHERLITNO)
				(MAPC #'(LAMBDA (NC.UNI)
					  (SETQ MERGE.UNIS
						(NCONC (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) NC.UNI)
						       MERGE.UNIS)))
				      (DS-LINK.UNIFIERS NC.LINK))
				;; both literals of link are not marked as removed and
				;; computing the merge unifiers with the binding.
				)))
		   (RDS-MARK SUBJECT.VIRTUAL.SI.LINKS (1+ RPTN)))
	     ;; select merge unifier or r- or rd-unifier to unit,
	     ;; push binding, and mark false, resolution,
	     ;; and double literals.
	     (unless para.p
	       (RED=CRR_MARK.OPERATING OBJECT.CLAUSE OBJECT.LITNO OBJECT.MARKS SUBJECT.CLAUSE SUBJECT.MARKS
				       (1+ RPTN) SUBJECT.VIRTUAL.SI.LINKS SRLINKS MERGE.UNIS)))))))

(DEFUN RED=CRR_MARK.OPERATING
       (OBJECT.CLAUSE OBJECT.OP.LITNO OBJECT.MARKS SUBJECT.CLAUSE SUBJECT.MARKS ACTUAL.SUBJECT.LITNO
	SUBJECT.VIRTUAL.SI.LINKS SRLINKS MUNIS)
						; edited:  7-jun-84 18:35:16
						; input:  'object.clause' and 'subject.clause' are
						;         two clauses. 'object.litno' is the number
						;         of a literal in 'object.clause',
						;         'subject.litno' one of 'subject.clause'.
						;         'subject.virtual.si.links' is an array with
						;         length = !'subject.clause'!, cell i contains
						;         a list of s-links from literal i of
						;         'subject.clause' to 'object.clause'.
						;         'srlinks' is an array of the same type,
						;         but with r- and rd-links to unit clauses
						;         as entries.
						;         'munis' is a list of unifiers merging the
						;         current binding with unifiers of a s-, si,
						;         or sid-link of the same literal of
						;         'subject.clause'.
						;         'subject.marks' and 'object.marks' are mark
						;         arrays for the literals of the two clauses.
						; effect: pushs a unifier from munis as new binding.
						;         it is selected the unifier less
						;         instancing. false and double literals of
						;         clause 'subject.clause' are marked.
						; value:  undefined.
  (let ((MIN 1000)
	CAND UNI LEFT RIGHT R.UNI)
    (MEMBER-IF #'(LAMBDA (MUNI)
		   (SETQ CAND 0)
		   (SMAPL #'(LAMBDA (MUNI.TAIL)
			      (SETQ LEFT (CAR MUNI.TAIL))
			      (SETQ RIGHT (SECOND MUNI.TAIL))
			      (unless (AND (DT-VARIABLE.IS RIGHT)
					   (EQL (DT-TERM.SORT RIGHT) (DT-TERM.SORT LEFT)))
				(SETQ CAND (1+ CAND))))
			  #'CDDR MUNI)
		   (COND ((ZEROP CAND) (SETQ MIN 0) (SETQ UNI MUNI) T) ((> MIN CAND) (SETQ UNI MUNI) (SETQ MIN CAND) NIL)))
	       MUNIS)
    (COND
      ((SETQ R.UNI
	     (RED=CRR_MARK.UNIT.RESOLUTION OBJECT.CLAUSE SUBJECT.CLAUSE SUBJECT.MARKS ACTUAL.SUBJECT.LITNO
					   SRLINKS MIN))
       (RDS-BINDING.PUSH (CAR R.UNI)))
      ((SETQ R.UNI
	     (RED=CRR_MARK.S.RESOLUTION SUBJECT.CLAUSE ACTUAL.SUBJECT.LITNO SUBJECT.MARKS SRLINKS OBJECT.CLAUSE
					OBJECT.OP.LITNO))
       (RDS-BINDING.PUSH (CAR R.UNI)))
      (MUNIS (RDS-BINDING.PUSH UNI)))
    (RED=CRR_MARK.DOUBLE.LITERALS.INTERNAL SUBJECT.CLAUSE SUBJECT.MARKS NIL)
    (RED=CRR_MARK.DOUBLE.LITERALS.EXTERNAL SUBJECT.CLAUSE SUBJECT.MARKS OBJECT.CLAUSE OBJECT.MARKS
					   SUBJECT.VIRTUAL.SI.LINKS)
    (RED=CRR_MARK.FALSE.LITERALS SUBJECT.CLAUSE SUBJECT.MARKS)))

(DEFUN RED=CRR_MARK.OBJECT.LITERALS
       (OBJECT.CLAUSE OBJECT.OPERATION.LITNO OBJECT.MARKS OBJECT.R.LINKS.TO.UNITS SUBJECT.CLAUSE
	SUBJECT.OPERATION.LITNO)
						; edited:  2-aug-84 13:08:52
						; input:  'object.clause' and 'subject.clause' are the
						;         clauses, with which
						;         a replacement resolution is attempted.
						;         'object.marks' is a mark array for clause
						;         'clause': cell i contains a mark of literal
						;         i of clause 'object.clause'.
						;         'object.r.links.to.units' is also an array
						;         with same length as clause 'object.clause'.
						;         cell i of it contains a list of r- and rd-
						;         links of literal i of clause 'object.clause'
						;         to unit clauses and to the literal
						;         'subject.operation.litno' of 'subject.-
						;         clause'.
						; effect: if it exists a r-, rd-, si- or sid-link
						;         as described in the functions
						;         'red=crr_mark.unit.resolution',
						;         'red=crr_mark.s.resolution', and
						;         'red=crr_mark.object.factoring'
						;         so that
						;         the literals on whose variables the binding
						;         is no variable renaming (literal must be
						;         removed for replacement resolution) can
						;         be resolved or factored
						;         then
						;         the corresponding operation unifier is
						;         merged into the binding and literal is
						;         marked with information about the operation.
						;         newly appearing double and false literals
						;         are not considered (only operations not
						;         modifying other literals than the one to be
						;         removed).
						; value:  not nil iff literal can not be marked as
						;         removed.
  (DECLARE (IGNORE OBJECT.OPERATION.LITNO))
  (PROG (FAILURE UNIS)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
	  (COND
	    ((AND (RED=CRR_MARKED.NOT.REMOVED OBJECT.MARKS (1+ RPTN))
		  (NOT (RED.SERVICE-BINDING.RENAMES.VARIABLES (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE (1+ RPTN)))))
	     (COND
	       ((SETQ UNIS
		      (RED=CRR_MARK.UNIT.RESOLUTION SUBJECT.CLAUSE OBJECT.CLAUSE OBJECT.MARKS (1+ RPTN)
						    OBJECT.R.LINKS.TO.UNITS 1))
		(RDS-BINDING.PUSH (CAR UNIS)))
	       ((SETQ UNIS
		      (RED=CRR_MARK.S.RESOLUTION OBJECT.CLAUSE (1+ RPTN) OBJECT.MARKS OBJECT.R.LINKS.TO.UNITS
						 SUBJECT.CLAUSE SUBJECT.OPERATION.LITNO))
		(RDS-BINDING.PUSH (CAR UNIS)))
	       ((NOT (RED=CRR_MARK.OBJECT.FACTORING OBJECT.CLAUSE (1+ RPTN) OBJECT.MARKS)) (SETQ FAILURE T) (SETQ RPTN 0))))))
	(RETURN FAILURE)))

(DEFUN RED=CRR_MARK.UNIT.RESOLUTION
       (PARTNER.CLAUSE CLAUSE MARKS LITERAL.TO.RESOLVE R.LINKS.TO.UNITS NUMBER.INSTANCE)
						; edited:  2-aug-84 13:02:42
						; input:  'clause' and 'partner.clause' are two
						;         clauses.
						;         a replacement resolution is attempted
						;         between them.
						;         'literal.to.remove' is the number of a
						;         literal in clause 'clause'.
						;         'marks' is a mark array for clause 'clause':
						;         cell i of it contains a mark of literal i
						;         of clause 'clause'.
						;         'literal.to.resolve' has mark nil (not
						;         removed) in 'marks'.
						;         'r.links.to.units' is also an array with
						;         same length as clause 'clause'. cell i of
						;         it contains a list of r- and rd-links of
						;         literal i of clause 'clause' to unit
						;         clauses and literal of replacement
						;         resolution of clause 'partner.clause'.
						;         'number.instance' is a natural number.
						; effect: if one of the unifiers of the
						;         'r.links.to.units' of literal
						;         'litno.to.resolve' without the links to the
						;         'partner.clause' instances less than
						;         'instance.number' variables of the clauses
						;         'clause' and 'partner.clause' under the
						;         current binding
						;         then
						;         literal 'litno.to.resolve' will be marked as
						;         removed with the list ((unit_clause . 1)
						;         rule_of_the_r-_or_rd-link_to_unit).
						;         r- or rd-link with unifier with minimal
						;         instance number is selected.
						; value:  list of unifiers:
						;         merge of the binding with the unifier
						;         described above.
  (LET
    ((VARIABLES.IN.CLAUSES (UNION (DS-CLAUSE.VARIABLES CLAUSE) (DS-CLAUSE.VARIABLES PARTNER.CLAUSE))) OTHERPAR NEW.NUMBER
     OP.PARTNER OP.UNI OP.LINK)
    (MAPC #'(LAMBDA (R.LINK) 
	      (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK CLAUSE))
	      (unless (EQl OTHERPAR PARTNER.CLAUSE)
		(MAPC #'(LAMBDA (UNI)
			  (SETQ NEW.NUMBER (LIST-LENGTH (INTERSECTION (UNI-UNIFIER.DOMAIN UNI (RDS-BINDING.TOP))
								      VARIABLES.IN.CLAUSES)))
			  (COND
			    ((< NEW.NUMBER NUMBER.INSTANCE) (SETQ NUMBER.INSTANCE NEW.NUMBER) (SETQ OP.LINK R.LINK)
			     (SETQ OP.PARTNER OTHERPAR) (SETQ OP.UNI UNI))))
		      (DS-LINK.UNIFIERS R.LINK))))
	  (RDS-MARK R.LINKS.TO.UNITS LITERAL.TO.RESOLVE))
    (COND
      ((AND OP.PARTNER
	    (SETQ OP.UNI
		  (MAPCAN #'(LAMBDA (RENAME.UNI) (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) RENAME.UNI))
			  (RED.SERVICE-RENAME (LIST (COPY-TREE OP.UNI)) (DS-CLAUSE.VARIABLES OP.PARTNER)))))
       (RDS-MARK.PUT MARKS LITERAL.TO.RESOLVE (LIST (CONS OP.PARTNER 1) (DS-LINK.RULE OP.LINK))) OP.UNI))))

(DEFUN RED=CRR_MARK.S.RESOLUTION
       (OBJECT.CLAUSE OBJECT.LITNO OBJECT.MARKS OBJECT.R.LINKS.TO.UNITS SUBJECT.CLAUSE SUBJECT.OPERATION.LITNO)
						; edited:  2-aug-84 13:15:30
						; input:  'object.clause' and 'subject.clause' are the
						;         clauses, with which
						;         a replacement resolution is attempted.
						;         'subject.operation.litno' is the number of
						;         the operation literal in 'subject.clause'
						;         (parent literal of replacement resolution
						;         link).
						;         'object.marks' is a mark array for clause
						;         'object.clause': cell i contains a mark of
						;         literal i of clause 'object.clause'.
						;         'object.r.links.to.units' is also an array
						;         with same length as clause 'object.clause'.
						;         cell i of it contains a list of r- and rd-
						;         links of literal i of clause 'object.clause'
						;         to unit clauses and to the literal
						;         'subject.operation.litno' in 'subject.-
						;         clause'.
						;         'object.litno' is the number of the literal
						;         in 'object.clause' to be resolved away.
						; effect: if
						;         it exists a r- or rd-link in the
						;         'object.r.links.to.units' for
						;         literal 'object.litno' incident with
						;         the literal 'subject.operation.litno'
						;         so that
						;         only variables in the literal
						;         'subject.operation.litno' are changed
						;         (compatible with the binding for the
						;         other literals in 'subject.clause').
						;         then
						;         the literal 'object.litno' is marked with
						;         the list (('subject.clause' .
						;         'subject.operation.litno')
						;         rule_of_the_r-_or_rd-link) as removed.
						; value:  if a unifier exists as described above then
						;         a list with one unifier: the current
						;         binding.
  (let (MARK matchers (VARS (DS-CLAUSE.VARIABLES OBJECT.CLAUSE)))
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
      (COND
	((NEQ (1+ RPTN) SUBJECT.OPERATION.LITNO)
	 (SETQ VARS (UNION (COPY-LIST (DS-CLAUSE.LIT.VARIABLES SUBJECT.CLAUSE (1+ RPTN))) VARS)))))
    (MEMBER-IF #'(LAMBDA (R.LINK)
		   (COND ((EQL SUBJECT.CLAUSE (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK OBJECT.CLAUSE))
			  (let ((interest.vars (SET-DIFFERENCE (DS-CLAUSE.LIT.VARIABLES SUBJECT.CLAUSE SUBJECT.OPERATION.LITNO)
							       VARS)))
			    (MEMBER-IF #'(LAMBDA (UNI)
					   (let* ((runis (RED.SERVICE-RENAME (list (copy-tree uni)) interest.vars)))
					     (when (and (UNI-UNIFIER.BECOMES.MATCHER (first runis) VARS)
							(UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) (first runis)))
					       (setq matchers uni)
					       (SETQ MARK
						     (LIST (CONS SUBJECT.CLAUSE (RED.SERVICE-OTHERLITNO.EXTERNAL
										  R.LINK OBJECT.CLAUSE))
							   (DS-LINK.RULE R.LINK))))))
				       (DS-LINK.UNIFIERS R.LINK))))))
	       (RDS-MARK OBJECT.R.LINKS.TO.UNITS OBJECT.LITNO))
    (COND (MARK (RDS-MARK.PUT OBJECT.MARKS OBJECT.LITNO MARK)
		(UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) matchers)))))

(DEFUN RED=CRR_MARK.OBJECT.FACTORING (OBJECT.CLAUSE OBJECT.REMOVE.LITNO OBJECT.MARKS)
						; edited:  2-aug-84 13:23:23
						; input:  'object.remove.litno' is the number of the
						;         literal in clause 'object.clause' to be
						;         removed by factoring.
						;         'object.marks' is a mark array for clause
						;         'object.clause': cell i contains a mark of
						;         literal i of clause 'object.clause'.
						;         'object.remove.litno' is marked with nil,
						;         i.e. not removed.
						; effect: if
						;         it exists a si- or sid-link incident with
						;         literal 'object.remove.litno' and another
						;         literal marked with not removed too
						;         so that
						;         if the link is oriented the literal
						;         'object.remove.litno' is the positive parent
						;         literal, for the other literal the binding
						;         is a variable renaming, and the unifier
						;         only modifies variables of literal
						;         'object.remove.litno'
						;         then
						;         one of the merge unifiers (the first one)
						;         of such a si- or sid-link-unifier is set as
						;         new binding and literal 'object.remove.-
						;         litno' is marked as removed in
						;         'red=crr_mark.double.literals.internal'.
  (PROG (OTHL OPU)
	(MEMBER-IF
	  #'(LAMBDA (NCI.LINKS)
	      (MEMBER-IF
		#'(LAMBDA (NCI.LINK) (SETQ OTHL (RED.SERVICE-OTHERLITNO.INTERNAL NCI.LINK OBJECT.REMOVE.LITNO))
			  (COND
			    ((AND
			       (OR (EQL OBJECT.REMOVE.LITNO (DS-LINK.POSLITNO NCI.LINK))
				   (NOT (RED.SERVICE-LINK.ORIENTED NCI.LINK)))
			       (RED.SERVICE-BINDING.RENAMES.VARIABLES (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE OTHL)))
			     (MEMBER-IF
			       #'(LAMBDA (NCI.UNI)
				   (COND
				     ((UNI-UNIFIER.BECOMES.MATCHER NCI.UNI (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE OTHL))
				      (SETQ OPU NCI.UNI))))
			       (DS-LINK.UNIFIERS NCI.LINK)))))
		NCI.LINKS))
	  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL) OBJECT.CLAUSE
				    OBJECT.REMOVE.LITNO))
	(RETURN
	  (COND
	    (OPU (RDS-BINDING.PUSH (CAR (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) OPU)))
		 (RED=CRR_MARK.DOUBLE.LITERALS.INTERNAL OBJECT.CLAUSE OBJECT.MARKS T)
		 (RED=CRR_MARK.FALSE.LITERALS OBJECT.CLAUSE OBJECT.MARKS) T)))))

(DEFUN RED=CRR_MARKED.NOT.REMOVED (MARK.ARRAY INDEX)
						; edited:  7-jun-84 18:45:46
						; input:  'mark.array' is an array, 'index' the index
						;         of one of its cells.
						; effect: -
						; value:  not nil iff the cell denoted by the index
						;         contains 'nil' or 'permuteable'.
  (MEMBER (RDS-MARK MARK.ARRAY INDEX) '(NIL PERMUTEABLE)))

(DEFUN RED=CRR_GENERALIZING.CONDITION
       (SUBJECT.CLAUSE OBJECT.CLAUSE SUBJECT.LITNO OBJECT.LITNO CONDITION OBJECT.MARKS SVSILINKS ORLINKS OP.LINK)
						; edited:  7-jun-84 18:48:02
						; input:  'subject.litno' describes a literal of
						;         clause 'subject.clause', 'object.litno' one
						;         of clause 'object.clause'.
						;         'condition' is one of the atoms 'unit',
						;         'simple', and 'generalizing'.
						;         'object.marks' is a mark array of clause
						;         'object.clause', length of 'object.marks' =
						;         number of literals of 'object.clause'.
						;         field i of 'object.marks' corresponds with
						;         literal i of 'object.clause'. if it contains
						;         't', 'double', or 'false', the literal is
						;         regarded as removed.
						;         'subject.virtual.si.links' is an array with
						;         length = !'subject.clause'!, cell i contains
						;         a list of s-links from literal i of
						;         'subject.clause' to 'object.clause'.
						; effect: if 'condition' is 'generalizing', simple
						;         condition is fulfilled as explained in
						;         'red=crr_simple.condition', and further
						;         generalizing conditions as described in
						;         'red=crr_generalizing.to.literals' are met,
						;         the removal mark of 'object.litno' is reset
						;         to not removed and binding can be changed
						;         as described in 'red=crr_generalizing.to.-
						;         unifier'.
						; value:  not nil iff all this conditions are met.
  (COND
    ((AND (EQL CONDITION 'GENERALIZING)
	  (RED=CRR_SIMPLE.CONDITION OBJECT.CLAUSE OBJECT.MARKS OBJECT.LITNO ORLINKS SUBJECT.CLAUSE SUBJECT.LITNO)
	  (LET ((ORV NIL))
						; computing all variables of the literals of clause
						; 'object.clause', not marked as removed.
	    (DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
	      (WHEN (RED=CRR_MARKED.NOT.REMOVED OBJECT.MARKS (1+ RPTN))
		(SETQ ORV (UNION (COPY-LIST (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE (1+ RPTN))) ORV))))
	    (RED=CRR_GENERALIZING.TO.LITERALS SUBJECT.CLAUSE OBJECT.CLAUSE SUBJECT.LITNO OBJECT.LITNO SVSILINKS ORV OP.LINK)))
     (RDS-MARK.PUT OBJECT.MARKS OBJECT.LITNO NIL)
     T)))

(DEFUN RED=CRR_GENERALIZING.TO.LITERALS
       (SUBJECT.CLAUSE OBJECT.CLAUSE SUBJECT.LITNO OBJECT.LITNO SVSILINKS OBJECT.REST.VARIABLES OP.LINK)
						; edited:  7-jun-84 19:16:49
						; input:  'subject.litno' describes a literal of
						;         clause 'subject.clause', 'object.litno' one
						;         of clause 'object.clause'.
						;         'subject.virtual.si.links' is an array with
						;         length = !'subject.clause'!, cell i contains
						;         a list of s-links from literal i of
						;         'subject.clause' to 'object.clause'.
						;         'rest.clause.variables' is the set of all
						;         variables of not remove marked literals of
						;         clause 'object.clause'.
						; effect: changes bindings as explained in
						;         'red=crr_generalizing.to.unifier'.
						; value:  not nil iff there exists a s-link between
						;         literals 'subject.litno' and 'object.litno'
						;         (for an oriented s-link, 'object.clause'
						;         must be the negative parent clause). with
						;         an unifier, so that conditions described in
						;         'red=crr_generalizing.to.unifier' are
						;         fulfilled.
  (LET ((FROM.LITERAL.VARIABLES (DS-CLAUSE.LIT.VARIABLES SUBJECT.CLAUSE SUBJECT.LITNO))
	(TO.LITERAL.VARIABLES (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE OBJECT.LITNO)))
    (MEMBER-IF #'(LAMBDA (S.LINK)
		   (AND (EQL OBJECT.CLAUSE (RED.SERVICE-OTHERPAR.EXTERNAL S.LINK SUBJECT.CLAUSE))
			(EQL OBJECT.LITNO (RED.SERVICE-OTHERLITNO.EXTERNAL S.LINK SUBJECT.CLAUSE))
			(OR (EQL OBJECT.CLAUSE (DS-LINK.NEGPAR S.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED S.LINK)))
			(MEMBER-IF #'(LAMBDA (S.UNI)
				       (RED=CRR_GENERALIZING.TO.UNIFIER S.UNI OBJECT.REST.VARIABLES TO.LITERAL.VARIABLES
									OBJECT.LITNO FROM.LITERAL.VARIABLES SUBJECT.LITNO
									SUBJECT.CLAUSE OBJECT.CLAUSE OP.LINK))
				   (DS-LINK.UNIFIERS S.LINK))))
	       (RDS-MARK SVSILINKS SUBJECT.LITNO))))

(DEFUN RED=CRR_GENERALIZING.TO.UNIFIER
       (S.UNIFIER REST.CLAUSE.VARIABLES TO.LITERAL.VARIABLES TO.LITNO FROM.LITERAL.VARIOABLES FROM.LITNO
	SUBJECT.CLAUSE OBJECT.CLAUSE OP.LINK)
						; edited: 14-apr-84 06:14:20
						; input:  's.unifier' is a unifier.
						;         'rest.clause.variables'(rcv) is the set of
						;         variables of literals of clause
						;         'object.clause'(oc) not marked as removed.
						;         'to.literal.variables'(tlv) is the set of
						;         variables of the literal of clause 'oc',
						;         that will be replaced by a literal of clause
						;         'subject.clause'(sc) with variableset
						;         'from.literal.variables'(flv).
						;         there can exist bindings of variables.
						;         the unifier (x1<-t1, ... , xn<-tn)
						;         with xi in 'rcv',
						;              ti =   binding(xi) if binding(xi)
						;                                    =//= nil, and
						;              ti =   xi          if binding(xi)
						;                                     =  nil
						;         is a variable renaming.
						; effect: let 'm1' be equivalent to current bindings
						;         so that without binding:
						;         domain('m1') intersection 'rcv' = emptyset,
						;         i.e. 'm1' does not modify 'rcv'.
						;
						;         let 'm1!tlv' be an unifier so that without
						;         bindings:
						;         domain('m1!tlv') = domain('m1') - 'tlv',
						;         and x<-t in 'm1!tlv' => x<-t in 'm1', i.e.
						;         'm1' without the part modifying 'tlv'.
						;
						;         let 'm1!tlv.r' be the unifier 'm1!tlv',
						;         where all variables of 'oc' in codomain
						;         ('m1!tlv') are renamed by new variables.
						;
						;         let 'm2' be equivalent to the merge of
						;         's.unifier' and a renaming for 'rcv' using
						;         the same renaming variables as in generating
						;         'm1!tlv.r' so that:
						;         with a binding 'm1!tlv.r' 'm2' is a matcher
						;         with variables(codomain('m2')) is a subset
						;         of variables of 'oc', and
						;         'm2' is no variable renaming.
						;
						;         if there exist 'm1' and 'm2', then the
						;         current bindings are replaced by the unifier
						;         'm1'. the old bindingunifier is pushed onto
						;         bindingstack.
						;
						; value:  not nil iff there exist 'm1' and 'm2'.
						;
						; example:situation 1 (success) before:
						;           z        z            a
						;           !        !            !
						;           v        v            v
						;         q(x) p(a b x)        -p(y b z) p(y u z)
						;         's.unifier' = y<-a, u<-b, z<-x,
						;         'rcv'       = {x},
						;         'tlv'       = {x},
						;         'flv'       = {y, u, z},
						;         'm1'       := z<-x, y<-a,
						;         'm1!tlv.r' := z<-x', y<-a,
						;         'm2'       := u<-b, x'<-x, y<-a.
						;         situation 1 (success) after:
						;                                 a   x        x
						;                                 !   !        !
						;                                 v   v        v
						;         q(x) p(a b x)        -p(y b z) p(y u z)
						;
						;         situation 2 (failure) before:
						;                x                x   x    x
						;                !                !   !    !
						;                v                v   v    v
						;         q(x) p(y a x)        -p(u a u) p(u v w)
						;         's.unifier' = u<-y, v<-a, w<-x,
						;         'rcv'       = {x},
						;         'tlv'       = {x, y},
						;         'flv'       = {u, v, w},
						;         'm1'       := binding,
						;         'm1!tlv.r' := u<-x',
						;         'm2'       does not exist.
						;         situation does not change and value := nil.
  (DECLARE (IGNORE OP.LINK SUBJECT.CLAUSE FROM.LITNO TO.LITNO))
  (LET ((ORENAMING (DS-CLAUSE.RENAMING OBJECT.CLAUSE))
	(OLD.BINDING (RDS-BINDING.TOP))
	BIND RCV.UNI SUCCESS)
						; all variables in the renaming bindings of 'rcv'
						; must be regarded as constants for computation of
						; 'm1'.
    (MEMBER-IF #'(LAMBDA (NEW.BINDING)
		   (RDS-BINDING.PUSH NEW.BINDING)
						; renaming all variables of 'to.literal.variables' in the bindings of
						; variables in 'from.literal.variables'.
		   (SETQ NEW.BINDING (COPY-TREE NEW.BINDING))
		   (MAPC #'(LAMBDA (VAR)
			     (COND
			       ((SETQ BIND (DT-VARIABLE.GET.BINDING VAR))
				(SMAPL
				  #'(LAMBDA (BIND.TAIL)
				      (WHEN (EQL VAR (CAR BIND.TAIL))
					(RPLACA (CDR BIND.TAIL) (UNI-APPLY.SUBSTITUTION ORENAMING BIND T))))
				  #'CDDR NEW.BINDING))))
			 FROM.LITERAL.VARIOABLES)
						; delete bindings of variables in 'tlv'.
		   (MAPC #'(LAMBDA (VAR)
			     (COND
			       ((DT-VARIABLE.GET.BINDING VAR)
				(SSOMEL #'(LAMBDA (BIND.TAIL) (COND ((EQL VAR (CAR BIND.TAIL)) (RPLACA (CDR BIND.TAIL) VAR))))
					#'CDDR
					NEW.BINDING)
				(SETQ NEW.BINDING (DELETE VAR NEW.BINDING)))))
			 TO.LITERAL.VARIABLES)
						; now current binding is 'm1!tlv.r'.
		   (RDS-BINDING.PUSH NEW.BINDING)
		   (SETQ RCV.UNI NIL)
		   (MAPC #'(LAMBDA (VAR)
			     (SETQ BIND (MEMBER VAR ORENAMING))
			     (SETQ RCV.UNI (CONS VAR (CONS (SECOND BIND) RCV.UNI))))
			 REST.CLAUSE.VARIABLES)
						; success is not nil iff 'm2' exists for the current
						; 'm1!tlv.r'.
		   (PROGN
		     (SETQ SUCCESS
			   (NOT
			     (EVERY
			       #'UNI-UNIFIER.IS.VARIABLE.RENAMING
			       (UNI-MERGE.LIST.OF.MATCHERLISTS.fit.on.sorts (LIST (LIST RCV.UNI) (LIST S.UNIFIER))
							       (DS-CLAUSE.VARIABLES OBJECT.CLAUSE) (RDS-BINDING.TOP)))))
						; old bindings are reset.
		     (RDS-BINDING.POP)
		     (COND
		       ((NOT SUCCESS)
						; removing 'm1!tlv.r' if no 'm2' exists for it.
			(RDS-BINDING.POP)))
		     SUCCESS))
	       (PROG2 (RDS-BINDING.PUSH NIL) (UNI-UNIFIER.BECOMES.MATCHER OLD.BINDING REST.CLAUSE.VARIABLES)
		      (RDS-BINDING.POP)))
    SUCCESS))

(DEFUN RED=CRR_REPLACE.LITERAL (TO.CLAUSE FROM.CLAUSE TO.LIT FROM.LIT S.LINK S.UNI)
						; edited: 14-apr-84 08:13:45
						; input:  'to.litno' describes a literal of clause
						;         'to.clause', 'from.litno' one of
						;         'from.clause'.
						;         's.link' is a s-link with unifier 's.uni'
						;         incident with these literals.
						; effect: replaces literal 'to.litno' by literal
						;         'from.litno'.
						;         predicate, sign, termlist, and properties
						;         are replaced, links inherited.
						;         variables of new termlist, that are not in
						;         'to.clause' are renamed by newly created
						;         ones.
						;         clause components 'variables' and 'renaming'
						;         are updated.
						; value:  list of incident clauses of removed links
						;         of 'to.litno'.
  (LET ((NEW.TERMLIST (UNI-APPLY.SUBSTITUTION S.UNI (DS-CLAUSE.TERMLIST FROM.CLAUSE FROM.LIT) T))
	NEWVARS NEWVARSRENAMING)
						; computing new variables.
    (SETQ NEWVARS (SET-DIFFERENCE (COPY-LIST (DT-TERMLIST.VARIABLES NEW.TERMLIST)) (DS-CLAUSE.VARIABLES TO.CLAUSE)))
    (SETQ NEWVARSRENAMING
	  (ZIP NEWVARS (MAPCAR #'(LAMBDA (VAR) (DT-VARIABLE.CREATE (DT-VARIABLE.SORT VAR))) NEWVARS) NIL))
						; renaming of new variables in new termlist.
    (SETQ NEW.TERMLIST (UNI-APPLY.SUBSTITUTION NEWVARSRENAMING NEW.TERMLIST NIL))
						; replacement.
    (PROG1 (red=ctl_REPLACE.LITERAL TO.CLAUSE TO.LIT (DS-CLAUSE.SIGN FROM.CLAUSE FROM.LIT)
				    (DS-CLAUSE.PREDICATE FROM.CLAUSE FROM.LIT) NEW.TERMLIST 'GENERALIZING (CONS S.LINK S.UNI))
						; removal and inheritance of links and properties.
	   (red=ctl_RECONSTRUCT NIL (LIST (LIST TO.CLAUSE TO.LIT))))))

(DEFUN RED=CRR_SIMPLE.CONDITION
       (OBJECT.CLAUSE OBJECT.MARKS OBJECT.LITNO ORLINKS SUBJECT.CLAUSE SUBJECT.LITNO)
						; edited:  7-jun-84 19:27:55
						; input:  'object.litno', 'object.clause', 'subject.-
						;         litno', and 'subject.clause' describe the
						;         two replacement resolution literals.
						;         'object.marks' and 'orlinks' are
						;         mark arrays of clause
						;         'object.clause', length of array =
						;         !'object.clause'!. cell i of 'object.marks'
						;         corresponds with literal i of
						;         'object.clause'. if cell i contains 't',
						;         'double' or 'false', literal is marked as
						;         removed.
						;         cell i of 'orlinks' contains r- and rd-links
						;         of literal i of 'object.clause' to unit
						;         clauses and literal 'subject.litno' of
						;         clause 'subject.clause'.
						; effect: double and false literals of 'object.clause'
						;         are marked as removed. if possible and
						;         necessary to get value 'not nil' for this
						;         function, two marks of literals in
						;         'object.marks' are exchanged.
						;         literals not replacement subsuming are tried
						;         to be removed by simulated operations on
						;         incident si-links or links in 'orlinks'.
						; value:  not nil iff variables of the not remove
						;         marked literals of 'object.clause' are
						;         renamed by the current binding. if there
						;         exist si- or sid-links of clause
						;         'object.clause', so that the exchange of the
						;         marks of the two parent literals of the link
						;         leads to the upper case, this will be
						;         considered.
  (RED=CRR_MARK.DOUBLE.LITERALS.INTERNAL OBJECT.CLAUSE OBJECT.MARKS T)
  (RED=CRR_MARK.FALSE.LITERALS OBJECT.CLAUSE OBJECT.MARKS)
  (RED=CRR_MARK.OBJECT.LITERALS OBJECT.CLAUSE OBJECT.LITNO OBJECT.MARKS ORLINKS SUBJECT.CLAUSE SUBJECT.LITNO)
  (RED=CRR_EXISTS.PERMUTATION.OF.DOUBLE.LITERALS OBJECT.CLAUSE OBJECT.MARKS))

(DEFUN RED=CRR_EXISTS.PERMUTATION.OF.DOUBLE.LITERALS (OBJECT.CLAUSE OBJECT.MARKS)
						; edited:  7-jun-84 20:40:00
						; input:  'object.marks' is a mark array of clause
						;         'object.clause' with length of array =
						;         number of literals of clause. cell i of
						;         'object.marks' corresponds with literal i
						;         of 'object.clause'. if a cell contains
						;         one of the atoms 't', 'double', and 'false',
						;         literal is remove marked.
						;         if there is a literal marked with
						;         'permuteable', a literal exists marked with
						;         'double' in 'object.clause', that is equal
						;         with the permuteable marked literal. on both
						;         literals the current binding is a variable
						;         renaming. the marks are exchangeable.
						;         after exchange, binding can be a variable
						;         renaming on all variables remaining in the
						;         clause 'object.clause'.
						; effect: exchange of such marks, if after exchange
						;         binding is a variable renaming as described
						;         above.
						; value:  not nil iff a permutation of marks exists
						;         as described, so that binding is a variable
						;         renaming.
  (PROG (VARIABLES OTHERLITNO SUCCESS)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
	  (COND
	    ((RED=CRR_MARKED.NOT.REMOVED OBJECT.MARKS (1+ RPTN))
						; collecting all variables of literals of
						; 'object.clause' currently marked as not removed.
	     (SETQ VARIABLES (UNION (COPY-LIST (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE (1+ RPTN))) VARIABLES)))))
	(COND
	  ((NOT (SETQ SUCCESS (RED.SERVICE-BINDING.RENAMES.VARIABLES VARIABLES)))
						; success = t: binding renames variables without
						; exchange of marks (deleting the other double
						; literal.
						; literal is marked with permuteable (exchangeable);
						; finding an adequate link to a literal for exchange.
	   (DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
	     (COND ((AND (EQL (RDS-MARK OBJECT.MARKS (1+ RPTN)) 'PERMUTEABLE)
			 (MEMBER-IF
			   #'(LAMBDA (NCI.LINKS)
			       (MEMBER-IF
				 #'(LAMBDA (NCI.LINK)
				     (COND ((AND (OR (EQL (1+ RPTN) (DS-LINK.POSLITNO NCI.LINK))
						     (NOT (RED.SERVICE-LINK.ORIENTED NCI.LINK)))
						 (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS NCI.LINK))
						 (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL NCI.LINK (1+ RPTN)))
						 (RED.SERVICE-BINDING.RENAMES.VARIABLES
						   (DS-CLAUSE.LIT.VARIABLES OBJECT.CLAUSE OTHERLITNO)))
						; the si- or sid-link with unifier nil,
						; permuteable marked parent; literal is positive
						; (after exchange remove marked) if link is oriented,
						; and binding is a variable renaming on the other
						; 'literal'.
					    (RDS-MARK.PUT OBJECT.MARKS OTHERLITNO NIL)
					    (RDS-MARK.PUT OBJECT.MARKS (1+ RPTN) 'DOUBLE)
						; exchange.
					    (COND
					      ((PROG1 (SETQ SUCCESS (RED=CRR_EXISTS.PERMUTATION.OF.DOUBLE.LITERALS
								      OBJECT.CLAUSE OBJECT.MARKS))
						; success = t: (binding renames variables) if
						; recursive call of 'red=crr_exists.permutation.of.-
						; double.literals' on the exchanged marks has success.
						      ))
					      (T	; back change if no success: take next permuteable
						; literal.
					       (RDS-MARK.PUT OBJECT.MARKS OTHERLITNO 'DOUBLE)
					       (RDS-MARK.PUT OBJECT.MARKS (1+ RPTN) 'PERMUTEABLE) NIL)))))
				 NCI.LINKS))
			   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL) OBJECT.CLAUSE
						     (1+ RPTN))))
						; termination of loop if renaming permutation is
						; found.
		    (SETQ RPTN 0))))))
	(RETURN SUCCESS)))

(DEFUN RED=CRR_PROTOCOL.RECONSTRUCT
       (OBJECT.CLAUSE OBJECT.LITNO OBJECT.MARKS SUBJECT.CLAUSE SUBJECT.LITNO SUBJECT.MARKS SUBJECT.VIRTUAL.SI.LINKS
	R.LINK)
						; edited:  7-jun-84 21:36:05
						; input:  'object.litno' is the number of a literal
						;         in 'object.clause', 'subject.litno' of
						;         'subject.clause'. 'object.marks' is a mark
						;         array of 'object.clause', 'subject.marks'
						;         one of 'subject.clause'. every cell of the
						;         arrays corresponds to a literal of the
						;         clauses and is 'double', 'false', or 't',
						;         iff the literal is removed after resolution
						;         between literal 'object.litno' and
						;         'subject.litno', and multiple
						;         factorizations.
						;         'subject.virtual.si.links'
						;         is an array containing
						;         s-links between the literals of
						;         'subject.clause' and 'object.clause'.
						; effect: -
						; value:  a list (unifier clauses resolutions
						;         multiples), where 'unifier' is the merge
						;         unifier of all operations applied to the
						;         set of clauses 'clauses'. 'resolutions'
						;         is a list of dotted pairs (literal_1 .
						;         literal_2) with literal_i is a dotted pair
						;         (clause_in_'clauses' . literal_number),
						;         denoting resolutions between the clauses.
						;         'multiples' describe the elimination of
						;         double literals after application of
						;         'unifier'. each element of the list
						;         'multiples' is a list of expressions
						;         (remaining_literal
						;         removed_literal rule_1 ... rule_n), where
						;         literals are dotted pairs (clause . number)
						;         and rule_i is a unification rule.
						;         first list is nil, the others are
						;         corresponding with elements of 'resolutions'
						;         in their order.
  (DECLARE (IGNORE  SUBJECT.VIRTUAL.SI.LINKS))
  (PROG
    ((OL.MARK (RDS-MARK OBJECT.MARKS OBJECT.LITNO)) RESOLUTIONS MULTIPLES
     CLAUSES FAILURE PATH RES.CLAUSE)
    ;; the old mark of the resolution literal 'object.-
    ;; litno' is saved in the variable 'ol.mark' and now
    ;; marked as removed with 't'. this
    ;; is necessary, because in generalizing case the label
    ;; of the object literal can be reset.
    (RDS-MARK.PUT OBJECT.MARKS OBJECT.LITNO T)
    ;; for all remove marked literals of 'subject.clause'
    ;; find the reason, i.e. the resolution unit for false
    ;; literals or the path of implications (si, sid, s) to
    ;; the next obtainable not remove marked literal.
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBJECT.CLAUSE))
      (CASE (RDS-MARK SUBJECT.MARKS (1+ RPTN))
	(DOUBLE	;; double literal: path of rules is constructed.
	  (COND
	    ((SETQ PATH
		   (RED=CRR_PROTOCOL.DOUBLE.LITERAL.PATH SUBJECT.CLAUSE (1+ RPTN)
							 SUBJECT.MARKS OBJECT.CLAUSE
							 OBJECT.MARKS NIL))
	     (SETQ MULTIPLES (CONS PATH MULTIPLES)))
	    (T (SETQ FAILURE T)
	       (SETQ RPTN 0))))
	(FALSE	;; false literal: clause for resolution is searched.
	  (COND
	    ((SETQ RES.CLAUSE (RED.SERVICE-FALSE.CLAUSE SUBJECT.CLAUSE (1+ RPTN) (RDS-BINDING.TOP)))
	     (push RES.CLAUSE CLAUSES)
	     (push (list (CONS SUBJECT.CLAUSE (1+ RPTN)) (CONS RES.CLAUSE 1) NIL) RESOLUTIONS))
	    (T (SETQ FAILURE T) (SETQ RPTN 0))))
	((T NIL PERMUTEABLE))
	(OTHERWISE
	  (PROGN	;; resolution with a unit clause.
	    (SETQ CLAUSES (CONS (CAAR (RDS-MARK SUBJECT.MARKS (1+ RPTN))) CLAUSES))
	    (push (cons (CONS SUBJECT.CLAUSE (1+ RPTN)) (RDS-MARK SUBJECT.MARKS (1+ RPTN))) RESOLUTIONS)))))
    (COND
      ((NOT FAILURE)
       ;; same for 'object.clause' if no failure in subject
       ;; case.
       (DODOWN (RPTN (DS-CLAUSE.NOLIT OBJECT.CLAUSE))
	 (CASE (RDS-MARK OBJECT.MARKS (1+ RPTN))
	   (DOUBLE
	     (COND
	       ((SETQ PATH
		      (RED=CRR_PROTOCOL.DOUBLE.LITERAL.PATH OBJECT.CLAUSE (1+ RPTN) OBJECT.MARKS SUBJECT.CLAUSE
							    SUBJECT.MARKS NIL))
		(SETQ MULTIPLES (CONS PATH MULTIPLES)))
	       (T (SETQ FAILURE T) (SETQ RPTN 0))))
	   (FALSE
	     (COND
	       ((SETQ RES.CLAUSE (RED.SERVICE-FALSE.CLAUSE OBJECT.CLAUSE (1+ RPTN) (RDS-BINDING.TOP)))
		(push RES.CLAUSE CLAUSES)
		(push (LIST (CONS OBJECT.CLAUSE (1+ RPTN)) (CONS RES.CLAUSE 1)) RESOLUTIONS))
	       (T (SETQ FAILURE T) (SETQ RPTN 0))))
	   ((T NIL PERMUTEABLE))
	   (OTHERWISE
	     (PROGN	;; resolution with a unit clause or a second time with
	       ;; the operation literal of the first resolution in
	       ;; 'subject.clause'.
	       ;;                  ---------------
	       ;; 'object.clause'  !   !   ! ... !
	       ;;                  ---------------
	       ;;                    ! __!
	       ;;                    !!
	       ;;                  -----------
	       ;; 'subject.clause' !   ! ... !
	       ;;                  -----------
	       (SETQ CLAUSES (CONS (CAAR (RDS-MARK OBJECT.MARKS (1+ RPTN))) CLAUSES))
	       (push (cons (CONS OBJECT.CLAUSE (1+ RPTN))
			   (RDS-MARK OBJECT.MARKS (1+ RPTN)))
		     RESOLUTIONS)))))))
    ;; reset of the mark of resolution literal
    ;; 'object.litno'.
    (RDS-MARK.PUT OBJECT.MARKS OBJECT.LITNO OL.MARK)
    (COND
      ((NOT FAILURE)
       ;; return of list. first element of 'clauses' is the
       ;; changed clause, first resolution is the
       ;; 'replacement resolution'.
       (RETURN
	 (LIST (RDS-BINDING.TOP)
	       (CONS OBJECT.CLAUSE (CONS SUBJECT.CLAUSE CLAUSES))
	       (SETQ RESOLUTIONS
		     (CONS
		       (LIST (CONS OBJECT.CLAUSE OBJECT.LITNO) (CONS SUBJECT.CLAUSE SUBJECT.LITNO) (DS-LINK.RULE R.LINK))
		       RESOLUTIONS))
	       (MAPCAR
		 #'(LAMBDA (RESOLUTION)
		     (COND ((EQL RESOLUTION (CAR RESOLUTIONS)) MULTIPLES)
			   ((EQL SUBJECT.CLAUSE (CAADR RESOLUTION))
			    (DREMAP (COPY-TREE MULTIPLES) NIL
				    #'(LAMBDA (MULTIPLES.TAIL) (EQL OBJECT.CLAUSE (CAADAR MULTIPLES.TAIL))) NIL))
			   ((EQL OBJECT.CLAUSE (CAADR RESOLUTION))
			    (DREMAP (COPY-TREE MULTIPLES) NIL
				    #'(LAMBDA (MULTIPLES.TAIL) (EQL SUBJECT.CLAUSE (CAADAR MULTIPLES.TAIL))) NIL))))
		 RESOLUTIONS)))))))

(DEFUN RED=CRR_PROTOCOL.DOUBLE.LITERAL.PATH (CLAUSE LITNO MARKS OTHERCLAUSE OTHERMARKS USED.LITS)
						; edited:  8-jun-84 16:53:54
						; input:  'marks' is a mark array of clause 'clause',
						;         'othermarks' one of clause 'otherclause'.
						;         length of mark array = length of its clause.
						;         cell i corresponds with literal i of the
						;         clause.
						;         'litno' is the number of a literal in clause
						;         'clause'.
						;         'used.lits' is a list with elements of the
						;         form (clause litno rule), where clause is
						;         'clause' or 'otherclause', rule is an
						;         unification rule.
						;         if a cell of the arrays contains a 'double',
						;         'false', or 't' the corresponding literal
						;         is marked as removed.
						;         there can exist bindings of variables.
						; effect: -
						; value:  list (('clause' . 'litno') (clause_x .
						;         litno_x) rule_1 ... rule_n), where clause_x
						;         and litno_x describe a literal reachable
						;         from our literal ('clause' . 'litno') about
						;         s-, si-, and sid-links with nil-unifiers
						;         and with the rules
						;         rule_1, ..., rule_n, not using literals
						;         'used.lits' in this path.
						;         literal x must be a not remove marked one.
						;         nil if no path exists.
  (PROG (RULE PATH OTHERPAR OTHERLITNO)
	(COND
	  ((MEMBER-IF
	     #'(LAMBDA (NC.LINKS)
		 (MEMBER-IF
		   #'(LAMBDA (NC.LINK)
		       (COND
			 ((AND (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS NC.LINK))
			       (OR (EQL LITNO (DS-LINK.POSLITNO NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
			  (SETQ OTHERPAR (DS-LINK.OTHERPAR NC.LINK CLAUSE))
			  (SETQ OTHERLITNO (DS-LINK.OTHERLITNO NC.LINK CLAUSE LITNO))
			  (COND
			    ((OR (AND (EQL OTHERPAR CLAUSE) (RED=CRR_MARKED.NOT.REMOVED MARKS OTHERLITNO))
				 (AND (EQL OTHERPAR OTHERCLAUSE) (RED=CRR_MARKED.NOT.REMOVED OTHERMARKS OTHERLITNO)))
			     (SETQ PATH
				   (LIST (CONS OTHERPAR OTHERLITNO) (CONS CLAUSE LITNO) (DS-LINK.RULE NC.LINK))))))))
		   NC.LINKS))
	     (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY) CLAUSE LITNO))
						; one of the obtainable literals is marked as not
						; removed.
	   )
	  ((MEMBER-IF
	     #'(LAMBDA (NC.LINKS)
		 (MEMBER-IF
		   #'(LAMBDA (NC.LINK)
		       (COND
			 ((AND (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS NC.LINK))
			       (OR (EQL LITNO (DS-LINK.POSLITNO NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
			  (SETQ RULE (DS-LINK.RULE NC.LINK)) (SETQ OTHERPAR (DS-LINK.OTHERPAR NC.LINK CLAUSE))
			  (SETQ OTHERLITNO (DS-LINK.OTHERLITNO NC.LINK CLAUSE LITNO))
			  (COND
			    ((AND (EQL OTHERPAR CLAUSE)
				  (NOT (MEMBER (CONS OTHERPAR OTHERLITNO) USED.LITS :TEST #'EQUAL)))
			     (SETQ PATH
				   (RED=CRR_PROTOCOL.DOUBLE.LITERAL.PATH CLAUSE OTHERLITNO MARKS OTHERCLAUSE
									 OTHERMARKS (CONS (CONS CLAUSE LITNO) USED.LITS))))
			    ((AND (EQL OTHERPAR OTHERCLAUSE)
				  (NOT (MEMBER (CONS OTHERPAR OTHERLITNO) USED.LITS :TEST #'EQUAL)))
			     (SETQ PATH
				   (RED=CRR_PROTOCOL.DOUBLE.LITERAL.PATH OTHERPAR OTHERLITNO OTHERMARKS CLAUSE
									 MARKS (CONS (CONS CLAUSE LITNO) USED.LITS))))))))
		   NC.LINKS))
	     (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY) CLAUSE LITNO))
						; for all remove marked literals it is searched a path
						; to not remove marked literals without usind the
						; literals used before in this path ('used.lits').
						; recursive call with 'expanded' 'used.lits'.
	   (NCONC1 PATH RULE)
	   (RPLACA (CDR PATH) (CONS CLAUSE LITNO))))
	(RETURN PATH)))