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

(DEFUN SEL=STR.INIT.MARK.RLINKS (RLINKS)
						; edited: 3. 6. 1982   hjo
						; input:  a list of rlinks
						; effect: activates and passivates the rlinks
						;         according to the selected resolution
						;         strategy.
  (SETQ RLINKS
	(REMOVE-IF-NOT
	  #'(LAMBDA (RLINK) (AND (NOT (DS-LINK.IS.MARKED PASSIVE RLINK)) (NOT (DS-LINK.IS.MARKED INHIBITED RLINK))))
	  RLINKS))
  (let (PASSIVE.LINKS (SUPPORTED.CLAUSES (SEL=CLAUSES SUPPORTED)))
    (CASE SEL*STR_RESOLUTION.STRATEGY
      (SET-OF-SUPPORT (SEL=STR.INIT.MARK.RLINKS.sos rlinks supported.clauses))
      ((BASIC-RESOLUTION all)
	(MAPC #'(LAMBDA (LINK) (SEL=INSERT.LINK LINK NIL ACTIVE) (DS-LINK.MARK ACTIVE LINK)) RLINKS))
      (UNIT-REFUTATION
	(MAPC #'(LAMBDA (LINK)
		  (COND ((OR (ds-clause.is.unit (DS-LINK.POSPAR LINK))
			     (ds-clause.is.unit (DS-LINK.NEGPAR LINK)))
			 (SEL=INSERT.LINK LINK NIL ACTIVE)
			 (DS-LINK.MARK ACTIVE LINK))
			(T (SEL=PASSIVATE LINK))))
	      RLINKS))
      (LINEAR
	(QCONC SEL*LINKS_RACTIVE
	       (SORT (REMOVE-IF-NOT
		       #'(LAMBDA (RLINK)
			   (COND ((SEL=STR.LINEAR.CONNECTED.TO.TOP.CLAUSE RLINK) (DS-LINK.MARK ACTIVE RLINK) T)
				 (T (DS-LINK.MARK PASSIVE RLINK)
				    (SETQ PASSIVE.LINKS (CONS RLINK PASSIVE.LINKS))
				    NIL)))
		       RLINKS)
		     #'(LAMBDA (LINK1 LINK2) (< (DS-LINK.NOLIT LINK1) (DS-LINK.NOLIT LINK2)))))
	(QCONC SEL*LINKS_RPASSIVE
	       (SORT PASSIVE.LINKS
		     #'(LAMBDA (LINK1 LINK2) (< (DS-LINK.NOLIT LINK1) (DS-LINK.NOLIT LINK2))))))
      (INDUCTION-SPECIAL
	(MAPC
	  #'(LAMBDA (LINK)
	      (COND
		((OR (MEMBER (DS-LINK.NEGPAR LINK) SUPPORTED.CLAUSES)
		     (MEMBER (DS-LINK.POSPAR LINK) SUPPORTED.CLAUSES))
		 (COND
		   ((OR (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.NEGPAR LINK))) (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.POSPAR LINK))))
		    (SEL=INSERT.LINK LINK T ACTIVE))
		   (T (SEL=INSERT.LINK LINK NIL ACTIVE)))
		 (DS-LINK.MARK ACTIVE LINK))
		(T (SEL=PASSIVATE LINK))))
	  RLINKS))
      (OTHERWISE
	(ERROR "sel=str.init.mark.links called but no valid strategy defined:: ~a"
	       SEL*STR_RESOLUTION.STRATEGY)))))

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

(DEFUN SEL=STR.MARK.SILINKS (SILINKS)
						; Edited:  05-NOV-1991 20:14
						; Authors: PRCKLN
						; Input:   A list of SI-Links
						; Effect:  Activates and passivates the silinks
						;          according to the selected resolution
						;          and paramodulation strategy.
						; Value:   Undefined
  (mapc #'(lambda (silink)
	    (when (AND (NOT (DS-LINK.IS.MARKED PASSIVE SILINK)) (NOT (DS-LINK.IS.MARKED INHIBITED SILINK)))
	      (CASE SEL*STR_paramodulation.STRATEGY
		(zhang-kapur (SEL=STR.MARK.SILINK.z.k silink))
		((snyder-lynch bachmair-ganzinger) (SEL=STR.MARK.SILINK.b.g silink)))))
	silinks))

(DEFUN SEL=STR.MARK.RLINKS (RLINKS)
						; edited: 3. 6. 1982   hjo
						; input:  a list of rlinks
						; effect: activates and passivates the rlinks
						;         according to the selected resolution
						;         strategy.
						;         possibly destroys the input list
  (SETQ RLINKS
	(DELETE-IF-NOT
	  #'(LAMBDA (RLINK) (AND (NOT (DS-LINK.IS.MARKED PASSIVE RLINK)) (NOT (DS-LINK.IS.MARKED INHIBITED RLINK))))
	  RLINKS))
  (CASE SEL*STR_RESOLUTION.STRATEGY
    (SET-OF-SUPPORT (SEL=STR.MARK.RLINKS.sos rlinks))
    ((BASIC-RESOLUTION all)
      (MAPC #'(LAMBDA (RLINK)
		(SEL=INSERT.LINK RLINK NIL ACTIVE)
		(DS-LINK.MARK ACTIVE RLINK))
	    RLINKS))
    (UNIT-REFUTATION
      (MAPC
        #'(LAMBDA (RLINK)
            (COND
              ((OR (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.POSPAR RLINK))) (EQL 1 (DS-CLAUSE.NOLIT (DS-LINK.NEGPAR RLINK))))
	       (SEL=INSERT.LINK RLINK NIL ACTIVE) (DS-LINK.MARK ACTIVE RLINK))
              (T (SEL=PASSIVATE RLINK))))
        RLINKS))
    (LINEAR
      (PROG ((LABEL (GET 'SEL=STRATEGIES 'SEL*LABEL))
	     (ACTIVE.CLAUSE (GET 'SEL=STRATEGIES 'SEL*ACTIVE.CLAUSE)))
	    (CASE LABEL
	      ((ACTIVE NIL)
	       (MAPC #'(LAMBDA (RLINK) (DS-LINK.MARK PASSIVE RLINK)) (SEL=LINKS R ACTIVE))
	       (COND ((SEL=LINKS R PASSIVE)
		      (RPLACA SEL*LINKS_RPASSIVE (NCONC (SEL=LINKS R ACTIVE) (SEL=LINKS R PASSIVE))))
		     (T (SETQ SEL*LINKS_RPASSIVE SEL*LINKS_RACTIVE)))
	       (SETQ SEL*LINKS_RACTIVE (LIST NIL))
	       (MAPC #'(LAMBDA (RLINK) (SEL=INSERT.LINK RLINK NIL ACTIVE) (DS-LINK.MARK ACTIVE RLINK))
		     (SORT RLINKS
			   #'(LAMBDA (LINK1 LINK2)
			       (COND ((> (DS-LINK.DEPTH LINK1) (DS-LINK.DEPTH LINK2)))
				     ((= (DS-LINK.DEPTH LINK1) (DS-LINK.DEPTH LINK2))
				      (< (DS-LINK.NOLIT LINK1) (DS-LINK.NOLIT LINK2))))))))
	      ((PASSIVE INHIBITED)
	       (MAPC #'(LAMBDA (RLINK)
			 (COND ((OR (= ACTIVE.CLAUSE (DS-LINK.POSPAR RLINK))
				    (= ACTIVE.CLAUSE (DS-LINK.NEGPAR RLINK)))
				(SEL=INSERT.LINK RLINK NIL ACTIVE)
				(DS-LINK.MARK ACTIVE RLINK))
			       (T (CASE LABEL
				    (PASSIVE (SEL=PASSIVATE RLINK))
				    (INHIBITED (SEL=INHIBIT RLINK))
				    (OTHERWISE (ERROR "illegal label in sel=str.mark.rlinks: ~a" LABEL))))))
		     RLINKS)
	       (SETQ SEL*LINKS_RACTIVE
		     (QCONC NIL
			    (SORT (SEL=LINKS R ACTIVE)
				  #'(LAMBDA (LINK1 LINK2) (< (DS-LINK.NOLIT LINK1) (DS-LINK.NOLIT LINK2)))))))
	      (OTHERWISE (ERROR "illegal label in sel=str.mark.rlinks: ~a" LABEL)))))
    (INDUCTION-SPECIAL
      (MAPC #'(LAMBDA (RLINK)
		(COND ((OR (MEMBER (DS-LINK.POSPAR RLINK) (SEL=CLAUSES SUPPORTED))
			   (MEMBER (DS-LINK.NEGPAR RLINK) (SEL=CLAUSES SUPPORTED)))
		       (COND ((OR (ds-clause.is.unit (DS-LINK.NEGPAR RLINK))
				  (ds-clause.is.unit (DS-LINK.POSPAR RLINK)))
			      (SEL=INSERT.LINK RLINK T ACTIVE))
			     (T (SEL=INSERT.LINK RLINK NIL ACTIVE))))
		      (T (SEL=PASSIVATE RLINK))))
	    RLINKS))
    (OTHERWISE
      (ERROR "sel=str.init.mark.links called but no valid strategy defined:: ~a" SEL*STR_RESOLUTION.STRATEGY))))

(DEFUN SEL=STR.INIT.MARK.PLINKS (PLINKS)
						; input:  a list of plinks
						; effect: classifies the plinks
						;         (see sel=str.mark.plink)
						; value:  undefined
  (SETQ PLINKS (REMOVE-IF-NOT #'(LAMBDA (PLINK)
				  (not (or (DS-LINK.IS.MARKED PASSIVE PLINK)
					   (DS-LINK.IS.MARKED INHIBITED PLINK)
					   (DS-LINK.IS.MARKED INHERITANCE.ONLY PLINK))))
			      PLINKS))
  (when (opt-is.completion) (MAPC #'SEL=STR.MARK.PLINK (cg-links 'piw all)))
  (CASE SEL*STR_RESOLUTION.STRATEGY
    ((BASIC-RESOLUTION SET-OF-SUPPORT UNIT-REFUTATION INDUCTION-SPECIAL all)
     (MAPC #'SEL=STR.MARK.PLINK PLINKS))
    (LINEAR (let ((PASSIVE.LINKS NIL))
	      (MAPC #'(LAMBDA (PLINK)
			(COND ((SEL=STR.LINEAR.CONNECTED.TO.TOP.CLAUSE PLINK) (SEL=STR.MARK.PLINK PLINK))
			      (T (SETQ PASSIVE.LINKS (CONS PLINK PASSIVE.LINKS)) (DS-LINK.MARK PASSIVE PLINK))))
		    PLINKS)
	      (QCONC SEL*LINKS_PPASSIVE
		     (SORT PASSIVE.LINKS
			   #'(LAMBDA (LINK1 LINK2) (< (DS-LINK.NOLIT LINK1) (DS-LINK.NOLIT LINK2)))))))
    (OTHERWISE
      (ERROR "invalid resolution strategy in sel=str.init.mark.plinks: ~a" SEL*STR_RESOLUTION.STRATEGY))))

(DEFUN SEL=STR.MARK.PLINKS (PLINKS &OPTIONAL ACTIVE.FLAG)
  (DECLARE (IGNORE ACTIVE.FLAG))
						; edited: 4. 6. 1982   hjo
						; input:  a list of plinks and a boolean value.
						; effect: the links are made active or passive.
						;         (see sel=str.mark.plink)
						; value:  undefined
  (SETQ PLINKS (REMOVE-IF-NOT #'(LAMBDA (PLINK)
				  (not (or (DS-LINK.IS.MARKED PASSIVE PLINK)
					   (DS-LINK.IS.MARKED INHIBITED PLINK)
					   (DS-LINK.IS.MARKED INHERITANCE.ONLY PLINK))))
			      PLINKS))
  (CASE SEL*STR_RESOLUTION.STRATEGY
    ((BASIC-RESOLUTION SET-OF-SUPPORT UNIT-REFUTATION INDUCTION-SPECIAL all)
     (MAPC #'SEL=STR.MARK.PLINK PLINKS))
    (LINEAR (let ((LABEL (GET 'SEL=STRATEGIES 'SEL*LABEL))
		  (ACTIVE.CLAUSE (GET 'SEL=STRATEGIES 'SEL*ACTIVE.CLAUSE)))
	      (CASE LABEL
		((ACTIVE NIL)
		 (MAPC #'(LAMBDA (PLINK) (DS-LINK.MARK PASSIVE PLINK)) (SEL=LINKS P ACTIVE))
		 (COND
		   ((SEL=LINKS P PASSIVE)
		    (RPLACA SEL*LINKS_PPASSIVE (NCONC (SEL=LINKS P ACTIVE) (SEL=LINKS P PASSIVE))))
		   (T (SETQ SEL*LINKS_PPASSIVE SEL*LINKS_PACTIVE)))
		 (SETQ SEL*LINKS_PACTIVE (LIST NIL))
		 (MAPC #'(LAMBDA (PLINK) (SEL=STR.MARK.PLINK PLINK T)) PLINKS))
		((PASSIVE INHIBITED)
		 (MAPC #'(LAMBDA (PLINK)
			   (COND ((OR (EQL ACTIVE.CLAUSE (DS-LINK.POSPAR PLINK)) (EQL ACTIVE.CLAUSE (DS-LINK.NEGPAR PLINK)))
				  (SEL=STR.MARK.PLINK PLINK T))
				 (T (CASE LABEL
				      (PASSIVE (SEL=PASSIVATE PLINK))
				      (INHIBITED (SEL=INHIBIT PLINK))
				      (OTHERWISE (ERROR "illegal label in sel=str.mark.plinks: ~a" LABEL))))))
		       PLINKS))
		(OTHERWISE (ERROR "illegal label in sel+str.mark.plinks : ~a" LABEL)))))
    (OTHERWISE (ERROR "invalid resolution strategy in sel=str.mark.plinks : ~a" SEL*STR_RESOLUTION.STRATEGY))))

#|(defun sel=p.look.ahead (plink)
  (unless (dt-getprop plink 'sel*resolvable)
    (dt-putprop plink 'sel*resolvable (red-p.look.ahead plink))))|#

(defun SEL=STR.MARK.PLINK.Z.K (plink frontflag)
  (let* ((negpar (DS-LINK.NEGPAR pLINK))
	 (neglitno (DS-LINK.neglitno pLINK))
	 (pospar (DS-LINK.posPAR pLINK))
	 (poslitno (DS-LINK.poslitno pLINK)))
    (unless (member plink (cg-links '(p piw) removed))
      (cond ((and (ds-clause.lit.is.max pospar poslitno)
		  (ds-clause.lit.is.max negpar neglitno))
	     (DS-LINK.MARK ACTIVE PLINK)
						;(sel=p.look.ahead plink) Old narrowing
	     (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
	    (T (SEL=PASSIVATE PLINK FRONTFLAG))))))

(defun SEL=STR.MARK.SILINK.z.k (silink)
  (let* ((par (DS-LINK.NEGPAR silink))
	 (neglitno (DS-LINK.neglitno silink))
	 (poslitno (DS-LINK.poslitno silink)))
    (if (and (ds-clause.lit.is.max par poslitno)
	     (ds-clause.lit.is.max par neglitno))
	(DS-LINK.MARK ACTIVE siLINK)
	(DS-LINK.MARK PASSIVE siLINK))))

(defun sel=clause.literal (clause litno uni)
  (dt-term_create (ds-clause.predicate clause litno)
		  (uni-apply.substitution uni (ds-clause.termlist clause litno) t)))

(defun sel=str.mark.plink.b.g.split (clause litno1 unifier)
  (let (pos neg lit)
    (ds-clause.do #'(lambda (litno2)
		      (cond ((= litno1 litno2) (setq lit (sel=clause.literal clause litno1 unifier)))
			    ((ds-sign.is.positive (ds-clause.sign clause litno2))
			     (push (sel=clause.literal clause litno2 unifier) pos))
			    (t (push (sel=clause.literal clause litno2 unifier) neg))))
		  clause)
    (values pos neg lit (ds-sign.is.positive (ds-clause.sign clause litno1)))))

(defun sel=str.mark.plink.b.g.split.sibling (clause litno1 sibling.litno unifier)
  (let (pos neg lit)
    (ds-clause.do #'(lambda (litno2)
		      (cond ((= litno1 litno2) (setq lit (sel=clause.literal clause litno1 unifier)))
			    ((= sibling.litno litno2))
			    ((ds-sign.is.positive (ds-clause.sign clause litno2))
			     (push (sel=clause.literal clause litno2 unifier) pos))
			    (t (push (sel=clause.literal clause litno2 unifier) neg))))
		  clause)
    (values pos neg lit (ds-sign.is.positive (ds-clause.sign clause litno1)))))

(defun sel=str.mark.plink.b.g.sibling (clause litno unifier)
						; Edited:  31-OCT-1991 19:40
						; Authors: PRCKLN
						; Input:   Specifies a positive equality literal
						; Effect:  -
						; Value:   1. Another litno
						;          2. A unifier
  (let* ((rule (ds-clause.lit.rewrite.rule clause litno))
	 t1 t2
	 (tl (ds-clause.termlist clause litno))
	 right sibling.litno sibling.uni)
    (cond (rule (setq right (dt-access (DT-TAF.OTHERSIDE rule) tl)))
	  ((ord-greater (setq t1 (uni-apply.substitution unifier (dt-access (DT-TAF.CREATE.LEFT) tl) t))
			(setq t2 (uni-apply.substitution unifier (dt-access (DT-TAF.CREATE.right) tl) t)))
	   (setq right t2))
	  ((ord-greater t2 t1) (setq right t1)))
    (when right
      (ds-clause.some #'(lambda (slitno)
			  (and (/= slitno litno)
			       (ds-clause.pos.equation clause slitno)
			       (let ((stl (ds-clause.termlist clause slitno)) suni)
				 (when
				   (or (and (setq suni (first (uni-merge.substitutions
								unifier
								(first (uni-unify.terms (uni-apply.substitution unifier
														(first stl) t)
											right)))))
					    (not (ord-greater (uni-apply.substitution suni (second stl) t)
							      (uni-apply.substitution suni right t))))
				       (and (setq suni (first (uni-merge.substitutions
								unifier
								(first (uni-unify.terms (uni-apply.substitution unifier
														(second stl) t)
											right)))))
					    (not (ord-greater (uni-apply.substitution suni (first stl) t)
							      (uni-apply.substitution suni right t)))))
				   (setq sibling.uni suni sibling.litno slitno)))))
		      clause)
      (values sibling.litno sibling.uni))))

(defun sel=str.mark.plink.b.g.strict.max.p (lit1 lit.list)
  (every #'(lambda (lit2) (and (not (equal lit1 lit2)) (not (ord-greater lit2 lit1))))
	 lit.list))

(defun sel=str.mark.plink.b.g.max.p (lit1 lit.list)
  (every #'(lambda (lit2) (not (ord-greater lit2 lit1)))
	 lit.list))

(defun sel=str.mark.plink.b.g.reductive (lit llist neg.list fct)
  (let ((left (dt-access fct (rest lit)))
	(right (dt-access (dt-taf.otherside fct) (rest lit))))
    (and (sel=str.mark.plink.b.g.strict.max.p lit llist)
	 (or (null left)
	     (and (not (ord-greater right left))       
		  (not (inside left neg.list)))))))

(defun sel=str.mark.plink.b.g.strict.p (pclause nclause lclause posclause.p peq neq leq poseq.p fcteq)
						; Edited:  06-NOV-1991 22:21
						; Authors: PRCKLN
						; Input:   PCLAUSE are the positive literals of the paramodulated
						;          clause as terms, i.e. without sign.
						;          NCLAUSE are the negative literals.
						;          LCLAUSE is the paramodulated literal as term without sign.
						;          It is not contained in PCLAUSE and NCLAUSE.
						;          POSCLAUSE.P is true iff LCLAUSE is a positive literal.
						;          The EQ parameters are those for the paramodulating clause,
						;          respectively.
						; Effect:  None.
						; Value:   True if the link is strict relative to the strategy of Bachmair
						;          and Ganzinger
  (declare (ignore poseq.p))
  #|(format t "~%Pos of clause: ~A" (ds-pname pclause))
  (format t "~%Neg of clause: ~A" (ds-pname nclause))
  (format t "~%Lit of clause: ~A" (ds-pname lclause))
  (format t "~%Pos of equation: ~A" (ds-pname peq))
  (format t "~%Neg of equation: ~A" (ds-pname neq))
  (format t "~%Lit of equation: ~A" (ds-pname leq))|#
  (and (sel=str.mark.plink.b.g.reductive leq (append peq neq) neq fcteq)
       (if posclause.p
	   (sel=str.mark.plink.b.g.strict.max.p lclause (append pclause nclause))
	   (and (sel=str.mark.plink.b.g.max.p lclause nclause)
		(sel=str.mark.plink.b.g.strict.max.p lclause pclause)))
       t))

(defun sel=str.mark.plink.b.g.merge.p (pclause nclause lclause posclause.p peq neq leq poseq.p fcteq)
  (declare (ignore posclause.p poseq.p))
  (and (sel=str.mark.plink.b.g.reductive leq (append peq neq) neq fcteq)
       (sel=str.mark.plink.b.g.reductive lclause (append pclause nclause) nclause fcteq)))

(defun SEL=STR.MARK.SILINK.B.G (silink)
  (let* ((unifier (first (ds-link.unifiers silink)))
	 (par (DS-LINK.NEGPAR silink))
	 (neglitno (DS-LINK.neglitno silink))
	 (poslitno (DS-LINK.poslitno silink)))
    (if (ds-sign.is.positive (ds-clause.sign par poslitno))
	(multiple-value-bind (pclause nclause literal)
	    (sel=str.mark.plink.b.g.split par neglitno unifier)
	  (declare (ignore nclause))
	  (if (sel=str.mark.plink.b.g.max.p literal pclause)
	      (DS-LINK.MARK ACTIVE siLINK)
	      (DS-LINK.MARK PASSIVE siLINK)))
	(DS-LINK.MARK PASSIVE siLINK))))

(defun SEL=STR.MARK.rLINK.B.G.is (rlink)
						; Edited:  09-NOV-1991 18:22
						; Authors: PRCKLN
						; Input:   An R-link
						; Effect:  None.
						; Value:   True iff the link active or passive according to the
						;          strategy of Bachmair and Ganzinger.
  (let* ((unifier (first (ds-link.unifiers rlink)))
	 (negpar (DS-LINK.NEGPAR rlink))
	 (neglitno (DS-LINK.neglitno rlink))
	 (pospar (DS-LINK.posPAR rlink))
	 (poslitno (DS-LINK.poslitno rlink))
	 (posfct (ds-link.posfct rlink)))
    (multiple-value-call #'sel=str.mark.plink.b.g.strict.p
			 (sel=str.mark.plink.b.g.split negpar neglitno unifier)
			 (sel=str.mark.plink.b.g.split pospar poslitno unifier)
			 posfct)))

(defun SEL=STR.MARK.rLINK.B.G (rlink)
						; Edited:  09-NOV-1991 18:21
						; Authors: PRCKLN
						; Input:   An R-link
						; Effect:  Labels the link active or passive according to the
						;          strategy of Bachmair and Ganzinger.
						; Value:   Undefined.
  (cond ((SEL=STR.MARK.rLINK.B.G.is rlink)
	 (SEL=INSERT.LINK rLINK nil ACTIVE)
	 (DS-LINK.MARK ACTIVE rLINK))
	(T (SEL=PASSIVATE rLINK nil))))

(defun SEL=STR.MARK.PLINK.B.G.is (plink)
						; Edited:  09-NOV-1991 18:22
						; Authors: PRCKLN
						; Input:   A P-link.
						; Effect:  None.
						; Value:   True iff the link active or passive according to the
						;          strategy of Bachmair and Ganzinger.
  (let* ((unifier (first (ds-link.unifiers plink)))
	 (negpar (DS-LINK.NEGPAR pLINK))
	 (neglitno (DS-LINK.neglitno pLINK))
	 (pospar (DS-LINK.posPAR pLINK))
	 (poslitno (DS-LINK.poslitno pLINK))
	 (posfct (ds-link.posfct plink)))
    (or (multiple-value-call #'sel=str.mark.plink.b.g.strict.p
			     (sel=str.mark.plink.b.g.split negpar neglitno unifier)
			     (sel=str.mark.plink.b.g.split pospar poslitno unifier)
			     posfct)
	(and (ds-clause.pos.equation negpar neglitno)		    
	     (multiple-value-bind (sibling.litno sibling.uni)
		 (sel=str.mark.plink.b.g.sibling negpar neglitno unifier)
	       (and sibling.litno
		    (multiple-value-call #'sel=str.mark.plink.b.g.merge.p
					 (sel=str.mark.plink.b.g.split.sibling
					   negpar neglitno sibling.litno sibling.uni)
					 (sel=str.mark.plink.b.g.split pospar poslitno unifier)
					 posfct)))))))

(defun SEL=STR.MARK.PLINK.B.G (plink frontflag)
						; Edited:  09-NOV-1991 18:21
						; Authors: PRCKLN
						; Input:   A P-link.
						; Effect:  Labels the link active or passive according to the
						;          strategy of Bachmair and Ganzinger.
						; Value:   Undefined.
  (cond ((SEL=STR.MARK.PLINK.B.G.is plink)
	 (DS-LINK.MARK ACTIVE PLINK)
	 (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
	(T (SEL=PASSIVATE PLINK FRONTFLAG))))

(defun SEL=STR.MARK.PLINK.H.C (plink frontflag)
  (let* ((supported.clauses (sel=clauses supported))
	 (negpar (DS-LINK.NEGPAR pLINK))
	 (neglitno (DS-LINK.neglitno pLINK))
	 (negnolit (ds-clause.nolit negpar))
	 (negterm (ds-link.negterm plink))
	 (pospar (DS-LINK.posPAR pLINK))
	 (poslitno (DS-LINK.poslitno pLINK))
	 (posnolit (ds-clause.nolit pospar))
	 #|(posterm (ds-link.posterm plink))|#)
    (cond ((ds-link.demodulation.is plink)
	   (DS-LINK.MARK ACTIVE PLINK)
	   (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
	  ((or (let* ((rrpos? (ds-clause.lit.rewrite.rule pospar poslitno))
		      (rrneg? (ds-clause.lit.rewrite.rule negpar neglitno))
		      (rr (if rrpos? pospar (if rrneg? negpar)))
		      (rrfct (if rrpos?
				 (ds-link.posfct plink)
				 (if rrneg? (ds-link.negfct plink)))))
		 (and rr
		      (if (and rrpos? rrneg?)
			  (and (equal (ds-link.posfct plink) (list rrpos?))
			       (= (first (ds-link.negfct plink)) rrneg?))
			  (equal rrfct (list (or rrpos? rrneg?))))))
	       (or (and (ds-clause.lit.rewrite.rule pospar poslitno)
			(not (or (ds-clause.lit.is.unfailing negpar neglitno)
				 (ds-clause.lit.rewrite.rule negpar neglitno))))
		   (and (ds-clause.lit.is.unfailing negpar neglitno)
			(not (or (ds-clause.lit.is.unfailing pospar poslitno)
				 (ds-clause.lit.rewrite.rule pospar poslitno)))))
	       (or (and (ds-clause.lit.rewrite.rule negpar neglitno)
			(not (or (ds-clause.lit.is.unfailing pospar poslitno)
				 (ds-clause.lit.rewrite.rule pospar poslitno))))
		   (and (ds-clause.lit.is.unfailing pospar poslitno)
			(not (or (ds-clause.lit.is.unfailing negpar neglitno)
				 (ds-clause.lit.rewrite.rule negpar neglitno)))))
	       (and (ds-clause.lit.is.unfailing pospar poslitno)
		    (ds-clause.lit.is.unfailing negpar neglitno)))
	   (DS-LINK.MARK ACTIVE PLINK)
	   (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
	  ((and (MEMBER NEGPAR SUPPORTED.CLAUSES) (MEMBER POSPAR SUPPORTED.CLAUSES))
	   (cond ((or (and (equal (list nil) (ds-link.unifiers plink))
			   (eql 1 negnolit)
			   (eql 1 posnolit))
		      (and (dt-constant.is negterm)
			   (dt-constant.is.skolem negterm)
			   (eql 1 negnolit)
			   (eql 1 posnolit)))
		  (DS-LINK.MARK ACTIVE PLINK)
		  (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
		 ((ds-link.demodulation.is plink)
		  (DS-LINK.MARK ACTIVE PLINK)
		  (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
		 (T (SEL=PASSIVATE PLINK FRONTFLAG))))
	  (T (SEL=PASSIVATE PLINK FRONTFLAG)))))

(defun SEL=STR.MARK.PLINK.s.l.is (plink)
						; Edited:  09-NOV-1991 18:22
						; Authors: PRCKLN
						; Input:   An R-link
						; Effect:  None.
						; Value:   True iff the link is basic according to the
						;          strategy of Bachmair and Ganzinger.
  (not (some #'(lambda (pos2) (DT-TAF.DEEPER.OR.EQUAL (ds-link.negfct plink) pos2))
	     (ds-clause.passive.positions (ds-link.negpar plink) (ds-link.neglitno plink)))))

(defun SEL=STR.MARK.PLINK.S.L (plink frontflag)
						; Edited:  09-NOV-1991 18:21
						; Authors: PRCKLN
						; Input:   A P-link.
						; Effect:  Labels the link active or passive according to the
						;          basic strategy of Snyder and Lynch.
						; Value:   Undefined.
  (cond ((and (SEL=STR.MARK.PLINK.B.G.is plink)
	      (SEL=STR.MARK.PLINK.S.L.is plink))
	 (DS-LINK.MARK ACTIVE PLINK)
	 (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
	(T (SEL=PASSIVATE PLINK FRONTFLAG))))

(defun SEL=STR.MARK.PLINK.d.is (plink)
						; Edited:  14-DEC-1991 13:47
						; Authors: PRCKLN
						; Input:   An R-link
						; Effect:  None.
						; Value:   True iff the link is basic according to the
						;          strategy of Bachmair and Ganzinger.
  (let* ((unifier (first (ds-link.unifiers plink)))
	 (negpar (DS-LINK.NEGPAR pLINK))
	 (neglitno (DS-LINK.neglitno pLINK))
	 (pospar (DS-LINK.posPAR pLINK))
	 (poslitno (DS-LINK.poslitno pLINK))
	 (posfct (ds-link.posfct plink)))
    (or (and (DS-CLAUSE.IS.EQUATION negpar neglitno)
	     (ds-clause.is.unit negpar)
	     (DS-CLAUSE.IS.EQUATION pospar poslitno)
	     (ds-clause.is.unit pospar)
	     (multiple-value-call #'sel=str.mark.plink.b.g.strict.p
				  (sel=str.mark.plink.b.g.split negpar neglitno unifier)
				  (sel=str.mark.plink.b.g.split pospar poslitno unifier)
				  posfct))
	(and (DS-CLAUSE.IS.EQUATION pospar poslitno)
	     (ds-clause.is.unit pospar)
	     (ds-sign.is.negative (ds-clause.sign negpar neglitno))
	     (multiple-value-call #'sel=str.mark.plink.b.g.strict.p
				  (sel=str.mark.plink.b.g.split negpar neglitno unifier)
				  (sel=str.mark.plink.b.g.split pospar poslitno unifier)
				  posfct)))))

(defun SEL=STR.MARK.PLINK.d (plink frontflag)
						; Edited:  14-DEC-1991 13:46
						; Authors: PRCKLN
						; Input:   A P-link.
						; Effect:  Labels the link active or passive according to the
						;          basic strategy of Snyder and Lynch.
						; Value:   Undefined.
  (cond ((SEL=STR.MARK.PLINK.d.is plink)
	 (DS-LINK.MARK ACTIVE PLINK)
	 (SEL=INSERT.LINK PLINK FRONTFLAG ACTIVE))
	(T (SEL=PASSIVATE PLINK FRONTFLAG))))

(DEFUN SEL=STR.MARK.PLINK (PLINK &OPTIONAL FRONTFLAG)
						; Edited:  30-OCT-1991 00:43
						; Authors: PRCKLN hjo
						; input:   a p-link and a boolean value.
						; effect:  the link is marked according to the
						;         selected strategy.
						;         none:    the link is passivated.
						;         rewrite: if the link is connected to a
						;         none unit clause, the link is passivated.
						;         if there exists a one way unifier and the
						;         term-complexity will be reduced, the link
						;         is activated.
						;         unit-ancestry:
						;         if both parents are unit clauses and the
						;         ancestors of the negative parent had never
						;         been paramodulated by the same equality, the
						;         link is activated.
  (COND ((and (not (opt-is.completion))
	      (NEQ 1 (DS-CLAUSE.NOLIT (DS-LINK.POSPAR PLINK))))
	 (SEL=PASSIVATE PLINK FRONTFLAG))
	(T (CASE SEL*STR_PARAMODULATION.STRATEGY
	     ((clause-graph heuristic-completion) (SEL=STR.MARK.PLINK.H.C plink frontflag))
	     (zhang-kapur (SEL=STR.MARK.PLINK.Z.K plink frontflag))
	     (bachmair-ganzinger (SEL=STR.MARK.PLINK.b.g plink frontflag))
	     (snyder-lynch (sel=str.mark.plink.s.l plink frontflag))
	     (dershowitz (sel=str.mark.plink.d plink frontflag))
	     (OTHERWISE (ERROR "unknown paramodulation strategy in sel=str.mark.plink: ~a"
			       SEL*STR_PARAMODULATION.STRATEGY))))))


(defun sel=mark.update.s.l (clause)
  (MAPC #'(LAMBDA (rLINK)
	    (if (SEL=STR.MARK.rLINK.B.G.is rlink)
		(COND ((member rlink (sel=links r passive))
		       (QDELETE SEL*LINKS_Rpassive rLINK)
		       (qconc1 SEL*LINKS_RACTIVE rLINK)))
		(COND ((member rlink (sel=links r active))
		       (QDELETE SEL*LINKS_Ractive rLINK)
		       (qconc1 SEL*LINKS_Rpassive rLINK)))))
	(ds-clause.all.links 'r clause))
  (MAPC #'(LAMBDA (pLINK)
	    (if (and (SEL=STR.MARK.PLINK.B.G.is plink)
		     (SEL=STR.MARK.PLINK.S.L.is plink))
		(COND ((member plink (sel=links p passive))
		       (QDELETE SEL*LINKS_Ppassive pLINK)
		       (qconc1 SEL*LINKS_PACTIVE pLINK)))
		(COND ((member plink (sel=links p active))
		       (QDELETE SEL*LINKS_Pactive pLINK)
		       (qconc1 SEL*LINKS_Ppassive pLINK)))))
	(ds-clause.all.links '(p piw) clause)))

(defun sel=mark.update.d (clause)
  (MAPC #'(LAMBDA (rLINK)
	    (if (SEL=STR.MARK.pLINK.d.is rlink)
		(COND ((member rlink (sel=links r passive))
		       (QDELETE SEL*LINKS_Rpassive rLINK)
		       (qconc1 SEL*LINKS_RACTIVE rLINK)))
		(COND ((member rlink (sel=links r active))
		       (QDELETE SEL*LINKS_Ractive rLINK)
		       (qconc1 SEL*LINKS_Rpassive rLINK)))))
	(ds-clause.all.links 'r clause))
  (MAPC #'(LAMBDA (pLINK)
	    (if (SEL=STR.MARK.pLINK.d.is plink)
		(COND ((member plink (sel=links p passive))
		       (QDELETE SEL*LINKS_Ppassive pLINK)
		       (qconc1 SEL*LINKS_PACTIVE pLINK)))
		(COND ((member plink (sel=links p active))
		       (QDELETE SEL*LINKS_Pactive pLINK)
		       (qconc1 SEL*LINKS_Ppassive pLINK)))))
	(ds-clause.all.links '(p piw) clause)))

(defun sel=mark.update.b.g (clause)
  (MAPC #'(LAMBDA (rLINK)
	    (if (SEL=STR.MARK.rLINK.B.G.is rlink)
		(COND ((member rlink (sel=links r passive))
		       (QDELETE SEL*LINKS_Rpassive rLINK)
		       (qconc1 SEL*LINKS_RACTIVE rLINK)))
		(COND ((member rlink (sel=links r active))
		       (QDELETE SEL*LINKS_Ractive rLINK)
		       (qconc1 SEL*LINKS_Rpassive rLINK)))))
	(ds-clause.all.links 'r clause))
  (MAPC #'(LAMBDA (pLINK)
	    (if (SEL=STR.MARK.pLINK.B.G.is plink)
		(COND ((member plink (sel=links p passive))
		       (QDELETE SEL*LINKS_Ppassive pLINK)
		       (qconc1 SEL*LINKS_PACTIVE pLINK)))
		(COND ((member plink (sel=links p active))
		       (QDELETE SEL*LINKS_Pactive pLINK)
		       (qconc1 SEL*LINKS_Ppassive pLINK)))))
	(ds-clause.all.links '(p piw) clause)))

(defun sel=mark.update.z.k (clause)
  (MAPC #'(LAMBDA (rLINK)
	    (if (and (ds-clause.lit.is.max (DS-LINK.NEGPAR rLINK) (DS-LINK.NEGlitno rLINK))
		     (ds-clause.lit.is.max (DS-LINK.POSPAR rLINK) (DS-LINK.POSlitno rLINK)))
		(COND ((member rlink (sel=links r passive))
		       (QDELETE SEL*LINKS_Rpassive rLINK)
		       (qconc1 SEL*LINKS_RACTIVE rLINK)))
		(COND ((member rlink (sel=links r active))
		       (QDELETE SEL*LINKS_Ractive rLINK)
		       (qconc1 SEL*LINKS_Rpassive rLINK)))))
	(ds-clause.all.links 'r clause))
  (MAPC #'(LAMBDA (pLINK)
	    (when (not (member plink (cg-links '(piw p) removed)))
	      (sel=insert.link plink t active)))
	(ds-clause.all.links '(p piw) clause)))

(defun sel=mark.update.h.c (clause)   
  (unless (member clause (sel=clauses supported))
    (SEL=INSERT.CLAUSE CLAUSE NIL SUPPORTED)
    (MAPC #'(LAMBDA (rLINK) (COND ((member rlink (sel=links r passive))
				   (QDELETE SEL*LINKS_Rpassive rLINK)
				   (qconc1 SEL*LINKS_RACTIVE rLINK))))
	  (ds-clause.all.links 'r clause))
    (MAPC #'(LAMBDA (pLINK) (COND ((member plink (sel=links p passive))
				   (QDELETE SEL*LINKS_Ppassive pLINK)
				   (qconc1 SEL*LINKS_PACTIVE pLINK))))
	  (ds-clause.all.links '(p piw) clause))))