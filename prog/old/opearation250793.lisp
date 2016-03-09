;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

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



(defun OP-RESET NIL
						; EDITED: 2-APR-82 13:42:48")
						; EFFECT: INITIALIZES THE COMMON-VARIABLES OF THE
						;         OPERATION MODULE.
						; VALUE:  UNDEFINED
  (SETQ OP*CLAUSECOUNTER 0)
  (SETQ OP*COUNTER.RESOLVENTS 0)
  (SETQ OP*COUNTER.PARAMODULANTS 0)
  (SETQ OP*COUNTER.FACTORS 0)
  (SETQ OP*COUNTER.instances 0)
  (SETQ OP*UNIFIER.BUFFER (BUFFER.CREATE 10))
  (SETQ OP*VARIABLE.BUFFER1 (BUFFER.CREATE 10))
  (SETQ OP*VARIABLE.BUFFER2 (BUFFER.CREATE 10)))

(DEFUN OP-SAVE (FILE)
						; EDITED:  1. 3. 1982   HJO
						; INPUT:   EITHER NIL OR THE NAME OF A FILE WHICH IS
						;          OPEN FOR OUTPUT.
						; EFFECT:  COMPUTES AN S-EXPRESSION REPRESENTING THE
						;          ACTUAL GRAPH SUCH THAT (EVAL S-EXPRESSION)
						;          CREATES THE VERY SAME GRAPH AS IT IS
						;          DURING INVOCATION OF THIS FUNCTION.
						;          IF FILE <> NIL, THE S-EXPRESSION IS WRITTEN
						;          ON FILE, OTHERWISE THE S-EXPRESSION IS THE
						;          VALUE OF THIS FUNCTION.
						; VALUE:   IF FILE = NIL THEN THE COMPUTED
						;          S-EXPRESSION, ELSE NIL.
  (let ((S-EXPRESSION `(PROGN (c "Operation save expression.")
			      ,@(MAPCAR #'(LAMBDA (ATOM) `(setq ,atom ',(symbol-value atom)))
					'(OP*LINK.COLOURS.LITERAL.INITIAL
					   OP*CLAUSECOUNTER
					   OP*COUNTER.RESOLVENTS
					   OP*COUNTER.PARAMODULANTS
					   OP*COUNTER.FACTORS
					   OP*COLOURS.CIRCLE.LINKS)))))
    (COND (FILE (CG-VIRTUAL.GRAPH FILE)
		(PROGN (PRIN1 S-EXPRESSION FILE) (TERPRI FILE))
		nil)
	  (T    `(PROGN ,(CG-VIRTUAL.GRAPH FILE) ,S-EXPRESSION)))))

(DEFUN OP-#RESOLVENTS NIL
						; EDITED: 5-APR-80 18:23:16")
						; VALUE:  NUMBER OF RESOLVENTS THAT HAVE BEEN CREATED
						;         SINCE GRAPH CONSTRUCTION.
  OP*COUNTER.RESOLVENTS)

(DEFUN OP-#PARAMODULANTS NIL
						; EDITED: 5-APR-80 18:24:44")
						; VALUE:  NUMBER OF PARAMODULANTS THAT HAVE BEEN
						;         CREATED SINCE GRAPH CONSTRUCTION.
  OP*COUNTER.PARAMODULANTS)

(DEFUN OP-#FACTORS NIL
						; EDITED: 5-APR-80 18:23:45")
						; VALUE:  NUMBER OF FACTORS THAT HAVE BEEN CREATED
						;         SINCE GRAPH CONSTRUCTION.
  OP*COUNTER.FACTORS)


(defun op=replace (var ren)
  (smapl #'(lambda (renrest)
	    (let ((dom (first renrest))
		  (cod (second renrest)))
	      (nsubst cod dom (dt-variable.sort var))))
	 #'cddr
	 ren))

(defun op=renamings (posvars negvars allvars)
						; Edited:  08-MAR-1991 21:55
						; Authors: PRCKLN
						; Input:   POSVARS are all variables in the pos parent of the new clause.
						;          NEGVARS are all variables in the neg parent of the new clause.
						;          ALLVARS are the variables really occurring in the new clause.
						; Effect:  -
						; Value:   1. A renaming for POSVARS
						;          2. A renaming for NEGVARS
						;          3. The renamings combined with additional renamings for the
						;             newly introduced variables.
  (let* ((posren (DT-VARIABLE.RENAMING.SUBSTITUTION posvars))
	 (negren (DT-VARIABLE.RENAMING.SUBSTITUTION negvars))
	 (REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION (set-difference (set-difference allvars posvars) negvars)))
	 (allren (append posren negren ren.subst)))
    (when (opt-get.option sort_literals)
      (smapc #'(lambda (term)
		 (op=replace term allren))
	     #'cddr
	     (rest allren)))
    (values posren negren allren)))
	      

(defun op=create.element.literals (link unifier)
						; Edited:  19-NOV-1991 22:23
						; Authors: PRCKLN
						; Input:   A LINK and its UNIFIER.
						; Effect:  -
						; Value:   The list of residue literals of LINK with this unifier.
  (declare (ignore unifier))
  (ds-link.sort.residue link)
  #|(smaplist #'(lambda (rest.uni)
		(ds-lit.create '- (DT-PREDICATE.element) (list (second rest.uni) (dt-variable.sort (first rest.uni)))))
	    #'cddr
	    unifier)|#)

(DEFUN OP-RESOLVE (LINK UNIFIER &optional complexity list.result)
						; Edited:  14-AUG-1990 23:08
						; Authors: PRCKLN SB
						; INPUT:  LINK IS AN RLINK OF THE ACTUAL GRAPH.
						;         UNIFIER IS A UNIFIER OF LINK.
						;         LINK MAY ALSO BE AN RIW-LINK OF A TWO
						;         LITERAL CLAUSE WITH NO RULE-LINKS
						;         COMPLEXITY: see OP-PARAMODULATE
						; EFFECT: THE RESOLVENT OF LINK AND ITS UNIT FACTORS
						;         ARE CREATED AND ADDED TO THE GRAPH.
						; VALUE:  NAME OF THE RESOLVENT OR NIL (TAUTOLOGY)
 (CASE (DS-LINK.COLOUR LINK)
    (R (let (nonolits
	     (NEGPAR (DS-LINK.NEGPAR LINK))
	     (NEGLITNO (DS-LINK.NEGLITNO LINK))
	     (POSPAR (DS-LINK.POSPAR LINK))
	     (POSLITNO (DS-LINK.POSLITNO LINK))
	     (ORIGINS (LIST NIL))
	     LITLIST REN.SUBST POSRHO NEGRHO)
	 (SETQ LITLIST (OP=TRANSMIT.LITERALS NEGPAR NEGLITNO UNIFIER LITLIST ORIGINS nil))
	 (SETQ LITLIST (OP=TRANSMIT.LITERALS POSPAR POSLITNO UNIFIER LITLIST ORIGINS nil))
	 (when (opt-is.with.residues)
	   (let* ((ll (op=create.element.literals link unifier))
		  (nnolits (length ll))
		  (jetzt (length litlist)))
	     (setq nonolits (reverse (do ((n jetzt (1+ n))
					  (i nil (cons (1+ n) i)))
					 ((= n (+ jetzt nnolits)) i))))
	     (setq litlist (append litlist ll))))
	 (multiple-value-SETQ (POSRHO NEGRHO REN.SUBST)
	   (op=renamings (DS-CLAUSE.VARIABLES POSPAR)
			 (DS-CLAUSE.VARIABLES NEGPAR)
			 (dt-termlist.variables (mapcan #'(lambda (lit) (copy-list (ds-lit.termlist lit))) litlist))))
	 (if complexity
	     (conses (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
	     (if list.result
		 (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)
		 (let* ((pname (OP=res_NEWNAME))
			(resolvent (DS-CLAUSE.CREATE pname (LIST NEGPAR POSPAR) (DS-LINK.DEPTH LINK)
						     (setq litlist (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))))
			(pars (list resolvent)))
		   (CG-INSERT.CLAUSE RESOLVENT (CONS LINK UNIFIER) (CAR ORIGINS) 'RESOLUTION LINK)
		   (setq origins nil)
		    #-:mkrp-basic(if (nar-narrow.info link)
				     (nar-pr.narrow link resolvent)
				   (PR-OPERATION 'RESOLUTION LINK UNIFIER RESOLVENT))
		    #+:mkrp-basic(PR-OPERATION 'RESOLUTION LINK UNIFIER RESOLVENT)
		   (red-rewrite resolvent)
		   (ds-rewrite.update resolvent)
		   (unless (ZEROP (DS-CLAUSE.NOLIT RESOLVENT))
		     (when nonolits (cons-reconstruct.links nil (list (cons resolvent nonolits))))
		     (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
			    (OP=POT.T.AND.F.LITNOS (list resolvent))
			    (cons-construct.links (list resolvent) nil))
			   (t (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS RESOLVENT)
			      (OP=INHERIT.LINKS RESOLVENT LINK UNIFIER POSRHO NEGRHO REN.SUBST)
			      (OP=create_P.PD.AND.PIW.LINKS resolvent unifier nil)
			      (OP=CONNECT.TO.PARENTS RESOLVENT))))
		   (SETQ POSRHO (DS-CLAUSE.VARIABLES RESOLVENT))
		   (SMAPC #'(LAMBDA (VAR) (COND ((NOT (MEMBER VAR POSRHO)) (DT-VARIABLE.DELETE VAR))))
			  #'CDDR (CDR REN.SUBST))
		   pars)))))
    (RIW (let ((resolvent (OP=RIW.RESOLVE LINK UNIFIER)))
	   (PR-OPERATION 'RESOLUTION LINK UNIFIER RESOLVENT)
	   resolvent))
    (OTHERWISE (ERROR "ILLEGAL LINK COLOUR IN OP-RESOLVE: ~A" (DS-LINK.COLOUR LINK)))))

(DEFUN OP=PARAMODULATE (LINK UNIFIER &optional complexity list.result)
						; EDITED:  "12-AUG-1989 22:06"
						; Authors: PRCKLN SB
						; INPUT:  LINK IS A PLINK OF THE ACTUAL GRAPH.
						;         UNIFIER IS A UNIFIER OF LINK.
						;         COMPEXITY is a flag.
						; EFFECT: If COMPLEXITY is NIL, THE PARAMODULANT OF LINK AND THE UNIT
						;         FACTORS ARE CREATED AND ADDED TO THE GRAPH.
						;         THE PARAMODULATION LITERAL IS HANDLED
						;         SEPARATELY.
						;         If COMPLEXITY is true the number of
						;         terms in the resulting paramodulant is returned.
						; VALUE:  NAME OF THE PARAMODULANT OR NIL (TAUTOLOGY)
  (let ((NEGPAR (DS-LINK.NEGPAR LINK))
	(NEGLITNO (DS-LINK.NEGLITNO LINK))
	(NEGFCT (DS-LINK.NEGFCT LINK))
	(POSPAR (DS-LINK.POSPAR LINK))
	(POSLITNO (DS-LINK.POSLITNO LINK))
	(POSFCT (DS-LINK.POSFCT LINK))
	(ORIGINS (LIST NIL))
	(positions (list nil))
	POSTERM.OTHERSIDE LITLIST REN.SUBST POSRHO NEGRHO)
    (SETQ POSTERM.OTHERSIDE
	  (COND ((DT-TAF.IS.LEFT POSFCT) (SECOND (DS-CLAUSE.TERMLIST POSPAR POSLITNO)))
		((DT-TAF.IS.RIGHT POSFCT) (CAR (DS-CLAUSE.TERMLIST POSPAR POSLITNO)))
		(T (ERROR "BAD ACCESS FUNCTION IN OP-PARAMODULATE. LINK=: ~A" LINK))))
    (SETQ LITLIST (OP=TRANSMIT.LITERALS NEGPAR NEGLITNO UNIFIER LITLIST ORIGINS positions))
    (SETQ LITLIST (OP=TRANSMIT.LITERALS POSPAR POSLITNO UNIFIER LITLIST ORIGINS positions))
    (SETQ LITLIST (OP=PAR_TRANSMIT.LITERAL NEGPAR NEGLITNO NEGFCT POSTERM.OTHERSIDE UNIFIER LITLIST ORIGINS positions))
    ;; NOTE: THIS HAS TO BE THE LAST CALL FOR LITERAL TRANSMISSION IF
    ;;     THE PARAMODULATION STEPS GENERATE A
    ;;     IRREFLEXIVE LITERAL ; IT THEN WILL BE THE FIRST ELEMENT OF LITLIST    
    (multiple-value-SETQ (POSRHO NEGRHO REN.SUBST)
      (op=renamings (DS-CLAUSE.VARIABLES POSPAR)
		    (DS-CLAUSE.VARIABLES NEGPAR)
		    (dt-termlist.variables (mapcan #'(lambda (lit) (copy-list (ds-lit.termlist lit))) litlist))))
    (if complexity
	(conses (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
	(if list.result
	    (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)
	    (let* ((pname (OP=PAR_NEWNAME))
		   (new.litlist (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
		   (paramodulant (DS-CLAUSE.CREATE pname (LIST NEGPAR POSPAR) (DS-LINK.DEPTH LINK) new.litlist))
		   (pars (list paramodulant)))
	      (CG-INSERT.CLAUSE PARAMODULANT (CONS LINK UNIFIER) (CAR ORIGINS) 'PARAMODULATION LINK)
	      (op=insert.positions paramodulant (first positions))
	      (setq origins nil)
	      (PR-OPERATION 'PARAMODULATION LINK UNIFIER PARAMODULANT)
	      (red-rewrite paramodulant)
	      (ds-rewrite.update paramodulant)
	      (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
		     (OP=POT.T.AND.F.LITNOS (list paramodulant))
		     (cons-construct.links (list paramodulant) nil))
		    (t (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS PARAMODULANT)
		       (OP=PAR_CREATE.POT.TRUE.AND.FALSE.LITNO.FOR.PARAMOD.LIT PARAMODULANT 1)
		       (OP=create_P.PD.AND.PIW.LINKS paramodulant unifier nil)	; LITNOS are ignored for p-Links
		       (OP=INHERIT.LINKS PARAMODULANT LINK UNIFIER POSRHO NEGRHO REN.SUBST '(1))
		       (OP=CONNECT.TO.PARENTS PARAMODULANT '(1))
		       (OP=PAR_CONNECT PARAMODULANT POSPAR POSLITNO POSFCT NEGPAR NEGFCT UNIFIER)))
	      pars)))))

(DEFUN OP=piw.PARAMODULATE (LINK UNIFIER &optional complexity list.result)
						; EDITED:  "12-AUG-1989 22:06"
						; Authors: PRCKLN SB
						; INPUT:  LINK IS A PLINK OF THE ACTUAL GRAPH.
						;         UNIFIER IS A UNIFIER OF LINK.
						;         COMPEXITY is a flag.
						; EFFECT: If COMPLEXITY is NIL, THE PARAMODULANT OF LINK AND THE UNIT
						;         FACTORS ARE CREATED AND ADDED TO THE GRAPH.
						;         THE PARAMODULATION LITERAL IS HANDLED
						;         SEPARATELY.
						;         If COMPLEXITY is true the number of
						;         terms in the resulting paramodulant is returned.
						; VALUE:  NAME OF THE PARAMODULANT OR NIL (TAUTOLOGY)
  (let ((NEGPAR (DS-LINK.NEGPAR LINK))
	(NEGLITNO (DS-LINK.NEGLITNO LINK))
	(NEGFCT (DS-LINK.NEGFCT LINK))
	(POSPAR (DS-LINK.POSPAR LINK))
	(POSLITNO (DS-LINK.POSLITNO LINK))
	(POSFCT (DS-LINK.POSFCT LINK))
	(ORIGINS (LIST NIL))
	(positions (list nil))
	POSTERM.OTHERSIDE LITLIST POSRHO NEGRHO REN.SUBST)
    (SETQ POSTERM.OTHERSIDE
	  (uni-apply.substitution (ds-clause.renaming pospar)
				  (dt-access (dt-taf.otherside posfct) (DS-CLAUSE.TERMLIST POSPAR POSLITNO))
				  t))
    (SETQ LITLIST (OP=TRANSMIT.LITERALS NEGPAR NEGLITNO UNIFIER LITLIST ORIGINS positions))
    (SETQ LITLIST (OP=TRANSMIT.LITERALS POSPAR POSLITNO unifier lITLIST ORIGINS positions (ds-clause.renaming pospar)))
    (SETQ LITLIST (OP=PAR_TRANSMIT.LITERAL NEGPAR NEGLITNO NEGFCT POSTERM.OTHERSIDE UNIFIER LITLIST ORIGINS positions))
    ;; NOTE: THIS HAS TO BE THE LAST CALL FOR LITERAL TRANSMISSION IF
    ;;     THE PARAMODULATION STEPS GENERATE A
    ;;     IRREFLEXIVE LITERAL ; IT THEN WILL BE THE FIRST ELEMENT OF LITLIST
    (SETQ REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION
		      (dt-termlist.variables (mapcan #'(lambda (lit) (copy-list (ds-lit.termlist lit))) litlist))))
    (if complexity
	(conses (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
	(if list.result
	    (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)
	    (let* ((pname (OP=PAR_NEWNAME))
		   (new.litlist (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T))
		   (paramodulant (DS-CLAUSE.CREATE pname (LIST NEGPAR POSPAR) (DS-LINK.DEPTH LINK) new.litlist))
		   (pars (list paramodulant)))	
	      (CG-INSERT.CLAUSE PARAMODULANT (CONS LINK UNIFIER) (CAR ORIGINS) 'PARAMODULATION LINK)
	      (op=insert.positions paramodulant (first positions))
	      (setq origins nil)
	      (PR-OPERATION 'PARAMODULATION LINK UNIFIER PARAMODULANT)
	      (red-rewrite paramodulant)
	      (ds-rewrite.update paramodulant)
	      (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
		     (OP=POT.T.AND.F.LITNOS (list paramodulant))
		     (cons-construct.links (list paramodulant) nil))
		    (t (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS PARAMODULANT)
		       (OP=PAR_CREATE.POT.TRUE.AND.FALSE.LITNO.FOR.PARAMOD.LIT PARAMODULANT 1)
		       (OP=INHERIT.LINKS PARAMODULANT LINK UNIFIER POSRHO NEGRHO REN.SUBST '(1))
		       (OP=create_P.PD.AND.PIW.LINKS paramodulant unifier nil)
		       (OP=CONNECT.TO.PARENTS PARAMODULANT '(1))
		       (OP=PAR_CONNECT PARAMODULANT POSPAR POSLITNO POSFCT NEGPAR NEGFCT UNIFIER)))
	      pars)))))

(DEFUN OP-PARAMODULATE (LINK UNIFIER &optional complexity list.result)
						; EDITED:  20-JAN-1990 19:26
						; Authors: PRCKLN SB
						; INPUT:   LINK IS A PLINK OF THE ACTUAL GRAPH.
						;          UNIFIER IS A UNIFIER OF LINK.
						;          COMPEXITY is a flag.
						; EFFECT:  If COMPLEXITY is NIL, THE PARAMODULANT OF LINK AND THE UNIT
						;          FACTORS ARE CREATED AND ADDED TO THE GRAPH.
						;          THE PARAMODULATION LITERAL IS HANDLED
						;          SEPARATELY.
						;          If COMPLEXITY is true the number of
						;          terms in the resulting paramodulant is returned.
						; VALUE:   NAME OF THE PARAMODULANT OR NIL (TAUTOLOGY)
  
  (let ((result (CASE (DS-LINK.COLOUR LINK)
		  (p (OP=PARAMODULATE link unifier complexity list.result))
		  (piw (OP=piw.PARAMODULATE link unifier complexity list.result))
		  (t (error "Wrong link colour in paramodulation: ~A" (DS-LINK.COLOUR LINK))))))
    result))

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


(DEFUN OP-FACTORIZE (LINK UNIFIER &optional complexity list.result)
						; EDITED: 27-APR-82 17:35:41")
						;         25. 4. 1983  SB
						; INPUT:  LINK IS A FLINK OF THE ACTUAL GRAPH
						;         UNIFIER IS A UNIFIER OF LINK.
						;         TIME IS THE STARTTIME FOR THIS OPERATION
						; EFFECT: THE FACTOR OF FLINK IS CREATED AND ADDED TO
						;         THE GRAPH.
						; VALUE:  NAME OF THE FACTOR (OR NIL).
  (OP=FACTORIZE LINK UNIFIER NIL complexity list.result))

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
	  (PR-OPERATION 'instantiate UNIFIER clause instance)
	  (red-rewrite instance)
	  (ds-rewrite.update instance)
	  (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
		 (OP=POT.T.AND.F.LITNOS (list instance))
		 (cons-construct.links (list instance) nil))
		((NOT (ZEROP (DS-CLAUSE.NOLIT INSTANCE)))
		 (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS INSTANCE)
		 (OP=INHERIT.LINKS INSTANCE nil UNIFIER ren.subst ren.subst ren.subst)
		 (OP=create_P.PD.AND.PIW.LINKS instance unifier nil)
		 (OP=CONNECT.TO.PARENTS instance)))
	  pars))))

(DEFUN OP-CREATE.UNIT.FACTORS (CLAUSE)
						; EDITED:  "12-AUG-1989 22:05"
						; INPUT: A CLAUSE
						; EFFECT: THE CLAUSE IS DEVIDED INTO GROUPS OF
						;         LITERALS BEEING CONNECTED BY COMPATIBLE
						;         SI-LINKS WITH EACH OTHER.
						;         EACH FACTOR WHICH MERGES EACH GROUP INTO
						;         ONE LITERAL IS GENERATED.
						;         IF SI-RULE LINKS ARE PRESENT, ONLY FACTORS
						;         OF TWO-LITERAL CLAUSES ARE CREATED.
						; VALUE:  THE LIST OF NEW FACTORS.
  (COND ((EQL 1 (DS-CLAUSE.NOLIT CLAUSE)) NIL)
	((MEMBER-IF #'(LAMBDA (LINK) (CONSP (DS-LINK.RULE LINK))) (DS-CLAUSE.ALL.LINKS 'SI CLAUSE))
	 (OP=TWO.CREATE.UNIT.FACTORS CLAUSE))
	(T (let ((LITNOS (OP=SI.COMPLETE.GROUPS CLAUSE)) UNIFIERS FACTORS FACTOR LITNO1)
	     (COND (LITNOS (SETQ UNIFIERS (LIST NIL))
			   (EVERY #'(LAMBDA (LITNOS)
				      (SETQ LITNO1 (CAR LITNOS))
				      (EVERY #'(LAMBDA (LITNO2)
						 (SETQ UNIFIERS
						       (UNI-MERGE.SUBSTITUTIONLISTS
							 UNIFIERS
							 (MAPCAN #'(LAMBDA (LINK) (COPY-LIST (DS-LINK.UNIFIERS LINK)))
								 (REMOVE-IF-NOT
								   #'(LAMBDA (LINK) (MEMBER LINK (DS-CLAUSE.LINKS 'SI CLAUSE LITNO2)))
								   (DS-CLAUSE.LINKS 'SI CLAUSE LITNO1))))))
					     (CDR LITNOS)))
				  LITNOS)
			   (MAPC #'(LAMBDA (UNIFIER)
				     (let ((LINK (CAR (DS-CLAUSE.LINKS 'SI CLAUSE 1)))
					   (REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION (DS-CLAUSE.VARIABLES CLAUSE)))
					   ORIGINS LITLIST)
				       (MAPC #'(LAMBDA (LITNO)
						 (SETQ LITLIST
						       (CONS
							 (DS-LIT.CREATE (DS-CLAUSE.SIGN CLAUSE LITNO)
									(DS-CLAUSE.PREDICATE CLAUSE LITNO)
									(UNI-APPLY.SUBSTITUTION UNIFIER
												(DS-CLAUSE.TERMLIST CLAUSE LITNO)
												T))
							 LITLIST))
						 (SETQ ORIGINS (CONS (LIST (LIST CLAUSE LITNO)) ORIGINS)))
					     (OP=NEGATIVE.SIDES CLAUSE LITNOS))
				       (SETQ FACTOR
					     (DS-CLAUSE.CREATE (OP=FAC_NEWNAME (DS-CLAUSE.PNAME CLAUSE))
							       (LIST CLAUSE)
							       (DS-CLAUSE.DEPTH CLAUSE)
							       (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)))
				       (push FACTOR FACTORS)
				       (ds-clause.rewrite.rule.set factor 1)
				       (CG-INSERT.CLAUSE FACTOR (CONS LINK UNIFIER) ORIGINS 'FACTORIZATION LINK)
				       (setq origins nil)
				       (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
					      (OP=POT.T.AND.F.LITNOS (list factor))
					      (cons-construct.links (list factor) nil))
					     (t (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS FACTOR)
						(OP=INHERIT.LINKS FACTOR LINK UNIFIER REN.SUBST REN.SUBST REN.SUBST)
						(OP=create_P.PD.AND.PIW.LINKS factor unifier nil)
						(OP=CONNECT.TO.PARENTS FACTOR)))
				       (PR-OPERATION 'FACTORIZATION LINK UNIFIER FACTOR)
				       (SETQ LITLIST (DS-CLAUSE.VARIABLES FACTOR))
				       (SMAPC #'(LAMBDA (VAR) (COND ((NOT (MEMBER VAR LITLIST)) (DT-VARIABLE.DELETE VAR))))
					      #'CDDR
					      (CDR REN.SUBST))))
				 UNIFIERS)))
	     FACTORS))))

(DEFUN OP=TWO.CREATE.UNIT.FACTORS (CLAUSE)
						; EDITED: 6. 4. 1983
						; INPUT: A CLAUSE
						; EFFECT: IF THE CLAUSE HAS EXACTLY TWO LITERALS AND
						;         IS UNIT FACTORIZABLE, THE UNIT FACTORS ARE
						;         CREATED.
						; VALUE:  THE LIST OF NEW FACTORS.
  (COND
    ((EQL 2 (DS-CLAUSE.NOLIT CLAUSE))
     (PROG (FACTORS FACTOR REN.SUBST ORIGINS LITLIST NEGLITNO)
	   (MAPC
	     #'(LAMBDA (LINK) (SETQ NEGLITNO (DS-LINK.NEGLITNO LINK))
		       (MAPC
			 #'(LAMBDA (UNIFIER)
			     (SETQ REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION (DS-CLAUSE.VARIABLES CLAUSE)))
			     (SETQ LITLIST
				   (LIST
				     (DS-LIT.CREATE (DS-CLAUSE.SIGN CLAUSE NEGLITNO) (DS-CLAUSE.PREDICATE CLAUSE NEGLITNO)
						    (UNI-APPLY.SUBSTITUTION UNIFIER (DS-CLAUSE.TERMLIST CLAUSE NEGLITNO) T))))
			     (SETQ ORIGINS (adjoin (LIST (LIST CLAUSE NEGLITNO)) ORIGINS :test #'equal))
			     (SETQ FACTOR
				   (DS-CLAUSE.CREATE (OP=FAC_NEWNAME (DS-CLAUSE.PNAME CLAUSE)) (LIST CLAUSE)
						     (DS-CLAUSE.DEPTH CLAUSE)
						     (UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)))
			     (SETQ FACTORS (CONS FACTOR FACTORS))
			     (CG-INSERT.CLAUSE FACTOR (CONS LINK UNIFIER) ORIGINS 'FACTORIZATION LINK)
			     (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS FACTOR)
			     (OP=INHERIT.LINKS FACTOR LINK UNIFIER REN.SUBST REN.SUBST REN.SUBST)
			     (OP=CONNECT.TO.PARENTS FACTOR) (PR-OPERATION 'FACTORIZATION LINK UNIFIER FACTOR)
			     (SETQ LITLIST (DS-CLAUSE.VARIABLES FACTOR))
			     (SMAPC
			       #'(LAMBDA (VAR) (COND ((NOT (MEMBER VAR LITLIST)) (DT-VARIABLE.DELETE VAR)))) #'CDDR
			       (CDR REN.SUBST))
			     (CG-REMOVE.UNIFIER LINK UNIFIER T 'ACTIVE))
			 (COPY-LIST (DS-LINK.UNIFIERS LINK))))
	     (DS-CLAUSE.LINKS 'SI CLAUSE 1))))))


(DEFUN OP-R.CHAIN (CHAIN)
						; EDITED: 9. 8. 1984
						; INPUT:  A RESOLUTION CHAIN: A LIST WITH ELEMENTS
						;         (SUBSTITUTION CLAUSE LINKS POINTERS LITNOS)
						;         SUBSTITUTION IS THE MERGED SUBSTITUTION
						;         OF LINKS.
						;         CLAUSE IS THE CLAUSE TO BE REDUCED BY
						;         SIMULTANEOUS RESOLUTION UPON LINKS
						;         (YIELDING USUALLY, BUT NOT NECESSARY A
						;         UNIT CLAUSE.)
						;         LITNOS IS THE LIST OF REMAINING LITERALS
						;         (NORMALLY ONLY ONE LITERAL)
						;         POINTERS IS A LIST OF ADDRESSES OF
						;         LINK TAILS IN FURTHER CHAIN ELEMENTS.
						;         (THESE LINKS HAVE TO BE REPLACED BY THEIR
						;         DESCENDANT WHEN OPERATING UPON THE
						;         CURRENT CHAIN ELEMENT.)
						; EFFECT: THE NEW CLAUSES ARE GENERATED BY
						;         SIMULTANEUS RESOLUTION UPON THE LINKS
						;         IN THE CHAIN ELEMENTS, AND INSERTED INTO
						;         THE GRAPH. THIS OPERATIONS ARE PROTOCOLLED
						;         AS A SEQUENCE OF RESOLUTIONS FOLLOWED BY
						;         A REPLACEMENT RESOLUTION STEP.
						; VALUE:  UNDEFINED.
  (MAPC #'(LAMBDA (ELEMENT)
	    (let* ((UNIFIER (first ELEMENT))
		   (CLAUSE (SECOND ELEMENT))
		   (LINKS (THIRD ELEMENT))
		   (POINTERS (FOURTH ELEMENT))
		   (LITNOS (fifth ELEMENT))
		   (RHO (DT-VARIABLE.RENAMING.SUBSTITUTION (union (DS-CLAUSE.VARIABLES CLAUSE) (DT-TERMLIST.VARIABLES unifier))))
		   (ORIGINS NIL)
		   (LITLIST NIL)
		   (TERMLIST NIL)
		   (TERMLISTS NIL)
		   (FIRST (CASE (DS-LINK.COLOUR (CAR LINKS))
			    (R (DS-LINK.THISLITNO (CAR LINKS) CLAUSE))
			    (SI (DS-LINK.POSLITNO (CAR LINKS)))
			    (OTHERWISE (ERROR "Illegal colour: ~A" (DS-LINK.COLOUR (CAR LINKS)))))))
	      (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		(PROGN
		  (unless (EQ (1+ RPTN) FIRST)	; TERMLISTS IS CREATED FOR THE PROTOCOL MODULE ONLY.
		    (SETQ TERMLIST
			  (UNI-APPLY.SUBSTITUTION RHO
						  (UNI-APPLY.SUBSTITUTION UNIFIER (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN)) T)
						  NIL))
		    (push TERMLIST TERMLISTS))
		  (when (MEMBER (1+ RPTN) LITNOS)
		    (push (DS-LIT.CREATE (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))
					 (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))
					 TERMLIST)
			  LITLIST)
		    (push (LIST (LIST CLAUSE (1+ RPTN))) ORIGINS))))
	      (let ((RESOLVENT (DS-CLAUSE.CREATE (OP=RES_NEWNAME)
						 (CONS CLAUSE
						       (MAPCAN #'(LAMBDA (LINK)
								   (COND ((EQL 'R (DS-LINK.COLOUR LINK))
									  (LIST (DS-LINK.OTHERPAR LINK CLAUSE)))))
							       LINKS))
						 (DS-LINK.DEPTH (MAXELT LINKS #'(LAMBDA (LINK) (DS-LINK.DEPTH LINK))))
						 LITLIST)))
		(CG-INSERT.CLAUSE RESOLVENT (CONS (CAR LINKS) UNIFIER) ORIGINS 'RESOLUTION LINKS)		
		(SETQ ORIGINS (DS-CLAUSE.VARIABLES RESOLVENT))
		(PR-OPERATION 'R.CHAIN RESOLVENT TERMLISTS ELEMENT)
		(red-rewrite resolvent)
		(ds-rewrite.update resolvent)
		(unless (ZEROP (DS-CLAUSE.NOLIT RESOLVENT))
		  (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
			 (OP=POT.T.AND.F.LITNOS (list resolvent))
			 (OP=INHERIT.LINKS RESOLVENT (CAR LINKS) UNIFIER RHO RHO RHO)
			 (OP=create_P.PD.AND.PIW.LINKS resolvent unifier nil)
			 (OP=CONNECT.TO.PARENTS RESOLVENT))
			(t (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS RESOLVENT)
			   (OP=INHERIT.LINKS RESOLVENT (CAR LINKS) UNIFIER RHO RHO RHO)
			   (OP=create_P.PD.AND.PIW.LINKS resolvent unifier nil)
			   (OP=CONNECT.TO.PARENTS RESOLVENT))))
		(MAPC #'(LAMBDA (LINKS) (RPLACA LINKS (or (CAR (CG-LINK_DESCENDANT.LINKS (CAR LINKS)))
							  (first links))))	; Try to repair problem with pointers 
		      POINTERS))		; if no descendents exist
	      (SMAPC #'(LAMBDA (VARIABLE) (unless (MEMBER VARIABLE ORIGINS) (DT-VARIABLE.DELETE VARIABLE)))
		     #'CDDR (CDR RHO))))
	chain))

#|(defun op-rp.chain (link result info)
  (let* ((UNIFIER (first (ds-link.unifiers link)))
	 (clause (DS-CLAUSE.CREATE (case (ds-link.colour link)
				     (r (OP=res_NEWNAME))
				     (p (OP=par_NEWNAME)))
				   (list (ds-link.pospar link) (ds-link.negpar link))
				   (DS-LINK.DEPTH link)
				   result)))
    (CG-INSERT.CLAUSE clause (CONS (CAR LINKS) UNIFIER) ORIGINS 'rp.chain)
    (PR-OPERATION 'Rp.CHAIN clause (cons (case (ds-link.colour link)
					   (r (list 'resolution (ds-link.pospar link) (ds-link.poslitno link)
						    (ds-link.negpar link) (ds-link.neglitno link)))
					   (p (list 'paramodulation (ds-link.pospar link) (ds-link.poslitno link)
						    (ds-link.negpar link) (ds-link.neglitno link))))
					 (nreverse info)))
    (red-rewrite clause)
    (ds-rewrite.update clause)
    (unless (ZEROP (DS-CLAUSE.NOLIT RESOLVENT))
      (cond ((opt-is.kz.completion)
	     (OP=POT.T.AND.F.LITNOS (list clause))
	     (cons-construct.links (list clause) nil))
	    (t (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS CLAUSE)
	       (OP=PAR_CREATE.POT.TRUE.AND.FALSE.LITNO.FOR.PARAMOD.LIT CLAUSE 1)
	       (OP=create_P.PD.AND.PIW.LINKS clause unifier nil)	; LITNOS are ignored for p-Links
	       (OP=INHERIT.LINKS CLAUSE LINK UNIFIER POSRHO NEGRHO REN.SUBST '(1))
	       (OP=CONNECT.TO.PARENTS CLAUSE '(1))
	       (OP=PAR_CONNECT CLAUSE POSPAR POSLITNO POSFCT NEGPAR NEGFCT UNIFIER))))))|#


(DEFUN OP=RIW.RESOLVE (LINK UNIFIER)
						; EDITED: 3. 5. 1984
						; INPUT:  AN RIW-LINK CONECTED TO A CLAUSE WITH TWO
						;         LITERALS AND A WEAK UNIFIER
						; EFFECT: THE RESOLVENT IS GENERATED AND THE LINKS
						;         ARE INHERITED (NO RULE-LINKS)
						; VALUE:  ADDRESS OF THE RESOLVENT.
  (let* ((CLAUSE (DS-LINK.POSPAR LINK))
	 (POSLITNO (DS-LINK.POSLITNO LINK))
	 (NEGLITNO (DS-LINK.NEGLITNO LINK))
	 (RENAMING (DS-CLAUSE.RENAMING CLAUSE))
	 (PREDICATE (DS-CLAUSE.PREDICATE CLAUSE 1))
	 UNIFIERS OTHERCLAUSE OTHERLITNO LITLIST)
    (SETQ LITLIST
	  (COND
	    ((EQL 1 POSLITNO)
	     (LIST (DS-LIT.CREATE '+ PREDICATE (UNI-APPLY.SUBSTITUTION UNIFIER (DS-CLAUSE.TERMLIST CLAUSE 1) T))
		   (DS-LIT.CREATE '- PREDICATE (UNI-APPLY.SUBSTITUTION UNIFIER
								       (UNI-APPLY.SUBSTITUTION RENAMING
											       (DS-CLAUSE.TERMLIST CLAUSE 2)
											       T)))))
	    (T (LIST (DS-LIT.CREATE '- PREDICATE (UNI-APPLY.SUBSTITUTION UNIFIER
									 (UNI-APPLY.SUBSTITUTION RENAMING
												 (DS-CLAUSE.TERMLIST CLAUSE 1)
												 T)))
		     (DS-LIT.CREATE '+ PREDICATE (UNI-APPLY.SUBSTITUTION UNIFIER (DS-CLAUSE.TERMLIST CLAUSE 2) T))))))
    (let* ((pname (OP=res_NEWNAME))
	   (rensubst (DT-VARIABLE.RENAMING.SUBSTITUTION RENAMING))
	   (new.litlist (UNI-APPLY.SUBSTITUTION rensubst LITLIST NIL))
	   (resolvent (DS-CLAUSE.CREATE pname (LIST CLAUSE) (1+ (DS-CLAUSE.DEPTH CLAUSE)) new.litlist))
	   (first t)
	   (pars nil)
	   (variables (ds-clause.variables resolvent))
	   (litnos (ds-clause.compute.max.litno resolvent)))
      (mapc #'(lambda (litno)
		(if first
		    (setq first nil)
		    (setq resolvent (DS-CLAUSE.CREATE (format nil "~A.C~A" PNAME litno)
						      (LIST CLAUSE) (1+ (DS-CLAUSE.DEPTH CLAUSE))
						      (UNI-APPLY.SUBSTITUTION 
							(DT-VARIABLE.RENAMING.SUBSTITUTION variables)
							new.LITLIST T))))
		(push resolvent pars)
		(ds-clause.rewrite.rule.set resolvent litno)		
		(CG-INSERT.CLAUSE RESOLVENT (CONS LINK UNIFIER)
				  (LIST (LIST (LIST CLAUSE 1)) (LIST (LIST CLAUSE 2))) 'RESOLUTION LINK)
						; LINK INHERITANCE
		(op=inherit1)
		(COND
		  ((SETQ UNIFIERS
			 (UNI-UNIFY.ATOMS PREDICATE (DS-CLAUSE.TERMLIST CLAUSE 1) PREDICATE (DS-CLAUSE.TERMLIST RESOLVENT 2)))
		   (CG-INSERT.LINK
		     (COND ((EQL 1 POSLITNO) (DS-LINK.CREATE 'R UNIFIERS CLAUSE 1 RESOLVENT 2 NIL NIL NIL))
			   (T (DS-LINK.CREATE 'R UNIFIERS RESOLVENT 2 CLAUSE 1 NIL NIL NIL)))
		     (LIST LINK) 'INHERITED (LIST LINK))
						; ONE OF THE CROSS LINKS IS SUFFICIENT
		   (COND ((SETQ UNIFIERS (UNI-UNIFY.ATOMS PREDICATE
							  (UNI-APPLY.SUBSTITUTION (DS-CLAUSE.RENAMING RESOLVENT)
										  (DS-CLAUSE.TERMLIST RESOLVENT POSLITNO) T)
							  PREDICATE (DS-CLAUSE.TERMLIST RESOLVENT NEGLITNO)))
			  (CG-INSERT.LINK (DS-LINK.CREATE 'RIW UNIFIERS RESOLVENT POSLITNO RESOLVENT NEGLITNO NIL NIL NIL)
					  (LIST LINK) 'INHERITED (LIST LINK))
			  (COND ((SETQ UNIFIERS (UNI-UNIFY.ATOMS PREDICATE (DS-CLAUSE.TERMLIST RESOLVENT 1)
								 PREDICATE (DS-CLAUSE.TERMLIST RESOLVENT 2)))
				 (CG-INSERT.LINK (DS-LINK.CREATE 'TI UNIFIERS RESOLVENT POSLITNO RESOLVENT NEGLITNO NIL NIL NIL)
						 (LIST LINK) 'INHERITED (LIST LINK))))))))
		(OP=CONNECT.TO.PARENTS RESOLVENT))
	    litnos)
      pars)))

(defun op-operate (link unifier list.result.flag)
  (case (ds-link.colour link)
    ((r riw) (op-resolve link unifier nil list.result.flag))
    ((p piw) (op-paramodulate link unifier nil list.result.flag))
    (si (op-factorize link unifier nil list.result.flag))))

(defun op=subsumed.p.map.rules (clause litno termlist othersides)
					; EDITED:  16-JUL-92
					; Authors: PRCKLN
					; INPUT:   A CLAUSE, a literal in it and the termlist of another literal.
                                        ;          A list of OTHERSIDES.
					; EFFECT:  -
					; VALUE:   A list of matchers to OTHERSIDES.
  (let ((all.unifiers nil))    
    (some #'(LAMBDA (RULE)
		    (let (unifiers)
		      (COND ((AND (OP=ADMISSIBLE.RULE RULE nil)
				  (SETQ UNIFIERS
					(OP=match (DS-CLAUSE.TERMLIST CLAUSE litno) TERMLIST
						  RULE)))
			     (push UNIFIERS all.UNIFIERs)
			     (push RULE all.UNIFIERs)))))
	  (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDES))
    all.unifiers))
  

(defun op=subsumed.p.occ (variables rest.litlist other.litlist matchers othersides termlist clause2 litnos2)
  (let* ((new.other.litlist (or other.litlist (list clause2))))
    (if (eql clause2 (first new.other.litlist))
	(some #'(lambda (litno)
		  (and (not (member litno (rest other.litlist)))
		       (let ((op*unifier.buffer (op=subsumed.p.map.rules clause2 litno termlist othersides))
			     RULE UNIFIERS)
			 (SsomeL #'(LAMBDA (TAIL)
					   (cond ((SETQ UNIFIERS (SECOND TAIL))
						  (SETQ RULE (CDAR TAIL))
						  (OP=C.RENAME UNIFIERS RULE)
						  (some #'(lambda (uni)
							    (op=subsumed.p variables
									   (rest rest.litlist)
									   (append new.other.litlist
										   (list litno))
									   (UNI-MERGE.MATCHERLISTS
									    (list uni)
									    matchers t)))	   
							unifiers))))
				 #'CDDR
				 OP*UNIFIER.BUFFER))))
	      litnos2)
      (op=subsumed.p variables
		     (rest rest.litlist)
		     new.other.litlist
		     matchers))))

(defun op=subsumed.p (variables rest.litlist other.litlist matchers)
						; OTHER.LITLIST is (CLAUSE LITNO1 ... LITNOn)
  (if (and matchers other.litlist (= (length (rest other.litlist)) (ds-clause.nolit (first other.litlist))))
      t
      (if (or (null matchers) (null rest.litlist))
	  nil
	  (let* ((lit (first rest.litlist))
		 (sign (ds-lit.sign lit))
		 (predicate (ds-lit.predicate lit))
		 (termlist (ds-lit.termlist lit)))	
	    (if (first other.litlist)
		(or (some #'(LAMBDA (OTHERSIDES)
			      (do ((litno (ds-clause.nolit (first other.litlist)) (1- litno))
				   (result nil))
				  ((or (zerop litno) result)
				   result)
				(when (and (eql sign (ds-clause.sign (first other.litlist) litno))
					   (eql predicate (ds-clause.predicate (first other.litlist) litno)))
				  (setq result (op=subsumed.p.occ variables rest.litlist other.litlist matchers
								  othersides
								  termlist (first other.litlist) (list litno))))))
			  (DS-PREDICATE.OTHERSIDES sign predicate 's))
		    (op=subsumed.p variables
				   (rest rest.litlist)
				   other.litlist
				   matchers))
		(some #'(LAMBDA (OTHERSIDES)
			  (some #'(LAMBDA (OCCURRENCE)
				    (op=subsumed.p.occ variables rest.litlist other.litlist matchers
						       othersides termlist (first occurrence) (rest occurrence)))
				(OP=PREDICATE.OCCURRENCES (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDES)
							  (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDES))))
		      (DS-PREDICATE.OTHERSIDES sign predicate 's)))))))

(defun op-subsumed.p (variables litlist &optional clause)
  (unless (opt-is.with.residues)
    (op=subsumed.p variables litlist (if clause (list clause) nil) (list nil))))

(defvar op*rule.dummy (list nil nil))




(DEFUN OP=PAR_CONNECT (PARAMODULANT POSPAR POSLITNO POSFCT NEGPAR NEGFCT UNIFIER)
						; EDITED: 19-OCT-83                              AP
						; INPUT:  SAME AS OP=PAR_INHERIT.LINKS.OF.EQ.OTHERSIDE
						; EFFECT: LITERAL LINKS FOR PARAMODULATION LITERAL ARE
						;         CREATED, if strat is HEURISTIC-COMPLETION then
						;         links to all literals are created.
						; VALUE:  UNDEFINED.
  (declare (ignore unifier negfct negpar posfct poslitno pospar))
  (do* ((litno (ds-clause.nolit paramodulant) (1- litno))
	(litnos (list litno) (cons litno litnos)))
       ((= 1 litno) (OP-CONSTRUCT.LITERAL.LINKS PARAMODULANT
						(if (opt-is.completion)
						    litnos
						    '(1))
						OP*LINK.COLOURS.LITERAL.INITIAL (CG-CLAUSES ALL) NIL))))







(DEFUN OP=CONNECT.TO.PARENTS (CLAUSE &optional LITLIST)
						; EDITED: 20-SEP-83                             AP
						; INPUT:  A NEWLY CREATED CLAUSE.
						;         LITLIST IS ALIST OF LITERALNUMBERS OF CLAUSE
						; EFFECT: CREATES DATA OF THE S-LINKS BETWEEN
						;         LITERALS OF CLAUSE 'CLAUSE' AND THEIR
						;         ANCESTOR LITERALS, IF LITERAL IS IN LITLIST.
						; VALUE:  LIST OF THE NEW LINKS.
  (let (PARENTCLAUSE)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
      (unless (MEMBER (1+ RPTN) LITLIST)
	(MAPC #'(LAMBDA (PARENT) (SETQ PARENTCLAUSE (CAR PARENT))
			(MAPC #'(LAMBDA (PARENTLITNO)
				  (CG-INSERT.LINK
				    (DS-LINK.CREATE 'S
						    (UNI-UNIFY.ATOMS (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))
								     (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN))
								     (DS-CLAUSE.PREDICATE PARENTCLAUSE PARENTLITNO)
								     (DS-CLAUSE.TERMLIST PARENTCLAUSE PARENTLITNO))
						    CLAUSE (1+ RPTN) PARENTCLAUSE PARENTLITNO NIL NIL)
				    NIL 'PARENTCONNECTOR NIL))
			      (CDR PARENT)))
	      (CG-CLAUSE_ANCESTOR.LITERALS CLAUSE (1+ RPTN)))))))

(DEFUN OP=INSERT.LINK (LINK)
						; EDITED: 27-JUN-83 11:28:17
						; INPUT:  A LINK
						; VALUE:  UNDEFINED
						; EFFECT: THE PROPERTY OP*ANCESTORS IS REMOVED
						;         AND THE LINK IS INSERTED INTO THE GRAPH.
  (PROG ((ANCESTORS (DT-GETPROP LINK 'OP*ANCESTORS)))
	(CG-INSERT.LINK LINK ANCESTORS (COND (ANCESTORS 'INHERITED)) ANCESTORS) (DT-REMPROP LINK 'OP*ANCESTORS)))

(DEFUN OP=SI.COMPLETE.GROUPS (CLAUSE)
						; EDITED: 6. 4. 1984
						; INPUT:  A CLAUSE
						; VALUE:  A PARTITION OF CLAUSE (LIST OF LITERAL
						;         GROUPS) SUCH THAT EACH LITERAL IS CONNECTED
						;         WITH EACH OTHER LITERAL IN THE SAME GROUP
						;         VIA AN SI-LINK.
  (PROG ((NOLIT (DS-CLAUSE.NOLIT CLAUSE)) OTHERLITNO LITNO LITNOS LENGTH GROUPS NUMBER)
	(DODOWN (RPTN NOLIT)
	  (COND ((NULL (DS-CLAUSE.LINKS 'SI CLAUSE (1+ RPTN))) (RETURN-FROM OP=SI.COMPLETE.GROUPS NIL))))
	(COND ((EQL 2 NOLIT) (RETURN '((1 2))))) (SETQ NUMBER 1) (SETQ LITNO NOLIT)
	(WHILE LITNO (SETQ LITNOS (LIST LITNO))
	       (MAPC
		 #'(LAMBDA (LINK) (SETQ OTHERLITNO (DS-LINK.OTHERLITNO LINK CLAUSE LITNO))
			   (DS-CLAUSE.LIT.PUTPROP CLAUSE OTHERLITNO 'OP*NUMBER NUMBER)
			   (DS-CLAUSE.LIT.PUTPROP CLAUSE OTHERLITNO 'OP*OTHERSIDES
						  (INSERT LITNO (DS-CLAUSE.LIT.GETPROP CLAUSE OTHERLITNO 'OP*OTHERSIDES))))
		 (DS-CLAUSE.LINKS 'SI CLAUSE LITNO))
	       (DODOWN (RPTN (1- LITNO))
		 (COND
		   ((EQL NUMBER (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'OP*NUMBER))
		    (MAPC
		      #'(LAMBDA (LINK) (SETQ OTHERLITNO (DS-LINK.OTHERLITNO LINK CLAUSE (1+ RPTN)))
				(SETQ LITNOS (INSERT OTHERLITNO LITNOS))
				(DS-CLAUSE.LIT.PUTPROP CLAUSE OTHERLITNO 'OP*NUMBER NUMBER)
				(DS-CLAUSE.LIT.PUTPROP CLAUSE OTHERLITNO 'OP*OTHERSIDES
						       (INS (1+ RPTN)
							    (DS-CLAUSE.LIT.GETPROP CLAUSE OTHERLITNO 'OP*OTHERSIDES))))
		      (DS-CLAUSE.LINKS 'SI CLAUSE (1+ RPTN)))
		    (SETQ LITNOS (INSERT (1+ RPTN) LITNOS)))))
	       (SETQ LENGTH (1- (LIST-LENGTH LITNOS)))
	       (DODOWN (RPTN LITNO)
		 (COND
		   ((AND (EQL NUMBER (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'OP*NUMBER))
			 (NEQ LENGTH (LIST-LENGTH (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'OP*OTHERSIDES))))
		    (SETQ LITNO NIL) (SETQ RPTN 0))))
	       (COND
		 (LITNO
		  (DODOWN (RPTN (1- LITNO))
		    (PROGN (SETQ LITNO (1+ RPTN))
			   (COND ((NULL (DS-CLAUSE.LIT.GETPROP CLAUSE (1+ RPTN) 'OP*NUMBER)) (SETQ RPTN 0)))))
		  (COND ((EQL 1 LITNO) (SETQ LITNO NIL))) (SETQ GROUPS (CONS LITNOS GROUPS)) (SETQ NUMBER (1+ NUMBER)))
		 (T (SETQ GROUPS NIL))))
	(DODOWN (RPTN NOLIT) (DS-CLAUSE.LIT.REMPROPS CLAUSE (1+ RPTN) '(OP*NUMBER OP*OTHERSIDES))) (RETURN GROUPS)))

(DEFUN OP=NEGATIVE.SIDES (CLAUSE LITNOS)
						; EDITED: 6. 4. 1984
						; INPUT:  A CLAUSE AND A LIST OF LITERAL GROUPS
						; VALUE:  A LIST OF LITERALS, EACH OF WHICH BELONGING
						;         TO EXACTLY ONE GROUP, SUCH THAT IT
						;         IS THE NEGATIVE SIDE OF ALL INCIDENT
						;         SI-LINKS.
  (MAPCAR
    #'(LAMBDA (LITNOS)
        (CAR
          (OR
            (MEMBER-IF
              #'(LAMBDA (LITNO)
                  (EVERY #'(LAMBDA (LINK) (EQL LITNO (DS-LINK.NEGLITNO LINK))) (DS-CLAUSE.LINKS 'SI CLAUSE LITNO)))
              LITNOS)
            LITNOS)))
    LITNOS))

