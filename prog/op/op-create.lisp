;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(DEFUN OP-CONSTRUCT.CLAUSES (CLAUSELIST CLAUSETYPE)
						; EDITED:  7. 8. 1984
						; INPUT:   A LIST OF CLAUSES AND A CLAUSETYPE.
						;          CLAUSETYPE MAY BE 'AXIOM OR 'THEOREM.
						;          EACH CLAUSE IS A LIST OF LITERALS, EACH OF
						;          WHICH IS A TRIPLE OF FORMAT (SIGN
						;          PREDICATE TERMLIST). SIGNS ARE +, -.
						;          PREDICATES, FUNCTIONS, VARIABLES AND
						;          CONSTANTS ARE DT-OBJECTS. OTHER TERMS ARE
						;          LISTS OF FORMAT (FUNCTION TERM1 ... TERMN).
						; EFFECT:  THE CLAUSES ARE CREATED AS DATA STRUCTURE
						;          OBJECTS AND BECOME PART OF THE ACTUAL
						;          CONNECTIONGRAPH. THE CLAUSENAMES ARE
						;          CLAUSENAMESTRING CONCATENATED WITH A
						;          NUMBER UNIQUE FOR EACH CLAUSE.
						;          EACH PREDICATE SYMBOL HAS TWO COMPONENTS,
						;          POSITIVE.OCCURENCES AND NEGATIVE.OCCURENCES
						;          WHICH ARE LISTS OF SUBLISTS OF FORMAT
						;          (CLAUSE LITNO1 LITNO2 ...). FOR ALL
						;          PREDICATE SYMBOLS OCCURING IN CLAUSELIST
						;          THESE LISTS ARE UPDATED ACCORDINGLY.
						; VALUE:   LIST OF NEWLY GENERATED CLAUSE ADDRESSES
						;          CORRESPONDING TO CLAUSELIST IN THE SAME
						;          ORDER.
  (let ((CLAUSENAMESTRING (CASE CLAUSETYPE
			    (AXIOM "A")
			    (THEOREM "T")
			    (OTHERWISE (ERROR "ILLEGAL CLAUSETYPE IN CONS-CONSTRUCT.CLAUSES, CLAUSETYPE : : ~A" CLAUSETYPE)))))
    (MAPCAN #'(LAMBDA (CLAUSE)
		(let* ((pname (format nil "~A~A" CLAUSENAMESTRING (incf OP*CLAUSECOUNTER)))
		       (clause1 (DS-CLAUSE.CREATE pname CLAUSETYPE 0 CLAUSE))
		       (pars (list clause1)))
		  (DT-PUTPROP CLAUSE1 'MISSING.LINKS (DS-LINK.COLOURS.FOR 'INITIAL))
		  (CG-INSERT.CLAUSE CLAUSE1 NIL NIL 'INITIAL NIL)
		  (OP=POT.T.AND.F.LITNOS (list clause1))
		  pars))
	    CLAUSELIST)))

(defun op-clause.rewrite.rule.ac.extend (clause2 litno pname clause clausetype variables)
  (declare (ignore clausetype))
  (let ((taf (ds-clause.lit.rewrite.rule clause2 litno)))
    (when (and taf (not (dt-getprop clause2 'cons*ac1)))
      (setq taf (list taf))
      (let ((left (dt-access taf (ds-clause.termlist clause2 litno))))
	(when (and (dt-term_c.term.is left)
		   (or (DT-FUNCTION.IS.MARKED ac (dt-term_topsymbol left))
		       (DT-FUNCTION.IS.MARKED ac1 (dt-term_topsymbol left))))
	  (let* ((var (dt-variable.create 'any))
		 (litlist (UNI-APPLY.SUBSTITUTION 
			    (DT-VARIABLE.RENAMING.SUBSTITUTION variables)
			    clause T))
		 (termlist (ds-lit.termlist (first (nthcdr (1- litno) litlist))))
		 clause1)
	    (setf (first termlist) (list (dt-term_topsymbol left) var (first termlist))
		  (second termlist) (list (dt-term_topsymbol left) var (second termlist)))
	    (setq clause1 (DS-CLAUSE.CREATE (format nil "~A.AC" pname)
					    (list clause2) 0 litlist))
	    (ds-clause.irreducible.set clause1 litno)
	    (DT-PUTPROP CLAUSE1 'MISSING.LINKS (DS-LINK.COLOURS.FOR 'INITIAL))
	    (CG-INSERT.CLAUSE CLAUSE1 NIL NIL 'INITIAL NIL)
	    (OP=POT.T.AND.F.LITNOS (list clause1))
	    (PR-OPERATION 'instantiate nil clause2 clause1)
	    clause1))))))



(DEFUN OP-CONSTRUCT.LITERAL.LINKS (CLAUSE LITNOS COLOURS CLAUSES RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  A CLAUSE, NIL OR A LIST OF LITERAL NUMBERS,
						;         A LIST OF LINK COLOURS,
						;         A LIST OF CLAUSES (CLAUSE MAY BE IN CLAUSES)
						;         RESTRICTION IS EITHER NIL OR THE ATOM
						;         NORULES OR A LIST OF RULE CLAUSES.
						; EFFECT: IF LITNOS = NIL ALL LITERAL LINKS
						;         BETWEEN CLAUSE AND CLAUSES FOR THE GIVEN
						;         COLOURS ARE CREATED. IF LITNOS =//= NIL
						;         THEN THE LINKS FOR THE LITERALS IN LITNOS
						;         ARE CREATED ONLY.
						;         IF RESTRICTION = NIL THEN THE LINKS FOR ALL
						;         RULES ARE CREATED. IF RESTRICTION = NORULES
						;         THEN LINKS FOR RULE = NIL AND
						;         RULE = SYMMETRIC ARE CREATED ONLY.
						;         IF RESTRICTION = (RULECLAUSE1 ...) THEN
						;         LINKS FOR THESE RULECLAUSES ARE CREATED ONLY
						; VALUE:  UNDEFINED.
  (MAPC #'(LAMBDA (COLOUR)
	    (COND ((OP=COLOUR.IS.X COLOUR) (OP=CREATE.XLINKS CLAUSE LITNOS COLOUR CLAUSES RESTRICTION))
		  ((OP=COLOUR.IS.XI COLOUR) (OP=CREATE.XILINKS CLAUSE LITNOS COLOUR RESTRICTION))
		  ((OP=COLOUR.IS.XIW COLOUR) (OP=CREATE.XIWLINKS CLAUSE LITNOS COLOUR RESTRICTION))))
	COLOURS))

(DEFUN OP=POT.T.AND.F.LITNOS (CLAUSELIST)
						; EDITED: 8-SEP-83                              AP
						; INPUT:  A LIST OF CLAUSE ADDRESSES.
						; EFFECT: FOR ALL CLAUSES IN 'CLAUSELIST' THE CLAUSE
						;         COMPONENTS 'POT.TRUE.LITNOS' AND
						;         'POT.FALSE.LITNOS' WILL BE SET, E.G. ALL
						;         LITERAL NUMBERS WILL BE FOUND WHERE THE
						;         LITERAL WITH A REFLEXIVE OR IRREFLEXIVE
						;         PREDICATE CAN BECOMEFALSE OR TRUE BY A
						;         SUBSTITUTION, FOR EXAMPLE: NOT X=A OR X=A
						;         (X<-A).
						; VALUE:  UNDEFINED.
  (MAPC #'(LAMBDA (CLAUSE)
	    (let ((POT.TRUE.LITNOS NIL)
		  (POT.FALSE.LITNOS NIL))
	      (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		(let ((PREDICATE (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)))
		      (SIGN.IS.POSITIVE (DS-SIGN.IS.POSITIVE (DS-CLAUSE.SIGN CLAUSE (1+ RPTN)))))
		  (COND ((OR (AND (NOT SIGN.IS.POSITIVE) (DT-PREDICATE.IS.MARKED REFLEXIVE PREDICATE))
			     (AND (DT-PREDICATE.IS.MARKED IRREFLEXIVE PREDICATE) SIGN.IS.POSITIVE))
			 (SETQ POT.FALSE.LITNOS (CONS (1+ RPTN) POT.FALSE.LITNOS)))
			((OR (AND (DT-PREDICATE.IS.MARKED REFLEXIVE PREDICATE) SIGN.IS.POSITIVE)
			     (AND (NOT SIGN.IS.POSITIVE) (DT-PREDICATE.IS.MARKED IRREFLEXIVE PREDICATE)))
			 (SETQ POT.TRUE.LITNOS (CONS (1+ RPTN) POT.TRUE.LITNOS))))))
	      (DS-CLAUSE.PUT.POTENTIALLY.FALSE.LITNOS CLAUSE POT.FALSE.LITNOS)
	      (DS-CLAUSE.PUT.POTENTIALLY.TRUE.LITNOS CLAUSE POT.TRUE.LITNOS)))
	CLAUSELIST))






(DEFUN OP=CREATE.XLINKS (CLAUSE LITNOS COLOUR CLAUSES RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  A CLAUSE, NIL OR A LIST OF LITERAL NUMBERS
						;         A LINK COLOUR FOR AN EXTERNAL LITERAL LINK,
						;         A LIST OF CLAUSES (CLAUSE MAY BE IN CLAUSES)
						;         RESTRICTION IS EITHER NIL OR THE ATOM
						;         NORULES OR A LIST OF RULE CLAUSES.
						; EFFECT: IF LITNOS = NIL ALL EXTERNAL LITERAL LINKS
						;         BETWEEN CLAUSE AND CLAUSES FOR THE GIVEN
						;         COLOUR ARE CREATED. IF LITNOS =//= NIL
						;         THEN THE LINKS FOR THE LITERALS IN LITNOS
						;         ARE CREATED ONLY.
						;         IF RESTRICTION = NIL THEN THE LINKS FOR ALL
						;         RULES ARE CREATED. IF RESTRICTION = NORULES
						;         THEN LINKS FOR RULE = NIL AND
						;         RULE = SYMMETRIC ARE CREATED ONLY.
						;         IF RESTRICTION = (RULECLAUSE1 ...) THEN
						;         LINKS FOR THESE RULECLAUSES ARE CREATED ONLY
						; VALUE:  UNDEFINED.
  (COND
    (CLAUSES
     (PROG (TERMLIST VARIABLES LASTVAR CLAUSE2 UNIFIERS)
	   (BUFFER.RESET OP*UNIFIER.BUFFER)
	   (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	     (COND ((AND LITNOS (NOT (MEMBER (1+ RPTN) LITNOS))))
		   (T (SETQ VARIABLES (DS-CLAUSE.LIT.VARIABLES CLAUSE (1+ RPTN))) (SETQ LASTVAR (LAST VARIABLES))
		      (SETQ TERMLIST (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN)))
		      (MAPC
			#'(LAMBDA (OTHERSIDES)
			    (MAPC
			      #'(LAMBDA (OCCURRENCE)
				  (COND
				    ((AND (NEQ CLAUSE (SETQ CLAUSE2 (CAR OCCURRENCE)))
					  (MEMBER CLAUSE2 CLAUSES))
				     (MAPC
				       #'(LAMBDA (LITNO2)
					   (MAPC
					     #'(LAMBDA (RULE)
						 (COND
						   ((AND (OP=ADMISSIBLE.RULE RULE RESTRICTION)
							 (SETQ UNIFIERS
							       (OP=UNIFY TERMLIST (DS-CLAUSE.TERMLIST CLAUSE2 LITNO2) RULE)))
						    (BUFFER.CONS UNIFIERS OP*UNIFIER.BUFFER)
						    (BUFFER.CONS RULE OP*UNIFIER.BUFFER))))
					     (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDES))
					   (COND
					     ((SETQ UNIFIERS (BUFFER.CONTENTS OP*UNIFIER.BUFFER))
					      (COND
						(VARIABLES (RPLACD LASTVAR (DS-CLAUSE.LIT.VARIABLES CLAUSE2 LITNO2)))
						(T (SETQ VARIABLES (DS-CLAUSE.LIT.VARIABLES CLAUSE2 LITNO2))))
					      (OP=C.REMOVE.INSTANCES UNIFIERS VARIABLES CLAUSE (1+ RPTN) CLAUSE2 LITNO2
								     COLOUR)
					      (COND (LASTVAR (RPLACD LASTVAR NIL)) (T (SETQ VARIABLES NIL)))
					      (OP=CREATE.LINKS COLOUR CLAUSE (1+ RPTN) CLAUSE2 LITNO2 UNIFIERS)))
					   (BUFFER.RESET OP*UNIFIER.BUFFER))
				       (CDR OCCURRENCE)))))
			      (OP=PREDICATE.OCCURRENCES (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDES)
							(DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDES))))
			(DS-PREDICATE.OTHERSIDES (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))
						 (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)) COLOUR)))))))))



(DEFUN OP=CREATE.XILINKS (CLAUSE LITNOS COLOUR RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  A CLAUSE, NIL OR A LIST OF LITERAL NUMBERS
						;         A LINK COLOUR FOR AN XI-LINK,
						;         RESTRICTION IS EITHER NIL OR THE ATOM
						;         NORULES OR A LIST OF RULE CLAUSES.
						; EFFECT: IF LITNOS = NIL ALL XI-LINKS
						;         FOR CLAUSE WITH THE GIVEN
						;         COLOUR ARE CREATED. IF LITNOS =//= NIL
						;         THEN THE LINKS FOR THE LITERALS IN LITNOS
						;         ARE CREATED ONLY.
						;         IF RESTRICTION = NIL THEN THE LINKS FOR ALL
						;         RULES ARE CREATED. IF RESTRICTION = NORULES
						;         THEN LINKS FOR RULE = NIL AND
						;         RULE = SYMMETRIC ARE CREATED ONLY.
						;         IF RESTRICTION = (RULECLAUSE1 ...) THEN
						;         LINKS FOR THESE RULECLAUSES ARE CREATED ONLY
						; VALUE:  UNDEFINED.
  (PROG ((VARIABLES (DS-CLAUSE.VARIABLES CLAUSE)) TERMLIST UNIFIERS) (BUFFER.RESET OP*UNIFIER.BUFFER)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (COND ((AND LITNOS (NOT (MEMBER (1+ RPTN) LITNOS))))
		(T (SETQ TERMLIST (DS-CLAUSE.TERMLIST CLAUSE (1+ RPTN)))
		   (MAPC #'(LAMBDA (OTHERSIDES)
			     (MAPC #'(LAMBDA (OCCURRENCE)
				       (COND ((EQL CLAUSE (CAR OCCURRENCE))
					      (MAPC #'(LAMBDA (LITNO2)
							(COND ((EQL LITNO2 (1+ RPTN))
							       (OP=CREATE.XI.CIRCLE.LINKS CLAUSE (1+ RPTN) VARIABLES TERMLIST
											  COLOUR OTHERSIDES RESTRICTION))
							      ((or (< LITNO2 (1+ RPTN))
								   (not (member litno2 litnos)))
							       (MAPC #'(LAMBDA (RULE)
									 (COND ((AND (OP=ADMISSIBLE.RULE RULE RESTRICTION)
										     (SETQ UNIFIERS
											   (OP=UNIFY TERMLIST
												     (DS-CLAUSE.TERMLIST
												       CLAUSE LITNO2)
												     RULE)))
										(BUFFER.CONS UNIFIERS OP*UNIFIER.BUFFER)
										(BUFFER.CONS RULE OP*UNIFIER.BUFFER))))
								     (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDES))
							       (COND
								 ((SETQ UNIFIERS (BUFFER.CONTENTS OP*UNIFIER.BUFFER))
								  (OP=C.REMOVE.INSTANCES UNIFIERS VARIABLES CLAUSE (1+ RPTN)
											 CLAUSE LITNO2 COLOUR)
								  (OP=CREATE.LINKS COLOUR CLAUSE (1+ RPTN) CLAUSE LITNO2
										   UNIFIERS)))
							       (BUFFER.RESET OP*UNIFIER.BUFFER))))
						    (CDR OCCURRENCE)))))
				   (OP=PREDICATE.OCCURRENCES (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDES)
							     (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDES))))
			 (DS-PREDICATE.OTHERSIDES (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))
						  (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN))
						  COLOUR)))))))


(DEFUN OP=CREATE.XIWLINKS (CLAUSE LITNOS COLOUR RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  A CLAUSE, NIL OR A LIST OF LITERAL NUMBERS
						;         A LINK COLOUR FOR AN XIW-LINK,
						;         RESTRICTION IS EITHER NIL OR THE ATOM
						;         NORULES OR A LIST OF RULE CLAUSES.
						; EFFECT: IF LITNOS = NIL ALL XIW-LINKS
						;         FOR CLAUSE WITH THE GIVEN
						;         COLOUR ARE CREATED. IF LITNOS =//= NIL
						;         THEN THE LINKS FOR THE LITERALS IN LITNOS
						;         ARE CREATED ONLY.
						;         IF RESTRICTION = NIL THEN THE LINKS FOR ALL
						;         RULES ARE CREATED. IF RESTRICTION = NORULES
						;         THEN LINKS FOR RULE = NIL AND
						;         RULE = SYMMETRIC ARE CREATED ONLY.
						;         IF RESTRICTION = (RULECLAUSE1 ...) THEN
						;         LINKS FOR THESE RULECLAUSES ARE CREATED ONLY
						; VALUE:  UNDEFINED.
  (let ((RENAMING (DS-CLAUSE.RENAMING CLAUSE))
	TERMLIST UNIFIERS)
    (BUFFER.RESET OP*UNIFIER.BUFFER)
    (Do ((litno1 (DS-CLAUSE.NOLIT CLAUSE) (1- litno1)))
	((zerop litno1))
      (unless (AND LITNOS (NOT (MEMBER litno1 LITNOS)))
	(SETQ TERMLIST (DS-CLAUSE.TERMLIST CLAUSE litno1))
	(MAPC #'(LAMBDA (OTHERSIDES)
		  (MAPC #'(LAMBDA (OCCURRENCE)
			    (when (EQL CLAUSE (CAR OCCURRENCE))
			      (MAPC #'(LAMBDA (LITNO2)
					(COND ((= LITNO2 litno1)
					       (OP=CREATE.XIW.CIRCLE.LINKS CLAUSE litno1 RENAMING TERMLIST COLOUR
									   OTHERSIDES RESTRICTION))
					      ((< LITNO2 litno1)
					       (MAPC #'(LAMBDA (RULE)
							 (when (AND (OP=ADMISSIBLE.RULE RULE RESTRICTION)
								    (SETQ UNIFIERS
									  (OP=WEAK.UNIFY TERMLIST
											 (DS-CLAUSE.TERMLIST CLAUSE LITNO2)
											 RENAMING RULE)))
							   (BUFFER.CONS UNIFIERS OP*UNIFIER.BUFFER)
							   (BUFFER.CONS RULE OP*UNIFIER.BUFFER)))
						     (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDES))
					       (COND
						 ((SETQ UNIFIERS (BUFFER.CONTENTS OP*UNIFIER.BUFFER))
						  (OP=C.REMOVE.INSTANCES UNIFIERS RENAMING CLAUSE litno1 CLAUSE LITNO2
									 COLOUR)
						  (OP=CREATE.LINKS COLOUR CLAUSE litno1 CLAUSE LITNO2 UNIFIERS)))
					       (BUFFER.RESET OP*UNIFIER.BUFFER))))
				    (CDR OCCURRENCE))))
			(OP=PREDICATE.OCCURRENCES (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDES)
						  (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDES))))
	      (DS-PREDICATE.OTHERSIDES (DS-CLAUSE.SIGN CLAUSE litno1)
				       (DS-CLAUSE.PREDICATE CLAUSE litno1) COLOUR))))))

(DEFUN OP=CREATE.XI.CIRCLE.LINKS (CLAUSE LITNO VARIABLES TERMLIST COLOUR OTHERSIDES RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  A LITERAL, THE VARIABLES OF CLAUSE, THE
						;         TERMLIST OF THIS LITERAL, A LINK COLOUR
						;         OF AN INTERNAL STRONG LITERAL LINK,
						;         A THE OTHERSIDES LISTS AND THE RULE
						;         RESTRICTION.
						; EFFECT: THE CIRCLE LINKS ARE CREATED
						; VALUE:  UNDEFINED.
  (COND ((AND (MEMBER COLOUR OP*COLOURS.CIRCLE.LINKS)
	      (EQL (DS-CLAUSE.PREDICATE CLAUSE LITNO) (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDES))
	      (EQL (DS-CLAUSE.SIGN CLAUSE LITNO) (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDES)))
	 (let (UNIFIERS)
	   (BUFFER.RESET OP*UNIFIER.BUFFER)
	   (MAPC #'(LAMBDA (RULE)
		     (COND ((AND (CONSP (CDR RULE)) (OP=ADMISSIBLE.RULE RULE RESTRICTION)
				 (SETQ UNIFIERS (OP=UNIFY TERMLIST TERMLIST RULE)))
			    (BUFFER.CONS UNIFIERS OP*UNIFIER.BUFFER) (BUFFER.CONS RULE OP*UNIFIER.BUFFER))))
		 (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDES))
	   (COND ((SETQ UNIFIERS (BUFFER.CONTENTS OP*UNIFIER.BUFFER))
		  (OP=C.REMOVE.INSTANCES UNIFIERS VARIABLES CLAUSE LITNO CLAUSE LITNO COLOUR)
		  (OP=CREATE.LINKS COLOUR CLAUSE LITNO CLAUSE LITNO UNIFIERS) (BUFFER.RESET OP*UNIFIER.BUFFER)))))))

(DEFUN OP=CREATE.XIW.CIRCLE.LINKS (CLAUSE LITNO RENAMING TERMLIST COLOUR OTHERSIDES RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  A LITERAL, THE VARIABLES OF CLAUSE, THE
						;         TERMLIST OF THIS LITERAL, A LINK COLOUR
						;         OF AN INTERNAL STRONG LITERAL LINK,
						;         A THE OTHERSIDES LISTS AND THE RULE
						;         RESTRICTION.
						; EFFECT: THE CIRCLE LINKS ARE CREATED
						; VALUE:  UNDEFINED.
  (COND ((AND (MEMBER COLOUR OP*COLOURS.CIRCLE.LINKS)
	      (EQL (DS-CLAUSE.PREDICATE CLAUSE LITNO) (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDES))
	      (EQL (DS-CLAUSE.SIGN CLAUSE LITNO) (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDES)))
	 (let (UNIFIERS)
	   (BUFFER.RESET OP*UNIFIER.BUFFER)
	   (MAPC #'(LAMBDA (RULE)
		     (COND ((AND (CONSP (CDR RULE)) (OP=ADMISSIBLE.RULE RULE RESTRICTION)
				 (SETQ UNIFIERS
				       (DREMAP (OP=WEAK.UNIFY (COPY-TREE TERMLIST) TERMLIST RENAMING RULE) NIL
					       #'(LAMBDA (TAIL) (UNI-WEAK.INSTANCE (CAR TAIL) RENAMING RENAMING NIL)))))
			    (BUFFER.CONS UNIFIERS OP*UNIFIER.BUFFER) (BUFFER.CONS RULE OP*UNIFIER.BUFFER))))
		 (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDES))
	   (COND ((SETQ UNIFIERS (BUFFER.CONTENTS OP*UNIFIER.BUFFER))
		  (OP=C.REMOVE.INSTANCES UNIFIERS RENAMING CLAUSE LITNO CLAUSE LITNO COLOUR)
		  (OP=CREATE.LINKS COLOUR CLAUSE LITNO CLAUSE LITNO UNIFIERS) (BUFFER.RESET OP*UNIFIER.BUFFER)))))))



(DEFUN OP=CREATE.LINKS (COLOUR CLAUSE1 LITNO1 CLAUSE2 LITNO2 UNIFIER.BUFFER)
						; EDITED: 3. 8. 1984
						; INPUT:  A LINK COLOUR, TWO LITERALS AND A LIST
						;         (RULE1 (S1 S2 ...) RULE2 (S3 S4 ...)  ...)
						;         WITH UNIFIERS FOR THE TWO LITERALS
						; EFFECT: THE NEW LINKS ARE CREATED.
						; VALUE:  UNDEFINED.
  (let (ORIENTATION RULE UNIFIERS)
    (SMAPL #'(LAMBDA (TAIL)
	       (COND ((SETQ UNIFIERS (SECOND TAIL))
		      (SETQ ORIENTATION (CAAR TAIL))
		      (SETQ RULE (CDAR TAIL))
		      (OP=C.RENAME UNIFIERS RULE)
		      (mapc #'(lambda (unifier)
				(CG-INSERT.LINK (COND ((EQL ORIENTATION 'NEGATIVE)
						       (DS-LINK.CREATE COLOUR (list UNIFIER) CLAUSE2 LITNO2 CLAUSE1 LITNO1
								       NIL NIL RULE))
						      (T (DS-LINK.CREATE COLOUR (list UNIFIER) CLAUSE1 LITNO1 CLAUSE2 LITNO2
									 NIL NIL RULE)))
						NIL 'INITIAL))
			    (uni-fit.on.literal.sort UNIFIERs)))))
	   #'CDDR UNIFIER.BUFFER)))

(DEFUN OP=create_P.PD.AND.PIW.LINKS (CLAUSE CREATOR &optional litnos)
  (DECLARE (ignore creator litnos))
						; EDITED:  16-JUN-81 19:06:18
						; Authors: PRCKLN 
						; INPUT:   A CLAUSE THE LINK BY WHICH CLAUSE WAS
						;          CREATED.
						; EFFECT:  ALL LINKS OF KIND P to literals of CLAUSE are newly
						;          created
						; VALUE:   NIL.
  (cons-construct.plinks (list clause)) nil)



(DEFUN OP=PAR_CREATE.POT.TRUE.AND.FALSE.LITNO.FOR.PARAMOD.LIT (CLAUSE LITNO)
						; EDITED: 20-SEP-83                             AP
						; INPUT:  'LITNO' IS A LITERAL OF CLAUSE 'CLAUSE'.
						; EFFECT: THE CLAUSE COMPONENTS POT.TRUE.LITNOS AND
						;         POT.FALSE.LITNOS WILL BE MODIFIED IF LITERAL
						;         'LITNO' IS POT. TRUE OR FALSE.
						; VALUE:  UNDEFINED.
  (PROG
    ((SIGN.IS.POSITIVE (DS-SIGN.IS.POSITIVE (DS-CLAUSE.SIGN CLAUSE LITNO)))
     (PREDICATE (DS-CLAUSE.PREDICATE CLAUSE LITNO)))	; (TERMLIST (DS-CLAUSE.TERMLIST CLAUSE LITNO)))
    (COND
      ((AND
         (OR (AND (NOT SIGN.IS.POSITIVE) (DT-PREDICATE.IS.MARKED REFLEXIVE PREDICATE))
	     (AND (DT-PREDICATE.IS.MARKED IRREFLEXIVE PREDICATE) SIGN.IS.POSITIVE)))
       (DS-CLAUSE.PUT.POTENTIALLY.FALSE.LITNOS CLAUSE
					       (CONS LITNO (DELETE LITNO (DS-CLAUSE.POTENTIALLY.FALSE.LITNOS CLAUSE)))))
      ((AND
         (OR (AND SIGN.IS.POSITIVE (DT-PREDICATE.IS.MARKED REFLEXIVE PREDICATE))
	     (AND (DT-PREDICATE.IS.MARKED IRREFLEXIVE PREDICATE) (NOT SIGN.IS.POSITIVE))))
       (DS-CLAUSE.PUT.POTENTIALLY.TRUE.LITNOS CLAUSE
					      (CONS LITNO (DELETE LITNO (DS-CLAUSE.POTENTIALLY.TRUE.LITNOS CLAUSE))))))))