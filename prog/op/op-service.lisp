;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))


(DEFVAR OP*LINK.COLOURS.LITERAL.INITIAL '(R S T ri SI TI RIW SIW TIW))

(DEFVAR OP*CLAUSECOUNTER 0)

(DEFVAR OP*COUNTER.RESOLVENTS 0)

(DEFVAR OP*COUNTER.PARAMODULANTS 0)

(DEFVAR OP*COUNTER.FACTORS 0)

(DEFVAR OP*COUNTER.instances 0)

(DEFVAR OP*COLOURS.CIRCLE.LINKS '(SIW))

(DEFVAR OP*UNIFIER.BUFFER NIL)

(DEFVAR OP*VARIABLE.BUFFER1 NIL)

(DEFVAR OP*VARIABLE.BUFFER2 NIL)

(DEFUN OP=UNIFY (TERMLIST1 TERMLIST2 RULE)
						; EDITED: 3. 8. 1984
						; INPUT:  TWO TERMLISTS AND A LIST
						;         (ORIENTATION) OR (ORIENTATION . SYMMETRIC)
						;         OR (ORIENTATION . RULEDESCRIPTOR)
						; VALUE:  THE UNIFIERS FOR THE TWO TERMLISTS WITH
						;         RESPECT TO RULE.
  (SETQ RULE (CDR RULE))
  (COND ((NULL RULE) (UNI-UNIFY.TERMLISTS TERMLIST1 TERMLIST2))
	((EQL RULE 'SYMMETRIC) (SETQ RULE op*rule.dummy)
						; RULE IS USED AS A DUMMY FOR STORING THE REVERSED TERMLIST1
	 (RPLACA RULE (SECOND TERMLIST1)) (RPLACA (CDR RULE) (CAR TERMLIST1)) (RPLACD (CDR RULE) (CDDR TERMLIST1))
	 (NCONC (UNI-UNIFY.TERMLISTS TERMLIST1 TERMLIST2)
		(UNI-UNIFY.TERMLISTS RULE TERMLIST2)))
	((CONSP RULE)
	 (PROG ((DUMMY (LAST TERMLIST1)))
	       (COND (DUMMY (RPLACD DUMMY TERMLIST2)) (T (SETQ TERMLIST1 TERMLIST2)))
	       (SETQ RULE
		     (MAPCAN #'(LAMBDA (TERMLIST) (UNI-UNIFY.TERMLISTS TERMLIST TERMLIST1)) (DS-RULE.TERMLISTS RULE)))
	       (COND (DUMMY (RPLACD DUMMY NIL))) (RETURN RULE)))
	(T (ERROR "ILLEGAL RULE IN OP=UNIFY: ~A" RULE))))

(DEFUN OP=match (TERMLIST1 TERMLIST2 RULE)
						; Edited:  26-SEP-1991 19:41
						; Authors: PRCKLN
						; EDITED: 3. 8. 1984
						; INPUT:  TWO TERMLISTS AND A LIST
						;         (ORIENTATION) OR (ORIENTATION . SYMMETRIC)
						;         OR (ORIENTATION . RULEDESCRIPTOR)
						; VALUE:  THE matchers FOR THE TWO TERMLISTS WITH
						;         RESPECT TO RULE.
  (SETQ RULE (CDR RULE))
  (COND ((NULL RULE) (UNI-unify1.TERMLISTS TERMLIST1 TERMLIST2 t))
	((EQL RULE 'SYMMETRIC) (SETQ RULE op*rule.dummy)
						; RULE IS USED AS A DUMMY FOR STORING THE REVERSED TERMLIST1
	 (RPLACA RULE (SECOND TERMLIST1)) (RPLACA (CDR RULE) (CAR TERMLIST1)) (RPLACD (CDR RULE) (CDDR TERMLIST1))
	 (NCONC (UNI-UNIFY1.TERMLISTS TERMLIST1 TERMLIST2 t)
		(UNI-UNIFY1.TERMLISTS RULE TERMLIST2 t)))
	((CONSP RULE)
	 (PROG ((DUMMY (LAST TERMLIST1)))
	       (COND (DUMMY (RPLACD DUMMY TERMLIST2))
		     (T (SETQ TERMLIST1 TERMLIST2)))
	       (SETQ RULE
		     (MAPCAN #'(LAMBDA (TERMLIST) (UNI-UNIFY1.TERMLISTS TERMLIST TERMLIST1 t)) (DS-RULE.TERMLISTS RULE)))
	       (COND (DUMMY (RPLACD DUMMY NIL))) (RETURN RULE)))
	(T (ERROR "ILLEGAL RULE IN OP=UNIFY: ~A" RULE))))

(DEFUN OP=WEAK.UNIFY (TERMLIST1 TERMLIST2 RENAMING RULE)
						; EDITED: 8. 3. 1984
						; INPUT:  TWO TERMLISTS, A RENAMING SUBSTITUTION
						;         AND A LIST (ORIENTATION . RULEDESCRIPTOR)
						; VALUE:  THE WEAK UNIFIERS FOR THE TWO TERMLISTS
  (COND
    ((EQL (CAR RULE) 'NEGATIVE)
     (PROG1 (OP=UNIFY TERMLIST1 (UNI-SWITCH RENAMING TERMLIST2) RULE) (UNI-SWITCH RENAMING TERMLIST2)))
    (T (PROG1 (OP=UNIFY (UNI-SWITCH RENAMING TERMLIST1) TERMLIST2 RULE) (UNI-SWITCH RENAMING TERMLIST1)))))
  
(defun op=c.rplaca (old new list)
  (let ((VARIABLE (MEMBER old list)))
    (when VARIABLE (RPLACA VARIABLE new))))


(defun op=c.nil.components (unifier variables)
						; Edited:  11-FEB-1991 22:21
						; Authors: PRCKLN
						; Input:   UNIFIER is a unifier
						; Effect:  Replaces domain and codomain of UNIFIER
						;          for all variables in VARIABLES by NIL.
						; Value:   Undefined.
  (SMAPL #'(LAMBDA (TAIL)
	     (let ((variable (MEMBER (CAR TAIL) VARIABLES)))
	       (COND (VARIABLE (RPLACA VARIABLE (SECOND TAIL)))
		     (T (RPLACA TAIL NIL)
			(RPLACA (CDR TAIL) NIL)))))
	 #'CDDR UNIFIER))

(DEFUN OP=C.REMOVE.INSTANCES (UNIFIER.BUFFER VARIABLES CLAUSE1 LITNO1 CLAUSE2 LITNO2 COLOUR)
						; EDITED: 3. 8. 1984
						; INPUT:  UNIFIER.BUFFER IS A LIST
						;         (RULE1 (S1 S2 ...) RULE2 (S3 S4 ...) ...)
						;         WITH UNIFIERS FOR (CLAUSE1 LITNO1)
						;                           (CLAUSE2 LITNO2).
						;         VARIABLES ARE THE VARIABLES OF THE TWO
						;         LITERALS.
						;         CONS CELL OF VARIABLES.
						;         COLOUR IS THE LINK COLOUR UNDER CONSTRUCTION
						; EFFECT: THE SUBSTITUTIONS ARE REDUCED TO THE
						;         VARIABLES IN BOTH LITERALS. INSTANCES OF
						;         OTHER SUBSTITUTIONS OR UNIFIERS ATTATCHED
						;         AT ALREADY EXISTING LINKS ARE REMOVED.
						;         UNIFIERS OF ALREADY EXISTING LINKS WHICH ARE
						;         INSTANCES OF NEW UNIFIERS ARE REMOVED TOO.
						;         UNIFIER.BUFFER IS DESTRUCTIVELY MODIFIED AND
						;         MAY CONTAIN EMPTY ELEMENTS.
						; VALUE:  UNDEFINED.
  (unless (DT-FUNCTION.THEORIES)  ; Infeasible with the actual match
    (let ((LINKS (MEMBER-IF #'(LAMBDA (LINK)
				(AND (EQL CLAUSE2 (DS-LINK.OTHERPAR LINK CLAUSE1))
				     (EQL LITNO2 (DS-LINK.OTHERLITNO LINK CLAUSE1 LITNO1))))
			    (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1)))
	  (UNIFS 0))
      (SMAPC #'(LAMBDA (UNIFIERS) (incf UNIFS (LIST-LENGTH UNIFIERS)))
	     #'CDDR
	     (CDR UNIFIER.BUFFER))
      (decf UNIFS)
      (COND ((OR LINKS (NOT (ZEROP UNIFS)))
	     (SETQ UNIFIER.BUFFER (CDR UNIFIER.BUFFER))
	     (SMAPL #'(LAMBDA (BUFFER.TAIL)
			(COND ((OR LINKS (NOT (ZEROP UNIFS)))
			       (MAPL #'(LAMBDA (UNIFIERS)
					 (let ((UNIFIER (CAR UNIFIERS)))
					   (buffer.multiple.cons VARIABLES OP*VARIABLE.BUFFER1)
					   (let ((VARIABLES1 (BUFFER.CONTENTS OP*VARIABLE.BUFFER1)))
					     (op=c.nil.components unifier variables1)
					     (UNI-CONSTANTIFY VARIABLES1)
					     (COND ((OR (AND (NOT (ZEROP UNIFS))
							     (SSOME #'(LAMBDA (UNIFIERS)
									(MEMBER-IF
									  #'(LAMBDA (UNIFIER2)
									      (COND ((AND (NEQ UNIFIER UNIFIER2)
											  (NEQ UNIFIER2 'REMOVED))
										     (buffer.multiple.cons VARIABLES
													   OP*VARIABLE.BUFFER2)
										     (let ((VARIABLES2
											     (BUFFER.CONTENTS OP*VARIABLE.BUFFER2)))
										       (SMAPL #'(LAMBDA (TAIL)
												  (op=c.rplaca (first tail)
													       (second tail)
													       variables2))
											      #'CDDR UNIFIER2)
										       (PROG1 (UNI-MATCHABLE VARIABLES2 VARIABLES1)
											      (BUFFER.RESET OP*VARIABLE.BUFFER2))))))
									  UNIFIERS))
								    #'CDDR UNIFIER.BUFFER))
							(AND LINKS
							     (MEMBER-IF
							       #'(LAMBDA (LINK)
								   (AND (EQL CLAUSE2 (DS-LINK.OTHERPAR LINK CLAUSE1))
									(EQL LITNO2 (DS-LINK.OTHERLITNO LINK CLAUSE1 LITNO1))
									(MEMBER-IF
									  #'(LAMBDA (UNIFIER2)
									      (buffer.multiple.cons VARIABLES OP*VARIABLE.BUFFER2)
									      (let ((VARIABLES2 (BUFFER.CONTENTS OP*VARIABLE.BUFFER2)))
										(SMAPL #'(LAMBDA (TAIL)
											   (op=c.rplaca (first tail)
													(second tail)
													variables2))
										       #'CDDR UNIFIER2)
										(PROG1 (UNI-MATCHABLE VARIABLES2 VARIABLES1)
										       (BUFFER.RESET OP*VARIABLE.BUFFER2))))
									  (DS-LINK.UNIFIERS LINK))))
							       (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1))))
						    (RPLACA UNIFIERS 'REMOVED)
						    (decf UNIFS))
						   (T (RPLACA UNIFIERS (DELETE NIL UNIFIER))))))
					 (BUFFER.RESET OP*VARIABLE.BUFFER1))
				     (CAR BUFFER.TAIL))
			       (RPLACA BUFFER.TAIL (DELETE 'REMOVED (CAR BUFFER.TAIL))))))
		    #'CDDR UNIFIER.BUFFER)
	     (COND (LINKS (MAPC #'(LAMBDA (LINK)
				    (COND ((AND (EQL CLAUSE2 (DS-LINK.OTHERPAR LINK CLAUSE1))
						(EQL LITNO2 (DS-LINK.OTHERLITNO LINK CLAUSE1 LITNO1)))
					   (MAPC
					     #'(LAMBDA (UNIFIER) (CG-REMOVE.UNIFIER LINK UNIFIER NIL "INSTANCE OF A NEW UNIFIER"))
					     (REMOVE-IF-NOT
					       #'(LAMBDA (UNIFIER)
						   (buffer.multiple.cons VARIABLES OP*VARIABLE.BUFFER1)
						   (let ((VARIABLES1 (BUFFER.CONTENTS OP*VARIABLE.BUFFER1)))
						     (SMAPL #'(LAMBDA (TAIL)
								(op=c.rplaca (first tail) (second tail) VARIABLES1))
							    #'CDDR UNIFIER)
						     (UNI-CONSTANTIFY VARIABLES1)
						     (PROG1
						       (SSOME
							 #'(LAMBDA (UNIFIERS)
							     (MEMBER-IF
							       #'(LAMBDA (UNIFIER2)
								   (buffer.multiple.cons VARIABLES OP*VARIABLE.BUFFER2)
								   (let ((VARIABLES2 (BUFFER.CONTENTS OP*VARIABLE.BUFFER2)))
								     (SMAPL #'(LAMBDA (TAIL)
										(op=c.rplaca (first tail) (second tail) VARIABLES2))
									    #'CDDR UNIFIER2)
								     (PROG1 (UNI-MATCHABLE VARIABLES2 VARIABLES1)
									    (BUFFER.RESET OP*VARIABLE.BUFFER2))))
							       UNIFIERS))
							 #'CDDR UNIFIER.BUFFER)
						       (BUFFER.RESET OP*VARIABLE.BUFFER1))))
					       (DS-LINK.UNIFIERS LINK))))))
				(DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1))))
	     (UNI-CLEAR.VARIABLES.AS.CONSTANTS))
	    ((CONSP (CDAR UNIFIER.BUFFER))
	     (op=c.nil.components (CAADR UNIFIER.BUFFER) variables)
	     (RPLACA (SECOND UNIFIER.BUFFER) (DELETE NIL (CAADR UNIFIER.BUFFER))))))))

(DEFUN OP=ADMISSIBLE.RULE (RULE RESTRICTION)
						; EDITED: 3. 8. 1984
						; INPUT:  RULE IS A LIST (ORIENTATION . NIL) OR
						;         (ORIENTATION . SYMMETRIC) OR
						;         (ORIENTATION   RULECLAUSE 1 2) ETC.
						;         RESTRICTION IS EITHER NIL OR THE ATOM
						;         'NORULES OR A LIST OF RULECLAUSES.
						; VALUE:  T IF THE RULE IS COMPATIBLE WITH RESTRICTION
  (SETQ RULE (CDR RULE))
  (COND ((EQL RESTRICTION 'NORULES) (ATOM RULE))
	((CONSP RESTRICTION) (AND (CONSP RULE) (MEMBER (CAR RULE) RESTRICTION)))
	(T)))



(DEFUN OP=PREDICATE.OCCURRENCES (PREDICATE SIGN)
						; EDITED: 3. 8. 1984
						; INPUT:  A PREDICATE AND A SIGN
						; VALUE:  THE OCCURRENCES LISTS FOR THIS PREDICATE
						;         AND SIGN.
  (CASE SIGN
    (+ (DT-PREDICATE.POSITIVE.OCCURRENCES PREDICATE))
    (- (DT-PREDICATE.NEGATIVE.OCCURRENCES PREDICATE))
    (OTHERWISE (ERROR "Illegal sign: ~A" SIGN))))

(DEFMACRO OP=COLOUR.IS.X (COLOUR)
						; EDITED: 5. 8. 84
						; INPUT:  A LINK COLOUR
						; VALUE:  SEE CODE.
  `(MEMBER ,COLOUR '(R S T)))

(DEFMACRO OP=COLOUR.IS.XI (COLOUR)
						; EDITED: 5. 8. 84
						; INPUT:  A LINK COLOUR
						; VALUE:  SEE CODE.
  `(MEMBER ,COLOUR '(SI TI)))

(DEFMACRO OP=COLOUR.IS.XIW (COLOUR)
						; EDITED: 5. 8. 84
						; INPUT:  A LINK COLOUR
						; VALUE:  SEE CODE.
  `(MEMBER ,COLOUR '(RIW SIW TIW)))

(DEFUN OP=C.RENAME (UNIFIERS RULE)
						; EDITED: 5. 8. 1984
						; INPUT:  A LIST OF UNIFIERS AND A RULEDESCRIPTOR
						; EFFECT: IF RULE REPRESENTS A TWO-LITERAL RULE
						;         THEN THE VARIABLES IN UNFIERS STEMMING
						;         FROM THIS RULE ARE DESTRUCTIVELY REPLACED
						;         BY NEWLY CREATED VARIABLES.
						; VALUE:  UNDEFINED.
						; REMARK: OP*VARIABLE.BUFFER1 IS USED]
  (COND
    ((CONSP RULE)
     (PROG ((VARIABLES (DS-CLAUSE.VARIABLES (CAR RULE))))
	   (COND
	     (VARIABLES
	      (MAPC
		#'(LAMBDA (UNIFIER)
		    (MAPC
		      #'(LAMBDA (VARIABLE)
			  (NSUBST (DT-VARIABLE.CREATE (DT-VARIABLE.SORT VARIABLE)) VARIABLE UNIFIER))
		      (OP=FIND.VARIABLES UNIFIER VARIABLES)))
		UNIFIERS)))))))

(DEFUN OP=FIND.VARIABLES (EXPRESSION VARIABLES)
						; EDITED: 5. 8. 1984
						; INPUT:  AN ARBITRARY TERM OR TERMLIST AND A
						;         LIST OF VARIABLES
						; VALUE:  THE LIST OF VARIABLES OCCURRING IN
						;         EXPRESSION AS WELL AS IN VARIABLES
						;         (GATHERED IN OP*VARIABLE.BUFFER1)
  (BUFFER.RESET OP*VARIABLE.BUFFER1)
  (OP=SEARCH.VARIABLES EXPRESSION VARIABLES)
  (PROG1 (BUFFER.CONTENTS OP*VARIABLE.BUFFER1) (BUFFER.RESET OP*VARIABLE.BUFFER1)))

(DEFUN OP=SEARCH.VARIABLES (EXPRESSION VARIABLES)
						; EDITED: 5. 8. 1984
						; INPUT:  AN ARBITRARY TERM OR TERMLIST, A LIST
						;         OF VARIABLES.
						; EFFECT: THE VARIABLES OCCURRING IN EXPRESSION AS
						;         WELL AS IN VARIABLES ARE GATHERED IN
						;         OP*VARIABLE.BUFFER1
						; VALUE:  UNDEFINED.
  (COND
    ((CONSP EXPRESSION)
     (MAPC #'(LAMBDA (EXPRESSION) (OP=SEARCH.VARIABLES EXPRESSION VARIABLES)) EXPRESSION))
    ((AND (DT-VARIABLE.IS EXPRESSION) (MEMBER EXPRESSION VARIABLES)) (BUFFER.INS EXPRESSION OP*VARIABLE.BUFFER1))))

(defun op=generate.passive.positions (termlist unifier)
						; Edited:  20-NOV-1991 19:58
						; Authors: PRCKLN
						; Input:   A termlist and a substitution
						; Effect:  -
						; Value:   A list of all positions in termlist where unifier can be applied.
  (op=generate.passive.positions.termlist termlist (uni-unifier.domain unifier) 1))

(defun op=generate.passive.positions.termlist (termlist vars no)
  (if termlist
      (nconc (op=generate.passive.positions.term (first termlist) vars no)
	     (op=generate.passive.positions.termlist (rest termlist) vars (1+ no)))
      nil))

(defun op=generate.passive.positions.term (term vars no)
  (if (dt-variable.is term)
      (if (member term vars)
	  (list (list no))
	  nil)
      (if (dt-term_c.term.is term)
	  (mapcar #'(lambda (subpos) (cons no subpos))
		  (op=generate.passive.positions.termlist (dt-term_arguments term) vars 1))
	  nil)))

(defun op=insert.positions (clause positions)
						; Edited:  20-NOV-1991 20:20
						; Authors: PRCKLN
						; Input:   A clause and a list of lists of positions,
						;          one for each literal in clause, in non-reverse direction.
						; Effect:  Inserts the positions into the literals
						; Value:   Undefined
  (when positions
    (setq positions (nreverse positions))
    (ds-clause.do #'(lambda (litno)
		      (ds-clause.put.passive.positions clause litno (pop positions)))
		  clause)))

(DEFUN OP=TRANSMIT.LITERALS (CLAUSE LITNO UNIFIER LITLIST ORIGINS POSITIONS &optional subst)
						; EDITED: 23-APR-82 13:06:41")
						;          25. 4. 1983  SB
						; INPUT:  CLAUSE, LITNO, UNIFIER SELF EXPLAINING.
						;         LITLIST IS A LIST OF LITERALS, ORIGINS A
						;         TCONC LIST SUCH THAT THE I-TH ELEMENT
						;         DESCRIBES WHERE THE I-TH LITERAL IN LITLIST
						;         STEMS FROM. POSITIONS is a qconc list describing passive
						;         positions if strategy is Snyder-Lynch.
						;         SUBST is a renaming substitution (Internal links).
						; EFFECT: UNIFIER IS APPLIED TO THE LITERALS OF CLAUSE
						;         EXCEPT THE LITNO-TH. THE RESULTING LITERALS
						;         ARE ADDED TO LITLIST AND ORIGINS and POSITIONS are UPDATED.
						; VALUE:  NEW VALUE OF LITLIST.
  (ds-clause.do #'(lambda (litno1)
		    (unless (eql litno1 LITNO) ; LITNO may be NIL (instantiation rule).
		      (let ((termlist (if subst
					  (UNI-APPLY.SUBSTITUTION subst (DS-CLAUSE.TERMLIST CLAUSE litno1) T)
					  (DS-CLAUSE.TERMLIST CLAUSE litno1))))
			(push (DS-LIT.CREATE (DS-CLAUSE.SIGN CLAUSE litno1)
					     (DS-CLAUSE.PREDICATE CLAUSE litno1)
					     (UNI-APPLY.SUBSTITUTION UNIFIER termlist T))
			      LITLIST)
			(when (eq 'snyder-lynch (opt-get.option er_paramodulation))
			  (qcons (op=generate.passive.positions termlist unifier) positions))
			(QCONS (LIST (LIST CLAUSE litno1)) ORIGINS))))
		clause)
  LITLIST)

(DEFUN OP=PAR_TRANSMIT.LITERAL (CLAUSE LITNO FCT TERM UNIFIER LITLIST ORIGINS positions &optional subst)
						; EDITED: 15-JUL-80 11:41:25")
						;         25. 4. 1983  SB
						; INPUT:  CLAUSE, LITNO DENOTE A LITERAL(NO EQUALITY),
						;         FCT IS THE ACCESS FUNCTION TO SOME TERM IN
						;         THE TERMLIST OF THIS LITERAL.
						;         TERM IS ONE SIDE OF AN EQUALITY, UNIFIER IS
						;         A SUBSTITUTION.
						;         LITLIST IS A LIST OF LITERALS, ORIGINS A
						;         TCONC LIST SUCH THAT THE I-TH ELEMENT
						;         DESCRIBES WHERE THE I-TH LITERAL IN LITLIST
						;         STEMS FROM. POSITIONS is a qconc list describing passive
						;         positions if strategy is Snyder-Lynch.
						;         SUBST is a renaming substitution (e.g. for internal operations).
						; EFFECT: UNIFIER IS APPLIED TO THE LITERAL AND THE
						;         EQUALITY TERM, WHICH THEREAFTER IS SUBSTITU-
						;         TED FOR THE TERM ACCESSED BY FCT IN LITERAL.
						;         THE MODIFIED LITERAL IS ADDED TO LITLIST AND
						;         ORIGINS IS UPDATED.
						; VALUE:  NEW VALUE OF LITLIST.
  (let ((TERMLIST (DS-CLAUSE.TERMLIST CLAUSE LITNO)))    
    (when (eq 'snyder-lynch (opt-get.option er_paramodulation))
      (qcons (cons fct (op=generate.passive.positions termlist unifier)) positions))
    (SETQ TERM (UNI-APPLY.SUBSTITUTION subst TERM T))
    (SETQ TERMLIST (UNI-APPLY.SUBSTITUTION UNIFIER TERMLIST T))
    (SETQ TERM (UNI-APPLY.SUBSTITUTION UNIFIER TERM T))
    (DT-REPLACE.TERM.IN.TERMLIST TERM FCT TERMLIST)
    (push (DS-LIT.CREATE (DS-CLAUSE.SIGN CLAUSE LITNO) (DS-CLAUSE.PREDICATE CLAUSE LITNO) TERMLIST) LITLIST)
    (QCONS (LIST (LIST CLAUSE LITNO)) ORIGINS) 
    LITLIST))

(DEFUN OP=RES_NEWNAME NIL
						; EDITED:12-FEB-82 15:51:21")
						; VALUE:  NEW NAME FOR A RESOLVENT.
  (format nil "R~A" (incf OP*COUNTER.RESOLVENTS)))

(DEFUN OP=PAR_NEWNAME NIL
						; EDITED:12-FEB-82 15:48:38")
						; VALUE:  NEW NAME FOR A PARAMODULANT
  (format nil "P~A" (incf OP*COUNTER.PARAMODULANTS)))

(DEFUN OP=FAC_NEWNAME (PARNAME)
						; EDITED:15-FEB-82 10:27:55")
						; VALUE:  NEW NAME FOR A FACTOR, PARNAME IS INTEGRATED
  (format nil "~A.F~A" parname (incf OP*COUNTER.FACTORS)))

(DEFUN OP=INST_NEWNAME (PARNAME)
						; EDITED:15-FEB-82 10:27:55")
						; VALUE:  NEW NAME FOR A INSTTOR, PARNAME IS INTEGRATED
  (format nil "~A.I~A" parname (incf OP*COUNTER.INSTances)))


(DEFUN OP=RENAME (RENAMING OBJECT)
						; EDITED:  10. 2. 1984
						; INPUT:  A RENAMING SUBSTITUTION AND AN ARBITRARY
						;         S-EXPRESSION.
						; EFFECT: THE RENAMING SUBSTITUTION IS
						;         DESTRUCTIVELY APPLIED TO THE OBJECT.
						; VALUE:  THE RENAMED OBJECT.
  (SMAPL #'(LAMBDA (TAIL) (NSUBST (SECOND TAIL) (first TAIL) OBJECT))
	 #'CDDR
	 RENAMING)
  OBJECT)

(DEFUN OP=REMOVE.COMPONENTS (UNIFIERS VARIABLES)
						; EDITED:  10. 2. 1984
						; INPUT:   A LIST OF SUBSTITUTIONS AND A VARIABLE LIST
						; EFFECT:  THE SUBSTITUTION COMPONENTS FOR VARIABLES
						;          NOT OCCURRING IN VARIABLES ARE
						;          REMOVED.
						; VALUE:   THE DESTRUCTIVELY CHANGED UNIFIERS.
  (MAPL
    #'(LAMBDA (TAIL)
        (COND
          ((CAR TAIL)
	   (RPLACA TAIL
		   (PROGN (SETQ TAIL (CAR TAIL))
			  (WHILE (AND TAIL (NOT (MEMBER (CAR TAIL) VARIABLES))) (SETQ TAIL (CDDR TAIL)))
			  (SMAPL
			    #'(LAMBDA (REST) (COND ((NOT (MEMBER (SECOND REST) VARIABLES)) (RPLACD REST (CDDDR REST)))))
			    #'CDDR (CDR TAIL))
			  TAIL)))
          (T (RETURN-FROM OP=REMOVE.COMPONENTS (LIST NIL)))))
    UNIFIERS)
  UNIFIERS)

(DEFUN OP=REMOVE.INSTANCES (XD.UNIFIERS X.UNIFIERS VARIABLES)
						; EDITED: 10. 2. 1984
						; INPUT:  TWO ASSOCIATION LISTS
						;         ((LINK SUBST1 SUBST2 ...)(LINK ...))
						;         AND A LIST OF VARIABLES.
						; EFFECT: THE SUBSTITUTIONS BEEING WEAK INSTANCES
						;         (ACCORDING TO THE GIVEN VARIABLES)
						;         OF ANOTHER SUBSTITUTION IN ONE OF THE
						;         LISTS ARE REMOVED. IF A SUBSTITUTION IN
						;         XD.UNIFIERS IS EQUAL TO SOME IN X.UNIFIERS
						;         THE ONE IN X.UNIFIERS IS REMOVED.
						;         EVERY VARIABLE IN THE REMAINING
						;         SUBSTITUTIONS WHICH IS NOT IN VARIABLES IS
						;         RENAMED.
						; VALUE:  (NCONC XD.UNIFIERS' X.UNIFIERS')
  (PROG (UNIFIER)
	(SETQ X.UNIFIERS
	      (DREMAP X.UNIFIERS NIL
		      #'(LAMBDA (L.UNIFIERS) (SETQ L.UNIFIERS (CAR L.UNIFIERS))
				(MAPL
				  #'(LAMBDA (TAIL)
				      (WHILE (SECOND TAIL) (SETQ UNIFIER (SECOND TAIL))
					     (COND
					       ((OR
						  (MEMBER-IF
						    #'(LAMBDA (L.UNIFIERS1)
							(MEMBER-IF
							  #'(LAMBDA (UNIFIER1) (UNI-WEAK.INSTANCE UNIFIER UNIFIER1 VARIABLES NIL T))
							  (CDR L.UNIFIERS1)))
						    XD.UNIFIERS)
						  (MEMBER-IF
						    #'(LAMBDA (L.UNIFIERS1)
							(MEMBER-IF
							  #'(LAMBDA (UNIFIER1)
							      (AND (NEQ UNIFIER UNIFIER1)
								   (UNI-WEAK.INSTANCE UNIFIER UNIFIER1 VARIABLES NIL T)))
							  (CDR L.UNIFIERS1)))
						    X.UNIFIERS))
						(RPLACD TAIL (CDDR TAIL)) T)
					       (T (SETQ TAIL NIL)))))
				  L.UNIFIERS)
				(NULL (CDR L.UNIFIERS)))))
	(SETQ XD.UNIFIERS
	      (DREMAP XD.UNIFIERS NIL
		      #'(LAMBDA (L.UNIFIERS) (SETQ L.UNIFIERS (CAR L.UNIFIERS))
				(MAPL
				  #'(LAMBDA (TAIL)
				      (WHILE (SECOND TAIL) (SETQ UNIFIER (SECOND TAIL))
					     (COND
					       ((OR
						  (MEMBER-IF
						    #'(LAMBDA (L.UNIFIERS1)
							(MEMBER-IF
							  #'(LAMBDA (UNIFIER1)
							      (AND (NEQ UNIFIER UNIFIER1)
								   (UNI-WEAK.INSTANCE UNIFIER UNIFIER1 VARIABLES NIL)))
							  (CDR L.UNIFIERS1)))
						    XD.UNIFIERS)
						  (MEMBER-IF
						    #'(LAMBDA (L.UNIFIERS1)
							(MEMBER-IF
							  #'(LAMBDA (UNIFIER1) (UNI-WEAK.INSTANCE UNIFIER UNIFIER1 VARIABLES NIL))
							  (CDR L.UNIFIERS1)))
						    X.UNIFIERS))
						(RPLACD TAIL (CDDR TAIL)) T)
					       (T (SETQ TAIL NIL)))))
				  L.UNIFIERS)
				(NULL (CDR L.UNIFIERS)))))
	(RETURN
	  (COND
	    ((DT-UNI.CREATES.VARIABLES)
	     (NCONC (OP=DISJOINTIFY XD.UNIFIERS VARIABLES) (OP=DISJOINTIFY X.UNIFIERS VARIABLES)))
	    (T (NCONC XD.UNIFIERS X.UNIFIERS))))))

(DEFUN OP=DISJOINTIFY (UNIFIERS VARIABLES)
						; EDITED: 10. 2. 1984
						; INPUT:  A LIST OF SUBSTITUTIONS AND A LIST OF
						;         VARIABLES.
						; EFFECT: EACH VARIABLE OCCURRING IN UNIFIERS
						;         WHICH IS NOT IN VARIABLES IS REPLACED
						;         BY A NEWLY CREATED VARIABLE.
						; VALUE:  THE DESTRUCTIVELY RENAMED UNIFIERS.
  (PROG (VARS NEWVARS)
	(MAPC
	  #'(LAMBDA (UNIFIER)
	      (COND
		(UNIFIER (SETQ VARS (NSET-DIFFERENCE (DT-TERMLIST.VARIABLES UNIFIER) VARIABLES))
			 (SETQ NEWVARS
			       (MAPCAR #'(LAMBDA (VAR) (DT-VARIABLE.CREATE (DT-VARIABLE.SORT VAR))) VARS))
			 (RPLACD UNIFIER (SUBPAIR NEWVARS VARS (CDR UNIFIER))))))
	  UNIFIERS)
	(RETURN UNIFIERS)))

(DEFUN OP=PARALLEL.LINKS (COLOUR CLAUSE1 LITNO1 CLAUSE2 LITNO2)
						; EDITED: 12. 2. 1984
						; INPUT:  A LINK COLOUR AND TWO LITERAL ADDRESSES
						; VALUE:  A LIST OF ALL LINKS OF THE GIVEN COLOUR
						;         BETWEEN THESE TWO LITERALS.
						; EFFECT: THESE LINKS ARE MARKED WITH A PROPERTY
						;         INHERITED.
  (let ((LINKS (DS-CLAUSE.LINKS COLOUR CLAUSE2 LITNO2)))
    (REMOVE-IF-NOT #'(LAMBDA (LINK)
		       (when (and (MEMBER LINK LINKS) (NOT (DT-GETPROP LINK 'INHERITED))
				  #-:mkrp-basic(not (nar-narrow.info link)))
			 (DT-PUTPROP LINK 'INHERITED T)
			 T))
		   (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1))))

(DEFUN OP=PARALLEL.LINKS.1 (COLOUR FLAG CLAUSE1 LITNO1 CLAUSE2 LITNO2)
						; EDITED: 12. 2. 1984
						; INPUT:  A LINK COLOUR, A BOOLEAN VALUE AND TWO
						;         LITERAL ADDRESSES.
						; VALUE:  IF FLAG = T:
						;         A LIST OF THOSE LINKS BETWEEN THE TWO
						;         LITERALS WHICH ARE EITHER FREE ORIENTED
						;         OR (CLAUSE1 LITNO1) IS THE POSITIVE SIDE.
						;         IF FLAG = NIL:
						;         A LIST OF THOSE LINKS BETWEEN THE TWO
						;         LITERALS WHICH ARE NOT FREE ORIENTED AND
						;         (CLAUSE1 LITNO1) IS THE POSITIVE SIDE.
						; EFFECT: THSES LINKS ARE MARKED WITH A PROPERTY
						;         'INHERITED.
  (COND
    ((OR (NEQ CLAUSE1 CLAUSE2) (NEQ LITNO1 LITNO2))
     (PROG ((LINKS (DS-CLAUSE.LINKS COLOUR CLAUSE2 LITNO2)))
	   (RETURN
	     (COND
	       (FLAG
		(REMOVE-IF-NOT
		  #'(LAMBDA (LINK)
		      (COND
			((NOT (DS-RULE.ORIENTED (DS-LINK.RULE LINK) COLOUR))
			 (COND ((MEMBER LINK LINKS) (DT-PUTPROP LINK 'INHERITED T) T)))
			((AND (EQL CLAUSE1 (DS-LINK.POSPAR LINK))
			      (OR (MEMBER COLOUR (DS-LINK.COLOURS.FOR 'AUTOLINKS)) (EQL CLAUSE2 (DS-LINK.NEGPAR LINK)))
			      (EQL LITNO2 (DS-LINK.NEGLITNO LINK)))
			 (DT-PUTPROP LINK 'INHERITED T) T)))
		  (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1)))
	       (T
		(REMOVE-IF-NOT
		  #'(LAMBDA (LINK)
		      (COND
			((AND (DS-RULE.ORIENTED (DS-LINK.RULE LINK) COLOUR) (EQL CLAUSE1 (DS-LINK.POSPAR LINK))
			      (OR (MEMBER COLOUR (DS-LINK.COLOURS.FOR 'AUTOLINKS)) (EQL CLAUSE2 (DS-LINK.NEGPAR LINK)))
			      (EQL LITNO2 (DS-LINK.NEGLITNO LINK)))
			 (DT-PUTPROP LINK 'INHERITED T) T)))
		  (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1)))))))
    (FLAG
     (REMOVE-IF-NOT
       #'(LAMBDA (LINK)
	   (AND (EQL (DS-LINK.POSLITNO LINK) (DS-LINK.NEGLITNO LINK))
		(OR (NOT (DS-RULE.ORIENTED (DS-LINK.RULE LINK) COLOUR)) (EQL 1 (SECOND (DS-LINK.RULE LINK))))
		(PROGN (DT-PUTPROP LINK 'INHERITED T) T)))
       (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1)))
    (T
     (REMOVE-IF-NOT
       #'(LAMBDA (LINK)
	   (AND (EQL (DS-LINK.POSLITNO LINK) (DS-LINK.NEGLITNO LINK)) (DS-RULE.ORIENTED (DS-LINK.RULE LINK) COLOUR)
		(EQL 2 (SECOND (DS-LINK.RULE LINK))) (PROGN (DT-PUTPROP LINK 'INHERITED T) T)))
       (DS-CLAUSE.LINKS COLOUR CLAUSE1 LITNO1)))))

(DEFmacro OP=COLOUR (COLOUR TARGET)
						; EDITED: 12. 2. 1984
						; INPUT:  A LINK COLOUR AND AN ATOM E,I OR IW
						; VALUE:  SEE CODE.
  (cond ((constantp target)
	 (setq target (eval target))
	 (CASE TARGET
	   (E
	     `(let ((colour ,colour))
		(CASE COLOUR (RIW 'R) (RIWD 'RD) (SIW 'S) (TIW 'T) (OTHERWISE (ERROR "ILLEGAL COLOUR IN OP=COLOUR: ~A" COLOUR)))))
	   (I
	     `(let ((colour ,colour))
		(CASE COLOUR
		  (RIW 'TI) (RIWD 'TI) (SIW 'SI) (TIW 'TI) (R 'TI) (RD 'TI) (S 'SI)
		  ((T) 'TI)
		  (OTHERWISE (ERROR "ILLEGAL COLOUR IN OP=COLOUR: ~A" COLOUR)))))
	   (IW
	     `(let ((colour ,colour))
		(CASE COLOUR
		  (R 'RIW) (RD 'RIWD) (S 'SIW) ((T) 'TIW) (RIW 'RIW) (RIWD 'RIWD) (SIW 'SIW) (TIW 'TIW)
		  (OTHERWISE (ERROR "ILLEGAL COLOUR IN OP=COLOUR: ~A" COLOUR)))))
	   (OTHERWISE `(OP==COLOUR ,COLOUR ,TARGET))))
	(t `(OP==COLOUR ,COLOUR ,TARGET))))

(DEFUN OP==COLOUR (COLOUR TARGET)
						; EDITED: 12. 2. 1984
						; INPUT:  A LINK COLOUR AND AN ATOM E,I OR IW
						; VALUE:  SEE CODE.
  (CASE TARGET
    (E
      (CASE COLOUR (RIW 'R) (RIWD 'RD) (SIW 'S) (TIW 'T) (OTHERWISE (ERROR "ILLEGAL COLOUR IN OP=COLOUR: ~A" COLOUR))))
    (I
      (CASE COLOUR
	(RIW 'TI) (RIWD 'TI) (SIW 'SI) (TIW 'TI) (R 'TI) (RD 'TI) (S 'SI)
	((T) 'TI)
	(OTHERWISE (ERROR "ILLEGAL COLOUR IN OP=COLOUR: ~A" COLOUR))))
    (IW
      (CASE COLOUR (R 'RIW) (RD 'RIWD) (S 'SIW) ((T) 'TIW) (RIW 'RIW) (RIWD 'RIWD) (SIW 'SIW) (TIW 'TIW)
	    (OTHERWISE (ERROR "ILLEGAL COLOUR IN OP=COLOUR: ~A" COLOUR))))
    (OTHERWISE (ERROR "ILLEGAL TARGET IN OP=COLOUR: ~A" TARGET))))

(DEFUN OP=REMOVE.PROPERTY (COLOURS CLAUSE LITNO)
						; EDITED:  "25-AUG-1989 18:25"
						; INPUT:  A LIST OF LINK COLOURS AND A LITERAL ADDRESS
						; EFFECT: THE PROPERTY 'INHERITED IS REMOVED FROM
						;         ALL LINKS WITH A COLOUR IN COLOURS
						;         CONNECTED TO THE LITERAL.
						; VALUE:  UNDEFINED.
  (MAPC
    #'(LAMBDA (COLOUR)
        (MAPC #'(LAMBDA (LINK) (DT-REMPROP LINK 'INHERITED)) (DS-CLAUSE.LINKS COLOUR CLAUSE LITNO)))
    COLOURS))