;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(in-package "MKRP")

(DEFVAR CONS*CLAUSES NIL)

(DEFVAR CONS*CLAUSECOUNTER NIL)

(DEFVAR CONS*LINK.COLOURS NIL)

(DEFVAR CONS*EQ.OCCURRENCES.NEG NIL)

(DEFVAR CONS*EQ.OCCURRENCES.POS NIL)

(DEFVAR CONS*RW.CLAUSES NIL)

(DEFVAR CONS*RW_EQ.OCCURRENCES.NEG NIL)

(DEFVAR CONS*RW_EQ.OCCURRENCES.POS NIL)

(DEFVAR CONS*NO.LINK.CLAUSES NIL)


(defun cons=sort (sorts)
  (if (opt-get.option sort_literals)
      (dt-constant.omega)
      (first sorts)))

(DEFUN cons-CONSTRUCT.ATTRIBUTE.CLAUSES NIL
						; EDITED: 5. 3. 1982     KHB
						; EFFECT:  FOR EACH PREDICATE:
						;          IF SYMBOL HAS THE    ATTRIBUTE
						;          REFLEXIVE A REFLEXIVITY CLAUSE IS CREATED
						;          AND INSERTED INTO THE GRAPH,
						;          IF SYMBOL HAS THE ATTRIBUTE IRREFLEXIVE
						;          IRREFLEXIVITY CLAUSE IS
						;          CREATED AND INSERTED INTO THE GRAPH.
						; VALUE:   A LIST OF THE NEW CLAUSE ADDRESSES.
  (let (ATTRIBUTES DOMAINSORTS VARIABLE NEW.CLAUSE NEW.CLAUSES)
    (MAPC
      #'(LAMBDA (PREDICATE)
	  (COND
	    ((NOT (DT-PREDICATE.REFL.CLAUSE PREDICATE))
	     (SETQ ATTRIBUTES (DT-PREDICATE.ATTRIBUTES PREDICATE))
	     (SETQ DOMAINSORTS (DT-PREDICATE.DOMAINSORTS PREDICATE))
	     (COND
	       ((AND (INTERSECTION '(REFLEXIVE IRREFLEXIVE) ATTRIBUTES)
		     (NOT (AND (EQL 2 (LIST-LENGTH DOMAINSORTS)) (EQL (CAR DOMAINSORTS) (SECOND DOMAINSORTS)))))
		(ERROR "ILLEGAL ATTRIBUTES FOR PREDICATE IN CONS=CREATE.SYMBOLS: ~A"
		       (LIST ATTRIBUTES (DS-PNAME PREDICATE)))))
	     (COND
	       ((MEMBER 'REFLEXIVE ATTRIBUTES)
		(SETQ VARIABLE (DT-VARIABLE.CREATE (CONS=SORT DOMAINSORTS)))
		(SETQ NEW.CLAUSE
		      (DS-CLAUSE.CREATE (format nil "R.~A" (DS-PNAME PREDICATE)) 'AXIOM 0
					(LIST (DS-LIT.CREATE '+ PREDICATE (LIST VARIABLE VARIABLE)))))
		(CG-INSERT.CLAUSE NEW.CLAUSE NIL NIL ATTRIBUTES NIL)
		(DS-CLAUSE.ADD.ATTRIBUTES NEW.CLAUSE '(REFLEXIVITY))
		(SETQ NEW.CLAUSES (CONS NEW.CLAUSE NEW.CLAUSES)))
	       ((MEMBER 'IRREFLEXIVE ATTRIBUTES) (SETQ VARIABLE (DT-VARIABLE.CREATE (CONS=SORT DOMAINSORTS)))
		(SETQ NEW.CLAUSE
		      (DS-CLAUSE.CREATE (format nil "IRR.~A" (DS-PNAME PREDICATE)) 'AXIOM 0
					(LIST (DS-LIT.CREATE '- PREDICATE (LIST VARIABLE VARIABLE)))))
		(CG-INSERT.CLAUSE NEW.CLAUSE NIL NIL ATTRIBUTES NIL)
		(DS-CLAUSE.ADD.ATTRIBUTES NEW.CLAUSE '(IRREFLEXIVITY))
		(SETQ NEW.CLAUSES (CONS NEW.CLAUSE NEW.CLAUSES)))))))
      (DT-PREDICATE.ALL))
    (MAPC #'(LAMBDA (function)
	      (when (or (DT-function.is.marked ac1 function) (DT-function.is.marked ac function))
		(let* ((v1 (dt-variable.create 'any))
		       (v2 (dt-variable.create 'any))
		       (v3 (dt-variable.create 'any))
		       (term1 (dt-term_create function (list v1 (dt-term_create function (list v2 v3)))))
		       (term2 (dt-term_create function (list (dt-term_create function (list v1 v2)) v3)))
		       (clause (DS-CLAUSE.CREATE (format nil "A.~A" (dt-pname function)) nil 0
						 (list (ds-lit.create '+ (first (DT-PREDICATE.EQUALITIES))
								      (list term1 term2))))))
		  (ds-clause.irreducible.set clause 1)
		  (DT-PUTPROP CLAUSE 'MISSING.LINKS (DS-LINK.COLOURS.FOR 'INITIAL))
		  (dt-putprop clause 'cons*a function)
		  (CG-INSERT.CLAUSE CLAUSE NIL NIL ATTRIBUTES NIL)
		  (push clause new.clauses)))
	      (COND
		((DT-function.is.marked ac1 function)
		 (SETQ VARIABLE (DT-VARIABLE.CREATE 'any))
		 (SETQ NEW.CLAUSE
		       (DS-CLAUSE.CREATE (format nil "AC1.~A" (DS-PNAME FUNCTIOn)) 'AXIOM 0
					 (LIST (DS-LIT.CREATE '+ (first (DT-PREDICATE.EQUALITIES))
							      (LIST (list function (dt-getprop function 'dt*null) variable)
								    variable)))))
		 (dt-putprop new.clause 'cons*ac1 function)
		 (CG-INSERT.CLAUSE NEW.CLAUSE NIL NIL ATTRIBUTES NIL)
		 (ds-clause.rewrite.rule.set new.clause 1) ; Gesperrt gegen Loeschung durch fehlende true litnos
		 (push NEW.CLAUSE NEW.CLAUSES))))
	  (DT-function.ALL))
    NEW.CLAUSES))

(DEFUN CONS-CONSTRUCT.LINKS (CLAUSES LINK.COLOURS)
						; EDITED:  7. 4. 1982    KHB
						; INPUT:   A LIST OF ADDRESSES OF CLAUSES OF THE
						;          ACTUAL GRAPH. THE SECOND ARGUMENT MAY BE A
						;          LINK.COLOUR A LIST OF LINK.COLOURS OR NIL.
						;          F AND IS AS T AND IR AS P AND IP ARE ONLY
						;          IN THE LIST TOGETHER.
						; EFFECT:  FOR EACH LINK.COLOUR IN LINK.COLOURS
						;          ALL LINKS CONNECTING CLAUSES AMONG
						;          THEMSELVES AND TO THE REST OF THE GRAPH
						;          ARE CREATED AND BECOME PART OF THE ACTUAL
						;          GRAPH. (IF LINK.COLOURS IS NIL THEN THE
						;          LINKS FOR ALL EXISTING INITIAL COLOURS
						;          ARE CREATED.)
						; VALUE:   UNDEFINED
  (SETQ LINK.COLOURS
	(COND ((NOT LINK.COLOURS) (DS-LINK.COLOURS.FOR 'INITIAL))
	      ((ATOM LINK.COLOURS) (LIST LINK.COLOURS))
	      (T LINK.COLOURS)))
  (CONS=CONSTRUCT.LINKS (SET-DIFFERENCE CLAUSES CONS*NO.LINK.CLAUSES)
			(INTERSECTION LINK.COLOURS (DS-LINK.COLOURS.FOR 'INITIAL))))

(DEFUN CONS-RESET NIL
						; EDITED:  7. 4. 1982
						; INPUT:   NONE
						; EFFECT:  MAKES THE EMPTY GRAPH THE ACTUAL GRAPH
						; VALUE:   UNDEFINED
  (SETQ CONS*NO.LINK.CLAUSES NIL)
  (CG-CREATE.EMPTY.GRAPH))

(DEFUN CONS-VIRTUAL.GRAPH (FILE)
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
  (PROG
    ((S-EXPRESSION
       (CONS 'PROGN
	     (MAPCAR #'(LAMBDA (ATOM) (LIST 'setq ATOM `',(EVAL ATOM)))
		     '(CONS*CLAUSES CONS*CLAUSECOUNTER CONS*LINK.COLOURS CONS*EQ.OCCURRENCES.NEG CONS*EQ.OCCURRENCES.POS
				    CONS*RW.CLAUSES CONS*RW_EQ.OCCURRENCES.NEG CONS*RW_EQ.OCCURRENCES.POS
				    CONS*NO.LINK.CLAUSES)))))
    (COND (FILE (OP-SAVE FILE) (PROGN (PRINC S-EXPRESSION FILE) (TERPRI FILE)))
	  (T (RETURN (LIST 'PROGN (OP-SAVE NIL) S-EXPRESSION))))))

(DEFUN CONS-RECONSTRUCT.LINKS (RW.RULES RW.LITERALS)
						; EDITED: 19-SEP-83 15:52:29           NB
						; INPUT:  LISTS OF THE FOLLOWING FORM:
						;         RW.RULES (R1 ... RK),
						;         RW.LITERALS:  (C1 L11 .. L1M1)
						;                         :   :      :
						;                       (CN LN1 .. LNMN))
						;
						;          AT WHICH R1..R2 ARE RW.RULES, C1..CN ARE
						;         RW.CLAUSES AND LJ1 .. LJMJ ARE THE MODIFIED
						;         LITERALS OF CJ WITH J = 1 ..N.
						; Remark: If a list contains no literals, the links to all literals are constructed
						; EFFECT: DELETES THE LINKS OF RW.RULES AND
						;         RW.LITERALS AND CREATES THEIR NEW LINKS
						; VALUE : UNDEFINED.
  (SETQ CONS*NO.LINK.CLAUSES (UNION RW.RULES CONS*NO.LINK.CLAUSES))
  (SETQ CONS*RW.CLAUSES (MAPCAR (FUNCTION CAR) RW.LITERALS))
  (MAPC #'(LAMBDA (RW.RULE)
	    (MAPC #'(LAMBDA (LINK)
		      (CG-REMOVE.LINK LINK 'REWRITE))
		  (DS-CLAUSE.LINKS (DS-LINK.COLOURS.FOR 'ALL) RW.RULE 1)))
	RW.RULES)
  (MAPC #'(LAMBDA (RW.CLAUSE)
	    (MAPC #'(LAMBDA (LITNO)
		      (MAPC #'(LAMBDA (LINK)
				(CG-REMOVE.LINK LINK 'REWRITE))
			    (DS-CLAUSE.LINKS (DS-LINK.COLOURS.FOR 'ALL) (CAR RW.CLAUSE) LITNO)))
		  (CDR RW.CLAUSE)))
	RW.LITERALS)
  (CONS=RW_CONSTRUCT.LINKS RW.LITERALS))

(DEFUN CONS=CONSTRUCT.LINKS (CLAUSES COLOURS)
						; EDITED:  4-OCT-83 15:08:19
						; INPUT:   A LIST OF CLAUSES AND A LIST OF
						;          LINK COLOURS
						; EFFECT:  SEE CONS-CONSTRUCT.LINKS
						; VALUE:   UNDEFINED
  (SETQ CONS*CLAUSES CLAUSES)
  (SETQ CONS*LINK.COLOURS COLOURS)
  (SETQ CONS*EQ.OCCURRENCES.POS (CONS=EQ.OCCURRENCES '+))
  (SETQ CONS*EQ.OCCURRENCES.NEG (CONS=EQ.OCCURRENCES '-))
  (let ((OLD.CLAUSES (SET-DIFFERENCE (CG-CLAUSES ALL) CLAUSES)))
    (MAPC #'(LAMBDA (CLAUSE)
	      (OP-CONSTRUCT.LITERAL.LINKS CLAUSE NIL COLOURS OLD.CLAUSES NIL)
	      (SETQ OLD.CLAUSES (CONS CLAUSE OLD.CLAUSES)))
	  CLAUSES))
  (when (AND CONS*EQ.OCCURRENCES.POS (intersection '(p piw) CONS*LINK.COLOURS))
    (CONS=CREATE.PLINKS))
  (MAPC #'(LAMBDA (CLAUSE)
	    (DT-PUTPROP CLAUSE 'MISSING.LINKS (SET-DIFFERENCE (DT-GETPROP CLAUSE 'MISSING.LINKS) CONS*LINK.COLOURS))
	    (COND ((NOT (DT-GETPROP CLAUSE 'MISSING.LINKS)) (DT-REMPROP CLAUSE 'MISSING.LINKS))))
	CLAUSES))

(defun cons-construct.plinks (clauses)
  (SETQ CONS*CLAUSES CLAUSES)
  (SETQ CONS*LINK.COLOURS '(p piw))
  (SETQ CONS*EQ.OCCURRENCES.POS (CONS=EQ.OCCURRENCES '+))
  (SETQ CONS*EQ.OCCURRENCES.NEG (CONS=EQ.OCCURRENCES '-))
  (when CONS*EQ.OCCURRENCES.POS (CONS=CREATE.PLINKS)))

(DEFUN CONS=CREATE.PLINKS NIL			; EDITED:  14. 4. 1982      KHB
						; INPUT:   NONE
						; EFFECT:  IF P IS IN CONS*LINK.COLOURS THEN
						;          CREATES ALL POSSIBLE P-LINKS BETWEEN ALL
						;          CLAUSES OF CONS*CLAUSES AND BETWEEN CLAUSES
						;          OF CONS*CLAUSES AND THE CLAUSES OF THE
						;          EXISTING PARTIAL PLINK-GRAPH.
						;          IF IP IS IN CONS*LINK.COLOURS THEN
						;          CREATES ALL POSSIBLE INTERNAL P-LINKS
						;          (IP-LINKS) IN CONS*CLAUSES.
						; VALUE:   UNDEFINED
  (let ((ALL.P.CLAUSES (UNION (CONS=EXISTING.GRAPH.CLAUSES 'P) CONS*CLAUSES)))
    (MAPC #'(LAMBDA (CLAUSE1)
	      (ds-clause.do #'(lambda (litno1)
				(CONS=PLINKS.TO.LITERAL CLAUSE1 litno1 (CONS=EQ.OCCURRENCES.IN.CLAUSES '+ CONS*CLAUSES)))
			    clause1))
	  (CONS=EXISTING.GRAPH.CLAUSES 'P))
    (MAPC #'(LAMBDA (CLAUSE1)
	      (ds-clause.do #'(lambda (litno1)
				(CONS=PLINKS.TO.LITERAL CLAUSE1 litno1 (CONS=EQ.OCCURRENCES.IN.CLAUSES '+ ALL.P.CLAUSES)))
			    clause1))
	  CONS*CLAUSES)
    (when (MEMBER 'PIW CONS*LINK.COLOURS)
      (CONS=PLINKS.TO.SAME.TERM))))

(DEFUN CONS=PLINKS.TO.LITERAL (CLAUSE LITNO EQ.OCCURRENCES)
						; Authors: PRCKLN KHB
						; EDITED:  13-JUL-1990 00:49
						; INPUT:   A CLAUSE, A LITERALNUMBER (LITNO) AND AN
						;          EQAULITY-OCCURRENCE-LIST (EQ.OCCURRENCES).
						;          EQ.OCCURRENCES IS A LIST OF SUBLISTS.
						;          E.G.: ((C1 1 3 6) (C2 3 5 9))
						;          CAR OF EACH SUBLIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERALNUMBERS. CLAUSE
						;          AND LITERALNUMBER DENOTE THE OCCURRENCE OF
						;          A (POSITIVE) EQUATION.
						; EFFECT:  CREATES ALL POSSIBLE P-LINKS (INCLUDING
						;          THE INTERNAL P-LINKS) BETWEEN THE
						;          LITNO'TH LITERAL OF CLAUSE AND THE
						;          EQUATIONS DETERMINED BY EQ.OCCURRENCES.
						; VALUE:   UNDEFINED
						;(when (ds-clause.lit.is.max clause litno)   ; Only to maximal literal
  (let ((TERMLIST (DS-CLAUSE.TERMLIST CLAUSE LITNO))
	(FCT (DT-TAF.CREATE.FIRST NIL)))
    (WHILE TERMLIST
      (when (or (not (ds-clause.lit.rewrite.rule clause litno))	; Only to left side of directed equations
      		;(eq (opt-get.option er_paramodulation) 'dershowitz)
		(equal fct (list (ds-clause.lit.rewrite.rule clause litno))))
	(CONS=PLINKS.TO.TERM EQ.OCCURRENCES CLAUSE LITNO (CAR TERMLIST) FCT))
      (SETQ TERMLIST (CDR TERMLIST))
      (SETQ FCT (DT-TAF.CREATE.NEXT FCT)))))

(DEFUN CONS=PLINKS.TO.TERM (EQ.OCCURRENCES CLAUSE LITNO TERM FCT)
						; EDITED:  14. 4. 1982   KHB
						; INPUT:   A CLAUSE, A LITERALNUMBER (LITNO), A TERM
						;          A TERM-ACCESS-FUNCTION (FCT) AND AN
						;          EQAULITY-OCCURRENCE-LIST (EQ.OCCURRENCES).
						;          EQ.OCCURRENCES IS A LIST OF SUBLISTS.
						;          E.G.: ((C1 1 3 6) (C2 3 5 9))
						;          CAR OF EACH SUBLIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERALNUMBERS. CLAUSE
						;          AND LITERALNUMBER DENOTE THE OCCURRENCE OF
						;          A (POSITIVE) EQUATION.
						;          TERM IS THE TERM OF THE LITNO'TH LITERAL OF
						;          CLAUSE, WHICH IS DETERMINED BY FCT.
						; EFFECT:  CREATES ALL POSSIBLE P-LINKS (INCLUDING
						;          THE INTERNAL P-LINKS) BETWEEN THE
						;          TERM AND THE EQUATIONS DETERMINED BY
						;          EQ.OCCURRENCES.
						; VALUE:   UNDEFINED
  (unless (DT-VARIABLE.IS TERM)
    (CONS=PLINKS.TO.TOPLEVEL.OF.TERM EQ.OCCURRENCES CLAUSE LITNO TERM FCT)
    (when (CONSP TERM)
      (SETQ TERM (CDR TERM)
	    FCT (DT-TAF.CREATE.FIRST FCT))
      (MAPC #'(LAMBDA (SUBTERM)
		(CONS=PLINKS.TO.TERM EQ.OCCURRENCES CLAUSE LITNO SUBTERM FCT)
		(SETQ FCT (DT-TAF.CREATE.NEXT FCT)))
	    TERM))))

(DEFUN CONS=PLINKS.TO.TOPLEVEL.OF.TERM (EQ.OCCURRENCES CLAUSE LITNO TERM FCT)
						; EDITED:  14. 4. 1982   KHB
						; INPUT:   A CLAUSE, A LITERALNUMBER (LITNO), A TERM
						;          A TERM-ACCESS-FUNCTION (FCT) AND AN
						;          EQAULITY-OCCURRENCE-LIST (EQ.OCCURRENCES).
						;          EQ.OCCURRENCES IS A LIST OF SUBLISTS.
						;          E.G.: ((C1 1 3 6) (C2 3 5 9))
						;          CAR OF EACH SUBLIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERALNUMBERS. CLAUSE
						;          AND LITERALNUMBER DENOTE THE OCCURRENCE OF
						;          A (POSITIVE) EQUATION.
						;          TERM IS THE TERM OF THE LITNO'TH LITERAL OF
						;          CLAUSE, WHICH IS DETERMINED BY FCT.
						; EFFECT:  IF TERM IS A CONSTANT OR VARIABLE,
						;          CREATES ALL POSSIBLE P-LINKS (INCLUDING
						;          THE INTERNAL P-LINKS) BETWEEN THE
						;          TERM AND THE EQUATIONS DETERMINED BY
						;          EQ.OCCURRENCES, ELSE CREATES ALL POSSIBLE
						;          P-LINKS BETWEEN THE TOPLEVEL FUNCTION-
						;          SYMBOL AND THE EQUATIONS DETERMINED BY
						;          EQ.OCCURRENCES.
						; VALUE:   UNDEFINED
  (MAPC #'(LAMBDA (CLAUSE.OCCUR)
	    (let ((EQ.CLAUSE (CAR CLAUSE.OCCUR)))
	      (MAPC #'(LAMBDA (EQ.LITNO)
			(CONS=PLINKS.BETWEEN.TERM.AND.EQUALITY CLAUSE LITNO TERM FCT EQ.CLAUSE EQ.LITNO))
		    (CDR CLAUSE.OCCUR))))
	EQ.OCCURRENCES))




(DEFUN CONS=PLINKS.BETWEEN.TERM.AND.EQUALITY (CLAUSE LITNO TERM FCT EQ.CLAUSE EQ.LITNO)
						; Authors: PRCKLN KHB
						; EDITED:  13-JUL-1990 00:01
						; INPUT:   A CLAUSE, A LITERALNUMBER (LITNO), A TERM,
						;          A TERM-ACCESS-FUNCTION (FCT), ANOTHER
						;          CLAUSE (EQ.CLAUSE) AND ANOTHER LITERAL-
						;          NUMBER (EQ.LITNO). EQ.CLAUSE AND EQ.LITNO
						;          DENOTE AN EQUATION. TERM IS THE TERM OF THE
						;          LITNO'TH LITERAL OF CLAUSE GIVEN BY FCT.
						; EFFECT:  CREATES ALL POSSIBLE P-LINKS (INCLUDING
						;          INTERNAL P-LINKS) BETWEEN THE TOPLEVEL
						;          OF TERM AND THE EQUATION.
						; VALUE:   UNDEFINED
  (when (AND (NEQ CLAUSE EQ.CLAUSE)
	     (or (ds-clause.lit.is.unfailing clause litno)
		 (ds-clause.lit.rewrite.rule clause litno)
		 (not (and (= 1 (ds-clause.nolit clause))
			   (dt-predicate.is.equality (ds-clause.predicate clause litno))
			   (ds-sign.is.positive (ds-clause.sign clause litno)))))
	     (MEMBER 'P CONS*LINK.COLOURS)
	     (not (dt-getprop eq.clause 'cons*a))
	     (if (dt-getprop eq.clause 'cons*ac1)
		 (and (consp term)
		      (eq (dt-term_topsymbol term) (dt-getprop eq.clause 'cons*ac1)))
		 t))
    (let ((EQ.TERMLIST (DS-CLAUSE.TERMLIST EQ.CLAUSE EQ.LITNO))
	  (rule.side (ds-clause.lit.rewrite.rule eq.clause eq.litno))
	  (unfail (ds-clause.lit.is.unfailing eq.clause eq.litno))
	  unifiers)
      (when (and (or unfail (eql 1 rule.side))
		 (setq unifiers (UNI-UNIFY.TERMS (first EQ.TERMLIST) TERM)))
	(if (dt-getprop eq.clause 'cons*ac1)
	    (setq unifiers (delete-if #'(lambda (unifier)
					  (and (eql (length unifier) 2)
					       (dt-term_equal term (second unifier))))
				      unifiers)))
	(when unifiers (CONS=PLINK.INSERT UNIFIERs CLAUSE LITNO EQ.CLAUSE EQ.LITNO FCT (DT-TAF.CREATE.LEFT)
					  (SECOND EQ.TERMLIST))))
      (when (and (or unfail (eql 2 rule.side))
		 (SETQ UNIFIERs (UNI-UNIFY.TERMS (SECOND EQ.TERMLIST) TERM)))
	(if (dt-getprop eq.clause 'cons*ac1)
	    (setq unifiers (delete-if #'(lambda (unifier)
					  (and (eql (length unifier) 2)
					       (dt-term_equal term (second unifier))))
				      unifiers)))
	(when unifiers (CONS=PLINK.INSERT UNIFIERs CLAUSE LITNO EQ.CLAUSE EQ.LITNO FCT (DT-TAF.CREATE.RIGHT)
					  (CAR EQ.TERMLIST)))))))

(defun cons=piw.completion.term (term1 term2 clause litno taf1 taf2)
						; Edited:  20-JAN-1990 19:57
						; Authors: PRCKLN@MAGIC
						; Input:   Two terms and a literal specification
						; Effect:  Creates piw links from TERM1 to TERM2
						;          and all subterms of TERM2
						; Value:   Undefined
  (when (consp term2)				; Non variable positions
    (mapc #'(lambda (unifier)
	      (let ((link (DS-LINK.CREATE 'PIW (list unifier) CLAUSE LITNO CLAUSE LITNO taf1 taf2)))
		(CG-INSERT.LINK link NIL 'INITIAL NIL)))
	  (UNI-UNIFY.TERMS (uni-apply.substitution (ds-clause.renaming clause) term1 t) term2))))

(defun cons=piw.completion.fct (term1 term2 clause litno taf1 taf2)
						; Edited:  20-JAN-1990 20:18
						; Authors: PRCKLN@MAGIC
						; Input:   Two terms in literal CLAUSE LITNO
						;          and the access functions for these terms
						; Effect:  Creates PIW-links between TERM1, TERM2 and all subterms of TERM2.
						; Value:   Undefined
  (cons=piw.completion.term term1 term2 clause litno taf1 taf2)
  (let ((taf (DT-TAF.CREATE.FIRST taf2)))
    (mapc #'(lambda (subterm)
	      (cons=piw.completion.fct term1 subterm clause litno taf1 taf)
	      (SETQ taf (DT-TAF.CREATE.NEXT taf)))
	  (dt-term_arguments term2))))

(defun cons=piw.completion (clause litno)
						; Edited:  20-JAN-1990 19:38
						; Authors: PRCKLN@MAGIC
						; Input:   Specification for a literal
						; Effect:  All internal plinks are created
						;          when needed for completion steps
						; Value:   undefined
  (unless (dt-getprop clause 'cons*a)
    (let ((left.taf (ds-clause.lit.rewrite.rule clause litno)))
      (if left.taf
	  (let ((left (dt-access (setq left.taf (list left.taf)) (ds-clause.termlist clause litno))))
	    (cons=piw.completion.fct left left clause litno left.taf left.taf))
	  (if (ds-clause.lit.is.unfailing clause litno)
	      (let ((left.taf (list 1))
		    (left (first (ds-clause.termlist clause litno)))
		    (right.taf (list 2))
		    (right (second (ds-clause.termlist clause litno))))
		(cons=piw.completion.fct left left clause litno left.taf left.taf)
		(cons=piw.completion.fct right left clause litno right.taf left.taf)
		(cons=piw.completion.fct left right clause litno left.taf right.taf)
		(cons=piw.completion.fct right right clause litno right.taf right.taf)))))))


(DEFUN CONS=PLINKS.TO.SAME.TERM NIL		; EDITED:  13-JUL-1990 00:44
						; INPUT:   NONE
						; EFFECT:  FOR EACH SIDE OF ALL NEW EQUATIONS:
						;          IF AT LEAST ONE VARIABLE OCCURS ONLY IN
						;          THE OTHER SIDE OF THE EQUATION,
						;          THEN CREATES AN IP-LINK (INTERNAL P-LINK)
						;          FROM THE CONSIDERED SIDE OF THE EQUATION
						;          TO ITSELF (TO THE TOPLEVEL-TERM OF THE SAME
						;          EQUALITY-SIDE).
						; VALUE:   UNDEFINED
  (MAPC #'(LAMBDA (CLAUSE.OCCUR)
	    (let ((CLAUSE (CAR CLAUSE.OCCUR)))
	      (MAPC #'(LAMBDA (LITNO) (cons=piw.completion clause litno))
		    (CDR CLAUSE.OCCUR))))
	(CONS=EQ.OCCURRENCES.IN.CLAUSES '+ CONS*CLAUSES)))


(DEFUN CONS=PLINK.INSERT (UNIFIERS CLAUSE LITNO EQ.CLAUSE EQ.LITNO FCT EQ.FCT TERM)
						; Authors: PRCKLN 
						; EDITED:  13-JUL-1990 00:45
						; INPUT:  A LIST OF UNIFIERS (UNIFIERS), TWO CLAUSES
						;         (CLAUSE EQ.CLAUSE), TWO LITERALNUMBERS
						;         (LITNO EQ.LITNO), TWO TERMACCESS-FUNCTIONS
						;         (FCT, EQ.FCT) AND A TERM.
						; EFFECT: A P-LINK FROM THE TERM GIVEN BY CLAUSE,
						;         LITNO AND FCT    TO ONE SIDE OF THE
						;         EQUATION GIVEN BY EQ.CLAUSE, EQ.LITNO AND
						;         EQ.FCT IS CREATED AND INSERTED INTO THE
						;         GRAPH. TERM IS THE OTHER SIDE OF THIS
						;         EQUATION. THE SORT OF TERM MUST BE
						;         ADMISSIBLE AT THE POSITION QHERE IT WILL
						;         REPLACE ANOTHER TERM. THE UNIFIER WILL BE
						;         FITTED TO SATISFY THIS CONDITION IF
						;         POSSIBLE, ELSE THE UNIFIER IS SET TO NIL,
						;         AND PARAMODULATION ON THE LINK IS foRBIDDEN,
						;         THE LINK MUST ONLY BE USED FOR INHERITANCE.
						; VALUE:  The link
  (setq unifiers (uni-fit.on.literal.sort (uni-fit.on.sort unifiers (DS-CLAUSE.ADMISSIBLE.SORT CLAUSE LITNO FCT) term)))
  (mapcar #'(lambda (unifier)
	      (let* ((unifiers (list unifier))
		     LINK     
		     (term (dt-access fct (ds-clause.termlist clause litno)))
		     (eq.term (dt-access eq.fct (ds-clause.termlist eq.clause eq.litno)))
		     matchers)
		(cond ((and (not (ds-clause.irreducible.is eq.clause eq.litno))
			    (ds-clause.lit.rewrite.rule eq.clause eq.litno)
			    (setq matchers (uni-unify1.terms eq.term term (DS-CLAUSE.VARIABLES clause))))
		       (SETQ LINK (DS-LINK.CREATE 'P matchers EQ.CLAUSE EQ.LITNO CLAUSE LITNO EQ.FCT FCT))
		       (CG-INSERT.LINK LINK NIL 'INITIAL NIL)
		       (ds-link.demodulation.set link))
		      (t (SETQ LINK (DS-LINK.CREATE 'P UNIFIERS EQ.CLAUSE EQ.LITNO CLAUSE LITNO EQ.FCT FCT))
			 (CG-INSERT.LINK LINK NIL 'INITIAL NIL)))
		link))
	  unifiers))
		

(DEFUN CONS=RW_CONSTRUCT.LINKS (CLAUSE.LITNOS)
						; EDITED: 23-SEP-83 14:33:48          NB
						; INPUT:  A LIST OF N SUBLISTS (CJ LJ1 .. LJMJ)
						;         WITH J = 1 ..N.
						;         CJ IS A RW.CLAUSE AND  LJ1 .. LJMJ  THE
						;         MODIFIED LITERALS OF CJ.
						; EFFECT: FOR EACH L.COLOUR IN CONS*NEW.LINK.COLOURS
						;          ALL LINKS CONNECTING CLAUSES AMONG
						;          THEMSELVES AND TO THE REST OF THE GRAPH
						;         ARE RECREATED AND BECOME PART OF THE ACTUAL
						;         GRAPH.
						;          FOR EACH IRREFLEXIVE LITERAL IN CLAUSES A
						;          REFLEXIVITY CLAUSE OF THE RESPECTIVE SORT
						;         IS RECREATED AND CONNECTED TO THE LITERAL
						;          BY EXACTLY ONE R-LINK. NO OTHER LINKS ARE
						;          CONNECTED TO THE REFLEXIVITY CLAUSES.
						; VALUE:   UNDEFINED.
  (let ((OLD.CLAUSES (REMOVE-IF-NOT #'(LAMBDA (CLAUSE) (NOT (ASSOC CLAUSE CLAUSE.LITNOS))) (CG-CLAUSES ALL))))
    (MAPC #'(LAMBDA (CLAUSE.LITNOS)
	      (OP-CONSTRUCT.LITERAL.LINKS (CAR CLAUSE.LITNOS) (CDR CLAUSE.LITNOS) (DS-LINK.COLOURS.FOR 'INITIAL) OLD.CLAUSES
					  NIL)
	      (SETQ OLD.CLAUSES (CONS (CAR CLAUSE.LITNOS) OLD.CLAUSES)))
	  CLAUSE.LITNOS))
  (SETQ CONS*RW_EQ.OCCURRENCES.POS (CONS=EQ.OCCURRENCES '+))
  (SETQ CONS*RW_EQ.OCCURRENCES.NEG (CONS=EQ.OCCURRENCES '-))
  (when CONS*RW_EQ.OCCURRENCES.POS (CONS=RECREATE.PLINKS CLAUSE.LITNOS)))

(DEFUN CONS=RECREATE.PLINKS (LIST.OF.CLAUSE.+.LITERALS)
						; EDITED: 23-SEP-83 14:48:19            NB
						; INPUT:  A LIST OF N SUBLISTS (CJ LJ1 .. LJMJ) WHERE
						;         CJ IS A RW.CLAUSE AND LJ1 .. LJMJ ARE THE
						;         MODIFIED LITERALS OF CJ WITH J = 1..N
						; EFFECT: IF P IS IN CONS*RW_LINK.COLOURS
						;         RECREATES ALL POSSIBLE P-LINKS BETWEEN ALL
						;         CLAUSES OF THE INPUT LIST AND BETWEEN ALL
						;         LITERALS LKJK OF THE CLAUSE CJ AND THE REST
						;         OF THE EXISTING PARTIAL PLINK-GRAPH
						;         IF PIW IS IN CONS*RW_LINK.COLOURS
						;         RECREATES ALL POSSIBLE INTERNAL P-LINKS
						;         (PIW-LINKS) IN CONS*RW.CLAUSES.
						; VALUE:   UNDEFINED
  (let ((RELEV.P.CLAUSES (CONS=EXISTING.GRAPH.CLAUSES 'P)) LIST)
    (MAPC #'(LAMBDA (CLAUSE.+.LITERALS)
	      (MAPC #'(LAMBDA (LITNO)
			(CONS=PLINKS.TO.LITERAL (CAR CLAUSE.+.LITERALS) LITNO
						(CONS=EQ.OCCURRENCES.IN.CLAUSES '+ RELEV.P.CLAUSES)))
		    (CDR CLAUSE.+.LITERALS)))
	  LIST.OF.CLAUSE.+.LITERALS)
    (SETQ CONS*RW_EQ.OCCURRENCES.POS
	  (MAPCAN #'(LAMBDA (CLAUSE.+.LITERALS)
		      (SETQ LIST (REMOVE-IF-NOT #'(LAMBDA (LITNO) (DS-CLAUSE.IS.EQUATION (CAR CLAUSE.+.LITERALS) LITNO))
						(CDR CLAUSE.+.LITERALS)))
		      (if LIST (LIST (CONS (CAR CLAUSE.+.LITERALS) LIST))))
		  (COPY-TREE LIST.OF.CLAUSE.+.LITERALS)))
    (MAPC #'(LAMBDA (CLAUSE)
	      (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		(unless (MEMBER (1+ RPTN)
				(CDAR (MEMBER-IF #'(LAMBDA (CLAUSE.+.LITERALS) (EQL CLAUSE (CAR CLAUSE.+.LITERALS)))
						 LIST.OF.CLAUSE.+.LITERALS)))
		  (CONS=PLINKS.TO.LITERAL CLAUSE (1+ RPTN) CONS*RW_EQ.OCCURRENCES.POS))))
	  (CONS=EXISTING.GRAPH.CLAUSES 'P))
    (CONS=RW_PLINKS.TO.SAME.TERM)))

(DEFUN CONS=RW_PLINKS.TO.SAME.TERM NIL		; EDITED: 23-SEP-83 15:35:53
						; INPUT:   NONE
						; EFFECT:  FOR EACH SIDE OF ALL EQUATIONS IN THE
						;          CLAUSES OF CONS*NEW.EQ.OCCURRENCES.NEG:
						;          IF AT LEAST ONE VARIABLE OCCURS ONLY IN
						;          THE OTHER SIDE OF THE EQUATION,
						;          THEN CREATES AN PIW-LINK (INTERNAL P-LINK)
						;          FROM THE CONSIDERED SIDE OF THE EQUATION
						;          TO ITSELF (TO THE TOPLEVEL-TERM OF THE SAME
						;          EQUALITY-SIDE).
						; VALUE:   UNDEFINED
  
  (MAPC #'(LAMBDA (CLAUSE.OCCUR)
	    (let ((CLAUSE (CAR CLAUSE.OCCUR)))
	      (MAPC #'(LAMBDA (LITNO) (cons=piw.completion clause litno))
		    (CDR CLAUSE.OCCUR))))
	CONS*RW_EQ.OCCURRENCES.POS))



(DEFUN CONS=EQ.OCCURRENCES (SIGN)
						; EDITED:  7. 4. 1982   KHB
						; INPUT:   A SIGN. SIGN MAY BE 'POS OR 'NEG.
						; VALUE:   THE OCCURRENCES OF EQUATIONS  WITH
						;          THE GIVEN SIGN IN FORM OF A LIST OF LISTS.
						;          E.G.: ((C1  1  2  3  4) (C2  2  5  6))
						;          CAR OF EACH SUCH LIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERAL-NUMBERS. CLAUSE
						;          AND LITERAL-NUMBER DENOTE THE OCCURRENCE
						;          OF AN EQUATION WITH POSITIVE SIGN, IF
						;          SIGN IS 'POS AND WITH NEGATIVE SIGN, IF
						;          SIGN IS 'NEG.
						;          THE LIST CONTAINS THE OCCURRENCES OF
						;          EQUATIONS IN ALL CREATED CLAUSES, I.E.
						;          ALSO IN CLAUSES FOR WHICH THE LINKS WILL
						;          BE CREATED IN LATER CALLS OF
						;          CONS-CONSTRUCT.LINKS.
						;          THE OCCURRENCES-LISTS DON'T CONTAIN THE
						;          OCCURRENCES IN REFLEXIVITY-CLAUSES.
  (let (EQ.OCCURRENCES)
    (SETQ EQ.OCCURRENCES
	  (MAPCAN #'(LAMBDA (PREDICATE)
		      (COPY-TREE (CASE SIGN
				   (+ (DT-PREDICATE.POSITIVE.OCCURRENCES PREDICATE))
				   (- (DT-PREDICATE.NEGATIVE.OCCURRENCES PREDICATE))
				   (OTHERWISE (ERROR "ILLEGAL SIGN-NAME IN CONS=EQ.OCCURRENCES: : ~A" SIGN)))))
		  (DT-PREDICATE.EQUALITIES)))
    (REMOVE-IF-NOT #'(LAMBDA (CLAUSE.OCCUR)
		       (NOT (INTERSECTION (DS-CLAUSE.ATTRIBUTES (CAR CLAUSE.OCCUR)) '(REFLEXIVITY IRREFLEXIVITY))))
		   EQ.OCCURRENCES)))


(DEFUN CONS=PREDICATE.OCCURRENCES (PREDICATE PRED.OCCUR.SIGN)
						; EDITED:  11. 3. 1982   KHB
						; INPUT:   A PREDICATE P AND A SIGN. SIGN MAY BE
						;          'POS OR 'NEG.
						; VALUE:   THE OCCURRENCES OF THE PREDICATE WITH
						;          THE GIVEN SIGN IN FORM OF A LIST OF LISTS.
						;          E.G.: ((C1  1  2  3  4) (C2  2  5  6))
						;          CAR OF EACH SUCH LIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERAL-NUMBERS. CLAUSE
						;          AND LITERAL-NUMBER DENOTE THE OCCURRENCE
						;          OF THE PREDICATE P WITH POSITIVE SIGN, IF
						;          SIGN IS 'POS AND WITH NEGATIVE SIGN, IF
						;          SIGN IS 'NEG.
						;          THE LIST CONTAINS THE OCCURRENCES OF THE
						;          PREDICATE IN ALL CREATED CLAUSES, I.E.
						;          ALSO IN CLAUSES FOR WHICH THE LINKS WILL
						;          BE CREATED IN LATER CALLS OF
						;          CONS-CONSTRUCT.LINKS.
						;          IF P IS AN EQUALITY-PREDICATE, THEN THE
						;          OCCURRENCES-LISTS OF ALL EQUALITY-
						;          PREDICATES ARE CONCATENATED. THE
						;          OCCURRENCES-LISTS DON'T CONTAIN THE
						;          OCCURRENCES IN REFLEXIVITY-CLAUSES.
  (cond ((DT-PREDICATE.IS.EQUALITY PREDICATE)
	 (CASE PRED.OCCUR.SIGN
	   (+ CONS*EQ.OCCURRENCES.POS)
	   (- CONS*EQ.OCCURRENCES.NEG)
	   (OTHERWISE (ERROR "ILLEGAL SIGN-NAME IN CONS=PREDICATE.OCCURRENCES: : ~A" PRED.OCCUR.SIGN))))    
	(t (CASE PRED.OCCUR.SIGN
	     (+ (DT-PREDICATE.POSITIVE.OCCURRENCES PREDICATE))
	     (- (DT-PREDICATE.NEGATIVE.OCCURRENCES PREDICATE))
	     (OTHERWISE (ERROR "ILLEGAL SIGN-NAME IN CONS=PREDICATE.OCCURRENCES: : ~A" PRED.OCCUR.SIGN))))))

(DEFUN CONS=PRED.OCCURRENCES.IN.CLAUSES (PREDICATE PRED.OCCUR.SIGN CLAUSES)
						; EDITED:  11. 3. 1982   KHB
						; INPUT:   A PREDICATE P, A SIGN WHICH MAY BE
						;          'POS OR 'NEG AND A LIST OF CLAUSES.
						; VALUE:   THE OCCURRENCES OF THE PREDICATE WITH
						;          THE GIVEN SIGN IN CLAUSES.
						;          THE VALUE IS A LIST OF LISTS.
						;          E.G.: ((C1  1  2  3  4) (C2  2  5  6))
						;          CAR OF EACH SUCH LIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERAL-NUMBERS. CLAUSE
						;          AND LITERAL-NUMBER DENOTE THE OCCURRENCE
						;          OF THE PREDICATE P WITH POSITIVE SIGN, IF
						;          SIGN IS 'POS AND WITH NEGATIVE SIGN, IF
						;          SIGN IS 'NEG.
						;          IF P IS AN EQUALITY-PREDICATE, THEN THE
						;          OCCURRENCES-LISTS OF ALL EQUALITY-
						;          PREDICATES ARE CONCATENATED. THE
						;          OCCURRENCES-LISTS DON'T CONTAIN THE
						;          OCCURRENCES IN REFLEXIVITY-CLAUSES.
  (REMOVE-IF-NOT #'(LAMBDA (CLAUSE.OCCURRENCES) (MEMBER (CAR CLAUSE.OCCURRENCES) CLAUSES))
		 (CONS=PREDICATE.OCCURRENCES PREDICATE PRED.OCCUR.SIGN)))


(DEFUN CONS=EQ.OCCURRENCES.IN.CLAUSES (SIGN CLAUSES)
						; EDITED:  7. 4. 1982   KHB
						; INPUT:   A SIGN WHICH MAY BE 'POS OR 'NEG AND A LIST
						;          OF CLAUSES.
						; VALUE:   THE OCCURRENCES OF EQUALITIES IN CLAUSES
						;          WITH THE GIVEN SIGN
						;          IN FORM OF A LIST OF LISTS.
						;          E.G.: ((C1  1  2  3  4) (C2  2  5  6))
						;          CAR OF EACH SUCH LIST DENOTES A CLAUSE AND
						;          CDR IS A LIST OF LITERAL-NUMBERS. CLAUSE
						;          AND LITERAL-NUMBER DENOTE THE OCCURRENCE
						;          OF AN EQUATION WITH POSITIVE SIGN, IF
						;          SIGN IS 'POS AND WITH NEGATIVE SIGN, IF
						;          SIGN IS 'NEG.)
  (let ((EQ.OCCURRENCES (cons=eq.occurrences sign)))
    (REMOVE-IF-NOT #'(LAMBDA (CLAUSE.OCCUR) (MEMBER (CAR CLAUSE.OCCUR) CLAUSES)) EQ.OCCURRENCES)))

(DEFUN CONS=EXISTING.GRAPH.CLAUSES (COLOUR)
						; EDITED:  5-APR-83 13:39:05
						; INPUT:   A LINK COLOUR
						; VALUE:   ALL CLAUSES OF THE PARTIAL GRAPH ,FOR
						;          WHICH THE LINKS OF COLOUR ARE ALREADY
						;          CREATED (I.E. BEFORE THIS CALL OF
						;          CONS-CONSTRUCT.LINKS)
  (REMOVE-IF-NOT #'(LAMBDA (CLAUSE)
		     (AND (NOT (MEMBER COLOUR (DT-GETPROP CLAUSE 'MISSING.LINKS)))
			  (NOT (MEMBER CLAUSE CONS*NO.LINK.CLAUSES))))
		 (CG-CLAUSES ALL)))

