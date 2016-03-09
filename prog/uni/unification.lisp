;;; -*- package: mkrp; syntax: common-lisp; mode: lisp -*-
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
MKRP, but only if it is not used for military purposes or any
military research. It is also forbidden to use MKRP in nuclear plants
or nuclear research, and for verifying programs in military 
and nuclear research.  A copy of this license is
supposed to have been given to you along with MKRP so you
can know your rights and responsibilities.  
Among other things, the copyright notice
must be preserved on all copies.  |#
(IN-PACKAGE "MKRP")

(DEFVAR UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL)

(DEFVAR UNI*VARIABLES.DECLARED.AS.CONSTANTS NIL)



(DEFVAR UNI*BINDINGFLAG NIL)

(DEFVAR UNI*BUFFER.STACK NIL)

(DEFVAR UNI*CONSTANTIFY.BUFFER NIL)

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
  #-:mkrp-basic (denz-init)
  (SETQ UNI*BUFFER.STACK (BUFFER.CREATE 0))
  (setq UNI*VARIABLES.REGARDED.AS.CONSTANTS nil)
  (SETQ UNI*CONSTANTIFY.BUFFER (BUFFER.CREATE 10)))

(DEFun UNI-UNIFY.TERMS (TERM1 TERM2 &OPTIONAL BOOLEAN.RESULTFLAG)
						; edited:  12-SEP-1990 19:01
						; Authors: PRCKLN HJB
						; input:   two arbitrary terms
						; value:   list of all most general unifiers under
						;          all given theories
  (cond ((DT-FUNCTION.THEORIES)
	 (thu-terms term1 term2 nil))
	(BOOLEAN.RESULTFLAG
	 (UNI=UNBIND (UNI=UNIFY.TERMS TERM1 TERM2)))
	(t (UNI=COPY (UNI=UNBIND (UNI=UNIFY.TERMS TERM1 TERM2))))))

(DEFun UNI-UNIFY.TERMLISTS (TL1 TL2 &OPTIONAL BOOLEAN.RESULTFLAG)
						; edited:  6-dec-83 11:59:54
						; Authors: BUERCKERT
						; input:  two arbitrary termlists
						; value:  list of all most general unifiers under
						;         all given theories
  (if (DT-FUNCTION.THEORIES)
      (thu-termlists tl1 tl2 nil)
      (if BOOLEAN.RESULTFLAG
	  (UNI=UNBIND (UNI=UNIFY.TERMLISTS TL1 TL2))
	  (UNI=COPY (UNI=UNBIND (UNI=UNIFY.TERMLISTS TL1 TL2))))))

(defun UNI-UNIFY.ATOMS (PRED1 TL1 PRED2 TL2 &OPTIONAL BOOLEAN.RESULTFLAG)
						; edited at 21-feb-83 16:27)
						; Authors: BUERCKERT
						; see     uni=unify.atoms
  (COND (BOOLEAN.RESULTFLAG (UNI=UNIFY.ATOMS PRED1 TL1 PRED2 TL2))
	(T (UNI=COPY (UNI=UNIFY.ATOMS PRED1 TL1 PRED2 TL2)))))

(DEFUN UNI-UNIFY.QUICK.TEST (EXPR1 EXPR2)
						; edited:  04.07.84  by  hjb
						; Authors: BUERCKERT
						; input:   two terms or two termlists of same length
						; value:   nil, if they are not unifiable
						;          t, if they may be unifiable
						; note:    to get a rather fast test the occurence
						;          test and the sort checks are omitted and a
						;          'loop'-implementation is used.
						;          see uni=unify.quick.test.
  (OR (EQL EXPR1 EXPR2) (UNI=VARIABLE.IS EXPR1) (UNI=VARIABLE.IS EXPR2)
      (COND
	((AND (DT-TERM.IS.ABBREVIATION EXPR1) (CONSP EXPR2)) (UNI=UNIFY.QUICK.TEST (DT-ABBREVIATION.TERM EXPR1) EXPR2))
	((AND (DT-TERM.IS.ABBREVIATION EXPR2) (CONSP EXPR1)) (UNI=UNIFY.QUICK.TEST EXPR1 (DT-ABBREVIATION.TERM EXPR2)))
	((AND (CONSP EXPR1) (CONSP EXPR2)) (UNI=UNIFY.QUICK.TEST EXPR1 EXPR2)))))

(DEFUN UNI-UNIFY.LIST.OF.MIXED.TERMLISTS (UNIFIERS BOOLEAN.RESULTFLAG)
						; input:  a list of even termlists
						; value:  a list of unifiers each of them unifies some
						;         of the input termlists in the sense of
						;         uni=unify.mixed.termlist.
  (SETQ UNIFIERS (UNI=UNBIND (MAPCAN (FUNCTION UNI=UNIFY.MIXED.TERMLIST) UNIFIERS)))
  (COND (BOOLEAN.RESULTFLAG (MAXIMA UNIFIERS (FUNCTION UNI=INSTANCE.IS)))
	(T (UNI=COPY (MAXIMA UNIFIERS (FUNCTION UNI=INSTANCE.IS))))))

(DEFMACRO UNI-UNIFY.MIXED.TERMLIST (TERMLIST &OPTIONAL BOOLEAN.RESULTFLAG)
						; edited: 13-oct-83 15:02:26                       hjb
						; Authors: BUERCKERT
						; see uni=unify.mixed.termlist
  (COND ((OR (EQ NIL BOOLEAN.RESULTFLAG) (AND (CONSTANTP BOOLEAN.RESULTFLAG) (NULL (EVAL BOOLEAN.RESULTFLAG))))
	 `(UNI=COPY (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST ,TERMLIST))))
	((CONSTANTP BOOLEAN.RESULTFLAG)
	 `(UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST ,TERMLIST)))
	(T `(IF ,BOOLEAN.RESULTFLAG
		(UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST ,TERMLIST))
		(UNI=COPY (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST ,TERMLIST)))))))


(DEFUN UNI=WEAK.UNION.OF.SUBSTITUTIONLISTS (UNIFIERS1 UNIFIERS2 VARIABLES &OPTIONAL COMPLEMENTFLAG)
						; edited: 24. 5. 1984
						; Authors: BUERCKERT
						; input:  two lists of unifiers, a list of variables
						;         and a boolean value
						; value:  union of the two substitutionlists where
						;         no element is a weak instance of another.
						;         (see uni-weak.instance)
  (PROG (UNIFIERS)
	(RETURN
	  (NCONC
	    (SETQ UNIFIERS
		  (REMOVE-IF-NOT
		    #'(LAMBDA (UNI1)
			(EVERY #'(LAMBDA (UNI2) (NOT (UNI=WEAK.INSTANCE UNI1 UNI2 VARIABLES COMPLEMENTFLAG))) UNIFIERS2))
		    UNIFIERS1))
	    (REMOVE-IF-NOT
	      #'(LAMBDA (UNI2)
		  (EVERY #'(LAMBDA (UNI1) (NOT (UNI=WEAK.INSTANCE UNI2 UNI1 VARIABLES COMPLEMENTFLAG))) UNIFIERS))
	      UNIFIERS2)))))

(DEFUN UNI-UNIFY.TERMLISTS.WITH.RULE (T1 T2 RULE BOOLEAN.RESULTFLAG)
						; edited: 30-sep-83 11:27:48
						; Authors: BUERCKERT
						; input:  two termlists and a description how to unify
						;         rule = nil  ordinary unification
						;         rule = symmetric (or asymmetric)
						;                unification under symmetry of the
						;                first two arguments
						;         rule = (c l1 l2 cl2 l1e)
						;                unification with the two literal
						;                clause c.
						; value:  a list with unifiers.
  (PROG (RESULT)
	(COND
	  (RULE
	   (PROG (UNI1 UNI2 UNIFIERS)
		 (CASE RULE
		   ((SYMMETRIC ASYMMETRIC) (SETQ UNI1 (UNI=UNBIND (UNI=UNIFY.TERMLISTS T1 T2)))
		    (SETQ RESULT
			  (COND (UNI1 (COND ((NULL (CAR UNI1)) UNI1)
					    ((NULL (SETQ UNI2 (UNI=UNBIND (UNI=UNIFY.TERMLISTS
									    (CONS (SECOND T1) (CONS (CAR T1) (CDDR T1))) T2))))
					     UNI1)
					    ((NULL (CAR UNI2)) UNI2) (T (UNI=UNION.OF.SUBSTITUTIONLISTS UNI1 UNI2))))
				(T (UNI=UNBIND (UNI=UNIFY.TERMLISTS (CONS (SECOND T1) (CONS (CAR T1) (CDDR T1))) T2))))))
		   (OTHERWISE
		     (SETQ RESULT
			   (COND
			     ((CDDDR RULE)
			      (PROG ((CLAUSE (CAR RULE)) (LITNO1 (SECOND RULE)) (LITNO2 (THIRD RULE)))
				    (SETQ UNI1 (UNI=UNIFY.TERMLISTS.WITH.RULE T1 T2 CLAUSE LITNO1 LITNO2))
				    (SETQ UNIFIERS
					  (COND (UNI1 (COND ((NULL (CAR UNI1)) UNI1)
							    ((NULL (SETQ UNI2 (UNI=UNIFY.TERMLISTS.WITH.RULE
										T1 T2 CLAUSE LITNO2 LITNO1)))
							     UNI1)
							    ((NULL (CAR UNI2)) UNI2)
							    (T (UNI=WEAK.UNION.OF.SUBSTITUTIONLISTS
								 UNI1 UNI2 (DS-CLAUSE.VARIABLES CLAUSE) T))))
						(T (UNI=UNIFY.TERMLISTS.WITH.RULE T1 T2 CLAUSE LITNO2 LITNO1))))
				    (COND ((AND UNIFIERS (CAR UNIFIERS)) (UNI=RENAME UNIFIERS (DS-CLAUSE.VARIABLES CLAUSE))))
				    (RETURN UNIFIERS)))
			     (T
			      (SETQ UNIFIERS (UNI=UNIFY.TERMLISTS.WITH.RULE T1 T2 (CAR RULE) (SECOND RULE) (THIRD RULE)))
			      (COND
				((AND UNIFIERS (CAR UNIFIERS)) (UNI=RENAME UNIFIERS (DS-CLAUSE.VARIABLES (CAR RULE)))))
			      UNIFIERS)))))))
	  (T (SETQ RESULT (UNI=UNBIND (UNI=UNIFY.TERMLISTS T1 T2)))))
	(RETURN (COND (BOOLEAN.RESULTFLAG RESULT) (T (UNI=COPY RESULT))))))



(DEFUN UNI=REMOVE.SUBSTITUTIONS (UNIFIERS VARLIST)
						; edited:  5-oct-83 17:36:16                       hjb
						; Authors: BUERCKERT
						; input:   a unifierlist, a variablelist
						; value:   the input-unifierlist without all subst's
						;          with variables not in varlist and without
						;          instances.
  (UNI=REMOVE.INSTANCES
    (MAPCAR
      #'(LAMBDA (UNIFIER)
          (DELETE NIL
		  (SMAPCON
		    #'(LAMBDA (TAIL) (COND ((MEMBER (CAR TAIL) VARLIST) (LIST (CAR TAIL) (SECOND TAIL))))) #'CDDR UNIFIER)))
      UNIFIERS)))

(DEFUN UNI-UNIFY.UNIFIER (UNIFIER VARIABLES RENAMEFLAG BOOLEAN.RESULTFLAG)
						; edited:  5-oct-83 17:10:42                       hjb
						; Authors: BUERCKERT
						; input:   a substitution, a list of variables and a
						;          renameflag
						; value:   a list of unifiers, so that each of them
						;          unifies dom(unifier) and cod(unifier),
						;          under a global bindingenvironment.
						; effect:  in the unifierlist of value all subst-comp.
						;          with domainvariables not in variables will
						;          be removed.
						;          if renameflag = t, all variables in cod. of
						;          the value unifiers not in the variablelist
						;          will be renamed.
  (PROG
    (RESULT
     (NEWUNIFIERS (COND (UNI*BINDINGFLAG (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNIFIER))) (T (LIST UNIFIER)))))
    (SETQ NEWUNIFIERS (UNI=REMOVE.SUBSTITUTIONS NEWUNIFIERS VARIABLES))
    (COND (RENAMEFLAG (SETQ RESULT (CDR (DT-TERM.RENAMED NEWUNIFIERS VARIABLES)))) (T (SETQ RESULT NEWUNIFIERS)))
    (RETURN (COND (BOOLEAN.RESULTFLAG RESULT) (T (UNI=COPY RESULT))))))

(defun uni-equal.substs (uni1 uni2)
  (and uni1 uni2
       (null (rest uni1))
       (null (rest uni2))
       (let ((uni1 (first uni1))
	     (uni2 (first uni2)))
	 (and (prog2 (UNI=SET.BINDINGS uni2)
		     (equal (list nil) (UNI=UNIFY.MIXED.TERMLIST UNI1))
		     (UNI=RESET.BINDINGS uni2))
	      (prog2 (UNI=SET.BINDINGS uni1)
		     (equal (list nil) (UNI=UNIFY.MIXED.TERMLIST UNI2))
		     (UNI=RESET.BINDINGS uni1))))))

(defun uni-unify.away (term1 term2)
  (let ((vars (dt-term.variables term1))
	pair)
    (mapcar #'(lambda (unifier)
		(while (setq pair (ssomel #'(lambda (rest.uni)
					      (if (and (dt-variable.is (first rest.uni))
						       (dt-variable.is (second rest.uni))
						       (member (second rest.uni) vars)
						       (not (member (first rest.uni) vars)))
						  (prog1 (cons (first rest.uni) (second rest.uni))
							 (psetf (first rest.uni) (second rest.uni)
								(second rest.uni) (first rest.uni)))))
					  #'cddr
					  unifier))
		  (smapl #'(lambda (rest.uni)
			     (setf (first rest.uni) (nsubst (rest pair) (first pair) (first rest.uni))))
			 #'cddr
			 (rest unifier)))
		(do ((rest.unifier unifier (cddr rest.unifier))
		     new.unifier)
		    ((null rest.unifier) new.unifier)
		  (unless (member (first rest.unifier) vars)
		    (setf new.unifier (cons (first rest.unifier) (cons (second rest.unifier) new.unifier))))))	      
	  (UNI=COPY (UNI=UNBIND (uni=unify.terms term1 term2))))))

(DEFUN UNI=UNIFY.TERMS (TERM1 TERM2)
						; edited: 16-sep-83 12:09:37                       hjb
						; Authors: BUERCKERT
						; input:  2 arbitrary terms
						; value:  unifiers = nil, if no unifier exists
						;         unifiers = (nil), if the terms are equal
						;         unifiers = list of unifiers, so that
						;         s(term1) = (term2) with each unifier s.
						; warning! returns with bindings, if only one unifier
  (SETQ TERM1 (UNI=GET.BINDING TERM1) TERM2 (UNI=GET.BINDING TERM2))
  (PROG (SORTS XNEW UNILIST
	 (SORT.OF.T1 (DT-TERM.SORT TERM1))
	 (SORT.OF.T2 (DT-TERM.SORT TERM2)))
	(COND ((EQL TERM1 TERM2) (RETURN (LIST NIL)))
	      ((AND (UNI=VARIABLE.IS TERM1) (UNI=VARIABLE.IS TERM2))
	       (COND
		 ((DT-SORT.IS.SUBSORT SORT.OF.T2 SORT.OF.T1) (DT-VARIABLE.PUT.BINDING TERM1 TERM2)
		  (RETURN (LIST (LIST TERM1 TERM2))))
		 ((DT-SORT.IS.SUBSORT SORT.OF.T1 SORT.OF.T2) (DT-VARIABLE.PUT.BINDING TERM2 TERM1)
		  (RETURN (LIST (LIST TERM2 TERM1))))
		 (T (SETQ SORTS (DT-SORT.GREATEST.COMMON.SUBSORTS SORT.OF.T1 SORT.OF.T2))
		    (COND ((NULL SORTS)
			   (RETURN NIL))
			  ((CDR SORTS)
			   (RETURN
			     (MAPCAR
			       #'(LAMBDA (SORT) (SETQ XNEW (DT-VARIABLE.CREATE SORT)) (LIST TERM1 XNEW TERM2 XNEW))
			       SORTS)))
			  (T (SETQ XNEW (DT-VARIABLE.CREATE (CAR SORTS)))
			     (DT-VARIABLE.PUT.BINDING TERM1 XNEW)
			     (DT-VARIABLE.PUT.BINDING TERM2 XNEW)
			     (RETURN (LIST (LIST TERM1 XNEW TERM2 XNEW))))))))
	      ((UNI=VARIABLE.IS TERM1)
	       (SETQ TERM2 (UNI=INSERT.BINDINGS.IN TERM2))
	       (COND ((IN TERM1 TERM2) (RETURN NIL))
		     ((DT-SORT.IS.SUBSORT SORT.OF.T2 SORT.OF.T1)
		      (DT-VARIABLE.PUT.BINDING TERM1 TERM2)
		      (RETURN (LIST (LIST TERM1 TERM2))))
		     ((CONSP TERM2) (RETURN (UNI=POLY.X.WITH.T TERM1 (UNI=INSERT.BINDINGS.IN TERM2))))
		     (T (RETURN NIL))))
	      ((UNI=VARIABLE.IS TERM2)
	       (SETQ TERM1 (UNI=INSERT.BINDINGS.IN TERM1))
	       (COND ((IN TERM2 TERM1) (RETURN NIL))
		     ((DT-SORT.IS.SUBSORT SORT.OF.T1 SORT.OF.T2) (DT-VARIABLE.PUT.BINDING TERM2 TERM1)
		      (RETURN (LIST (LIST TERM2 TERM1))))
		     ((CONSP TERM1) (RETURN (UNI=POLY.X.WITH.T TERM2 (UNI=INSERT.BINDINGS.IN TERM1))))
		     (T (RETURN NIL))))
	      ((OR (UNI=CONSTANT.IS TERM1) (UNI=CONSTANT.IS TERM2))
	       (cond ((or (and (consp term1) (DT-FUNCTION.IS.MARKED ac1 (CAR TERM1)))
			  (and (consp term2) (DT-FUNCTION.IS.MARKED ac1 (CAR TERM2))))
		      (SETQ UNILIST (thu-terms (UNI=INSERT.BINDINGS.IN TERM1) (UNI=INSERT.BINDINGS.IN TERM2)
					       UNI*VARIABLES.REGARDED.AS.CONSTANTS))
		      (COND
			((AND (CAR UNILIST) (NULL (CDR UNILIST))) (UNI=SET.BINDINGS (CAR UNILIST))
						; if only one unifier, bindings will be set
			 ))
		      (RETURN UNILIST))
		     (t (RETURN NIL))))
	      ((AND (DT-TERM.IS.ABBREVIATION TERM1) (DT-TERM.IS.ABBREVIATION TERM2))
	       (RETURN NIL)))
	(COND ((DT-TERM.IS.ABBREVIATION TERM1) (SETQ TERM1 (DT-ABBREVIATION.TERM TERM1)))
	      ((DT-TERM.IS.ABBREVIATION TERM2) (SETQ TERM2 (DT-ABBREVIATION.TERM TERM2))))
	(COND ((NEQ (CAR TERM1) (CAR TERM2)) (RETURN NIL))
	      ((DT-FUNCTION.IS.MARKED commutativE (CAR TERM1))
	       (SETQ UNILIST (comm=UNIFY (UNI=INSERT.BINDINGS.IN TERM1) (UNI=INSERT.BINDINGS.IN TERM2)))
	       (COND
		 ((AND (CAR UNILIST) (NULL (CDR UNILIST))) (UNI=SET.BINDINGS (CAR UNILIST))
						; if only one unifier, bindings will be set
		  ))
	       (RETURN UNILIST))
	      (T (RETURN (UNI=UNIFY.TERMLISTS (CDR TERM1) (CDR TERM2)))))))

(DEFUN UNI=UNIFY.TERMLISTS (TERMLIST1 TERMLIST2)
						; edited: 16-sep-83 11:18:38   
						; Authors: BUERCKERT
						; input:  2 arbitrary termlists of same length
						; value:  unifiers = nil, if not unifiable
						;         unifiers = (nil), if the termlists are equal
						;         unifiers = list of unifiers, so that
						;         s(termlist1) = s(termlist2) with each
						;         unifier s.
						; effect: while there is only one unifier in unifiers
						;         it will be worked with its bindings.
						;         if there are more unifiers, their bindings
						;         will be set before calling uni=unify.terms
						;         and reset after returning.
						; warning: returns with bindings, if only one unifier
  (PROG ((UNIFIERS (LIST NIL)) NEWUNIFIERS T1 T2)
	(WHILE (AND UNIFIERS (NULL (CAR UNIFIERS)) TERMLIST1)
	  (SETQ T1 (CAR TERMLIST1))
	  (SETQ TERMLIST1 (CDR TERMLIST1))
	  (SETQ T2 (CAR TERMLIST2))
	  (SETQ TERMLIST2 (CDR TERMLIST2))
	  (SETQ UNIFIERS (UNI=UNIFY.TERMS T1 T2)))
	(WHILE (AND UNIFIERS TERMLIST1)
	  (PROG (UNILIST)
		(COND
		  ((CDR UNIFIERS)		; unifiers = list of at least two unifiers
		   (SETQ T1 (CAR TERMLIST1))
		   (SETQ TERMLIST1 (CDR TERMLIST1)) (SETQ T2 (CAR TERMLIST2)) (SETQ TERMLIST2 (CDR TERMLIST2))
		   (MAPC
		     #'(LAMBDA (UNIFIER)
			 (UNI=SET.BINDINGS UNIFIER) (SETQ UNILIST (UNI=UNIFY.TERMS T1 T2))
						; t1,t2 will be unified under the bindings of unifier
			 (UNI=RESET.BINDINGS UNIFIER)
			 (COND
			   (UNILIST
			    (COND
			      ((CDR UNILIST)
			       (SETQ UNILIST (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST UNIFIER UNILIST)))
			      (T (UNI=RESET.BINDINGS (CAR UNILIST))
				 (SETQ UNILIST (LIST (UNI=COMPOSITION.OF.SUBSTITUTIONS UNIFIER (CAR UNILIST))))))
			    (COND (NEWUNIFIERS (SETQ NEWUNIFIERS (NCONC NEWUNIFIERS UNILIST))) (T (SETQ NEWUNIFIERS UNILIST))))))
		     UNIFIERS)
		   (COND ((AND (CAR NEWUNIFIERS) (NULL (CDR NEWUNIFIERS))) (UNI=SET.BINDINGS (CAR NEWUNIFIERS))))
		   (SETQ UNIFIERS NEWUNIFIERS)
		   (SETQ NEWUNIFIERS NIL))
		  (T				; unifiers = list of only one unifier, with bindings]
		   (SETQ T1 (CAR TERMLIST1))
		   (SETQ TERMLIST1 (CDR TERMLIST1)) (SETQ T2 (CAR TERMLIST2)) (SETQ TERMLIST2 (CDR TERMLIST2))
		   (SETQ UNILIST (UNI=UNIFY.TERMS T1 T2))
		   (PROG ((UNIFIER (CAR UNIFIERS)))
			 (COND
			   (UNILIST
			    (COND
			      ((CDR UNILIST) (UNI=RESET.BINDINGS UNIFIER)
			       (SETQ NEWUNIFIERS (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST UNIFIER UNILIST)))
			      (T (SETQ NEWUNIFIERS (LIST (UNI=COMPOSITION.OF.SUBSTITUTIONS UNIFIER (CAR UNILIST)))))))
			   (T (UNI=RESET.BINDINGS UNIFIER))))
		   (SETQ UNIFIERS NEWUNIFIERS) (SETQ NEWUNIFIERS NIL)))))
	(RETURN UNIFIERS)))

(DEFUN UNI=UNIFY.ATOMS (PRED1 TERMLIST1 PRED2 TERMLIST2)
						; edited: 17-dec-80 09:59:30
						; Authors: BUERCKERT
						; input:  pred1,pred2 - predicate symbols
						;         termlist1,termlist2 - termlists
						;         where (pred1 termlist1) and
						;         (pred2 termlist2) form two atomic formulas
						;         which shall be unified
						; effect: returns value
						; value:  list of most general unifiers which unify
						;         the given atoms under a given heory and
						;         with respect to symmetric properties of the
						;         given predicate symbols.
  (LET (UNIFIERS)
    (COND
      ((DT-PREDICATE.ARE.SAME PRED1 PRED2) (SETQ UNIFIERS (UNI=UNBIND (UNI=UNIFY.TERMLISTS TERMLIST1 TERMLIST2)))
       (COND
	 ((OR (NOT (DT-PREDICATE.IS.SYMMETRIC PRED1)) (AND UNIFIERS (NULL (CAR UNIFIERS))))
	  UNIFIERS)
	 (T (UNI=UNION.OF.SUBSTITUTIONLISTS UNIFIERS
					    (UNI=UNBIND
					      (UNI=UNIFY.TERMLISTS (CONS (SECOND TERMLIST1) (CONS (CAR TERMLIST1)
												  (CDDR TERMLIST1))) TERMLIST2))))))
      (T NIL))))

(DEFUN UNI=UNIFY.QUICK.TEST (E1 E2)
						; edited:  04.07.84  by  hjb
						; Authors: BUERCKERT
						; input:   two terms or two termlists of same length
						; value:   nil, if they are not unifiable
						;          t, if they may be unifiable
						; note:    to get a rather fast test the occurence
						;          test and the sort checks are omitted and a
						;          'loop'-implementation is used.
  (PROG (RESULTFLAG (EL1 (CAR E1)) (EL2 (CAR E2)) (EE1 (CDR E1)) (EE2 (CDR E2))) LOOP
	(COND
	  ((NOT
	     (SETQ RESULTFLAG
		   (OR (EQL EL1 EL2) (UNI=VARIABLE.IS EL1) (UNI=VARIABLE.IS EL2)
		       (COND
			 ((AND (DT-TERM.IS.ABBREVIATION EL1) (CONSP EL2)) (UNI=UNIFY.QUICK.TEST (DT-ABBREVIATION.TERM EL1) EL2))
			 ((AND (DT-TERM.IS.ABBREVIATION EL2) (CONSP EL1)) (UNI=UNIFY.QUICK.TEST EL1 (DT-ABBREVIATION.TERM EL2)))
			 ((AND (CONSP EL1) (CONSP EL2)) (UNI=UNIFY.QUICK.TEST EL1 EL2))))))
	   (RETURN NIL))
	  ((NOT EE1) (RETURN RESULTFLAG))
	  (T (SETQ EL1 (CAR EE1)) (SETQ EL2 (CAR EE2)) (SETQ EE1 (CDR EE1)) (SETQ EE2 (CDR EE2)) (GO LOOP)))))

(DEFUN UNI=UNIFY.MIXED.TERMLIST (TERMLIST)
						; edited: 27-sep-83 14:19:06                       hjb
						; Authors: BUERCKERT
						; input:  termlist - a unifier, especially number of
						;         toplevel-elements should be even.
						; effect: if for example termlist = (t1 t2 t3 t4),
						;         then the 2 termlists (t1 t3) and (t2 t4)
						;         will be unified.
						; value:  same as in uni=unify.termlists by unifying
						;         the termlists above.
						; warning]returns with bindings, if only one unifier]
  (IF TERMLIST
      (LET ((UNIFIERS (LIST NIL)) NEWUNIFIERS T1 T2)
	(WHILE (AND UNIFIERS (NULL (CAR UNIFIERS)) TERMLIST)
	  (SETQ T1       (CAR TERMLIST)
		T2       (SECOND TERMLIST)
		TERMLIST (CDDR TERMLIST)
		UNIFIERS (UNI=UNIFY.TERMS T1 T2)))
	(WHILE (AND UNIFIERS TERMLIST)
	  (LET (UNILIST)
	    (COND ((CDR UNIFIERS)		; unifiers = list of at least 2 unifiers
		   (SETQ T1 (CAR TERMLIST))
		   (SETQ T2 (SECOND TERMLIST)) (SETQ TERMLIST (CDDR TERMLIST))
		   (MAPC #'(LAMBDA (UNIFIER)
			     (UNI=SET.BINDINGS UNIFIER)
			     (SETQ UNILIST (UNI=UNIFY.TERMS T1 T2))
						; t1,t2 will be unified under the bindings of unifier
			     (UNI=RESET.BINDINGS UNIFIER)
			     (IF UNILIST
				 (COND ((CDR UNILIST)
					(SETQ UNILIST (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
							UNIFIER UNILIST)))
				       (T (UNI=RESET.BINDINGS (CAR UNILIST))
					  (SETQ UNILIST (LIST (UNI=COMPOSITION.OF.SUBSTITUTIONS
								UNIFIER (CAR UNILIST))))))
				 (IF NEWUNIFIERS
				     (SETQ NEWUNIFIERS (NCONC NEWUNIFIERS UNILIST))
				     (SETQ NEWUNIFIERS UNILIST))))
			 UNIFIERS)
		   (COND ((AND (CAR NEWUNIFIERS) (NULL (CDR NEWUNIFIERS))) (UNI=SET.BINDINGS (CAR NEWUNIFIERS))))
		   (SETQ UNIFIERS NEWUNIFIERS) (SETQ NEWUNIFIERS NIL))
		  (T				; unifiers = list of only one unifier
		   (SETQ T1 (CAR TERMLIST))
		   (SETQ T2 (SECOND TERMLIST)) (SETQ TERMLIST (CDDR TERMLIST)) (SETQ UNILIST (UNI=UNIFY.TERMS T1 T2))
		   (PROG ((UNIFIER (CAR UNIFIERS)))
			 (COND
			   (UNILIST
			    (COND
			      ((CDR UNILIST) (UNI=RESET.BINDINGS UNIFIER)
			       (SETQ NEWUNIFIERS
				     (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST UNIFIER UNILIST)))
			      (T (SETQ NEWUNIFIERS (LIST (UNI=COMPOSITION.OF.SUBSTITUTIONS UNIFIER (CAR UNILIST)))))))
			   (T (UNI=RESET.BINDINGS UNIFIER))))
		   (SETQ UNIFIERS NEWUNIFIERS) (SETQ NEWUNIFIERS NIL)))))
	UNIFIERS)
      (LIST NIL)))

(DEFUN UNI=UNIFY.TERMLISTS.WITH.RULE (TERMLIST1 TERMLIST2 CLAUSE LITNO1 LITNO2)
						; edited: 30-sep-83 13:12:48
						; Authors: BUERCKERT
						; input:  two termlists and a rule-clause
						; value:  the unifiers of termlist1 and termlist2
						;         with respect to the rule clauses.
						;         the variables of the rule clause are
						;         not renamed.
  (PROG
    ((VARIABLES (DS-CLAUSE.VARIABLES CLAUSE)) (TL1 (DS-CLAUSE.TERMLIST CLAUSE LITNO1))
     (TL2 (DS-CLAUSE.TERMLIST CLAUSE LITNO2)) (TL (APPEND TERMLIST1 (COPY-LIST TERMLIST2))) UNI1 UNI2)
    (AND (SETQ UNI1 (UNI=UNBIND (UNI=UNIFY.TERMLISTS (APPEND TL1 (COPY-LIST TL2)) TL)))
	 (SETQ UNI1 (UNI=REMOVE.COMPONENTS UNI1 VARIABLES)) (NULL (CAR UNI1)) (RETURN UNI1))
    (COND
      ((DT-PREDICATE.IS.SYMMETRIC (DS-CLAUSE.PREDICATE CLAUSE LITNO1))
       (SETQ TL1 (CONS (SECOND TL1) (CONS (CAR TL1) (CDDR TL1))))
       (COND
	 ((SETQ UNI2 (UNI=REMOVE.COMPONENTS (UNI=UNBIND (UNI=UNIFY.TERMLISTS (APPEND TL1 (COPY-LIST TL2)) TL)) VARIABLES))
	  (COND ((NULL (CAR UNI2)) (RETURN UNI2))) (SETQ UNI1 (UNI=WEAK.UNION.OF.SUBSTITUTIONLISTS UNI1 UNI2 VARIABLES T))))
       (COND
	 ((DT-PREDICATE.IS.SYMMETRIC (DS-CLAUSE.PREDICATE CLAUSE LITNO2))
	  (SETQ TL2 (CONS (SECOND TL2) (CONS (CAR TL2) (CDDR TL2))))
	  (COND
	    ((SETQ UNI2
		   (UNI=REMOVE.COMPONENTS (UNI=UNBIND (UNI=UNIFY.TERMLISTS (APPEND TL1 (COPY-LIST TL2)) TL)) VARIABLES))
	     (COND ((NULL (CAR UNI2)) (RETURN UNI2)))
	     (SETQ UNI1 (UNI=WEAK.UNION.OF.SUBSTITUTIONLISTS UNI1 UNI2 VARIABLES T))))
	  (COND
	    ((SETQ UNI2
		   (UNI=REMOVE.COMPONENTS
		     (UNI=UNBIND (UNI=UNIFY.TERMLISTS (APPEND (DS-CLAUSE.TERMLIST CLAUSE LITNO1) (COPY-LIST TL2)) TL)) VARIABLES))
	     (COND ((NULL (CAR UNI2)) (RETURN UNI2)))
	     (SETQ UNI1 (UNI=WEAK.UNION.OF.SUBSTITUTIONLISTS UNI1 UNI2 VARIABLES T)))))))
      ((DT-PREDICATE.IS.SYMMETRIC (DS-CLAUSE.PREDICATE CLAUSE LITNO2))
       (COND
	 ((SETQ UNI2
		(UNI=REMOVE.COMPONENTS
		  (UNI=UNBIND
		    (UNI=UNIFY.TERMLISTS (APPEND TL1 (CONS (SECOND TL2) (CONS (CAR TL2) (CDDR TL2)))) TL))
		  VARIABLES))
	  (COND ((NULL (CAR UNI2)) (RETURN UNI2))) (SETQ UNI1 (UNI=WEAK.UNION.OF.SUBSTITUTIONLISTS UNI1 UNI2 VARIABLES T))))))
    (RETURN UNI1)))

(defun uni=unmatchable (term1 term2)
						; Edited:  27-AUG-1989 01:17
						; Authors: PRCKLN@MUNIN
						; Input:   Two terms
						; Effect:  -
						; Value:   If a fast check states that there cannot be any matcher
						;          s(term1) = term2 the this function returns true, i.e. if
						;          it returns true there can be no matcher.
  (if (dt-constant.is term1)
      (if (dt-constant.is term2)
	  (not (eq term1 term2))
	  (if (consp term2)
	      t
	      nil))
      (if (consp term1)
	  (if (intersection (DT-FUNCTION.ATTRIBUTES (CAR TERM1)) (DT-FUNCTION.THEORIES))
	      nil
	      (if (dt-constant.is term2)
		  t
		  (if (consp term2)
		      (or (not (eq (first term1) (first term2)))
			  (some #'uni=unmatchable (rest term1) (rest term2)))
		      nil)))
	  nil)))

(DEFUN UNI-UNIFY1.TERMS (TERM1 TERM2 VARIABLES &OPTIONAL BOOLEAN.RESULTFLAG)
						; input:  two terms and a list of variables or t,
						;         and a flag.
						; value:  a list of matchers s with s(term1) = term2.
						;         if the flag is t, only t or nil is returned.
						; effect: if variables = t then the variables of term2
						;         else the variables in the list will be
						;         regarded as uni-constants (with respect to
						;         their bindings, if exist).
						; note:   variables must contain all variables of
						;         term2.
						;         you can call this function with third
						;         parameter = nil, but then the variables of
						;         term2 should be declared as uni-constants]
  (if (DT-FUNCTION.THEORIES)
      (thu-terms term1 term2
		 (let (VARIABLE.BUFFER)
		   (COND
		     ((EQL VARIABLES T)
		      (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK)
			    VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES TERM2 VARIABLE.BUFFER))))
		     ((AND VARIABLES UNI*BINDINGFLAG)
		      (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK)
			    VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES VARIABLES VARIABLE.BUFFER)))))
		   variables))
      (if (uni=unmatchable term1 term2)
	  nil
	  (let (VARIABLE.BUFFER MATCHER MATCHER.BUFFER)
	    (COND
	      ((EQL VARIABLES T)
	       (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK)
		     VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES TERM2 VARIABLE.BUFFER))))
	      ((AND VARIABLES UNI*BINDINGFLAG)
	       (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK)
		     VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES VARIABLES VARIABLE.BUFFER)))))
	    (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS VARIABLES)
	    (COND
	      ((DT-FUNCTION.THEORIES)
	       (SETQ MATCHER (UNI=UNBIND (UNI=UNIFY.TERMS TERM1 TERM2))
		     MATCHER (if matcher
				 (if BOOLEAN.RESULTFLAG t (UNI=COPY MATCHER)))))
	      (T (SETQ MATCHER.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
		 (UNI=MATCH.TERMS TERM1 TERM2 MATCHER.BUFFER)
		 (SETQ MATCHER (BUFFER.CONTENTS MATCHER.BUFFER))
		 (COND ((EQL (CAR MATCHER) 'FAIL) (SETQ MATCHER NIL))
		       ((NULL MATCHER) (SETQ MATCHER (COND (BOOLEAN.RESULTFLAG T) (T (LIST NIL)))))
		       (T (UNI=RESET.BINDINGS MATCHER)
			  (SETQ MATCHER (COND (BOOLEAN.RESULTFLAG T) (T (LIST (COPY-TREE MATCHER)))))))
		 (PUSH.BUFFER.STACK MATCHER.BUFFER UNI*BUFFER.STACK)))
	    (COND (VARIABLE.BUFFER (PUSH.BUFFER.STACK VARIABLE.BUFFER UNI*BUFFER.STACK)))
	    (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL)
	    MATCHER))))

(DEFUN UNI-UNIFY1.TERMLISTS (TERMLIST1 TERMLIST2 VARIABLES &OPTIONAL BOOLEAN.RESULTFLAG)
						; input:  two termlists and a list of variables or t,
						;         and a flag.
						; value:  a list of matchers s with s(termlist1) =
						;         termlist2.
						;         if the flag is t, only t or nil is returned.
						; effect: if variables = t then the variables of term=
						;         list2 else the variables in the list will be
						;         regarded as uni-constants (with respect to
						;         their bindings, if exist).
						; note:   variables must contain all variables of
						;         termlist2.
						;         you can call this function with third
						;         parameter = nil, but then the variables of
						;         termlist2 should be declared as constants]
  (PROG (VARIABLE.BUFFER MATCHER MATCHER.BUFFER)
	(COND
	  ((EQL VARIABLES T) (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	   (SETQ VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES TERMLIST2 VARIABLE.BUFFER))))
	  ((AND VARIABLES UNI*BINDINGFLAG) (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	   (SETQ VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES VARIABLES VARIABLE.BUFFER)))))
	(SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS VARIABLES)
	(COND
	  ((DT-FUNCTION.THEORIES) (SETQ MATCHER (UNI=UNBIND (UNI=UNIFY.TERMLISTS TERMLIST1 TERMLIST2)))
	   (SETQ MATCHER (COND ((AND MATCHER BOOLEAN.RESULTFLAG) T) (MATCHER (UNI=COPY MATCHER)))))
	  (T (SETQ MATCHER.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	     (UNI=MATCH.TERMLISTS TERMLIST1 TERMLIST2 MATCHER.BUFFER) (SETQ MATCHER (BUFFER.CONTENTS MATCHER.BUFFER))
	     (COND ((EQL (CAR MATCHER) 'FAIL) (SETQ MATCHER NIL))
		   ((NULL MATCHER) (SETQ MATCHER (COND (BOOLEAN.RESULTFLAG T) (T (LIST NIL)))))
		   (T (UNI=RESET.BINDINGS MATCHER)
		      (SETQ MATCHER (COND (BOOLEAN.RESULTFLAG T) (T (LIST (COPY-TREE MATCHER)))))))
	     (PUSH.BUFFER.STACK MATCHER.BUFFER UNI*BUFFER.STACK)))
	(COND (VARIABLE.BUFFER (PUSH.BUFFER.STACK VARIABLE.BUFFER UNI*BUFFER.STACK)))
	(SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL) (RETURN MATCHER)))

(DEFUN UNI-UNIFY1.ATOMS (PRED1 TERMLIST1 PRED2 TERMLIST2 VARIABLES &OPTIONAL BOOLEAN.RESULTFLAG)
						; input:  two predicates,two termlists and a list of
						;         variables or t, and a flag.
						; value:  a list of matchers for the termlists if the
						;         the predicates are equal, else nil.
						;         if the flag is t, only t or nil is returned.
						; effect: if the predicate is symmetric, then the
						;         matchers of the termlists where on of them
						;         is switched are insert in the result, too.
						;         see uni-unify1.termlists for more informa=
						;         tion, especially for the role of variables.
  (PROG (VARIABLE.BUFFER MATCHER1 MATCHER2 MATCHER1.BUFFER MATCHER2.BUFFER RESULT)
	(COND
	  ((EQL VARIABLES T) (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	   (SETQ VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES TERMLIST2 VARIABLE.BUFFER))))
	  ((AND VARIABLES UNI*BINDINGFLAG) (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	   (SETQ VARIABLES (BUFFER.CONTENTS (UNI=FIND.VARIABLES VARIABLES VARIABLE.BUFFER)))))
	(SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS VARIABLES)
	(COND
	  ((DT-FUNCTION.THEORIES) (SETQ RESULT (UNI=UNBIND (UNI=UNIFY.ATOMS PRED1 TERMLIST1 PRED2 TERMLIST2)))
	   (SETQ RESULT (COND ((AND RESULT BOOLEAN.RESULTFLAG) T) (RESULT (UNI=COPY RESULT)))))
	  ((DT-PREDICATE.ARE.SAME PRED1 PRED2) (SETQ MATCHER1.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	   (UNI=MATCH.TERMLISTS TERMLIST1 TERMLIST2 MATCHER1.BUFFER) (SETQ MATCHER1 (BUFFER.CONTENTS MATCHER1.BUFFER))
	   (COND
	     ((DT-PREDICATE.IS.SYMMETRIC PRED1) (SETQ MATCHER2.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	      (COND
		((EQL (CAR MATCHER1) 'FAIL)
		 (UNI=MATCH.TERMLISTS (CONS (SECOND TERMLIST1) (CONS (CAR TERMLIST1) (CDDR TERMLIST1))) TERMLIST2
				      MATCHER2.BUFFER)
		 (SETQ MATCHER2 (BUFFER.CONTENTS MATCHER2.BUFFER))
		 (COND ((EQL (CAR MATCHER2) 'FAIL) (SETQ RESULT NIL))
		       ((NULL MATCHER2) (SETQ RESULT (COND (BOOLEAN.RESULTFLAG T) (T (LIST NIL)))))
		       (T (UNI=RESET.BINDINGS MATCHER2)
			  (SETQ RESULT (COND (BOOLEAN.RESULTFLAG T) (T (LIST (COPY-TREE MATCHER2))))))))
		(BOOLEAN.RESULTFLAG		; now we know that the atoms are matchable and hence
						; we have to do nothing else but to return t.
		 (UNI=RESET.BINDINGS MATCHER1)
		 (SETQ RESULT T))
		((NULL MATCHER1)
						; both termlists are equal,(nil) is most general
		 (SETQ RESULT (LIST NIL)))
		(T (UNI=RESET.BINDINGS MATCHER1)
		   (UNI=MATCH.TERMLISTS (CONS (SECOND TERMLIST1) (CONS (CAR TERMLIST1) (CDDR TERMLIST1))) TERMLIST2
					MATCHER2.BUFFER)
		   (SETQ MATCHER2 (BUFFER.CONTENTS MATCHER2.BUFFER))
		   (COND ((EQL (CAR MATCHER2) 'FAIL) (SETQ RESULT (LIST (COPY-TREE MATCHER1))))
			 ((NULL MATCHER2)
						; the switched termlists are equal,(nil) are the mgm's
			  (SETQ RESULT (LIST NIL)))
			 ((UNI=BINDING.IS.INSTANCE.OF MATCHER1)
						; note that matcher2 is the bindingenviroment]
			  (UNI=RESET.BINDINGS MATCHER2)
			  (SETQ RESULT (LIST (COPY-TREE MATCHER1))))
			 (T (UNI=RESET.BINDINGS MATCHER2) (UNI=SET.BINDINGS MATCHER1)
			    (SETQ RESULT
				  (COND ((UNI=BINDING.IS.INSTANCE.OF MATCHER1) (LIST (COPY-TREE MATCHER2)))
					(T (LIST (COPY-TREE MATCHER1) (COPY-TREE MATCHER2)))))
			    (UNI=RESET.BINDINGS MATCHER1)))))
	      (PUSH.BUFFER.STACK MATCHER2.BUFFER UNI*BUFFER.STACK))
	     ((EQL (CAR MATCHER1) 'FAIL) (SETQ RESULT NIL))
	     ((NULL MATCHER1) (SETQ RESULT (COND (BOOLEAN.RESULTFLAG T) (T (LIST NIL)))))
	     (T (UNI=RESET.BINDINGS MATCHER1)
		(SETQ RESULT (COND (BOOLEAN.RESULTFLAG T) (T (LIST (COPY-TREE MATCHER1)))))))
	   (PUSH.BUFFER.STACK MATCHER1.BUFFER UNI*BUFFER.STACK)))
	(COND (VARIABLE.BUFFER (PUSH.BUFFER.STACK VARIABLE.BUFFER UNI*BUFFER.STACK)))
	(SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL) (RETURN RESULT)))

(DEFUN UNI-MATCHABLE (TERMLIST1 TERMLIST2)
						; edited: 4. 8. 1984
						; Authors: BUERCKERT
						; input:  two termlists (possibly with bindings)
						;         the variables in termlist2 are declared
						;         as constants.
						; value:  t if termlist2 is an instance of termlist1
						;         else nil.
  (COND ((DT-FUNCTION.THEORIES) (UNI=UNBIND (UNI=UNIFY.TERMLISTS TERMLIST1 TERMLIST2)))
	(T
	 (PROG (MATCHER MATCHER.BUFFER) (SETQ MATCHER.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	       (UNI=MATCH.TERMLISTS TERMLIST1 TERMLIST2 MATCHER.BUFFER) (SETQ MATCHER (BUFFER.CONTENTS MATCHER.BUFFER))
	       (PUSH.BUFFER.STACK MATCHER.BUFFER UNI*BUFFER.STACK)
	       (COND ((NEQ (CAR MATCHER) 'FAIL) (UNI=RESET.BINDINGS MATCHER) (RETURN T)))))))

(DEFUN UNI-UNIFIER.IS.MATCHER (UNIFIERS PRED1 TERMLIST1 PRED2 TERMLIST2 VARIABLES &OPTIONAL COPYFLAG)
  (DECLARE (IGNORE PRED1 TERMLIST1 PRED2 TERMLIST2))
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; input:  unifiers - a list of unifiers of
						;           pred1(termlist1) and
						;           pred2(termlist2)
						;         pred1, pred2 - two predicates
						;         termlist1,termlist2 - two termlists
						;         variables - a superset of the
						;           variables of termlist2
						;         copyflag - a flag
						; effect: returns value; if copyflag = nil
						;         unifiers is destructively changed
						;         during computation else not
						; value:  a list of matchers of pred1(term-
						;         list1) and pred2(termlist2) if they
						;         are matchable, otherwise nil
  
  (PROG (MATCHER CAR.TAIL CADR.TAIL RESULT)
	(MAPC
	  #'(LAMBDA (UNI) (SETQ MATCHER (COND (COPYFLAG (COPY-TREE UNI)) (T UNI)))
		    (SMAPL
		      #'(LAMBDA (TAIL) (SETQ CAR.TAIL (CAR TAIL)) (SETQ CADR.TAIL (SECOND TAIL))
				(COND
				  ((MEMBER CAR.TAIL VARIABLES)
				   (COND
				     ((AND (UNI=VARIABLE.IS CADR.TAIL) (NOT (MEMBER CADR.TAIL VARIABLES)))
				      (NSUBST CAR.TAIL CADR.TAIL MATCHER) (RPLACA TAIL CADR.TAIL))
				     (T (SETQ MATCHER 'FAIL))))))
		      #'(LAMBDA (TAIL) (COND ((NEQ MATCHER 'FAIL) (CDDR TAIL)) (T NIL))) MATCHER)
		    (COND ((NEQ MATCHER 'FAIL) (SETQ RESULT (CONS MATCHER RESULT)))))
	  UNIFIERS)
	(RETURN RESULT)))

(DEFUN UNI-UNIFIER.CANBE.MATCHER (UNIFIER VARIABLES)
  (PROG ((VAR (CAR UNIFIER))
	 (TERM (SECOND UNIFIER))
	 (REST (CDDR UNIFIER)))
     LOOP
	(COND ((AND (MEMBER VAR VARIABLES) (OR (NOT (UNI=VARIABLE.IS TERM)) (MEMBER TERM VARIABLES))) (RETURN NIL))
	      ((NOT REST) (RETURN T))
	      (T (SETQ VAR (CAR REST)
		       TERM (SECOND REST)
		       REST (CDDR REST))
		 (GO LOOP)))))


(DEFUN UNI-UNIFIER.BECOMES.MATCHER (UNIFIER VARIABLES &OPTIONAL BOOLEAN.RESULTFLAG COPY.FLAG)
						; edited: 30-nov-83 12:49:40                       hjb
						; Authors: BUERCKERT
						; input:  a substitution and a list of variables
						; value:  list of matchers with respect to variables
						;         under a bindingenviroment, if possible.
						;         nil, if not.
						; effect: unifier will be merged with a substitution
						;         representing the bindingenviroment and then
						;         it will be made to a matcher with respect to
						;         a variablelist obtained by applying the
						;         bindings to the inputlist of variables.
  (WHEN COPY.FLAG (SETQ UNIFIER (COPY-TREE UNIFIER)))
  (IF UNIFIER
      (LET (VLIST MATCHERS)
	(SETQ VLIST (UNI=TEMP.FIND.VARIABLES VARIABLES))
	(SETQ MATCHERS (IF UNI*BINDINGFLAG
			   (IF BOOLEAN.RESULTFLAG
			       (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNIFIER))
			       (UNI=COPY (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNIFIER))))
			   (LIST UNIFIER)))
	(UNI=MAKE.MATCHER MATCHERS VLIST))
      (LIST NIL)))

(DEFUN UNI=MATCH.TERMS (T1 T2 MATCHER.BUFFER)
						; input:  two terms and a buffer which contents is a
						;         substitution or nil.
						; value:  matcher, changed depending on matchability
						;         of t1 and t2:
						;         its contents is s with s(t1) = t2 or is nil
						;         if t1 = t2 or matcher is 'fail' if the are
						;         not matchable.
						; effect: if the terms are not matchable the bindings
						;         of the input matcher are reset and the
						;         is push back to uni*buffer.stack
  (PROG (BINDING.OF.T1 BINDING.OF.T2)
	(COND ((EQL T1 T2) NIL)
	      ((UNI=VARIABLE.IS T2)
	       (COND
		 ((SETQ BINDING.OF.T2 (DT-VARIABLE.GET.BINDING T2))
		  (UNI=MATCH.TERMS T1 BINDING.OF.T2 MATCHER.BUFFER))
		 (T (ERROR "Variable in t2 while matching t1 t2: ~a" t2))))
	      ((UNI=VARIABLE.IS T1)
	       (COND
		 ((SETQ BINDING.OF.T1 (DT-VARIABLE.GET.BINDING T1))
		  (UNI=MATCH.TERMS BINDING.OF.T1 T2 MATCHER.BUFFER))
		 (T (SETQ BINDING.OF.T2 (COND (UNI*BINDINGFLAG (UNI=INSERT.BINDINGS.IN T2))
					      (T T2)))
		    (COND
		      ((DT-SORT.IS.SUBSORT (DT-TERM.SORT BINDING.OF.T2) (UNI=VARIABLE.SORT T1))
		       (DT-VARIABLE.PUT.BINDING T1 BINDING.OF.T2) (BUFFER.CONS BINDING.OF.T2 MATCHER.BUFFER)
		       (BUFFER.CONS T1 MATCHER.BUFFER))
		      (T (UNI=RESET.BINDINGS (BUFFER.CONTENTS MATCHER.BUFFER)) (BUFFER.CONS 'FAIL MATCHER.BUFFER))))))
	      ((OR (UNI=CONSTANT.IS T1) (UNI=CONSTANT.IS T2))
	       (UNI=RESET.BINDINGS (BUFFER.CONTENTS MATCHER.BUFFER))
	       (BUFFER.CONS 'FAIL MATCHER.BUFFER))
	      ((AND (DT-TERM.IS.ABBREVIATION T1) (DT-TERM.IS.ABBREVIATION T2))
	       (UNI=RESET.BINDINGS (BUFFER.CONTENTS MATCHER.BUFFER))
	       (BUFFER.CONS 'FAIL MATCHER.BUFFER))
	      (T (COND ((DT-TERM.IS.ABBREVIATION T1) (SETQ T1 (DT-ABBREVIATION.TERM T1))))
		 (COND ((DT-TERM.IS.ABBREVIATION T2) (SETQ T2 (DT-ABBREVIATION.TERM T2))))
		 (COND
		   ((NEQ (CAR T1) (CAR T2))
		    (UNI=RESET.BINDINGS (BUFFER.CONTENTS MATCHER.BUFFER))
		    (BUFFER.CONS 'FAIL MATCHER.BUFFER))
		   (T (UNI=MATCH.TERMLISTS (CDR T1) (CDR T2) MATCHER.BUFFER)))))))

(DEFUN UNI=MATCH.TERMLISTS (TL1 TL2 MATCHER.BUFFER)
						; input:  two termlists and a buffer which contents
						;         is a substitution or nil.
						; value:  if tl1 and tl2 are matchable, then a buffer
						;         with contents s, such that s(tl1) = tl2
						;         or s = nil if tl1 = tl2.
						;         if they are not matchable, then 'fail'.
						; effect: if not matchable, the bindings of the sub=
						;         stitution in matcher are reset and the
						;         buffer is pushed back to uni*buffer.stack
  (SMAPC #'(LAMBDA (T1 T2) (UNI=MATCH.TERMS T1 T2 MATCHER.BUFFER))
	 #'(LAMBDA (TAIL) (COND ((NEQ (CAR (BUFFER.CONTENTS MATCHER.BUFFER)) 'FAIL) (CDR TAIL))))
	 TL1 TL2))

(DEFUN UNI=MATCH.WITH.BUFFER (TERMLIST1 TERMLIST2 BOOLEAN.RESULTFLAG)
  (PROG (MATCHER MATCHER.BUFFER)
	(COND
	  ((DT-FUNCTION.THEORIES) (SETQ MATCHER (UNI=UNBIND (UNI=UNIFY.TERMLISTS TERMLIST1 TERMLIST2)))
	   (SETQ MATCHER (COND ((AND MATCHER BOOLEAN.RESULTFLAG) T) (MATCHER (UNI=COPY MATCHER)))))
	  (T (SETQ MATCHER.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK))
	     (UNI=MATCH.TERMLISTS TERMLIST1 TERMLIST2 MATCHER.BUFFER) (SETQ MATCHER (BUFFER.CONTENTS MATCHER.BUFFER))
	     (COND ((EQL (CAR MATCHER) 'FAIL) (SETQ MATCHER NIL))
		   ((NULL MATCHER) (SETQ MATCHER (COND (BOOLEAN.RESULTFLAG T) (T (LIST NIL)))))
		   (T (UNI=RESET.BINDINGS MATCHER)
		      (SETQ MATCHER (COND (BOOLEAN.RESULTFLAG T) (T (LIST (COPY-TREE MATCHER)))))))
	     (PUSH.BUFFER.STACK MATCHER.BUFFER UNI*BUFFER.STACK)))
	(RETURN MATCHER)))

(DEFUN UNI=MAKE.MATCHER (UNIFIERS VARIABLES)
						; edited:  1-dec-83 18:09:30                       hjb
						; Authors: BUERCKERT
						; input:   list of unifiers, list of variables
						; value:   list of matchers, where their codomains are
						;          subsets of variables.
						;          nil, if the unifiers can't be changed
						; remark:  unifiers will be destroyed
  (LET (CAR.TAIL CADR.TAIL RESULT)
    (MAPC #'(LAMBDA (MATCHER)
	      (SMAPL #'(LAMBDA (TAIL)
			 (SETQ CAR.TAIL (FIRST TAIL))
			 (SETQ CADR.TAIL (SECOND TAIL))
			 (WHEN (MEMBER CAR.TAIL VARIABLES)
			   (COND ((AND (UNI=VARIABLE.IS CADR.TAIL)
				       (NOT (MEMBER CADR.TAIL VARIABLES))
				       (EQL (DT-TERM.SORT CAR.TAIL) (DT-TERM.SORT CADR.TAIL)))
				  (NSUBST CAR.TAIL CADR.TAIL MATCHER)
				  (RPLACA TAIL CADR.TAIL))
				 (T (SETQ MATCHER 'FAIL)))))
		     #'(LAMBDA (TAIL) (COND ((NEQ MATCHER 'FAIL) (CDDR TAIL)) (T NIL)))
		     MATCHER)
	      (COND ((NEQ MATCHER 'FAIL) (SETQ RESULT (CONS MATCHER RESULT)))))
	  UNIFIERS)
    RESULT))

(DEFMACRO UNI-EQUAL.TERMS (TERM1 TERM2 &OPTIONAL BINDFLAG)
						; edited at 22-feb-83 09:53)
						; input:  t1,t2 - terms, bindflag - a flag
						; effect: returns value
						; value:  t,if t1 = t2
						;         under the given theories and under a
						;         bindingenviroment (if bindflag = t)
						;         nil, else
  `(COND (,BINDFLAG (UNI=EQUAL.TERMS (UNI=INSERT.BINDINGS.IN ,TERM1)
				     (UNI=INSERT.BINDINGS.IN ,TERM2)))
	 (T (UNI=EQUAL.TERMS ,TERM1 ,TERM2))))


(DEFUN UNI-EQUAL.TERMLISTS (TL1 TL2 &OPTIONAL BINDFLAG)
						; edited at 22-feb-83 09:54)
						; input:  tl1,tl2 - termlists, bindflag - a flag
						; effect: returns value
						; value:  t,if tl1 = tl2
						;         under the given theories and under a
						;         bindingenviroment (if bindflag = t)
						;         nil, else
  (COND ((NEQ (LIST-LENGTH TL1) (LIST-LENGTH TL2)) NIL)
	(BINDFLAG (UNI=EQUAL.TERMLISTS (UNI=INSERT.BINDINGS.IN TL1) (UNI=INSERT.BINDINGS.IN TL2)))
	(T (UNI=EQUAL.TERMLISTS TL1 TL2))))

(DEFUN UNI-EQUAL.ATOMS (PRED1 TL1 PRED2 TL2 &OPTIONAL BINDFLAG)
						; edited at 22-feb-83 09:55)
						; input:  pred1,pred2 - predicate symbols
						;         tl1,tl2 - termlists
						;         where (pred1 tl1) and (pred2 tl2) are
						;         atomic formulas which shall be equal
						;         bindflag - a flag
						; effect: returns value
						; value:  t,if (pred1 tl1) = (pred2 tl2)
						;         under the given theories and under a
						;         bindingenviroment (if bindflag = t),with
						;         respect to symmetric properties of the
						;         given predicate symbols else nil
  (COND (BINDFLAG (SETQ TL1 (UNI=INSERT.BINDINGS.IN TL1))
		  (SETQ TL2 (UNI=INSERT.BINDINGS.IN TL2))))
  (COND ((NOT (DT-PREDICATE.ARE.SAME PRED1 PRED2)) NIL)
	((NOT (DT-PREDICATE.IS.SYMMETRIC PRED1)) (UNI=EQUAL.TERMLISTS TL1 TL2))
	(T (OR (UNI=EQUAL.TERMLISTS TL1 TL2)
	       (UNI=EQUAL.TERMLISTS (CONS (SECOND TL1) (CONS (CAR TL1) (CDDR TL1))) TL2)))))

(DEFUN UNI=EQUAL.TERMS (TERM1 TERM2)
						; edited at 22-feb-83 09:53)
						; input:  t1,t2 - terms
						; effect: returns value
						; value:  t,if t1 = t2
						;         under the given theories else nil
  (EQUAL TERM1 TERM2))


(DEFUN UNI=EQUAL.TERMLISTS (TL1 TL2) 
						; edited at 22-feb-83 09:54)
						; input:  tl1,tl2 - termlists
						; effect: returns value
						; value:  t,if tl1 = tl2
						;         under the given theories else nil
  (EQUAL TL1 TL2))


(DEFMACRO UNI-MERGE.SUBSTITUTIONS (S1 S2 &OPTIONAL BNDEV BOOLEAN.RESULTFLAG)
						; edited:  8-nov-83 11:17:44                       hjb
						; Authors: BUERCKERT
						;  see uni=merge.substitutions
  `(COND (,BOOLEAN.RESULTFLAG (UNI=MERGE.SUBSTITUTIONS ,S1 ,S2 ,BNDEV))
	 (T (UNI=COPY (UNI=MERGE.SUBSTITUTIONS ,S1 ,S2 ,BNDEV)))))

(DEFMACRO UNI-MERGE.SUBSTITUTIONLISTS (LIST1 LIST2 &OPTIONAL BNDEV BOOLEAN.RESULTFLAG)
						; edited at 21-feb-83 17:35)
						;  see uni=merge.substitutionlists
  `(COND (,BOOLEAN.RESULTFLAG (UNI=REMOVE.INSTANCES (UNI=MERGE.SUBSTITUTIONLISTS ,LIST1 ,LIST2 ,BNDEV)))
	 (T (UNI=COPY (UNI=REMOVE.INSTANCES (UNI=MERGE.SUBSTITUTIONLISTS ,LIST1 ,LIST2 ,BNDEV))))))

(DEFMACRO UNI-MERGE.LIST.OF.SUBSTITUTIONLISTS (LIST &OPTIONAL BNDEV DESTRUCTIVEFLAG)
						; edited:  7-dec-83 11:23:59                       hjb
						; Authors: BUERCKERT
						; input:  list - list of substitutionlists
						;         bndev - unifier with bindings
						;         destructiveflag - a flag
						; value:  see uni=merge.list.of.substitutionlists
						; effect: if destructiveflag = t, list will be
						;         destroyed, else not.
  `(COND (,DESTRUCTIVEFLAG (UNI=MERGE.LIST.OF.SUBSTITUTIONLISTS ,LIST ,BNDEV))
	 (T (UNI=MERGE.LIST.OF.SUBSTITUTIONLISTS (COPY-TREE ,LIST) ,BNDEV))))

(DEFUN UNI-MERGE.BINDING.WITH.SUBSTITUTION (BNDEV UNIFIER)
						; edited: 22-dec-83 13:12:59                       hjb
						; Authors: BUERCKERT
						; input:  bndev - a substitution with bindings
						;         unifier - a substitution
						; effect: regards unifier as termlist and unifies it
						;         under the bindings of bndev - like uni-unify
						;         mixed.termlist. then the result - a list of
						;         substitutions will be composed with bndev.
						;         so the value is the same as uni-merge.
						;         substitutions (bndev unifier) without any
						;         bindings.
						; value:  list of substitutions merging bndev and
						;         unifier; or nil, if they are incompatible.
  (IF (NULL UNIFIER)
      (LIST BNDEV)
      (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
	BNDEV (UNI=COPY (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNIFIER))) T)))

(DEFUN UNI-MERGE1.SUBSTITUTIONS (S1 S2 VARIABLES &OPTIONAL BNDEV BOOLEAN.RESULTFLAG)
						; edited at 22-feb-83 10:03)
						; input:  s1, s2 - substitutions
						;         variables - t,nil or a list of all
						;         variables which occur in the rhs of s1
						; effect: if variables = t, it will be computed
						;         explicitly, else the given value is used
						; value:  list of all substitutions obtained by
						;         merging s1 and s2 where the variables of
						;         the rhs of s1 are regarded as constant
						;         nil if they are incompatible
  (COND ((AND (NEQ VARIABLES T) BNDEV)
	 (SETQ VARIABLES (UNI=TEMP.FIND.VARIABLES VARIABLES)))
	((EQL VARIABLES T)
	 (SETQ VARIABLES (SMAPCON #'(LAMBDA (TERM) (UNI=TEMP.FIND.VARIABLES (SECOND TERM))) #'CDDR S1))))
  (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS VARIABLES)
  (PROG1 (IF BOOLEAN.RESULTFLAG
	     (UNI=MERGE.SUBSTITUTIONS S1 S2 BNDEV)
	     (UNI=COPY (UNI=MERGE.SUBSTITUTIONS S1 S2 BNDEV)))
	 (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL)))

(DEFUN UNI-MERGE1.SUBSTITUTIONLISTS (LIST1 LIST2 VARIABLES BNDEV BOOLEAN.RESULTFLAG)
						; edited at 21-feb-83 17:55)
						; input:  list1,list2 - substitutionlists
						;         variables - a list of variables
						; effect: returns value
						; value:  list of all substitutions obtained by
						;         merging list1 and list2 where the
						;         variables are regarded as constants
						;         nil if s1 and s2 are incompatible
  (WHEN BNDEV (SETQ VARIABLES (UNI=TEMP.FIND.VARIABLES VARIABLES)))
  (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS VARIABLES)
  (PROG1 (IF BOOLEAN.RESULTFLAG
	     (UNI=MERGE.SUBSTITUTIONLISTS LIST1 LIST2 BNDEV)
	     (UNI=COPY (UNI=MERGE.SUBSTITUTIONLISTS LIST1 LIST2 BNDEV)))
	 (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL)))

(DEFUN UNI-MERGE1.LIST.OF.SUBSTITUTIONLISTS (LIST VARIABLES BNDEV BOOLEAN.RESULTFLAG)
						; edited at 21-feb-83 18:01)
						; input:  a list of lists of substitutions
						;         variables - a list of variables to be
						;         regarded as constants
						; value:  a list of merged substitutions
  (COND (BNDEV (SETQ VARIABLES (UNI=TEMP.FIND.VARIABLES VARIABLES)))) (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS VARIABLES)
  (PROG1
    (COND (BOOLEAN.RESULTFLAG (UNI=MERGE.LIST.OF.SUBSTITUTIONLISTS LIST BNDEV))
	  (T (UNI=COPY (UNI=MERGE.LIST.OF.SUBSTITUTIONLISTS LIST BNDEV))))
    (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL)))

(DEFMACRO UNI-MERGE.MATCHERS (MATCHER1 MATCHER2 &OPTIONAL RESULTFLAG)
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; input:  matcher1,matcher2 - two matchers, i.e.
						;         two substitutions where the codomain is
						;         regarded as ground
						;         resultflag - t or nil
						; effect: returns value
						; value:  resultflag = t: list of all substitutions
						;             obtained by merging matcher1 and
						;             matcher2; nil if they are incompatible
						;         resultflag = nil: t if matcher1 and
						;             matcher2 are compatible else nil
  `(COND (,RESULTFLAG (UNI=COPY (UNI=MERGE.MATCHERS ,MATCHER1 ,MATCHER2 ,RESULTFLAG)))
	 (T (UNI=MERGE.MATCHERS ,MATCHER1 ,MATCHER2 ,RESULTFLAG))))

(DEFUN UNI-MERGE.MATCHERLISTS (MATCHERLIST1 MATCHERLIST2 RESULTFLAG)
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; input:  matcherlist1,matcherlist2 - two lists of
						;         matchers ,i.e. two lists of
						;         substitutions where the codomain is
						;         regarded as ground
						;         resultflag - t or nil
						; effect: returns value
						; value:  resultflag = t: list of all substitutions
						;             obtained by merging matcherlist1 and
						;             matcherlist2; nil if they are
						;             incompatible
						;         resultflag = nil: t if matcherlist1 and
						;             matcherlist2 are compatible else nil
  (COND (RESULTFLAG (UNI=COPY (UNI=MERGE.MATCHERLISTS MATCHERLIST1 MATCHERLIST2 RESULTFLAG)))
	(T (UNI=MERGE.MATCHERLISTS MATCHERLIST1 MATCHERLIST2 RESULTFLAG))))

(defun uni-merge.list.of.matcherlists.fit.on.sorts (UNIFIERS.LIST VARIABLES BNDEV)
						; Edited:  10-DEC-1991 23:38
						; Authors: PRCKLN
						; Input:   See UNI-MERGE.LIST.OF.MATCHERLISTS.
						; Effect:  -
						; Value:   A matcherlist which is the merge of UNIFIERS.LIST, if
						;          such matchers exist. If sort literals are present the
						;          sort conditions are checked.
  (let ((result (UNI-MERGE.LIST.OF.MATCHERLISTS UNIFIERS.LIST VARIABLES BNDEV)))
    (if (opt-get.option sort_literals)
	(mapcan #'(lambda (subst) (upr-match.substitution subst variables)) result)
	result)))

(DEFUN UNI-MERGE.LIST.OF.MATCHERLISTS (UNIFIERS-LIST VARIABLES BNDEV)
						; edited:  5-dec-83 10:53:14                       hjb
						; Authors: BUERCKERT
						; input:  list of unifierlists
						;         list of variables
						;         substitution with bindings or nil
						; effect: each unifierlist will be merged with bndev.
						;         if none of this merges is nil, they will be
						;         changed to matcherlists with respect to the
						;         list of variables (after appliing bndev).
						;         if bndev = nil the input unifierlists will
						;         changed to matcherlists with respect to the
						;         input variables. if this changings are
						;         possible the new list of matcherlists will
						;         be merged to a matcherlist.
						; value:  the matcherlist or nil if any of the above
						;         computations fails.
 
  (let (MATCHERS-LIST RETURNFLAG)
    (COND (BNDEV (SETQ VARIABLES (UNI=TEMP.FIND.VARIABLES VARIABLES))))
    (SETQ MATCHERS-LIST
	  (SMAPCAR
	    #'(LAMBDA (SUBSTLIST)
		(COND (BNDEV (SETQ SUBSTLIST
				   (MAPCAN #'(LAMBDA (SUBST) (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST SUBST))) SUBSTLIST))
			     (COND ((NULL SUBSTLIST) (SETQ RETURNFLAG T))
				   (T (DELETE NIL SUBSTLIST)
				      (when (CAR SUBSTLIST)
					(SETQ SUBSTLIST (UNI=MAKE.MATCHER (UNI=REMOVE.INSTANCES SUBSTLIST) VARIABLES))
					(COND ((NULL SUBSTLIST) (SETQ RETURNFLAG T)))))))
		      (T (SETQ SUBSTLIST (UNI=MAKE.MATCHER SUBSTLIST VARIABLES)) (COND ((NULL SUBSTLIST) (SETQ RETURNFLAG T)))))
		SUBSTLIST)
	    #'(LAMBDA (TAIL) (COND (RETURNFLAG NIL) (T (CDR TAIL)))) (COPY-TREE UNIFIERS-LIST)))
    (COND (RETURNFLAG (SETQ MATCHERS-LIST NIL) nil)
	  (T (let ((RESULT (CAR MATCHERS-LIST)))
	       (MAPC
		 #'(LAMBDA (SUBSTLIST) (SETQ RESULT (UNI=MERGE.MATCHERLISTS (COPY-TREE RESULT) SUBSTLIST T)))
		 (CDR MATCHERS-LIST))
	       RESULT)))))

(DEFMACRO UNI-INSTANCE.IS (UNIFIER1 UNIFIER2)
						; input:  two substitutions
						; value:  t, if unifier1 is an instance of unifier2,
						;         nil,else.
  `(UNI=INSTANCE.IS ,UNIFIER1 ,UNIFIER2))

(DEFMACRO UNI-BINDING.IS.INSTANCE.OF (UNIFIER)
						; edited: 29-nov-83 16:49:22                       hjb
						; Authors: BUERCKERT
						; input:  an arbitrary substitution
						; value:  t, if the domain and codomain of unifier are
						;            equal under a bindingenviroment         -
						;         nil, else
  `(LET ((UNIFIER ,UNIFIER))
     (COND ((NULL UNIFIER) T)
	   (T (UNI=BINDING.IS.INSTANCE.OF UNIFIER)))))

(DEFMACRO UNI-WEAK.INSTANCE (UNIFIER1 UNIFIER2 VARIABLES &OPTIONAL COMPLEMENTFLAG BOOLEAN.RESULTFLAG)
  `(UNI=WEAK.INSTANCE ,UNIFIER1 ,UNIFIER2 ,VARIABLES ,COMPLEMENTFLAG ,BOOLEAN.RESULTFLAG))

(DEFUN UNI=MERGE.SUBSTITUTIONS (UNI1 UNI2 BNDEV)
						; edited:  7-nov-83 10:54:30                       hjb
						; Authors: BUERCKERT
						; input:  uni1, uni2 - unifiers
						;         bndev - unifier with bindings or nil
						; value:  list of unifiers merging uni1 and uni2, if
						;         bndev = nil
						;         list of unifiers merging uni1, uni2 and
						;         bndev, if it is an unifier
						; effect: the bindings of bndev will stay
						; remark: (merge r (merge (s t)) is the same as
						;         (merge r (append s t)) where r,s,t are
						;         unifiers
  (COND (BNDEV (IF (AND (NULL UNI1) (NULL UNI2))
		   (LIST BNDEV)
		   (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
		     BNDEV (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST (DELETE NIL (APPEND UNI1 (COPY-LIST UNI2))))) T)))
	((NULL UNI1) (LIST UNI2))
	((NULL UNI2) (LIST UNI1))
	(T (PROG2 (UNI=SET.BINDINGS UNI1)
		  (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST UNI1 (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNI2)))
		  (UNI=RESET.BINDINGS UNI1)))))

(DEFUN UNI=MERGE.SUBSTITUTIONLISTS (L1 L2 &OPTIONAL BNDEV)
						; edited:  8-nov-83 12:12:46                       hjb
						; Authors: BUERCKERT
						; input: two lists of substitutions
						;         bndev - unifier with bindings
						; value: list of all unifiers obtained by merging each
						;        unifier of l1 with each of l2.
  (PROG (RESULT)
	(COND ((OR (NULL L1) (NULL L2)) (RETURN NIL))
	      (BNDEV
	       (COND
		 ((AND (NOTANY #'(LAMBDA (SUBST) SUBST) L1) (NOTANY #'(LAMBDA (SUBST) SUBST) L2)) (RETURN (LIST BNDEV)))
		 ((NOTANY #'(LAMBDA (SUBST) SUBST) L1)
		  (SETQ RESULT
			(MAPCAN
			  #'(LAMBDA (UNI2)
			      (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST BNDEV
										    (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNI2)) T))
			  L2)))
		 ((NOTANY #'(LAMBDA (SUBST) SUBST) L2)
		  (SETQ RESULT (MAPCAN #'(LAMBDA (UNI1)
					   (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
					     BNDEV (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNI1)) T))
				       L1)))
		 (T (MAPC #'(LAMBDA (UNI1)
			      (MAPC #'(LAMBDA (UNI2)
					(SETQ RESULT (NCONC (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST
									  (NCONC (COPY-TREE UNI1) (COPY-TREE UNI2))))
							    (COPY-TREE RESULT))))
				    L2))
			  L1)
		    (SETQ RESULT (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST BNDEV RESULT)))))
	      ((NOTANY #'(LAMBDA (SUBST) SUBST) L1) (RETURN L2)) ((NOTANY #'(LAMBDA (SUBST) SUBST) L2) (RETURN L1))
	      (T (SETQ RESULT (MAPCAN #'(LAMBDA (UNI1)
					  (UNI=SET.BINDINGS UNI1)
					  (MAPC #'(LAMBDA (UNI2)
						    (SETQ RESULT (NCONC (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST (COPY-TREE UNI2)))
									(COPY-TREE RESULT))))
						L2)
					  (UNI=RESET.BINDINGS UNI1)
					  (PROG1 (SETQ RESULT (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
								(COPY-TREE UNI1) RESULT))
						 (SETQ RESULT NIL)))
				      L1))))
	(COND ((NULL RESULT) (RETURN NIL))
	      (T (DELETE NIL RESULT)
		 (RETURN RESULT)))))

(DEFUN UNI=MERGE.LIST.OF.SUBSTITUTIONLISTS (UNIFIERS-LIST &OPTIONAL BNDEV)
						; edited: 10-oct-83 10:21:00                       hjb
						; Authors: BUERCKERT
						; input:  list of lists of unifiers
						;         bndev - unifier with bindings
						; value:  list of all unifiers merging the inputlist
						;         nil, if they are incompatible
						; effect: if destructiveflag = t, unifiers-list will
						; warning]unifiers-list will be destroyed]]]
  (COND ((OR (NULL UNIFIERS-LIST) (MEMBER NIL UNIFIERS-LIST)) NIL)
	((EVERY #'(LAMBDA (UNIFIERS) (NOTANY (FUNCTION (LAMBDA (SUBST) SUBST)) UNIFIERS)) UNIFIERS-LIST)
						; each unifierlist in unifiers-lists is of the form
						; (nil nil ... nil) or (nil).
	 (LIST BNDEV))
	(T (SETQ UNIFIERS-LIST (REMOVE (LIST NIL) UNIFIERS-LIST))
	   (COND
	     ((MEMBER-IF #'CDR UNIFIERS-LIST)
						; there are unifierlists with more than one unifier
	      (let ((UNIFIERS (CAR UNIFIERS-LIST)))
		(COND (BNDEV (MAPC #'(LAMBDA (UNIFIERS2)
				       (SETQ UNIFIERS
					     (let (U)
					       (MAPC #'(LAMBDA (UNI1)
							 (MAPC #'(LAMBDA (UNI2)
								   (SETQ U (NCONC (COPY-TREE U)
										  (LIST (NCONC (COPY-TREE UNI1)
											       (COPY-TREE UNI2))))))
							       UNIFIERS2))
						     UNIFIERS)
					       U)))
				   (CDR UNIFIERS-LIST))
			     (SETQ UNIFIERS
				   (MAPCAN
				     #'(LAMBDA (UNIFIER)
					 (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
					   BNDEV (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST UNIFIER)) T))
				     UNIFIERS))
			     (DELETE NIL UNIFIERS) (COND ((CAR UNIFIERS) (SETQ UNIFIERS (UNI=REMOVE.INSTANCES UNIFIERS)))))
		      (T (MAPC #'(LAMBDA (UNIFIERS2)
				   (SETQ UNIFIERS (UNI=MERGE.SUBSTITUTIONLISTS UNIFIERS UNIFIERS2))
				   (SETQ UNIFIERS (UNI=REMOVE.INSTANCES UNIFIERS)))
			       (CDR UNIFIERS-LIST))))
		UNIFIERS))
	     (T ;; there are only unifierlists with one unifier
	      (COND (BNDEV
		     (UNI=REMOVE.INSTANCES
		       (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
			 BNDEV (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST (MAPCON (FUNCTION CAAR) (CDR UNIFIERS-LIST)))) T)))
		    (T (UNI=SET.BINDINGS (CAAR UNIFIERS-LIST))
		       (PROG1
			 (UNI=REMOVE.INSTANCES
			   (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
			     (CAAR UNIFIERS-LIST)
			     (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST (MAPCON (FUNCTION CAAR) (CDR UNIFIERS-LIST))))))
			 (UNI=RESET.BINDINGS (CAAR UNIFIERS-LIST))))))))))

(DEFUN UNI=MERGE.MATCHERS (MATCHER1 MATCHER2 RESULTFLAG)
						; edited: 12-oct-83 12:32:21                       hjb
						; Authors: BUERCKERT
						; input: two matchers, domain regarded as ground
						;        resultflag = t or = nil
						; value: resultflag = t: list of unifiers merging the
						;        matchers, if they are compatible; nil, else
						;        resultflag = nil: t, if the matchers are
						;        compatible; nil, else
  (COND ((NULL MATCHER1) (COND (RESULTFLAG (LIST MATCHER2)) (T T))) ((NULL MATCHER2) (COND (RESULTFLAG (LIST MATCHER1)) (T T)))
	(T
	 (PROG (BINDING (RESULT (COND (RESULTFLAG (COPY-TREE MATCHER1)) (T T)))) (UNI=SET.BINDINGS MATCHER1)
	       (SMAPL
		 #'(LAMBDA (TAIL)
		     (COND
		       (RESULT (SETQ BINDING (DT-VARIABLE.GET.BINDING (CAR TAIL)))
			       (COND (BINDING (COND ((NOT (UNI=EQUAL.TERMS BINDING (SECOND TAIL))) (SETQ RESULT NIL))))
				     (T (COND (RESULTFLAG (SETQ RESULT (CONS (CAR TAIL) (CONS (SECOND TAIL) RESULT))))))))))
		 #'CDDR MATCHER2)
	       (UNI=RESET.BINDINGS MATCHER1) (COND ((AND RESULT RESULTFLAG) (RETURN (LIST RESULT))) (T (RETURN RESULT)))))))

(DEFUN UNI=MERGE.MATCHERLISTS (MATCHERLIST1 MATCHERLIST2 RESULTFLAG)
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; input:  matcherlist1,matcherlist2 - two lists of
						;         matchers ,i.e. two lists of
						;         substitutions where the codomain is
						;         regarded as ground
						;         resultflag - t or nil
						; effect: returns value
						; value:  resultflag = t: list of all substitutions
						;             obtained by merging matcherlist1 and
						;             matcherlist2; nil if they are
						;             incompatible
						;         resultflag = nil: t if matcherlist1 and
						;             matcherlist2 are compatible else nil
  (COND ((OR (NULL MATCHERLIST1) (NULL MATCHERLIST2)) NIL)
	(T
	 (PROG (RESULT MERGE FLAG)
	       (MAPC
		 #'(LAMBDA (MATCHER1)
		     (COND
		       ((NOT FLAG)
			(MAPC
			  #'(LAMBDA (MATCHER2)
			      (COND
				((NOT FLAG) (SETQ MERGE (UNI=MERGE.MATCHERS MATCHER1 MATCHER2 RESULTFLAG))
				 (COND (RESULTFLAG (SETQ RESULT (NCONC MERGE RESULT)))
				       (T (COND (MERGE (SETQ RESULT T) (SETQ FLAG T))))))))
			  MATCHERLIST2))))
		 MATCHERLIST1)
	       (RETURN RESULT)))))

(DEFUN UNI=INSTANCE.IS (UNI1 UNI2)
						; edited: 29-nov-83 16:49:22                       hjb
						; Authors: BUERCKERT
						; input:  two arbitrary substitutions
						; value:  t, if there is a substitution s with
						;            uni1 = s*uni2
						;         nil, else
  (COND ((NULL (CAR UNI2)) T)
	((NULL (CAR UNI1)) NIL)
	((EQL UNI1 UNI2) T)
	(T (PROG2 (UNI=SET.BINDINGS UNI1)
		  (UNI=BINDING.IS.INSTANCE.OF UNI2)
		  (UNI=RESET.BINDINGS UNI1)))))

(DEFUN UNI=BINDING.IS.INSTANCE.OF (UNIFIER)
						; edited: 29-nov-83 16:49:22                       hjb
						; Authors: BUERCKERT
						; input:  an arbitrary substitution
						; value:  t, if the domain and codomain of unifier are
						;            equal under a bindingenviroment         -
						;         nil, else
  (PROG (RESULT)
	(SMAPL
	  #'(LAMBDA (TAIL)
	      (COND
		((UNI=EQUAL.TERMS (UNI=GET.BINDING (CAR TAIL)) (UNI=INSERT.BINDINGS.IN (SECOND TAIL)))
		 (SETQ RESULT T))
		(T (SETQ RESULT NIL))))
	  #'(LAMBDA (TAIL) (COND (RESULT (CDDR TAIL))))
	  UNIFIER)
	(RETURN RESULT)))

(DEFUN UNI=WEAK.INSTANCE (U1 U2 VARIABLES &OPTIONAL COMPLEMENTFLAG BOOLEAN.RESULTFLAG)
						; edited:  8-nov-83 10:57:45                       hjb
						; Authors: BUERCKERT
						; input:  u1, u2 - unifiers
						;         variables - list of variables
						;         complementflag
						; value:  list of all substitutions s with
						;         u1(x) = s*u2(x) for all x in variables, if
						;         comlementflag = nil, else for all x not in
						;         variables
  (when COMPLEMENTFLAG
    (SETQ VARIABLES
	  (SET-DIFFERENCE (UNION (UNI=TEMP.FIND.VARIABLES U1) (UNI=TEMP.FIND.VARIABLES U2)) VARIABLES)))
  (let (TL1 TL2 VARIABLE.BUFFER)
    (MAPC #'(LAMBDA (X)
	      (push (UNI=APPLY.SUBSTITUTION U1 X T) TL1)
	      (push (UNI=APPLY.SUBSTITUTION U2 X T) TL2))
	  VARIABLES)
    (SETQ VARIABLE.BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK)) (UNI=FIND.VARIABLES TL1 VARIABLE.BUFFER)
    (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS (BUFFER.CONTENTS VARIABLE.BUFFER))
    (PROG1 (UNI=MATCH.WITH.BUFFER TL2 TL1 BOOLEAN.RESULTFLAG)
	   (PUSH.BUFFER.STACK VARIABLE.BUFFER UNI*BUFFER.STACK) (SETQ UNI*VARIABLES.REGARDED.AS.CONSTANTS NIL))))

(DEFUN UNI=POLY.VARIABLE.TERM.R (RANGESORT TERM SORT.OF.VARIABLES)
						; input: a term; a sort which has to be greater than
						;        term-sort after weakening. and an assoclist
						;        of the already weakened variables.
						; value: the assoclist of all maximal weaking subst
						;        of term, which make sort of term less or eq
						;        to the input.sort. the substitutions are in
						;        the form   .. var1 sort1 var2 sort2 ...
  (PROG
    (MAX.SUBSORTS MAX.TUPLE.LIST MATCH SUBTERM.SORTS MATCH.POINTER SUBTERM.SORTS.POINTER WEAK.TERM.LIST
     WEAK.POS.LIST AKT.POS CHANGE.FLAG)		; max.subsorts: all greatest common subsorts of
						;   term and input-sort
    (SETQ MAX.SUBSORTS
	  (DT-SORT.GREATEST.COMMON.SUBSORTS RANGESORT (UNI=POLY.TERM.SORT TERM SORT.OF.VARIABLES)))
    (COND ((NULL MAX.SUBSORTS) (RETURN NIL)))	; max.tuple.list: a list of all maximal.tupels of
						; domainsorts of f, for which the corresponding
						; rangesort is in max.subsorts
    (SETQ MAX.TUPLE.LIST
	  (MAPCAN
	    #'(LAMBDA (MAX.SORT) (COPY-TREE (DT-FUNCTION.MAX.DOMAINS.OF.RANGE (CAR TERM) MAX.SORT))) MAX.SUBSORTS))
    (COND ((NULL MAX.TUPLE.LIST) (RETURN NIL)))	; match (i) = nil,if for term = f (t1 t2 ...)
						; the sort of t(i) cannot be changed by weakening
						; substitutions. = t, if this may be possible
    (SETQ MATCH
	  (MAPCAR
	    #'(LAMBDA (SUBTERM)
		(COND
		  ((OR
		     (AND (UNI=VARIABLE.IS SUBTERM)
			  (DT-SORT.DIRECT.SUBSORTS (UNI=POLY.TERM.SORT SUBTERM SORT.OF.VARIABLES)))
		     (AND (CONSP SUBTERM) (DT-FUNCTION.MIN.RANGES (CAR SUBTERM))
			  (NOT
			    (MEMBER (UNI=POLY.TERM.SORT SUBTERM SORT.OF.VARIABLES) (DT-FUNCTION.MIN.RANGES (CAR SUBTERM))))))
		   T)
		  (T NIL)))
	    (CDR TERM)))			; subterm.sorts (i) = sort of t(i)
    (SETQ SUBTERM.SORTS
	  (MAPCAR #'(LAMBDA (SUBTERM)
		      (UNI=POLY.TERM.SORT SUBTERM SORT.OF.VARIABLES)) (CDR TERM)))	; max.tupels are removed
						; from max.tuple.list, if
						; there exists no substitution, which makes the
						; sort-tupel of (t1 t2 ..) less than this max.tuple
    (SETQ MAX.TUPLE.LIST
	  (DREMAP MAX.TUPLE.LIST NIL
		  #'(LAMBDA (TAIL) (SETQ MATCH.POINTER MATCH) (SETQ SUBTERM.SORTS.POINTER SUBTERM.SORTS)
			    (SOMEL
			      #'(LAMBDA (MAX.DOMAIN.SORT.TAIL)
				  (PROG1
				    (COND
				      ((CAR MATCH.POINTER)
				       (COND
					 ((NULL
					    (DT-SORT.GREATEST.COMMON.SUBSORTS (CAR MAX.DOMAIN.SORT.TAIL)
									      (CAR SUBTERM.SORTS.POINTER))))))
				      (T
				       (COND
					 ((NOT (DT-SORT.IS.SUBSORT (CAR SUBTERM.SORTS.POINTER) (CAR MAX.DOMAIN.SORT.TAIL))))
					 (T (RPLACA MAX.DOMAIN.SORT.TAIL (CAR SUBTERM.SORTS.POINTER)) (SETQ CHANGE.FLAG T) NIL))))
				    (SETQ MATCH.POINTER (CDR MATCH.POINTER))
				    (SETQ SUBTERM.SORTS.POINTER (CDR SUBTERM.SORTS.POINTER))))
			      (CAR TAIL)))))
    (COND ((NULL MAX.TUPLE.LIST) (RETURN NIL)))	; every max.tuple in tuple.list will be changed at
						; place i to subterm.sorts (i), if match (i) = nil
						; max.tupels are removed from max.tuple.list, if they
						; are not maximal or equal to an other tuple.
    (COND (CHANGE.FLAG (MAXIMA MAX.TUPLE.LIST (FUNCTION DT-FUNCTION.TUPLE.LESS)))) (SETQ MATCH.POINTER MATCH) (SETQ AKT.POS 1)
    (MAPC
      #'(LAMBDA (SUBTERM)
	  (COND
	    ((CAR MATCH.POINTER) (SETQ WEAK.POS.LIST (CONS AKT.POS WEAK.POS.LIST))
	     (SETQ WEAK.TERM.LIST (CONS SUBTERM WEAK.TERM.LIST))))
	  (SETQ MATCH.POINTER (CDR MATCH.POINTER)) (SETQ AKT.POS (1+ AKT.POS)))
      (CDR TERM))
    (RETURN (UNI=POLY.WEAKENING MAX.TUPLE.LIST WEAK.TERM.LIST WEAK.POS.LIST SORT.OF.VARIABLES TERM RANGESORT))))

(DEFUN UNI=POLY.TERM.SORT (TERM SORT.OF.VARIABLES)
						; input: a term and a assoc.list of the variables
						;          already  weakened and the sorts of them.
						; value: the sort of this term with respect to the
						;        weakening substitution.
  (CASE (DT-TYPE TERM) (CONSTANT (DT-CONSTANT.SORT TERM))
	(VARIABLE
	  (OR
	    (SECOND (SSOME #'(LAMBDA (VARIABLE) (EQL TERM VARIABLE)) #'CDDR SORT.OF.VARIABLES)) (UNI=VARIABLE.SORT TERM)))
	(ABBREVIATION (DT-ABBREVIATION.SORT TERM))
	(OTHERWISE
	  (COND
	    ((DT-FUNCTION.IS.POLYMORPHIC (CAR TERM))
	     (DT-FUNCTION.SORT (CAR TERM)
			       (MAPCAR #'(LAMBDA (SUBTERM) (UNI=POLY.TERM.SORT SUBTERM SORT.OF.VARIABLES)) (CDR TERM))))
	    (T (DT-FUNCTION.MAX.RANGE.SORT (CAR TERM)))))))

(DEFUN UNI=POLY.WEAKENING (TUPLE.LIST TERM.LIST TERM.POSITIONS SORT.OF.VARIABLES TERM RANGE.SORT)
						; input: tuple.list: some maximal domain-range.tuples
						;        term.list: the arguments of the function.
						;        term.positions: positions of the above terms
						;        sort.of.variables: a weakening substitution
						;        term: the term to be weakened
						;        range.sort: the sort the term should have
						;        after the weakening
						; value: the list of all weakening substitutions
						;          in the internal representation.
						;          nil if there is none
  (PROG
    (RET.SUBST
     (AKT.SUBST
       (MAPCAR #'(LAMBDA (TUPLE) (CONS TUPLE (LIST (COPY-TREE SORT.OF.VARIABLES)))) TUPLE.LIST))
     AKT.TERM MAX.TUPLE.SORT AKT.TERM.SORT AKT.TERM.POS AKT.TERM.MAX.SUBSORTS MAX.TUPLE.SUBST.NEW SUBST.NEW
     TUPLE.SUBST.PREL)
    ;; akt.subst is a list of the form:
    ;;  (...  ((s1 s2 ...) (x1 t1 ...) ... )    ...)
    ;;  (s1 s2 ...) is a maximal tuple, the next elements
    ;; are the substitutions, which are to be changed into
    ;; right weakening substitutions
    ;; while works until all preliminary substitutions are
    ;; changed. this right weak. subst. are all in
    ;; ret.subst, which contains at every time only
    ;; maximal substitutions relativ to ret.subst.
    (WHILE AKT.SUBST (SETQ AKT.TERM (CAR TERM.LIST)) (SETQ AKT.TERM.POS (CAR TERM.POSITIONS))
	   (SETQ TERM.LIST (CDR TERM.LIST)) (SETQ TERM.POSITIONS (CDR TERM.POSITIONS))
	   ;; this is a loop first over all max.tupels, then
	   ;; over the corresponding substitutions.
	   (SETQ AKT.SUBST
		 (MAPCAR
		   #'(LAMBDA (MAX.TUPLE.SUBST)
		       ;; all substitutions are computed.
		       (SETQ MAX.TUPLE.SUBST.NEW
			     (CONS (CAR MAX.TUPLE.SUBST)
				   (MAPCAN
				     #'(LAMBDA (SUBST)
					 (PROG NIL
					       ;; for the substitution subst,the tuple max.tuple.subst
					       ;; and the akt.term all substitutions are generated,
					       ;; which fullfill the sort-less condition
					       ;; possibly in recursive calls. the new substitution
					       ;; are inserted in akt.subst.
					       (SETQ MAX.TUPLE.SORT (CAR (NTHCDR (1- AKT.TERM.POS) (CAR MAX.TUPLE.SUBST))))
					       (COND
						 ((DT-SORT.IS.SUBSORT (SETQ AKT.TERM.SORT (UNI=POLY.TERM.SORT AKT.TERM SUBST))
								      MAX.TUPLE.SORT)
						  (RETURN (LIST SUBST))))
					       (COND
						 ((NULL
						    (SETQ AKT.TERM.MAX.SUBSORTS
							  (DT-SORT.GREATEST.COMMON.SUBSORTS AKT.TERM.SORT MAX.TUPLE.SORT)))
						  (RETURN NIL)))
					       (COND
						 ((AND (NOT (UNI=VARIABLE.IS AKT.TERM))
						       (NOT (DT-SORT.IS.SUBSORT AKT.TERM.SORT MAX.TUPLE.SORT))
						       (MEMBER AKT.TERM.SORT (DT-FUNCTION.MIN.RANGES (CAR AKT.TERM))))
						  (RETURN NIL)))
					       (COND
						 ((UNI=VARIABLE.IS AKT.TERM)
						  ;; if akt.term is a variable, the new substitutions
						  ;; can be constructed immediately.
						  (RETURN
						    (MAPCAR
						      #'(LAMBDA (MAX.SUBSORT) (SETQ SUBST.NEW (COPY-TREE SUBST))
								(COND
								  ((MEMBER AKT.TERM SUBST.NEW)
								   (RPLACA (CDR SUBST.NEW) MAX.SUBSORT))
								  (T (SETQ SUBST.NEW (CONS AKT.TERM (CONS MAX.SUBSORT SUBST.NEW)))))
								SUBST.NEW)
						      AKT.TERM.MAX.SUBSORTS)))
						 (T	;; if akt.term is a list, the new substitutions have
						  ;; to be constructed by a recursive call to
						  ;; uni=poly.variable.term.r
						  (RETURN (UNI=POLY.VARIABLE.TERM.R MAX.TUPLE.SORT AKT.TERM SUBST))))))
				     (CDR MAX.TUPLE.SUBST))))
		       ;; subtitutions, which are not maximal, are deleted.
		       (COND
			 ((CDDR MAX.TUPLE.SUBST.NEW)
			  (SETQ MAX.TUPLE.SUBST.NEW
				(RPLACD MAX.TUPLE.SUBST.NEW
					(MAXIMA (CDR MAX.TUPLE.SUBST.NEW) (FUNCTION UNI=POLY.SUBST.LESS)))))
			 (T MAX.TUPLE.SUBST.NEW)))
		   AKT.SUBST))
	   ;; the substitutions are now checked, if all
	   ;; conditions are satisfied. right substitutions
	   ;; are moved from akt.subst to ret.subst
	   (SETQ AKT.SUBST
		 (MAPCAR
		   #'(LAMBDA (AKT.TUPLE.SUBST)
		       (SETQ TUPLE.SUBST.PREL
			     (CONS (CAR AKT.TUPLE.SUBST)
				   (MAPCAN
				     #'(LAMBDA (SUBST)
					 (LIST
					   (COND
					     ((DT-SORT.IS.SUBSORT (UNI=POLY.TERM.SORT TERM SUBST) RANGE.SORT)
					      (PROG1 NIL
						     (COND
						       ((MEMBER-IF
							  #'(LAMBDA (RET.SUBST.EL) (UNI=POLY.SUBST.LESS SUBST RET.SUBST.EL))
							  RET.SUBST)
							NIL)
						       (T (SETQ RET.SUBST (CONS SUBST RET.SUBST))))))
					     (T SUBST))))
				     (CDR AKT.TUPLE.SUBST))))
		       (COND
			 ((AND (NOT (MEMBER NIL AKT.TUPLE.SUBST)) (MEMBER NIL TUPLE.SUBST.PREL)) (DELETE NIL TUPLE.SUBST.PREL))
			 (T TUPLE.SUBST.PREL)))
		   AKT.SUBST))
	   (MAXIMA RET.SUBST (FUNCTION UNI=POLY.SUBST.LESS))
	   (SETQ AKT.SUBST (DREMAP AKT.SUBST NIL #'(LAMBDA (AKT.SUBST.TAIL) (NULL (CDAR AKT.SUBST.TAIL))))))
    (RETURN RET.SUBST)))

(DEFUN UNI=POLY.SUBST.LESS (SUB.SUBST SUPER.SUBST)
						; input: two substitutions in internal representation
						; value: t , if sub.subst is less than super.subst.
						;        nil else.
						;        less than with respect to the induced order
						;        on weakening substitutions by the sort-order
  (PROG (SUBST.POINTER) (COND ((< (LIST-LENGTH SUPER.SUBST) (LIST-LENGTH SUB.SUBST)) (RETURN NIL)))
	(SMAPL
	  #'(LAMBDA (SUPER.TAIL)
	      (COND
		((SETQ SUBST.POINTER (MEMBER (CAR SUPER.TAIL) SUB.SUBST))
		 (COND
		   ((NOT (DT-SORT.IS.SUBSORT (SECOND SUBST.POINTER) (SECOND SUPER.TAIL)))
		    (RETURN-FROM UNI=POLY.SUBST.LESS NIL))))
		(T (RETURN-FROM UNI=POLY.SUBST.LESS NIL))))
	  #'CDDR SUPER.SUBST)
	(RETURN T)))

(defun uni=variable.sort (variable)
  (if (opt-get.option sort_literals)
      'any
      (DT-VARIABLE.SORT VARIABLE)))

(DEFUN UNI=POLY.X.WITH.T (VARIABLE TERM)
						; input: a variable and a term  to unify
						; condition: sort of term greater than sort of var.
						; value: all maximal weakening substitutions, which
						;        make the sorts compatible.
  (COND ((DT-SORT.IS.SUBSORT (DT-TERM.SORT TERM) (uni=VARIABLE.SORT VARIABLE)) (LIST (LIST VARIABLE TERM)))
	((NOT (DT-FUNCTION.IS.POLYMORPHIC (CAR TERM))) NIL)
	(T
	 (MAPCAR
	   #'(LAMBDA (SUBST)
	       (SMAPL
		 #'(LAMBDA (PAIR.TAIL) (RPLACA (CDR PAIR.TAIL) (DT-VARIABLE.CREATE (SECOND PAIR.TAIL))))
		 #'CDDR SUBST)
	       (CONS VARIABLE (CONS (UNI=APPLY.SUBSTITUTION SUBST (COPY-TREE TERM)) SUBST)))
	   (UNI=POLY.VARIABLE.TERM.R (UNI=VARIABLE.SORT VARIABLE) TERM NIL)))))

(DEFUN UNI=POLY.SORT.WITH.T (SORT TERM)
						; input: a sort and a term
						; condition: sort of term greater than sort.
						; value: all maximal weakening substitutions, which
						;        make the sort of term less than or eq sort
  (COND ((DT-SORT.IS.SUBSORT (DT-TERM.SORT TERM) SORT) (LIST NIL)) ((NOT (DT-FUNCTION.IS.POLYMORPHIC (CAR TERM))) NIL)
	(T
	 (MAPCAR
	   #'(LAMBDA (SUBST)
	       (SMAPL
		 #'(LAMBDA (PAIR.TAIL) (RPLACA (CDR PAIR.TAIL) (DT-VARIABLE.CREATE (SECOND PAIR.TAIL)))) #'CDDR SUBST)
	       SUBST)
	   (UNI=POLY.VARIABLE.TERM.R SORT TERM NIL)))))

(defun uni-apply.substitution.to.litlist (subst litlist)
						; Edited:  19-AUG-1990 14:21
						; Authors: PRCKLN
						; Input:   A substitution and a list of literals.
						; Effect:  -
						; Value:   SUBST(LITLIST)
  (mapcar #'(lambda (lit)
	      (ds-lit.create (ds-lit.sign lit)
			     (ds-lit.predicate lit)
			     (mapcar #'(lambda (term) (uni-apply.substitution subst term t))
				     (ds-lit.termlist lit))))
	  litlist))


(DEFun UNI-APPLY.SUBSTITUTION (SUBSTITUTION TERM &OPTIONAL COPYFLAG)
						; edited: 13-oct-83 15:02:26                       hjb
						; Authors: BUERCKERT
						; see uni=apply.substitution
  (UNI=APPLY.SUBSTITUTION SUBSTITUTION TERM COPYFLAG))

(DEFMACRO UNI-UNION.OF.SUBSTITUTIONLISTS (UNIFIERS1 UNIFIERS2)
						; edited: 13-oct-83 14:47:52                       hjb
						; Authors: BUERCKERT
						; see uni=union.of.substitutionlists
  `(UNI=UNION.OF.SUBSTITUTIONLISTS ,UNIFIERS1 ,UNIFIERS2))

(defun uni-literal.sort.unifier (res)
						; Edited:  19-NOV-1991 22:05
						; Authors: PRCKLN
						; Input:   RES is an object (:RESIDUE UNIFIER . RESIDUE)
						; Effect:  -
						; Value:   The UNIFIER component.
  (second res))

(defun uni-literal.sort.residue (res)
						; Edited:  19-NOV-1991 22:05
						; Authors: PRCKLN
						; Input:   RES is an object (:RESIDUE UNIFIER . RESIDUE)
						; Effect:  -
						; Value:   The RESIDUE component.
  (rest (rest res)))

(defun uni-literal.sort.is (res)
						; Edited:  19-NOV-1991 22:05
						; Authors: PRCKLN
						; Input:   RES is an object (:RESIDUE UNIFIER . RESIDUE)
						;          or a unifier.
						; Effect:  -
						; Value:   True iff RES is a :RESIDUE object.
  (eql (first res) :residue))

(defun uni-fit.on.literal.sort (unifiers)
						; Edited:  19-NOV-1991 21:43
						; Authors: PRCKLN
						; Input:   A list of unifiers.
						; Effect:  -
						; Value:   A list of dotted pairs (UNIFIER . RESIDUE) if
						;          SORT_LITERALS is switched on, else UNIFIERS.
  (if (opt-get.option sort_literals)
      (mapcan #'(lambda (unifier)
		  (mapcar #'(lambda (subst.resid)
			      (if (rest subst.resid)
				  (cons :residue subst.resid)
				  (first subst.resid)))
			  (upr-unify.substitution unifier)))
	      unifiers)
      unifiers))

(DEFUN UNI-FIT.ON.SORT (UNIFIERS SORT TERM)
						; edited at 21-feb-83 18:05)
						; input:  a unifier, a sort and a term
						; effect: returns value
						; value:  a modified unifier with respect to the
						;         sort
  (LET (SUBST.TERM SUBST.SORT)
    (COND
      ((NOT (UNI=VARIABLE.IS TERM))
       (COND ((DT-SORT.IS.SUBSORT (DT-TERM.SORT TERM) SORT) UNIFIERS)
	     ((AND (CONSP TERM) (DT-FUNCTION.IS.POLYMORPHIC (CAR TERM)))
	      (MAPCAN #'(LAMBDA (UNIFIER)
			  (SETQ SUBST.TERM (UNI=APPLY.SUBSTITUTION UNIFIER TERM T))
			  (COND ((DT-SORT.IS.SUBSORT (DT-TERM.SORT SUBST.TERM) SORT) (LIST UNIFIER))
				(T (UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST
				     UNIFIER (UNI=POLY.SORT.WITH.T SORT SUBST.TERM)))))
		      UNIFIERS))
	     (T NIL)))
      (T (MAPCAN #'(LAMBDA (SUBST)
		     (SETQ SUBST.TERM
			   (SECOND (DO ((RESTUNI SUBST (CDDR RESTUNI)))
				       ((OR (EQL (CAR RESTUNI) TERM) (NULL RESTUNI)) RESTUNI))))
		     (COND ((NOT SUBST.TERM) (SETQ SUBST.TERM TERM))) (SETQ SUBST.SORT (DT-TERM.SORT SUBST.TERM))
		     (COND ((DT-SORT.IS.SUBSORT SUBST.SORT SORT) (CONS SUBST NIL))	; nil zusaetzlich eingefuegt (christoph)
			   ((UNI=VARIABLE.IS SUBST.TERM)
			    (MAPCAR
			      #'(LAMBDA (SORTE)
				  (UNI=COMPOSITION.OF.SUBSTITUTIONS SUBST (LIST SUBST.TERM (DT-VARIABLE.CREATE SORTE))))
			      (DT-SORT.GREATEST.COMMON.SUBSORTS SUBST.SORT SORT)))))
		 UNIFIERS)))))

(DEFUN UNI-UNIFIER.IS.VARIABLE.RENAMING (SUBSTITUTION)
						; edited: 10-feb-81 16:11:09
						; Authors: BUERCKERT
						; input:  an arbitrary substitution s
						; value:  t , if s is a renaming substitution
						;         else nil
  (PROG ((RIGHTHANDSIDES (SMAPCAR #'IDENTITY #'CDDR (CDR SUBSTITUTION))))
	(RETURN
	  (AND (EVERY #'(LAMBDA (VAR) (UNI=VARIABLE.IS VAR)) RIGHTHANDSIDES)
	       (EQUAL RIGHTHANDSIDES (INTERSECTION RIGHTHANDSIDES RIGHTHANDSIDES))
	       (SEVERYL #'(LAMBDA (TAIL) (EQUAL (DT-TERM.SORT (CAR TAIL)) (DT-TERM.SORT (SECOND TAIL))))
			#'CDDR
			SUBSTITUTION)))))

(DEFMACRO UNI-SWITCH (REN OBJ)
						; edited: 12.4.84
						; Authors: BUERCKERT
						; input:  a renaming substitution and a termlist
						; value:  termlist with switched variables
						; note:   see uni=switch for an explanation
  `(UNI=SWITCH ,REN ,OBJ))

(DEFMACRO UNI-RENAME (UNIFIERS VARIABLES)
						; edited: 30-sep-83 15:00:33
						; Authors: BUERCKERT
						; input:  an s-expression and a list of variables
						; effect: all variables in unifiers listed in
						;         variables are destructively replaced
						;         by new variables
						; value:  the renamed unifiers
  `(UNI=RENAME ,UNIFIERS ,VARIABLES))

(DEFMACRO UNI-REMOVE.INSTANCES (UNIFIERLIST)
						; edited: 13-oct-83 12:56:15                       hjb
						; Authors: BUERCKERT
						; see uni=remove.instances
  `(UNI=REMOVE.INSTANCES ,UNIFIERLIST))

(DEFMACRO UNI-REMOVE.COMPONENTS (UNIFIERS VARIABLES)
						; edited: 14-oct-83 10:35:35                       hjb
						; Authors: BUERCKERT
						; see uni=remove.components
  `(UNI=REMOVE.COMPONENTS ,UNIFIERS ,VARIABLES))

(DEFUN UNI-CONSTANTIFY (EXPRESSION)
						; edited: 4. 8. 1984
						; Authors: BUERCKERT
						; input:  an arbitrary term or termlist
						;         (with bindings)
						; effect: the dt-variables occurring in expression and
						;         their bindings are declared as constants
						; value:  these variables (i.e. the buffer contents of
						;         uni*dt.variable.buffer. so, don't work des-
						;         structively on this list or this buffer]])
  (BUFFER.RESET UNI*CONSTANTIFY.BUFFER)
  (SETQ UNI*VARIABLES.DECLARED.AS.CONSTANTS
	(BUFFER.CONTENTS (DT-FIND.VARIABLES EXPRESSION UNI*CONSTANTIFY.BUFFER))))

(DEFMACRO UNI-DECLARE.VARIABLES.AS.CONSTANTS (VARIABLES)
						; edited at 21-feb-83 18:08)
						; input:  a list of variables
						; effect: the varaibles are declared as constants
						; value:  variables
  `(SETQ UNI*VARIABLES.DECLARED.AS.CONSTANTS ,VARIABLES))

(DEFMACRO UNI-CLEAR.VARIABLES.AS.CONSTANTS NIL
						; edited at 22-feb-83 10:11)
						; input:  nil
						; effect: the declaration of variables as
						;         constants is reset
						; value:  nil
  `(SETQ UNI*VARIABLES.DECLARED.AS.CONSTANTS NIL))

(DEFUN UNI-UNIFIER.DOMAIN (SUBST &OPTIONAL BINDFLAG)
						; edited: 12-jan-84 16:55:39                       hjb
						; Authors: BUERCKERT
						; input:  subst - substitution
						;         bindflag = t or nil
						; value:  if bindflag = nil, domain of subst.
						;         if bindflag = t, list of domains of the
						;         list of substituions obtained by unifiing
						;         subst under the bindingenviroment, if this
						;         list has more than one domain. the domain
						;         itself, if it is only one.
  (IF BINDFLAG
      (LET ((RESULT (MAPCAR #'(LAMBDA (UNI) (UNI=UNIFIER.DOMAIN UNI))
			    (UNI=UNBIND (UNI=UNIFY.MIXED.TERMLIST SUBST)))))
	(IF (CDR RESULT) RESULT (CAR RESULT)))
      (UNI=UNIFIER.DOMAIN SUBST)))

(DEFMACRO UNI-UNIFIER.CODOMAIN (SUBST)
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; input  :subst - substitution
						; effect :returns value
						; value  :the codomain of the substitution
						;         as a list,for example if subst =
						;         (x a y a z b) then (a a b) is
						;         returned
  `(SMAPCAR #'(LAMBDA (TERM) TERM) #'CDDR (CDR ,SUBST)))

(DEFMACRO UNI-SET.BINDINGS.OF.SUBSTITUTION (UNIFIER)
						; edited: 14-oct-83 11:02:41                       hjb
						; Authors: BUERCKERT
						; input:  an unifier
						; value:  nil
						; effect: pushs the terms of cod(unifier) into the
						;         bindingstag of the variables of its domain
						; warning]you should reset this bindings, if you don't
						;         use the unifier repeatedly without any other
						;         unificationstep]]
  `(PROGN (UNI=SET.BINDINGS ,UNIFIER) (SETQ UNI*BINDINGFLAG T)))

(DEFMACRO UNI-RESET.BINDINGS.OF.SUBSTITUTION (UNIFIER &OPTIONAL REMAINING.BINDINGS)
						; edited: 14-oct-83 11:14:54
						; Authors: BUERCKERT
						; input:  an unifier with bindings  and a flag.
						; value:  nil
						; effect: deletes all bindings of unifier
						;         if remaining.bindings = t, the bindingflag
						;         remaines switched on.
  `(PROGN (UNI=RESET.BINDINGS ,UNIFIER)
	  (COND (,REMAINING.BINDINGS) (T (SETQ UNI*BINDINGFLAG NIL)))))

(DEFUN UNI=UNION.OF.SUBSTITUTIONLISTS (UNIFIERS1 UNIFIERS2)
						; edited at 22-feb-83 10:16)
						; input: unifiers1,unifiers2 - list of
						;        substitutions
						; effect:returns value
						; value: union of the two substitutionlists
						;        where no element is an instance of
						;        another
  (PROG (UNIFIERS)
	(RETURN
	  (NCONC
	    (SETQ UNIFIERS
		  (REMOVE-IF-NOT
		    #'(LAMBDA (UNI1) (EVERY #'(LAMBDA (UNI2) (NOT (UNI=INSTANCE.IS UNI1 UNI2))) UNIFIERS2))
		    UNIFIERS1))
	    (REMOVE-IF-NOT
	      #'(LAMBDA (UNI2) (EVERY #'(LAMBDA (UNI1) (NOT (UNI=INSTANCE.IS UNI2 UNI1))) UNIFIERS))
	      UNIFIERS2)))))


(DEFUN UNI=SWITCH (RENAMING OBJECT)
						; edited: 12. 2. 1984
						; Authors: BUERCKERT
						; input:  a renaming substitution and a termlist
						; effect: all variables in object are switched
						;         according to renaming.
						; example: renaming = (x y)
						;          object = (y (f x x z))
						;          ==>      (x (f y y z))
						; value:   the destructively changed object.
  (when OBJECT
    (COND ((CONSP OBJECT) (MAPL #'(LAMBDA (TAIL) (RPLACA TAIL (UNI=SWITCH RENAMING (CAR TAIL)))) OBJECT) OBJECT)
	  (T (SSOMEL #'(LAMBDA (TAIL)
			 (COND ((EQL OBJECT (CAR TAIL)) (SETQ OBJECT (SECOND TAIL)))
			       ((EQL OBJECT (SECOND TAIL)) (SETQ OBJECT (CAR TAIL)))))
		     #'CDDR RENAMING)
	     OBJECT))))

(DEFUN UNI=RENAME (UNIFIERS VARIABLES)
						; edited: 30-sep-83 15:00:33
						; Authors: BUERCKERT
						; input:  an s-expression and a list of variables
						; effect: all variables in unifiers listed in
						;         variables are destructively replaced
						;         by new variables
						; value:  the renamed unifiers
  (MAPC
    #'(LAMBDA (VARIABLE) (NSUBST (DT-VARIABLE.CREATE (dt-VARIABLE.SORT VARIABLE)) VARIABLE UNIFIERS)) VARIABLES)
  UNIFIERS)

(DEFMACRO UNI=REMOVE.INSTANCES (UNIFIERS)
						; edited: 30-sep-83 15:08:43
						; Authors: BUERCKERT
						; input:  a list of unifiers
						; effect: all instances of other unifiers are removed
						; value:  the 'cleaned' list of unifiers
  `(MAXIMA ,UNIFIERS (FUNCTION UNI=INSTANCE.IS)))



(DEFUN UNI=REMOVE.COMPONENTS (UNIFIERS VARIABLES)
						; edited: 30-sep-83 14:00:52
						; Authors: BUERCKERT
						; input:  a list of unifiers and a list of variables
						; effect: the substitution components of variables
						;         are removed from unifiers
						; value:  the shortened unifiers.
  (COND
    (VARIABLES
     (MAPL
       #'(LAMBDA (UNIFIERS.TAIL)
	   (SMAPL
	     #'(LAMBDA (UNIFIER.TAIL)
		 (COND
		   ((MEMBER (CAR UNIFIER.TAIL) VARIABLES) (RPLACA UNIFIER.TAIL NIL) (RPLACA (CDR UNIFIER.TAIL) NIL))))
	     #'CDDR (CAR UNIFIERS.TAIL))
	   (RPLACA UNIFIERS.TAIL (DELETE NIL (CAR UNIFIERS.TAIL))))
       UNIFIERS)
     (UNI=REMOVE.INSTANCES UNIFIERS))
    (T UNIFIERS)))

(DEFMACRO UNI=UNIFIER.DOMAIN (SUBSTITUTION)
						; edited: 12-jan-84 16:49:56                       hjb
						; Authors: BUERCKERT
						; input:  a substitution
						; value:  the domain of substitution
  `(SMAPCAR #'(LAMBDA (VARS) VARS) #'CDDR ,SUBSTITUTION))

(DEFUN UNI=APPLY.SUBSTITUTION (SUBSTITUTION TERM &OPTIONAL COPYFLAG)
						; edited:  8-dec-83 18:10:59                       hjb
						; Authors: BUERCKERT
						; input:  substitution - an arbitrary substitution
						;         term - a term or a substitution
						;         copyflag - a flag
						; effect: if copyflag = t, substitution is applied to
						;         a copy of term, else term is destroyed,
						;         The codomain terms of SUBSTITUTION will be copied too.
						; value:  nil, if tern = nil, else substitution(term).
  #|(when (opt-get.option sort_literals)
    (smapc #'(lambda (term)
	       (|#
  (COND (TERM (WHEN COPYFLAG (SETQ TERM (COPY-TREE TERM)))
	      (SMAPL #'(LAMBDA (SUBSTITUTES)
			 (let ((dom (CAR SUBSTITUTES))
			       (cod (SECOND SUBSTITUTES)))
			   (IF (AND (ATOM TERM) (EQL TERM dom))
			       (SETQ TERM (IF COPYFLAG
					      (COPY-TREE cod)
					      cod))
			       (nsubst (IF COPYFLAG
					   (COPY-TREE cod)
					   cod) dom term)#|
			       (uni=substitute dom
					       (IF COPYFLAG
						   (COPY-TREE cod)
						   cod) TERM)|#)))
		     #'CDDR
		     SUBSTITUTION)
	      TERM)))

(defun uni=substitute (dom cod term)
  (if (dt-variable.is term)
      (if (= dom term)
	  cod
	  (if (opt-get.option sort_literals)
	      (progn (dt-variable.putsort term (uni=substitute dom cod (copy-tree (dt-variable.sort term))))
		     term)
	      term))
      (if (dt-constant.is term)
	  term
	  (if (atom term)
	      term
	      (progn (mapl #'(lambda (subterms.rest) (rplaca subterms.rest (uni=substitute dom cod (first subterms.rest))))
			   term)
		     term)))))

(DEFUN UNI=APPLY.SUBSTITUTION.TO.SUBSTITUTION (S1 S2)
						; edited:  9-dec-83 17:46:56                       hjb
						; Authors: BUERCKERT
						; input:  two substitutions s1 and s2, where s2 may
						;         have bindings
						; effect: s1 will be applied to s2  and to its
						;         bindings, if there are.
						; value:  the new substitution
						; warning:the intersection of dom(s2) and var(s1) must
						;         be empty!
  (let ((RESULT (COPY-TREE S2)))
    (SMAPL #'(LAMBDA (SUBSTITUTES)
	       (NSUBST (COPY-TREE (SECOND SUBSTITUTES)) (CAR SUBSTITUTES) RESULT))
	   #'CDDR
	   S1)
    (when (DT-VARIABLE.GET.BINDING (CAR S2))
      (SMAPL #'(LAMBDA (TAIL)
		 (DT-VARIABLE.DELETE.BINDING (CAR TAIL))
		 (DT-VARIABLE.PUT.BINDING (CAR TAIL) (SECOND TAIL)))
	     #'CDDR
	     RESULT))
    RESULT))

(DEFUN UNI=FIND.VARIABLES (EXPRESSION VARIABLE.BUFFER)
						; Edited: 4. 8. 1984
						; Authors: BUERCKERT
						; Input:  an arbitrary term or termlist (with
						;         bindings) and a buffer.
						; Effect: the uni-variables of expression resp. its
						;         bindings are inserted in 'variable.buffer'.
						; Value:  the buffer, whose contents are the variables
  (COND ((CONSP EXPRESSION)
	 (MAPC #'(LAMBDA (EXPRESSION) (UNI=FIND.VARIABLES EXPRESSION VARIABLE.BUFFER)) EXPRESSION))
	((UNI=VARIABLE.IS EXPRESSION)
	 (let ((BINDING (DT-VARIABLE.GET.BINDING EXPRESSION)))
	   (if BINDING
	       (UNI=FIND.VARIABLES BINDING VARIABLE.BUFFER)
	       (BUFFER.INS EXPRESSION VARIABLE.BUFFER)))))
  VARIABLE.BUFFER)

(DEFUN UNI=COMPOSITION.OF.SUBSTITUTIONS (SUBST1 SUBST2 &OPTIONAL FLAG)
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; input:  two substitutions where domain of subst1
						;         and domain of subst2 have no variables in
						;         common, and a flag
						; value:  a substitution which is obtained by
						;         composing subst2 and subst1
						; effect: if flag = t, subst2 will not be applied to
						;         the bindings of subst1
						; warning:the value is not always a substitution,
						;         e.g.if subst1 = (x b) and subst2 = (y f(x))
						;         then subst2*subst1=(y f(x) x b)
						;         a condition is:domain of subst1 and codomain
						;         of subst2 have no variables in common
  (COND ((NULL SUBST1) SUBST2)
	((NULL SUBST2) SUBST1)
	(FLAG (NCONC (UNI=APPLY.SUBSTITUTION SUBST2 SUBST1 T) SUBST2))
	(T (NCONC (UNI=APPLY.SUBSTITUTION.TO.SUBSTITUTION SUBST2 SUBST1) SUBST2))))

(DEFUN UNI=COMPOSITION.OF.SUBSTITUTION.AND.SUBSTITUTIONLIST (SUBSTITUTION SUBSTITUTIONLIST &OPTIONAL FLAG)
						; edited:  6-dec-83 12:10:49
						; Authors: BUERCKERT
						; confer uni-composition.of.substitutions where
						; subst2 is a list of substitutions
  (COND ((NULL SUBSTITUTION) SUBSTITUTIONLIST) ((NULL SUBSTITUTIONLIST) NIL)
	((NULL (CAR SUBSTITUTIONLIST)) (LIST SUBSTITUTION))
	(T
	 (MAPCAR #'(LAMBDA (SUBST) (UNI=COMPOSITION.OF.SUBSTITUTIONS SUBSTITUTION SUBST FLAG)) SUBSTITUTIONLIST))))

(DEFUN UNI=COPY (UNIFIERS)
						; edited: 4. 9. 1984
						; Authors: BUERCKERT
						; input:  a list of unifiers
						; effect: the codomain terms are copied.
						; value:  the list of unifiers
  (MAPC
    #'(LAMBDA (UNIFIER)
        (SMAPL #'(LAMBDA (TAIL) (RPLACA TAIL (COPY-TREE (CAR TAIL)))) #'CDDR (CDR UNIFIER)))
    UNIFIERS)
  UNIFIERS)

(DEFMACRO UNI=CONSTANT.IS (OBJECT)
						; edited: 18-apr-80 15:23:22
						; Authors: BUERCKERT
  `(LET ((OBJECT ,OBJECT))
     (OR (DT-TERM.IS.CONSTANT OBJECT)
	 (MEMBER OBJECT UNI*VARIABLES.REGARDED.AS.CONSTANTS)
	 (MEMBER OBJECT UNI*VARIABLES.DECLARED.AS.CONSTANTS))))

(DEFMACRO UNI=VARIABLE.IS (OBJECT)
						; edited at 10.07.84  by  hjb
						; input: a s-expression
						; value: nil, if object is no variable or it is
						;        one without bindings and it is regarded
						;        or declared as constant.
  `(LET ((OBJECT ,OBJECT))
     (AND (DT-TERM.IS.VARIABLE OBJECT)
	  (OR (DT-VARIABLE.GET.BINDING OBJECT)
	      (not (or (MEMBER OBJECT UNI*VARIABLES.REGARDED.AS.CONSTANTS)
		       (MEMBER OBJECT UNI*VARIABLES.DECLARED.AS.CONSTANTS)))))))

(DEFUN UNI=GET.BINDING (TERM)
						; edited: 17-sep-83 11:34:41                       hjb
						; Authors: BUERCKERT
						; input:  term
						; value:  term, if it is not a variable or if it is a
						;         variable without any binding.
						;         binding(term), if it exists.
  (if (UNI=VARIABLE.IS TERM)
      (let ((BINDING.OF.TERM (DT-VARIABLE.GET.BINDING TERM)))
	(if BINDING.OF.TERM
	    (UNI=INSERT.BINDINGS.IN BINDING.OF.TERM)
	    TERM))
      term))

(DEFMACRO UNI=SET.BINDINGS (UNIFIER)
						; edited: 16-sep-83 13:15:28                       hjb
						; Authors: BUERCKERT
						; input:  unifier
						; value:  nil
						; effect: pushs the term of each variable in domain of
						;         unifier into the binding stag of this
						;         variable. unifier will not be changed]
  `(SMAPL #'(LAMBDA (L) (DT-VARIABLE.PUT.BINDING (CAR L) (SECOND L))) #'CDDR ,UNIFIER))

(DEFUN UNI=UNBIND (UNILIST)
						; edited: 28-oct-83 10:44:38                       hjb
						; Authors: BUERCKERT
						; input:  list of unifiers
						; value:  returns input
						; effect: if there is only one unifier in unilist, its
						;         bindings will be deleted. if there are more
						;         unifiers in unilist, they should have no
						;         bindings and so none will be deleted]
  (PROG1 UNILIST (when (AND UNILIST (CAR UNILIST) (NULL (CDR UNILIST)))
		   (UNI=RESET.BINDINGS (CAR UNILIST)))))

(DEFMACRO UNI=RESET.BINDINGS (UNIFIER)
						; edited: 13-oct-83 11:51:33                       hjb
						; Authors: BUERCKERT
						; input:  unifier - a substitution
						; value:  nil
						; effect: the bindings of all variables of domain of
						;         termlist will be deleted.
  `(SMAPC #'(LAMBDA (VAR) (DT-VARIABLE.DELETE.BINDING VAR)) #'CDDR ,UNIFIER))

(DEFUN UNI-INSERT.BINDINGS.IN (EXPRESSION)
  (UNI=INSERT.BINDINGS.IN expression))

(DEFUN UNI=INSERT.BINDINGS.IN (EXPRESSION)
						; edited at 13-aug-84 14:48)
						; edited: 13. 8. 1984
						; Authors: BUERCKERT
						; input:  an arbitrary term or termlist
						; effect: the variable bindings are inserted into the
						;         term.
						; value:  the term (or termlist) with all bindings
						;         incorporated.
						; warning: the old expression is not destroyed, but
						;         the new expression may share some
						;         substructures with the old one]
  (if (ATOM EXPRESSION)
      (if (DT-TERM.IS.VARIABLE EXPRESSION)
	  (let ((BINDING (DT-VARIABLE.GET.BINDING EXPRESSION)))
	    (if BINDING
		(UNI=INSERT.BINDINGS.IN BINDING)
		EXPRESSION))
	  EXPRESSION)
      (let* ((OLDCAR (CAR EXPRESSION))
	     (OLDCDR (CDR EXPRESSION))
	     (NEWCAR (UNI=INSERT.BINDINGS.IN OLDCAR))
	     NEWCDR)
	(when OLDCDR (SETQ NEWCDR (UNI=INSERT.BINDINGS.IN OLDCDR)))
	(if (OR (NEQ OLDCAR NEWCAR) (NEQ OLDCDR NEWCDR))
	    (CONS NEWCAR NEWCDR)
	    EXPRESSION))))

(DEFUN UNI-DELETE.ALL.BINDINGS NIL
						; edited: 18-jan-84 16:14:50                       hjb
						; Authors: BUERCKERT
						; input:  nil
						; value:  nil
						; effect: deletes all still existing bindings and
						;         prints the variables, which had bindings.
  (LET ((LIST (POP.BUFFER.STACK UNI*BUFFER.STACK)))
    (DODOWN (RPTN (MEM-ALL.ADR))
      (COND
	((DT-VARIABLE.IS (1+ RPTN))
	 (COND
	   ((DT-VARIABLE.GET.BINDING (1+ RPTN)) (DT-VARIABLE.DELETE.BINDING (1+ RPTN)) (BUFFER.CONS (1+ RPTN) LIST))))))
    (PRINC "variables which had bindings:" T)
    (TERPRI T) (PROGN (PRINC (BUFFER.CONTENTS LIST))
		      (TERPRI))
    (PUSH.BUFFER.STACK LIST UNI*BUFFER.STACK)
    NIL))

(DEFUN UNI-SHOW.ALL.BINDINGS NIL
						; edited:  18-jan-84 16:14:50
						; Authors: BUERCKERT
						; input:   nil
						; value:   nil
						; effect: prints lists of the variables with their
						;         bindings, if they have.
  (PROG ((BINDING (POP.BUFFER.STACK UNI*BUFFER.STACK)))
	(DODOWN (RPTN (MEM-ALL.ADR))
	  (COND
	    ((DT-VARIABLE.IS (1+ RPTN))
	     (COND
	       ((DT-VARIABLE.GET.BINDING (1+ RPTN)) (BUFFER.CONS (DT-VARIABLE.GET.BINDING (1+ RPTN)) BINDING)
		(BUFFER.CONS (1+ RPTN) BINDING))))))
	(PRINC "binding unifier:" T) (TERPRI T) (PROGN (PRINC (BUFFER.CONTENTS BINDING)) (TERPRI))
	(PUSH.BUFFER.STACK BINDING UNI*BUFFER.STACK)))

(DEFUN UNI=TEMP.FIND.VARIABLES (EXPR)
  (PROG ((BUFFER (POP.BUFFER.STACK UNI*BUFFER.STACK)))
	(RETURN
	  (PROG1 (COPY-TREE (BUFFER.CONTENTS (UNI=FIND.VARIABLES EXPR BUFFER))) (PUSH.BUFFER.STACK BUFFER UNI*BUFFER.STACK)))))


(defun uni-equal (t1 t2)
  (if (dt-function.theories) (member nil (uni-unify.terms t1 t2)) (equal t1 t2)))

(defun uni-restrict (uni vars)
  (smapcon #'(lambda (restuni)
	       (if (member (first restuni) vars)
		   (list (first restuni) (second restuni))
		 nil))
	   #'cddr
	   uni))