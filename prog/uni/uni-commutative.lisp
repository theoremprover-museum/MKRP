;;; -*- package: mkrp; syntax: common-lisp; mode: lisp -*-
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
MKRP, but only if it is not used for military purposes or any
military research. It is also forbidden to use MKRP in nuclear plants
or nuclear research, and for verifying programs in military 
and nuclear research.  A copy of this license is
supposed to have been given to you along with MKRP so you
can know your rights and responsibilities.  
Among other things, the copyright notice
must be preserved on all copies.  |#

(defun comm=unify (term1 term2)
  (comm=unify.terms term1 term2))

(defun comm=unify.terms (term1 term2)
  (let (SORTS XNEW 
	(SORT.OF.T1 (DT-TERM.SORT TERM1))
	(SORT.OF.T2 (DT-TERM.SORT TERM2)))
    (COND ((EQL TERM1 TERM2) (list nil))
	  ((AND (UNI=VARIABLE.IS TERM1) (UNI=VARIABLE.IS TERM2))
	   (COND ((DT-SORT.IS.SUBSORT SORT.OF.T2 SORT.OF.T1)
		  (LIST (LIST TERM1 TERM2)))
		 ((DT-SORT.IS.SUBSORT SORT.OF.T1 SORT.OF.T2)
		  (LIST (LIST TERM2 TERM1)))
		 (T (SETQ SORTS (DT-SORT.GREATEST.COMMON.SUBSORTS SORT.OF.T1 SORT.OF.T2))
		    (COND ((NULL SORTS) nil)
			  ((CDR SORTS)
			   (MAPCAR #'(LAMBDA (SORT) (SETQ XNEW (DT-VARIABLE.CREATE SORT)) (LIST TERM1 XNEW TERM2 XNEW))
				   SORTS))
			  (T (SETQ XNEW (DT-VARIABLE.CREATE (CAR SORTS)))
			     (LIST (LIST TERM1 XNEW TERM2 XNEW)))))))
	  ((UNI=VARIABLE.IS TERM1)
	   (COND ((IN TERM1 TERM2) nil)
		 ((DT-SORT.IS.SUBSORT SORT.OF.T2 SORT.OF.T1)
		  (LIST (LIST TERM1 TERM2)))
		 ((CONSP TERM2)
		  (UNI=POLY.X.WITH.T TERM1 term2))
		 (T NIL)))
	  ((UNI=VARIABLE.IS TERM2)
	   (COND ((IN TERM2 TERM1) NIL)
		 ((DT-SORT.IS.SUBSORT SORT.OF.T1 SORT.OF.T2)
		  (LIST (LIST TERM2 TERM1)))
		 ((CONSP TERM1)
		  (UNI=POLY.X.WITH.T TERM2 term1))
		 (T NIL)))
	  ((OR (UNI=CONSTANT.IS TERM1) (UNI=CONSTANT.IS TERM2))
	   NIL)
	  ((AND (DT-TERM.IS.ABBREVIATION TERM1) (DT-TERM.IS.ABBREVIATION TERM2))
	   NIL)
	  ((NEQ (CAR TERM1) (CAR TERM2)) NIL)
	  ((MEMBER 'commutativE (DT-FUNCTION.ATTRIBUTES (CAR TERM1)))
	   (nconc (comm=UNIFY.termlists (rest term1) (rest term2))
		  (comm=UNIFY.termlists (rest term1) (reverse (rest term2)))))
	  (T (UNI=UNIFY.TERMLISTS (CDR TERM1) (CDR TERM2))))))

(defun comm=UNIFY.termlists (termlist1 termlist2)
  (if termlist1
      (comm=insert (comm=unify.terms (first termlist1) (first termlist2))
		   (comm=UNIFY.termlists (rest termlist1) (rest termlist2)))
      (list nil)))

(defun comm=insert (unis1 unis2)
  (mapcan #'(lambda (uni2)
	      (if (null uni2)
		  unis1
		  (mapcan #'(lambda (uni1)
			      (if (null uni1)
				  (list uni2)				  
				  (let ((unis (list uni2)))
				    (while uni1
				      (setq unis (mapcan #'(lambda (uni)
							     (comm=insert.comp (first uni1) (second uni1) uni))
							 unis))
				      (setq uni1 (rest (rest uni1))))
				    unis)))
			  unis1)))
	  unis2))

(defun comm=insert.comp (dom cod uni)
  (cond ((null uni) (list (list dom cod)))
	(t (setq uni (subst cod dom uni))
	   (let ((comp (list dom cod))
		 (good nil)
		 (unis (list nil)))
	     (ssomel #'(lambda (restuni)
			 (if (not (uni=variable.is (first restuni)))
			     (progn (setq unis (comm=insert (comm=unify.terms (first restuni) (second restuni)) unis)) nil)
			     (if (in (first restuni) (second restuni))
				 (progn (setq unis nil) t)
				 (progn (setq good (cons (first restuni) (cons (second restuni) good)))
					(setq comp (subst (second restuni) (first restuni) comp))
					nil))))
		     #'cddr
		     uni)
	     (if (not (uni=variable.is (first comp)))
		 (setq unis (comm=insert (comm=unify.terms (first comp) (second comp)) unis))
		 (if (in (first comp) (second comp))
		     (setq unis nil) 
		     (setq good (cons (first comp) (cons (second comp) good)))))
	     (comm=insert (list good) unis)))))