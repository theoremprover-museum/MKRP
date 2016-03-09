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


(DEFUN SEL=MANUAL NIL
						; edited: 11-dec-83 16:49:55        ne
						; effect: if a link was selected by the user, it is
						;         worked off. thereafter control is passed
						;         to the next operation block.
						; value:  undefined.
  (PROG ((LINK (GET 'SEL=MANUAL 'SEL*LINK)))
	(CASE SEL*REDUCE.DEDUCE
	  (DEDUCE
	    (COND ((SYMBOL-VALUE 'SEL=MANUAL) (SEL=PASS.CONTROL NIL NIL))
		  ((NULL LINK) (ERROR "sel=manual called in deduce mode with link = nil.: ~a" NIL))
		  (T (SETQ SEL=MANUAL T) (SEL=MAKE.DEDUCTION.CODE LINK 'FIRST))))
	  (REDUCE
	    (COND ((NULL LINK) (ERROR "sel=manual called in reduce mode with link = nil.: ~a" NIL))
		  (T (REMPROP 'SEL=MANUAL 'LINK) (SEL=MAKE.REDUCTION.CODE (CG-CLAUSES INSERTED) NIL (LIST LINK)))))
	  (OTHERWISE (ERROR "illegal reduce.deduce in sel=manual:: ~a" SEL*REDUCE.DEDUCE))))
  (SEL=RETURN))

(defun sel=manual.exec ()
						; Edited:  13-MAR-1991 00:14
						; Authors: PRCKLN
						; effect: the next link to be operated upon can be
						;         selected by the user in a dialogue. in this
						;         case the property 'link of the atom
						;         sel=manual becomes the selected link.
						; value:  if the user selected a link then t else nil.
  (let (ANSWER DIALOGUE.END RESULT)
    (WHILE (NOT DIALOGUE.END)
      (format t "Manual link selection. ~%")
      (format t "  Enter a link with operational colour, or~%")
      (format t "        m       for `menu', to select one, or~%")
      (format t "        ok      to let the system decide automatically, or~%")
      (format t "        off     to switch off manual control.~%")
      (SETQ ANSWER (READ T))
      (COND ((EQL ANSWER 'OFF)
	     (opt-put.option gen_manual.control nil)
	     (SETQ RESULT NIL DIALOGUE.END T))
	    ((EQL ANSWER 'ok) (SETQ RESULT NIL DIALOGUE.END T))
	    ((member ANSWER '(m menu))
	     (let ((select (sel=manual.menu)))
	       (setf result                      select
		     (GET 'SEL=MANUAL 'SEL*LINK) select
		     DIALOGUE.END                T)))
	    ((AND (DS-LINK.IS ANSWER)
		  (MEMBER ANSWER (CG-LINKS (DS-LINK.COLOURS.FOR 'OPERATION.extended) ALL)))
	     (SETF (GET 'SEL=MANUAL 'SEL*LINK) ANSWER
		   RESULT                      T
		   DIALOGUE.END                T))
	    ((DS-LINK.IS ANSWER)
	     (format t "Link ~A is no operational link of the actual graph.~%" ANSWER))
	    (T (format t "~A is no link.~%" answer))))
    RESULT))

(DEFUN SEL=MANUAL.ACTIVATE? NIL
						; edited: 11-dec-83 16:45:26        ne
						; effect: the next link to be operated upon can be
						;         selected by the user in a dialogue. in this
						;         case the property 'link of the atom
						;         sel=manual becomes the selected link.
						; value:  if the user selected a link then t else nil.
  (when (OPT-GET.OPTION GEN_MANUAL.CONTROL)
    (SEL=STRATEGIES.ACTIVATE?.exec)
    (sel=manual.exec)))

(defun sel=print.to.string (res stream)
  (setq res (mapcar #'(lambda (lit) (list (ds-lit.sign lit) (cons (ds-lit.predicate lit) (ds-lit.termlist lit))))
		    res))
  #+symbolics
  (scl:with-character-style ('(:swiss :roman :very-small) stream)
    (PP-PRINT.LITERALS res stream))
  #-symbolics
  (PP-PRINT.LITERALS res stream))

(defun sel=manual.list.items (colour links)
						; Edited:  12-MAR-1991 18:00
						; Authors: PRCKLN
						; Input:   A COLOUR and a list of links of this colour.
						; Effect:  -
						; Value:   A list of elements (NAME STRING . LINK) one for each link,
						;          normally the result of the
						;          corresponding operation.
  (mapcar #'(lambda (link)
	      (cons (format nil "~A ~A-~A " (if (DS-LINK.IS.MARKED active link) "*" " ") colour link)
		    (cons (with-output-to-string (st)
			    (let ((res (ds-link.result link)))
			      (unless res
				(setq res (op-operate link (first (ds-link.unifiers link)) t))
				(ds-link.put.result link res))
			      (sel=print.to.string res st)))
			  link)))
	  links))

(defun sel=manual.menu ()
						; Edited:  12-MAR-1991 17:46
						; Authors: PRCKLN
						; Input:   -
						; Effect:  -
						; Value:   A link to proceed.
  (mkrp-menu.scroll (cons (list "Ok" "Select automatically")
			  (nconc (sel=manual.list.items 'p (cg-links p all))
				 (sel=manual.list.items 'piw (cg-links piw all))
				 (sel=manual.list.items 'r (cg-links r all))
				 (sel=manual.list.items 'si (cg-links si all))))
		    (opt-get.option gen_common.lisp)))

(DEFUN SEL=MANUAL.UPDATE (&OPTIONAL FLAG)
  (DECLARE (IGNORE FLAG))
						; edited: 11-dec-83 17:06:22                ne
						; effect: none. dummy function.
  (when (OPT-GET.OPTION GEN_MANUAL.CONTROL)
    (SEL=STRATEGIES.update.exec)))