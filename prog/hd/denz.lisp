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

;;;  import


(import '(TH-AC-MKRP::ac-match th-ac-MKRP::e-compl*ac-functions)
	(find-package "MKRP"))

(defun denz-init ()
  (setq e-compl*ac-functions (denz=trans.to.joerg
			       (delete nil (mapcar #'(lambda (func) (if (DT-FUNCTION.IS.MARKED ac func) func nil))
						   (dt-function.all))))))
(defun denz=trans.from.joerg (object)
  (let ((hd.object
	  (cond ((consp object)
		 (if (cdr object)
		     (dt-term_create (denz=trans.from.joerg (first object)) (mapcar #'denz=trans.from.joerg (rest object)))
		     (denz=trans.from.joerg (first object))))
		((get object 'mkrp*object))
		(t (error "No new creation of consts, vars, and funs allowed.")))))
    hd.object))

(defun denz=trans.subst.from.joerg (subst)
  (cond ((eq subst t) (list nil))
	(subst (list (mapcan #'(lambda (comp)
				 (list (denz=trans.from.joerg (first comp))
				       (denz=trans.from.joerg (second comp))))
			     subst)))
	(t nil)))

(defun denz=trans.to.joerg (object)
  (if (listp object)
      (mapcar #'denz=trans.to.joerg object)
      (or (let ((res (dt-getprop object 'mkrp*object)))
	    (when res
	      (if (symbolp res)
		  (unless (get res 'mkrp*object)
		    (setf (get res 'mkrp*object) object))
		  (unless (get (first res) 'mkrp*object)
		    (setf (get (first res) 'mkrp*object) object))))
	    res)		
	  (let ((joerg.object
		  (cond ((dt-function.is object)
			 (let ((new (mkrp-intern (dt-pname object))))
			   (dt-putprop object 'mkrp*object new)
			   (setf (get new 'mkrp*object) object)
			   new))
			((dt-constant.is object) 
			 (let ((new (mkrp-intern (dt-pname object))))
			   (dt-putprop object 'mkrp*object (list new))
			   (setf (get new 'mkrp*object) object)
			   (list new)))
			((dt-variable.is object)
			 (let ((new (mkrp-intern (dt-pname object))))
			   (dt-putprop object 'mkrp*object new)
			   (setf (get new 'mkrp*object) object)
			   new))
			(t
			 (let ((fun (denz=trans.to.joerg (dt-term_topsymbol object)))
			       (args (mapcar #'denz=trans.to.joerg (dt-term_arguments object))))
			   (cons fun args))))))
	    joerg.object))))

(defvar uni*th.term.top)

(defun denz=term.create (terml) (dt-term_create (if (dt-function.is uni*th.term.top)
						    uni*th.term.top
						    (setq uni*th.term.top (dt-function.create "AUX" 'any '(any any))))
						terml))

(defun denz=trans.remove.l (tl)
  (declare (ignore tl))
  nil)

(defun denz-match (term1 term2)
  (unwind-protect (denz=trans.subst.from.joerg (ac-match (denz=trans.to.joerg term1) (denz=trans.to.joerg term2)))
    (denz=trans.remove.l (list term1 term2))))

(defun denz-match.list (terml1 terml2)
  (unwind-protect (denz=trans.subst.from.joerg (ac-match (denz=trans.to.joerg (denz=term.create terml1))
							 (denz=trans.to.joerg (denz=term.create terml2))))
    (denz=trans.remove.l (append terml1 terml2))))