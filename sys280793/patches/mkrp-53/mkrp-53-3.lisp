;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.3
;;; Reason: Compatibility
;;; Function MKRP::MKRP-MAKE.PATHNAME:  Host for options file
;;; Written by prckln, 3/26/92 23:42:44
;;; while running on JS-SFBUX2 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.2, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.19DEC91) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.4:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 505.



(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MKRP:PROG;SERV;SERVICEFILES.LISP.NEWEST")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer repair.lisp >mkrp F: (10)
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Package: mkrp; Syntax: common-lisp; Mode: lisp -*-")

(defun sel-weight (litlist)
						      ; Edited:  06-FEB-1992 20:32
						      ; Authors: MKRP
						      ; Input:   A literal list.
						      ; Effect:  -
						      ; Value:   The heuristic value according to option STR_POLYNOMIAL.WEIGHT.
  (let ((res 0))
    (mapc #'(lambda (lit)
	      (incf res 2)
	      (incf res (sel=weight_eval (dt-term_create (ds-lit.predicate lit) (ds-lit.termlist lit)))))
	  litlist)
    res))

(defun sel=weigth_set.foreign (symbol)
						; Edited:  01-FEB-1992 14:28
						; Authors: PRCKLN
						; Input:   SYMBOL is a function, predicate, or constant.
						; Effect:  Resets the Polynomial to weight function and predicate
						;          symbols
						; Value:   -
  (let ((entry (cassoc (intern (dt-pname symbol) (find-package "MKRP")) (opt-get.option er_weight.polynomials))))
    (dt-putprop symbol 'sel*weight
		(if entry
		    (compile nil `(lambda ,(first entry) ,@(rest entry)))
		    #'(lambda (&rest args) (if args (apply #'+ 2 args) 1))))
    (dt-getprop symbol 'sel*weight)))

;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;SERV;SERVICEFILES.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-")

#-allegro
(defun mkrp-make.pathname (directory.example.name.flag name type odefaults)
						; Edited:  07-FEB-1989 21:15
						; Authors: PRCKLN
						; Input:   DIRECTORY.EXAMPLE.NAME.FLAG is a flag,
						;          NAME and TYPE are two strings, and
						;          ODEFAULTS is a pathname, a string, or a list.
						; Effect:  -
						; Value:   A pathname
  (let* ((defaults (if odefaults
		       (if (symbolp odefaults)
			   (funcall (mkrp-converse.filename) (symbol-name odefaults))
			   odefaults)
		       ""))
	 (pathname (pathname defaults)))
    (make-pathname :directory (if directory.example.name.flag
				  (or (and (not (equal (pathname-directory pathname) '(:relative)))
					   (pathname-directory pathname))				      
				      (append (pathname-directory (pathname (mkrp=get.default.directory)))
					      (list (mkrp=get.default.name))))
				  (pathname-directory pathname))
		   :name (or (pathname-name defaults)
			     name)
		   :type (or (and (not (eq :unspecific (pathname-type defaults))) (pathname-type defaults))
			     type)
		   :host (or (and #-symbolics(string/= "" (namestring defaults))
				  #+symbolics t ;nil ??????????
				  (pathname-host defaults))
			     (pathname-host (pathname (mkrp=get.default.directory))))
		   :defaults pathname)))

