;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.5
;;; Reason: Function MKRP::MKRP-MAKE.PATHNAME:  Uppercase
;;; Function MKRP::OS=OPEN.INPUT.FILE:  sting and symbol equiv
;;; Function MKRP::OS=PROTOCOL.PROC:  symbol code file
;;; Written by prckln, 4/02/92 19:27:53
;;; while running on JS-SFBUX1 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.4, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.19DEC91) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.9:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 519.

;;; Patch file for MKRP version 53.5
;;; Written by prckln, 4/03/92 14:54:27
;;; while running on JS-SFBUX2 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.4, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.19DEC91) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.9:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 505.





(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MKRP:PROG;SERV;SERVICEFILES.LISP.NEWEST"
  "MKRP:PROG;OS;OPERATINGSYSTEM.LISP.NEWEST")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OS;OPERATINGSYSTEM.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-")

(DEFUN OS=OPEN.INPUT.FILE (USER.GIVEN.SYMBOL ACTUAL.STORE.SYMBOL command file.type NAME TYPE)
  ;; Input:  user.given.symbol is given by the user as some input file, this may be nil in order to obtain the default
  ;;         actual.store.symbol is the name of the symbol in this module in whose value cell the pathname
  ;;         or stream is stored.
  ;; Effect: Opens stream for input and sets it on actual.store.symbol. If this doesn't work a warning is printed.
  ;; Value:  True, if the file could be opened, nil otherwise
  (IF USER.GIVEN.SYMBOL
      (COND ((SECOND (MULTIPLE-VALUE-LIST
		       (TESTEVAL (mkrp-make.PATHNAME t nil (mkrp-default.lisp) (funcall (mkrp-converse.filename) (STRING USER.GIVEN.SYMBOL))) T)))
	     (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'ERROR.ILLEGAL.FILE USER.GIVEN.SYMBOL command file.type) *standard-output*)
	     NIL)
	    ((NOT (PROBE-FILE (mkrp-make.PATHNAME t nil (mkrp-default.lisp) (funcall (mkrp-converse.filename) (STRING USER.GIVEN.SYMBOL)))))
	     (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'ERROR.NO.FILE.EXISTS USER.GIVEN.SYMBOL command file.type) *standard-output*)
	     NIL)
	    (actual.store.symbol (print (SETF (SYMBOL-VALUE ACTUAL.STORE.SYMBOL) (string USER.GIVEN.SYMBOL))) T)
	    (t t))
      (COND ((and (not (SYMBOL-VALUE ACTUAL.STORE.SYMBOL))
		  (not ACTUAL.STORE.SYMBOL)
		  (not (probe-file (MKRP-MAKE.PATHNAME T name type NIL))))
	     (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'ERROR.NO.FILE.EXISTS  (MKRP-MAKE.PATHNAME T name type NIL) command file.type)
			    *standard-output*)
	     nil)
	    ((NOT (SYMBOL-VALUE ACTUAL.STORE.SYMBOL))
	     (when ACTUAL.STORE.SYMBOL
	       (SETF (SYMBOL-VALUE ACTUAL.STORE.SYMBOL)
		     (MKRP-MAKE.PATHNAME T NAME TYPE NIL)))
	     t)
	    ((STREAMP (SYMBOL-VALUE ACTUAL.STORE.SYMBOL))
	     (MAKE-PATHNAME :VERSION :NEWEST
			    :DEFAULTS (TRUENAME (SYMBOL-VALUE ACTUAL.STORE.SYMBOL))))
	    ((FILE.EXISTS (SYMBOL-VALUE ACTUAL.STORE.SYMBOL))
	     (MAKE-PATHNAME :VERSION :NEWEST
			    :DEFAULTS (SYMBOL-VALUE ACTUAL.STORE.SYMBOL)))
	    (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'ERROR.NO.FILE.EXISTS (symbol-value ACTUAL.STORE.SYMBOL) command file.type)
			      *standard-output*) NIL))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OS;OPERATINGSYSTEM.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-")

(DEFUN OS=PROTOCOL.PROC (CODE.FILE LIST.FILE)	; INPUT:  LIST OF 0-2 ATOMS:
						;         [<CODE.FILE> [<LIST.FILE>]]
						; EFFECT: PREPARES A PROOF PROTOCOL FROM THE RAW DATA
						;         ON CODE.FILE.
						; VALUE:  UNDEFINED.
  (COND
    ((OS=SYSTEM.IS.READY.FOR 'PROTOCOL)
     (OS=OPEN.LIST.FILE LIST.FILE)
     (PRO-LIST.PROTOCOL (MKRP-MAKE.PATHNAME T (mkrp-default.code) (mkrp-default.lisp) CODE.FILE)
			OS*ACTUAL.LIST.FILE)
     (OS=CLOSE.LIST.FILE)
     (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'PROTOCOL.PROC_LIST.FILE.MESSAGE OS*ACTUAL.LIST.FILE) *standard-output*))))


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
		       (if (pathnamep odefaults)
			   odefaults
			   (funcall (mkrp-converse.filename) (if (symbolp odefaults)
								 (symbol-name odefaults)
								 odefaults)))
		       ""))
	 (pathname (pathname defaults)))
    (make-pathname :directory (if directory.example.name.flag
				  (or (and (not (equal (pathname-directory pathname) '(:relative)))
					   (pathname-directory pathname))				      
				      (append (pathname-directory (pathname (mkrp=get.default.directory)))
					      (list (mkrp=get.default.name))))
				  (pathname-directory pathname))
		   :name (funcall (mkrp-converse.filename) (or (pathname-name defaults)
							       name))
		   :type (funcall (mkrp-converse.filename)
				  (or (and (not (eq :unspecific (pathname-type defaults))) (pathname-type defaults))
				      type))
		   :host (or (and #-symbolics(string/= "" (namestring defaults))
				  #+symbolics t	      ;nil ??????????
				  (pathname-host defaults))
			     (pathname-host (pathname (mkrp=get.default.directory))))
		   :defaults pathname)))

