;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.4
;;; Reason: Function MKRP::OPT=PUT:  correction for demon
;;; Function MKRP::OPT-PUT.OPTION:  dito
;;; Function MKRP::OPT-SET.STANDARD:  dito
;;; Function MKRP::OPT=WND_PUT.OPTION:  dito
;;; Function MKRP::OPT-PUT.LIST.OPTIONS:  dito
;;; Written by prckln, 3/28/92 02:34:55
;;; while running on Magic File Server from FEP0:>GENERA-8-0-INC-KKL-BEWEISER.LOAD.1
;;; with Genera 8.0.1, Logical Pathnames Translation Files NEWEST, IP-TCP 422.2,
;;; RPC 415.0, Embedding Support 407.0, UX Support 416.0,
;;; Experimental Network RPC 415.0, Experimental NFS Client 415.0, CLX 419.0,
;;; C Runtime 416.0, Compiler Tools Package 411.0, Compiler Tools Runtime 411.0,
;;; C Packages 413.0, Minimal Lexer Runtime 416.0, Lexer Package 415.0,
;;; Syntax Editor Runtime 411.0, Experimental X Server 409.0, X Remote Screen 418.1,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.3, microcode 3640-MIC 430,
;;; FEP 127, Fep0:>v127-lisp.flod(64), Fep0:>v127-loaders.flod(64),
;;; Fep0:>v127-info.flod(64), Fep0:>v127-debug.flod(38), 1067x748 B&W Screen,
;;; Machine serial number 5603.



(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MKRP:PROG;OPT;OPTIONS.LISP.NEWEST")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OPT;OPTIONS.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")


(DEFUN OPT=PUT (OPTID VALUE)
						; Edited:  28-MAR-1992 02:28
						; Authors: PRCKLN
						; Input:   an option-identifier (litatom) without the 'opt* - prefix
						; Effect:  sets the global-var opt*optid to value
						; Value:   value (= input)
  (SETf (symbol-value (mkrp-INTERN (format nil "OPT*~A" OPTID))) value)
  (eval (opt=get.option.demon (ASSOC optid OPT*ALL.OPTIONS))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OPT;OPTIONS.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")


(DEFMACRO OPT-PUT.OPTION (OPTID VALUE)
  ;; input:  -an litatom (the name of an option without the opt*-prefix)
  ;;         -the value of the option (any lisp-object)
  ;; value:  t -- if the option-value was within the li-
  ;;         mits of its arg-range nil -- else
  ;; effect: checks if value is legal (see argrange in opt*all.options) and sets it to the corres-
  ;;         ponding standardized value. if it is not legal nothing happens
  `(LET ((STVAL (OPT=CHECK ',OPTID ,VALUE)))
     (COND ((EQL 'ILLEGAL.VALUE STVAL) NIL)
	   (T (OPT=PUT ',OPTID STVAL) T))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OPT;OPTIONS.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")


(DEFMACRO OPT-SET.STANDARD (&optional (defvarflag nil))
  ;; input: If true a list of defvars else a list of setqs
  ;; value: undefined
  ;; effect: defines or sets all options (in opt*all.options) to the corresponding default-value.
  `(PROGN ,@(MAPCAR #'(LAMBDA (OPTION.DESCRIPTOR)
			(if defvarflag
			    `(DEFvar ,(INTERN (format nil "OPT*~A" (OPT=GET.OPTION.NAME OPTION.DESCRIPTOR))
					      (find-package "MKRP")))
			    `(opt=put ',(OPT=GET.OPTION.NAME OPTION.DESCRIPTOR)
				      ',(OPT=GET.DEFAULT.VALUE OPTION.DESCRIPTOR))))
		    OPT*ALL.OPTIONS)))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OPT;OPTIONS.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")


#+(or symbolics explorer)
(DEFUN OPT=WND_PUT.OPTION (OPTID VALUE)
  (let ((STVAL (OPT=CHECK OPTID VALUE)))
    (COND ((EQL 'ILLEGAL.VALUE STVAL) NIL)
	  (T (eval `(OPT=PUT ',OPTID ',STVAL)) T))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MKRP:PROG;OPT;OPTIONS.LISP.NEWEST")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")

(DEFUN OPT-PUT.LIST.OPTIONS (NAME.VALUE.LIST)
						      ; input:  -name.value.list is a list of dotted pairs
						      ;         (....(optid . value)....) with optid = name"
						      ;         of an option (without opt*-prefix) and value=   the value it is to be set
						      ; value:  t -- if all values are legal (see argranges of the optids in opt*all.options).
						      ;         nil -- else.
						      ; effect: checks all values of the optids and sets 
						      ;         the vars opt*optid to the standardised values
						      ;         for the legal ones.
  (LET (NORMALIZED.VALUE LIST.OF.NORMALIZED.VALUES illegal)
    (mapc #'(LAMBDA (NAME.VALUE)
	      (SETQ NORMALIZED.VALUE (OPT=CHECK (CAR NAME.VALUE) (CDR NAME.VALUE)))
	      (if (EQL NORMALIZED.VALUE 'ILLEGAL.VALUE)
		  (setq illegal t)
		  (SETQ LIST.OF.NORMALIZED.VALUES (NCONC1 LIST.OF.NORMALIZED.VALUES (cons (car name.value) NORMALIZED.VALUE)))))
	  NAME.VALUE.LIST)
    (MAPC #'(LAMBDA (NAME.VALUE) (OPT=PUT (CAR NAME.VALUE) (cdr name.value)))
	  LIST.OF.NORMALIZED.VALUES)
    (not illegal)))

