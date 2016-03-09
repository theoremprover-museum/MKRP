;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

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

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(defun mkrp-intern (string) (intern string (find-package "MKRP")))

(DEFVAR KKL*ALPHORDER.BUFFER.1 (MAKE-ARRAY 10 :ELEMENT-TYPE #-allegro'STRING-CHAR #+allegro'character 
					   :FILL-POINTER 0 :ADJUSTABLE T))
(DEFVAR KKL*ALPHORDER.BUFFER.2 (MAKE-ARRAY 10 :ELEMENT-TYPE #-allegro'STRING-CHAR #+allegro'character
					   :FILL-POINTER 0 :ADJUSTABLE T))
(DEFun ALPHORDER (X Y &KEY (STRING-COMPARE-FUNCTION #'STRING-LESSP))
  ;; INPUT  X AND Y ANY LISP OBJECTS
  ;; VALUE  NOT NIL IFF X IS LEXICOGRAPHICALLY SMALLER THAN
  ;;	    Y. NUMBERS ARE SMALLER THAN SYMBOLS AND COMPARED
  ;;	    WITH <. SYMBOLS AND STRINGS ARE SMALLER THAN
  ;;	    LISTS.
  ;; EFFECT NOT DIRECTLY COMPARABLE DATATYPES ARE CONVERTED
  ;;	    WITH PRINC-TO-STRING INTO STRINGS.
  (COND ((NUMBERP X) (IF (NUMBERP Y) (< X Y) T))
	((NUMBERP Y) NIL)
	((LISTP X) (IF (LISTP Y) (PROG1 (FUNCALL STRING-COMPARE-FUNCTION
						 (WITH-OUTPUT-TO-STRING (BUFFER KKL*ALPHORDER.BUFFER.1)
						   (PRINC X BUFFER)  KKL*ALPHORDER.BUFFER.1)
						 (WITH-OUTPUT-TO-STRING (BUFFER KKL*ALPHORDER.BUFFER.2)
						   (PRINC Y BUFFER)  KKL*ALPHORDER.BUFFER.2))
					(SETF (FILL-POINTER KKL*ALPHORDER.BUFFER.1) 0
					      (FILL-POINTER KKL*ALPHORDER.BUFFER.2) 0))))
	((LISTP Y) T)
	(T (PROG1 (FUNCALL STRING-COMPARE-FUNCTION
			   (WITH-OUTPUT-TO-STRING (BUFFER KKL*ALPHORDER.BUFFER.1)
			     (PRINC X BUFFER)  KKL*ALPHORDER.BUFFER.1)
			   (WITH-OUTPUT-TO-STRING (BUFFER KKL*ALPHORDER.BUFFER.2)
			     (PRINC Y BUFFER)  KKL*ALPHORDER.BUFFER.2))
		  (SETF (FILL-POINTER KKL*ALPHORDER.BUFFER.1) 0
			(FILL-POINTER KKL*ALPHORDER.BUFFER.2) 0)))))


(defmacro UNTIL (CONDITION &body BODY)
  `(tagbody until.tag
	    ,@ BODY
	    (unless ,CONDITION (GO until.tag))))

(DEFun PUTASSOC (ITEM DATUM A-LIST &KEY (TEST (FUNCTION EQL)))
  (IF (SOME (FUNCTION (LAMBDA (PAIR) (IF (FUNCALL TEST (CAR PAIR) ITEM)
					 (PROGN (SETF (CDR PAIR) DATUM) T ))))
	    A-LIST)
      A-LIST
      (nconc1 a-list (CONS ITEM DATUM))))

(DEFun DELETE-NTH (LIST N)
  (COND ((< N 0) LIST)
	((ZEROP N)
	 (CDR LIST))
	(T (PROG1 LIST
		  (COND ((SETQ LIST (NTHCDR  (1- N) LIST))
			 (RPLACD LIST (CDDR LIST))))))))

(defmacro WHILE (CONDITION &Body BODY)
  `(tagbody while.tag
	    (when ,CONDITION
	      ,@BODY
	      (GO while.tag))))

(defmacro  nconc1 (a b)
  `(nconc ,a (list ,b)))

(defun ATTACH (X L)
  (COND ((CONSP L)
	 (RPLACA (RPLACD L (CONS (CAR L)
				 (CDR L)))
		 X))
	((NULL L)
	 (LIST X))
	(T (ERROR "ERROR IN ATTACH, SECOND ARGUMENT IS NO LIST: ~S" L))))

(CL:DEFUN KKL=MAP.MACROS (AFN SFN LISTS stepflag ACCESSFN CONNECTFN ABORTFORM EMPTYFORM &OPTIONAL NON-NIL)
  (DECLARE (ignore stepflag))
						;EDITED: 18. 4. 1984
						;REMARK: THIS FUNCTION IS CALLED BY THE MACROS OF THE MAP FUNCTIONS
						;        (MAPC, MAPCAR, EVERY, SOME ...) AND PRODUCES THE APPROPRIATE CODE.
						;INPUT:
						;        AFN:       THE USER DEFINED APPLYFUNCTION
						;        SFN:       NIL OR THE USER DEFINED STEPFUNCTION
						;                   (NIL <=> (FUNCTION CDR) )
						;                   LISTS: A LIST OF LISTS
						;        ACCESSFN:  A FUNCTION DETERMINING THE SUBLISTS TO WHICH AFN IS APPLIED.
						;                   ('CAR FOR MAPC, MAPCAR ETC, NIL FOR MAPL,MAPLIST ETC.)
						;        CONNECTFN: A FUNCTION FOR CONNECTING THE PARTIAL RESULTS.
						;                   (EITHER NCONC, NCONC1 OR NIL.)
						;        ABORTFORM: NIL OR AN EXPRESSION CONTAINING THE KEYWORD 'RESULT
						;                   WHICH IS APPLIED TO PARTIAL RESULTS AND DETERMINES
						;                   INTERMEDIATE EXITS.
						;        EMPTYFORM: AN EXPRESSION CONTAINING THE KEYWORD 'RESULT DETERMINING
						;                   THE RESULT OF THE MAPFUNCTION IF ONE
						;                   OF THE LISTS BECOMES EMPTY.
						;        NON-NIL    A FLAG INDICATING THAT NON NIL
						;                   RESULTS SHALL ONLY BE CONNECTED
						;VALUE:  THE MACRO EXPANSION.
  (LET ((COUNTER 0.) LISTVARS AFNFORM EXITFORM)
    (COND ((AND ABORTFORM (NULL AFN))
	   (SETQ AFN 'NULL)))
    (COND ((NULL AFN)
	   (SETQ EMPTYFORM NIL)))
    (SETQ LISTVARS (MAPCAR #'(LAMBDA (NEVER-USED) (DECLARE (IGNORE NEVER-USED))
				     (INTERN (CONCATENATE 'STRING "KKL.MAP.LIST" (PRINC-TO-STRING (INCF COUNTER)))
					     (find-package "MKRP")))
			   LISTS))
    (COND (AFN (SETQ AFNFORM (MAPCAR #'(LAMBDA (VAR) (COND ((NULL ACCESSFN) VAR) (T (LIST ACCESSFN VAR))))
				     LISTVARS))
	       (SETQ AFNFORM (IF (AND (CONSP AFN) (EQ (CAR AFN) 'FUNCTION) (SYMBOLP (CADR AFN)))
				 (CONS (CADR AFN) AFNFORM)
				 (LIST* 'FUNCALL AFN AFNFORM)))))
    (SETQ EXITFORM `(COND (,(COND ((CDR LISTS) (CONS 'OR (MAPCAR #'(LAMBDA (VAR) (LIST 'NULL VAR)) LISTVARS)))
				  (T (LIST 'NULL (CAR LISTVARS))))
			   (RETURN ,(IF CONNECTFN '(PROG1 (CDR KKL.RESULT) (RPLACD KKL.RESULT NIL))
					(SUBST 'KKL.RESULT 'RESULT EMPTYFORM))))))
    `(PROG (,@(MAPCAR #'(LAMBDA (VAR LIST) (LIST VAR LIST)) LISTVARS LISTS)
	    ,@(COND ((OR ABORTFORM EMPTYFORM CONNECTFN) (COND (AFN (LIST 'KKL.RESULT)))))
	    ,@(IF NON-NIL (LIST 'KKL*INTERMEDIATE.RESULT))
	    ,@(IF CONNECTFN (LIST `(KKL*POINTER ,(LIST 'list NIL)))))
	   ,@(IF CONNECTFN (LIST '(SETQ KKL.RESULT KKL*POINTER)))
	LOOP
	   ,EXITFORM
	   ,@(COND (AFN (COND ((EQ CONNECTFN 'NCONC1)
			       (IF NON-NIL
				   `((IF (SETQ KKL*INTERMEDIATE.RESULT ,AFNFORM)
					 (RPLACD KKL*POINTER (SETQ KKL*POINTER (CONS KKL*INTERMEDIATE.RESULT NIL)))))
				   `((RPLACD KKL*POINTER (SETQ KKL*POINTER (CONS ,AFNFORM NIL))))))
			      ((EQ CONNECTFN 'NCONC)
			       `((NCONC KKL*POINTER ,AFNFORM)
				 (SETQ KKL*POINTER (LAST KKL*POINTER))))
			      ((OR ABORTFORM EMPTYFORM)
			       `((SETQ KKL.RESULT ,AFNFORM)))
			      (T (LIST AFNFORM)))))
	   ,@(COND (ABORTFORM (LIST (SUBST 'KKL.RESULT 'RESULT ABORTFORM))))
	   ,@(MAPCAR #'(LAMBDA (VAR)
			 (LIST 'SETQ VAR (COND ((NULL SFN) (LIST 'CDR VAR))
					       ((AND (CONSP SFN) (EQ (CAR SFN) 'FUNCTION) (SYMBOLP (CADR SFN)))
						(LIST (CADR SFN) VAR))
					       (T (LIST 'FUNCALL SFN VAR)))))
		     LISTVARS)
	   (GO LOOP))))

(cl:eval-when
  (compile eval)
  (CL:DEFMACRO KKL=MAKE.MAPS (&rest PARAMETERS)
    `(PROGN ,@(MAPCAR (FUNCTION (LAMBDA (PARAMETER)
				  `(DEFMACRO ,(CAR PARAMETER)
							  (KKL.MAP.AFCT ,@(IF (fifth parameter) '(KKL.MAP.SFCT) NIL)
									&REST KKL.MAP.LISTS)
				     ;; maps across lists.
				     (kkl=map.macros ,@(cdr parameter)))))
		      parameters))))

(kkl=make.maps
  (SMAPCAR          KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR 'NCONC1 NIL                                  NIL  )
  (MAPCAR-NOT-NIL   kkl.MAP.AFCT NIL          KKL.MAP.LISTS nil 'CAR 'NCONC1 NIL                                  NIL T)
  (SMAPCAR-NOT-NIL  KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR 'NCONC1 NIL                                  NIL T)
  (MAPLIST-NOT-NIL  KKL.MAP.AFCT NIL          KKL.MAP.LISTS nil NIL  'NCONC1 NIL                                  NIL T)
  (SMAPLIST         KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  'NCONC1 NIL                                  NIL  )
  (SMAPLIST-NOT-NIL KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  'NCONC1 NIL                                  NIL T)
  (SMAPC            KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR NIL     NIL                                  NIL  )
  (SMAPL            KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  NIL     NIL                                  NIL  )
  (SMAPCAN          KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR 'NCONC  NIL                                  NIL  )
  (SMAPCON          KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  'NCONC  NIL                                  NIL  )
  (SEVERY           KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR NIL     '(COND ((NULL RESULT) (RETURN NIL))) T)
  (SEVERYL          KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  NIL     '(COND ((NULL RESULT) (RETURN NIL))) T)
  (EVERYL           KKL.MAP.AFCT NIL          KKL.MAP.LISTS nil NIL  NIL     '(COND ((NULL RESULT) (RETURN NIL))) T)
  (SNOTANY          KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR NIL     '(COND (RESULT (RETURN NIL)))        T)
  (SNOTANYL         KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  NIL     '(COND (RESULT (RETURN NIL)))        T)
  (NOTANYL          KKL.MAP.AFCT NIL          KKL.MAP.LISTS nil NIL  NIL     '(COND (RESULT (RETURN NIL)))        T)
  (SSOME            KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   'CAR NIL     '(COND (RESULT (RETURN RESULT)))     NIL)
  (SSOMEL           KKL.MAP.AFCT KKL.MAP.SFCT KKL.MAP.LISTS t   NIL  NIL     '(COND (RESULT (RETURN RESULT)))     NIL)
  (SOMEL            KKL.MAP.AFCT NIL          KKL.MAP.LISTS nil NIL  NIL     '(COND (RESULT (RETURN RESULT)))     NIL))


(defmacro c (&rest c)
  (declare (ignore c))
  nil)

(defun enterlisp (end.symbol)
  (terpri) (princ '=)
  (let ((ex (read)))
    (while (not (or end.symbol ex))
      (terpri) (princ (eval ex))
      (terpri) (princ '=)
      (setq ex (read)))))

(defun readline (&optional (stream *terminal-io*))
  (nreverse (let ((res nil) new (strin (read-line stream nil :eof)))
	      (unless (eq strin :eof)
		(with-input-from-string (st strin)
		  (while (not (eq :eof (setq new (read st nil :eof))))
		    (setq res (cons new res)))))
	      res)))

(defun mkrp-read.lines (chars string)
						      ; Edited:  09-OCT-1991 20:35
						      ; Authors: PRCKLN
						      ; Input:   A list of characters
						      ; Effect:  Separates STRING into a list of strings everywhere when one
						      ;          one in CHARS is reached.
						      ; Value:   A list of strings according to effect.
  (with-input-from-string (stream string)
    (let ((achar (read-char stream nil :eof))
	  (res nil))
      (while (not (eq :eof achar))
	(let ((st nil))
	  (while (not (or (eq achar :eof) (member achar chars :test #'char=)))
	    (push achar st)
	    (setq achar (read-char stream nil :eof)))
	  (setq achar (read-char stream nil :eof))
	  (push (coerce (nreverse st) 'string) res)))
      (or (nreverse res) (list "")))))

(defmacro kwote (expression)
  `(list 'quote ,EXPRESSION))


(defun date ()
  (multiple-value-bind (second minute hour day month year day.of.week daylight.saving.time.p time.zone)
      (decode-universal-time (get-universal-time))	; Mysterious Allegro error
    (declare (ignore second day.of.week daylight.saving.time.p time.zone))
    (forMat nil "~2,1,0,'0@a-~A,~a ~2,1,0,'0@a:~2,1,0,'0@a"
	    day
	    (case month
	      (1 "JAN")
	      (2 "FEB")
	      (3 "MAR")
	      (4 "APR")
	      (5 "MAY")
	      (6 "JUN")
	      (7 "JUL")
	      (8 "AUG")
	      (9 "SEP")
	      (10 "OCT")
	      (11 "NOV")
	      (12 "DEC"))
	    year
	    hour
	    minute)))

(defun read-file (pathname)
  (let ((current.package *package*))
    (with-open-file (stream pathname)
      (unwind-protect
	  (do ((expression (list (read stream nil 'eof-reached))
			   (cons (read stream nil 'eof-reached) expression)))
	      ((eq (car expression) 'eof-reached) (nreverse (cdr expression)))
	    (when (and (consp (car expression)) (eq (caar expression) 'in-package))
	      (eval (car expression))))
	(setq *package* current.package)))))

(defmacro DODOWN ((VARIABLE NUMBER &OPTIONAL RESULT) &BODY FORMS)
  `(PROG ((,VARIABLE (1- ,NUMBER)))
	 (COND ((MINUSP ,VARIABLE) (RETURN ,RESULT)))
      LOOP
	 ,@FORMS
	 (SETQ ,VARIABLE (1- ,VARIABLE))
	 (COND ((MINUSP ,VARIABLE) (RETURN ,RESULT))
	       (T (GO LOOP)))))

(defmacro aset (x y z)
  `(setf (aref ,x ,y) ,z))

(defmacro neq (a b)
  `(not (eql ,a ,b)))

#+(or symbolics explorer)
(DEFMACRO testeval (form &optional messageflag)
  ;; input:  'form' is a lisp form.
  ;;         'messageflag' is a flag.
  ;; effect: evaluates 'form'.
  ;;         if an error occurs and messageflag is not nil the error message is printed on *error-output*.
  ;; values: 1. The value of the evaluation of 'form' if no error occurred, nil else.
  ;;         2. Not nil iff an error occurred.
  `(global:catch-error (car (cons ,form nil)) ,messageflag))

#+coral
(lisp:defmacro testeval (form &optional messageflag)
  `(let* ((ccl::*warn-if-redefine* nil)
          (ccl::*break-on-errors* nil)
          (error (symbol-function 'error))
	  (cerror (symbol-function 'cerror))
	  (warn (symbol-function 'warn))
	  result1 result2
	  (message.p ,messageflag))
     (unwind-protect
	 (progn (setf (symbol-function 'error)
		      (function (lambda (message &rest args)
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t))))
		      (symbol-function 'cerror)
		      (function (lambda (m1 message &rest args)
				  (declare (ignore m1))
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t))))
		      (symbol-function 'warn)
		      (function (lambda (message &rest args)
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t)))))
		(multiple-value-setq (result1 result2) (catch 'errortag (values ,form nil))))
       (setf (symbol-function 'error)
	     error
	     (symbol-function 'cerror)
	     cerror
	     (symbol-function 'warn)
	     warn))
     (values result1 result2))
  )

#+kcl
(defmacro testeval (form &optional messageflag)  
						; input:  'form' is a lisp form.
						;         'messageflag' is a flag.
						; effect: evaluates 'form'.
						;         if an error occurs and messageflag is not nil
						;         the error message is printed on *error-output*.
						; values: 1. The value of the evaluation of 'form' if no error occurred, nil else.
						;         2. Not nil iff an error occurred.
  `(let ((handler #'system:universal-error-handler))
     (unwind-protect
	 (progn (setf (symbol-function 'system:universal-error-handler)
		      #'(lambda (ename corr fname cstring estring &rest args)
			  (when ,messageflag
			    (apply (function format) *error-output* estring args))
			  (throw 'errortag 
			    (values nil t))))
		(catch 'errortag (values ,form nil)))
       (setf (symbol-function 'system:universal-error-handler) handler))))

#+dec
(lisp:defmacro testeval (form &optional messageflag)
  `(let ((*universal-error-handler*
	   (function (lambda (fname esfun &rest args)
		       (when ,messageflag
			 (print (list* fname esfun args)
				*error-output*))
		       (throw 'errortag 
			 (values nil t))))))
     (catch 'errortag (values ,form nil))))


#+poplog
(lisp:defmacro testeval (form &optional messageflag)
  `(let* ((*break-on-errors* nil)
	  (error (symbol-function 'error))
	  (cerror (symbol-function 'cerror))
	  (warn (symbol-function 'warn))
	  result1 result2
	  (message.p ,messageflag))
     (unwind-protect
	 (progn (setf (symbol-function 'error)
		      (function (lambda (message &rest args)
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t))))
		      (symbol-function 'cerror)
		      (function (lambda (m1 message &rest args)
				  (declare (ignore m1))
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t))))
		      (symbol-function 'warn)
		      (function (lambda (message &rest args)
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t)))))
		(multiple-value-setq (result1 result2) (catch 'errortag (values ,form nil))))
       (setf (symbol-function 'error)
	     error
	     (symbol-function 'cerror)
	     cerror
	     (symbol-function 'warn)
	     warn))
     (values result1 result2))
  )
#-(or symbolics explorer coral kcl dec poplog)
(lisp:defmacro testeval (form &optional messageflag)
  `(let ((error (symbol-function 'error))
	 (cerror (symbol-function 'cerror))
	 (warn (symbol-function 'warn))
	 result1 result2
	 (message.p ,messageflag))
     (unwind-protect
	 (progn (setf (symbol-function 'error)
		      (function (lambda (message &rest args)
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t))))
		      (symbol-function 'cerror)
		      (function (lambda (m1 message &rest args)
				  (declare (ignore m1))
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t))))
		      (symbol-function 'warn)
		      (function (lambda (message &rest args)
				  (when message.p
				    (apply (function format) *error-output* message args))
				  (throw 'errortag
				    (values nil t)))))
		(multiple-value-setq (result1 result2) (catch 'errortag (values ,form nil))))
       (setf (symbol-function 'error)
	     error
	     (symbol-function 'cerror)
	     cerror
	     (symbol-function 'warn)
	     warn))
     (values result1 result2))
  )

(DEFun LASTN  (LIST NUMBER)
						; (a b c d e) 3 --> ((a b) c d e)
  (cond ((atom list) nil)
	((ZEROP NUMBER) (list list))
	(t (do ((n (- (length list) number) (1- n))
		(list list (rest list))
		(x nil (cons (first list) x)))
	       ((zerop n) (cons (nreverse x) list))))))

(DEFun FIRSTN (LIST N)
  (LET* ((NEW-LIST (CONS NIL NIL))
	 (POINTER NEW-LIST))
    (WHILE (AND LIST (> N 0))
      (RPLACD POINTER (CONS (CAR LIST) NIL))
      (SETQ POINTER (CDR POINTER))
      (SETQ LIST (CDR LIST))
      (SETQ N (1- N)))
    (CDR NEW-LIST)))

(DEFun SUBPAIR (NEW OLD TREE &KEY SHARE (TEST (FUNCTION EQL)))

  (NSUBPAIR NEW OLD (COPY-TREE TREE) :SHARE SHARE :TEST TEST))


(DEFun NSUBPAIR (NEW OLD TREE &KEY SHARE (TEST (FUNCTION EQL)))
  (WHEN (AND (NOT (SOME (FUNCTION
			  (LAMBDA (N O)
			    (COND ((FUNCALL TEST O TREE)
				   (SETQ TREE (IF (NOT SHARE)
						  (COPY-TREE N)
						  N))
				   T))))
			NEW OLD))
	     (CONSP TREE))
    (RPLACA TREE (NSUBPAIR NEW OLD (CAR TREE) :SHARE SHARE :TEST TEST))
    (RPLACD TREE (NSUBPAIR NEW OLD (CDR TREE) :SHARE SHARE :TEST TEST)))
  TREE)


(defun conses (ex)
  (if (consp ex)
      (+ 1 (conses (car ex)) (conses (cdr ex)))
      0))

(defun mkrp-rem.file (pathname)
  (with-open-file (st pathname)
    (let (ex)
      (while (neq (setq ex (read st nil :eof)) :eof)
	(cond ((consp ex)
	       (case (first ex)
		 (defvar (makunbound (second ex)))
		 (defun (fmakunbound (second ex))))))))))


(defun mkrp-menu (item.list cl.flag)
						; Edited:  10-MAR-1990 22:12
						; Authors: PRCKLN
						; Input:   A list of strings or pairs (STRING . VALUE)
						;          and a flag indicating
						;          whether only pure common lisp is available.
						; Effect:  Prints list of strings on screen
						; Value:   One of the strings or if VALUE is given VALUE,
						;          selected by the user.
  (if cl.flag
      (do ((items item.list (rest items))
	   (n 0 (+ 1 n)))
	  ((null items)
	   (format *standard-output* "~%Enter number: ")
	   (let ((res (read *standard-input*)))
	     (if (and (integerp res) (> res -1))
		 (let ((res1 (nth res item.list)))
		   (if (stringp res1)
		       res1
		       (rest res1))))))
	(format *standard-output* "~%~3D: ~5A ~A" n
		(if (stringp (first items)) (first items) (first (first items)))
		(if (stringp (first items)) "" (second (first items)))))
      #+(or symbolics explorer)
      (let ((menu (TV:MAKE-WINDOW 'TV:MOMENTARY-MENU
				  :LABEL "Choose link"
				  :ITEM-LIST (append '(("" :no-select nil))
						     (mapcar #'(lambda (item)
								 (if (stringp item)
								     item
								     (list (format nil "~A ~A" (first item) (second item))
									   :value (rest (rest item)))))
							     item.list)
						     '(("" :no-select nil))))))
	(global::send menu :choose))))


#+(or symbolics explorer)
(scl:defflavor mkrp=menu () (tv:borders-mixin tv:centered-label-mixin
					      tv:top-box-label-mixin
					      tv:margin-scrolling-with-flashy-scrolling-mixin
					      tv:mouse-sensitive-text-scroll-window
					      ;tv:pane-no-mouse-select-mixin
					      tv:window)
  (:default-init-plist
    :sensitive-item-types :sensitive-type-p
    :margin-scroll-regions '(:top :bottom)
    ))

#+(or symbolics explorer)
(Si:define-character-style-families si:*b&w-screen* si:*standard-character-set*
  '(:family :text
    (:size :large (:face :mkrp fonts:s35ger))))

#+(or symbolics explorer)
(defparameter mkrp*menu (tv:make-window 'mkrp=menu
				  :label `(:string "Choose Link" :character-style (:text :mkrp :large))))

#+(or symbolics explorer)
(defvar mkrp*select)

#+(or symbolics explorer)
(defun mkrp=menu.expose (itemlist)
  (let ((ex.window *terminal-io*))
    (scl:send mkrp*menu :set-items 0)
    (makunbound 'mkrp*select)
    (mapc #'(lambda (string)
	      (scl:send mkrp*menu :append-item string))
	  itemlist)
    (scl:send mkrp*menu :expose)
    (scl:send mkrp*menu :scroll-to 0 :absolute)
    (while (not (consp (second (setq mkrp*select (scl:send mkrp*menu :any-tyi))))))
    ;(scl:send mkrp*menu :deselect)
    ;(scl:send mkrp*menu :deexpose)
    ;(scl:send mkrp*menu :deactivate)
    (scl:send ex.window :select)
    (third (second mkrp*select))))


(defun mkrp-menu.scroll (item.list cl.flag)
						; Edited:  10-MAR-1990 22:12
						; Authors: PRCKLN
						; Input:   A list of strings or pairs (STRING STRING . VALUE)
						;          and a flag indicating
						;          whether only pure common lisp is available.
						; Effect:  Prints list of strings on screen
						; Value:   One of the strings or if VALUE is given VALUE,
						;          selected by the user.
  (if cl.flag
      (do ((items item.list (rest items))
	   (n 0 (+ 1 n)))
	  ((null items)
	   (format *standard-output* "~%Enter number: ")
	   (let ((res (read *standard-input*)))
	     (if (and (integerp res) (> res -1))
		 (let ((res1 (nth res item.list)))
		   (if (stringp res1)
		       res1
		       (rest (rest res1)))))))
	(format *standard-output* "~%~3D:  ~5A ~A" n
		(if (stringp (first items)) (first items) (first (first items)))
		(if (stringp (first items)) "" (second (first items)))))
      #+(or symbolics explorer)
      (mkrp=menu.expose (mapcan #'(lambda (item)
				    (if (stringp item)
					(list (list "" item item))
					(if (find #\return (second item)
						  :test #'(lambda (c1 c2) (= (char-code c1) (char-code c2))))
					    (mkrp=menu.split item)
					    (list (list (first item) (second item) (rest (rest item)))))))
				item.list))))

#+(or symbolics explorer)
(defun mkrp=menu.split (item)
  (let ((string (second item))
	(list nil)
	(position nil))
    (while (setq position (position #\return string :from-end t :test #'(lambda (c1 c2) (= (char-code c1) (char-code c2)))))
      (push (subseq string (1+ position)) list)
      (setq string (subseq string 0 position)))
    (cons (list (first item) string (cddr item))
	  (mapcar #'(lambda (new)
		      (list :no new (cddr item)))
		  list))))

#+(or symbolics explorer)
(scl:defmethod (:print-item mkrp=menu) (item ignore ignore)
  (scl:send scl:self :item item :whole-item
	    #'(lambda (item window)
		(if (eq (first item) :no)
		    (format window "~10A~A" "" (second item))
		    (progn (scl:send window :item item :first-part
				     #'(lambda (item window) (format window "~10A" (first item))))
			   (format window "~A" (second item)))))))


#+(or symbolics explorer)
(scl:defmethod (:sensitive-type-p mkrp=menu) (mouse-sensitive-item)
  (case (tv:displayed-item-type mouse-sensitive-item)
      (:first-part (list 1))))

#+(or symbolics explorer)
(scl:defmethod (:who-line-documentation-string mkrp=menu) ()
  (format nil "L,M,R: Select Operation Link by Clicking on the Name of the Link"))
  ;M: Delete link from active list    R: Delete Link"))



(defun mkrp-time (time)
  (* 1000 (/ (- (get-internal-run-time) time) internal-time-units-per-second)))

(unless (and (find-symbol "IGNORE") 
	     (fboundp (find-symbol "IGNORE")))
 (defun ignore (&rest args)
   (declare (ignore args))
   nil))
