;;; -*- Syntax: Common-lisp; Mode: LISP; Package: MKRP -*-
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

(DEFVAR EDT*THEOREM.FLAG NIL)

(DEFVAR EDT*UNDO.MODE NIL)

(DEFVAR EDT*UNDO.STACK NIL)

(DEFVAR EDT*SYMBOL.TABLE.LIST NIL)

(DEFVAR EDT*SYMBOL.TABLE.UNDO.STACK NIL)

(DEFVAR EDT*SAVED.INDICATOR NIL)

(defvar EDT*SAVing.INDICATOR nil)

(DEFVAR EDT*SEPARATOR NIL)

(DEFVAR EDT*INPUT.FILES NIL)

(DEFVAR EDT*DIALOGUE.INPUT.FILE NIL)

(DEFVAR EDT*DIALOGUE.OUTPUT.FILE NIL)

(DEFVAR EDT*COMMANDS NIL)

(DEFVAR EDT*LINELENGTH 72)

(DEFVAR EDT*STANDARD.READTABLE)

(DEFVAR EDT*COMMAND.READTABLE)

(DEFVAR EDT*SEPARATOR.READTABLE)

(DEFVAR EDT*-READTABLE)

(DEFVAR EDT*NUMBER.READTABLE)

(DEFVAR EDT*NORMAL.READTABLE)



(defun edt-set.readtables nil			; all readtables are set.
  
  (setq EDT*SEPARATOR #\_)
  (setq edt*standard.readtable (copy-readtable nil))
  (set-syntax-from-char #\, #\a edt*standard.readtable *readtable*)
  (set-syntax-from-char #\` #\a edt*standard.readtable *readtable*)
  (set-syntax-from-char #\: #\a edt*standard.readtable *readtable*)
  (set-macro-character #\: #'(lambda (stream char)
			       (declare (ignore stream char))
			       '\:)
		       nil edt*standard.readtable)
  (setq edt*separator.readtable (copy-readtable nil))
  (set-macro-character edt*separator #'(lambda (stream char)
					 (declare (ignore stream char))
					 #\newline)
		       nil edt*separator.readtable)
  (setq edt*-readtable (copy-readtable edt*separator.readtable))
  (set-macro-character #\- #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*-readtable)
  (setq edt*number.readtable (copy-readtable edt*separator.readtable))
  (set-macro-character #\0 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)
  (set-macro-character #\1 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\2 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\3 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\4 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\5 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\6 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\7 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\8 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)  
  (set-macro-character #\9 #'(lambda (stream char)
			       (declare (ignore stream))
			       char)
		       nil edt*number.readtable)
  (setq edt*command.readtable (copy-readtable edt*number.readtable)) 
  (setq edt*normal.readtable (copy-readtable nil)))





(defun EDT-STANDARD-READTABLE () edt*standard.readtable)



(defun edt=read (stream readtbl)
  (let ((*readtable* readtbl)) (read stream)))


(DEFUN EDT=READP (&OPTIONAL FILE)
						; EDITED:24-AUG-83.
						; INPUT: A FILE NAME.
						; EFFECT: -
						; VALUE: true, iff there is something to be read for the editor
						;        on the editor buffer.
  (listen file))



(DEFUN EDT=FORMULA.TO.INSERT (stream)		; EDITED: 04-12-84                by cl
						; INPUT : stream from which a formula can be read
						; EFFECT: asks user to supply an input formula
						; VALUE : a list (the input formula)
  (let ((res (testeval (readline stream) t)))
    res))


(defun EDT=CHANGED.FORMULA (formula)		; EDITED: 05-DEZ-84 09:46:02      by cl
						; INPUT : a list (formula to be changed)
						; EFFECT: asks user to edit formula calling editv
						; VALUE : the changed formula (as a list)
  (if (opt-get.option gen_common.lisp)
      ()
      (let ((ed.result (edt=edit.changed formula)))
	(if (and (consp ed.result) (eq :list (first ed.result)))
	    (second ed.result)
	    ed.result))))

(defun edt=get.cl (pathname)
						; Edited:  29-MAR-1991 05:22
						; Authors: PRCKLN
						; Input:   A pathname
						; Effect:  Inserts all formulae on PATHNAME into active area if possible.
						; Value:   Some output
  (with-open-file (file pathname)
    (let ((*readtable* edt*standard.readtable)
	  (list nil)
	  elem)
      (while (neq 'edt*eof (setq elem (read file nil 'edt*eof)))
	(setq list (append (if (or (symbolp elem)
				   (and (consp elem)
					(symbolp (first elem))
					(char= #\~ (char (symbol-name (first elem)) 0))))
			       (reverse (eval elem))
			       (list elem))
			   list)))
      (mapc #'(lambda (formula) (fmt-insert (EDT=CREATE.FORMULA FORMULA NIL NIL) 1))
	    list)
      (list :list
	    (edt=outputs 'get.formulas (length list) pathname)))))


(defun EDT=get (file)				; EDITED: 17-NOV-84 09:56:13 ap
						; INPUT : a file name or nil
						; EFFECT: reads file contents into standalone editor
						;         window (or just opens it), and inserts a
						;         list of infix formulas into the editor
						; VALUE : undefined
  (if (opt-get.option gen_common.lisp)
      (edt=get.cl (mkrp-make.pathname t (mkrp-default.formulae) (mkrp-default.lisp) file))
      (let ((ed.result (edt=edit.file (mkrp-make.pathname t (mkrp-default.formulae) (mkrp-default.lisp) file))))
	(setq ed.result (nreverse (if (and (consp ed.result) (eq (first ed.result) :list))
				      (cdr ed.result)
				      (list ed.result))))
	(mapc #'(lambda (formula) (fmt-insert (EDT=CREATE.FORMULA FORMULA NIL NIL) 1))
	      ed.result)
	(list :list
	      (edt=outputs 'get.formulas (length ed.result) (mkrp-make.pathname t (mkrp-default.formulae) (mkrp-default.lisp) file))
	      (edt=shift.up (length ed.result))))))

(defparameter edt*buffer nil)


(DEFUN EDT-EDIT (THEOREM.FLAG)			; Edited:24-mar-83.
						; input: t or nil.
						; effect:if 'theorem.flag' = true the
						;        editor is called to edit a theorem. the
						;        symbols of the axioms are in the symbol-
						;        table.
						;        else the symbol.table will be reset.
						;        the editor communicates with the user until
						;        user ends the work.
						;        initializes common variables.
						; value: list of formulas in active area.
						; remark:'returnfrom' in 'edt=work'.
						;        'st-fix' in 'edt=work' puts all symbols in
						;        symbol-table.
  (SETQ EDT*INPUT.FILES (LIST (LIST *STANDARD-INPUT* 0 *STANDARD-INPUT*)))
  (SETQ EDT*DIALOGUE.INPUT.FILE *STANDARD-INPUT*)
  (SETQ EDT*DIALOGUE.OUTPUT.FILE *STANDARD-OUTPUT*)
  (setq edt*buffer nil)
  (setq EDT*COMMANDS '(INSERT DELETE
			      +SHIFT -SHIFT
			      EDIT
			      CHANGE
			      READ
			      WRITE
			      GET
			      write.get
			      EXECUTE
			      SWITCH
			      UNDO
			      REPLACE
			      PPRINT
			      SHOW
			      SSHOW
			      PREFIX
			      INFIX
			      STATE
			      OK
			      !
			      LISP
			      V
			      HELP		
			      HHELP))
  (SETQ EDT*THEOREM.FLAG THEOREM.FLAG)
  (setq EDT*UNDO.MODE 'ON)
  (setq EDT*SAVED.INDICATOR 'SAVED)
  (SETQ EDT*UNDO.STACK NIL)
  (setq EDT*SYMBOL.TABLE.LIST 'STACK1)
  (setq EDT*SYMBOL.TABLE.UNDO.STACK 'STACK2)
  (ST-CLEAR.STACK EDT*SYMBOL.TABLE.UNDO.STACK)
  (COND ((NULL THEOREM.FLAG) (ST-RESET) (COM-RESET)) (T (ST-FIX)))
  (FMT-RESET 1)
  (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
  (PROG1 (EDT=WORK) (FMT-RESET 1) (SETQ EDT*UNDO.STACK NIL)))

(defun edt=get.file.active (pathname)
  (if (edt=formulae.file.is pathname)
      (progn (edt=get.cl  pathname) (edt=shift.up (FMT-NUMBER.OF.FORMULAS 0)) nil)
      (COND
	((EQ 'ERROR (TESTEVAL '(mkrp-LOAD.FILE pathname EDT*NORMAL.READTABLE)))
	 (EDT=ERRORS 'NOT.FILE pathname)))))

(DEFUN EDT-GET.FILES (AXIOM.FILE THEOREM.FILE)
  (DECLARE (special AXIOM.FILE THEOREM.FILE))
						; EDITED:3-JUN-83.
						; INPUT: 2 FILES IN THE COMMAND-WRITE-FORMAT, WITH
						;        COMPATIBLE SYMBOL-TABLES.
						; EFFECT:LOADS FORMULAS AND SYMBOLTABLE OF
						;        'AXIOM.FILE' AFTER FMT- AND ST-RESET AND
						;        'THEOREM.FILE' AFTER ANOTHER FMT-RESET. THEN
						;        ST-FIX IS MADE.
						; VALUE: LIST       , WITH CAR IS A LIST OF THE
						;        FORMULAS FROM 'AXIOM.FILE' AND CADR IS A LIST
						;        OF THE FORMULAS FROM 'THEOREM.FILE'.
						;        ERRORMESSAGE IF FILE DOESN'T EXIST.          
  (PROG (RESULT
	 (ax.pathname (mkrp-make.pathname t (mkrp-default.ax) (mkrp-default.lisp) axiom.FILE))
	 (th.pathname (mkrp-make.pathname t (mkrp-default.th) (mkrp-default.lisp) theorem.FILE)))
	(setq EDT*SYMBOL.TABLE.LIST 'STACK1)
	(setq EDT*SYMBOL.TABLE.UNDO.STACK 'STACK2)
	(ST-CLEAR.STACK EDT*SYMBOL.TABLE.UNDO.STACK)
	(FMT-RESET 1)
	(ST-RESET)
	(com-reset)
	(FMT-SET.LOAD.FLAG T)
	(edt=get.file.active ax.pathname)
	(SETQ RESULT (EDT=GET.ACTIVE.FORMULAS NIL))
	(FMT-RESET 1)
	(edt=get.file.active th.pathname)
	(SETQ RESULT (LIST RESULT (EDT=GET.ACTIVE.FORMULAS NIL)))
	(ST-FIX)
	(RETURN RESULT)))

(DEFUN EDT=WORK NIL
						; EDITED:29-MAR-83.
						; INPUT:  -
						; EFFECT:LOOP    TO      READ AND EXECUTE COMMANDS.
						;        AFTER EACH COMMAND ERROR- OR READY-MESSAGE IS
						;        PRINTED ON EDT DIALOGUE OUTPUT FILE. VALUE OF
						;        'EDT=EXECUTE.COMMAND' = ATOM 'PROVE.OR.EDIT'
						;        OR 'ATP.TOP.LEVEL' TERMINATES THE LOOP.
						; VALUE: A LIST OF THE FORMULAS OF ACTIVE AREA.
						; REMARK:IN THIS FUNCTION IS A 'RETURNFROM' = (@).
  (let ((RESULT 'NO.RESULT) (NOT.END T))
    (WHILE NOT.END
      (fresh-line EDT*DIALOGUE.OUTPUT.FILE)
      (PRINc "> "  EDT*DIALOGUE.OUTPUT.FILE)
      (SETQ RESULT (EDT=EXECUTE.COMMAND (EDT=READ.COMMAND)))
      (CASE RESULT
	(PROVE.OR.EDIT (SETQ NOT.END NIL)	; (A) BACK TO THE CALLING FUNCTION.
		       (EDT=WRITELN (EDT=OUTPUTS 'LEAVE) EDT*DIALOGUE.OUTPUT.FILE T)
		       (SETQ RESULT (EDT=GET.ACTIVE.FORMULAS NIL)))
	(ATP.TOP.LEVEL (SETQ NOT.END NIL)	; (B) GO TO ATP-TOP-LEVEL.
		       (throw 'command nil))
	((NIL)         (EDT=COMMAND.COUNTER))	; (C) WORK OF EDITOR GOES ON.
	(OTHERWISE     (EDT=COMMAND.COUNTER)	; (D) PRINT OF READY-MESSAGE OF EXECUTED COMMAND AND
						;     WORK GOES ON.
		       
		       (mapc #'(lambda (result) (EDT=WRITELN RESULT EDT*DIALOGUE.OUTPUT.FILE T))
			     (if (and (consp result) (eq (car result) :list)) (cdr result) (list result))))))
    RESULT))

(DEFUN EDT=GET.ACTIVE.FORMULAS (COPY.FLAG)
						; EDITED:3-JUN-83 16:21:58
						; INPUT: T OR NIL.
						; EFFECT:COPIES ALL FORMULAS OF ACTIVE AREA IF
						;        COPY.FLAG IS T.
						; VALUE: LIST OF THESE COPIES OR LIST OF THE
						;        ORIGINAL FORMULAS IF FLAG IS NIL.            
  (PROG ((RESULT NIL))
	(DODOWN (RPTN (FMT-NUMBER.OF.FORMULAS 1))
	  (SETQ RESULT
		(CONS (COND (COPY.FLAG (COPY-TREE (FMT-FORMULA (1+ RPTN) 1))) (T (FMT-FORMULA (1+ RPTN) 1))) RESULT)))
	(RETURN RESULT)))

(DEFUN EDT=NORM.COMMAND.NAME (NAME)
						; EDITED:28-APR-83.
						; INPUT: ATOM.
						; EFFECT: -
						; VALUE: IF ATOM IS AN ABBREVIATION OF A COMMAND THE
						;        WHOLE COMMAND-NAME, ELSE ATOM.               
  (CASE NAME
    (I 'INSERT)
    (D 'DELETE)
    (+ '+SHIFT)
    (++ '++SHIFT)
    (- '-SHIFT)
    (-- '--SHIFT)
    (E 'EDIT)
    (C 'CHANGE)
    (R 'READ)
    (W 'WRITE)
    (G 'GET)
    (wg 'write.get)
    (EXEC 'EXECUTE)
    (SW 'SWITCH)
    (U 'UNDO)
    ((REP X) 'REPLACE)
    (PP 'PPRINT)
    (S 'SHOW)
    (SS 'SSHOW)
    ((PRE F) 'PREFIX)
    ((L LIST IN) 'INFIX)
    (ST 'STATE)
    (H 'HELP)
    (HH 'HHELP)
    (OTHERWISE NAME)))

(DEFUN EDT=READ.COMMAND NIL			; EDITED:24-MAR-83.
									      ; INPUT:  -
									      ; EFFECT:READS FROM THE COMMAND INPUT FILE.
									      ;        ASKS FOR INPUT ON THE DIALOGUE INPUT FILE.
									      ; VALUE: ERRORMESSAGE (COMMAND DOESN'T EXIST),
									      ;        READY-MESSAGE (NOT EXECUTED BECAUSE OF !),
									      ;        OR A LIST OF THE COMMAND AND ITS ARGUMENTS. 
  (let ((NUMBER 0) COMMAND ARGUMENTS
	(buffer (edt=read.command.line (EDT=INPUT.FILE))))
    (with-input-from-string (stream buffer)
      (SETQ COMMAND (EDT=GET.COMMAND stream))
      (SETQ COMMAND
	    (CASE COMMAND
	      ((DELETE INFIX PREFIX)
									      ; COMMANDS OF THE FORM 'COMMAND [<FROM>][-][<TO>]'.
	       (SETQ ARGUMENTS (EDT=READLN STREAM EDT*-READTABLE))
	       (COND ((NULL (CAR ARGUMENTS)) (CONS COMMAND (CONS T (CONS T ARGUMENTS))))
		     ((AND (MEMBER (CAR ARGUMENTS) '(#\-)) (INTEGERP (SECOND ARGUMENTS)))
		      (CONS COMMAND (CONS NIL (CDR ARGUMENTS))))
		     ((MEMBER (CAR ARGUMENTS) '(#\-)) (CONS COMMAND (CONS NIL (CONS NIL (CDR ARGUMENTS)))))
		     ((AND (MEMBER (SECOND ARGUMENTS) '(#\-)) (INTEGERP (THIRD ARGUMENTS)))
		      (CONS COMMAND (CONS (CAR ARGUMENTS) (CDDR ARGUMENTS))))
		     ((MEMBER (SECOND ARGUMENTS) '(#\-)) (CONS COMMAND (CONS (CAR ARGUMENTS) (CONS NIL (CDDR ARGUMENTS)))))
		     (T (CONS COMMAND (CONS (CAR ARGUMENTS) ARGUMENTS)))))
	      (-shift							      ;-SHIFT AND --SHIFT IN ALL FASHIONS.
		(SETQ ARGUMENTS (EDT=READLN STREAM EDT*NUMBER.READTABLE))
		(CASE (CAR ARGUMENTS)
		  ((- -SHIFT) (CONS '--SHIFT (CONS T (CDR ARGUMENTS))))
		  (SHIFT (COND ((CDR ARGUMENTS)
				(WHILE (INTEGERP (SECOND ARGUMENTS)) (SETQ NUMBER (+ (* 10 NUMBER) (SECOND ARGUMENTS)))
				       (SETQ ARGUMENTS (CDR ARGUMENTS)))
				(CONS '-SHIFT (CONS NUMBER (CDR ARGUMENTS))))
			       (T '(-SHIFT NIL))))
		  ((NIL) '(-SHIFT nil))
		  (OTHERWISE (WHILE (member (car arguments) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
			       (SETQ NUMBER (+ (* 10 NUMBER) (parse-integer (princ-to-string (CAR ARGUMENTS)))))
			       (SETQ ARGUMENTS (CDR ARGUMENTS)))
			     (CONS '-SHIFT (CONS NUMBER ARGUMENTS)))))
	      (--SHIFT							      ;-SHIFT AND --SHIFT IN ALL FASHIONS.
		(SETQ ARGUMENTS (EDT=READLN STREAM EDT*NUMBER.READTABLE))
		(CONS '-SHIFT (CONS T (CDR ARGUMENTS))))
	      ((L/ LIST/ IN/ INFIX/ L- LIST- IN- INFIX-)
									      ;COMMANDS OF THE FORM 'COMMAND-', WRITTEN WITHOUT
									      ;BLANK AND WITHOUT COMMANDS S- AND SS-.
	       (NCONC (list 'INFIX nil) (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
	      ((F/ PRE/ PREFIX/ F- PRE- PREFIX-)
	       (NCONC (list 'PREFIX nil) (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
	      ((D/ DELETE/ D- DELETE-) (NCONC (list 'DELETE nil) (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
	      (INSERT							      ;INSERT WITH REQUEST.
		(CONS COMMAND (EDT=FORMULA.TO.INSERT stream)))
	      ((S/ S- SHOW/ SHOW-)					      ;SHOW- AND SSHOW-.
	       (NCONC (list 'SHOW '/) (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
	      ((SS/ SS- SSHOW/ SSHOW-)
	       (NCONC (list 'SSHOW '/) (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
	      ((SSHOW SHOW) ;COMMANDS WITH REQUEST FOR SYMBOLS.
	       (SETQ ARGUMENTS (EDT=READLN STREAM EDT*separator.READTABLE))
	       (unless arguments (EDT=WRITELN (list "SHOW commands only with arguments") EDT*DIALOGUE.OUTPUT.FILE T))
	       (CONS COMMAND arguments))
	      (REPLACE (SETQ ARGUMENTS (EDT=ASK.READLN 'SYMBOLS? EDT*SEPARATOR.READTABLE))
		       (CONS COMMAND
			     (COND
			       ((EQL 1 (LIST-LENGTH ARGUMENTS))
				(RPLACD ARGUMENTS (EDT=ASK.READLN 'NEW? EDT*SEPARATOR.READTABLE)))
			       (T ARGUMENTS))))
	      ((GET READ NOTCOMPILEREAD WRITE PPRINT HHELP)
									      ;COMMANDS WITH FILES.
	       (CONS COMMAND (EDT=ASK.READLN 'FILE? EDT*SEPARATOR.READTABLE stream)))
	      ((write.get)
	       (CONS COMMAND (EDT=ASK.READLN 'FILE? EDT*SEPARATOR.READTABLE stream)))
	      ((EXECUTE)
									      ;COMMANDS WITH FILES.
	       (CONS COMMAND (EDT=ASK.READLN 'execute EDT*SEPARATOR.READTABLE stream)))
	      ((EDIT CHANGE V SWITCH +SHIFT HELP STATE UNDO)		      ;COMMANDS WITH >= 0 INPUTS.
	       (CONS COMMAND (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
	      ((++SHIFT OK ! LISP)					      ;COMMANDS WITHOUT INPUTS.
	       (LIST COMMAND T))
	      (OTHERWISE
		(let ((NUMBER 0) (CODE-CHARS (princ-to-string COMMAND)))
									      ;OTHERWISE WRITTEN COMMANDS.
		  (COND
		    ((and (string/= "" code-chars) (char= #\+ (elt CODE-CHARS 0)))    ;+SHIFT.
		     (NCONC (LIST '+SHIFT (intern (subseq CODE-CHARS 1) (find-package "MKRP")))
			    (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)))
		    ((INTEGERP COMMAND)					      ;+SHIFT AS 12 OR +3.
		     (SETQ ARGUMENTS (CONS COMMAND (EDT=READLN STREAM EDT*NUMBER.READTABLE)))
		     (WHILE (INTEGERP (CAR ARGUMENTS)) (SETQ NUMBER (+ (* 10 NUMBER) (CAR ARGUMENTS)))
			    (SETQ ARGUMENTS (CDR ARGUMENTS)))
		     (CONS '+SHIFT (CONS NUMBER ARGUMENTS)))
		    ((AND (atom COMMAND) (COM-EXPRESSION.PREFIX COMMAND))
									      ;INSERT WITHOUT 'I'.
		     (CONS 'INSERT (CONS COMMAND (EDT=READLN STREAM EDT*SEPARATOR.READTABLE))))
		    (T							      ;ELSE ERROR.
		     (EDT=READLN STREAM EDT*SEPARATOR.READTABLE)
		     (EDT=ERRORS 'COMMAND command))))))))
    (COND ((OR (NOT (MEMBER '! (CDR COMMAND))) (EQUAL COMMAND '(HELP !))) COMMAND)
	  (T (EDT=OUTPUTS 'NOT.EXECUTED (CAR COMMAND))))))

(DEFUN EDT=IGNORE.COMMAND NIL
						; EDITED: 19-AUG-83 14:16:21
						; INPUT:  -
						; EFFECT: THE NEXT COMMAND ON THE INPUT FILE OF THE
						;         EDITOR WILL BE IGNORED, E.G. CONTENTS OF
						;         INPUT BUFFER WILL BE READ UNTIL
						;         EDT*SEPARATOR OF THE NEXT COMMAND.
						; VALUE:  UNDEFINED.                                  
  (let ((INP (COND ((EDT=READP (EDT=INPUT.FILE)) (EDT=READ (EDT=INPUT.FILE) EDT*SEPARATOR.READTABLE))
		   (T EDT*SEPARATOR))))
    (WHILE (EQ INP EDT*SEPARATOR)
      (SETQ INP (COND ((EDT=READP (EDT=INPUT.FILE)) (EDT=READ (EDT=INPUT.FILE) EDT*SEPARATOR.READTABLE))
		      (T EDT*SEPARATOR))))
    (WHILE (NEQ INP EDT*SEPARATOR)
      (SETQ INP (COND ((EDT=READP (EDT=INPUT.FILE)) (EDT=READ (EDT=INPUT.FILE) EDT*SEPARATOR.READTABLE)))))))

(DEFUN EDT=WRITELN (PRINT.LIST FILE END.OF.LINE.FLAG)
						; EDITED:21-MAR-83.
						; INPUT: A LIST OF S-EXPRESSIONS AND AN OPEN FILE.
						;        'END.OF.LINE.FLAG' IS T OR NIL.
						; EFFECT:THE PRINTSTRINGS (PRIN1) OF THE TOP-LEVEL-
						;        ELEMENTS OF 'PRINT.LIST' ARE PRINTED AS
						;        RECORDS OF A LENGTH OF (LINELENGTH)
						;        CODE-CHARS ON FILE 'FILE'. IF
						;        'END.OF.LINE.FLAG' = T THE LAST LINE HAS ONLY
						;        NCHARS(LINE) CODE-CHARS.
						; VALUE: NIL.                                         
  (WHILE PRINT.LIST
    (fresh-line file)
    (PRINc (CAR PRINT.LIST) FILE)
    (SETQ PRINT.LIST (CDR PRINT.LIST))
    (COND ((OR PRINT.LIST END.OF.LINE.FLAG) (terpri file))))
  NIL)

(DEFUN EDT=READLN (STREAM READTBLE)
						; EDITED:21-APR-83.
						; INPUT: 'FILE' IS A FILE OPENED FOR INPUT.
						; EFFECT:READS ALL S-EXPRESSIONS OF INPUT-BUFFER,
						;        UNTIL IT'S EMPTY OR THE LAST S-EXPRESSION
						;        WAS EQUAL EDT*SEPARATOR.
						; VALUE: LIST OF INPUTS.                              
  (let ((*readtable* readtble))
    (COND
      ((EDT=READP STREAM)
       (let ((WORD (READ STREAM nil :eof)))
	 (COND ((NEQ WORD EDT*SEPARATOR) (CONS WORD (EDT=READLN STREAM READTBLE)))
	       (T NIL))))
      (T NIL))))


(defun edt=read.command.line (stream)
  (cond (edt*buffer (pop edt*buffer))
	(t (setq edt*buffer (mkrp-read.lines `(,edt*separator) (read-line stream nil :eof)))
	   (pop edt*buffer))))

(DEFUN EDT=ASK.READLN (QUESTION READTBLE &optional (stream (EDT=INPUT.FILE)))
						      ; EDITED:24-AUG-83.
						      ; INPUT: 'QUESTION' IS AN ATOM TO SELECT AN OUTPUT TO
						      ;       ASK THE USER FOR INPUTS.
						      ; EFFECT:IF THERE IS NOT INPUT, E.G. INPUT BUFFER IS
						      ;        EMPTY IF INPUT FILE IS TERMINAL, USER WILL BE
						      ;        ASKED. THEN TERMINAL WAITS FOR INPUT.
						      ;        ELSE  IF THERE IS AN INPUT, IT WILL BE READ.
  (if (EDT=READP stream)
      (EDT=READLN stream READTBLE)
      (if (member question '(save execute))
	  (progn (EDT=WRITELN (EDT=OUTPUTS (if (eq question 'save) 'save 'file?))
			      EDT*DIALOGUE.OUTPUT.FILE NIL)
		 (CONS (princ-to-string (EDT=READ EDT*DIALOGUE.INPUT.FILE READTBLE))
		       (EDT=READLN EDT*DIALOGUE.INPUT.FILE READTBLE)))
	  (list nil))))

(defun EDT=EXECUTE.COMMAND (COMMAND)
						; EDITED:25-MAR-83.
						; INPUT: LIST.
						; EFFECT:EXECUTES COMMAND (CAR LIST) IF IT EXIST.
						; VALUE: READY- OR ERROR-MESSAGE.                     
  (CASE (CAR COMMAND)
    (INSERT           (EDT=INSERT (CDR COMMAND)))
    (DELETE           (EDT=DELETE (SECOND COMMAND) (THIRD COMMAND)))
    ((+SHIFT ++SHIFT) (EDT=SHIFT.UP (SECOND COMMAND)))
    ((-SHIFT --SHIFT) (EDT=SHIFT.DOWN (SECOND COMMAND)))
    (EDIT             (EDT=EDIT (SECOND COMMAND)))
    (CHANGE           (EDT=CHANGE (SECOND COMMAND)))
    (READ             (EDT=READ* (SECOND COMMAND)))
    (WRITE            (EDT=WRITE (SECOND COMMAND)))
    (WRITE.get        (EDT=WRITE.get (SECOND COMMAND)))
    (GET              (EDT=GET (SECOND COMMAND)))
    (EXECUTE          (EDT=EXECUTE (SECOND COMMAND)))
    (SWITCH           (EDT=SWITCH (SECOND COMMAND) (THIRD COMMAND)))
    (UNDO             (EDT=UNDO (SECOND COMMAND)))
    (REPLACE          (EDT=REPLACE (SECOND COMMAND) (THIRD COMMAND)))
    (PPRINT           (EDT=PPRINT (SECOND COMMAND)))
    (SHOW             (EDT=SHOW (CDR COMMAND) NIL EDT*DIALOGUE.OUTPUT.FILE))
    (SSHOW            (EDT=SHOW (CDR COMMAND) T EDT*DIALOGUE.OUTPUT.FILE))
    (PREFIX           (EDT=PREFIX (SECOND COMMAND) (THIRD COMMAND)))
    (INFIX            (EDT=INFIX (SECOND COMMAND) (THIRD COMMAND)))
    (STATE            (FMT-COMMAND 'STATE T))
    (OK               (Edt=END 'OK))
    (!                (EDT=END '!))
    (V                (EDT=V (SECOND COMMAND)))
    (LISP             (ENTERLISP 'OK))
    (HELP             (EDT=HELP (SECOND COMMAND)))
    (HHELP            (EDT=HHELP (SECOND COMMAND)))
    (OTHERWISE COMMAND)))

(DEFUN EDT=INSERT (FORMULA) 
  (if (or formula (opt-get.option gen_common.lisp))
      (edt=insert.internal formula)
      (let ((ed.result (edt=edit.expression formula)))
	(setq ed.result (nreverse (if (and (consp ed.result) (eq (first ed.result) :list))
				      (cdr ed.result)
				      (list ed.result))))
	(mapc #'(lambda (formula) (fmt-insert (EDT=CREATE.FORMULA FORMULA NIL NIL) 1))
	      ed.result)
	(list :list
	      (edt=outputs 'insert.formulas (length ed.result))
	      (edt=shift.up (length ed.result))))))

(defun edt=insert.internal (formula)            ; EDITED:29-MAR-83.
						; INPUT: A S-EXPRESSION.
						; EFFECT:CALLS COMPILER FOR 'FORMULA'.
						;        IF IT IS SYNTACTICALLY AND SEMANTICALLY
						;        CORRECT, IT' S INSERTED AS LAST FORMULA IN
						;        THE ACTIVE AREA, ELSE IN THE PASSIVE.
						;        IF EDITOR WORKS IN THEOREM-MODE, TYPE- AND
						;        PROPERTY-FORMULAS ARE INSERTED IN PASSIVE
						;        AREA.
						; VALUE: READY- OR ERROR-MESSAGE.
  (let ((VALUE.OF.COMPILATION (COM-COMPILE FORMULA)))
    (COND ((OR (NULL (CDR VALUE.OF.COMPILATION))
	       (AND EDT*THEOREM.FLAG (MEMBER (CAR VALUE.OF.COMPILATION) '(TYPE PROPERTY))))
						;(A1) INSERTING IN PASSIVE AREA.
	   (FMT-INSERT (EDT=CREATE.FORMULA FORMULA NIL NIL) 1)
						;(A2) UNDO-HANDLING.
	   (EDT=PUSH.UNDO 0 0 1 'INSERT)
						;(A3) DESTRUCTIVE-FLAG.
	   (setq EDT*SAVED.INDICATOR (if (eql edt*saving.indicator 'do.not.save)
					 (progn (setq edt*saving.indicator nil)
						'saved)
					 'NOT.SAVED))
						;(A4) ERROR-MESSAGE.
	   (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
	   (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
	   (COND ((CDR VALUE.OF.COMPILATION) (EDT=ERRORS 'THEOREM.INSERT (1+ (FMT-NUMBER.OF.FORMULAS 1))))
		 (T (EDT=ERRORS 'INSERT.PASSIVE (1+ (FMT-NUMBER.OF.FORMULAS 1))))))
	  (T					;(B1) INSERTING IN ACTIVE AREA AND SYMBOL-TABLE-PUSH.
	   (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
	   (FMT-INSERT (EDT=CREATE.FORMULA FORMULA (CDR VALUE.OF.COMPILATION) (CAR VALUE.OF.COMPILATION)) 1)
	   (FMT-SHIFT 0 1)
						;(B2) UNDO-HANDLING.
	   (EDT=PUSH.UNDO 1 0 2 'INSERT)
						;(B3) DESTRUCTIVE-FLAG.
	   (setq EDT*SAVED.INDICATOR (if (eql edt*saving.indicator 'do.not.save)
					 (progn (setq edt*saving.indicator nil)
						'saved)
					 'NOT.SAVED))
						;(B4) READY-MESSAGE.
	   (EDT=OUTPUTS 'INSERT.ACTIVE (FMT-NUMBER.OF.FORMULAS 1))))))

(DEFUN EDT=DELETE (FROM TO)
						; EDITED:29-MAR-83.
						; INPUT: 2 ATOMS, INTEGER, T OR NIL.
						;        'FROM' = NIL AND 'FROM' < 1 MEANS 'FROM' = 1,
						;        'TO' = NIL AND 'TO' > NUMBER OF ALL FORMULAS
						;        MEANS 'TO' = NUMBER OF LAST FORMULA,
						;        'TO', 'FROM' = T CORRESPONDS TO 'TO', 'FROM'
						;        = NUMBER OF LAST FORMULA IN THE ACTIVE AREA.
						; EFFECT:THE FORMULAS 'FROM' - 'TO' WILL BE REMOVED.
						; VALUE: READY- OR ERROR-MESSAGE.                     
  (PROG
    ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)) (NUMBER.PASSIVE (FMT-NUMBER.OF.FORMULAS 0)) NUMBER.OF.ALL.FORMULAS)
    (SETQ NUMBER.OF.ALL.FORMULAS (+ NUMBER.ACTIVE NUMBER.PASSIVE))	;(1) COMPUTING EXISTING NUMBERS FOR INPUT NIL OR T.
						;'TO' ISN'T INTEGER, ERROR EDITOR EMPTY.
    (COND ((ZEROP NUMBER.OF.ALL.FORMULAS) (RETURN (EDT=ERRORS 'EMPTY))) ((NULL TO) (SETQ TO NUMBER.OF.ALL.FORMULAS))
	  ((AND (EQ TO T) (ZEROP NUMBER.ACTIVE)) (SETQ TO 1)) ((EQ TO T) (SETQ TO NUMBER.ACTIVE))
	  ((NOT (INTEGERP TO))
	   (RETURN (EDT=ERRORS 'NOT.NUMBER TO))))	;'FROM' ISN'T INTEGER.
    (COND ((NULL FROM) (SETQ FROM 1)) ((AND (EQ FROM T) (ZEROP NUMBER.ACTIVE)) (SETQ FROM 1)) ((EQ FROM T) (SETQ FROM NUMBER.ACTIVE))
	  ((NOT (INTEGERP FROM)) (RETURN (EDT=ERRORS 'NOT.NUMBER FROM))))	;'TO' < 'FROM' ==> EXCHANGE.
    (COND ((< TO FROM) (PROG ((HELP TO)) (SETQ TO FROM) (SETQ FROM HELP))))	;TO ISN'T EXISTING FORMULA-NUMBER ==> ERROR OR
						;BECOMES UPPER BOUND.
    (COND ((< TO 1) (RETURN (EDT=ERRORS 'NOT.EXIST 1 NUMBER.OF.ALL.FORMULAS)))
	  ((< TO NUMBER.ACTIVE) (RETURN (EDT=ERRORS 'DELETE)))
	  ((< NUMBER.OF.ALL.FORMULAS TO)
	   (SETQ TO NUMBER.OF.ALL.FORMULAS)))	;'FROM ISN'T EXISTING FORMULA.NUMBER ==> ERROR OR
						;BECOMES LOWER BOUND.
    (COND ((< NUMBER.OF.ALL.FORMULAS FROM) (RETURN (EDT=ERRORS 'NOT.EXIST 1 NUMBER.OF.ALL.FORMULAS)))
	  ((< FROM 1) (SETQ FROM 1)))
    (RETURN
      (PROG ((SHIFTS (+ 1 NUMBER.ACTIVE (- FROM)))
	     (DELETES (+ 1 TO (- FROM))))
	    (COND ((MINUSP SHIFTS) (SETQ SHIFTS 0)))	;(2) FORMULAS TO DELETE IN ACTIVE AREA ARE SHIFTED
						;    IN THE PASSIVE AREA.
	    (DODOWN (RPTN SHIFTS)
	      (PROGN (FMT-SHIFT 1 0) (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
		     (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.UNDO.STACK)))	;(3) DELETING.
	    (DODOWN (RPTN DELETES) (FMT-DELETE (+ FROM SHIFTS (- NUMBER.ACTIVE))))
						;(4) WITH 'ST-PUSH.SYMBOLTABLE' UNDO-HANDLING.
						(EDT=PUSH.UNDO 0 SHIFTS (+ SHIFTS DELETES) 'DELETE)	;(5) DESTRUCTIVE-FLAG.
						(setq EDT*SAVED.INDICATOR 'NOT.SAVED)	;(6) READY-MESSAGE.
						(RETURN (EDT=OUTPUTS (COND ((EQ FROM TO) 'DELETE.READY.1) (T 'DELETE.READY.N)) FROM TO))))))

(DEFUN EDT=SHIFT.UP (NUMBER)
						; EDITED:29-MAR-83.
						; INPUT: ATOM, NATURAL ('NUMBER' <= NUMBER OF FORMULAS
						;        IN THE PASSIVE AREA), NIL (=1), OR T (=NUMBER
						;        OF FORMULAS IN ACTIVE AREA).
						; EFFECT:SHIFTS THE FIRST 'NUMBER' FORMULAS FROM
						;        PASSIVE IN ACTIVE AREA, AS FAR AS THEY ARE
						;        SEMANTICALLY AND SYNTACTICALLY CORRECT.
						; VALUE: READY- OR ERROR-MESSAGE.                     
  (PROG ((HELP.NUMBER NUMBER) (MESSAGE NIL)
	 (NUMBER.PASSIVE (FMT-NUMBER.OF.FORMULAS 0)) SHIFTED.FORMULAS)	;(1) COMPUTING AN EXISTING NUMBER FOR T OR NIL.
	(COND
	  ((OR (AND (INTEGERP NUMBER) (< NUMBER.PASSIVE NUMBER)) (EQ NUMBER T)) (SETQ HELP.NUMBER (SETQ NUMBER NUMBER.PASSIVE)))
	  ((NULL NUMBER) (SETQ HELP.NUMBER (SETQ NUMBER 1)))
	  ((AND (INTEGERP NUMBER) (< NUMBER 1)) (SETQ HELP.NUMBER (SETQ NUMBER 0))))
	(RETURN
	  (COND
	    ((ZEROP NUMBER.PASSIVE)		;(2A) 1ST ERRORCASE: PASSIVE AREA EMPTY.
	     (EDT=ERRORS 'PASSIVE.EMPTY))
	    ((NOT (INTEGERP NUMBER))		;(2B) 2ND ERRORCASE: INPUT NOT INTEGER.
	     (EDT=ERRORS 'NOT.NUMBER NUMBER))
	    (T					;(3) SHIFTING.
						;    IF FORMULA IS NOT CORRECT, LOOP IS BROKEN.
	     (WHILE (NOT (ZEROP HELP.NUMBER))
	       (COND ((SETQ MESSAGE (EDT=SHIFT.ONE.UP)) (SETQ HELP.NUMBER 0)) (T (SETQ HELP.NUMBER (1- HELP.NUMBER)))))
						;(4) UNDO-HANDLING.
	     (EDT=PUSH.UNDO (SETQ SHIFTED.FORMULAS (- NUMBER.PASSIVE (FMT-NUMBER.OF.FORMULAS 0))) 0 (* 3 SHIFTED.FORMULAS)
			    '+SHIFT)
						;(5) DESTRUCTIVE-FLAG.
	     (setq EDT*SAVED.INDICATOR (if (eql edt*saving.indicator 'do.not.save)
					   (progn (setq edt*saving.indicator nil)
						  'saved)
					   'NOT.SAVED))
						;(6) READY- AND ERROR-MESSAGE.
	     (NCONC
	       (EDT=OUTPUTS
		 (COND ((EQl SHIFTED.FORMULAS 0) 'SHIFT.UP.READY.0) ((EQl SHIFTED.FORMULAS 1) 'SHIFT.UP.READY.1)
		       (T 'SHIFT.UP.READY.N))
		 SHIFTED.FORMULAS)
	       MESSAGE))))))

(DEFUN EDT=SHIFT.ONE.UP NIL
						; EDITED:29-MAR-83.
						; INPUT:  -
						; EFFECT:FIRST FORMULA IS SHIFTED FROM PASSIVE IN
						;        ACTIVE AREA, IF IT IS SYNTACTICALLY AND
						;        SEMANTICALLY CORRECT., INCLUDING SYMBOL-
						;        TABLE-HANDLING, AND ADDING RESULT OF THE
						;        COMPILATION TO THE FORMULA.
						; VALUE: NIL, IF FORMULA IS CORRECT, ELSE ERROR-MESS..
  (PROG ((RESULT.OF.COMPILATION (COM-COMPILE (CAR (FMT-FORMULA 1 0)))))
	(RETURN
	  (COND
	    ((NULL (CDR RESULT.OF.COMPILATION)) (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
	     (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST) (EDT=ERRORS 'FIRST.PASSIVE (1+ (FMT-NUMBER.OF.FORMULAS 1))))
	    ((AND EDT*THEOREM.FLAG (MEMBER (CAR RESULT.OF.COMPILATION) '(TYPE PROPERTY)))
	     (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST) (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
	     (EDT=ERRORS 'THEOREM.INSERT (1+ (FMT-NUMBER.OF.FORMULAS 1))))
	    (T (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
	       (FMT-INSERT
		 (EDT=CREATE.FORMULA (EDT=GET.INFIX.FORMULA (FMT-FORMULA 1 0)) (CDR RESULT.OF.COMPILATION)
				     (EDT=GET.INFIX.FORMULA RESULT.OF.COMPILATION))
		 1)
	       (FMT-DELETE 2) (FMT-SHIFT 0 1))))))

(DEFUN EDT=SHIFT.DOWN (NUMBER)
						; EDITED:29-MAR-83.
						; INPUT: ATOM, NATURAL ('NUMBER' <= NUMBER OF FORMULAS
						;        IN THE ACTIVE AREA), NIL (=1), OR T (=NUMBER
						;        OF FORMULAS IN ACTIVE AREA).
						; EFFECT:SHIFTS THE LAST 'NUMBER' FORMULAS FROM ACTIVE
						;        IN PASSIVE AREA.
						; VALUE: READY- OR ERROR-MESSAGE.                     
  (PROG ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)))	;(1) COMPUTING AN EXISTING NUMBER FOR INPUT T OR NIL.
	(COND ((OR (AND (INTEGERP NUMBER) (< NUMBER.ACTIVE NUMBER)) (EQ NUMBER T)) (SETQ NUMBER NUMBER.ACTIVE))
	      ((NULL NUMBER) (SETQ NUMBER 1)) ((AND (INTEGERP NUMBER) (< NUMBER 1)) (SETQ NUMBER 0)))
	(RETURN
	  (COND
	    ((ZEROP NUMBER.ACTIVE)		;(2A) 1ST ERRORCASE: ACTIVE AREA EMPTY.
	     (EDT=ERRORS 'ACTIVE.EMPTY))
	    ((NOT (INTEGERP NUMBER))		;(2B) 2ND ERRORCASE: INCORRECT INPUT.
	     (EDT=ERRORS 'NOT.NUMBER NUMBER))
	    (T					;(3) SHIFTING.
	     (DODOWN (RPTN NUMBER) (EDT=SHIFT.ONE.DOWN))	;(4) WITH SYMBOL TABLE PUSH IN 'EDT=SHIFT.ONE.DOWN'
						;    UNDO-HANDLING.
	     (EDT=PUSH.UNDO 0 NUMBER (* 3 NUMBER) '-SHIFT)	;(5) DESTRUCTIVE-FLAG.
	     (setq EDT*SAVED.INDICATOR 'NOT.SAVED)	;(6) READY-MESSAGE.
	     (EDT=OUTPUTS
	       (COND ((EQl NUMBER 0) 'SHIFT.UP.READY.0) ((EQl NUMBER 1) 'SHIFT.DOWN.READY.1) (T 'SHIFT.DOWN.READY.N))
	       NUMBER))))))

(DEFUN EDT=SHIFT.ONE.DOWN NIL
						; EDITED:29-MAR-83.
						; INPUT:  -
						; EFFECT:LAST FORMULA IS SHIFTED FROM THE ACTIVE TO
						;        THE PASSIVE AREA, INCLUDING SYMBOLTABLE-
						;        HANDLING, FMT-SHIFT, AND THROWING AWAY ALL
						;        COMPONENTS OF THE FORMULA EXCEPT INFIXFORM.
						; VALUE: NIL.                                         
  (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
  (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.UNDO.STACK)
  (FMT-SHIFT 1 0)
  (FMT-INSERT (EDT=CREATE.FORMULA (EDT=GET.INFIX.FORMULA (FMT-FORMULA 1 0)) NIL NIL) 1) (FMT-DELETE 2) NIL)

(DEFUN EDT=EDIT (FORMULA.NUMBER)		; EDITED:29-MAR-83.
						; INPUT: NATURAL OR NIL, 1 <= 'FORMULA.NUMBER' <= NUM=
						;        BER OF  FORMULAS IN THE EDITOR. NIL CORRES=
						;        PONDS TO THE NUMBER OF THE LAST FORMULA IN
						;        THE ACTIVE AREA.
						; EFFECT:CALLS THE LISPEDITOR FOR THE
						;        'FORMULA.NUMBER.TH FORMULA. IF THE FORMULA IS
						;        IN THE ACTIVE AREA
						;        THE POSSIBILITY THAT THERE DON' T REST ALL
						;        FORMULAS IS RESPECTED; THEY ARE SHIFTED IN
						;        THE PASSIVE AREA.
						; VALUE: ERROR- OR READY-MESSAGE.                     
  (PROG ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)) NUMBER.OF.ALL.FORMULAS SHIFTS NOT.SHIFTED.FORMULAS)
	(SETQ NUMBER.OF.ALL.FORMULAS (+ NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 0)))
	(unless FORMULA.NUMBER
	  (cond ((ZEROP NUMBER.ACTIVE) (SETQ FORMULA.NUMBER 1))
		(T (SETQ FORMULA.NUMBER NUMBER.ACTIVE))))
	(RETURN
	  (COND
	    ((ZEROP NUMBER.OF.ALL.FORMULAS)	;(2A) 1ST ERRORCASE: ACTIVE AND PASSIVE AREA EMPTY.
	     (EDT=ERRORS 'EMPTY))
	    ((NOT (INTEGERP FORMULA.NUMBER))	;(2B) 2ND ERRORCASE: INCORRECT INPUT.
	     (EDT=ERRORS 'NOT.NUMBER FORMULA.NUMBER))
	    ((OR (< FORMULA.NUMBER 1) (< NUMBER.OF.ALL.FORMULAS FORMULA.NUMBER))
	     (EDT=ERRORS 'NOT.EXIST.1 1 NUMBER.OF.ALL.FORMULAS))
	    ((< NUMBER.ACTIVE FORMULA.NUMBER)	;(A) FORMULA IN PASSIVE AREA.
						;(A3) EDITING.
	     (FMT-EDIT (- FORMULA.NUMBER NUMBER.ACTIVE))
						;(A4) UNDO-HANDLING.
	     (EDT=PUSH.UNDO 0 0 1 'EDIT)
						;(A5) DESTRUCTIVE-FLAG.
	     (setq EDT*SAVED.INDICATOR 'NOT.SAVED)
						;(A6) READY-MESSAGE.
	     (EDT=OUTPUTS 'EDITED FORMULA.NUMBER))
	    (T					;(B) FORMULA IN ACTIVE AREA.
						;(B3) SHIFTING IN THE PASSIVE AREA, EDITING, AND
						;     SHIFTING BACK AS FAR AS POSSIBLE.
	     (SETQ SHIFTS (+ NUMBER.ACTIVE 1 (- FORMULA.NUMBER))) (DODOWN (RPTN SHIFTS) (EDT=SHIFT.ONE.DOWN)) (FMT-EDIT 1)
	     (DODOWN (RPTN SHIFTS) (COND ((EDT=SHIFT.ONE.UP) (SETQ RPTN 1))))
	     (SETQ NOT.SHIFTED.FORMULAS (- NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)))
						;(B4) UNDO-HANDLING.
	     (EDT=PUSH.UNDO (- SHIFTS NOT.SHIFTED.FORMULAS) SHIFTS
			    (1+ (* 3 (+ SHIFTS SHIFTS (- NOT.SHIFTED.FORMULAS)))) 'EDIT)
						;(B5) DESTRUCTIVE-FLAG.
	     (setq EDT*SAVED.INDICATOR 'NOT.SAVED)
						;(B6) READY- AND ERROR-MESSAGE.
	     (COND ((ZEROP NOT.SHIFTED.FORMULAS) (EDT=OUTPUTS 'EDITED FORMULA.NUMBER))
		   (T
		    (NCONC (EDT=OUTPUTS 'EDITED FORMULA.NUMBER)
			   (EDT=ERRORS 'FIRST.PASSIVE (1+ (FMT-NUMBER.OF.FORMULAS 1)))))))))))

(DEFUN EDT=CHANGE (FORMULA.NUMBER)
						; EDITED:29-MAR-83.
						; INPUT: NATURAL OR NIL, 1 <= 'FORMULA.NUMBER' <= NUM=
						;        BER OF  FORMULAS IN THE EDITOR. NIL CORRES=
						;        PONDS TO THE NUMBER OF THE LAST FORMULA IN
						;        THE ACTIVE AREA.
						; EFFECT:THE 'FORMULA.NUMBER'TH FORMULA IS REPLACED BY
						;        A NEW FORMULA, ENTERED BY THE USER.
						;        IF THE FORMULA IS IN THE ACTIVE AREA
						;        THE POSSIBILITY THAT THERE DON' T REST ALL
						;        FORMULAS IS RESPECTED; THEY ARE SHIFTED IN
						;        THE PASSIVE AREA.
						; VALUE: ERROR- OR READY-MESSAGE.                     
  (let ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)) FORMULA NUMBER.OF.ALL.FORMULAS SHIFTS NOT.SHIFTED.FORMULAS)
    (SETQ NUMBER.OF.ALL.FORMULAS (+ NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 0)))
    (when (NULL FORMULA.NUMBER)			;(1) COMPUTING EXISTING NUMBER FOR INPUT NIL.
      (setq formula.number (if (ZEROP NUMBER.ACTIVE) 1 number.active)))
    (COND ((ZEROP NUMBER.OF.ALL.FORMULAS)	;(2A) 1ST ERRORCASE: ACTIVE AND PASSIVE AREA EMPTY.
	   (EDT=ERRORS 'EMPTY))
	  ((NOT (INTEGERP FORMULA.NUMBER))	;(2B) 2ND ERRORCASE: INCORRECT INPUT.
	   (EDT=ERRORS 'NOT.NUMBER FORMULA.NUMBER))
	  ((OR (< FORMULA.NUMBER 1) (< NUMBER.OF.ALL.FORMULAS FORMULA.NUMBER))
	   (EDT=ERRORS 'NOT.EXIST.1 1 NUMBER.OF.ALL.FORMULAS))
	  ((< NUMBER.ACTIVE FORMULA.NUMBER)	;(A) FORMULA IN PASSIVE AREA.
						;(A3) CHANGING.
	   (SETQ FORMULA (EDT=GET.INFIX.FORMULA (FMT-FORMULA (- FORMULA.NUMBER NUMBER.ACTIVE) 0)))
	   (SETQ FORMULA (EDT=CHANGED.FORMULA FORMULA))
	   (COND ((MEMBER '! FORMULA) (EDT=OUTPUTS 'NOT.EXECUTED 'CHANGE))
		 (T (FMT-INSERT (EDT=CREATE.FORMULA FORMULA NIL NIL) (- FORMULA.NUMBER NUMBER.ACTIVE))
		    (FMT-DELETE (1+ (- FORMULA.NUMBER NUMBER.ACTIVE)))
						;(A4) UNDO-HANDLING.
		    (EDT=PUSH.UNDO 0 0 2 'CHANGE)
						;(A5) DESTRUCTIVE-FLAG.
		    (setq EDT*SAVED.INDICATOR 'NOT.SAVED)
						;(A6) READY-MESSAGE.
		    (EDT=OUTPUTS 'CHANGED FORMULA.NUMBER))))
	  (T					;(B) FORMULA IN ACTIVE AREA.
						;(B3) SHIFTING IN THE PASSIVE AREA, CHANGING AND
						;     SHIFTING BACK AS FAR AS POSSIBLE.
	   (SETQ SHIFTS  (+ NUMBER.ACTIVE 1 (- FORMULA.NUMBER)))
	   (EDT=SHIFT.DOWN SHIFTS)
	   (SETQ FORMULA (CAR (FMT-FORMULA 1 0)))
	   (SETQ FORMULA (EDT=CHANGED.FORMULA FORMULA))
	   (COND ((MEMBER '! FORMULA)
		  (EDT=PUSH.UNDO 0 SHIFTS (* 3 SHIFTS) 'NN)
		  (EDT=UNDO 1)
		  (EDT=OUTPUTS 'NOT.EXECUTED 'CHANGE))
		 (T (FMT-INSERT (EDT=CREATE.FORMULA FORMULA NIL NIL) 1) (FMT-DELETE 2)
		    (DODOWN (RPTN SHIFTS) (COND ((EDT=SHIFT.ONE.UP) (SETQ rptn 1))))
		    (SETQ NOT.SHIFTED.FORMULAS (- NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)))
						;(B4) UNDO-HANDLING.
		    (EDT=PUSH.UNDO (- SHIFTS NOT.SHIFTED.FORMULAS) SHIFTS
				   (1+ (1+ (* 3 (+ SHIFTS SHIFTS (- NOT.SHIFTED.FORMULAS))))) 'CHANGE)
						;(B5) DESTRUCTIVE-FLAG.
		    (setq EDT*SAVED.INDICATOR 'NOT.SAVED)
						;(B6) READY- AND ERROR-MESSAGE.
		    (COND ((ZEROP NOT.SHIFTED.FORMULAS) (EDT=OUTPUTS 'CHANGED FORMULA.NUMBER))
			  (T (NCONC (EDT=OUTPUTS 'CHANGED FORMULA.NUMBER)
				    (EDT=ERRORS 'FIRST.PASSIVE (1+ (FMT-NUMBER.OF.FORMULAS 1))))))))))))

(defun edt=formulae.file.is (file)
						; Edited:  29-MAR-1991 05:13
						; Authors: PRCKLN
						; Input:   A file name
						; Effect:  -
						; Value:   True iff file is suitable for the GET command.
  (or (not (probe-file file))
      (let ((*readtable* edt*standard.readtable))
	(multiple-value-bind (res err)
	    (testeval (with-open-file (file file)
			(let ((read.res (read file nil :eof)))
			  (or (eq :eof read.res)
			      (not (eq 'cond (first read.res))))))
		      t)
	  (and res (not err))))))

(DEFUN EDT=READ* (FILE)
  (DECLARE (special file))
						; EDITED:5-MAY-83.
						; INPUT: 'FILE' IS A FILE WRITTEN BY EDT=WRITE.
						; EFFECT:LOADS SYMBOL-TABLE, IF IT'S AN INITIAL READ
						;        AND FORMULAS.
						; VALUE: READY-OR ERROR-MESSAGE.                      
  (prog ((MESSAGE NIL) NUMBER.ACTIVE.NEW NUMBER.PASSIVE.NEW (RESET? (FMT-IS.RESET))
	 (ax.th.pathname (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) FILE))
	 (*READTABLE* EDT*standard.READTABLE)
	 (NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)) (NUMBER.PASSIVE (FMT-NUMBER.OF.FORMULAS 0)))
	(cond ((MEMBER FILE (LIST EDT*DIALOGUE.INPUT.FILE))
	       (RETURN (EDT=ERRORS 'FILE)))
	      ((edt=formulae.file.is ax.th.pathname)
	       (return (edt=get ax.th.pathname)))
	      (t (FMT-SET.LOAD.FLAG (AND RESET? (NOT EDT*THEOREM.FLAG)))
		 (ST-SET.LOAD.FLAG (AND RESET? (NOT EDT*THEOREM.FLAG)))
		 (fresh-line *terminal-io*)
		 (when (second (multiple-value-list
				 (testeval (mkrp-LOAD.FILE ax.th.pathname EDT*standard.READTABLE)
					   t)))
		   (RETURN (EDT=ERRORS 'NOT.FILE FILE)))
		 (ST-SET.LOAD.FLAG T)
		 (FMT-SET.LOAD.FLAG T)
		 (when (AND EDT*THEOREM.FLAG RESET?)
		   (let ((SHIFTS (- (FMT-NUMBER.OF.FORMULAS 0) NUMBER.PASSIVE)))
		     (WHILE (NOT (ZEROP SHIFTS))
		       (if (SETQ MESSAGE (EDT=SHIFT.ONE.UP)) (SETQ SHIFTS 0)) (SETQ SHIFTS (1- SHIFTS)))))
		 (SETQ NUMBER.ACTIVE.NEW  (FMT-NUMBER.OF.FORMULAS 1)
		       NUMBER.PASSIVE.NEW (FMT-NUMBER.OF.FORMULAS 0))
		 (EDT=PUSH.UNDO (- NUMBER.ACTIVE.NEW NUMBER.ACTIVE) 0
				(if EDT*THEOREM.FLAG (1+ (* 3 (- NUMBER.ACTIVE.NEW NUMBER.ACTIVE))) 1)
				'READ)
		 (RETURN (NCONC (EDT=OUTPUTS (COND ((AND (EQ NUMBER.ACTIVE NUMBER.ACTIVE.NEW)
							 (EQ NUMBER.PASSIVE NUMBER.PASSIVE.NEW))      'READ.READY.00)
						   ((AND (EQ NUMBER.ACTIVE NUMBER.ACTIVE.NEW)
							 (EQ NUMBER.PASSIVE (1- NUMBER.PASSIVE.NEW))) 'READ.READY.01)
						   ((AND (EQ NUMBER.ACTIVE (1- NUMBER.ACTIVE.NEW))
							 (EQ NUMBER.PASSIVE NUMBER.PASSIVE.NEW))      'READ.READY.10)
						   ((AND (EQ NUMBER.ACTIVE (1- NUMBER.ACTIVE.NEW))
							 (EQ NUMBER.PASSIVE (1- NUMBER.PASSIVE.NEW))) 'READ.READY.11)
						   ((EQ NUMBER.ACTIVE.NEW  NUMBER.ACTIVE)             'READ.READY.0N)
						   ((EQ NUMBER.PASSIVE.NEW NUMBER.PASSIVE)            'READ.READY.N0)
						   ((EQ NUMBER.ACTIVE.NEW  (1+ NUMBER.ACTIVE))        'READ.READY.1N)
						   ((EQ NUMBER.PASSIVE.NEW (1+ NUMBER.PASSIVE))       'READ.READY.N1)
						   (T                                                 'READ.READY.NN))
					     (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) FILE)
					     (- NUMBER.ACTIVE.NEW NUMBER.ACTIVE) (- NUMBER.PASSIVE.NEW NUMBER.PASSIVE)
					     (1+ NUMBER.ACTIVE)   NUMBER.ACTIVE.NEW   (1+ NUMBER.ACTIVE.NEW)
					     (+ NUMBER.ACTIVE.NEW NUMBER.PASSIVE.NEW  (- NUMBER.PASSIVE)))
				MESSAGE))))))

(DEFUN EDT=WRITE.get (FILE)
						; EDITED: 08-Oct-91
						; INPUT:  A file.
						; EFFECT: Prints the active formulas on a file such, that they
						;         are readable by GET.
						; VALUE:  Ready- or error-message.                      
  (if (MEMBER FILE '(T))
      (EDT=ERRORS 'FILE)
      (let ((STREAM (mkrp-OPENOUT (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) file)
				  nil)))
	(dotimes (nr (fmt-number.of.formulas 1))
	  (print (edt=get.prefix.formula (FMT-FORMULA (1+ nr) 1)) stream))
	(CLOSEFILE STREAM)
	(setq EDT*SAVED.INDICATOR 'SAVED)
	(EDT=OUTPUTS 'SAVED (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) file))
	nil)))

(DEFUN EDT=WRITE (FILE)
						      ; EDITED:5-MAY-83.
						      ; INPUT: ATOM.
						      ; EFFECT:PRINTS SYMBOL-TABLE AND FORMULAS ON FILE
						      ;        'FILE' SO THAT IT CAN BE READ BY FUNCTION
						      ;        'LOAD'.
						      ; VALUE: READY-OR ERROR-MESSAGE.                      
  (if (MEMBER FILE '(T))
      (EDT=ERRORS 'FILE)
      (if (testeval (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) file))
	  (let ((STREAM (mkrp-OPENOUT (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) file)
				      nil)))
	    (PRINc "(COND (" STREAM) (FMT-WRITE STREAM)
	    (ST-SAVE STREAM NIL) (PRINc "))" STREAM)
	    (CLOSEFILE STREAM)
	    (setq EDT*SAVED.INDICATOR 'SAVED)
	    (EDT=OUTPUTS 'SAVED (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) file)))
	  (edt=errors 'not.file file))))

(DEFUN EDT=EXECUTE (FILE)			; EDITED: 18-AUG-83 20:34:58
						; INPUT:  NAME OF EXISTING SAM FILE.
						; EFFECT: THE CONTENTS OF FILE 'FILE' WILL BE
						;         INTERPRETED AS A SEQUENCE OF COMMANDS.
						;         THAT IS DONE BY SETTING 'FILE' AS INPUT FILE
						;         FOR THE EDITOR. COMMAND OK ON FILE 'FILE'
						;         CAUSES RESETTING OF INPUT FILE.
						; VALUE:  NIL OR ERROR-MESSAGE.                       
  (COND ((EDT=IS.INPUT.FILE.SEQUENCE.RECURSIVE FILE) (EDT=ERRORS 'RECURSIVE FILE))
	((EDT=SET.INPUT.FILE FILE) NIL)
	(T (EDT=ERRORS 'NOT.FILE FILE))))

(DEFUN EDT=SWITCH (FORMULA.NUMBER1 FORMULA.NUMBER2)
						; EDITED:29-MAR-83.
						; INPUT: 2 NATURALS, NUMBER OF FORMULAS IN ACTIVE AREA
						;        < 'FORMULA.NUMBER1', 'FORMULA.NUMBER2' <=
						;        NUMBER OF ALL FORMULAS IN THE EDITOR.
						;        NIL CORRESPONDS TO NUMBER OF FIRST FORMULA IN
						;        PASSIVE AREA, RESPECTIVELY NUMBER OF LAST
						;        FORMULA.
						; EFFECT:EXCHANGES THE TWO SO SPECIFIED FORMULAS.
						; VALUE: READY- OR ERROR-MESSAGE.                     
  (PROG
    ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)) (NUMBER.PASSIVE (FMT-NUMBER.OF.FORMULAS 0)) NUMBER.OF.ALL.FORMULAS)
    (SETQ NUMBER.OF.ALL.FORMULAS (+ NUMBER.ACTIVE NUMBER.PASSIVE))	;(1) COMPUTING NUMBERS FOR INPUT NIL.
    (COND ((NULL FORMULA.NUMBER2) (SETQ FORMULA.NUMBER2 (1+ NUMBER.ACTIVE))))
    (COND ((NULL FORMULA.NUMBER1) (SETQ FORMULA.NUMBER1 NUMBER.OF.ALL.FORMULAS)))
    (RETURN
      (COND
	((ZEROP NUMBER.PASSIVE)			;(2A) PASSIVE AREA EMPTY.
	 (EDT=ERRORS 'PASSIVE.EMPTY))
	((NOT (INTEGERP FORMULA.NUMBER1))	;(2B) 2ND ERRORCASE: NOT IN PASSIVE AREA.
	 (EDT=ERRORS 'NOT.NUMBER FORMULA.NUMBER1))
	((NOT (INTEGERP FORMULA.NUMBER2)) (EDT=ERRORS 'NOT.NUMBER FORMULA.NUMBER2))
	((OR (< FORMULA.NUMBER1 (1+ NUMBER.ACTIVE)) (< NUMBER.OF.ALL.FORMULAS FORMULA.NUMBER1))
	 (EDT=ERRORS 'NOT.EXIST.1 (1+ NUMBER.ACTIVE) NUMBER.OF.ALL.FORMULAS))
	((OR (< NUMBER.OF.ALL.FORMULAS FORMULA.NUMBER2) (< FORMULA.NUMBER2 (1+ NUMBER.ACTIVE)))
	 (EDT=ERRORS 'NOT.EXIST.1 (1+ NUMBER.ACTIVE) NUMBER.OF.ALL.FORMULAS))
	(T					;(3) EXCHANGING.
	 (FMT-SWITCH (- FORMULA.NUMBER1 NUMBER.ACTIVE) (- FORMULA.NUMBER2 NUMBER.ACTIVE))
						;(4) UNDO-HANDLING.                                   (EDT=PUSH.UNDO 0 0 1 'SWITCH)
						;(5) DESTRUCTIVE-FLAG.                                (setq EDT*SAVED.INDICATOR 'NOT.SAVED)
						;(6) READY-MESSAGE.
	 (EDT=OUTPUTS 'EXCHANGED FORMULA.NUMBER1 FORMULA.NUMBER2))))))

(DEFUN EDT=UNDO (SWITCH.OR.NUMBER)		; EDITED:29-MAR-83.
						; INPUT: ATOM, 'ON', 'OFF', 'EIN', 'AUS', OR NATURAL.
						; EFFECT:IF 'SWITCH' IN (OFF, AUS) UNDO-MODE IS
						;        SWITCHED OFF, IF 'SWITCH' IN (ON, EIN)
						;        UNDO-MODE IS SWITCHED ON.
						;        ELSE THE 'NUMBER.OR.SWITCH' LAST
						;        COMMANDS ON THE UNDO.STACK ARE EXECUTED.
						; VALUE: ERROR- OR READY-MESSAGE.                     
  (unless SWITCH.OR.NUMBER (SETQ SWITCH.OR.NUMBER 1))
  (CASE SWITCH.OR.NUMBER
    ((ON EIN) (setq EDT*UNDO.MODE 'ON) (EDT=OUTPUTS 'UNDO.ON))
    ((OFF AUS) (setq EDT*UNDO.MODE 'OFF) (SETQ EDT*UNDO.STACK NIL) (EDT=OUTPUTS 'UNDO.OFF))
    (OTHERWISE
      (let ((READY NIL) (END.LOOP NIL))
	(WHILE (AND (NULL END.LOOP) (NOT (ZEROP SWITCH.OR.NUMBER)))
	  (COND ((EQ EDT*UNDO.MODE 'OFF) (SETQ READY (NCONC READY (EDT=OUTPUTS 'UNDO.OFF))) (SETQ END.LOOP T))
		((NULL EDT*UNDO.STACK) (SETQ READY (NCONC READY (EDT=ERRORS 'UNDO.STACK.EMPTY))) (SETQ END.LOOP T))
		(T (SETQ READY (NCONC READY (EDT=OUTPUTS 'UNDO.END (FOURTH EDT*UNDO.STACK))))
		   (DODOWN (RPTN (CAR EDT*UNDO.STACK))
		     (PROGN (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST) (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)
			    (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)))
		   (DODOWN (RPTN (SECOND EDT*UNDO.STACK))
		     (PROGN (ST-POP.SYMBOLTABLE EDT*SYMBOL.TABLE.UNDO.STACK)
			    (ST-PUSH.SYMBOLTABLE EDT*SYMBOL.TABLE.LIST)))
		   (DODOWN (RPTN (THIRD EDT*UNDO.STACK)) (FMT-UNDO)) (SETQ EDT*UNDO.STACK (CDDDDR EDT*UNDO.STACK))
		   (SETQ SWITCH.OR.NUMBER (1- SWITCH.OR.NUMBER)))))
	READY))))

(DEFUN EDT=PUSH.UNDO
       (NUMBER.OF.THROW.AWAYS NUMBER.OF.SYMBOL.TABLE.UNDOS NUMBER.OF.FMT.UNDOS LAST.DESTRUCTIVE.COMMAND)
						; EDITED:29-MAR-83.
						; INPUT: 3 NATURALS AND AN ATOM.
						; EFFECT:PUSHES THE INPUTS IN THEIR ORDER ON
						;        UNDO-STACK.
						; VALUE: NEW UNDO-STACK.                              
  (COND
    ((EQ EDT*UNDO.MODE 'ON)
     (SETQ EDT*UNDO.STACK
	   (CONS NUMBER.OF.THROW.AWAYS
		 (CONS NUMBER.OF.SYMBOL.TABLE.UNDOS
		       (CONS NUMBER.OF.FMT.UNDOS (CONS LAST.DESTRUCTIVE.COMMAND EDT*UNDO.STACK))))))))

(DEFUN EDT=REPLACE (OLD NEW)
						; EDITED:22-APR-83.
						; INPUT: 2 ATOMS.
						; EFFECT:REPLACES SYMBOL 'OLD' BY SYMBOL 'NEW' IN
						;        SYMBOL-TABLE AND ALL FORMULAS.
						;        IF THERE ARE COMPATIBILITY-PROBLEMS,
						;        BEFORE DOING THAT, USER WILL BE ASKED IF HE
						;        WANT IT. IF 'NEW' IS IN THE SYMBOL-TABLE AND
						;        NOT COMPATIBLE WITH 'OLD' THE
						;        FORMULAS WILL BE COMPILED NEW.
						; VALUE: ERROR- OR READY-MESSAGE.                     
  (PROG
    ((NEW.NOT.NEW (MEMBER NEW (ST-ALL.SYMBOLNAMES)))
     (NEW.NOT.CONSISTANT
       (NOT
	 (AND (EQ (ST-GET.SYMBOL.CLASSIFICATION OLD 'KIND) (ST-GET.SYMBOL.CLASSIFICATION NEW 'KIND))
	      (EQ (ST-GET.SYMBOL.CLASSIFICATION OLD 'ARITY) (ST-GET.SYMBOL.CLASSIFICATION NEW 'ARITY))
	      (EQUAL (ST-GET.SYMBOL.CLASSIFICATION OLD 'DOMAIN) (ST-GET.SYMBOL.CLASSIFICATION NEW 'DOMAIN))
	      (EQ (ST-GET.SYMBOL.CLASSIFICATION OLD 'RANGE) (ST-GET.SYMBOL.CLASSIFICATION NEW 'RANGE))
	      (EQUAL (ST-GET.SYMBOL.CLASSIFICATION OLD 'SUBSORTS) (ST-GET.SYMBOL.CLASSIFICATION NEW 'SUBSORTS))
	      (EQUAL (ST-GET.SYMBOL.CLASSIFICATION OLD 'SUPERSORTS) (ST-GET.SYMBOL.CLASSIFICATION NEW 'SUPERSORTS))
	      (EQUAL (ST-GET.SYMBOL.CLASSIFICATION OLD 'ATTRIBUTE) (ST-GET.SYMBOL.CLASSIFICATION NEW 'ATTRIBUTE))
	      (EQUAL (ST-GET.SYMBOL.CLASSIFICATION OLD 'DATA) (ST-GET.SYMBOL.CLASSIFICATION NEW 'DATA)))))
     (SHIFTS 0) (APPEARANCE.NEW 0) (APPEARANCE.OLD 0) (NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)))
    (RETURN
      (COND
        ((NOT (AND (ATOM NEW) (ATOM OLD)))	;(1A) 1ST ERRORCASE: SYMBOLS NOT ATOMS.
	 (EDT=ERRORS 'SYMBOLS))
        ((OR (COM-KEYWORD NEW) (COM-KEYWORD OLD))	;(1B) 2ND ERRORCASE: KEYWORD.
	 (EDT=ERRORS 'KEYWORD))
        ((OR (EQ NEW OLD))			;(1C) NOTHING TO REPLACE.
	 (EDT=OUTPUTS 'REPLACED OLD NEW))
        (T					;(2) QUESTION TO USER, IF NEW IN SYMBOL-TABLE.
	 (COND
	   (NEW.NOT.NEW (EDT=WRITELN (EDT=OUTPUTS 'NEW.NOT.NEW NEW) EDT*DIALOGUE.OUTPUT.FILE T)
			(COND
			  (NEW.NOT.CONSISTANT
			   (WHILE (AND (< SHIFTS NUMBER.ACTIVE) (OR (< APPEARANCE.NEW 1) (< APPEARANCE.OLD 1))) (SETQ SHIFTS (1+ SHIFTS))
				  (COND
				    ((INSIDE OLD (CAR (FMT-FORMULA SHIFTS 1))) (SETQ APPEARANCE.OLD (1+ APPEARANCE.OLD))))
				  (COND
				    ((INSIDE NEW (CAR (FMT-FORMULA SHIFTS 1))) (SETQ APPEARANCE.NEW (1+ APPEARANCE.NEW)))))
			   (SETQ SHIFTS (1+ (- NUMBER.ACTIVE SHIFTS)))
			   (EDT=WRITELN (EDT=OUTPUTS 'NEW.NOT.CONSISTANT SHIFTS) EDT*DIALOGUE.OUTPUT.FILE NIL)))
			(EDT=WRITELN (EDT=OUTPUTS 'REPLACE? OLD NEW) EDT*DIALOGUE.OUTPUT.FILE T)))
	 (COND
	   ((OR (NOT NEW.NOT.NEW) (MEMBER (READ EDT*DIALOGUE.INPUT.FILE) '(Y JA YES)))
						;    REPLACE DEPENDING EVENTUALLY ON ANSWER OF USER.
						;(3) SOMETHING LIKE 'REP X Y' IN 'X,Y:Z'.
	    (COND
	      ((MEMBER (ST-GET.SYMBOL.CLASSIFICATION OLD 'KIND) '(VARIABLE SORT))
	       (DODOWN (RPTN NUMBER.ACTIVE) (EDT=REPLACE.IN.ATOMS (FMT-FORMULA RPTN 1) OLD NEW))
	       (DODOWN (RPTN (FMT-NUMBER.OF.FORMULAS 0)) (EDT=REPLACE.IN.ATOMS (FMT-FORMULA (1+ RPTN) 0) OLD NEW))))
						;(4) REPLACE.
	    (FMT-REPLACE OLD NEW 0)
	    (FMT-REPLACE OLD NEW 1) (ST-REPLACE.SYMBOL OLD NEW)
						;(5) NEW COMPILATION.
	    (COND
	      ((AND NEW.NOT.NEW NEW.NOT.CONSISTANT) (DODOWN (RPTN SHIFTS) (EDT=SHIFT.ONE.DOWN))
	       (DODOWN (RPTN SHIFTS) (COND ((EDT=SHIFT.ONE.UP) (SETQ RPTN 1))))))
						;(6) DESTRUCTIVE-FLAG.
	    (setq EDT*SAVED.INDICATOR 'NOT.SAVED)
						;(7) UNDO IMPOSSIBLE.
	    (setq EDT*UNDO.STACK NIL)
	    (ST-CLEAR.STACK EDT*SYMBOL.TABLE.UNDO.STACK)
						;(8) READY-MESSAGE.
	    (EDT=OUTPUTS 'REPLACED OLD NEW))
	   (T (EDT=OUTPUTS 'NOT.REPLACED))))))))

(DEFUN EDT=REPLACE.IN.ATOMS (FORMULA OLD NEW)
						; EDITED:22-APR-83.
						; INPUT: 'FORMULA' IS A S-EXPRESSION, 'NEW' AND 'OLD'
						;        ARE ATOMS.
						; EFFECT:IF IN 'FORMULA' APPEARS AN ATOM LIKE X,YZ:INT
						;        AND 'OLD' = YZ AND 'NEW' = ZZ, THEN THE ATOM
						;        WILL BE REPLACED BY X,ZZ:INT. THE ATOM IS NOT
						;        A KEYWORD OF THE COMPILER AND NOT A SYMBOL IN
						;        SYMBOL-TABLE.
						; VALUE: UNDEFINED.                                   
  (DODOWN (RPTN (LIST-LENGTH FORMULA))
    (COND ((CONSP (CAR FORMULA)) (EDT=REPLACE.IN.ATOMS (CAR FORMULA) OLD NEW) (SETQ FORMULA (CDR FORMULA)))
	  ((AND (NOT (ST-IS.IN.SYMBOLTABLE (CAR FORMULA))) (NOT (COM-KEYWORD (CAR FORMULA))))
	   (RPLACA FORMULA
		   (intern (COERCE (EDT=REPLACE.PART.OF.LIST (COERCE (CAR FORMULA) 'LIST) (COERCE OLD 'LIST) (COERCE NEW 'LIST))
				   'string)
			   (find-package "MKRP")))
	   (SETQ FORMULA (CDR FORMULA)))
	  (T (SETQ FORMULA (CDR FORMULA))))))

(DEFUN EDT=REPLACE.PART.OF.LIST (LIST OLD NEW)
						; EDITED:22-APR-83.
						; INPUT: 3 LISTS OF CODE-CHARS.
						; EFFECT:IF THE LIST 'OLD' IS A PART OF THE LIST
						;        'LIST' AND BEFORE AND AFTER IT ARE NIL, ',',
						;        OR ':' IT WILL BE REPLACED BY THE LIST 'NEW'.
						; VALUE: THE MODIFIED LIST.                           
  (PROG ((POINTER1 LIST) (POINTER2 LIST) (POINTER.OLD OLD) (NEW.NEW (COPY-TREE NEW)))
	;;(1) POINTER1 BECOMES FIRST APPEARANCE OF , OR :.
	(WHILE (NOT (MEMBER (CAR POINTER1) '(UNQUOTE |:| NIL))) (SETQ POINTER1 (CDR POINTER1)))
	;;(2) COMPARISON OF THE LIST UP TO , OR : WITH OLD.
	(WHILE (AND (NOT (AND (NULL POINTER2) (NULL POINTER.OLD)))
		    (EQ (CAR POINTER.OLD) (CAR POINTER2)))
	  (SETQ POINTER2 (CDR POINTER2))
	  (SETQ POINTER.OLD (CDR POINTER.OLD)))
	;;(3) PROCESSING RESULT OF COMPARISON.
	(COND
	  ((AND (MEMBER (CAR POINTER2) '(UNQUOTE |:| NIL)) (NULL POINTER.OLD))
	   ;;(3A) EQUAL.
	   ;;(3A1) THIS PART OF LIST (= OLD) IS REPLACED BY NEW.
	   (RPLACD (NTHCDR (1- (LIST-LENGTH NEW)) NEW) POINTER2)	;;(3A2) DO THE SAME FOR THE REST OF THE LIST AND
	   ;;      REPLACE IT BY THE NEW REST.
	   (COND
	     ((MEMBER (CAR POINTER2) '(UNQUOTE |:|)) (RPLACD POINTER2 (EDT=REPLACE.PART.OF.LIST (CDR POINTER2) OLD NEW.NEW))))
	   ;;(3A3) VALUE IS NEW BECAUSE OF (3A1).
	   (RETURN NEW))
	  (T	;;(3B) NOT EQUAL.
	   ;;(3B1) APPENDING THE NEW REST AS IN (3A2).
	   (COND
	     ((MEMBER (CAR POINTER1) '(UNQUOTE |:|)) (RPLACD POINTER1 (EDT=REPLACE.PART.OF.LIST (CDR POINTER1) OLD NEW))))
	   ;;(3B2) VALUE IS LIST.
	   (RETURN LIST)))))

(DEFUN EDT=PPRINT (FILE)
						; EDITED:18-APR-83.
						; INPUT: ATOM.
						; EFFECT:PRINTS THE SYMBOL-TABLE AND ALL FORMULAS ON
						;        FILE 'FILE'.
						; VALUE: NIL OR ERROR-MESSAGE.
  (COND ((MEMBER FILE '(NIL T)) (EDT=ERRORS 'FILE))
	(T (let ((STREAM (mkrp-OPENOUT (mkrp-make.pathname nil "EDTPP" "text" file) nil)))
	     (LINELENGTH EDT*LINELENGTH STREAM)
	     (EDT=WRITELN (EDT=OUTPUTS 'SPACE.ST) STREAM T)
	     (EDT=SHOW (list '/) T STREAM)
	     (EDT=WRITELN '(" " " " " ") STREAM T)
	     (FMT-COMMAND 'PP STREAM)
	     (CLOSEFILE STREAM)
	     (EDT=OUTPUTS 'PP.READY FILE)))))

(DEFUN EDT=SHOW (LIST.OF.SYMBOLS.AND.TYPES PRETTY.FLAG FILE)
						; EDITED:15-JUN-83.
						; INPUT: 'LIST.OF.SYMBOLS.AND.TYPES' = X1...XN, WHERE
						;        EACH XI IS AN ATOM OR A LIST OF ATOMS.
						;        'PRETTY.FLAG' = T OR NIL.
						;        'FILE' IS A FILE OPEN FOR OUTPUT.
						; EFFECT:PRINTS ERROR-MESSAGE, IF XI IS NOT A SYMBOL-
						;        NAME OR A LIST OF KIND-NAMES (SORT, FUNCTION,
						;        PREDICATE, CONSTANT).
						;        ELSE SYMBOL-TABLES OF THE WITH THE XI
						;        SPECIFIED SYMBOLS ARE PRINTED ONE FOR EACH
						;        KIND ON FILE 'FILE'.
						;        'PRETTY.FLAG' = NIL PRINTS TABLES WITHOUT
						;        SLASHES.
						; VALUE: NIL.                                         
  (PROG
    ((ALL.SYMBOL.NAMES (ST-ALL.SYMBOLNAMES)) (TYPE.LIST NIL) (SYMBOL.LIST NIL)
     (ALL.TYPES (list 'SORT 's 'sorte 'CONSTANT 'c 'konstante 'PREDICATE 'p 'praedikat 'function 'f 'funktion)))
    ;;SEPaRATES 'LIST.OF.SYMBOLS.AND.TYPES' IN SYMBOLS AND
    ;;TYPES. ERRORS FOR NOT EXISTING SYMBOLS.
    (WHILE LIST.OF.SYMBOLS.AND.TYPES
      (COND
	((CONSP (CAR LIST.OF.SYMBOLS.AND.TYPES)) (SETQ TYPE.LIST (NCONC (CAR LIST.OF.SYMBOLS.AND.TYPES) TYPE.LIST)))
	((MEMBER (CAR LIST.OF.SYMBOLS.AND.TYPES) '(- /)) (SETQ TYPE.LIST ALL.TYPES) (SETQ SYMBOL.LIST NIL))
	((NOT (MEMBER (CAR LIST.OF.SYMBOLS.AND.TYPES) ALL.SYMBOL.NAMES))
	 (EDT=WRITELN (EDT=OUTPUTS 'NOT.IN.ST (CAR LIST.OF.SYMBOLS.AND.TYPES)) FILE T))
	(T (SETQ SYMBOL.LIST (CONS (CAR LIST.OF.SYMBOLS.AND.TYPES) SYMBOL.LIST))))
      (SETQ LIST.OF.SYMBOLS.AND.TYPES (CDR LIST.OF.SYMBOLS.AND.TYPES)))
    (SETQ LIST.OF.SYMBOLS.AND.TYPES TYPE.LIST)
    (SETQ TYPE.LIST NIL)
    ;;ERRORS FOR NOT EXISTING TYPES.
    (WHILE LIST.OF.SYMBOLS.AND.TYPES
      (COND ((NOT (MEMBER (CAR LIST.OF.SYMBOLS.AND.TYPES) ALL.TYPES))
	     (EDT=WRITELN (EDT=OUTPUTS 'NOT.KIND (CAR LIST.OF.SYMBOLS.AND.TYPES)) FILE T))
	    (T (push (CASE (CAR LIST.OF.SYMBOLS.AND.TYPES)
		       ((S SORT SORTE) 'SORT)
		       ((P PREDICATE PRAEDIKAT) 'PREDICATE)
		       ((C K CONSTANT KONSTANTE) 'CONSTANT)
		       (OTHERWISE 'FUNCTION))
		     TYPE.LIST)))
      (pop LIST.OF.SYMBOLS.AND.TYPES))
    ;;COMPREHANDING SINGLE SYMBOLS AND SYMBOLS OF ENTERED
    ;;KINDS.
    (WHILE ALL.SYMBOL.NAMES
      (COND
	((MEMBER (ST-GET.SYMBOL.CLASSIFICATION (CAR ALL.SYMBOL.NAMES) 'KIND) TYPE.LIST)
	 (SETQ LIST.OF.SYMBOLS.AND.TYPES (CONS (CAR ALL.SYMBOL.NAMES) LIST.OF.SYMBOLS.AND.TYPES))))
      (SETQ ALL.SYMBOL.NAMES (CDR ALL.SYMBOL.NAMES)))
    (SETQ SYMBOL.LIST (UNION SYMBOL.LIST LIST.OF.SYMBOLS.AND.TYPES))
    ;;PRINTING 4 TABLES.
    (mapc #'(lambda (type)
	      (EDT=PRINT.TABLE TYPE PRETTY.FLAG SYMBOL.LIST FILE))
	  all.types)))

(DEFUN EDT=PRINT.TABLE (TABLE.SELECTOR PRETTY.FLAG LIST.OF.SYMBOLS FILE)
						; EDITED:8-APR-83.
						; INPUT: 'TABLE.SELECTOR' IS ONE OF THE ATOMS SORT,
						;        CONSTANT, FUNCTION, PREDICATE OR
						;        VARIABLE,
						;        'PRETTY.FLAG' IS T OR NIL, AND
						;        'FILE' AN OPEN FILE.
						; EFFECT:PRINTS A TABLE WITH ALL SYMBOLS OF THE KIND
						;        'TABLE.SELECTOR' WITH THEIR PROPERTIES ON THE
						;        FILE 'FILE. IF 'PRETTY.FLAG' = T SOME DASHES
						;        AND TITLES MORE ARE PRINTED.
						; VALUE: NIL.
  (let
    ((SYMBOLS&TABULATOR (EDT=SYMBOLS&TABULATOR TABLE.SELECTOR LIST.OF.SYMBOLS (1- (LINELENGTH NIL FILE)))) SYMBOLS TABULATOR)
    (SETQ SYMBOLS (CAR SYMBOLS&TABULATOR))
    (SETQ TABULATOR (CDR SYMBOLS&TABULATOR))
    (when SYMBOLS (TERPRI FILE) (EDT=WRITELN (EDT=OUTPUTS TABLE.SELECTOR 1) FILE T))
    (COND
      ((AND PRETTY.FLAG SYMBOLS)
       (EDT=WRITE.TAB (EDT=OUTPUTS TABLE.SELECTOR 2) TABULATOR T FILE)
       (EDT=PRINT.TABLE.DASH TABULATOR (LIST-LENGTH (CAR SYMBOLS)) FILE)))
    (mapc #'(lambda (symbol)
	      (EDT=WRITE.TAB SYMBOL TABULATOR PRETTY.FLAG FILE))
	  symbols)))

(DEFUN EDT=PRINT.TABLE.DASH (TABULATOR TIMES FILE)
								; EDITED:26-APR-83.
								; INPUT: 'TABULATOR' IS AN ARRAY OF NATURALS,
								;        'TIMES' A NATURAL, AND 'FILE' A FILE OPEN FOR
								;        OUTPUT. LENGTH OF 'TABULATOR' > 'TIMES.
								; EFFECT:PRINTS SOMETHING LIKE ----+--+---------
								;        WHERE THE LENGTHS ARE COMPUTED BY TABULATOR
								;        (I) - TABULATOR(I-1) - 1 (1 <= I <= TIMES +
								;        1.
								; VALUE: UNDEFINED.                                   
  (let ((left 0))
    (DODOWN (RPTN (- (AREF TABULATOR (1+ LEFT)) (AREF TABULATOR LEFT))) (PRINc "-" FILE))
    (SETQ LEFT (1+ LEFT))
    (DODOWN (RPTN (1- TIMES))
      (PRINc "+" FILE)
      (DODOWN (RPTN (- (AREF TABULATOR (1+ LEFT)) (AREF TABULATOR LEFT))) (PRINc "-" FILE))
      (SETQ LEFT (1+ LEFT)))
    (PRINc "-" FILE)
    (terpri file)))

(DEFUN EDT=WRITE.TAB (LIST.OF.OUTPUTS COLUMNS TABLE.INDICATOR FILE)
						; EDITED:8-APR-83.
						; INPUT: 'LIST.OF.OUTPUTS' IS LIST OF S-EXPRESSIONS
						;        'COLUMNS' IS AN ARRAY OF NATURALS IN
						;        ORDER, LENGTH > LENGTH OF 'LIST.OF.OUTPUTS',
						;        'TABLE.INDICATOR' IS T OR NIL. 'FILE' IS A
						;        FILE OPEN FOR OUTPUT.
						; EFFECT:PRINTS ON FILE 'FILE':
						;        (CAR 'LIST.OF.OUTPUTS') / (CADR ...) ... OR
						;        ( ...                 )   ( ...    ) ... IF
						;        'TABLE.INDICATOR' = NIL. POSITIONS OF SLASHES
						;        ARE DETERMINED BY 'LIST.OF.COLUMNS'.
						;        IF LAST POSITION IS TO BIG FOR ONE LINE THE
						;        LINE WILL BE SEPERATED IN EQUAL PARTS.
						; VALUE: UNDEFINED.                                   
  (when LIST.OF.OUTPUTS
    (do ((number 1 (1+ number))
	 (output LIST.OF.OUTPUTS (cdr output)))
	((null output))
      (format file "~va~a"
	      (- (aref columns number) (aref columns (1- number)))
	      (first output)
	      (if (and table.indicator (rest output)) "|" "")))
    (terpri file)))

(DEFUN EDT=LIST.SYMBOL.PROPERTIES (SYMBOL.NAME KIND.OF.SYMBOL)
						; EDITED:8-APR-83.
						; INPUT: 'SYMBOL.NAME' IS AN ATOM, 'KIND.OF.SYMBOL'
						;        ONE OF THE ATOMS SORT, FUNCTION, PREDICATE,
						;        OR CONSTANT.
						; EFFECT: -
						; VALUE: IF 'KIND.OF.SYMBOL' IS ONE OF THE DESCRIBED
						;        ATOMS A LIST OF THE SYMBOL NAME AND ITS
						;        PROPERTIES (ARITY, DOMAIN,...), ELSE NIL.    
  (CONS SYMBOL.NAME
	(CASE KIND.OF.SYMBOL
	  (SORT
	    (LIST (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'SUBSORTS)
		  (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'SUPERSORTS)
		  (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'ATTRIBUTE)))
	  (CONSTANT
	    (LIST (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'RANGE)
		  (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'ATTRIBUTE)))
	  (FUNCTION
	    (LIST (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'ARITY) (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'DOMAIN)
		  (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'RANGE) (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'ATTRIBUTE)))
	  (PREDICATE
	    (LIST (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'ARITY) (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'DOMAIN)
		  (ST-GET.SYMBOL.CLASSIFICATION SYMBOL.NAME 'ATTRIBUTE)))
	  (OTHERWISE NIL))))

(DEFUN EDT=SYMBOLS&TABULATOR (KIND ALL.SYMBOLS MAX.POSITION)
						      ; EDITED:8-APR-83.
						      ; INPUT: ATOM 'KIND' IS ONE OF THE ATOMS
						      ;        FUNCTION, PREDICATE, SORT, OR
						      ;        CONSTANT, 'ALL.SYMBOLS' IS A LIST OF ALL
						      ;        SYMBOLS IN THE CURRENT SYMBOL TABLE.
						      ; EFFECT: -
						      ; VALUE: DOTTED PAIR.
						      ;        CAR IS A LIST OF THE SYMBOL.PROPERTIES (
						      ;        ARITY, DOMAIN,...).
						      ;        CDR IS AN ARRAY OF THE ADEQUATE POSITIONS TO
						      ;        PRINT THE DASHES IN A TABLE.                 
  (let ((PROPERTIES (EDT=OUTPUTS KIND 2))
	NUMBER.OF.ALL.POSITIONS
	(TABULATOR (MAKE-ARRAY 6))
	(NUMBER 1)
	(KIND.OF.CANDIDATE KIND)
		(SYMBOLS NIL))
    ;;(1) INITIALIZATION.
    ;;(1A) 'PROPERTIES ARE THE NAMES OF THE PROPERTIES OF
    ;;     SYMBOLS OF THE KIND 'KIND'.
    ;;(1B) LENGTH OF PROPERTIES IS NOT KNOWN, TABULATOR=
    ;;     COLLUMNS ARE ALL 0.
    ;;(1C) 'KIND.OF.CANDIDATE' IS KIND CORRESPONDING TO
    ;;     'PROPERTIES'.
    ;;INITIALIZATION IS SO DIFFICULT BECAUSE OF THE 2
    ;;RESULTS OF THE FUNCTION.
    (DODOWN (RPTN 6) (setf (aref TABULATOR RPTN) 0))
    ;;(2) ALL SYMBOLS WILL BE EXAMINED, WETHER THEY ARE OF
    ;;    THE ASKED KIND.
    (DODOWN (RPTN (1+ (LIST-LENGTH ALL.SYMBOLS)))     ;(2A)  THE EXAMINED SYMBOL IS OF THE WISHED KIND.
      (COND
	((EQ KIND.OF.CANDIDATE KIND)		      ;(2A1) IT IS PUT IN THE LIST.
	 (SETQ SYMBOLS (CONS PROPERTIES SYMBOLS))     ;(2A2) LOOKING WETHER THE PROPERTIES ARE LONGER THAN
						      ;      THE UP TO NOW LISTED ONES.
	 (mapc #'(lambda (property)
		   (COND ((< (AREF TABULATOR NUMBER) (PRINT-LENGTH PROPERTy NIL))
			  (setf (aref TABULATOR NUMBER) (PRINT-LENGTH PROPERTy NIL))))
		   (SETQ NUMBER (1+ NUMBER)))
	       properties)))
      ;;(2B) THE PROPERTIES OF THE NEXT SYMBOL.
      (SETQ PROPERTIES (EDT=LIST.SYMBOL.PROPERTIES (CAR ALL.SYMBOLS) KIND))
      ;;(2C) THE KIND OF THE NEXT SYMBOL.
      (SETQ KIND.OF.CANDIDATE (ST-GET.SYMBOL.CLASSIFICATION (CAR ALL.SYMBOLS) 'KIND))
      ;;(2D) THE POINTER FOR LOOKING OF LENGTH OF PROPERTIES
      ;;     IS RESET.
      (SETQ NUMBER 1)
      ;;(2E) THE NEW NEXT SYMBOL.
      (SETQ ALL.SYMBOLS (CDR ALL.SYMBOLS)))
    ;;(3) VALUE.
    ;;(3A) ALL SYMBOLS FOR THE CURRENT TABLE.
    (SETQ SYMBOLS (CDR (NREVERSE SYMBOLS)))
    ;;(3B) ALL POSITIONS OF A TABLE LINE FREE FOR
    ;;     INFORMATION (NOT SLASHES).
    (SETQ NUMBER.OF.ALL.POSITIONS (- MAX.POSITION (1+ (LIST-LENGTH (CAR SYMBOLS)))))
    ;;(3C) NOT ENOUGH POSITIONS IN ONE PRINT LINE FOR ONE
    ;;     TABLE LINE ==> COMPUTING NEW WIDTHS OF TABLE
    ;;     COLUMNS.
    (COND
      ((AND SYMBOLS
	    (< NUMBER.OF.ALL.POSITIONS
	       (let ((NUMBER 0)) (DODOWN (RPTN 6) (incf NUMBER (AREF TABULATOR RPTN))) number)))
       (let ((NUMBER.TO.ADD (LIST-LENGTH (CAR SYMBOLS)))
	     (GREATEST.WIDTH 0) WIDTH.OF.COLUMNS REST ADD.TO.WIDTH
	     ADD.TO.GREATEST.WIDTH POSITION.OF.GREATEST.WIDTH)
	 ;;(3C1) INITIALIZE ALL WITH THE SAME WIDTH.
	 (multiple-value-SETQ (WIDTH.OF.COLUMNS rest) (truncate NUMBER.OF.ALL.POSITIONS (LIST-LENGTH (CAR SYMBOLS))))
	 ;;(3C2) THE COLUMNS WHICH ARE TO WIDE WILL BE REDUCED.
	 (DODOWN (RPTN 6)
	   (COND
	     ((AND (NOT (ZEROP (AREF TABULATOR RPTn))) (> WIDTH.OF.COLUMNS (AREF TABULATOR RPTN)))
	      (SETQ REST (+ REST (- WIDTH.OF.COLUMNS (AREF TABULATOR RPTN))))
	      (SETQ NUMBER.TO.ADD (1- NUMBER.TO.ADD)))
	     ((< GREATEST.WIDTH (AREF TABULATOR RPTN))
	      (SETQ POSITION.OF.GREATEST.WIDTH (1+ RPTN))
	      (SETQ GREATEST.WIDTH (AREF TABULATOR RPTN)))))
	 ;;(3C3) THE SO GAINED POSITIONS ARE DISTRIBUTED TO THE
	 ;;      COLUMNS, WHICH ARE TO SMALL.
	 (multiple-value-setq (ADD.TO.WIDTH ADD.TO.GREATEST.WIDTH) (truncate REST NUMBER.TO.ADD)) 
	 (DODOWN (RPTN 6)
	   (COND
	     ((EQ RPTN POSITION.OF.GREATEST.WIDTH)
	      (setf (aref TABULATOR RPTN) (+ ADD.TO.GREATEST.WIDTH WIDTH.OF.COLUMNS ADD.TO.WIDTH)))
	     ((< WIDTH.OF.COLUMNS (AREF TABULATOR RPTN))
	      (setf (aref TABULATOR RPTN) (+ ADD.TO.WIDTH WIDTH.OF.COLUMNS))))))))
    ;;(3D) COMPUTING POSITIONS OF TABLE SLASHES.
    (setf (aref tabulator 0) 1)
    (dotimes (index 5)
      (setf (aref TABULATOR (1+ index)) (+ 1 (AREF TABULATOR index) (AREF TABULATOR (1+ index)))))
    (setf (aref tabulator 0) 0)
    (CONS SYMBOLS TABULATOR)))

(DEFUN EDT=PREFIX (FROM TO)
						; EDITED:29-MAR-83.
						; INPUT: 2 ATOMS, NIL, T OR INTEGER.
						;        'FROM' = NIL AND 'FROM' < 1 MEANS 'FROM' = 1,
						;        'TO' = T, 'TO' = NIL OR TO > NUMBER OF
						;        FORMULAS IN ACTIVE AREA CORRESPONDS TO 'TO' =
						;        NUMBER OF LAST FORMULA IN ACTIVE AREA,
						;        'FROM' = T MEANS 'FROM' = NUMBER OF LAST
						;        FORMULA IN ACTIVE AREA.
						; EFFECT:PRINTS THE PREFIXFORM OF FORMULAS 'FROM' -
						;        ON PRIMARY OUTPUT FILE.
						; VALUE: NIL OR ERROR-MESSAGE.                        
  (PROG ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)))
	(COND ((ZEROP NUMBER.ACTIVE) (RETURN (EDT=ERRORS 'ACTIVE.EMPTY))) ((NULL TO) (SETQ TO NUMBER.ACTIVE))
	      ((EQ TO T) (SETQ TO NUMBER.ACTIVE))
	      ((NOT (INTEGERP TO))
	       (RETURN (EDT=ERRORS 'NOT.NUMBER TO))))	;'FROM' ISN'T INTEGER.
	(COND ((NULL FROM) (SETQ FROM 1)) ((EQ FROM T) (SETQ FROM NUMBER.ACTIVE))
	      ((NOT (INTEGERP FROM))
	       (RETURN (EDT=ERRORS 'NOT.NUMBER FROM))))	;'TO' < 'FROM' ==> EXCHANGE.
	(COND ((< TO FROM)
	       (let ((HELP TO))
		 (SETQ TO FROM)
		 (SETQ FROM HELP))))		;TO ISN'T EXISTING FORMULA-NUMBER ==> ERROR OR
						;BECOMES UPPER BOUND.
	(COND ((< TO 1) (RETURN (EDT=ERRORS 'NOT.EXIST 1 NUMBER.ACTIVE)))
	      ((< NUMBER.ACTIVE TO)
	       (SETQ TO NUMBER.ACTIVE)))	;'FROM ISN'T EXISTING FORMULA.NUMBER ==> ERROR OR
						;BECOMES LOWER BOUND.
	(COND ((< NUMBER.ACTIVE FROM) (RETURN (EDT=ERRORS 'NOT.EXIST 1 NUMBER.ACTIVE)))
	      ((< FROM 1) (SETQ FROM 1)))	;EXECUTION WITH FMT.
	(RETURN (FMT-COMMAND 'PRINT.FORM 'PREFIX FROM TO))))

(DEFUN EDT=INFIX (FROM TO)
						; EDITED:29-MAR-83.
						; INPUT: 2 ATOMS, INTEGER, T OR NIL.
						;        'FROM' = NIL AND 'FROM' < 1 MEANS 'FROM' = 1,
						;        'TO' = NIL AND 'TO' > NUMBER OF ALL FORMULAS
						;        MEANS 'TO' = NUMBER OF LAST FORMULA,
						;        'TO', 'FROM' = T CORRESPONDS TO 'TO', 'FROM'
						;        = NUMBER OF LAST FORMULA IN THE ACTIVE AREA.
						; EFFECT:PRINTS THE INFIXFORM OF THE FORMULAS 'FROM' -
						;        'TO' ON PRIMARY OUTPUT FILE.
						; VALUE: NIL OR ERROR-MESSAGE.                        
  (PROG ((NUMBER.ACTIVE (FMT-NUMBER.OF.FORMULAS 1)) NUMBER.OF.ALL.FORMULAS)
	(SETQ NUMBER.OF.ALL.FORMULAS (+ (FMT-NUMBER.OF.FORMULAS 1) (FMT-NUMBER.OF.FORMULAS 0)))
	(COND ((ZEROP NUMBER.OF.ALL.FORMULAS) (RETURN (EDT=ERRORS 'EMPTY))) ((NULL TO) (SETQ TO NUMBER.OF.ALL.FORMULAS))
	      ((AND (EQ TO T) (ZEROP NUMBER.ACTIVE)) (SETQ TO 1)) ((EQ TO T) (SETQ TO NUMBER.ACTIVE))
	      ((NOT (INTEGERP TO)) (RETURN (EDT=ERRORS 'NOT.NUMBER TO))))	;'FROM' ISN'T INTEGER.
	(COND ((NULL FROM) (SETQ FROM 1))
	      ((AND (EQ FROM T) (ZEROP NUMBER.ACTIVE)) (SETQ FROM 1))
	      ((EQ FROM T) (SETQ FROM NUMBER.ACTIVE))
	      ((NOT (INTEGERP FROM))
	       (RETURN (EDT=ERRORS 'NOT.NUMBER FROM))))	;'TO' < 'FROM' ==> EXCHANGE.
	(COND ((< TO FROM)
	       (let ((HELP TO))
		 (SETQ TO FROM)
		 (SETQ FROM HELP))))		;TO ISN'T EXISTING FORMULA-NUMBER ==> ERROR OR
						;BECOMES UPPER BOUND.
	(COND ((< TO 1) (RETURN (EDT=ERRORS 'NOT.EXIST 1 NUMBER.OF.ALL.FORMULAS)))
	      ((< NUMBER.OF.ALL.FORMULAS TO)
	       (SETQ TO NUMBER.OF.ALL.FORMULAS)))	;'FROM ISN'T EXISTING FORMULA.NUMBER ==> ERROR OR
						;BECOMES LOWER BOUND.
	(COND ((< NUMBER.OF.ALL.FORMULAS FROM) (RETURN (EDT=ERRORS 'NOT.EXIST 1 NUMBER.OF.ALL.FORMULAS)))
	      ((< FROM 1) (SETQ FROM 1)))	;EXECUTION WITH FMT.
	(RETURN (FMT-COMMAND 'PRINT.FORM 'INFIX FROM TO EDT*DIALOGUE.OUTPUT.FILE))))

(DEFUN EDT=END (KIND.OF.END)
						; edited:25-mar-83.
						; input: 'kind.of.end' is one of the atoms '!' or 'ok'
						; effect:the user will be asked if he really want to
						;        terminate the editor, if 'edt*saved.indica=
						;        tor' = 'not.saved'.
						; value: if he answers 'yes' or 'edt*saved.indicator'
						;        = 'saved', value of the function becomes atom
						;        'prove.or.edit' ('kind.of.end' = ok) or
						;        'atp.top.level' ('kind.of.end' =]), else nil.
  (let ((WISH.TO.END 'YES))			;LEAVING THE EXECUTE COMMAND.
    (when (AND (NOT (EDT=IS.FIRST.INPUT.FILE))
	       (EQ KIND.OF.END 'OK))
      (SETQ KIND.OF.END NIL))
    (when (AND (EDT=IS.FIRST.INPUT.FILE) (EQ EDT*SAVED.INDICATOR 'NOT.SAVED))
      (SETQ WISH.TO.END (intern (princ-to-string (CAR (EDT=ASK.READLN 'SAVE EDT*STANDARD.READTABLE *terminal-io*)))
				(find-package "MKRP"))))
    (unless (EDT=IS.FIRST.INPUT.FILE)
      (EDT=RESET.INPUT.FILE))
    (if (MEMBER WISH.TO.END '(Y YES J JA))
	(CASE KIND.OF.END
	  (!  (ST-FIX) 'ATP.TOP.LEVEL)
	  (OK (ST-FIX) 'PROVE.OR.EDIT)
	  (OTHERWISE NIL))
	nil)))

(DEFUN EDT=V (SWITCH)
						; EDITED:13-APR-83.
						; INPUT: ATOM.
						; EFFECT:If 'SWITCH' IS EIN, ON, OR T THE
						;        screen-control is switched on, else it's
						;        switched off.
						; VALUE: UNDEFINED. 
  #+(or symbolics explorer)
  (COND ((MEMBER SWITCH '(EIN ON T)) (TV:KBD-ESC-MORE 1)) (T (TV:KBD-ESC-MORE 0)))
  #-(or symbolics explorer)
  (declare (ignore switch)))

(DEFUN EDT=HELP (COMMAND)
						; EDITED:24-MAR-83.
						; INPUT: ATOM, NIL OR NAME OF A POSSIBLE COMMAND OF
						;        THE EDITOR.
						; EFFECT:PRINTS A LIST OF ALL POSSIBLE COMMANDS IF
						;        'COMMAND' = NIL, ELSE THE COMMAND 'COMMAND'
						;        WILL BE EXPLAINED.
						; VALUE: NIL OR ERROR-MESSAGE (COMMAND NOT DEFINED).  
  (SETQ COMMAND (EDT=NORM.COMMAND.NAME COMMAND))
  (COND ((NULL COMMAND) (EDT=WRITELN (EDT=OUTPUTS 'COMMANDS) EDT*DIALOGUE.OUTPUT.FILE T) NIL)
	((NOT (MEMBER COMMAND EDT*COMMANDS)) (EDT=ERRORS 'COMMAND))
	(T (EDT=WRITELN (EDT=OUTPUTS COMMAND) EDT*DIALOGUE.OUTPUT.FILE T) NIL)))

(DEFUN EDT=HHELP (FILE)
						; EDITED:25-MAR-83.
						; INPUT: ATOM.
						; EFFECT:PRINTS DESCRIPTION OF ALL POSSIBLE COMMANDS
						;        ON FILE 'FILE'.
						; VALUE: READY-MESSAGE.                               
  (PROG
    ((STREAM
       (COND ((MEMBER FILE '(T NIL)) EDT*DIALOGUE.OUTPUT.FILE)
	     (T (mkrp-OPENOUT (mkrp-make.pathname nil nil nil file) nil))))
     (COMMAND.LIST (CONS 'HHELP.HEADER EDT*COMMANDS)))
    (DODOWN (RPTN (LIST-LENGTH COMMAND.LIST))
      (PROGN (EDT=WRITELN (EDT=OUTPUTS (CAR COMMAND.LIST)) STREAM T) (TAB (LINELENGTH NIL STREAM) 0 STREAM) (TERPRI STREAM)
	     (SETQ COMMAND.LIST (CDR COMMAND.LIST))))
    (COND ((NEQ STREAM EDT*DIALOGUE.OUTPUT.FILE) (CLOSEFILE STREAM))) (RETURN (EDT=OUTPUTS 'HHELP.READY FILE))))

(DEFUN EDT=CREATE.FORMULA (INFIX PREFIX TYPE)
						; EDITED: 18-AUG-83 20:32:29
						; INPUT:  3 S-EXPRESSIONS.
						; EFFECT: -
						; VALUE:  THE LIST OF THE 3 S-EXPRESSIONS (INFIX
						;         PREFIX TYPE).                               
  (LIST INFIX PREFIX TYPE))

(DEFUN EDT=GET.INFIX.FORMULA (FORMULA)
						; EDITED:24-AUG-83.
						; INPUT: A FORMULA WITH 3 COMPONENTS.
						; EFFECT: -
						; VALUE: THE INFIX COMPONENT OF THE FORMULA.          
  (CAR FORMULA))

(DEFUN EDT=GET.prefix.FORMULA (FORMULA)
						; EDITED: 8-Oct-91.
						; INPUT:  A formula with 3 components.
						; EFFECT: -
						; VALUE:  The prefix component of the FORMULA.          
  (CAR FORMULA))

(DEFUN EDT=IS.FIRST.INPUT.FILE NIL
						; EDITED: 18-AUG-83 21:07:26
						; INPUT:  -
						; EFFECT: -
						; VALUE:  T, IF NO RESET OF INPUT FILE IS POSSIBLE,
						;         NIL, IF THERE IS ANOTHER FILE AS THE FIRST
						;         ONE IN THE EDT*INPUT.FILES LIST.            
  (EQl 1 (LIST-LENGTH EDT*INPUT.FILES)))

(DEFUN EDT=INPUT.FILE NIL
						; EDITED: 18-AUG-83 20:52:06
						; INPUT:  -
						; EFFECT: -
						; VALUE:  NAME OF INPUT FILE FOR THE EDITOR, E.G.
						;         THE FIRST ELEMENT OF THE EDT*INPUT.FILES
						;         LIST.                                       
  (CAAR EDT*INPUT.FILES))

(DEFUN EDT=SET.INPUT.FILE (FILE)
						      ; EDITED: 18-AUG-83 20:54:33
						      ; INPUT:  NAME OF A FILE.
						      ; EFFECT: PUSHES 'FILE' ON THE STACK EDT*INPUT.FILES,
						      ;         E.G. IT BECOMES NEW INPUT FILE OF THE
						      ;         EDITOR.
						      ; VALUE:  NEW EDT*INPUT.FILES LIST OR NIL, IF FILE
						      ;         DOESN' T EXIST.                             
  (COND ((NOT (FILE.EXISTS FILE)) nil)
	(T
	 (let ((STREAM (mkrp-OPENIN (mkrp-make.pathname t (if EDT*THEOREM.FLAG (mkrp-default.th) (mkrp-default.ax)) (mkrp-default.lisp) FILE))))
	   (SETQ EDT*INPUT.FILES (CONS (LIST STREAM 0 FILE) EDT*INPUT.FILES))))))

(DEFUN EDT=RESET.INPUT.FILE NIL
						; EDITED: 18-AUG-83 20:58:13
						; INPUT:  -
						; EFFECT: NEW INPUT FILE OF THE EDITOR WILL BE THIS,
						;         WHICH HAS BEEN USED BEFORE THE CURRENTLY
						;         USED ONE.                                   
  (CLOSEFILE (EDT=INPUT.FILE))
  (SETQ EDT*INPUT.FILES (CDR EDT*INPUT.FILES))
  (COND ((NOT (EDT=IS.FIRST.INPUT.FILE))
	 (DODOWN (RPTN (CADAR EDT*INPUT.FILES))
	   (EDT=IGNORE.COMMAND)))))


(DEFUN EDT=IS.INPUT.FILE.SEQUENCE.RECURSIVE (FILE)
						; EDITED: 19-AUG-83 14:28:55
						; INPUT:  NAME OF A FILE.
						; EFFECT: -
						; VALUE:  T, IF 'FILE' IS NOW USED A SECOND TIME IN
						;         A RECURSION OF COMMAND EXECUTE, E.G. IF
						;         'FILE' IS IN EDT*INPUT.FILES, NIL ELSE      
  (INSIDE FILE EDT*INPUT.FILES))

(DEFUN EDT=COMMAND.COUNTER NIL
						; EDITED: 19-AUG-83 14:32:42
						; INPUT:  -
						; EFFECT: THE NUMBER OF ON THE CURRENT INPUT FILE
						;         EXECUTED COMMANDS STORED IN EDT*INPUT.FILES
						;         WILL BE INCREMENTED BY 1.
						; VALUE:  NEW NUMBER.                                 
  (incf (CADAR EDT*INPUT.FILES)))

(DEFUN EDT=OUTPUTS (OUTPUTCASE &REST ARGUMENTS)
						; EDITED:25-MAR-83.
						; INPUT: ATOM, TO SELECT AN OUTPUT (LIST OF LINES OR
						;        WORDS) AND A LIST OF ARGUMENTS, WHITCH CAN
						;        BE SUBSTITUTED IN THE MESSAGE.
						; EFFECT: -
						; VALUE: THE SELECTED OUTPUT.
  #-:mkrp.deutsch
  (CASE OUTPUTCASE
    (INSERT					;THE FOLLOWING OUTPUTS ARE FOR HELP AND HHELP, THEY
						;DESCRIBE COMMANDS.
      '("I[NSERT] <FORMULA>" "<FORMULA>" "------------------"
	"If formula <FORMULA> is syntactically and semantically correct, it will "
	"be inserted as last one in active area, else as first one in passive area."))
    (DELETE
      '("D[ELETE] [<FROM>][-][<TO>]"
	"D[ELETE] [<FROM>][/][<TO>]"
	"--------------------------"
	"If <TO> is greater or equal than number of last formula in active"
	"area, the corresponding formulas will be deleted."
	"Examples: D-   deletes all formulas,"
	"          D 2  deletes formula 2,"
	"          D 3- deletes all formulas from the third one,"
	"          D    deletes the last formula of active area if it exists, else"
	"               the first of passive area."))
    ((+SHIFT ++SHIFT)
     '("+[SHIFT] [<NUMBER>]" "++[SHIFT]" "-------------------"
       "+SHIFT shifts the first <NUMBER> formulas of the passive area into the"
       "active area, if they are syntactically and semantically correct."
       "Default-value of <NUMBER> is 1."
       "++SHIFT shifts all formulas."
       "Example: * 1 *             * 1 *" "           2       + 2     * 2 *"
       "           3     ------>   * 3 *" "           4                 4"))
    ((-SHIFT --SHIFT)
     '("-[SHIFT] [<NUMBER>]"
       "--[SHIFT]"
       "-------------------"
       "-SHIFT shifts the last <NUMBER> formulas of the active area into the"
       "passive area."
       "Default-value of <NUMBER> is 1."
       "--SHIFT shifts all formulas."
       "Example: * 1 *      -      * 1 *"
       "         * 2 *   ------>     2"))
    (EDIT
      '("E[DIT] [<NR>]"
	"-------------"
	"The Lisp-editor is called for the formula <NR>."
	"If the active area is not empty, the default-value of <NR> is the"
	"number of last formula in active area, else 1."))
    (CHANGE
      (LIST "C[HANGE] [<NR>]" "---------------"
	    "The formula <NR> is printed on the terminal. User shall enter a new"
	    "formula to replace the printed one."
	    "If the active area is not empty, default-value of <NR> is the number"
	    "of last formula in active area, else 1."))
    (READ
      '("R[EAD] <FILE>" "-------------"
	"<FILE> must be created by the WRITE-command. If the editor is in"
	"the initial state, the formulas are inserted into the same areas "
	"containing them at WRITE time.  OTherwise all formulas are inserted"
	"into the passive area."))
    (WRITE
      '("W[RITE] <FILE>" "--------------"
	"The contents of the editor will be saved on file <FILE>, so that it  "
	"can be restored with the READ-command."))
    (WRITE.GET
      '("W[RITE.]G[ET] <FILE>" "--------------------"
	"The contents of the editor will be saved on file <FILE>, so that it  "
	"can be restored with the GET-command."))
    (GET
      '("G[ET] <FILE>" "------------"
	"<FILE> contains a sequence of formulas in the following format:"
	"         (<FORMULA1>)"
	"         (<FORMULA2>)"
	"" "" ""
	"         (<FORMULAN>)"
	"         STOP"
	"THE FORMULAS ARE INSERTED INTO THE PASSIVE AREA"))
    (EXECUTE
      '("EXEC[UTE] <FILE>" "----------------"
	"The file <FILE> contains a sequence of editor commands in the following"
	"format:"
	"         <COMMAND1>!"
	"         <COMMAND2>!"
	"" "" ""
	"         <COMMANDN>!"
	"         OK"
	"The commands will be executed."
	"For dialogue furthermore is used the terminal." "Take care for the right number of )."))
    (SWITCH
      '("SW[ITCH] [<NR1>] [<NR2>]" "------------------------"
	"If formula <NR1> and formula <NR2> are in the passive area they will" "be exchanged."
	"If the user only gives one number, this and first formula are taken,"
	"SWITCH alone exchanges first and last formula of passive area."))
    (UNDO
      '("U[NDO] [<NUMBER>]" "U[NDO] ON" "U[NDO] OFF" "-----------------"
	"Undo on and undo off are switching undo-mode on or off, respectively."
	"Undo undoes the last <NUMBER> destructive commands, as INSERT,"
	"DELETE, EDIT, CHANGE, +SHIFT, -SHIFT, SWITCH, READ, AND GET."
	"No <NUMBER> means one command."))
    (REPLACE
      '("REP[LACE] <OLD> <NEW>" "X <OLD> <NEW>" "---------------------"
	"Replace replaces the symbol <OLD> by the symbol <NEW> in active and passive"
	"area as well as in symbol-table." "Replace cannot be undone,"
	"nor can any commands executed prior to a REPLACE."))
    (PPRINT
      '("PP[RINT] <FILE>" "---------------"
	"PPRINT writes the symbol-table and all formulas in a readable form on" "the given file."))
    (SSHOW
      (LIST "SS[HOW] <X1>...<XN>" "SS[HOW] -" "-------------------"
	    "SSHOW is a more beautyful version of SHOW."
	    "Each <XI> must be a symbol-name or a list of kinds."
	    "There are the kinds S[ORT], F[UNCTION], P[REDICATE] and C[ONSTANT]."
	    "All symbols that are so specified will be shown on the terminal."
	    "Example: SS (P) F  will show all predicates and symbol F,"
	    "         SS-       all symbols."))
    (SHOW
      (LIST "S[HOW] <X1>...<XN>" "S[HOW] -" "------------------"
	    "Each <XI> must be a symbol-name or a list of kinds."
	    "There are the kinds S[ORT], F[UNCTION], P[REDICATE] and C[ONSTANT]"
	    "All symbols that are so specified will be shown on the terminal."
	    "Example: S (P) F  will show all predicates and symbol F,"
	    "         S-       all symbols."))
    (PREFIX
      '("PRE[FIX] [<FROM>][-][<TO>]" "PRE[FIX] [<FROM>][/][<TO>]" "F [<FROM>][-][<TO>]"
	"F [<FROM>][/][<TO>]" "--------------------------"
	"PREFIX writes the compiled (prefix-) form of the formulas <FROM> to <TO>"
	"on the screen."
	"Examples: PRE- or PRE/ all formulas of the active area,"
	"          PRE 3        formula 3, if active,"
	"          PRE -5       the formulas 1 to 5, if they are in the active area,"
	"          PRE          the last formula of active area."))
    (INFIX
      '("IN[FIX] [<FROM>][-][<TO>]" "IN[FIX] [<FROM>][/][<TO>]" "L[IST] [<FROM>][-][<TO>]"
	"L[IST] [<FROM>][/][<TO>]" "-------------------------"
	"INFIX writes the infixform (input-language) of the formulas <FROM> to <TO>" "on the screen."
	"Examples: IN- or IN/ all formulas," "          IN7        the seventh formula,"
	"          IN -5      the formulas 1 to 5,"
	"          IN         the last formula of active area, or if it doesn't exist"
	"                     the first of passive area."))
    (state
      '("ST[ATE]" "-------" "STATE shows the user, how the formulas are distributed on the areas."
	"Example: AREA 1 :  3 formulas         (active area)"
	"         AREA 0 :  1 formula          (passive area)"))
    (OK
      '("OK" "--" "OK terminates the editor and returns to the caller."
	"OK used on a file for the EXECUTE command terminates the EXECUTE command."))
    (!
      '("!" "-" "! terminates the editor and returns to ATP-top-level."
	"! in the input of a command cancels it."))
    (LISP '((MKRP-DEFAULT.LISP) "----" "LISP calls the lisp system." "You can return to the editor by OK."))
    (V
      '("V ON   " "V [OFF]" "-------" "V ON and V T will switch on the manual teletype-control."
	"V OFF and V will switch it off."))
    (HELP
      '("H[ELP] [<COMMAND>]" "------------------"
	"HELP prints a list of all possible commands on the screen."
	"HELP <COMMAND> explaines the command <COMMAND>."))
    (HHELP
      '("HH[ELP] <FILE>" "--------------"
	"HHELP prints explanations for all possible commands on file <FILE>."))
    (COMMANDS
      '("Possible commands  :" "--------------------"
	"I[NSERT]  D[ELETE]  +[SHIFT]  -[SHIFT]  E[DIT]    C[HANGE]  R[EAD] "
	"W[RITE]   G[ET]     W[RITE.]G[ET]       SW[ITCH]  U[NDO]    REP[LACE]"
	"PP[RINT]  S[HOW]    SS[HOW]   PRE[FIX]  IN[FIX]   ST[ATE]   OK"
        "V         LISP      !         H[ELP]    HH[ELP]   EXEC[UTE]"))
    (HHELP.HEADER
      '("   Commands of the formula-editor : " "   ================================ "
	"This is an editor for predicate logic formulas written in PLL."
	"The formulas are kept in two different areas: if a formula was accepted"
	"by the compiler, it is included into the active area. In this case"
	"symbol table entries and the prefix form exist for the formula and " "are accessable."
	"Other formulas (e.g. such with syntax errors) are stored in the passive"
	"area. Thus even erroneous input is not lost and the passive area can be"
	"used as a scratch pad."
	"When terminated the editor returns the list of the formulas in the active"
	"area for further processing (e.g. by the theorem prover). The passive"
	"formulas are not considered."
	"Below is a list of the editor commands. Several commands can be concatenated"
	"by the separator  _"
	"Every command must begin with an atom (Insert without command name)."))
    (LEAVE					;OUTPUTS FOR READY-MESSAGE.
      '("Editor terminated."))
    (NOT.EXECUTED
      (LIST (format nil "Command ~A not executed because of !." (CAR ARGUMENTS))))
    (SAVED
      (LIST (format nil "Formulas and symbol-table saved on file ~A." (CAR ARGUMENTS))))
    (otherwise (edt=second.part.english outputcase arguments)))
  #+:mkrp.deutsch
  (CASE OUTPUTCASE
    (INSERT					;THE FOLLOWING OUTPUTS ARE FOR HELP AND HHELP, THEY
						;DECLARE COMMANDS.
      '("I[NSERT] <FORMEL>" "<FORMEL>" "-----------------"
	"FALLS DIE FORMEL <FORMEL> SYNTAKTISCH UND SEMANTISCH KORREKT IST, WIRD "
	"SIE ALS LETZTE IN DEN AKTIVEN BEREICH EINGEFUEGT, SONST ALS ERSTE IN DEN" "PASSIVEN."))
    (DELETE
      '("D[ELETE] [<VON>][-][<BIS>]" "D[ELETE] [<VON>][/][<BIS>]" "--------------------------"
	"FALLS <BIS> GROESSER ODER GLEICH DER NUMMER DER LETZTEN FORMEL IM "
	"AKTIVEN BEREICH IST, WERDEN DIE ENTSPRECHENDEN FORMELN GELOESCHT."
	"Z.B.: D-    LOESCHT ALLE FORMELN, " "      D 2   LOESCHT FORMEL 2,"
	"      D 3-  LOESCHT ALLE FORMELN AB DER DRITTEN,"
	"      D     DIE LETZTE DES AKTIVEN BEREICHS, FALLS SIE EXISTIERT, SONST"
	"            DIE ERSTE DES PASSIVEN."))
    ((+SHIFT ++SHIFT)
     '("+[SHIFT] [<ANZAHL>]" "++[SHIFT]" "-------------------"
       "+SHIFT SCHIEBT DIE ERSTEN <ANZAHL> FORMELN DES PASSIVEN IN DEN AKTIVEN"
       "BEREICH, SOWEIT DIESE SYNTAKTISCH UND SEMANTISCH KORREKT SIND."
       "FEHLENDES <ANZAHL> WIRD 1 GESETZT." "++SHIFT VERSCHIEBT ALLE FORMELN."
       "Z.B.: * 1 *             * 1 *" "        2       + 2     * 2 *" "        3     ------>   * 3 *"
       "        4                 4"))
    ((-SHIFT --SHIFT)
     '("-[SHIFT] [<ANZAHL>]" "--[SHIFT]" "-------------------"
       "-SHIFT SCHIEBT DIE LETZTEN <ANZAHL> FORMELN AUS DEM AKTIVEN IN DEN " "PASSIVEN BEREICH."
       "FEHLENDES <ANZAHL> WIRD 1 GESETZT." "--SHIFT VERSCHIEBT ALLE FORMELN."
       "Z.B.: * 1 *      -      * 1 *" "      * 2 *   ------>     2"))
    (EDIT
      '("E[DIT] [<NR>]" "-------------" "FUER DIE FORMEL <NR> WIRD DER LISPEDITOR AUFGERUFEN."
	"BEI FEHLENDEM <NR> WIRD DIE LETZTE FORMEL DES AKTIVEN BEREICHS GENOMMEN,"
	"IST DIESER LEER, DIE ERSTE DES PASSIVEN."))
    (CHANGE
      (LIST "C[HANGE] [<NR>]" "---------------"
	    "DIE FORMEL <NR> WIRD AUSGEGEBEN. DANN WIRD DIE EINGABE EINER NEUEN FORMEL"
	    "VOM BENUTZER ERWARTET."
	    "BEI FEHLENDEM <NR> WIRD DIE LETZTE FORMEL DES AKTIVEN BEREICHS GENOMMEN,"
	    "IST DIESER LEER, DIE ERSTE DES PASSIVEN."))
    (READ
      '("R[EAD] <DATEI>" "--------------"
	"DIE AUF DER DATEI <DATEI> GESPEICHERTEN FORMELN WERDEN AM BEGINN EINER"
	"SITZUNG ENTSPRECHEND IHRER FRUEHEREN ZUGEHOERIGKEIT AUF DIE BEREICHE "
	"VERTEILT, SONST ALLE IN DEN PASSIVEN BEREICH EINGEFUEGT."
	"DIE DATEI MUSS VOM KOMMANDO 'WRITE' BESCHRIEBEN WORDEN SEIN."))
    (WRITE
      '("W[RITE] <DATEI>" "---------------"
	"DER INHALT DES EDITORS WIRD SO AUF DIE DATEI <DATEI> GESCHRIEBEN, DASS"
	"ER DURCH DAS KOMMANDO READ WIEDER GELADEN WERDEN KANN."))
    (GET
      '("G[ET] <DATEI>" "-------------" "DIE ENTSPRECHEND DEM UNTEN AUFGEFUEHRTEN BEISPIEL AUF DER DATEI"
	"<DATEI> STEHENDEN FORMELN, WERDEN IN DEN PASSIVEN BEREICH EINGEFUEGT." "Z.B.: (FORMEL1)"
	"      (FORMEL2)" "        ." "        ." "        ." "      (FORMELN)" "      STOP"))
    (EXECUTE
      '("EXEC[UTE] <DATEI>" "----------------"
	"DIE IM FOLGENDEN FORMAT AUF DER DATEI <DATEI> STEHENDEN EDITORKOMMANDOS" "WERDEN AUSGEFUEHRT:"
	"         <KOMMANDO1>!" "         <KOMMANDO2>!" "              ." "              ."
	"              ." "         <KOMMANDON>!" "         OK"
	"FUER DEN DIALOG MIT DEM BENUTZER WIRD WEITERHIN DAS TERMINAL BENUTZT."
	"BITTE AUF KORREKTE KLAMMERUNG ACHTEN."))
    (SWITCH
      '("SW[ITCH] [<NR1>] [<NR2>]" "------------------------"
	"FALLS DIE FORMELN <NR1> UND <NR2> IM PASSIVEN BEREICH SIND, WERDEN SIE" "VERTAUSCHT."
	"FALLS NUR <NR1> EINGEGEBEN WURDE, WIRD <NR1> MIT DER ERSTEN VERTAUSCHT,"
	"OHNE EINGABE DIE ERSTE MIT DER LETZTEN."))
    (UNDO
      '("U[NDO] [<ANZAHL>]" "U[NDO] EIN" "U[NDO] AUS" "-----------------"
	"MACHT DIE LETZTEN <ANZAHL> DESTRUKTIVEN KOMMANDOS, ALS DA SIND INSERT,"
	"DELETE, +SHIFT, -SHIFT, SWITCH, READ, GET, CHANGE UND EDIT RUECKGAENGIG."
	"NACH EINEM REPLACE IST UNDO NICHT MEHR MOEGLICH." "FEHLENDES <ANZAHL> WIRD 1 GESETZT."))
    (REPLACE
      '("REP[LACE] <ALT> <NEU>" "X <ALT> <NEU>" "---------------------"
	"REPLACE ERSETZT DAS SYMBOL <ALT> DURCH DAS SYMBOL <NEU> IM AKTIVEN UND"
	"PASSIVEN BEREICH, SOWIE DER SYMBOLTABELLE." "REPLACE KANN NICHT RUECKGAENGIG GEMACHT WERDEN."
	"AUCH FUER DIE VORHER AUSGEFUEHRTEN DESTRUKTIVEN KOMMANDOS IST EIN UNDO" "NICHT MEHR MOEGLICH."))
    (PPRINT
      '("PP[RINT] <DATEI>" "----------------"
	"PPRINT SCHREIBT SYMBOLTABELLE UND FORMELMENGE IN EINER LESBAREN FORM"
	"AUF DIE ANGEGEBENE DATEI."))
    (SSHOW
      (LIST "SS[HOW] <X1>...<XN>" "SS[HOW] -" "-------------------"
	    "SSHOW IST EINE NOBLERE VERSION VON SHOW."
	    "JEDES <XI> IST EIN SYMBOL ODER EINE LISTE VON ARTEN."
	    "ES GIBT DIE ARTEN S[ORTE], F[UNKTION], P[RAEDIKAT] UND K[ONSTANTE]."
	    "ALLE SO SPEZIFIZIERTEN SYMBOLE WERDEN DEM BENUTZER MIT IHREN" "EIGENSCHAFTEN AUSGEGEBEN."
	    "Z.B.:    SS-       GIBT ALLE SYMBOLE AUS,"
	    "         SS (K) FF ALLE KONSTANTEN UND DAS SYMBOL FF."))
    (SHOW
      (LIST "S[HOW] <X1>...<XN>" "S[HOW] -" "------------------"
	    "JEDES <XI> IST EIN SYMBOL ODER EINE LISTE VON ARTEN."
	    "ES GIBT DIE ARTEN S[ORTE], F[UNKTION], P[RAEDIKAT] UND K[ONSTANTE]."
	    "ALLE SO SPEZIFIZIERTEN SYMBOLE WERDEN DEM BENUTZER MIT IHREN" "EIGENSCHAFTEN AUSGEGEBEN."
	    "Z.B.:    S-       GIBT ALLE SYMBOLE AUS,"
	    "         S (K) FF ALLE KONSTANTEN UND DAS SYMBOL FF."))
    (PREFIX
      '("PRE[FIX] [<VON>][-][<BIS>]" "PRE[FIX] [<VON>][/][<BIS>]" "F [<VON>][-][<BIS>]"
	"F [<VON>][/][<BIS>]" "--------------------------"
	"PREFIX GIBT DIE UEBERSETZTE (PRAEFIX=)VERSION DER FORMELN <VON> BIS "
	"<BIS> AUF DEN BILDSCHIRM AUS." "Z.B.: PRE- ODER PRE/ ALLE FORMELN DES AKTIVEN BEREICHS,"
	"      PRE67          FORMEL 67, FALLS AKTIV,     "
	"      PRE -5         FORMELN 1 BIS 5 FALLS 5 IM AKTIVEN BEREICH,"
	"      PRE            LETZTE FORMEL DES AKTIVEN BEREICHS."))
    (INFIX
      '("IN[FIX] [<VON>][-][<BIS>]" "IN[FIX] [<VON>][/][<BIS>]" "L[IST] [<VON>][-][<BIS>]"
	"L[IST] [<VON>][/][<BIS>]" "-------------------------"
	"INFIX GIBT DIE INFIXFORM (EINGABESPRACHE) DER FORMELN <VON> BIS <BIS> AUS. "
	"Z.B.: IN- ODER IN/ ALLE FORMELN, " "      IN 2         DIE ZWEITE FORMEL, "
	"      IN 6-        ALLE AB DER SECHSTEN,"
	"      IN           DIE LETZTE DES AKTIVEN BEREICHS, FALLS DIESE EXISTIERT,"
	"                   SONST DIE ERSTE DES PASSIVEN."
	"FORMELN DES AKTIVEN BEREICHS WERDEN MIT * GEKENNZEICHNET . "))
    (STATE
      '("ST[ATE]" "-------" "STATE TEILT DEM BENUTZER MIT, WIE DIE FORMELN AUF DIE BEREICHE VERTEILT"
	"SIND." "Z.B.: BEREICH 1 :  3 FORMELN      (AKTIVER BEREICH)"
	"      BEREICH 0 :  1 FORMEL       (PASSIVER BEREICH)"))
    (OK
      '("OK" "--" "OK BEENDET DEN EDITOR UND GIBT AN DEN AUFRUFER ZURUECK."
	"OK AUF EINER DATEI, DIE FUER DAS EXECUTEKOMMANDO BENUTZT WIRD, BEENDET " D "DIESES KOMMANDO."))
    (!
      '("!" "-" "! BEENDET DEN EDITOR UND GIBT AUF ATP-TOP-LEVEL ZURUECK."
	"! IN DER EINGABE EINES KOMMANDOS BRICHT DIESES AB."))
    (LISP '((MKRP-DEFAULT.LISP) "----" "LISP RUFT DAS LISPSYSTEM AUF." "MIT OK KOMMT MAN IN DEN EDITOR ZURUECK."))
    (V
      '("V EIN" "V [AUS]" "-------" "V EIN UND V T SCHALTEN DIE MANUELLE BILDSCHIRMSTEUERUNG EIN,"
	"V ODER V AUS SCHALTET SIE AB."))
    (HELP
      '("H[ELP] [<KOMMANDO>]" "-------------------"
	"HELP GIBT EINE LISTE DER MOEGLICHEN KOMMANDOS AUF DEM BILDSCHIRM AUS."
	"HELP <KOMMANDO> ERKLAERT DAS KOMMANDO <KOMMANDO>."))
    (HHELP
      '("HH[ELP] <DATEI>" "---------------"
	"HHELP SCHREIBT DIE ERKLAERUNGEN FUER ALLE KOMMANDOS AUF DIE DATEI <DATEI>."
	"DIESE KANN AUSGEDRUCKT WERDEN."))
    (COMMANDS
      '("Moegliche Kommandos :" "---------------------"
	"I[NSERT]  D[ELETE]  +[SHIFT]  -[SHIFT]  E[DIT]    C[HANGE]  R[EAD] "
	"W[RITE]   G[ET]     W[RITE.]G[ET]       SW[ITCH]  U[NDO]    REP[LACE]"
	"PP[RINT]  S[HOW]    SS[HOW]   PRE[FIX]  IN[FIX]   ST[ATE]   OK"
        "V         LISP      !         H[ELP]    HH[ELP]   EXEC[UTE]"))
    (HHELP.HEADER
      '("   KOMMANDOS DES FORMELEDITORS :" "   ============================="
	"MIT DIESEM EDITOR KOENNEN  IN PLL  GESCHRIEBENE PRAEDIKATENLOGISCHE"
	"FORMELN EDITIERT WERDEN. ES GIBT ZWEI  BEREICHE  IN  DENEN  FORMELN "
	"GEHALTEN WERDEN: SIND SIE VOM  COMPILER ALS KORREKT ERKANNT WORDEN, "
	"BEFINDEN   SIE   SICH  IM AKTIVEN  BEREICH.  DANN  EXISTIEREN  AUCH "
	"SYMBOLTABELLENEINTRAEGE UND  PRAEFIXFORM  DER FORMELN UND MIT DEN "
	"ENTSPRECHENDEN KOMMANDOS KANN DARAUF ZUGEGRIFFEN WERDEN."
	"ANDERE, D.H. NICHT UEBERSETZTE ODER NICHT KORREKTE FORMELN SIND"
	"IM PASSIVEN BEREICH ABGESPEICHERT, DAMIT FEHLERHAFTE EINGABEN NICHT"
	"VERLOREN GEHEN. DIESER BEREICH KANN AUCH ALS SCHMIERZETTEL DIENEN."
	"BEI VERLASSEN DES EDITORS WIRD EINE LISTE DER AKTIVEN FORMELN "
	"AN DEN BEWEISER UEBERGEBEN, OHNE DASS DER PASSIVE BEREICH " "BERUECKSICHTIGT WIRD."
	"ES FOLGT EINE BESCHREIBUNG DER VERSCHIEDENEN EDITORKOMMANDOS."
	"MEHRERE KOMMANDOS KOENNEN MIT DEM SEPARATOR _ ANEINANDERGEREIHT WERDEN."
	"JEDES KOMMANDO MUSS MIT EINEM ATOM BEGINNEN (INSERT OHNE " "KOMMANDONAME)."))
    (LEAVE					;OUTPUTS FOR READY-MESSAGE.
      '("EDITOR BEENDET."))
    (NOT.EXECUTED
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "KOMMANDO ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " WEGEN DES ! NICHT AUSGEFUEHRT."))))
    (SAVED
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMELN UND SYMBOLTABELLE AUF DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GESICHERT."))))
    (otherwise (edt=second.part.german outputcase arguments))))

(defun edt=second.part.english (indicator &optional arguments)
  (case indicator
    (INSERT.ACTIVE (LIST (format nil "Inserted at position ~A in active area." (CAR ARGUMENTS))))
    (HHELP.READY (LIST (format nil "Explanations for all commands written on file ~A." (CAR ARGUMENTS))))
    (PP.READY (LIST (format nil "Symbol-table and formulas written on file ~A." (CAR ARGUMENTS))))
    (READ.READY.00 (LIST (format nil "File ~A has been written when editor was empty." (CAR ARGUMENTS))))
    (READ.READY.01 (LIST (format nil "One formula loaded from file ~A." (CAR ARGUMENTS))
			 (format nil "Inserted in the passive area at position ~A." (SECOND (CDDDDR ARGUMENTS)))))
    (READ.READY.10 (LIST (format nil "Symbol-table and one formula loaded from file ~A." (CAR ARGUMENTS))
			 (format nil "Inserted in the active area at position ~A." (FOURTH ARGUMENTS))))
    (READ.READY.11 (LIST (format nil "Symbol-table and 2 formulas loaded from file ~A." (CAR ARGUMENTS))
			 (format nil "One inserted in the active area at position ~A." (FOURTH ARGUMENTS))
			 (format nil "One inserted in the passive area at position ~A." (SECOND (CDDDDR ARGUMENTS)))))
    (READ.READY.0N
      (LIST (format nil "~A formulas loaded from file ~A." (CAR ARGUMENTS) (THIRD ARGUMENTS))
	    (format nil "inserted in the passive area at positions ~A to ~A."
		    (SECOND (CDDDDR ARGUMENTS)) (THIRD (CDDDDR ARGUMENTS)))))
    (READ.READY.N0
      (LIST (format nil "Symbol-table and ~a formulas loaded from file ~a." (SECOND ARGUMENTS) (CAR ARGUMENTS))
	    (format nil "Inserted in the active area at positions ~a to ~A." (FOURTH ARGUMENTS) (CAR (CDDDDR ARGUMENTS)))))
    (READ.READY.1N
      (LIST (format nil "Symbol-table and formulas loaded from file ~A." (CAR ARGUMENTS))
	    (format nil "ONE OF THESE FORMULAS INSERTED IN THE ACTIVE AREA AT POSITION ~A." (FOURTH ARGUMENTS))
	    (format nil "~A inserted in the passive area at positions ~A to ~A"
		    (THIRD ARGUMENTS) (sixth ARGUMENTS) (seventh ARGUMENTS))))
    (READ.READY.N1
      (LIST
	(format nil "Symbol-table and formulas loaded from file ~A." (CAR ARGUMENTS))
	(format nil "One of these formulas inserted in the passive area at position ~A." (sixth ARGUMENTS))
	(format nil "~A INSERTED IN THE ACTIVE AREA AT POSITIONS ~A to ~A." (SECOND ARGUMENTS)
		(FOURTH ARGUMENTS) (fifth ARGUMENTS))))
    (READ.READY.NN (LIST (format nil "Symbol-table and formulas loaded from file ~A." (CAR ARGUMENTS))
			 (format nil "~A of these formulas inserted in the passive area at positions ~A to ~A.."
				 (THIRD ARGUMENTS) (sixth ARGUMENTS) (seventh ARGUMENTS))
			 (format nil "~A inserted in the active  area at positions ~A to ~A."
				 (SECOND ARGUMENTS) (FOURTH ARGUMENTS) (fifth ARGUMENTS))))
    (DELETE.READY.N (LIST (format nil "Formulas ~A to ~A deleted." (CAR ARGUMENTS) (SECOND ARGUMENTS))))
    (DELETE.READY.1 (LIST (format nil "Formula ~A deleted." (CAR ARGUMENTS))))
    (SHIFT.UP.READY.N `(,(format nil "~A formulas shifted into the active area." (CAR ARGUMENTS))))
    (SHIFT.UP.READY.1 (LIST "One formula shifted into the active area."))
    (SHIFT.UP.READY.0 (LIST "No shift executed."))
    (SHIFT.DOWN.READY.N
      (LIST (format nil "~A formulas shifted into the passive area." (CAR ARGUMENTS))))
    (SHIFT.DOWN.READY.1 '("One formula shifted into the passive area."))
    (get.formulas (list (format nil "~D formulas got from file ~A." (first arguments) (second arguments))))
    (insert.formulas (list (format nil "~D formulas inserted into passive area." (first arguments))))
    (EXCHANGED (LIST (format nil "Formulas ~A and ~A exchanged." (CAR ARGUMENTS) (SECOND ARGUMENTS))))
    (REPLACED (LIST (format nil "Symbol ~A replaced by symbol ~A." (CAR ARGUMENTS) (SECOND ARGUMENTS))))
    (NOT.REPLACED '("REPLACE NOT EXECUTED."))
    (EDITED (LIST (FORMAT NIL "Formula ~A edited." (CAR ARGUMENTS))))
    (CHANGE.FORMULA (LIST "Please enter a new formula for the following one:"))
    (CHANGED (LIST (format nil "Formula ~a changed." (CAR ARGUMENTS))))
    (UNDO.END (LIST (format nil "Undo of ~A executed." (CAR ARGUMENTS))))
    (UNDO.ON '("Undo-mode switched on."))
    (UNDO.OFF '("Undo-mode switched off."))
    (SAVE					;OUTPUTS FOR COMMUNICATIONS IN COMMANDS WITH USER.
      (LIST "Formulas not saved. Terminate? (Y/N)"))
    (NEW.NOT.NEW (LIST (format nil "Symbol ~a already in symbol-table." (CAR ARGUMENTS))))
    (NEW.NOT.CONSISTANT (LIST (format nil "New compilation of ~a formulas necessary." (Car ARGUMENTS))))
    (REPLACE? (LIST (format nil "Perform replacement of ~a by ~a? (y/n)" (CAR ARGUMENTS) (SECOND ARGUMENTS))))
    (FORMULA? '("Formula?"))
    (FILE? '("File?"))
    (SYMBOLS? '("Symbols?"))
    (NEW? '("<NEW>?"))
    (SORT (CASE (CAR ARGUMENTS)
	    (1					;OUTPUTS FOR PRINT OF SYMBOL-TABLE.
	      '("Sorts" "-----"))
	    (OTHERWISE '(NAME SUBSORTS SUPERSORTS ATTRIBUTES))))
    (CONSTANT (CASE (CAR ARGUMENTS) (1 '("Constants" "---------")) (OTHERWISE '(NAME SORT ATTRIBUTES))))
    (FUNCTION (CASE (CAR ARGUMENTS) (1 '("Functions" "---------")) (OTHERWISE '(NAME ARITY DOMAIN RANGE ATTRIBUTES))))
    (PREDICATE (CASE (CAR ARGUMENTS)
		 (1 '("Predicates" "----------"))
		 (OTHERWISE '(NAME ARITY DOMAIN ATTRIBUTES))))
    (SPACE.ST '("   S Y M B O L - T A B L E   :" "   ===========================" " " " " " "))
    (NOT.KIND (LIST (format nil "Symbols of the kind ~A don't exist." (CAR ARGUMENTS))))
    (DATUM "Date of file generation:  ")
    (NOT.IN.ST (LIST (format nil "The symbol ~A isn't in the symbol-table." (CAR ARGUMENTS))))
    (OTHERWISE '("UNDEFINED OUTPUT."))))

(defun edt=second.part.german (indicator &optional arguments)
  (case indicator
    (INSERT.ACTIVE
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMEL ALS ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING ". IN AKTIVEN BEREICH EINGEFUEGT."))))
    (HHELP.READY
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "ERKLAERUNGEN FUER ALLE KOMMANDOS AUF DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) " GESCHRIEBEN.")))
    (PP.READY
      (LIST
	(CONCATENATE 'STRING "SYMBOLTABELLE UND FORMELN LESBAR AUF DATEI "
		     (PRINC-TO-STRING (CAR ARGUMENTS)) " GESCHRIEBEN.")))
    (READ.READY.00
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "DATEI ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     " WURDE ANGELEGT ALS KEINE FORMELN IM EDITOR WAREN.")))
    (READ.READY.01
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINE FORMEL VON DATEI ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " GELADEN."))
	(CONCATENATE 'STRING (PRINC-TO-STRING "ALS ") (PRINC-TO-STRING (SECOND (CDDDDR ARGUMENTS)))
		     (PRINC-TO-STRING ". IN DEN PASSIVEN BEREICH EINGEFUEGT."))))
    (READ.READY.10
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOLTABELLE UND EINE FORMEL VON DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GELADEN."))
	(CONCATENATE 'STRING (PRINC-TO-STRING "ALS ") (PRINC-TO-STRING (FOURTH ARGUMENTS))
		     (PRINC-TO-STRING ". IN DEN AKTIVEN BEREICH EINGEFUEGT."))))
    (READ.READY.11
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOLTABELLE UND ZWEI FORMELN VON DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GELADEN."))
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINE ALS ") (PRINC-TO-STRING (FOURTH ARGUMENTS))
		     (PRINC-TO-STRING ". IN DEN AKTIVEN BEREICH EINGEFUEGT,"))
	(CONCATENATE 'STRING (PRINC-TO-STRING "DIE ANDERE ALS ")
		     (PRINC-TO-STRING (SECOND (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". IN DEN PASSIVEN."))))
    (READ.READY.0N
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING (THIRD ARGUMENTS)) (PRINC-TO-STRING " FORMELN VON DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GELADEN UND "))
	(CONCATENATE 'STRING (PRINC-TO-STRING "ALS ") (PRINC-TO-STRING (SECOND (CDDDDR ARGUMENTS)))
		     (PRINC-TO-STRING ". BIS ") (PRINC-TO-STRING (THIRD (CDDDDR ARGUMENTS)))
		     (PRINC-TO-STRING ". IN DEN PASSIVEN BEREICH EINGEFUEGT."))))
    (READ.READY.N0
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOLTABELLE UND ") (PRINC-TO-STRING (SECOND ARGUMENTS))
		     (PRINC-TO-STRING " FORMELN VON DATEI ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " GELADEN UND "))
	(CONCATENATE 'STRING (PRINC-TO-STRING "ALS ") (PRINC-TO-STRING (FOURTH ARGUMENTS))
		     (PRINC-TO-STRING ". BIS ") (PRINC-TO-STRING (CAR (CDDDDR ARGUMENTS)))
		     (PRINC-TO-STRING ". IN DEN AKTIVEN BEREICH EINGEFUEGT."))))
    (READ.READY.1N
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOLTABELLE UND FORMELN VON DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GELADEN."))
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINE DIESER FORMELN ALS ")
		     (PRINC-TO-STRING (FOURTH ARGUMENTS)) (PRINC-TO-STRING ". IN DEN AKTIVEN BEREICH EINGEFUEGT,"))
	(CONCATENATE 'STRING (PRINC-TO-STRING (THIRD ARGUMENTS)) (PRINC-TO-STRING " ALS ")
		     (PRINC-TO-STRING (SECOND (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". BIS ")
		     (PRINC-TO-STRING (THIRD (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". IN DEN PASSIVEN."))))
    (READ.READY.N1
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOLTABELLE UND FORMELN VON DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GELADEN."))
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINE DIESER FORMELN ALS ")
		     (PRINC-TO-STRING (SECOND (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". IN DEN PASSIVEN BEREICH EINGEFUEGT,"))
	(CONCATENATE 'STRING (PRINC-TO-STRING (SECOND ARGUMENTS)) (PRINC-TO-STRING " ALS ")
		     (PRINC-TO-STRING (FOURTH ARGUMENTS)) (PRINC-TO-STRING ". BIS ")
		     (PRINC-TO-STRING (CAR (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". IN DEN AKTIVEN."))))
    (READ.READY.NN
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOLTABELLE UND FORMELN VON DATEI ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " GELADEN."))
	(CONCATENATE 'STRING (PRINC-TO-STRING (THIRD ARGUMENTS)) (PRINC-TO-STRING " DIESER FORMELN ALS ")
		     (PRINC-TO-STRING (SECOND (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". BIS ")
		     (PRINC-TO-STRING (THIRD (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". IN DEN PASSIVEN BEREICH EINGEFUEGT,"))
	(CONCATENATE 'STRING (PRINC-TO-STRING (SECOND ARGUMENTS)) (PRINC-TO-STRING " ALS ")
		     (PRINC-TO-STRING (FOURTH ARGUMENTS)) (PRINC-TO-STRING ". BIS ")
		     (PRINC-TO-STRING (CAR (CDDDDR ARGUMENTS))) (PRINC-TO-STRING ". IN DEN AKTIVEN."))))
    (DELETE.READY.N
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMELN ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " - ") (PRINC-TO-STRING (SECOND ARGUMENTS)) (PRINC-TO-STRING " GELOESCHT."))))
    (DELETE.READY.1
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMEL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " GELOESCHT."))))
    (SHIFT.UP.READY.N
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " FORMELN IN DEN AKTIVEN BEREICH GESCHOBEN. "))))
    (SHIFT.UP.READY.1 (LIST "EINE FORMEL IN DEN AKTIVEN BEREICH GESCHOBEN."))
    (SHIFT.UP.READY.0 (LIST "NICHTS GEAENDERT."))
    (SHIFT.DOWN.READY.N
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " FORMELN IN DEN PASSIVEN BEREICH GESCHOBEN."))))
    (SHIFT.DOWN.READY.1 (LIST "EINE FORMEL IN DEN PASSIVEN BEREICH GESCHOBEN."))
    (get.formulas (list (format nil "~D Formeln von Datei ~A geladen." (first arguments) (second arguments))))
    (insert.formulas (list (format nil "~D Formeln in den passiven Bereich geschrieben." (first arguments))))
    (EXCHANGED
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMELN ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " UND ") (PRINC-TO-STRING (SECOND ARGUMENTS)) (PRINC-TO-STRING " VERTAUSCHT."))))
    (REPLACED
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " DURCH SYMBOL ") (PRINC-TO-STRING (SECOND ARGUMENTS)) (PRINC-TO-STRING " ERSETZT."))))
    (NOT.REPLACED '("KEINE ERSETZUNG AUSGEFUEHRT."))
    (EDITED
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMEL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " EDITIERT."))))
    (CHANGED
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "FORMEL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " ERSETZT."))))
    (CHANGE.FORMULA (LIST "BITTE FUER FOLGENDE FORMEL EINE NEUE EINGEBEN:"))
    (UNDO.END
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " RUECKGAENGIG GEMACHT. "))))
    (UNDO.ON '("UNDO-MODUS EINGESCHALTET.")) (UNDO.OFF '("UNDO-MODUS AUSGESCHALTET."))
    (SAVE					;OUTPUTS FOR COMMUNICATION IN COMMANDS WITH USER.
      (LIST "FORMELN NICHT GESICHERT. AUFHOEREN? (J/N)"))
    (NEW.NOT.NEW
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "SYMBOL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " SCHON IN SYMBOLTABELLE VORHANDEN."))))
    (NEW.NOT.CONSISTANT
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "NEUE UEBERSETZUNG VON ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " FORMELN NOTWENDIG."))))
    (REPLACE?
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "ERSETZUNG VON ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " DURCH ") (PRINC-TO-STRING (SECOND ARGUMENTS))
		     (PRINC-TO-STRING " DURCHFUEHREN? (J/N)"))))
    (FORMULA? '("FORMEL?")) (SYMBOLS? '("SYMBOLE?")) (FILE? '("DATEI?")) (NEW? '("<NEU>?"))
    (SORT
      (CASE (CAR ARGUMENTS)
	(1					;OUTPUTS FOR PRINT OF SYMBOL TABLE.
	  '("SORTEN" "------"))
	(OTHERWISE '(NAME UNTERSORTEN OBERSORTEN ATTRIBUTE))))
    (CONSTANT
      (CASE (CAR ARGUMENTS) (1 '("KONSTANTEN" "----------")) (OTHERWISE '(NAME SORTE ATTRIBUTE))))
    (FUNCTION
      (CASE (CAR ARGUMENTS) (1 '("FUNKTIONEN" "----------"))
	    (OTHERWISE '(NAME STELL. DEF.BEREICH WERTEBEREICH ATTRIBUTE))))
    (PREDICATE
      (CASE (CAR ARGUMENTS) (1 '("PRAEDIKATE" "----------"))
	    (OTHERWISE '(NAME STELL. DEFINITIONSBEREICH ATTRIBUTE))))
    (SPACE.ST '("   S Y M B O L T A B E L L E   :" "   =============================" " " " "))
    (NOT.KIND (LIST (format nil "Symbole der Art ~A gibt es nicht." (CAR ARGUMENTS))))
    (DATUM "DATEIERSTELLUNGSDATUM:  ")
    (NOT.IN.ST
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "DAS SYMBOL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " IST NICHT VORHANDEN. "))))
    (OTHERWISE '("AUSGABE NICHT DEFINIERT."))))

(DEFUN EDT=ERRORS (ERRORCASE &REST ARGUMENTS)
						; EDITED:30-MAR-83.                                   
						; INPUT: ATOM, TO SELECT AN ERROR-MESSAGE (LIST OF    
						;        LINES) AND A LIST OF ARGUMENTS, WHICH CAN BE 
						;        SUBSTITUTED IN THE MESSAGE.                  
						; EFFECT: -                                           
						; VALUE: THE SELECTED ERROR-MESSAGE.                  
  #-:mkrp.deutsch
  (CASE ERRORCASE
    (NOT.FILE (LIST (format nil "Input-error: File ~a doesn't exist." (CAR ARGUMENTS))))
    (INSERT.PASSIVE
      (LIST (format nil "Input-error: Formula inserted in passive area at position ~A." (CAR ARGUMENTS))))
    (THEOREM.INSERT
      (LIST "Input-error: Formula not allowed in theorem-mode."
	    (format nil "Inserted into active area at position ~A." (CAR ARGUMENTS))))
    (NOT.NUMBER
      (LIST (format nil "Input-error: ~A is no natural number." (CAR ARGUMENTS))))
    (NOT.EXIST
      (LIST (format nil "INput-error: Specified formulas not in ~A to ~A." (CAR ARGUMENTS) (SECOND ARGUMENTS))))
    (NOT.EXIST.1
      (LIST (format nil "Input-error: Specified formula not in ~A to ~A." (CAR ARGUMENTS) (SECOND ARGUMENTS))))
    (DELETE (LIST "Input-error: <TO> less than number of last formula in active area."))
    (UNDO.STACK.EMPTY (LIST "Input-error: Undo-stack is empty."))
    (FIRST.PASSIVE
      (LIST (format nil "Input-error: Formula ~A not correct." (CAR ARGUMENTS))))
    (COMMAND (LIST (format nil "Input-error: Command ~s not defined." (first arguments))
		   "HELP gives you the possible commands."))
    (PASSIVE.EMPTY (LIST "Input-error: Passive area empty."))
    (ACTIVE.EMPTY (LIST "Input-error: Active area empty."))
    (KEYWORD (LIST "Input-error: One of the symbols is a keyword."))
    (SYMBOLS (LIST "Input-error: One of the symbols is no atom."))
    (FILE (LIST "Input-error: <FILE> is T or NIL."))
    (EMPTY (LIST "Input-error: Active and passive area empty."))
    (RECURSIVE
      (LIST (format nil "Input-error: File ~A is used recursive in execute commands." (CAR ARGUMENTS))))
    (OTHERWISE '("Undefined error.")))
  #+:mkrp.deutsch
  (CASE ERRORCASE
    (NOT.FILE
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: DATEI ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " EXISTIERT NICHT."))))
    (INSERT.PASSIVE
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: FORMEL AUF POSITION ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " IN PASSIVEN BEREICH EINGEFUEGT."))))
    (THEOREM.INSERT
      (LIST "EINGABEFEHLER: FORMELN DIESES TYPS IM THEOREMMODUS VERBOTEN."
	    (CONCATENATE 'STRING (PRINC-TO-STRING "ALS ") (PRINC-TO-STRING (CAR ARGUMENTS))
			 (PRINC-TO-STRING ". IN PASSIVEN BEREICH EINGEFUEGT."))))
    (NOT.NUMBER
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " IST KEINE NATUERLICHE ZAHL."))))
    (NOT.EXIST
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: ANGEGEBENE FORMELN NICHT ZWISCHEN ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " UND ") (PRINC-TO-STRING (SECOND ARGUMENTS))
		     (PRINC-TO-STRING "."))))
    (NOT.EXIST.1
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: ANGEGEBENE FORMEL NICHT ZWISCHEN ")
		     (PRINC-TO-STRING (CAR ARGUMENTS)) (PRINC-TO-STRING " UND ") (PRINC-TO-STRING (SECOND ARGUMENTS))
		     (PRINC-TO-STRING "."))))
    (DELETE (LIST "EINGABEFEHLER: <BIS> KLEINER ALS NUMMER DER LETZTEN FORMEL IM AKTIVEN BEREICH."))
    (UNDO.STACK.EMPTY (LIST "EINGABEFEHLER: UNDOSTAPEL LEER."))
    (FIRST.PASSIVE
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: FORMEL ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " NICHT KORREKT."))))
    (LESS.PASSIVE.NIL
      (LIST "EINGABEFEHLER: <ANZAHL> GROESSER ALS ANZAHL DER FORMELN IM PASSIVEN BEREICH."))
    (COMMAND (LIST "EINGABEFEHLER: KOMMANDO NICHT DEFINIERT. 'HELP' GIBT" "DIE MOEGLICHEN KOMMANDOS AUS."))
    (EMPTY (LIST "EINGABEFEHLER: AKTIVER UND PASSIVER BEREICH LEER."))
    (RECURSIVE
      (LIST
	(CONCATENATE 'STRING (PRINC-TO-STRING "EINGABEFEHLER: DATEI ") (PRINC-TO-STRING (CAR ARGUMENTS))
		     (PRINC-TO-STRING " WURDE IN EXECUTEKOMMANDOS"))
	"REKURSIV BENUTZT."))
    (PASSIVE.EMPTY (LIST "EINGABEFEHLER: PASSIVER BEREICH LEER."))
    (ACTIVE.EMPTY (LIST "EINGABEFEHLER: AKTIVER BEREICH LEER."))
    (KEYWORD (LIST "EINGABEFEHLER: MINDESTENS EINES DER SYMBOLE IST EIN SCHLUESSELWORT."))
    (SYMBOLS (LIST "EINGABEFEHLER: MINDESTENS EINER DER EINGABEPARAMETER IST KEIN ATOM."))
    (FILE (LIST "EINGABEFEHLER: <DATEI> DARF NICHT T ODER NIL SEIN."))
    (OTHERWISE (LIST "EINGABEFEHLER: UNDEFINIERTER FEHLER."))))

(DEFUN EDT=GET.COMMAND (stream)
  (let* ((*READTaBLE* edt*command.readtable)
	 (COMMAND (READ stream nil (intern "" (find-package "MKRP")))))
    (EDT=NORM.COMMAND.NAME COMMAND)))

