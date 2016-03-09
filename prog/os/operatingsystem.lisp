;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

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

(export 'mkrp (find-package "MKRP"))

(DEFUN MKRP (&optional directory example hc error)
						; Input:  DIRECTORY is a legal input for the DEFINE.DIRECTORY command.
						;         Example: "/user2/prckln/mkrp/examples/classic/"
						;         EXAMPLE is a legal input for the DEFINE.EXAMPLE command.
						;         Example: "steamroller"
						;         HC is a flag.
                                                ;         ERROR is a flag.
						; value:  undefined
						; effect: If no input is specified this is the
						;         startfunction and top loop of the atp-
						;         system. Initializes the system.
						;         If DIRECTORY and EXAMPLE are specified the system
						;         takes the problem file from this directory and executes an RP
						;         followed by a HP command (if HP is true), using the options in a file
						;         options.lisp in the specified directory without any user dialogue.
						;         If no problem file exists or is older than existing AX
						;         and TH file in editor or GET format the system uses those as input.
                                                ;         If ERROR is true the system is not called in dialog mode.
  (let ((*print-pretty* nil)
	(*package* (find-package "MKRP")))
    (UNWIND-PROTECT (CATCH 'MKRP
		      (when directory (OS=DEFINE.DIRECTORY (list directory)))
		      (when example (OS=DEFINE.EXAMPLE.NAME (list example)))
		      (cond ((and directory (or (and (probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.ax) (mkrp-default.lisp) NIL))
						     (probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.th) (mkrp-default.lisp) NIL)))
						(probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.problem) (mkrp-default.lisp) NIL))))
			     (with-open-file (*standard-output* (MKRP-MAKE.PATHNAME T "batch" (mkrp-default.text) NIL) :direction :output)
			       (OS=OPT.READ (MKRP-MAKE.PATHNAME T (mkrp-default.options) (mkrp-default.lisp) nil))
			       (if (and (probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.problem) (mkrp-default.lisp) NIL))
					(or (not (probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.ax) (mkrp-default.lisp) NIL)))
					    (> (file-write-date (MKRP-MAKE.PATHNAME T (mkrp-default.problem) (mkrp-default.lisp) NIL))
					       (file-write-date (MKRP-MAKE.PATHNAME T (mkrp-default.ax) (mkrp-default.lisp) NIL))))
					(or (not (probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.th) (mkrp-default.lisp) NIL)))
					    (> (file-write-date (MKRP-MAKE.PATHNAME T (mkrp-default.problem) (mkrp-default.lisp) NIL))
					       (file-write-date (MKRP-MAKE.PATHNAME T (mkrp-default.th) (mkrp-default.lisp) NIL)))))
				   (OS=CONSTRUCT.REFUTE.protocol nil)
				   (OS=formula.CONSTRUCT.REFUTE.protocol nil))
			       (when hc
				 #+(or symbolics explorer)
				 (hci::hardcopy-file (OR OS*ACTUAL.LIST.FILE
							 (MKRP-MAKE.PATHNAME T (mkrp-default.list)
									     (if (opt-get.option pr_latex)
										 "tex" (mkrp-default.text))
									     NIL))
						     hci:*default-text-printer*))))
			    ((not error)
			       #+(or symbolics explorer) (GLOBAL:SEND *STANDARD-OUTPUT* ':CLEAR-WINDOW)
			       #+symbolics
			       (TV:WITH-CHARACTER-Style ('(:text :mkrp :large))
				 (format t "~%~%~%                          M K R P     Theorem Prover~%"))
			       #-symbolics
			       (format t "~%~%~%                 M K R P     Theorem Prover~%")
			       (TERPRI T)
			       (TERPRI T)
			       (SETQ OS*RESET.FLG T)
			       (OS=INIT)
			       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION '\@ATP_SESSION.LANGUAGE) *standard-output*)
			       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION '\@ATP_EXPLAIN.HELP) *standard-output*)
			       (TERPRI T)
			       (WHILE T (CATCH 'COMMAND (OS=READ&EXECUTE.ATP.COMMAND))))
			    (t (warn "Files in ~s ~s not found, MKRP not run." directory example))))
      (MKRP-CLOSE.ALL.STREAMS))))

(DEFUN OS=INIT ()				; input:  nil
						; value:  undefined
						; effect: initializes the variable os*alloewd.commands
						;         the current atp-version (see: init-system.-
						;         type) is set by selecting the os*commands of
						;         this version out of all possible commands.
						;         these commands and their spellings are saved
						;         in os*command.spellings.
						;         the dialogue-language of the option- and
						;         the edit-module is set depending on the
						;         value of os*language.
  (when (fboundp 'edt-set.readtables)
    (edt-set.readtables)))

(DEFVAR OS*READTABLE (LET ((RT (COPY-READTABLE NIL)))
		       (SET-SYNTAX-FROM-CHAR #\; #\a RT RT)
		       RT))

(DEFUN OS=READ&EXECUTE.ATP.COMMAND NIL
						; INPUT:  NONE
						; VALUE:  UNDEFINED
						; EFFECT: READS THE NEXT ATP-COMMAND AND EXECUTES IT.
  (FRESH-LINE)
  (PRINC "@ ")  
  (LET (COMMAND.LIST COMMAND.NAME COMMAND)
    (LET ((*READTABLE* OS*READTABLE))
      (SETQ COMMAND.LIST (READLINE *terminal-io*)))
    (SETQ COMMAND.NAME (CAR COMMAND.LIST)
	  COMMAND      (OS=NORMALIZE.ALLOWED.COMMAND COMMAND.NAME)
	  COMMAND.LIST (CDR COMMAND.LIST))
    (CASE COMMAND
      (V       (OS=VDT (CAR COMMAND.LIST)))
      (HC      (OS=HARDCOPY (CAR COMMAND.LIST)))
      (HELP    (OS=HELP (CAR COMMAND.LIST)))
      (EXIT    (THROW 'MKRP NIL))
      (LOGOFF  #+(or symbolics explorer)(progn (GLOBAL:LOGOUT) (THROW 'MKRP NIL))
	       #+lucid(user::quit))
      (LISP    (ENTERLISP 'OK))
      (OPTIONS (if (opt-get.option gen_common.lisp)
		   (OS=OPTION)
		   (OPT-WND_MAINWINDOW-EXPOSE)))
      (DEFINE.DIRECTORY        (OS=DEFINE.DIRECTORY    COMMAND.LIST))
      (DEFINE.EXAMPLE.NAME     (OS=DEFINE.EXAMPLE.NAME COMMAND.LIST))
      (SELECT.EXAMPLE          (OS=select.EXAMPLE      COMMAND.LIST))
      (EDIT                    (OS=EDIT COMMAND.LIST))
      (EDIT.CONSTRUCT                    (OS=EDIT.CONSTRUCT COMMAND.LIST))
      (EDIT.CONSTRUCT.REFUTE             (OS=EDIT.CONSTRUCT.REFUTE COMMAND.LIST))
      (EDIT.CONSTRUCT.REFUTE.PROTOCOL    (OS=EDIT.CONSTRUCT.REFUTE.PROTOCOL COMMAND.LIST))
      (FORMULA                           (OS=FORMULA COMMAND.LIST))
      (FORMULA.CONSTRUCT                 (OS=FORMULA.CONSTRUCT COMMAND.LIST))
      (FORMULA.CONSTRUCT.REFUTE          (time (OS=FORMULA.CONSTRUCT.REFUTE COMMAND.LIST)))
      (FORMULA.CONSTRUCT.REFUTE.PROTOCOL (OS=FORMULA.CONSTRUCT.REFUTE.PROTOCOL COMMAND.LIST))
      (CONSTRUCT                         (TIME (OS=CONSTRUCT COMMAND.LIST)))
      (CONSTRUCT.REFUTE                  (time (OS=CONSTRUCT.REFUTE COMMAND.LIST)))
      (CONSTRUCT.REFUTE.PROTOCOL         (OS=CONSTRUCT.REFUTE.PROTOCOL COMMAND.LIST))
      (REFUTE          (TIME (OS=REFUTE COMMAND.LIST)))
      (REFUTE.PROTOCOL (OS=REFUTE.PROTOCOL COMMAND.LIST))
      (PROTOCOL        (OS=PROTOCOL COMMAND.LIST))
      (VIEW.PROTOCOL   (mkrp-view.file (OR OS*ACTUAL.LIST.FILE (MKRP-MAKE.PATHNAME T (mkrp-default.list)
										   (if (opt-get.option pr_latex) "tex" (mkrp-default.text))
										   NIL))))
      (HARDCOPY.PROTOCOL #+symbolics
			 (hci::hardcopy-file-menu (OR OS*ACTUAL.LIST.FILE
						      (MKRP-MAKE.PATHNAME T (mkrp-default.list)
									  (if (opt-get.option pr_latex) "tex" (mkrp-default.text))
									  NIL))))
      (B               #+(or symbolics explorer)
		       (GLOBAL:SEND *STANDARD-OUTPUT* ':SET-DEFAULT-CHARACTER-STYLE '(:FIX :ROMAN :LARGE)))
      (N               #+(or symbolics explorer)
		       (GLOBAL:SEND *STANDARD-OUTPUT* ':SET-DEFAULT-CHARACTER-STYLE '(:FIX :ROMAN :NORMAL)))
      (F               #+(or symbolics explorer)
		       (GLOBAL:SEND *STANDARD-OUTPUT* ':SET-DEFAULT-CHARACTER-STYLE (READ T)))
      (INDUCTION       (OS=INDUCTION))
      (OTHERWISE       (OS=WRITE.TEXT
			 (OS.E-GET.EXPLANATION 'READ&EXECUTE.ATP.COMMAND_ERROR=ILLEGAL.COMMAND COMMAND.NAME COMMAND.LIST)
			 *standard-output*)))
    (TERPRI T) ))

(DEFUN OS=NORMALIZE.ALLOWED.COMMAND (COMMAND)
						; INPUT: -ATOM, SPELLING OF AN OS-COMMAND
						; VALUE:  ATOM, SYNTAX-DESCRITION OF THE INPUT OS-
						;               COMMAND.
						; REMARKS: ALL ALLOWED OS-COMMANDS AND THEIR SPELLINGS
						;         ARE STORED IN OS*COMMAND.SPELLINGS (SEE:
						;         OS=INIT).
  (LET ((NORMALIZED.COMMAND (CAAR (MEMBER-IF #'(LAMBDA (COMMAND.SPELLINGS) (MEMBER COMMAND (CDDR COMMAND.SPELLINGS)))
					     OS*COMMAND.SPELLINGS))))
    (IF NORMALIZED.COMMAND NORMALIZED.COMMAND 'ILLEGAL.COMMAND)))

(DEFUN \@ NIL
						; INPUT:  NONE
						; VALUE:  UNDEFINED
						; EFFECT: TERMINATES IMMEDIATELY THE EXECUTION OF THE
						;         CURRENT COMMAND//FUNCTION AND RETURNS TO THE
						;         ATP TOP-LOOP.
						; REMARKS: --USE ONLY IF YOU ARE IN THE BREAK-MODE!--
  (mkrp-closE.ALL.STREAMS)
  (THROW 'COMMAND NIL))

(defparameter os*opened.p nil "Used for construct.refute combination to suppress graph file creation")

(DEFUN OS=VDT (ARG)
						; INPUT: -ATOM
						; VALUE:  UNDEFINED
						; EFFECT: CALLS THE INTERLISP-FUNCTION VDT WITH THE
						;         ARGUMENT DEPENDING ON THE INPUT.
  #+symbolics
  (CASE ARG
    ((N NO NEIN NIL) (TV:KBD-ESC-MORE 0))
    ((Y YES J JA OK T) (TV:KBD-ESC-MORE 1))
    (OTHERWISE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'VDT_ERROR=ILLEGAL.ARGUMENT ARG (READLINE)) *standard-output*)))
  #+explorer
  (CASE ARG
    ((N NO NEIN NIL) (TV:KBD-terminal-MORE 0))
    ((Y YES J JA OK T) (TV:KBD-terminal-MORE 1))
    (OTHERWISE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'VDT_ERROR=ILLEGAL.ARGUMENT ARG (READLINE)) *standard-output*)))
  #-(or symbolics explorer)
  (declare (ignore arg)))

(DEFUN OS=HARDCOPY (SWITCH)
						; INPUT: -ATOM
						; VALUE:  UNDEFINED.
						; EFFECT: SWITCHES HARDCOPY ON OR OFF RESPECTIVELY.
						;         SWITCH = N,NIL :   HARDCOPY OFF.
						;         SWITCH = T:        HARDCOPY ON PRINTER.
						;         SWITCH = <FILE>:   HARDCOPY ON <FILE>.
  (COND ((EQ SWITCH 'N) (SETQ SWITCH NIL)))
  (COND
    ((NULL (DRIBBLE SWITCH)) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HARDCOPY_ERROR=ILLEGAL.ARGUMENT) *standard-output*))))

(DEFUN OS=HELP (COM)
						; INPUT: -ATOM, PREFERABLY A VALID OS-COMMAND.
						; VALUE:  UNDEFINED
						; EFFECT: IF INPUT=VALID OS-COMMAND:
						;            PRINTS AN EXPLANATION OF THE COMMAND,
						;         ELSE:
						;            PRINTS ALL ALLOWED OS-COMMANDS OF THE
						;            CURRENT ATP-VERSION.
  (let ((COMMAND (OS=NORMALIZE.ALLOWED.COMMAND COM)) (COMMAND.NAME COM))
    (cond (COM
	   (CASE COMMAND
	     (V (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP) *standard-output*))
	     (VIEW.PROTOCOL (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_VP) *standard-output*))
	     (HARDCOPY.PROTOCOL (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_hP) *standard-output*))
	     (HC (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_HC) *standard-output*))
	     (HELP (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_HELP) *standard-output*))
	     (EXIT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_EXIT) *standard-output*))
	     (LOGOFF (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_LOGOFF) *standard-output*))
	     (DEFINE.DIRECTORY (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_DEFINE.DIRECTORY) *standard-output*))
	     (DEFINE.EXAMPLE.NAME (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_DEFINE.EXAMPLE.NAME) *standard-output*))
	     (select.EXAMPLE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_select.EXAMPLE) *standard-output*))
	     (LISP (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_LISP) *standard-output*))
	     (EDIT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_EDIT) *standard-output*))
	     (EDIT.CONSTRUCT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_EDIT.CONSTRUCT) *standard-output*))
	     (EDIT.CONSTRUCT.REFUTE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_EDIT.CONSTRUCT.REFUTE) *standard-output*))
	     (EDIT.CONSTRUCT.REFUTE.PROTOCOL
	       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_EDIT.CONSTRUCT.REFUTE.PROTOCOL) *standard-output*))
	     (FORMULA (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_FORMULA) *standard-output*))
	     (FORMULA.CONSTRUCT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_FORMULA.CONSTRUCT) *standard-output*))
	     (FORMULA.CONSTRUCT.REFUTE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_FORMULA.CONSTRUCT.REFUTE) *standard-output*))
	     (FORMULA.CONSTRUCT.REFUTE.PROTOCOL
	       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_FORMULA.CONSTRUCT.REFUTE.PROTOCOL) *standard-output*))
	     (CONSTRUCT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_CONSTRUCT) *standard-output*))
	     (CONSTRUCT.REFUTE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_CONSTRUCT.REFUTE) *standard-output*))
	     (CONSTRUCT.REFUTE.PROTOCOL
	       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_CONSTRUCT.REFUTE.PROTOCOL) *standard-output*))
	     (REFUTE (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_REFUTE) *standard-output*))
	     (REFUTE.PROTOCOL (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_REFUTE.PROTOCOL) *standard-output*))
	     (PROTOCOL (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_PROTOCOL) *standard-output*))
	     (OPTIONS (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_OPTIONS) *standard-output*))
	     (SUBSYSTEMS (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_SUBSYSTEMS) *standard-output*))
	     (INDUCTION (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_INDUCTION) *standard-output*))
	     (ILLEGAL.COMMAND
	       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP_COMMAND.NOT.IMPLEMENTED COMMAND.NAME) *standard-output*))
	     (OTHERWISE (OS=HELP.EXPLANATION))))
	  (T (OS=HELP.EXPLANATION)))
    NIL))

(DEFUN OS=HELP.EXPLANATION NIL (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'HELP.EXPLANATION_EXPLAIN.HELP) *standard-output*))

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
	    (actual.store.symbol (SETF (SYMBOL-VALUE ACTUAL.STORE.SYMBOL) (string USER.GIVEN.SYMBOL)) T)
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

(defun os=output.file.name.check (file command file.type)
  (cond ((AND FILE (NOT (FILENAME.CHECK FILE)))
	 (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'ERROR.ILLEGAL.FILE FILE command file.type) *standard-output*)
	 NIL)
	(t t)))

(DEFUN OS=INDUCTION NIL				; input:  none
						; value:  undefined
						; effect: calls the induction theorem prover and asks for a theory.
  (FORMAT T "~&No induction prover available~&"))


(DEFUN OS=CONSTRUCT.and.REFUTE (graph.file CODE.FILE proof.comment)
						; input:  Two file names and a list of strings
						; effect: opens the code file for protocol, constructs
						;         the initial graphs, starts the refutation
						;         and closes the code file.
						;         Creates and uses graph file when splitting is used.
						; value:  undefined.
  (OS=OPEN.CODE.FILE CODE.FILE)
  (setq os*opened.p nil)
  (unless (OS=CONSTRUCT.PROC OS*ACTUAL.PROBLEM.FILE GRAPH.FILE OS*ACTUAL.CODE.FILE PROOF.COMMENT t)
    (if os*opened.p
	(OS=REFUTE.PROC OS*ACTUAL.GRAPH.FILE NIL OS*ACTUAL.CODE.FILE)
	(OS=REFUTE.PROC.cr OS*ACTUAL.GRAPH.FILE OS*ACTUAL.CODE.FILE)))
  (OS=CLOSE.CODE.FILE))

(DEFUN OS=CONSTRUCT (PARAM.LIST)		; input:  list of 0-4 atoms:
						;         [<problem.file> [<graph.file> [<code.file>
						;         [<proof.comment> ]]]]
						; effect: opens the code file for protocol, creates
						;         a set of initial graphs from a problem file
						;         and closes the code file.
						; value:  undefined.
  (LET ((PROBLEM.FILE  (FIRST PARAM.LIST))
	(GRAPH.FILE    (SECOND PARAM.LIST)) (CODE.FILE (THIRD PARAM.LIST))
	(PROOF.COMMENT (FOURTH PARAM.LIST)) (REST (NTHCDR 4 PARAM.LIST)))
    (WHEN (OS=CONSTRUCT.INPUT.CONTROL PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
      (OS=OPEN.CODE.FILE CODE.FILE)
      (OS=CONSTRUCT.PROC OS*ACTUAL.PROBLEM.FILE GRAPH.FILE OS*ACTUAL.CODE.FILE PROOF.COMMENT nil)
      (OS=CLOSE.CODE.FILE))))

(DEFUN OS=CONSTRUCT.INPUT.CONTROL (PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
						; input:  problem.file graph.file code.file, rest
						; effect: checking the input for construct and sending a message by error.
						; value:  t if the input is ok, else nil.
  (LET ((RESULT T))
    (WHEN REST
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
      (SETQ RESULT NIL))
    (SETQ RESULT (and (OS=OPEN.INPUT.FILE PROBLEM.FILE 'OS*ACTUAL.PROBLEM.FILE 'CONSTRUCT 'problem
					  (mkrp-default.problem) (mkrp-default.lisp))
		      (os=output.file.name.check GRAPH.FILE 'CONSTRUCT 'graph)
		      (os=output.file.name.check CODE.FILE 'CONSTRUCT 'CODE)))
    (unless RESULT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'CONSTRUCT) *standard-output*))
    RESULT))

(DEFUN OS=CONSTRUCT.REFUTE (PARAM.LIST)		; input:  list of 0-6 atoms:
						;         [<problem.file> [<graph.file>
						;         [<code.file> [<proof.comment>
						; effect: opens the code file for protocol, constructs
						;         the initial graphs, starts the refutation
						;         and closes the code file.
						; value:  undefined.
  (LET ((PROBLEM.FILE  (FIRST PARAM.LIST))
	(GRAPH.FILE    (SECOND PARAM.LIST))
	(CODE.FILE     (THIRD PARAM.LIST))
	(PROOF.COMMENT (FOURTH PARAM.LIST))
	(REST          (NTHCDR 6 PARAM.LIST)))
    (WHEN (OS=CONSTRUCT.REFUTE.INPUT.CONTROL PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
      (OS=CONSTRUCT.and.REFUTE graph.file code.file proof.comment))))

(DEFUN OS=CONSTRUCT.REFUTE.INPUT.CONTROL (PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
						; input:  problem.file graph.file code.file rest
						; effect: checking the input and sending a message by error.
						; value:  t if the input is ok, else nil.
  (LET ((RESULT T))
    (WHEN REST
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
      (SETQ RESULT NIL))
    (SETQ RESULT (and (OS=OPEN.INPUT.FILE PROBLEM.FILE 'OS*ACTUAL.PROBLEM.FILE
					  'CR
					  'PROBLEM
					  (mkrp-default.problem) (mkrp-default.lisp))
		      (os=output.file.name.check GRAPH.FILE 'CONSTRUCT.REFUTE 'GRAPH)
		      (os=output.file.name.check CODE.FILE 'CONSTRUCT.REFUTE 'CODE)))
    (COND
      ((NOT RESULT)
       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'CONSTRUCT.REFUTE) *standard-output*)))
    RESULT))

(DEFUN OS=CONSTRUCT.REFUTE.PROTOCOL (PARAM.LIST)
						; INPUT:  LIST OF 0-7 ATOMS:
						;         [<PROBLEM.FILE> [<GRAPH.FILE>
						;         [<CODE.FILE> [<LIST.FILE>
						;         [<PROOF.COMMENT>
						; EFFECT: OPENS THE CODE FILE, CONSTRUCTS THE INITIAL
						;         GRAPH, STARTS THE REFUTATION, CLOSES THE
						;         CODE FILE AND STARTS A PROTOCOL PROCESS.
						; VALUE:  UNDEFINED.
  (PROG
    ((PROBLEM.FILE (FIRST PARAM.LIST))
     (GRAPH.FILE (SECOND PARAM.LIST))
     (CODE.FILE (THIRD PARAM.LIST))
     (LIST.FILE (FOURTH PARAM.LIST))
     (PROOF.COMMENT (FIFTH PARAM.LIST))
     (REST (NTHCDR 7 PARAM.LIST)))
    (WHEN (OS=CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL PROBLEM.FILE GRAPH.FILE CODE.FILE LIST.FILE REST)
      (OS=CONSTRUCT.and.REFUTE graph.file code.file proof.comment)
      (OS=PROTOCOL.PROC OS*ACTUAL.CODE.FILE LIST.FILE))))

(DEFUN OS=CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL (PROBLEM.FILE GRAPH.FILE CODE.FILE LIST.FILE REST)
						; INPUT:  PROBLEM.FILE GRAPH.FILE CODE.FILE LIST.FILE
						;         REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (LET ((RESULT T))
    (WHEN REST
      (OS=WRITE.TEXT
	(OS.E-GET.EXPLANATION 'CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
      (SETQ RESULT NIL))
    (SETQ RESULT (and result
		      (OS=OPEN.INPUT.FILE PROBLEM.FILE 'OS*ACTUAL.PROBLEM.FILE
					  'cp
					  'PROBLEM
					  (mkrp-default.problem) (mkrp-default.lisp))
		      (os=output.file.name.check GRAPH.FILE 'Cr 'GRAPH)
		      (os=output.file.name.check CODE.FILE 'Cp 'CODE)
		      (os=output.file.name.check LIST.FILE 'Cp 'LIST)))
    (UNLESS RESULT
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'CONSTRUCT.REFUTE.PROTOCOL) *standard-output*))
    RESULT))

(DEFUN OS=CONSTRUCT.PROC (PROBLEM.FILE GRAPH.FILE CODE.FILE PROOF.COMMENT cr)
						; input:  list of 0-4 atoms:
						;         [<problem.file> [<graph.file>
						;         [<code.file> [<proof.comment> ]]]]
						;         CR is a flag that is true iff construct and refute is combined
						; effect: opens the graph file, prepares the initial
						;         graph and closes the graph file.
						; value:  undefined.
						; remark: code.file is open and remains so.
  (LET (RESULT)
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'CONSTRUCT.PROC_START.MESSAGE) *standard-output*)
      (SETQ PROOF.COMMENT
	    (COND (PROOF.COMMENT (OS.E-GET.EXPLANATION 'CONSTRUCT.PROC_PROOF.COMMENT PROOF.COMMENT)) (T NIL)))
      (OS=OPEN.GRAPH.FILE GRAPH.FILE)
      (SETQ RESULT (case (opt-get.option gen_other.prover)
		     (ER (PREP.er-INITIAL.GRAPH PROBLEM.FILE os*actual.graph.file CODE.FILE PROOF.COMMENT))
		     (C (prep.c-initial.graph PROBLEM.FILE os*actual.graph.file CODE.FILE PROOF.COMMENT))
		     (mkrp (PREP-INITIAL.GRAPH PROBLEM.FILE OS*ACTUAL.GRAPH.FILE CODE.FILE PROOF.COMMENT (not cr)))))
      (OS=CLOSE.GRAPH.FILE)
      (COND ((EQ RESULT 'SUCCESS) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'CONSTRUCT.PROC_THEOREM.PROVED) *standard-output*)))))

(DEFUN OS=DEFINE.DIRECTORY (DIRECTORY.STRING.SYMBOL)
  (SETQ OS*ACTUAL.PROBLEM.FILE NIL)
  (SETQ OS*ACTUAL.CODE.FILE NIL)
  (SETQ OS*ACTUAL.GRAPH.FILE NIL)
  (SETQ OS*ACTUAL.LIST.FILE NIL)
  (MKRP-SET.DEFAULT.DIRECTORY (if (stringp DIRECTORY.STRING.SYMBOL)
				  DIRECTORY.STRING.SYMBOL
				  (#+symbolics string-upcase #-symbolics string-downcase (STRING (CAR DIRECTORY.STRING.SYMBOL))))))

(DEFUN OS=DEFINE.EXAMPLE.NAME (EXAMPLE.NAME.STRING.SYMBOL)
  (SETQ OS*ACTUAL.PROBLEM.FILE NIL)
  (SETQ OS*ACTUAL.CODE.FILE NIL)
  (SETQ OS*ACTUAL.GRAPH.FILE NIL)
  (SETQ OS*ACTUAL.LIST.FILE NIL)
  (MKRP-SET.DEFAULT.NAME (if (stringp EXAMPLE.NAME.STRING.SYMBOL)
			     EXAMPLE.NAME.STRING.SYMBOL
			     (#+symbolics string-upcase #-symbolics string-downcase (PRINC-TO-STRING (CAR EXAMPLE.NAME.STRING.SYMBOL))))))

(DEFUN OS=select.EXAMPLE (PATHNAME)
  (SETQ OS*ACTUAL.PROBLEM.FILE NIL)
  (SETQ OS*ACTUAL.CODE.FILE NIL)
  (SETQ OS*ACTUAL.GRAPH.FILE NIL)
  (SETQ OS*ACTUAL.LIST.FILE NIL)
  (os=climb.directory  (pathname (if (stringp pathname)
				     pathname
				     (string-downcase (PRINC-TO-STRING (CAR  pathname)))))))

(defun os=climb.directory (directory)
  (let ((res (mkrp-menu (nconc (set-difference (mapcar #'(lambda (pathname) (pathname-name pathname))
						       (directory directory))
					       `((MKRP-DEFAULT.LIST) (MKRP-DEFAULT.OPTIONS) (MKRP-DEFAULT.FORMULAE) (MKRP-DEFAULT.CODE) (MKRP-DEFAULT.GRAPH) ,(MKRP-DEFAULT.PROBLEM) (MKRP-DEFAULT.TEMPPREPAXIOMS))
					       :test #'string-equal)
			       (list "Up" "Return"))
			(opt-get.option gen_common.lisp))))
    (cond ((string= res "Return")
	   (MKRP-SET.DEFAULT.DIRECTORY (string (make-pathname :host (pathname-host directory)
							      :directory (nbutlast (pathname-directory directory)))))
	   (MKRP-SET.DEFAULT.NAME (string-downcase (first (last (pathname-directory directory))))))
	  ((string= res "Up")
	   (os=climb.directory (make-pathname :host (pathname-host directory)
					      :directory (nbutlast (pathname-directory directory)))))
	  ((null res)
	   (os=climb.directory directory))
	  (t (os=climb.directory (make-pathname :host (pathname-host directory)
						:directory (append (pathname-directory directory) (list res))))))))

(DEFUN OS=EDIT (PARAM.LIST)
						; INPUT:  LIST OF 0-1 ATOMS:
						;         [<PROBLEM.FILE>]
						; VALUE:  UNDEFINED
						; EFFECT: EDIT THE PROBLEM FILE.
						;
  (LET ((PROBLEM.FILE (CAR PARAM.LIST))
	(REST (CDR PARAM.LIST)))
    (WHEN (OS=EDIT.INPUT.CONTROL PROBLEM.FILE REST)
      (OS=EDIT.PROC PROBLEM.FILE))))

(DEFUN OS=EDIT.INPUT.CONTROL (PROBLEM.FILE REST)
						; INPUT:  PROBLEM.FILE REST
						; EFFECT: CHECKING THE INPUT FOR EDIT AND
						;         SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (LET ((RESULT T))
    (WHEN REST (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'EDIT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*) (SETQ RESULT NIL))
    (setq result (os=output.file.name.check PROBLEM.FILE 'EDIT 'PROBLEM))
    (UNLESS RESULT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'EDIT) *standard-output*))
    RESULT))

(DEFUN OS=EDIT.CONSTRUCT (PARAM.LIST)
						; INPUT:  LIST OF 0-4 ATOMS:
						;         [<PROBLEM.FILE> [<GRAPH.FILE>
						;         [<CODE.FILE> [<PROOF.COMMENT> ]]]]
						; EFFECT: CREATES A PROBLEM DESCRIPTION, OPENS
						;         CODE FILE FOR PROTOCOL, CONSTRUCTS THE
						;         INITIAL GRAPH AND CLOSES THE CODE FILE.
						; VALUE:  UNDEFINED.
  (PROG ((PROBLEM.FILE (FIRST PARAM.LIST))
	 (GRAPH.FILE (SECOND PARAM.LIST))
	 (CODE.FILE (THIRD PARAM.LIST))
	 (PROOF.COMMENT (FOURTH PARAM.LIST))
	 (REST (NTHCDR (1- 5) PARAM.LIST)))
	(COND
	  ((OS=EDIT.CONSTRUCT.INPUT.CONTROL PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
	   (OS=EDIT.PROC PROBLEM.FILE)
	   (OS=OPEN.CODE.FILE CODE.FILE)
	   (OS=CONSTRUCT.PROC OS*ACTUAL.PROBLEM.FILE GRAPH.FILE OS*ACTUAL.CODE.FILE PROOF.COMMENT nil)
	   (OS=CLOSE.CODE.FILE)))))

(DEFUN OS=EDIT.CONSTRUCT.INPUT.CONTROL (PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
						; INPUT:  PROBLEM.FILE GRAPH.FILE CODE.FILE REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (PROG ((RESULT T))
	(WHEN REST
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'EDIT.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
	  (SETQ RESULT NIL))
	(setq result (and result
			  (os=output.file.name.check PROBLEM.FILE 'EDIT.CONSTRUCT 'PROBLEM)
			  (os=output.file.name.check GRAPH.FILE 'EDIT.CONSTRUCT 'GRAPH)
			  (os=output.file.name.check CODE.FILE 'EDIT.CONSTRUCT 'CODE)))
	(WHEN (NOT RESULT)
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'EDIT.CONSTRUCT) *standard-output*))
	(RETURN RESULT)))

(DEFUN OS=EDIT.CONSTRUCT.REFUTE (PARAM.LIST)
						; INPUT:  LIST OF 0-4 ATOMS:
						;         [<PROBLE.FILE> [<GRAPH.FILE>
						;         [<CODE.FILE> [<PROOF.COMMENT> ]]]]
						; EFFECT: CREATES A PROBLEM DESCRIPTION OF THE EDITED
						;         FORMULAS, OPENS THE CODE FILE FOR PROTOCOL,
						;         CONSTRUCTS THE INITIAL GRAPH, STARTS THE
						;         REFUTATION AND CLOSES THE CODE FILE.
						; VALUE:  UNDEFINED.
  (PROG
    ((PROBLEM.FILE (FIRST PARAM.LIST))
     (GRAPH.FILE (SECOND PARAM.LIST))
     (CODE.FILE (THIRD PARAM.LIST))
     (PROOF.COMMENT (FOURTH PARAM.LIST))
     (REST (NTHCDR (1- 5) PARAM.LIST)))
    (WHEN (OS=EDIT.CONSTRUCT.REFUTE.INPUT.CONTROL PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
      (OS=EDIT.PROC PROBLEM.FILE)
      (OS=CONSTRUCT.and.REFUTE graph.file code.file proof.comment))))

(DEFUN OS=EDIT.CONSTRUCT.REFUTE.INPUT.CONTROL (PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
						; INPUT:  PROBLEM.FILE GRAPH.FILE CODE.FILE REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, NIL ELSE.
  (PROG ((RESULT T))
	(WHEN REST
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'EDIT.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
	  (SETQ RESULT NIL))
	(setq result (and result
			  (os=output.file.name.check PROBLEM.FILE 'Er 'PROBLEM)
			  (os=output.file.name.check GRAPH.FILE 'Er 'GRAPH)
			  (os=output.file.name.check CODE.FILE 'ER 'CODE)))
	(WHEN (NOT RESULT)
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'EDIT.CONSTRUCT.REFUTE) *standard-output*))
	(RETURN RESULT)))

(DEFUN OS=EDIT.CONSTRUCT.REFUTE.PROTOCOL (PARAM.LIST)
						; INPUT:  LIST OF 0-5 ATOMS:
						;         [<PROBLEM.FILE> [<GRAPH.FILE>
						;         [<CODE.FILE> [<LIST.FILE>
						;         [<PROOF.COMMENT> ]]]]]
						; EFFECT: CREATES A PROBLEM DESCRIPTION OF THE EDITED
						;         FORMULAS, OPENS THE CODE FILE FOR PROTOCOL,
						;         CONSTRUCTS THE INITIAL GRAPH, STARTS THE
						;         REFUTATION, CLOSES THE CODE FILE AND STARTS
						;         A PROTOCOL PROCESS.
						; VALUE:  UNDEFINED.
						; VALUE:  UNDEFINED.
  (PROG
    ((PROBLEM.FILE (FIRST PARAM.LIST))
     (GRAPH.FILE (SECOND PARAM.LIST))
     (CODE.FILE (THIRD PARAM.LIST))
     (LIST.FILE (FOURTH PARAM.LIST))
     (PROOF.COMMENT (FIFTH PARAM.LIST))
     (REST (NTHCDR (1- 6) PARAM.LIST)))
    (WHEN (OS=EDIT.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL PROBLEM.FILE GRAPH.FILE CODE.FILE LIST.FILE REST)
      (OS=EDIT.PROC PROBLEM.FILE)
      (OS=CONSTRUCT.and.REFUTE graph.file code.file proof.comment)
      (OS=PROTOCOL.PROC OS*ACTUAL.CODE.FILE LIST.FILE))))

(DEFUN OS=EDIT.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL (PROBLEM.FILE GRAPH.FILE CODE.FILE LIST.FILE REST)
						; INPUT:  PROBLEM.FILE GRAPH.FILE CODE.FILE
						;         LIST.FILE REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (PROG ((RESULT T))
	(COND
	  (REST
	   (OS=WRITE.TEXT
	     (OS.E-GET.EXPLANATION 'EDIT.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
	   (SETQ RESULT NIL)))
	(setq result (and result
			  (os=output.file.name.check PROBLEM.FILE
						     'EP 'PROBLEM)
			  (os=output.file.name.check GRAPH.FILE
						     'Ep 'GRAPH)
			  (os=output.file.name.check CODE.FILE
						     'Ep 'CODE)
			  (os=output.file.name.check LIST.FILE
						     'Ep 'LIST)))
	(WHEN (NOT RESULT)
	  (OS=WRITE.TEXT
	    (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'EDIT.CONSTRUCT.REFUTE.PROTOCOL) *standard-output*))
	(RETURN RESULT)))

(DEFUN OS=EDIT.PROC (PROBLEM.FILE)
						; INPUT:  NAME OF THE PROBLEM FILE.
						; EFFECT: EDIT THE PROBLEM FILE AND GLOBAL:SENDS A WARNING
						;         MESSAGE IF THERE ARE NO AXIOMS OR THEOREMS.
						; VALUE:  UNDEFINED.
  (LET (AXIOMS THEOREMS)
    (WHEN (OS=SYSTEM.IS.READY.FOR 'EDIT)
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'EDIT.PROC_START.EDIT.AXIOMS) *standard-output*)
      (SETQ AXIOMS (MKRP-WITH.PATHNAME PROBLEM.FILE (EDT-EDIT NIL)))
      (COND ((NULL AXIOMS) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'EDIT.PROC_WARNING=NO.AXIOMS) *standard-output*)))
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'EDIT.PROC_START.EDIT.THEOREMS) *standard-output*)
      (SETQ THEOREMS (MKRP-WITH.PATHNAME PROBLEM.FILE (EDT-EDIT t)))
      (UNLESS THEOREMS
	(OS=WRITE.TEXT (OS.E-GET.EXPLANATION (IF AXIOMS 'EDIT.PROC_WARNING=NO.THEOREMS 'EDIT.PROC_ERROR=NOTHING.TO.PROVE))
		       *standard-output*))
      (IF (OR AXIOMS THEOREMS)
	  (OS=WRITE.PROBLEM.FILE axioms THEOREMS (OS.E-GET.EXPLANATION 'EDIT.PROC_PROOF.COMMENT (DATE)) PROBLEM.FILE)
	  NIL))))

(DEFUN OS=FORMULA (PARAM.LIST)
						; INPUT:  LIST OF 2-3 ATOMS
						;         <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>]
						; VALUE:  UNDEFINED
						; EFFECT: CREATES A PROBLEM DESCRIPTION FROM COMPILED
						;         FORMULA FILES.						;
  (LET ((AXIOM.FILE (CAR PARAM.LIST))
	(THEOREM.FILE (SECOND PARAM.LIST))
	(PROBLEM.FILE (THIRD PARAM.LIST))
	(REST (CDDDR PARAM.LIST)))
    (when (OS=FORMULA.INPUT.CONTROL AXIOM.FILE THEOREM.FILE PROBLEM.FILE REST)
      (OS=FORMULA.PROC AXIOM.FILE THEOREM.FILE PROBLEM.FILE))))

(DEFUN OS=FORMULA.INPUT.CONTROL (AXIOM.FILE THEOREM.FILE PROBLEM.FILE REST)
						; INPUT:  AXIOM.FILE THEOREM.FILE PROBLEM.FILE REST
						; EFFECT: CHECKING THE INPUT FOR FORMULA AND
						;         GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (let ((RESULT T))
    (when REST
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'FORMULA.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
      (SETQ RESULT NIL))
    (setq result (and result
		      (OS=OPEN.INPUT.FILE axiom.file nil 'formula 'axiom (mkrp-default.ax) (mkrp-default.lisp))
		      (OS=OPEN.INPUT.FILE theorem.file nil 'formula 'axiom (mkrp-default.th) (mkrp-default.lisp))
		      (os=output.file.name.check PROBLEM.FILE 'FORMULA 'PROBLEM)))
    (unless RESULT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'FORMULA) *standard-output*))
    RESULT))

(DEFUN OS=FORMULA.CONSTRUCT (PARAM.LIST)
						; INPUT:  LIST OF 3-6 ATOMS:
						;         <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>
						;         [<GRAPH.FILE> [<CODE.FILE>
						;         [<PROOF.COMMENT> ]]]]
						; EFFECT: CREATES A PROBLEM DESCRIPTION OF COMPILED
						;         FORMULA FILES, OPENS THE CODE FILE FOR
						;         PROTOCOL, CONSTRUCTS THE INITIAL GRAPH AND
						;         CLOSES THE CODE FILE.
						; VALUE:  UNDEFINED.
  (PROG
    ((AXIOM.FILE (FIRST PARAM.LIST)) (THEOREM.FILE (SECOND PARAM.LIST))
     (PROBLEM.FILE (THIRD PARAM.LIST)) (GRAPH.FILE (FOURTH PARAM.LIST))
     (CODE.FILE (FIFTH PARAM.LIST)) (PROOF.COMMENT (SIXTH PARAM.LIST)) (REST (NTHCDR (1- 7) PARAM.LIST)))
    (COND
      ((OS=FORMULA.CONSTRUCT.INPUT.CONTROL AXIOM.FILE THEOREM.FILE PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
       (OS=FORMULA.PROC AXIOM.FILE THEOREM.FILE PROBLEM.FILE)
       (OS=OPEN.CODE.FILE CODE.FILE)
       (OS=CONSTRUCT.PROC OS*ACTUAL.PROBLEM.FILE GRAPH.FILE OS*ACTUAL.CODE.FILE PROOF.COMMENT nil)
       (OS=CLOSE.CODE.FILE)))))

(DEFUN OS=FORMULA.CONSTRUCT.INPUT.CONTROL (AXIOM.FILE THEOREM.FILE PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
						; INPUT:  AXIOM.FILE THEOREM.FILE PROBLEM.FILE
						;         GRAPH.FILE CODE.FILE REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (PROG ((RESULT T))
	(COND
	  (REST
	   (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
	   (SETQ RESULT NIL)))
	(setq result (and result
			  (OS=OPEN.INPUT.FILE axiom.file nil 'formula.construct 'axiom (mkrp-default.ax) (mkrp-default.lisp))
			  (OS=OPEN.INPUT.FILE theorem.file nil 'formula.construct 'axiom (mkrp-default.th) (mkrp-default.lisp))
			  (os=output.file.name.check PROBLEM.FILE 'FORMULA.CONSTRUCT 'PROBLEM)
			  (os=output.file.name.check GRAPH.FILE 'FORMULA.CONSTRUCT 'GRAPH)
			  (os=output.file.name.check CODE.FILE 'FORMULA.CONSTRUCT 'CODE)))
	(COND
	  ((NOT RESULT)
	   (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'FORMULA.CONSTRUCT) *standard-output*)))
	(RETURN RESULT)))

(DEFUN OS=FORMULA.CONSTRUCT.REFUTE (PARAM.LIST)
						; INPUT:  LIST OF 3-6 ATOMS:
						;         <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>
						;         [<PROOF.COMMENT>]]]
						; EFFECT: CREATES A PROBLEM DESCRIPTION OF COMPILED
						;         FORMULA FILES, OPENS CODE FILE FOR PROTOCOL,
						;         CONSTRUCTS THE INITIAL GRAPH, STARTS THE
						;         REFUTATION AND CLOSES THE CODE FILE.
						; VALUE:  UNDEFINED.
  (LET ((AXIOM.FILE (FIRST PARAM.LIST))
	(THEOREM.FILE (SECOND PARAM.LIST))
	(PROBLEM.FILE (THIRD PARAM.LIST))
	(GRAPH.FILE (FOURTH PARAM.LIST))
	(CODE.FILE (FIFTH PARAM.LIST))
	(PROOF.COMMENT (SIXTH PARAM.LIST))
	(REST (NTHCDR 8 PARAM.LIST)))
    (WHEN (OS=FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL AXIOM.FILE THEOREM.FILE PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
      (OS=FORMULA.PROC AXIOM.FILE THEOREM.FILE PROBLEM.FILE)
      (OS=CONSTRUCT.and.REFUTE graph.file code.file proof.comment))))

(DEFUN OS=FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL
       (AXIOM.FILE THEOREM.FILE PROBLEM.FILE GRAPH.FILE CODE.FILE REST)
						; INPUT:  AXIOM.FILE THEOREM.FILE PROBLEM.FILE
						;         GRAPH.FILE CODE.FILE
						;         REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (PROG ((RESULT T))
	(COND
	  (REST
	   (OS=WRITE.TEXT
	     (OS.E-GET.EXPLANATION 'FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
	   (SETQ RESULT NIL)))
	(setq result (and result
			  (OS=OPEN.INPUT.FILE axiom.file nil 'fr 'axiom (mkrp-default.ax) (mkrp-default.lisp))
			  (OS=OPEN.INPUT.FILE theorem.file nil 'fr 'axiom (mkrp-default.th) (mkrp-default.lisp))
			  (os=output.file.name.check PROBLEM.FILE
						     'FR 'PROBLEM)
			  (os=output.file.name.check GRAPH.FILE
						     'Fr 'GRAPH)
			  (os=output.file.name.check CODE.FILE 'Fr 'CODE)))
	(WHEN (NOT RESULT)
	  (OS=WRITE.TEXT
	    (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'FORMULA.CONSTRUCT.REFUTE) *standard-output*))
	(RETURN RESULT)))

(DEFUN OS=FORMULA.CONSTRUCT.REFUTE.PROTOCOL (PARAM.LIST)
						; INPUT:  LIST OF 3-9 ATOMS:
						;         <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>
						;         [<GRAPH.FILE> [<CODE.FILE> [<LIST.FILE>
						;         [<PROOF.COMMENT>]]]]]
						; EFFECT: CREATES A PROBLEM DESCRIPTION OF COMPILED
						;         FORMULA FILES, OPENS CODE FILE FOR PROTOCOL,
						;         CONSTRUCTS THE INITIAL GRAPH, STARTS THE
						;         REFUTATION, CLOSES THE CODE FILE AND STARTS
						;         THE PROTOCOL PROCESS.
						; VALUE:  UNDEFINED.
  (PROG
    ((AXIOM.FILE (FIRST PARAM.LIST)) (THEOREM.FILE (SECOND PARAM.LIST))
     (PROBLEM.FILE (THIRD PARAM.LIST)) (GRAPH.FILE (FOURTH PARAM.LIST))
     (CODE.FILE (FIFTH PARAM.LIST)) (LIST.FILE (SIXTH PARAM.LIST))
     (PROOF.COMMENT (SEVENTH PARAM.LIST))
     (REST (NTHCDR 7 PARAM.LIST)))
    (WHEN (OS=FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL AXIOM.FILE THEOREM.FILE PROBLEM.FILE GRAPH.FILE
							      CODE.FILE LIST.FILE REST)
      (OS=FORMULA.PROC AXIOM.FILE THEOREM.FILE PROBLEM.FILE)
      (OS=CONSTRUCT.and.REFUTE graph.file code.file proof.comment)
      (OS=PROTOCOL.PROC OS*ACTUAL.CODE.FILE LIST.FILE))))

(DEFUN OS=FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL (AXIOM.FILE THEOREM.FILE PROBLEM.FILE
							   GRAPH.FILE CODE.FILE LIST.FILE REST)
						; INPUT:  AXIOM.FILE THEOREM.FILE PROBLEM.FILE
						;         GRAPH.FILE CODE.FILE LIST.FILE REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (PROG ((RESULT T))
	(COND
	  (REST
	   (OS=WRITE.TEXT
	     (OS.E-GET.EXPLANATION 'FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
	   (SETQ RESULT NIL)))
	(setq result (and result
			  (OS=OPEN.INPUT.FILE axiom.file nil 'fp 'axiom (mkrp-default.ax) (mkrp-default.lisp))
			  (OS=OPEN.INPUT.FILE theorem.file nil 'fp 'axiom (mkrp-default.th) (mkrp-default.lisp))
			  (os=output.file.name.check PROBLEM.FILE 'FP 'PROBLEM)
			  (os=output.file.name.check GRAPH.FILE 'Fp 'GRAPH)
			  (os=output.file.name.check
			    CODE.FILE 'fp 'code)
			  (os=output.file.name.check LIST.FILE
						     'Fp 'LIST)))
	(UNLESS RESULT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED
							    'FORMULA.CONSTRUCT.REFUTE.PROTOCOL) *standard-output*))
	(RETURN RESULT)))

(DEFUN OS=FORMULA.PROC (AXIOM.FILE THEOREM.FILE PROBLEM.FILE)
						; INPUT:  AXIOM.FILE THEOREM.FILE PROBLEM.FILE
						; EFFECT: CREATES THE PROBLEM FILE FROM COMPILED
						;         FORMULA FILES.
						; VALUE:  UNDEFINED.
  (let ((EDT.RESULT (EDT-GET.FILES AXIOM.FILE THEOREM.FILE)))
    (OS=WRITE.PROBLEM.FILE (first edt.result) (SECOND EDT.RESULT)
			   (OS.E-GET.EXPLANATION 'FORMULA.PROC_PROOF.COMMENT (DATE) AXIOM.FILE THEOREM.FILE) PROBLEM.FILE)))

(DEFUN OS=PROTOCOL (PARAM.LIST)			; INPUT:  LIST OF 0-2 ATOMS:
						;         [<CODE.FILE> [<LIST.FILE>]]
						; EFFECT: CREATES A PROOF PROTOCOL.
						; VALUE:  UNDEFINED.
  (LET ((CODE.FILE (FIRST PARAM.LIST))
	(LIST.FILE (SECOND PARAM.LIST))
	(REST (NTHCDR 3 PARAM.LIST)))
    (WHEN (OS=PROTOCOL.INPUT.CONTROL CODE.FILE LIST.FILE REST)
      (OS=PROTOCOL.PROC OS*ACTUAL.CODE.FILE LIST.FILE))))

(DEFUN OS=PROTOCOL.INPUT.CONTROL (CODE.FILE LIST.FILE REST)
						; INPUT:  CODE.FILE LIST.FILE REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (LET ((RESULT T))
    (WHEN REST
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
      (SETQ RESULT NIL))
    (SETQ RESULT (and result
		      (OS=OPEN.INPUT.FILE CODE.FILE
					  'OS*ACTUAL.CODE.FILE
					  'PROTOCOL
					  'CODE
					  (mkrp-default.code)
					  (mkrp-default.lisp))
		      (os=output.file.name.check LIST.FILE 'PROTOCOL 'LIST)))
    (unless RESULT (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'PROTOCOL) *standard-output*))
    RESULT))

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

(DEFUN OS=REFUTE (PARAM.LIST)			; INPUT_  LIST OF 0-3 ATOMS:
						;         [<GRAPH.FILE> [<SPLITPART.ID> [<CODE.FILE>]]]
						; VALUE:  UNDEFINED.
						; EFFECT: OPENS THE CODE FILE FOR PROTOCOL, REFUTES
						;         THE INITIAL GRAPHS AND CLOSES THE CODE FILE.
  (LET ((GRAPH.FILE (FIRST PARAM.LIST))
	(SPLITPART.ID (SECOND PARAM.LIST))
	(CODE.FILE (THIRD PARAM.LIST))
	(REST (NTHCDR 3 PARAM.LIST)))
    (WHEN (OS=REFUTE.INPUT.CONTROL GRAPH.FILE CODE.FILE SPLITPART.ID REST)
      (OS=OPEN.CODE.FILE CODE.FILE)
      (OS=REFUTE.PROC OS*ACTUAL.GRAPH.FILE SPLITPART.ID OS*ACTUAL.CODE.FILE)
      (OS=CLOSE.CODE.FILE))))

(DEFUN OS=REFUTE.INPUT.CONTROL (GRAPH.FILE CODE.FILE SPLITPART.ID REST)
						; INPUT:  GRAPH.FILE CODE.FILE SPLITPART.ID REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (LET ((RESULT T))
    (WHEN REST (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*) (SETQ RESULT NIL))
    (SETQ RESULT (and (OS=OPEN.INPUT.FILE GRAPH.FILE 'OS*ACTUAL.GRAPH.FILE
					  'REFUTE
					  'GRAPH
					  (mkrp-default.graph) (mkrp-default.lisp))
		      (os=output.file.name.check CODE.FILE 'REFUTE 'CODE)))
    (COND
      (SPLITPART.ID
       (COND
	 ((NOT (NUMBERP SPLITPART.ID))
	  (OS=WRITE.TEXT
	    (OS.E-GET.EXPLANATION 'REFUTE.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID SPLITPART.ID) *standard-output*)
	  (SETQ RESULT NIL)))))
    (COND
      ((NOT RESULT) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'REFUTE) *standard-output*)))
    RESULT))

(DEFUN OS=REFUTE.PROTOCOL (PARAM.LIST)		; INPUT:  LIST OF 0-4 ATOMS:
						;         [<GRAPH.FILE> [<SPLITPART.ID> [<CODE.FILE>
						;         [<LIST.FILE>]]]]
						; EFFECT: OPENS THE CODE FILE FOR PROTOCOL, STARTS THE
						;         REFUTATION, CLOSES THE CODE FILE AND STARTS
						;         A PROTOCOL PROCESS.
						; VALUE:  UNDEFINED.
  (PROG
    ((GRAPH.FILE (FIRST PARAM.LIST))
     (SPLITPART.ID (SECOND PARAM.LIST))
     (CODE.FILE (THIRD PARAM.LIST))
     (LIST.FILE (FOURTH PARAM.LIST))
     (REST (NTHCDR 4 PARAM.LIST)))
    (COND
      ((OS=REFUTE.PROTOCOL.INPUT.CONTROL GRAPH.FILE CODE.FILE LIST.FILE SPLITPART.ID REST)
       (COND
	 (T (OS=OPEN.CODE.FILE CODE.FILE)
	    (OS=REFUTE.PROC OS*ACTUAL.GRAPH.FILE SPLITPART.ID OS*ACTUAL.CODE.FILE)
            (OS=CLOSE.CODE.FILE)
	    (OS=PROTOCOL.PROC OS*ACTUAL.CODE.FILE LIST.FILE)))))))

(DEFUN OS=REFUTE.PROTOCOL.INPUT.CONTROL
       (GRAPH.FILE CODE.FILE LIST.FILE SPLITPART.ID REST)
						; INPUT:  GRAPH.FILE CODE.FILE LIST.FILE SPLITPART.ID
						;         REST
						; EFFECT: CHECKING THE INPUT
						;         AND GLOBAL:SENDING A MESSAGE BY ERROR.
						; VALUE:  T IF THE INPUT IS OK, ELSE NIL.
  (let ((RESULT T))
    (WHEN REST
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS REST) *standard-output*)
      (SETQ RESULT NIL))
    (SETQ RESULT (and result
		      (OS=OPEN.INPUT.FILE GRAPH.FILE 'OS*ACTUAL.GRAPH.FILE
					  'rp
					  'graph
					  (mkrp-default.graph) (mkrp-default.lisp))
		      (os=output.file.name.check CODE.FILE 'REFUTE.PROTOCOL 'CODE)
		      (os=output.file.name.check LIST.FILE 'REFUTE.PROTOCOL 'LIST)))
    (WHEN (AND SPLITPART.ID (NOT (NUMBERP SPLITPART.ID)))
      (OS=WRITE.TEXT
	(OS.E-GET.EXPLANATION 'REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID SPLITPART.ID) *standard-output*)
      (SETQ RESULT NIL))
    (WHEN (NOT RESULT)
      (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'COMMON.ERROR.MESSAGE_COMMAND.IGNORED 'REFUTE.PROTOCOL) *standard-output*))
    RESULT))

(DEFUN OS=REFUTE.PROC (GRAPH.FILE SPLITPART.ID CODE.FILE)
						; INPUT:  LIST OF 0-3 ATOMS:          [<GRAPH.FILE>
						;         [<SPLITPART.ID> [<CODE.FILE> ]]]
						; EFFECT: CHECKING THE SYSTEM FOR REFUTATION AND
						;         GLOBAL:SENDING THE PROOF-RESULT.
						; VALUE:  UNDEFINED.
  (PROG (PROOFRESULT)
	(WHEN (OS=SYSTEM.IS.READY.FOR 'REFUTE)
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROC_START.MESSAGE) *standard-output*)
	  (COND ((NOT SPLITPART.ID) (SETQ PROOFRESULT (CTL-REFUTE.GRAPHS.ON.FILE GRAPH.FILE CODE.FILE)))
		(T (SETQ PROOFRESULT (CTL-REFUTE.GRAPH.ON.FILE GRAPH.FILE SPLITPART.ID CODE.FILE))))
	  (COND
	    ((EQ PROOFRESULT 'SUCCESS) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROC_THEOREM.PROVED) *standard-output*))
	    (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROC_THEOREM.NOT.PROVED) *standard-output*))))))


(DEFUN OS=REFUTE.PROC.cr (GRAPH.FILE CODE.FILE)
						; INPUT:  LIST OF 0-3 ATOMS:          [<GRAPH.FILE>
						;         [<SPLITPART.ID> [<CODE.FILE> ]]]
						; EFFECT: CHECKING THE SYSTEM FOR REFUTATION AND
						;         GLOBAL:SENDING THE PROOF-RESULT.
						; VALUE:  UNDEFINED.
  (PROG (PROOFRESULT)
	(WHEN (OS=SYSTEM.IS.READY.FOR 'REFUTE)
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROC_START.MESSAGE) *standard-output*)
	  (SETQ PROOFRESULT (CTL-REFUTE.GRAPHS.ON.FILE1 GRAPH.FILE CODE.FILE))
	  (COND
	    ((EQ PROOFRESULT 'SUCCESS) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROC_THEOREM.PROVED) *standard-output*))
	    (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'REFUTE.PROC_THEOREM.NOT.PROVED) *standard-output*))))))

(DEFUN OS=SYSTEM.IS.READY.FOR (ACTION)
						; INPUT:  ONE OF THE SYSTEMPARTS:
						;         EDIT, CONSTRUCT, REFUTE OR PROTOCOL.
						; EFFECT: IF THE SYSTEMPART IS NOT LOADED ALL
						;         SPLITPARTS ARE REMOVED AND THE SYSTEMPART
						;         IS ADDED TO THE SYSTEM.
						; VALUE:  T IF SYSTEMPART IS LOADED, ELSE NIL.   
  (DECLARE (IGNORE ACTION))
  T)

(DEFUN OS=OPEN.CODE.FILE (CODEFILENAME)
						; INPUT:  A codeFILENAME.
						; EFFECT: OPENS THE FILE. IF THE FILENAME IS NIL,
						;         A STANDARD FILENAME IS USED.
						; VALUE:  UNDEFINED.
  (COND
    ((OPT-GET.OPTION PR_PROTOCOL)
     (SETQ OS*ACTUAL.CODE.FILE (MKRP-OPENOUT (MKRP-MAKE.PATHNAME T (mkrp-default.code) (mkrp-default.lisp)
								 CODEFILENAME)
					     NIL)))
    (T (SETQ OS*ACTUAL.CODE.FILE NIL))))

(DEFUN OS=CLOSE.CODE.FILE NIL
						; INPUT:  NONE.
						; EFFECT: CLOSES THE FILE.
						; VALUE:  UNDEFINED.
  (WHEN OS*ACTUAL.CODE.FILE
    (LET ((FILE.VERSION (TRUENAME OS*ACTUAL.CODE.FILE)))
      (CLOSEFILE OS*ACTUAL.CODE.FILE)
      (SETQ OS*ACTUAL.CODE.FILE (make-pathname :defaults FILE.VERSION :version :newest)))))

(DEFUN OS=OPEN.LIST.FILE (LISTFILENAME)
						; INPUT:  A LIST FILENAME.
						; EFFECT: OPENS THE FILE. IF THE FILENAME IS NIL,
						;         A STANDARD FILENAME IS USED.
						; VALUE:  UNDEFINED.
  (COND
    ((OPT-GET.OPTION PR_PROTOCOL)
     (SETQ OS*ACTUAL.LIST.FILE (MKRP-OPENOUT (MKRP-MAKE.PATHNAME T (mkrp-default.list) (if (opt-get.option pr_latex) "tex" (mkrp-default.text)) LISTFILENAME)
					     NIL (MKRP-DEFAULT.TEXT) (opt-get.option pr_latex))))
    (T (SETQ OS*ACTUAL.LIST.FILE NIL))))

(DEFUN OS=CLOSE.LIST.FILE NIL
						; INPUT:  NONE.
						; EFFECT: CLOSES THE FILE.
						; VALUE:  UNDEFINED.
  (WHEN OS*ACTUAL.LIST.FILE
    (LET ((FILE.VERSION (TRUENAME OS*ACTUAL.LIST.FILE)))
      (CLOSEFILE OS*ACTUAL.LIST.FILE)
      (SETQ OS*ACTUAL.LIST.FILE FILE.VERSION))))

(DEFUN OS=OPEN.PROBLEM.FILE (PROBLEMFILENAME)
						; INPUT:  A PROBLEM FILENAME.
						; EFFECT: OPENS THE FILE. IF THE FILENAME IS NIL,
						;         A STANDARD FILENAME IS USED.
						; VALUE:  UNDEFINED.
  (SETQ OS*ACTUAL.PROBLEM.FILE
	(MKRP-OPENOUT (MKRP-MAKE.PATHNAME T (mkrp-default.problem) (mkrp-default.lisp) PROBLEMFILENAME)
					   NIL)))

(DEFUN OS=CLOSE.PROBLEM.FILE NIL
						; INPUT:  NONE.
						; EFFECT: CLOSES THE FILE.
						; VALUE:  UNDEFINED.
  (LET ((FILE.VERSION (TRUENAME OS*ACTUAL.PROBLEM.FILE)))
    (CLOSEFILE OS*ACTUAL.PROBLEM.FILE)
    (SETQ OS*ACTUAL.PROBLEM.FILE FILE.VERSION)))

(DEFUN OS=OPEN.GRAPH.FILE (GRAPHFILENAME)
						; INPUT:  A GRAPH FILENAME.
						; EFFECT: OPENS THE FILE. IF THE FILENAME IS NIL,
						;         A STANDARD FILENAME IS USED.
						; VALUE:  UNDEFINED.
  (SETQ OS*ACTUAL.GRAPH.FILE (MKRP-OPENOUT (MKRP-MAKE.PATHNAME T (mkrp-default.graph) (mkrp-default.lisp) GRAPHFILENAME)
					   nil "LISP" (eq (opt-get.option gen_other.prover) 'c))))

(DEFUN OS=CLOSE.GRAPH.FILE NIL
						; INPUT:  NONE.
						; EFFECT: CLOSES THE FILE.
						; VALUE:  UNDEFINED.
  (LET ((FILE.VERSION (TRUENAME OS*ACTUAL.GRAPH.FILE)))
    (CLOSEFILE OS*ACTUAL.GRAPH.FILE)
    (SETQ OS*ACTUAL.GRAPH.FILE FILE.VERSION)))

(DEFUN OS=WRITE.PROBLEM.FILE (AXIOMS THEOREMS COMMENT PROBLEM.FILE)
						; input:  axioms, theorems, comment and a problemfile name.
						; effect: opens the problem file for problem-
						;         specification and writes the s-expression
						;         (prep-problem.specification  axioms.infix   axioms.prefix
						;                                      theorems.infix theorems.prefix
						;                                      comment        expression)
						;         on the file, where expression is a dummy
						;         argument which when evaluated creates the
						;         very same data-objects, as there are
						;         during the invocation of this function.
						; value:  undefined.
  (PROG
    ((AXIOMS.INFIX (MAPCAR (FUNCTION CAR) AXIOMS)) OUTPUTSTREAM
     (AXIOMS.PREFIX (MAPCAR (FUNCTION CADR) AXIOMS))
     (THEOREMS.INFIX (MAPCAR (FUNCTION CAR) THEOREMS))
     (THEOREMS.PREFIX (MAPCAR (FUNCTION CADR) THEOREMS)))
    (OS=OPEN.PROBLEM.FILE PROBLEM.FILE)
    (SETQ OUTPUTSTREAM OS*ACTUAL.PROBLEM.FILE)
    (PRINC "(" OUTPUTSTREAM)
    (PRINC "PREP-PROBLEM.SPECIFICATION" OUTPUTSTREAM)
    (PRINC " " OUTPUTSTREAM)
    (PRINC `',AXIOMS.INFIX OUTPUTSTREAM)
    (PRINC " " OUTPUTSTREAM)
    (PRINC `',AXIOMS.PREFIX OUTPUTSTREAM)
    (PRINC " " OUTPUTSTREAM)
    (PRINC `',THEOREMS.INFIX OUTPUTSTREAM)
    (PRINC " " OUTPUTSTREAM)
    (PRINC `',THEOREMS.PREFIX OUTPUTSTREAM)
    (PRINC " " OUTPUTSTREAM)
    (PRIN1 `',COMMENT OUTPUTSTREAM)
    (PRINC " " OUTPUTSTREAM)
    (PRINC "(" OUTPUTSTREAM)
    (PRINC 'PROGN OUTPUTSTREAM)
    (DT-SAVE OUTPUTSTREAM)
    (PRINC "))" OUTPUTSTREAM)
    (OS=CLOSE.PROBLEM.FILE)))


(DEFUN OS=OPTION NIL
						; INPUT:  NONE
						; VALUE:  UNDEFINED
						; EFFECT: TOP LOOP FOR THE OPTIONS-MODULE. READS AND
						;         EXECUTES ALL OPTIONS-COMMANDS.
						;         IS TERMINATED WITH THE COMMAND 'OK' ]
  (PROG (SWITCH VALUE com.list)
	(OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPTION_ENTER.MODULE) *standard-output*)
     NEXTINPUT
	(PRINC "& " T)
	(setq com.list (READLINE t)
	      switch   (first com.list)
	      com.list (rest com.list))
	(CASE switch
	  (OK          (RETURN NIL))
	  (V           (OS=VDT (first com.list)))
	  ((H HELP)    (OS=OPT.HELP (first com.list)))
	  ((P PRINT)   (OS=OPT.PRINT (first com.list)))
	  ((PP PPRINT) (OS=OPT.PRETTYPRINT (first com.list)))
	  ((R READ)    (OS=OPT.READ (first com.list)))
	  ((W WRITE)   (OS=OPT.WRITE (first com.list)))
	  ((LISP L)    (ENTERLISP 'OK))
	  (OTHERWISE   (COND ((MEMBER SWITCH (OPT-ALL.AREAS)) (OS=OPT.PRINT SWITCH))
			     ((MEMBER SWITCH (OPT-ALL.OPTIONS)) (SETQ VALUE (first com.list))
			      (COND ((EVAL `(OPT-PUT.OPTION ,SWITCH ',VALUE)) T)
				    (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPTION_ILLEGAL.OPTION.VALUE SWITCH) *standard-output*))))
			     (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPTION_GET.HELP SWITCH) *standard-output*)
				(setq com.list (READLINE t)
				      switch   (first com.list)
				      com.list (rest com.list))
				(GO NEXTINPUT)
						; AVOIDS EVALUATION OF NON-EXISTENT SWITCH
				))))
	(COND ((AND (NOT (MEMBER SWITCH (OPT-ALL.AREAS)))
		    (NOT (MEMBER SWITCH '(H V ? HELP P PRINT PP PPRINT R READ W WRITE L LISP))))
	       (PRINC SWITCH T) (PRINC " = " T) (PRINC (EVAL `(OPT-GET.OPTION ,SWITCH)) T) (TERPRI T)))
	(GO NEXTINPUT)))


(DEFUN OS=OPT.PRINT (AREA)
						; INPUT: -ATOM, PREFERABLY A LEGAL OPTION-AREA NAME.
						; VALUE:  UNDEFINED
						; EFFECT: IF INPUT=NIL :
						;             PRINTS ALL OPTION-AREAS, THEIR OPTIONS
						;             AND THEIR CURRENT VALUES ON TERMINAL,
						;         ELSEIF INPUT=OPTION-AREA NAME :
						;              PRINTS THIS OPTION-AREA AS ABOVE,
						;         ELSEIF INPUT='MOD' OR 'MODULES':
						;             PRINTS A HELPING HINT ON TERMINAL.
  (COND ((MEMBER AREA (OPT-ALL.AREAS)) (OS=PRINT.OPTION.AREA AREA T))
	((NULL AREA) (MAPC (FUNCTION (LAMBDA (AREA) (OS=PRINT.OPTION.AREA AREA T))) (OPT-ALL.AREAS)))
	(T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.PRINT_GET.HELP AREA) *standard-output*))))

(DEFUN OS=PRINT.OPTION.AREA (AREA FILE)
						; INPUT:-ATOM, NAME OF AN OPTION-AREA.
						;       -ATOM, A PRINT-FILE-NAME, PRESUMABLY =T.
						; VALUE: UNDEFINED
						; EFFECT: PRINTS THE AREA WTIH A HEADLINE AND ALL
						;        AREA-OPTIONS TOGETHER WITH THEIR CURRENT
						;        VALUES.
  (OS=WRITE.TEXT (OPT-GET.AREA.HEADLINE AREA) FILE)
  (MAPC #'(LAMBDA (OP) (format file "~A  ~A~%" op (EVAL `(OPT-GET.OPTION ,OP))))
	(OPT-GET.AREA.OPTIONS AREA)))

(DEFUN OS=OPT.READ (&OPTIONAL FILE)
						; INPUT: -ATOM, NAME OF AN OPTION-FILE.
						; VALUE:  UNDEFINED
						; EFFECT  READS THE OPTIONS FROM FILE AND, IF ALL(]])
						;         OPTIONS ARE LEGAL, SETS THEM TO THEIR VALUES
						;         STORED ON FILE.
						; REAMRKS: THE OPT-FILE SETS WHEN LOADED THE VAR
						;         ALL.OPTIONS.SET? TO T IF ALL OPTIONS ARE
						;         LEGAL(SEE OPT-PUT.LIST.OPTIONS,OS=OPT.WRITE)
						;         AND SETS THEIR VALUES. IF THIS IS NOT THE
						;         CASE THE OPTIONS REMAIN UNCHANGED AND AN
						;         ERROR MESSAGE IS PRINTED.
  (LET ((PFILE FILE) (OPT*ALL.OPTIONS.SET? nil))
    (DECLARE (SPECIAL OPT*ALL.OPTIONS.SET?))
    (COND
      ((OR (NULL PFILE) (NOT (FILENAME.CHECK PFILE)))
       (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.READ_FILENAME?) *standard-output*)
       (SETQ PFILE (READ T)))
      (T NIL))
    (COND
      ((OR (NULL PFILE) (FILENAME.CHECK PFILE))
       (when pfile
	 (unless (stringp pfile) (setq pfile (string-downcase (princ-to-string pfile))))
	 (setq pfile (pathname pfile)))
       (if (probe-file (MKRP-MAKE.PATHNAME T (mkrp-default.options) (mkrp-default.lisp) PFILE))
	   (progn (MKRP-LOAD.FILE (MKRP-MAKE.PATHNAME T (mkrp-default.options) (mkrp-default.lisp) PFILE))
		  (COND (OPT*ALL.OPTIONS.SET? (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.READ_OPTIONS.SET) *standard-output*))
			(T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.READ_INCONSISTENT.OPTIONS) *standard-output*))))
	   (warn "Options file ~A  does not exist." (MKRP-MAKE.PATHNAME T (mkrp-default.options) (mkrp-default.lisp) PFILE))))
      (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT_ERROR=ILLEGAL.FILENAME PFILE (READLINE)) *standard-output*)))))

(DEFUN OS=OPT.WRITE (&OPTIONAL FILE)
						; INPUT: -ATOM, NAME OF AN SAVE-FILE FOR THE OPTIONS.
						; VALUE:  UNDEFINED
						; EFFECT: WRITES ALL OPTIONS AND THEIR CURRENT VALUES
						;         ON FILE.
						; REMARKS: WHEN <FILE> IS LOADED AGAIN THE VARIABLE
						;         ALL.OPTIONS.SET? IS SET TO THE VALUE OF OPT-
						;         PUT.LIST.OPTIONS. IT INDICATES WHETHER THE
						;         OPTIONS ARE ALL (]]) LEGAL. ONLY IF THIS
						;         VARIBLE IS =T THE OPTIONS ARE SET]]
						;         PLEASE CHECK --- OS=OPT.READ ---
  (let ((PFILE FILE))
    (COND ((OR (NULL PFILE) (NOT (FILENAME.CHECK PFILE)))
	   (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.WRITE_FILENAME?) *standard-output*)
	   (SETQ PFILE (READ T)))
	  (T NIL))
    (COND ((OR (NULL PFILE) (FILENAME.CHECK PFILE))
	   (when pfile
	     (unless (stringp pfile) (setq pfile (string-downcase (princ-to-string pfile))))
	     (setq pfile (pathname pfile)))
	   (SETQ PFILE (MKRP-OPENOUT (MKRP-MAKE.PATHNAME T (mkrp-default.options) (mkrp-default.lisp) PFILE)
				     NIL))
	   (PROGN (PRIN1 (LIST 'SETQ 'OPT*ALL.OPTIONS.SET? (OPT-SAVE NIL)) PFILE)
		  (TERPRI PFILE))
	   (CLOSEFILE PFILE)
	   (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT_OUTPUT.END) *standard-output*))
	  (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT_ERROR=ILLEGAL.FILENAME PFILE (READLINE)) *standard-output*)))
    (TERPRI T)))

(DEFUN OS=OPT.PRETTYPRINT (&OPTIONAL (FILE nil))
						; Edited:  26-AUG-1990 00:02
						; Authors: PRCKLN
						; INPUT:   Name of a print-file for the options
						;          and their explanations.
						; Value:   Undefined
						; Effect:  Writes for all areas the area-options, their
						;          default values, an explanation of their
						;          arguments and the effects of the options on
						;          <FILE> in pretty-print format.
  (let ((PFILE FILE))
    (COND ((OR (NULL PFILE) (NOT (FILENAME.CHECK PFILE)))
	   (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.PRETTYPRINT_FILENAME?) *standard-output*)	
	   (SETQ PFILE (READ T)))
	  (T NIL))
    (when (eq t pfile) (setq pfile *standard-output*))
    (if (OR (NULL PFILE) (FILENAME.CHECK PFILE))
	(let ((stream (if (and (symbolp pfile) (boundp pfile) (STREAMP (symbol-value PFILE)))
			  (symbol-value PFILE)
			  (progn (when pfile
				   (unless (stringp pfile) (setq pfile (string-downcase (princ-to-string pfile))))
				   (setq pfile (pathname pfile)))
				 (MKRP-OPENOUT (MKRP-MAKE.PATHNAME T "printoptionshelp" (mkrp-default.text) PFILE)
					       NIL)))))
	  (TERPRI STREAM)
	  (OS=OPT.WRITE.AT.POS (OS.E-GET.EXPLANATION 'OPT.PRETTYPRINT_HEADLINE) STREAM 1 120)
	  (terpri stream)
	  ;(when (opt-get.option pr_latex)
	  ;  (format stream "~%\\def\mkrpoption#1#2
	  (MAPC #'(LAMBDA (AREA.NAME)
		    ;;  ---- PRINT OPTION-AREA AND ITS EXPLANATION ----
		    (format stream "~A    ~A~2%" AREA.NAME (OPT-GET.AREA.EXPLANATION AREA.NAME))
		    (MAPC #'(LAMBDA (OPTION.NAME)
			      ;; PRINT THE OPTION, ITS DEFAULT VALUE AND EXPLANATION.
			      (format stream "~vA~A~%" 40 OPTION.NAME
				      (OS.E-GET.EXPLANATION 'OPT.PRETTYPRINT_DEFAULTVAL (OPT-GET.DEFAULT.VALUE OPTION.NAME)))
			      (OS=OPT.WRITE.AT.POS (OPT-GET.OPTION.TEXT OPTION.NAME) STREAM 42 120)
			      (TERPRI STREAM))
			  (OPT-GET.AREA.OPTIONS AREA.NAME))
		    (format stream "~3%"))
		(OPT-ALL.AREAS))
	  (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT_OUTPUT.END) *standard-output*)
	  (unless (and (symbolp pfile) (boundp pfile) (STREAMP (symbol-value PFILE))) (closefile stream)))
	(OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT_ERROR=ILLEGAL.FILENAME PFILE (READLINE)) *standard-output*))
    (TERPRI T)))

(DEFUN OS=OPT.WRITE.AT.POS (LIST.OF.TEXTSTRINGS FILE FIRST.POS LAST.POS)
  (DECLARE (IGNORE FIRST.POS LAST.POS))
						; INPUT: -LIST OF STRINGS (..  STRING #K ....)
						;        -ATOM, A VALID FILENAME.
						;        -INTEGER, THE FIRST PRINT-COLUMN.
						;        -INTEGER, THE LAST PRINT-COLUMN.
						; VALUE:  UNDEFINED
						; EFFECT: PRINTS ON <FILE> EACH STRING OF <LIST.OF.-
						;         TEXT.STRINGS> IN ONE LINE STARTING WITH
						;         <FIRST.POS>. THE LINES ARE FILLED UP WITH
						;         SPACES UP TO <LAST.POS> AND A NEW LINE IS
						;         STARTED.
  (MAPC
    (FUNCTION (LAMBDA (STRING)  (PRINC STRING FILE)  (TERPRI FILE)))
    LIST.OF.TEXTSTRINGS))

(DEFUn OS=OPT.HELP (UNKNOWN)
						; INPUT: -ATOM, NAME (SPELLING) OF AN OPTIONS-COMMAND.
						; VALUE:  UNDEFINED
						; EFFECT: IF INPUT=LEGAL OPTIONS-COMMAND :
						;             PRINTS AN EXPLANATION OF THE COMMAND ON
						;             TERMINAL,
						;         ELSEIF INPUT=OPTION OR OPTION-AREA NAME:
						;             PRINTS ON TERMINAL AN EXPLANATION OF THE
						;             OPTION OR THE AREA AND OF ITS VALUES
						;             AND//OR EFFECT.
						;         ELSE:
						;             PRINTS ALL ALLOWED OPTIONS-COMMANDS AND
						;             ALL LEGAL OpTION-AREAS ON TERMINAL.
  (CASE UNKNOWN
    ((H HELP) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_HELP) *standard-output*))
    ((P PRINT) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_PRINT) *standard-output*))
    ((PP PPRINT) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_PPRINT) *standard-output*))
    ((R READ) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_READ) *standard-output*))
    ((W WRITE) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_WRITE) *standard-output*))
    ((L LISP) (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_LISP) *standard-output*))
    (OK (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_OK) *standard-output*))
    (V (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_V) *standard-output*))
    (OTHERWISE
      (COND ((MEMBER UNKNOWN (OPT-ALL.OPTIONS)) (OS=WRITE.TEXT (OPT-GET.OPTION.TEXT UNKNOWN) *standard-output*))
	    ((MEMBER UNKNOWN (OPT-ALL.AREAS)) (OS=WRITE.TEXT (OPT-GET.AREA.EXPLANATION UNKNOWN) *standard-output*)
	     (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_AREA.ABBREVIATION) *standard-output*))
	    (T (OS=WRITE.TEXT (OS.E-GET.EXPLANATION 'OPT.HELP_ALL.COMMANDS) *standard-output*)
	       (MAPC
		 (FUNCTION
		   (LAMBDA (AREA) (PRINC (CONCATENATE 'STRING (PRINC-TO-STRING "    ") (PRINC-TO-STRING AREA)) T)))
		 (OPT-ALL.AREAS))
	       (TERPRI T))))))



(DEFUN OS=WRITE.TEXT (TEXT FILE)
						; INPUT:-A LIST OF STRINGS (--STRK--) TO BE PRINTED
						;        ON FILE.
						;       -A FILE-NAME, PRESUMABLY T (=TERMINAL), THE
						;        TEXT IS TO BE PRINTED ON.
						; VALUE: UNDEFINED
						; EFFECT: PRINTS TEXT (ONE SRTING IN ONE LINE]) ON
						;        FILE.
  (FRESH-LINE FILE)
  (MAPC #'(LAMBDA (TXT)
	    (PRINC TXT FILE)
	    (TERPRI file))
	TEXT))

(DEFVAR OS*COMMAND.SPELLINGS
	'((V "V" V)
	  (HC "HC" HC)
	  (HELP "HE[LP]" H HELP)
	  (EXIT "EX[IT]" EX EXIT)
	  (LISP "L[ISP]" L LISP)
	  (LOGOFF "LO[GOFF]" LO LOGOFF)
	  (OPTIONS "O[PTIONS]" O OPTIONS)
	  (SUBSYSTEMS "S[UBSYSTEMS]" S SUBSYSTEMS)
	  (EDIT "E[DIT]" E EDIT)
	  (B "B" B BIG)
	  (N "N" N NORMAL)
	  (EDIT.CONSTRUCT "E[DIT.]C[ONSTRUCT]" EC EDIT.CONSTRUCT EDIT.C ECONSTRUCT E.CONSTRUCT)
	  (EDIT.CONSTRUCT.REFUTE "E[DIT.CONSTRUCT.]R[EFUTE]" ER EDIT.CONSTRUCT.REFUTE EREFUTE E.REFUTE)
	  (EDIT.CONSTRUCT.REFUTE.PROTOCOL "E[DIT.CONSTRUCT.REFUTE.]P[ROTOCOL]" EP EDIT.CONSTRUCT.REFUTE.PROTOCOL
					  EDIT.CONSTRUCT.REFUTE.P EPROTOCOL E.PROTOCOL)
	  (FORMULA "F[ORMULA]" F FORMULA)
	  (FORMULA.CONSTRUCT "F[ORMULA.]C[ONSTRUCT]" FC FORMULA.CONSTRUCT FORMULA.C FCONSTRUCT F.CONSTRUCT)
	  (FORMULA.CONSTRUCT.REFUTE "F[ORMULA.CONSTRUCT.]R[EFUTE]" FR FORMULA.CONSTRUCT.REFUTE FORMULA.CONSTRUCT.R
				    FREFUTE F.REFUTE)
	  (FORMULA.CONSTRUCT.REFUTE.PROTOCOL "F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL]" FP
					     FORMULA.CONSTRUCT.REFUTE.PROTOCOL FORMULA.CONSTRUCT.REFUTE.P FPROTOCOL F.PROTOCOL)
	  (CONSTRUCT "C[ONSTRUCT]" C CONSTRUCT)
	  (CONSTRUCT.REFUTE "C[ONSTRUCT.]R[EFUTE]" CR CONSTRUCT.REFUTE CONSTRUCT.R CREFUTE C.REFUTE)
	  (CONSTRUCT.REFUTE.PROTOCOL "C[ONSTRUCT.REFUTE.]P[ROTOCOL]" CP CONSTRUCT.REFUTE.PROTOCOL CONSTRUCT.REFUTE.P
				     CPROTOCOL C.PROTOCOL)
	  (REFUTE "R[EFUTE]" R REFUTE)
	  (REFUTE.PROTOCOL "R[EFUTE.]P[ROTOCOL]" RP REFUTE.PROTOCOL REFUTE.P RPROTOCOL R.PROTOCOL)
	  (RESUME "RES[UME]" RES RESUME)
	  (PROTOCOL "P[ROTOCOL]" P PROTOCOL)
	  (VIEW.PROTOCOL "V[iew.]P[rotocol]" VP VIEW.PROTOCOL)
	  (HARDCOPY.PROTOCOL "H[ardcopy.]P[rotocol]" HP HARDCOPY.PROTOCOL)
	  (INDUCTION "I[ND[UCTION]]" I IND INDUCTION)
	  (DEFINE.DIRECTORY "D[EFINE.]D[IRECTORY]" DD DEFINE.DIRECTORY)
	  (DEFINE.EXAMPLE.NAME "D[EFINE.]E[XAMPLE.NAME]" DE DEFINE.EXAMPLE.NAME)
	  (select.EXAMPLE "S[ELECT.]E[XAMPLE]" sE SELECT.EXAMPLE)))


; (DEFVAR OS*LAST.COL 120) Not used

(DEFVAR OS*RESET.FLG NIL)

(SETQ OS*RESET.FLG NIL)

(DEFVAR OS*ACTUAL.PROBLEM.FILE NIL)

(DEFVAR OS*ACTUAL.CODE.FILE NIL)

(DEFVAR OS*ACTUAL.GRAPH.FILE NIL)

(DEFVAR OS*ACTUAL.LIST.FILE NIL)