;;; -*- mode: lisp; syntax: common-lisp; package: mkrp -*-

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


(DEFUN PR-CONSTRUCT.START (FILE SYSTEM.VERSION &optional COMMENT)
					; Edited:  08-APR-1992 19:11
					; Authors: MKRP CL TKS
					; input : code file, version of system, a string comment
					; effect: sets linelength of code file to 120 and
					;         prints <CONSTRUCTION system.version date (C comment)>
					;         top-level elements of comment will be prin-
					;         in separate lines.
					; value : undefined
  
  (case (opt-get.option pr_protocol)
	(standard (PR=CONSTRUCT.START file SYSTEM.VERSION COMMENT))
	(post (PO-CONSTRUCT.START FILE comment))))


(DEFUN PR-INFIX.FORM (AXIOMS.INFIX THEOREMS.INFIX)
						      ; Edited:  08-APR-1992 19:22
						      ; Authors: MKRP CL
						      ; input : two lists
						      ; effect: prints <AXIOMS.INFIX   (...) ... (...)>
						      ;                <THEOREMS.INFIX (...) ... (...)>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=INFIX.FORM AXIOMS.INFIX THEOREMS.INFIX))))
					
(DEFUN PR-PREFIX.FORM (AXIOMS.PREFIX THEOREMS.PREFIX)
						      ; Edited:  08-APR-1992 19:22
						      ; Authors: MKRP CL
						      ; input : two lists
						      ; effect: prints <'axioms.prefix   (...) ... (...)>
						      ;                <'theorems.prefix (...) ... (...)>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=PREFIX.FORM AXIOMS.PREFIX THEOREMS.PREFIX))
    (post (po-PREFIX.FORM AXIOMS.PREFIX THEOREMS.PREFIX))))

					
(DEFUN PR-OPTIONS NIL	
						      ; Edited:  08-APR-1992 19:23			
						      ; Authors: MKRP CL
						      ; input : none
						      ; effect: prints <OPTIONS ... all options and their
						      ;                      values as dotted pairs ... >
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=OPTIONS.TOP))))

(DEFUN PR-AXIOMS.START NIL			
					; Edited:  08-APR-1992 19:26
					; Authors: MKRP CL
					; input : none
					; effect: prints <LINK.COLOURS R ... PER>
					;                <AXIOMS (START.TIME time in sec/977)
  (case (opt-get.option pr_protocol)
	(standard (PR=AXIOMS.START))
	(post (PO-AXIOMS.START))))
					
(DEFUN PR-AXIOMS.END (RESULT)	
					; Edited:  08-APR-1992 19:27
					; Authors: MKRP CL
					; input : a list (SUCCESS AXIOMS.UNSATISFIABLE)
					;             or (SUCCESS   ..empty clause..)  or nil
					; effect: prints (END.TIME  time in sec/997 )
					;                (FINAL     actual clauses  )
					;                (RESULT    result reason   )>
					; value : undefined
  (case (opt-get.option pr_protocol)
	(standard (PR=AXIOMS.end result))))

(DEFUN PR-THEOREMS.START (SPLITPART.IDENTIFIER &optional SPLITFLAG)
  (declare (ignore splitflag))		
						      ; Edited:  08-APR-1992 19:39
						      ; Authors: MKRP CL
						      ; input : a list of integers and a boolean value
						      ; effect: prints <THEOREMS (SPLITPART.IDENTIFIER ..)
						      ;                          (START.TIME    ..time.. )
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=THEOREMS.START SPLITPART.IDENTIFIER))))
					
(DEFUN PR-THEOREMS.END (RESULT)	
						      ; Edited:  08-APR-1992 19:30
						      ; Authors: MKRP CL
						      ; input : one of the following lists:
						      ;          (SUCCESS THEOREMS.VALID)
						      ;          (SUCCESS ..empty.clause..)
						      ;          (FAILURE GRAPH.COLLAPSED)
						      ;          (FAILURE GRAPH.SATISFIABLE ..model..)
						      ;          (SPLIT   ..file.. ..splitpart.indicators..)
						      ; effect: prints (END.TIME time in sec/997)
						      ;                (FINAL    actual clauses )
						      ;                (RESULT   result reason  )>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=THEOREMS.END result))    
    (post (po-theoremS.end result))))

(DEFun PR-CONSTRUCT.END ()
  (case (opt-get.option pr_protocol)
    (standard (PR=CONSTRUCT.END))))


(DEFUN PR-SPLITPARTS.START (FILE SYSTEM.VERSION &optional COMMENT)
						      ; Edited:  08-APR-1992 19:31
						      ; Authors: MKRP CL
						      ; input : code file and version of system
						      ; effect: sets linelength of code file to 120 and
						      ;         prints <SPLITPARTS system.version date>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=SPLITPARTS.START FILE SYSTEM.VERSION COMMENT))))

					
(DEFUN PR-REFUTATION.START (SPLITPART.INDICATOR RESUMPTION.FLAG STEP.NUMBER)
  (declare (ignore splitpart.indicator resumption.flag step.number))
						      ; Edited:  08-APR-1992 19:32
						      ; Authors: MKRP CL
						      ; input : a list of lists of integers, a flag indica-
						      ;         ting if splitpart is continued, and an in-
						      ;         teger (or nil, if new splitpart begins)
						      ; effect: prints    <'refutation
						      ;                       ('start.time ..in msec..)
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=REFUTATION.START))
    (post (po-REFUTATION.START))))

					
(DEFUN PR-PARTIAL.GRAPH (PARTIAL.CLAUSE.LIST)	
						      ; Edited:  08-APR-1992 19:34; edited: 22-apr-83 08:10:18  by cl
						      ; Authors: MKRP CL
						      ; input : list of clauses
						      ; effect: prints  <PARTIAL ... list of clauses as
						      ;                          described in pr=clauses ...>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=PARTIAL.GRAPH PARTIAL.CLAUSE.LIST))
    (post (po-PARTIAL.GRAPH PARTIAL.CLAUSE.LIST))))


(DEFUN PR-INITIAL.GRAPH ()	
						      ; Edited:  08-APR-1992 19:35
						      ; Authors: MKRP CL		
						      ; input : none
						      ; effect: prints  <INITIAL ... list of clauses as
						      ;                          described in pr=clauses ...>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (PR=INITIAL.GRAPH))))


(DEFUN PR-OPERATION (OPERATION.TYPE &REST ARGUMENTS)
						      ; edited:  3-jul-84 10:45:06  by cl
						      ; input : an atom, and arguments according to the type of operation
						      ; effect: prints <OPERATION <CLAUSE  ..see PR=CLAUSE..>
						      ;                           <op.type ..see PR=op.type..>
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (pr=operation OPERATION.TYPE ARGUMENTS))
    (post (po-operation operation.type arguments))))

(DEFUN PR-STATISTICS (TIME)
  (declare (ignore time))			; edited:  3-jul-84 17:19:28  by cl
						; input : an integer (time in sec/997)
						; effect: prints the following (example) :
						;   (STATISTICS (CLAUSES (ALL . 44) (INSERTED . 2) (REMOVED . 1) (CHANGED . 0))
						;               (LINKS (ALL . (223 12 ... 0 2))   (INSERTED . (12 0 ... 0 1))
						;               (REMOVED . (20 2 ... 1 1)) (CHANGED . (1 0 ... 0 0))))
						;         numbers separated for different link colours.
						; value : undefined
  nil)


(DEFUN PR-REFUTATION.END (SPLITPART.IDENTIFIER RESULT)
						      ; Edited:  08-APR-1992 19:36
						      ; Authors: MKRP CL
						      ; input : a list of lists of integers and a list:
						      ;            (SUCCESS ..empty.clause..)
						      ;         OR (FAILURE GRAPH.SATISFIABLE ..model..),
						      ;            (FAILURE LINKS.INOPERABLE),
						      ;            (FAILURE ABORTED.MAXSTEPS),
						      ;            (FAILURE ABORTED.MANUALLY),
						      ;            (SPLIT ..file.. ..splitpart.indicators..)
						      ; effect: prints  (END.TIME ..in sec/977..)
						      ;                 (SYMBOLS  ....)
						      ;                 (SPLITPART.IDENTIFIER  ....)
						      ;                 (RESULT result reason)   >
						      ; value : undefined
  (case (opt-get.option pr_protocol)
    (standard (pr=REFUTATION.END SPLITPART.IDENTIFIER RESULT))
    (post (po-REFUTATION.END))))


(DEFUN PR-SPLITPARTS.END () 
  (case (opt-get.option pr_protocol)
    (standard (pr=SPLITPARTS.END))))

