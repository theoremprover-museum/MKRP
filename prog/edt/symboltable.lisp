;; -*- Package: MKRP; Syntax: Common-Lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

(DEFUN ST-RESET NIL
						;EDITED AT 7-JUL-82 |16:54|
						;INPUT:  NIL.
						;EFFECT: ERASES THE STACKS AND RESETS MEMORY
						;VALUE:  UNDEFINED.
  (SETQ ST*STACK1 (LIST NIL)) (SETQ ST*STACK2 (LIST NIL)) (SETQ ST*LOAD.FLAG T) (SETQ ST*SYMBOL.ADDRESSES (LIST NIL)) (DT-RESET))

(DEFUN ST-FIX NIL
						;EDITED AT 7-JUL-82 |16:57|
						;INPUT:  NIL.
						;EFFECT: CLEARS STACK1 AND STACK2
						;VALUE:  UNDEFINED.
  (SETQ ST*STACK2 (LIST NIL)) (SETQ ST*STACK1 (LIST NIL)))

(DEFUN ST-POP.SYMBOLTABLE (STACK.INDICATION)
						; EDITED AT 9-JUL-82 |11:42|
						; INPUT:  ONE OF THE ATOMS STACK1, STACK2.
						; EFFECT: THE SPECIFIED STACK IS DECREMENTED.
						;        THIS MEANS,THAT THE S-EXPRESSIONS IN THE
						;        TOP OF THE STACK ARE EVALUATED.
						; CAUTION:   THE STRUCTURES OF STACK1 AND STACK2
						;           ARE DIFFERENT.
						; VALUE : T IF THE STACK IS NOT EMPTY NIL ELSE.
  (COND ((NULL STACK.INDICATION) (SETQ STACK.INDICATION 'STACK1)))
  (COND ((ST=STACK.EMPTY STACK.INDICATION) NIL)
	(T (CASE STACK.INDICATION
	     (STACK1 (ST=RELEASE.SYMBOLTABLE (CAR ST*STACK1)) (SETQ ST*STACK1 (CDR ST*STACK1)))
	     (STACK2 (ST-PUSH.SYMBOLTABLE 'STACK1) (EVAL (CAR ST*STACK2)) (SETQ ST*STACK2 (CDR ST*STACK2)))
	     (OTHERWISE (ERROR "ST=STACK.POP : ILLEGAL STACKINDICATION =~A" STACK.INDICATION)))
	   T)))

(DEFUN ST-PUSH.SYMBOLTABLE (STACK.INDICATION)
						; EDITED AT 8-JUL-82 |13:18|
						; INPUT:  ONE OF THE ATOMS STACK1 OR STACK2.
						; EFFECT: (CAR STACK1)   IS PUSHED INTO THE STACK SPECIFIED.
						; VALUE:  UNDEFINED.
  (COND ((NULL STACK.INDICATION) (SETQ STACK.INDICATION 'STACK1)))
  (CASE STACK.INDICATION
    (STACK1 (SETQ ST*STACK1 (CONS NIL ST*STACK1)))
    (STACK2
      (push (CONS 'PROGN
		  (MAPCAR #'(LAMBDA (ENTRY)
			      (let ((SYMBOLNAME (CAR ENTRY)))
				(LIST 'ST-ENTER.SYMBOLENTRY `',SYMBOLNAME `',(ST=GET.SYMBOLKIND SYMBOLNAME) NIL
				      `',(ST=GET.SYMBOLATTRIBUTE SYMBOLNAME) `',(ST=GET.SYMBOLDOMAIN SYMBOLNAME)
				      `',(ST=GET.SYMBOLRANGE SYMBOLNAME) `',(ST=GET.SYMBOLDATA SYMBOLNAME))))
			  (CAR ST*STACK1)))
	    ST*STACK2)
      (ST=RELEASE.SYMBOLTABLE (CAR ST*STACK1))
      (RPLACA ST*STACK1 NIL))
    (OTHERWISE (ERROR "ST=STACK.PUSH : ILLEGAL STACKINDICATION =~A" STACK.INDICATION)))
  NIL)

(DEFUN ST-CLEAR.STACK (STACK.INDICATION)
						;EDITED AT 8-JUL-82 |13:21|
						;INPUT:  ONE OF THE ATOMS STACK1 OR STACK2.
						;EFFECT: CLEARS THE STACK INDICATED.
						;VALUE:  UNDEFINED.
  (COND ((NULL STACK.INDICATION) (SETQ STACK.INDICATION 'STACK1)))
  (COND ((EQL STACK.INDICATION 'STACK2) (SETQ ST*STACK2 (LIST 'NIL))) (T (ST=CLEAR.STACK STACK.INDICATION))))

(DEFUN ST-STACK.EMPTY (STACK.INDICATION)
						;EDITED AT 8-JUL-82 |13:23|
						;INPUT:  ONE OF THE ATOMS STACK1, STACK2.
						;EFFECT: RETURNS VALUE.
						;VALUE:  T IF THE SYMBOLTABLE IS EMPTY, NIL ELSE.
  (COND ((NULL STACK.INDICATION) (SETQ STACK.INDICATION 'STACK1))) (ST=STACK.EMPTY STACK.INDICATION))

(DEFUN ST-STACK.LENGTH (&optional STACK.INDICATION)
						;EDITED AT 8-JUL-82 |13:25|
						;INPUT:  ONE OF THE ATOMS STACK1, STACK2.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE LENGTH OF THE STACK.
  (COND ((NULL STACK.INDICATION) (SETQ STACK.INDICATION 'STACK1)))
  (1-
    (CASE STACK.INDICATION (STACK1 (LIST-LENGTH ST*STACK1)) (STACK2 (LIST-LENGTH ST*STACK2))
	  (OTHERWISE (ERROR "ST=STACK.LENGTH : ILLEGAL STACKINDICATION =~A" STACK.INDICATION)))))

(DEFUN ST-ENTER.SYMBOLENTRY
       (SYMBOLNAME &optional SYMBOLKIND SYMBOLARITY SYMBOLATTRIBUTE SYMBOLDOMAIN SYMBOLRANGE SYMBOLDATA SYMBOLADDRESS)
						;EDITED AT 5-JUL-82 |14:42|
						;INPUT:   A SYMBOLNAME AND ITS FULL CLASSIFICATION.
						;EFFECT:  IF SYMBOLNAME IS NEW, A NEW ENTRY IN THE
						;         DATASTRUCTURES IS CREATED.IF SYMBOLNAME IS
						;         NOT IN (CAR STACK1), THEN THE REQUIRED
						;         S-EXPRESSION TO REMOVE THIS ENTRY IS
						;         PLACED IN (CAR STACK1).
						;VALUE :  UNDEFINED.
  (PROG (ADDRESS)
	(COND
	  ((NOT (ASSOC SYMBOLNAME ST*SYMBOL.ADDRESSES))
	   (CASE SYMBOLKIND
	     ((VARIABLE SYS-VAR) (SETQ ADDRESS (DT-VARIABLE.CREATE SYMBOLRANGE)) (DT-PUTPROP ADDRESS 'DT*ST-KIND NIL))
	     ((CONSTANT SYS-CONST)
	      (SETQ ADDRESS (DT-CONSTANT.CREATE SYMBOLNAME SYMBOLRANGE))
	      (DT-PUTPROP ADDRESS 'DT*ST-KIND NIL))
	     ((FUNCTION SYS-FUNCT)
	      (SETQ ADDRESS (DT-FUNCTION.CREATE SYMBOLNAME SYMBOLRANGE SYMBOLDOMAIN))
	      (DT-FUNCTION.PUT.ATTRIBUTES ADDRESS SYMBOLATTRIBUTE)
	      (DT-PUTPROP ADDRESS 'DT*ST-KIND NIL))
	     (PREDICATE
	       (PROG1 (SETQ ADDRESS (DT-PREDICATE.CREATE SYMBOLNAME SYMBOLDOMAIN))
		      (DT-PREDICATE.ADD.ATTRIBUTES ADDRESS SYMBOLATTRIBUTE)
		      (DT-PUTPROP ADDRESS 'DT*ST-KIND NIL)))
	     (SORT
	       (PROG1 (SETQ ADDRESS T)
		      (DT-SORT.CREATE SYMBOLNAME NIL T)
		      (MAPC #'(LAMBDA (RANGESORT)
				(COND ((NOT (ST-IS.IN.SYMBOLTABLE RANGESORT))
				       (ST-ENTER.SYMBOLENTRY RANGESORT 'SORT))))
			    SYMBOLRANGE)
		      (DT-SORT.ST.PUT.DIRECT.SUPERSORTS SYMBOLNAME SYMBOLRANGE)))
	     (OTHERWISE (ERROR "WRONG SYMBOLKIND~A" NIL)))
	   (COND ((AND (NUMBERP ADDRESS) SYMBOLDATA) (DT-PUTPROP ADDRESS 'ST*DATA SYMBOLDATA))
		 ((AND ADDRESS SYMBOLDATA) (SETF (get 'ST*DATA SYMBOLNAME) SYMBOLDATA)))
	   (COND (ADDRESS (PUTASSOC SYMBOLNAME ADDRESS ST*SYMBOL.ADDRESSES)))
	   (ST=PUT.NEW.ON.STACK SYMBOLNAME ADDRESS))
	  (T
	   (RETURN
	     (ST-CHANGE.SYMBOLENTRY SYMBOLNAME SYMBOLKIND SYMBOLARITY SYMBOLATTRIBUTE SYMBOLDOMAIN SYMBOLRANGE SYMBOLDATA
				    SYMBOLADDRESS))))
	(RETURN ADDRESS)))

(DEFUN ST-CHANGE.SYMBOLENTRY (SYMBOLNAME SYMBOLKIND SYMBOLARITY SYMBOLATTRIBUTE SYMBOLDOMAIN
			      SYMBOLRANGE SYMBOLDATA SYMBOLADDRESS)
  (declare (special symbolarity symboladdress symbolkind))
						;EDITED AT 5-JUL-82 |14:42|
						;INPUT:   A SYMBOLNAME AND ITS FULL CLASSIFICATION.
						;EFFECT:  IF SYMBOLNAME IS NOT IN (CAR STACK1)
						;                              THEN THE REQUIRED
						;         S-EXPRESSION TO REMOVE THIS ENTRY IS
						;         PLACED IN (CAR STACK1).
						;VALUE :  UNDEFINED.
  (PROG (ADDRESS)
	(COND
	  ((SETQ ADDRESS (Cdr (aSSOC SYMBOLNAME ST*SYMBOL.ADDRESSES)))
	   (COND ((NOT (ASSOC SYMBOLNAME (CAR ST*STACK1))) (ST=PUT.CHANGE.ON.STACK SYMBOLNAME ADDRESS)))
	   (CASE (ST=GET.SYMBOLKIND SYMBOLNAME)
	     ((VARIABLE SYS-VAR) (DT-VARIABLE.PUTSORT ADDRESS SYMBOLRANGE))
	     ((CONSTANT SYS-CONST) (DT-CONSTANT.PUTSORT ADDRESS SYMBOLRANGE))
	     ((FUNCTION SYS-FUNCT)
	      (PROG1 (DT-FUNCTION.CHANGE ADDRESS SYMBOLRANGE SYMBOLDOMAIN NIL)
		     (DT-FUNCTION.PUT.ATTRIBUTES ADDRESS SYMBOLATTRIBUTE)))
	     (PREDICATE
	       (PROG1 (DT-PREDICATE.PUTSORT ADDRESS SYMBOLDOMAIN) (DT-PREDICATE.PUT.ATTRIBUTES ADDRESS SYMBOLATTRIBUTE)))
	     (SORT (PROG1 (MAPC #'(LAMBDA (RANGESORT)
				    (unless (ST-IS.IN.SYMBOLTABLE RANGESORT) (ST-ENTER.SYMBOLENTRY RANGESORT 'SORT)))
				SYMBOLRANGE)
			  (DT-SORT.ST.PUT.DIRECT.SUPERSORTS SYMBOLNAME SYMBOLRANGE)))
	     (OTHERWISE NIL))
	   (COND ((AND (NUMBERP ADDRESS) SYMBOLDATA) (DT-PUTPROP ADDRESS 'ST*DATA SYMBOLDATA))
		 ((AND ADDRESS SYMBOLDATA) (SETF (get 'ST*DATA SYMBOLNAME) SYMBOLDATA)))))
	(RETURN ADDRESS)))

(DEFUN ST-ENTER.SYMBOL.CLASSIFICATION (SYMBOLNAME CLASSIFICATION ENTRY)
						;EDITED AT 6-JUL-82 |10:38|
						;INPUT:   SYMBOLNAME: AN ATOM.
						;         CLASSIFICATION: ONE OF THE ATOMS
						;                               ARITY, DOMAIN, RANGE,
						;                         SUBSORTS, SUPERSORTS,ATTRI-
						;                         BUTE, DATA, ADDRESS.
						;         ENTRY: THE NEW ENTRY FOR CLASSIFICATION.
						;EFFECT:  IF CLASSIFICATION IS DIFFERENT FROM DATA
						;         AND SYMBOLNAME OCCURS IN THE SYMBOLTABLE
						;         ENTRY IS THE NEW CONTENT OF THE COLUMN
						;         GIVEN BY CLASSIFICATION. IF SYMBOLNAME DOES
						;         NOT OCCUR IN THE SYMBOLTABLE THE TABLE
						;         REMAINS UNCHANGED. IN THE CASE OF
						;         CLASSIFICATION = DATA THE FUNCTION WORKS
						;         LIKE ST-PUT.SYMBOL.DATA.
						;VALUE:   NIL, IF SYMBOLNAME IS NOT IN THE TABLE AND
						;         CLASSIFICATION IS UNEQUAL TO 'DATA'. ELSE
						;         NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)) KIND)
	(COND  ((NOT (EQUAL ENTRY (ST-GET.SYMBOL.CLASSIFICATION SYMBOLNAME CLASSIFICATION)))
		(SETQ KIND (ST=GET.SYMBOLKIND SYMBOLNAME))
		(ST=PUT.CHANGE.ON.STACK SYMBOLNAME ADDRESS)
		(CASE CLASSIFICATION
		  (ARITY NIL)
		  (ATTRIBUTE
		    (CASE KIND ((FUNCTION SYS-FUNCT) (DT-FUNCTION.PUT.ATTRIBUTES ADDRESS ENTRY))
			  (PREDICATE (DT-PREDICATE.PUT.ATTRIBUTES ADDRESS ENTRY)) (OTHERWISE NIL)))
		  ((DOMAIN SUBSORTS)
		   (CASE KIND ((FUNCTION SYS-FUNCT) (DT-FUNCTION.CHANGE.ONE.ENTRY ADDRESS 'MAX.DOMAINSORTS ENTRY))
			 (PREDICATE (DT-PREDICATE.PUTSORT ADDRESS ENTRY)) (OTHERWISE NIL)))
		  ((RANGE SUPERSORTS)
		   (CASE KIND ((VARIABLE SYS-VAR) (DT-VARIABLE.PUTSORT ADDRESS ENTRY))
			 ((CONSTANT SYS-CONST) (DT-CONSTANT.PUTSORT ADDRESS ENTRY))
			 ((FUNCTION SYS-FUNCT) (DT-FUNCTION.CHANGE.ONE.ENTRY ADDRESS 'MAX.RANGESORT ENTRY))
			 (SORT
			   (PROG1
			     (MAPC
			       (FUNCTION
				 (LAMBDA (RANGESORT)
				   (COND ((NOT (ST-IS.IN.SYMBOLTABLE RANGESORT)) (ST-ENTER.SYMBOLENTRY RANGESORT 'SORT)))))
			       ENTRY)
			     (DT-SORT.ST.PUT.DIRECT.SUPERSORTS SYMBOLNAME ENTRY)))
			 (OTHERWISE NIL)))
		  (ADDRESS (ERROR "ENTER.CL~A" NIL))
		  (DATA
		    (CASE KIND (SORT (SETF (get'ST*DATA SYMBOLNAME) ENTRY))
			  ((VARIABLE SYS-VAR FUNCTION SYS-FUNCT PREDICATE CONSTANT SYS-CONST)
			   (DT-PUTPROP ADDRESS 'ST*DATA ENTRY)) (OTHERWISE NIL)))
		  (OTHERWISE (ERROR "ST-ENTER.SYMBOL.CLASSIFICATION, ILLEGAL ARGUMENT: CLASSIFICATION =~A" CLASSIFICATION)))))
	(RETURN ADDRESS)))

#| Not used
(DEFUN ST-PUT.SYMBOL.DATA (SYMBOLNAME SYMBOLDATA)
						;EDITED AT 5-JUL-82 |14:16|
						;INPUT:  SYMBOLNAME : A LITERAL ATOM.
						;        SYMBOLDATA : AN S-EXPRESSION.
						;EFFECT: IF SYMBOLNAME HAS AN ENTRY IN THE
						;        SYMBOLTABLE THEN THE DATA COLUMN IS CHANGED
						;        TO SYMBOLDATA, ELSE A NEW ENTRY IS CREATED
						;        WITH DATA COLUMN SET TO SYMBOLDATA.
						;VALUE : T, IF SYMBOLNAME HAS ALREADY AN ENTRY IN THE
						;        SYMBOLTABLE, ELSE NIL.
  (ST-ENTER.SYMBOLENTRY SYMBOLNAME (ST=GET.SYMBOLKIND SYMBOLNAME) (ST=GET.SYMBOLARITY SYMBOLNAME)
			(ST=GET.SYMBOLATTRIBUTE SYMBOLNAME) (ST=GET.SYMBOLDOMAIN SYMBOLNAME)
			(ST=GET.SYMBOLRANGE SYMBOLNAME) SYMBOLDATA))|#

(DEFUN ST-GET.SYMBOL.CLASSIFICATION (SYMBOLNAME CLASSIFICATION)
						;EDITED AT 5-JUL-82 |13:32|
						;INPUT:  SYMBOLNAME: AN ATOM
						;        CLASSIFICATION: ONE OF THE ATOMS
						;                        KIND, ARITY, DOMAIN, RANGE,
						;                        SUBSORTS, SUPERSORTS, ATTRI-
						;                        BUTE, DATA
						;EFFECT: RETURNS VALUE.
						;VALUE:  NIL IF SYMBOLNAME IS NOT IN THE TABLE OR HAS
						;        NO ENTRY FOR CLASSIFICATION, ELSE THE ENTRY
						;        SPECIFIED BY CLASSIFICATION.
  (CASE CLASSIFICATION
    (KIND (ST=GET.SYMBOLKIND SYMBOLNAME))
    (ARITY (ST=GET.SYMBOLARITY SYMBOLNAME))
    (ATTRIBUTE (ST=GET.SYMBOLATTRIBUTE SYMBOLNAME))
    ((DOMAIN SUBSORTS) (ST=GET.SYMBOLDOMAIN SYMBOLNAME))
    ((RANGE SUPERSORTS) (ST=GET.SYMBOLRANGE SYMBOLNAME))
    (ADDRESS (CASSOC SYMBOLNAME ST*SYMBOL.ADDRESSES))
    (DATA (ST=GET.SYMBOLDATA SYMBOLNAME))
    (OTHERWISE (ERROR "ST-GET.SYMBOL.CLASSIFICATION, ILLEGAL ARGUMENT: CLASSIFICATION =~A" CLASSIFICATION))))

(DEFUN ST-GET.TRANSITIVE.CLOSURE (SORT)
						;INPUT:   A SORT
						;VALUE: THE TRANSITIVE CLOSURE OF THIS SORT.
						;       I.E. ALL SORTS, WHICH ARE <= SORT
  (DT-SORT.TRANSITIVE.CLOSURE SORT))

(DEFUN ST-GET.INVERSE.TRANSITIVE.CLOSURE (SORT)
						;INPUT:   A SORT
						;VALUE: THE TRANSITIVE CLOSURE OF THIS SORT FOR >=
						;       I.E. ALL SORTS, WHICH ARE >= SORT
  (DT-SORT.INVERSE.TRANSITIVE.CLOSURE SORT))

(DEFUN ST-ALL.SYMBOLNAMES NIL
						;EDITED AT 8-JUL-82 |12:12|
						;INPUT:   NIL.
						;EFFECT:  RETURNS VALUE.
						;VALUE :  A LIST OF ALL SYMBOLNAMES OCCURING IN THE
						;         SYMBOLTABLE.
  (MAPCAR (FUNCTION (LAMBDA (ENTRY) (CAR ENTRY))) (CDR ST*SYMBOL.ADDRESSES)))

(DEFUN ST-IS.IN.SYMBOLTABLE (SYMBOLNAME)
						;EDITED AT 8-JUL-82 |12:14|
						;INPUT:   A SYMBOLNAME.
						;EFFECT:  RETURNS VALUE.
						;VALUE :  T IF SYMBOLNAME OCCURES IN THE TABLE, NIL
						;         ELSE.
  (ASSOC SYMBOLNAME ST*SYMBOL.ADDRESSES))

(DEFUN ST-REMOVE.SYMBOL (SYMBOLNAME)
						;EDITED AT 8-JUL-82 |12:41|
						;INPUT:  SYMBOLNAME : A LITERAL ATOM.
						;EFFECT: REMOVES THE SYMBOL SYMBOLNAME AND IT'S ENTRY
						;        FROM THE SYMBOLTABLE.
						;VALUE : UNDEFINED.
  (PROG (SYMBOLTABLE ADDRESS)
	(MAPL
	  (FUNCTION
	    (LAMBDA (STACKTAIL) (SETQ SYMBOLTABLE (CAR STACKTAIL))
		    (MAPC
		      (FUNCTION
			(LAMBDA (TABLETAIL)
			  (COND ((EQL (CAR TABLETAIL) SYMBOLNAME) (RPLACA STACKTAIL (DELETE TABLETAIL SYMBOLTABLE))))))
		      SYMBOLTABLE)))
	  ST*STACK1)
	(SETQ ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME))
	(CASE (DT-TYPE ADDRESS) (VARIABLE (DT-VARIABLE.DELETE ADDRESS)) (PREDICATE (DT-PREDICATE.DELETE ADDRESS))
	      (FUNCTION (DT-FUNCTION.DELETE ADDRESS)) (CONSTANT (DT-CONSTANT.DELETE ADDRESS)) (OTHERWISE NIL))
	(COND ((AND (NOT (DT-TYPE ADDRESS)) ADDRESS) (DT-SORT.ST.REMOVE SYMBOLNAME))) (REMASSOC SYMBOLNAME ST*SYMBOL.ADDRESSES)))

(DEFUN ST-REPLACE.SYMBOL (SYMBOL.X SYMBOL.Y)
						;EDITED AT 6-JUL-82 |11:07|
						;INPUT:  SYMBOL.X, SYMBOL.Y : TWO ATOMS
						;EFFECT: REPLACES EACH OCCURRENCE OF SYMBOL.X BY
						;        SYMBOL.Y IN THE SYMBOLTABLE.
						;VALUE : T IF AN REPLACEMENT IS PERFORMED.
  (PROG ((ADDRESS (CASSOC SYMBOL.X ST*SYMBOL.ADDRESSES)))
	(COND
	  (ADDRESS
	   (CASE (DT-TYPE ADDRESS)
	     (VARIABLE NIL)
	     (CONSTANT (DT-CONSTANT.PUTPNAME ADDRESS SYMBOL.Y))	
	     (FUNCTION (DT-FUNCTION.PUTPNAME ADDRESS SYMBOL.Y))
	     (PREDICATE (DT-PREDICATE.PUTPNAME ADDRESS SYMBOL.Y))
	     (OTHERWISE (ERROR "WRONG TYPE OF ADDRESS~A" NIL)))
	   (MAPC
	     (FUNCTION
	       (LAMBDA (TABLE)
		 (MAPL
		   (FUNCTION
		     (LAMBDA (ENTRY)
		       (COND ((EQL (ST=GET.SYMBOLKIND (CAR ENTRY)) 'SORT) (ERROR "RENAMING OF SORTS IS NOT ALLOWED~A" NIL)))
		       (COND ((EQL (CAR ENTRY) SYMBOL.X) (RPLACA ENTRY SYMBOL.Y)))))
		   TABLE)))
	     ST*STACK1)
	   (NSUBST SYMBOL.Y SYMBOL.X ST*SYMBOL.ADDRESSES)))))

(DEFUN ST-SAVE (FILE STACKS)
						;EDITED: 13.7.83               NE
						;INPUT:  FILE: NIL OR NAME OF A FILE OPEN FOR OUTPUT.
						;        STACKS: ANY SUBSET OF (STACK1 STACK2 STACK3)
						;                NIL IS TREATED LIKE (STACK1)
						;        RESETFLAG: BOOLEAN.
						;EFFECT: COMPUTES AN S-EXPRESSION WHICH ON EVALUATION
						;        RESTORES THE SAVE TIME CONTENTS OF THE GIVEN
						;        STACKS. IF RESETFLAG = T, THE STACKS' EVAL
						;        TIME CONTENTS WILL BE DELETED, OTHERWISE THE
						;        SAVE TIME CONTENTS ARE JUST ADDED.
						;        THIS S-EXPRESSION IS WRITTEN ON FILE IF FILE
						;        IS NOT NIL, OTHERWISE RETURNED AS VALUE.
						;VALUE:  IF FILE = NIL THEN THE COMPUTED S-EXPRESSION
						;                      ELSE NIL.
  (unless STACKS (SETQ STACKS '(STACK1)))
  (unless (EQUAL STACKS '(STACK1)) (cERROR "Continue"
					   "NOT SUPPORTED STACK-INIDCATION"))
  (let (EXPRESSION)
    (SETQ EXPRESSION
	  `(PROGN (setq ST*STACK1 ',ST*STACK1) (setq ST*SYMBOL.ADDRESSES ',ST*SYMBOL.ADDRESSES)))
    (COND ((NULL FILE) (LIST 'COND (CONS 'ST*LOAD.FLAG (NCONC1 (DT-SAVE NIL) EXPRESSION))))
	  (T (PRINC "(COND (ST*LOAD.FLAG " FILE) (DT-SAVE FILE) (PRINC EXPRESSION FILE) (PRINC "))" FILE) NIL))))

(DEFUN ST-LOAD (LIST.OF.STACK.ENTRYS)
						;EDITED: 13-MAY-83 10:28:45
  
						;INPUT:  LIST.OF.STACK.ENTRYS IS A LIST OF
						;               (STACK-INDICATION STACK-ENTRYS)
						;        STACK-ENTRYS IS A LIST OF LISTS OF ARGUMENTS
						;        OF THE FUNCTION ST-ENTER.SYMBOLENTRY.
						;EFFECT: IF THE COMMON VARIABLE ST*LOAD.FLAG IS T,
						;        LIST.OF.STACK.ENTRY IS EVALUATED TO THE NEW
						;        STACKS OF SYMBOLTABLES.
						;VALUE : T IF THE STACKS ARE UPDATED.
  (COND
    (ST*LOAD.FLAG
     (MAPC
       (FUNCTION
	 (LAMBDA (STACK)
	   (PROG ((STACK.INDICATION (CAR STACK)) (STACK.ENTRY (SECOND STACK))) (C (ST-CLEAR.STACK STACK.INDICATION) *)
		 (SMAPC
		   (FUNCTION
		     (LAMBDA (SYMBOLTABLE)
		       (MAPC (FUNCTION (LAMBDA (ENTRY) (APPLY (FUNCTION ST-ENTER.SYMBOLENTRY) ENTRY))) SYMBOLTABLE)))
		   (FUNCTION
		     (LAMBDA (SYMBOLTABLELIST)
		       (COND ((CDR SYMBOLTABLELIST) (ST-PUSH.SYMBOLTABLE STACK.INDICATION) (CDR SYMBOLTABLELIST)))))
		   STACK.ENTRY))))
       LIST.OF.STACK.ENTRYS)
     T)))

(DEFUN ST-SET.LOAD.FLAG (FLAG)
						;EDITED: 13-JUL-83 12:52:36        NE
  
						;INPUT:  T OR NIL
						;EFFECT: COMMON VARIABLE ST*LOAD.FLAG IS CHANGED.
						;        IF FLAG = NIL, SUBSEQUENT CALLS OF ST-LOAD
						;        WILL HAVE NO EFFECT UNTIL THE FLAG IS SET
						;        TO T.
						;VALUE:  UNDEFINED.
  (SETQ ST*LOAD.FLAG FLAG))

(DEFUN ST-LOAD.FLAG NIL
						;EDITED: 13-JUL-83 12:52:36        NE
						;INPUT:  NO ARGUMENTS
						;        USES COMMON VARIABLE ST*LOAD.FLAG READ-ONLY.
						;VALUE:  T IF ST-LOAD WOULD CURRENTLY DO ANYTHING,
						;        ELSE NIL.
  ST*LOAD.FLAG)

(DEFUN ST-READ (SEXPRESSION)
						;EDITED AT 9-JUL-82 |11:43|
						;INPUT:  A FILENAME OR A SEXPRESSION DENOTING ONE OR
						;        MORE SYMBOLTABLESTACKS.
						;EFFECT: THE OLD STACKS ARE CLEARED AND OVERWRITEN.
						;VALUE:  UNDEFINED
  (COND ((ATOM SEXPRESSION) (LOAD SEXPRESSION)) (T (EVAL SEXPRESSION))))

(DEFUN ST-CREATE.VARIABLE (SORT)
						;INPUT: A SORT
						;VALUE: A VARIABLE ADDRESS
						;EFFECT: A VARIABLE WITH SORT SORT IS CREATED
  (PROG ((ADDRESS (DT-VARIABLE.CREATE SORT))) (DT-PUTPROP ADDRESS 'DT*ST-KIND 'SYS-VAR) (DT-PUTPROP ADDRESS 'ST*DATA NIL)
	(ST=PUT.NEW.ON.STACK (INTERN (DT-VARIABLE.PNAME ADDRESS)
				     (find-package "MKRP")) ADDRESS)
	(PUTASSOC (INTERN (DT-VARIABLE.PNAME ADDRESS)
			  (find-package "MKRP")) ADDRESS ST*SYMBOL.ADDRESSES) (RETURN ADDRESS)))

(DEFUN ST-CREATE.CONSTANT (SORT)
						;INPUT: A SORT
						;VALUE: A CONSTANT ADDRESS
						;EFFECT: A CONSTANT WITH SORT SORT IS CREATED
  (PROG ((ADDRESS (DT-CONSTANT.CREATE NIL SORT))) (DT-PUTPROP ADDRESS 'DT*ST-KIND 'SYS-CONST) (DT-PUTPROP ADDRESS 'ST*DATA NIL)
	(ST=PUT.NEW.ON.STACK (INTERN (DT-CONSTANT.PNAME ADDRESS)
				     (find-package "MKRP")) ADDRESS)
	(PUTASSOC (INTERN (DT-CONSTANT.PNAME ADDRESS)
			  (find-package "MKRP")) ADDRESS ST*SYMBOL.ADDRESSES) (RETURN ADDRESS)))

(DEFUN ST-CREATE.FUNCTION (DOMAIN RANGE SORTLIST)
						;INPUT: DOMAINSORTS , RANGE SORT AND COMPLETE
						;       SORTLIST. (SORTLIST = NIL FOR NOT POLY-
						;       MORPHIC FUNCTIONS.)
						;VALUE: A FUNCTION ADDRESS
						;EFFECT: A FUNCTION WITH INPUT VALUES IS CREATED
  (PROG ((ADDRESS (DT-FUNCTION.CREATE NIL RANGE DOMAIN SORTLIST))) (DT-PUTPROP ADDRESS 'DT*ST-KIND 'SYS-FUNCT)
	(DT-PUTPROP ADDRESS 'ST*DATA NIL) (ST=PUT.NEW.ON.STACK (INTERN (DT-FUNCTION.PNAME ADDRESS)
								       (find-package "MKRP")) ADDRESS)
	(PUTASSOC (INTERN (DT-FUNCTION.PNAME ADDRESS)
			  (find-package "MKRP")) ADDRESS ST*SYMBOL.ADDRESSES) (RETURN ADDRESS)))

(DEFUN ST=GET.SYMBOLENTRY (SYMBOLNAME)
						;EDITED AT 9-JUL-82 |11:48|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  AN ARRAY, DENOTING THE ENTRIES OF SYMBOLNAME
  (CASSOC SYMBOLNAME ST*SYMBOL.ADDRESSES))

(DEFUN ST=GET.SYMBOLARITY (SYMBOLNAME)
						;EDITED AT 6-JUL-82 |11:19|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE ARITY ENTRY OF SYMBOLNAME, IF SYMBOL-
						;        NAME HAS AN ENTRY IN THE TABLE, ELSE NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)))
	(RETURN
	  (COND
	    ((NUMBERP ADDRESS)
	     (CASE (DT-TYPE ADDRESS) (FUNCTION (DT-FUNCTION.ARITY ADDRESS))
		   (PREDICATE (LIST-LENGTH (DT-PREDICATE.DOMAINSORTS ADDRESS))) (OTHERWISE NIL)))
	    (T NIL)))))

(DEFUN ST=GET.SYMBOLATTRIBUTE (SYMBOLNAME)
						;EDITED AT 6-JUL-82 |11:19|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE ATTRIBUTE ENTRY OF SYMBOLNAME,IF SYMBOL-
						;        NAME HAS AN ENTRY IN THE TABLE, ELSE NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)))
	(RETURN
	  (COND
	    ((NUMBERP ADDRESS)
	     (CASE (DT-TYPE ADDRESS) (FUNCTION (DT-FUNCTION.ATTRIBUTES ADDRESS)) (PREDICATE (DT-PREDICATE.ATTRIBUTES ADDRESS))
		   (OTHERWISE NIL)))
	    (T NIL)))))

(DEFUN ST=GET.SYMBOLDATA (SYMBOLNAME)
						;EDITED AT 6-JUL-82 |11:20|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE DATA ENTRY OF SYMBOLNAME, IF SYMBOL-
						;        NAME HAS AN ENTRY IN THE TABLE, ELSE NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)))
	(RETURN (COND ((NUMBERP ADDRESS) (DT-GETPROP ADDRESS 'ST*DATA)) (T (GET SYMBOLNAME 'ST*DATA))))))

(DEFUN ST=GET.SYMBOLDOMAIN (SYMBOLNAME)
						;EDITED AT 6-JUL-82 |11:21|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE DOMAIN ENTRY OF SYMBOLNAME, IF SYMBOL-
						;        NAME HAS AN ENTRY IN THE TABLE, ELSE NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)))
	(RETURN
	  (COND
	    ((NUMBERP ADDRESS)
	     (CASE (DT-TYPE ADDRESS) (FUNCTION (DT-FUNCTION.DOMAINSORTS ADDRESS)) (PREDICATE (DT-PREDICATE.DOMAINSORTS ADDRESS))
		   (OTHERWISE NIL)))
	    (ADDRESS (DT-SORT.DIRECT.SUBSORTS SYMBOLNAME)) (T NIL)))))

(DEFUN ST=GET.SYMBOLKIND (SYMBOLNAME)
						;EDITED AT 6-JUL-82 |11:21|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE KIND ENTRY OF SYMBOLNAME, IF SYMBOL-
						;        NAME HAS AN ENTRY IN THE TABLE, ELSE NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)))
	(RETURN
	  (COND ((NUMBERP ADDRESS) (COND ((DT-GETPROP ADDRESS 'DT*ST-KIND)) (T (DT-TYPE ADDRESS)))) (ADDRESS 'SORT) (T NIL)))))

(DEFUN ST=GET.SYMBOLRANGE (SYMBOLNAME)
						;EDITED AT 7-JUL-82 |17:02|
						;INPUT:  SYMBOLNAME - AN ATOM, DENOTING AN ENTRY IN
						;        THE SYMBOLTABLE.
						;EFFECT: RETURNS VALUE.
						;VALUE:  THE RANGE ENTRY OF SYMBOLNAME, IF SYMBOL-
						;        NAME HAS AN ENTRY IN THE TABLE, ELSE NIL.
  (PROG ((ADDRESS (ST=GET.SYMBOLENTRY SYMBOLNAME)))
	(RETURN
	  (COND
	    ((NUMBERP ADDRESS)
	     (CASE (DT-TYPE ADDRESS) (FUNCTION (DT-FUNCTION.MAX.RANGE.SORT ADDRESS)) (CONSTANT (DT-CONSTANT.SORT ADDRESS))
		   (VARIABLE (DT-VARIABLE.SORT ADDRESS)) (OTHERWISE NIL)))
	    (ADDRESS (DT-SORT.DIRECT.SUPERSORTS SYMBOLNAME)) (T NIL)))))

(DEFUN ST=PUT.NEW.ON.STACK (SYMBOLNAME ADDRESS)
						;EDITED AT 5-JUL-82 |14:42|
						;INPUT:   A SYMBOLNAME AND ITS FULL CLASSIFICATION.
						;EFFECT:  IF SYMBOLNAME IS NEW, A NEW ENTRY IN THE
						;         DATASTRUCTURES IS CREATED.IF SYMBOLNAME IS
						;         NOT IN (CAR STACK1), THEN THE REQUIRED
						;         S-EXPRESSION TO REMOVE THIS ENTRY IS
						;         PLACED IN (CAR STACK1).
						;VALUE :  UNDEFINED.
  (COND
    ((NUMBERP ADDRESS)
     (RPLACA ST*STACK1
	     (CONS
	       (LIST SYMBOLNAME 'PROGN
		     (LIST
		       (CASE (DT-TYPE ADDRESS)
			 (VARIABLE 'DT-VARIABLE.DELETE)
			 (PREDICATE 'DT-PREDICATE.DELETE)
			 (CONSTANT 'DT-CONSTANT.DELETE)
			 (FUNCTION 'DT-FUNCTION.DELETE)
			 (OTHERWISE (ERROR "WRONG TYPE: ~A" NIL)))
		       ADDRESS)
		     (LIST 'setq 'ST*SYMBOL.ADDRESSES (LIST 'REMASSOC `',SYMBOLNAME 'ST*SYMBOL.ADDRESSES)))
	       (CAR ST*STACK1))))
    (ADDRESS
     (RPLACA ST*STACK1
	     (CONS
	       (LIST SYMBOLNAME 'PROGN (LIST 'DT-SORT.ST.REMOVE `',SYMBOLNAME)
		     (LIST 'setq 'ST*SYMBOL.ADDRESSES (LIST 'REMASSOC `',SYMBOLNAME 'ST*SYMBOL.ADDRESSES)))
	       (CONS (LIST SYMBOLNAME 'REMPROP `',SYMBOLNAME ''ST*DATA) (CAR ST*STACK1)))))))

(DEFUN ST=PUT.CHANGE.ON.STACK (SYMBOLNAME ADDRESS)
						;EDITED AT 5-JUL-82 |14:42|
						;INPUT:   A SYMBOLNAME AND ITS FULL CLASSIFICATION.
						;EFFECT:  IF SYMBOLNAME IS NEW, A NEW ENTRY IN THE
						;         DATASTRUCTURES IS CREATED.IF SYMBOLNAME IS
						;         NOT IN (CAR STACK1), THEN THE REQUIRED
						;         S-EXPRESSION TO REMOVE THIS ENTRY IS
						;         PLACED IN (CAR STACK1).
						;VALUE :  UNDEFINED.
  (COND
    ((NUMBERP ADDRESS)
     (RPLACA ST*STACK1
	     (CONS
	       (CASE (DT-TYPE ADDRESS)
		 (VARIABLE (LIST SYMBOLNAME 'DT-VARIABLE.PUTSORT ADDRESS `',(DT-VARIABLE.SORT ADDRESS)))
		 (CONSTANT (LIST SYMBOLNAME 'DT-CONSTANT.PUTSORT ADDRESS `',(DT-CONSTANT.SORT ADDRESS)))
		 (FUNCTION
		   (LIST SYMBOLNAME 'PROG1 NIL
			 (LIST 'DT-FUNCTION.CHANGE ADDRESS `',(DT-FUNCTION.SORT ADDRESS) `',(DT-FUNCTION.DOMAINSORTS ADDRESS) NIL)
			 (LIST 'DT-FUNCTION.PUT.ATTRIBUTES ADDRESS `',(DT-FUNCTION.ATTRIBUTES ADDRESS))))
		 (PREDICATE
		   (LIST SYMBOLNAME 'PROG1 NIL (LIST 'DT-PREDICATE.PUTSORT ADDRESS `',(DT-PREDICATE.DOMAINSORTS ADDRESS))
			 (LIST 'DT-PREDICATE.PUT.ATTRIBUTES ADDRESS `',(DT-PREDICATE.ATTRIBUTES ADDRESS))))
		 (OTHERWISE NIL))
	       (CONS
		 `(,SYMBOLNAME PROG1 NIL
		   (DT-PUTPROP ,ADDRESS 'st*data ',(DT-GETPROP ADDRESS 'ST*DATA))
		   (DT-PUTPROP ,ADDRESS 'dt*st-kind ',(DT-GETPROP ADDRESS 'DT*ST-KIND)))
		 (CAR ST*STACK1)))))
    (ADDRESS
     (RPLACA ST*STACK1
	     (CONS
	       (LIST SYMBOLNAME 'DT-SORT.ST.PUT.DIRECT.SUPERSORTS `',SYMBOLNAME
		     `',(DT-SORT.DIRECT.SUPERSORTS SYMBOLNAME))
	       (CONS `(,SYMBOLNAME  setf (get ',SYMBOLNAME 'st*data) ',(GET SYMBOLNAME 'ST*DATA))
		     (CAR ST*STACK1)))))))


(DEFUN ST=STACK.EMPTY (STACK.INDICATION)
						;EDITED AT 8-JUL-82 |13:24|
						;INPUT:  ONE OF THE ATOMS STACK1, STACK2 OR STACK3.
						;EFFECT: RETURNS VALUE.
						;VALUE:  T IF THE SYMBOLTABLE IS EMPTY, NIL ELSE.
  (COND
    ((EQUAL (LIST NIL)
	    (CASE STACK.INDICATION (STACK1 ST*STACK1) (STACK2 ST*STACK2)
		  (OTHERWISE (ERROR "ST=STACK.EMPTY : ILLEGAL STACKINDICATION = ~A" STACK.INDICATION))))
     T)
    (T NIL)))

(DEFUN ST=CLEAR.STACK (STACK.INDICATION)
						;EDITED AT 8-JUL-82 |13:22|
						;INPUT:  ONE OF THE ATOMS STACK1, STACK2 OR STACK3.
						;EFFECT: CLEARS THE STACK INDICATED.
						;VALUE:  UNDEFINED.
  (PROG (STACK)
	(CASE STACK.INDICATION (STACK1 (SETQ STACK ST*STACK1) (SETQ ST*STACK1 (LIST NIL)))
	      (STACK2 (SETQ STACK ST*STACK2) (SETQ ST*STACK2 (LIST NIL)))
	      (OTHERWISE (ERROR "ST=CLEAR.STACK. ILLEGAL STACKINDICATION = ~A" STACK.INDICATION)))
	(MAPC (FUNCTION ST=RELEASE.SYMBOLTABLE) STACK)))

(DEFUN ST=RELEASE.SYMBOLTABLE (SYMBOLTABLE)
						; EDITED AT 8-JUL-82 |13:03|
						; INPUT:  A SYMBOLTABLE.
						; EFFECT: THE ARRAYS, USED FOR THE SYMBOLENTRIES ARE ADDED TO THE LIST ST*FREE.ROWS.
						; VALUE:  UNDEFINED
  (MAPC (FUNCTION (LAMBDA (ELEMENT) (EVAL (CDR ELEMENT))))
	SYMBOLTABLE))

(DEFVAR ST*STACK1 (LIST NIL))

(DEFVAR ST*STACK2 (LIST NIL))

(DEFVAR ST*LOAD.FLAG T)

(DEFVAR ST*SYMBOL.ADDRESSES (LIST NIL))

