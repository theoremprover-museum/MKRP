;;; -*- Syntax: Common-lisp; Mode: LISP; Package: MKRP -*-
(IN-PACKAGE "MKRP" :use '("CL"))

(DEFVAR FMT*NUMBER.OF.AREAS NIL)



(DEFVAR FMT*UNDO.STACK NIL)

(defvar FMT*LOAD.FLAG nil)

(defvar FMT*AREA.POINTERS nil)

(defvar FMT*UNDO.INDICATOR nil)

(defvar fmt*language 'english)

(DEFUN FMT-EDIT (&OPTIONAL FORMULA.NUMBER)
						; EDITED:28-FEB-83.
						; INPUT: NATURAL, 1 <= 'FORMULA.NUMBER' <= NUMBER OF
						;        FORMULAS IN AREA 0.
						; EFFECT:CALLS THE LISPEDITOR FOR THE CAR OF THE
						;        'FORMULA.NUMBER'TH FORMULA OF AREA 0. PUTS
						;        THE CORRESPONDING COMMANDS ON THE UNDO-STACK.
						; VALUE: NIL OR ERROR-MESSAGE (INCORRECT INPUT).
  (COND ((OR (NOT (integerP FORMULA.NUMBER))
	     (< FORMULA.NUMBER 0))
						; (1A) 1ST ERRORCASE: NOT NATURAL.
	 (FMT=ERRORS NATURAL))
	((OR (EQl FORMULA.NUMBER 0)
	     (< (FMT=NUMBER.OF.FORMULAS 0) FORMULA.NUMBER))
						; (1B) 2ND ERRORCASE: POSITION NOT DEFINED.
	 (FMT=ERRORS AREA))
	(T (let ((FORMULA.TO.EDIT (FMT=FORMULA FORMULA.NUMBER 0)))	; (2) UNDO-HANDLING.
	     (FMT=PUSH.UNDO (LIST 'EDIT 'PROGN
				  (LIST 'FMT-DELETE FORMULA.NUMBER)
				  (LIST 'FMT-INSERT
					(KWOTE (COPY-tree FORMULA.TO.EDIT))
					FORMULA.NUMBER)))	; (3) CALL OF EDITOR.
	     (rplaca formula.to.edit
		     (let* ((*readtable* (edt-standard-readtable))
			    (val (EDT=edit.expression (list (CAR FORMULA.TO.EDIT)))))
		       (if (and (consp val) (eq (car val) :list)) (second val) val))))
	   nil)))

(DEFUN FMT-RESET (MAX.AREA.NUMBER)
						; EDITED:23-FEB-83.
						; INPUT: NATURAL, 'MAX.AREA.NUMBER' > 0.
						; EFFECT:INITIALIZES FMT WITH 'MAX.AREA.NUMBER'+1
						;        EMPTY AREAS, AN EMPTY UNDO-STACK, AND THE
						;        GLOBAL VARIABLE 'NUMBER.OF.AREAS' WITH
						;        'MAX.AREA.NUMBER'+1.
						;        SETS 'UNDO.INDICATOR' ON 'NOT.UNDO'.
						; VALUE: NIL.
  (SETQ FMT*AREA.POINTERS (MAKE-ARRAY (LIST (1+ MAX.AREA.NUMBER))))
  (DODOWN (RPTN (1+ MAX.AREA.NUMBER))
    (setf (aref FMT*AREA.POINTERS RPTN) (LIST NIL)))
  (SETQ FMT*UNDO.STACK NIL)
  (SETQ FMT*NUMBER.OF.AREAS (1+ MAX.AREA.NUMBER)) (SETQ FMT*UNDO.INDICATOR 'NOT.UNDO) (SETQ FMT*LOAD.FLAG T) NIL)

(DEFUN FMT-IS.RESET NIL
						;EDITED:5-MAY-83.
						;INPUT:  -
						;EFFECT: -
						;VALUE: T IF UNDO-STACK IS EMPTY ELSE FALSE.
  (COND ((NULL FMT*UNDO.STACK) T) (T NIL)))

(DEFUN FMT-SET.LOAD.FLAG (FLAG)
						;EDITED:1-JUN-83.
						;INPUT: T OR NIL.
						;EFFECT:SETS FMT*LOAD.FLAG CORRESPONDING TO INPUT.
						;       IF FLAG IS T FORMULAS OF FILES IN FMT-LOAD
						;       ARE INSERTED IN ALL AREAS.
						;VALUE: NIL
  (SETQ FMT*LOAD.FLAG FLAG) NIL)

(DEFUN FMT-SET.LANGUAGE (LANGUAGE)
						;EDITED:24-FEB-83.
						;INPUT: ATOM 'ENGLISH OR 'DEUTSCH.
						;EFFECT:DEFINES THE ANSWERING LANGUAGE OF THE MODULE
						;       IN THE ERRORCASE.
						;VALUE: NIL.
  (SETQ FMT*LANGUAGE LANGUAGE) NIL)

(DEFUN FMT-INSERT (FORMULA N)
						;EDITED:23-FEB-83.
						;INPUT: A S-EXPRESSION 'FORMULA' AND AN INTEGER N,
						;       WITH 1 <= //'N'// <= 1+NUMBER OF FORMULAS IN
						;       AREA 0.
						;EFFECT:INSERTS 'FORMULA' IN AREA 0 BEFORE THE 'N'TH
						;       ELEMENT. IF N IS NEGATIVE, IT'S COUNTED FROM
						;       BEHIND. THE NAME 'INSERT' AND AN ANTIVALENT
						;       FUNCTION IS PUSHED ON THE UNDO-STACK.
						;VALUE: NIL OR ERROR-MESSAGE (INPUT NOT OK).
  (COND ((NOT (INTEGERP N))			; (1A) 1ST ERRORCASE: N ISN'T INTEGER.                
	 (FMT=ERRORS INTEGER))
	((OR (< (ABS N) 1) (< (1+ (FMT=NUMBER.OF.FORMULAS 0)) (ABS N)))	; (1B) 2ND ERRORCASE: POSITION IN AREA NOT DEFINED.  
	 (FMT=ERRORS AREA))
	(T ;; (2) INSERT.                                        
	 (QINSERT-NTH (FMT=AREA 0) N FORMULA)
	 ;; (3) UNDOHANDLING.                                   
	 (FMT=PUSH.UNDO (LIST 'INSERT 'FMT-DELETE N))
	 ;; (4) VALUE OF FUNCTION, IF NOT ERROR.               
	 NIL)))

(DEFUN FMT-DELETE (FORMULA.NUMBER)
						;EDITED:23-FEB-83.
						;INPUT: INTEGER, 1 <= //'FORMULA.NUMBER'// <=
						;       NUMBER OF FORMULAS IN AREA 0.
						;EFFECT:REMOVES 'FORMULA.NUMBER'TH FORMULA OF AREA
						;       0. IF 'FORMULA.NUMBER' IS NEGATIVE, THEN
						;       IT'S COUNTED FROM BEHIND. AN ANTIVALENT
						;       FUNCTION AND THE
						;       NAME 'DELETE' IS PUT ON THE UNDO-STACK.
						;VALUE: NIL OR ERROR-MESSAGE (INPUT NOT OK).
  (COND
    ((NOT (INTEGERP FORMULA.NUMBER))		; (1A) 1ST ERRORCASE: FORMULA.NUMBER ISN'T INTEGER. 
     (FMT=ERRORS INTEGER))
    ((OR (< (ABS FORMULA.NUMBER) 1) (< (FMT=NUMBER.OF.FORMULAS 0) (ABS FORMULA.NUMBER)))
						; (1B) 2ND ERRORCASE: POS. ISN'T DEFINED IN AREA 0. 
     (FMT=ERRORS AREA))
    (T						; (2) UNDOHANDLING.                                 
     (FMT=PUSH.UNDO
       (LIST 'DELETE 'FMT-INSERT (KWOTE (COPY-TREE (FMT=FORMULA FORMULA.NUMBER 0))) FORMULA.NUMBER))
						; (3) DELETE.                                       
     (QDELETE-NTH (FMT=AREA 0) FORMULA.NUMBER)
						; (4) VALUE OF FUNCTION IS NIL.                       
     NIL)))

(DEFUN FMT-SWITCH (FORMULA.NUMBER1 FORMULA.NUMBER2)
						;EDITED:24-FEB-83.
						;INPUT: 2 NATURALS, 1 <= 'FORMULA.NUMBER1',
						;       'FORMULA.NUMBER2' <= NUMBER OF FORMULAS IN
						;       AREA 0.
						;EFFECT:EXCHANGES THE 'FORMULA.NUMBER1'TH AND THE
						;       'FORMULA.NUMBER2'TH FORMULA IN AREA 0.
						;VALUE: NIL OR ERROR-MESSAGE (INCORRECT INPUT).
  (COND
    ((OR (NOT (INTEGERP FORMULA.NUMBER1)) (NOT (INTEGERP FORMULA.NUMBER2)) (< FORMULA.NUMBER1 0) (< FORMULA.NUMBER2 0))
						; (1A) 1ST ERRORCASE: NOT NATURAL NUMBER.             
     (FMT=ERRORS NATURAL))
    ((OR (ZEROP FORMULA.NUMBER1) (ZEROP FORMULA.NUMBER2) (< (FMT=NUMBER.OF.FORMULAS 0) FORMULA.NUMBER1)
	 (< (FMT=NUMBER.OF.FORMULAS 0) FORMULA.NUMBER2))
						; (1B) 2ND ERRORCASE: POSITION IN AREA NOT DEFINED.   
     (FMT=ERRORS AREA))
    (T						; (2) EXCHANGE.                                       
     (FMT=EXCHANGE.CAR (NTHCDR (1- FORMULA.NUMBER1) (CAR (FMT=AREA 0))) (NTHCDR (1- FORMULA.NUMBER2) (CAR (FMT=AREA 0))))
						; (3) UNDOHANDLING.                                   
     (FMT=PUSH.UNDO (LIST 'SWITCH 'FMT-SWITCH FORMULA.NUMBER1 FORMULA.NUMBER2))
						; (4) VALUE OF FUNCTION BECOMES NIL.                  
     NIL)))

(DEFUN FMT=EXCHANGE.CAR (CELL1 CELL2)
						;EDITED:10-MAR-83.
						;INPUT: 2 POINTERS ON CELLS.
						;EFFECT:CARS OF CELL1 AND CELL2 ARE EXCHANGED.
						;VALUE: NIL.
  (PROG ((HELP (CAR CELL1))) (RPLACA CELL1 (CAR CELL2)) (RPLACA CELL2 HELP)))

(DEFUN FMT-SHIFT (FROM.AREA TO.AREA)
						;EDITED:24-FEB-83.
						;INPUT: 2 NATURAL NUMBERS., 0 <= 'FROM.AREA',
						;       'TO.AREA' <= MAXIMUM AREA NUMBER.
						;EFFECT:FIRST FORMULA WILL BE DELETED IN 'FROM.AREA'
						;       AND PUT AT LAST POSITION IN 'TO.AREA' IF
						;       'FROM.AREA' < 'TO.AREA, OR LAST FORMULA OF
						;       'FROM.AREA' IS DELETED AND INSERTED AS FIRST
						;       FORMULA IN 'TO.AREA' IF 'TO.AREA' <
						;       'FROM.AREA'.
						;VALUE: NIL OR ERROR-MEESAGE (INPUT ERROR).
  (COND
    ((OR (NOT (INTEGERP FROM.AREA)) (NOT (INTEGERP TO.AREA)) (< FROM.AREA 0) (< TO.AREA 0))
						; (1A) 1ST ERRORCASE: NOT NAT. NUMBER.                
     (FMT=ERRORS NAT))
    ((OR (< FMT*NUMBER.OF.AREAS (1+ TO.AREA)) (< FMT*NUMBER.OF.AREAS (1+ FROM.AREA)))
						; (1B) 2ND ERRORCASE: AREA NOT DEFINED.               
     (FMT=ERRORS INDEX))
    ((ZEROP (FMT=NUMBER.OF.FORMULAS FROM.AREA))	; (1C) 3RD ERRORCASE: AREA EMPTY.                     
     (FMT=ERRORS EMPTY))
    (T						; (2) SHIFT: CONDITIONAL, RESPECTING EFFECT.          
     (COND
       ((< TO.AREA FROM.AREA) (QINSERT-NTH (FMT=AREA TO.AREA) 1 (SECOND (FMT=AREA FROM.AREA)))
	(QDELETE-NTH (FMT=AREA FROM.AREA) -1))
       ((< FROM.AREA TO.AREA) (QINSERT-NTH (FMT=AREA TO.AREA) -1 (CAAR (FMT=AREA FROM.AREA)))
	(QDELETE-NTH (FMT=AREA FROM.AREA) 1)))
						; (3) UNDO HANDLING.                                  
     (FMT=PUSH.UNDO (LIST 'SHIFT 'FMT-SHIFT TO.AREA FROM.AREA))
						; (4) VALUE OF FUNCTION BECOMES NIL.                  
     NIL)))

(DEFUN FMT-REPLACE (OLDSYMBOL NEWSYMBOL AREA.NUMBER)
						;EDITED:24-FEB-83.
						;INPUT: 2 ATOMS AND A NATURAL,
						;       1 <= 'AREA.NUMBER' <= MAXIMAL AREA NUMBER.
						;EFFECT:REPLACES ALL APPEARENCES OF 'OLDSYMBOL' IN
						;       AREA 'AREA.NUMBER' BY 'NEWSYMBOL'.
						;VALUE: NIL OR ERROR-MESSAGE (INCORRECT INPUT).
						;REMARK:REPLACE ISN'T UNDOABLE.
  (COND
    ((NOT (AND (ATOM OLDSYMBOL) (ATOM NEWSYMBOL)))	; (1A) 1ST ERRORCASE: OLDSYMBOL OR NEWSYMBOL ARE NOT  
						;      ATOMS.                                         
     (FMT=ERRORS ATOM))
    ((OR (< AREA.NUMBER -1) (< FMT*NUMBER.OF.AREAS (1+ AREA.NUMBER)))
						; (1B) 2ND ERRORCASE: AREA ISN'T DEFINED.             
     (FMT=ERRORS INDEX))
    (T						; (2) REPLACE.                                        
     (NSUBST NEWSYMBOL OLDSYMBOL (CAR (FMT=AREA AREA.NUMBER)))
						; (3) VALUE OF FUNCTION IS NIL.                       
     NIL)))

(DEFUN FMT-WRITE (FILE)
						;EDITED:4-MAR-83.
						;INPUT: ATOM, WHICH IS THE NAME OF A FILE OR NIL.
						;EFFECT:MAKES A S-EXPRESSION X, SO THAT THE EVALUA=
						;       TION OF X JOINES THE NOW EXISTING FORMULAS
						;       TO THE THEN BEING AREAS, AS SPECIFIED IN
						;       FMT-LOAD. X IS WRITTEN ON 'FILE' OR IF 'FILE'
						;       = NIL THE VALUE OF THIS FUNCTION IS X.
						;VALUE: NIL OR X.
  (PROG ((LOAD.LIST NIL))
	(DODOWN (RPTN FMT*NUMBER.OF.AREAS) (SETQ LOAD.LIST (CONS (CAR (FMT=AREA RPTN)) LOAD.LIST)))
	(COND ((EQ FILE NIL) (RETURN (CONS 'FMT-LOAD LOAD.LIST)))
	      (T (format file "(FMT-LOAD ~S~{~%  ~A~})" (DATE) LOAD.LIST)))))

(defmacro fmt-load (date &rest formula.lists)
  `(fmt=load ',date ',(copy-list formula.lists)))

(DEFUN FMT=LOAD (DATE FORMULA.LISTS)
						;EDITED:4-MAR-83.
						;INPUT: ANY NUMBER OF LISTS OF S-EXPRESSIONS.
						;EFFECT:INSERTS THE FORMULAS OF INPUT-LISTS IN THE
						;       CORRESPONDING AREAS, IF LAST EXECUTED
						;       FUNCTION WAS A RESET OR FMT*LOAD.FLAG IS T
						;       AND NUMBER OF AREAS AS
						;       NUMBER OF INPUT-LISTS ARE THE SAME. ELSE ALL
						;       FORMULAS ARE PUT IN AREA 0.
						;       IN AREA 0 AT FRONT, IN ALL OTHER AREAS AT THE
						;       END.
						;       AN ANTIVALENT COMMAND IS LAID ON THE
						;       UNDO-STACK.
						;VALUE: NIL.
  (PRINC (FMT=OUTPUTS DATUM) *STANDARD-OUTPUT*) (PRINC DATE *STANDARD-OUTPUT*)
  (TERPRI *STANDARD-OUTPUT*)
  (PROG
    ((UNDO.LIST NIL) (NUMBER.OF.INSERTED.FORMULAS 0)
     (POINTER.OF.LAST.FORMULA (NTHCDR (LIST-LENGTH (CAR FORMULA.LISTS)) (CAR FORMULA.LISTS))))
    (COND
      ((AND FMT*LOAD.FLAG (EQ (LIST-LENGTH FORMULA.LISTS) FMT*NUMBER.OF.AREAS))
						; (1) IN ALL AREAS.                                   
						; (1A) DISTRIBUTION.                                  
       (SETQ UNDO.LIST (CONS (LIST 'FMT=CUT (LIST-LENGTH (CAR FORMULA.LISTS)) 1 0) UNDO.LIST))
       (RPLACA (FMT=AREA 0) (NCONC (CAR FORMULA.LISTS) (CAR (FMT=AREA 0)))) (SETQ FORMULA.LISTS (CDR FORMULA.LISTS))
       (COND ((NULL (CDR (FMT=AREA 0))) (RPLACD (FMT=AREA 0) POINTER.OF.LAST.FORMULA)))
       (DODOWN (RPTN (1- FMT*NUMBER.OF.AREAS))
	 (PROGN
	   (SETQ UNDO.LIST
		 (CONS (LIST 'FMT=CUT (LIST-LENGTH (CAR FORMULA.LISTS)) -1 (- FMT*NUMBER.OF.AREAS (1+ RPTN))) UNDO.LIST))
	   (QCONC (FMT=AREA (- FMT*NUMBER.OF.AREAS (1+ RPTN))) (CAR FORMULA.LISTS)) (SETQ FORMULA.LISTS (CDR FORMULA.LISTS))))
						; (1B) UNDO HANDLING.                                 
       (FMT=PUSH.UNDO (CONS 'LOAD (CONS 'PROGN UNDO.LIST))) (RETURN T))
      (T					; (2A) INSERTING IN AREA 0.                           
       (DODOWN (RPTN (LIST-LENGTH FORMULA.LISTS))
	 (PROGN
	   (SETQ NUMBER.OF.INSERTED.FORMULAS
		 (+ NUMBER.OF.INSERTED.FORMULAS (LIST-LENGTH (CAR FORMULA.LISTS))))
	   (RPLACA (FMT=AREA 0) (NCONC (CAR FORMULA.LISTS) (CAR (FMT=AREA 0)))) (SETQ FORMULA.LISTS (CDR FORMULA.LISTS))))
						; (2B) CORRECTING TCONC POINTER.                      
       (COND ((NULL (CDR (FMT=AREA 0))) (RPLACD (FMT=AREA 0) POINTER.OF.LAST.FORMULA)))
						; (2C) UNDO HANDLING.                                 
       (FMT=PUSH.UNDO (LIST 'LOAD 'FMT=CUT NUMBER.OF.INSERTED.FORMULAS 1 0))))))

(DEFUN FMT=CUT (NUMBER.TO.CUT POSITION.TO.CUT AREA)
						;EDITED:4-MAR-83.
						;INPUT: NATURAL, 'NUMBER.TO.CUT' <= NUMBER OF
						;       FORMULAS IN AREA 'AREA'.
						;       'POSITION.TO.CUT' 1 OR -1.
						;       0 <= 'AREA' <= MAX.AREA.NUMBER.
						;EFFECT:DELETES THE 'NUMBER.TO.CUT' FIRST OR LAST
						;       FORMULAS OF AREA 'AREA'.
						;VALUE: NIL.
						;REMARK:NO INPUT-ERROR-CHECK.
						;       UNDO OF LOAD.
						;(1) DELETING.
  (DODOWN (RPTN NUMBER.TO.CUT) (QDELETE-NTH (FMT=AREA AREA) POSITION.TO.CUT))
						; (2) VALUE OF FUNCTION IS NIL.                       
  NIL)

(DEFUN FMT-UNDO NIL
						;EDITED:28-FEB-83.
						;INPUT:  -
						;EFFECT:THE LAST DESTRUCTIVE COMMAND IS ANNULED,
						;       AND ONE ELEMENT IS PUT FROM THE UNDO-STACK.
						;VALUE: NIL OR ERROR-MESSAGE (UNDO-STACK EMPTY).
  (COND ((EQ FMT*UNDO.STACK NIL) (FMT=ERRORS UNDO.STACK.EMPTY))
	(T (SETQ FMT*UNDO.INDICATOR 'UNDO) (EVAL (CDAR FMT*UNDO.STACK)) (SETQ FMT*UNDO.STACK (CDR FMT*UNDO.STACK))
	   (SETQ FMT*UNDO.INDICATOR 'NOT.UNDO) NIL)))

(DEFUN FMT=PUSH.UNDO (COMMAND.TO.PUSH)
						;EDITED:11-MAR-83.
						;INPUT: 'COMMAND.TO.PUSH' IS A LIST, WITH LAST
						;       DESTRUCTIVE COMMAND AS FIRST TOP LEVEL
						;       ELEMENT AND ANTIVALENT FUNCTIONS AS REST.
						;EFFECT:PUSHES 'COMMAND.TO.PUSH' ON UNDO-STACK, IF
						;       UNDO INDICATOR IS ON NOT UNDO.
						;VALUE: THE NEW UNDO-STACK.
  (COND ((EQ FMT*UNDO.INDICATOR 'NOT.UNDO) (SETQ FMT*UNDO.STACK (CONS COMMAND.TO.PUSH FMT*UNDO.STACK)))))

(DEFUN FMT-COMMAND (COMMAND &REST ARGUMENTS)	; EDITED:14-MAR-83.
						; INPUT: A NOT DESTRUCTIVE COMMAND:
						;        STATE,
						;        PP 'FILE',
						;        PRINT.FORM 'FORM.SORT' 'FROM' 'TO',
						; EFFECT:THE COMMAND WILL BE EXECUTED, AND OUTPUT IS
						;        PRODUCED ON THE TERMINAL OR ON A FILE.
						; VALUE: NIL OR ERROR-MESSAGE.
  (CASE COMMAND
    (STATE
      (COND ((AND (< 0 (LIST-LENGTH ARGUMENTS)) (NOT (mkrp-OUTSTREAMP (CAR ARGUMENTS)))) (FMT=ERRORS FILE))
	    (T (FMT=STATE (CAR ARGUMENTS)) NIL)))
    (PP
      (COND ((AND (< 0 (LIST-LENGTH ARGUMENTS)) (NOT (mkrp-INSTREAMP (CAR ARGUMENTS)))) (FMT=ERRORS FILE))
	    (T (FMT=PP (CAR ARGUMENTS)) NIL)))
    (PRINT.FORM
      (COND ((NULL ARGUMENTS) (FMT=PRINT.FORM 'INFIX NIL NIL *STANDARD-OUTPUT*) NIL)
	    ((NOT (MEMBER (CAR ARGUMENTS) '(INFIX PREFIX))) (FMT=ERRORS PRINT.FORM))
	    ((OR
	       (AND (= (LIST-LENGTH ARGUMENTS) 3) (NEQ (SECOND ARGUMENTS) NIL) (NOT (INTEGERP (SECOND ARGUMENTS))))
	       (AND (= (LIST-LENGTH ARGUMENTS) 4) (NEQ (SECOND ARGUMENTS) NIL) (NEQ (THIRD ARGUMENTS) NIL)
		    (NOT (INTEGERP (SECOND ARGUMENTS))) (NOT (INTEGERP (THIRD ARGUMENTS)))))
	     (FMT=ERRORS NIL.INTEGER))
	    (T (FMT=PRINT.FORM (CAR ARGUMENTS) (SECOND ARGUMENTS) (THIRD ARGUMENTS) (FOURTH ARGUMENTS)) NIL)))
    (OTHERWISE (FMT=ERRORS COMMAND))))

(DEFUN FMT=PP (FILE)
						;EDITED:11-MAR-83.
						;INPUT: A FILE 'FILE', WHICH IS OPEN.
						;EFFECT:MAKES A 'PRETTY' PRINT OF ALL FORMULAS ON
						;       'FILE'. EXAMPLE:
						;       F O R M U L A S  :
						;
						;       ** 1 **  'FORMULA1'
						;
						;       *  2  *  'FORMULA2'
						;       THE NUMBER OF ASTERISKS GIVES THE AREA.
						;VALUE: NIL.
  (PROG
    ((FORMULA.INDEX 0) (ALL.FORMULAS (FMT=NUMBER.OF.FORMULAS 'ALL)) AREA.NUMBER
     FORMULA.POSITION)
    (PRINC (FMT=OUTPUTS SPACE.FORMULAS) FILE) (TERPRI FILE) (PRINC (FMT=OUTPUTS ===) FILE)
    (TERPRI FILE) (TERPRI FILE) (TERPRI FILE)
    (DODOWN (RPTN (FMT=NUMBER.OF.FORMULAS 'ALL))
      (PROGN (SETQ FORMULA.INDEX (1+ FORMULA.INDEX)) (SETQ AREA.NUMBER (FMT=AREA.OF.FORMULA FORMULA.INDEX))
	     (SETQ FORMULA.POSITION (FMT=POSITION.OF.FORMULA.IN.AREA AREA.NUMBER FORMULA.INDEX))
	     (FMT=WRITE.NUMBER.OF.AREA (1- FMT*NUMBER.OF.AREAS) AREA.NUMBER FORMULA.INDEX
				       (- (length (princ-to-string FORMULA.INDEX)) (length (princ-to-string FORMULA.INDEX))) FILE)
	     (FMT=PRINT.INFIX.FORM (CAR (FMT=FORMULA FORMULA.POSITION AREA.NUMBER)) FILE
				   (+ (LENGth (princ-to-string ALL.FORMULAS)) 3 FMT*NUMBER.OF.AREAS FMT*NUMBER.OF.AREAS))
	     (TERPRI FILE)))))

(DEFUN FMT=STATE (FILE)
						;EDITED:10-MAR-83.
						;INPUT: FILE 'FILE', OPEN.
						;EFFECT:PRINTS THE NUMBER OF FORMULAS IN THE AREAS IN
						;       A READABLE FORM ON PRIMARY OUTPUT FILE.
						;VALUE: NIL.
  (terpri file)
  (DODOWN (RPTN FMT*NUMBER.OF.AREAS)
    (let ((AREA.NUMBER RPTN))
      (PRINc (FMT=OUTPUTS AREA) FILE)
      (format file "~VA~A :  " (- (LENGth (princ-to-string FMT*NUMBER.OF.AREAS))
				  (LENGth (princ-to-string AREA.NUMBER ))) "" AREA.NUMBER)
      (COND ((ZEROP (FMT=NUMBER.OF.FORMULAS AREA.NUMBER)) (PRINc (FMT=OUTPUTS EMPTY) FILE))
	    ((eql (FMT=NUMBER.OF.FORMULAS AREA.NUMBER) 1) (PRINc (FMT=OUTPUTS FORMULA) FILE))
	    (T (PRINc (FMT=NUMBER.OF.FORMULAS AREA.NUMBER) FILE) (PRINC (FMT=OUTPUTS FORMULAS) FILE)))
      (TERPRI FILE)))
  nil)

(DEFUN FMT=PRINT.FORM (FORM.SORT FROM.FORMULA TO.FORMULA STREAM)
						; EDITED:10-MAR-83.
						; INPUT: 'FORM.SORT' IS ATOM 'INFIX OR 'PREFIX.
						;        'FROM.FORMULA' AND 'TO.FORMULA' ARE NIL OR
						;        INTEGER, 'FROM.FORMULA' <= 'TO.FORMULA'.
						; EFFECT:PRINTS FORMULAS 'FROM.FORMULA' TO
						;        'TO.FORMULA' ONTO PRIMARY OUTPUT FILE.
						;        EXAMPLE: ** 2 **  FORMULA2
						;                    3     FORMULA3
						;        THE NUMBER OF ASTERISKS GIVES THE AREA
						;        NUMBER. IF 'FROM.FORMULA' = NIL THEN THE LESS
						;        IF 'TO.FORMULA' = NIL THEN THE GREATEST
						;        POSSIBLE ONE IS TAKEN.
						; VALUE: NIL.
  (PROG ((NUMBER.OF.ALL.FORMULAS (FMT=NUMBER.OF.FORMULAS 'ALL)) FORMULA.INDEX AREA.NUMBER
	 FORMULA.POSITION)			;(1) CONVERTING 'FROM.FORMULA' AND 'TO.FORMULA' INTO
						;    EXISTING POSITIONS.
						;(1A) 'FROM.FORMULA'.
	(COND ((OR (NULL FROM.FORMULA) (< FROM.FORMULA 1))
	       (SETQ FROM.FORMULA 1)))		;(1B) 'TO.FORMULA'.
	(COND
	  ((OR (NULL TO.FORMULA) (< NUMBER.OF.ALL.FORMULAS TO.FORMULA)) (SETQ TO.FORMULA NUMBER.OF.ALL.FORMULAS)))
	(SETQ FORMULA.INDEX (1- FROM.FORMULA))
	(DODOWN (RPTN (- (1+ TO.FORMULA) FROM.FORMULA))
	  (PROGN				;(2) SETTING OF RUNNING VARIABLES.
						;(2A) 'FORMULA.INDEX' IS NUMBER OF FORMULA IN THE
						;     ENUMERATION OF ALL FORMULAS IN FMT.
	    (SETQ FORMULA.INDEX (1+ FORMULA.INDEX))
						;(2B) 'AREA.NUMBER' IS NUMBER OF AREA INCLUDING
						;     FORMULA WITH NUMBER 'FORMULA.INDEX'.
	    (SETQ AREA.NUMBER (FMT=AREA.OF.FORMULA FORMULA.INDEX))
						;(2C) 'FORMULA.POSITION' IS NUMBER OF FORMULA
						;     'FORMULA.INDEX' IN AREA 'AREA.NUMBER'.
	    (SETQ FORMULA.POSITION (FMT=POSITION.OF.FORMULA.IN.AREA AREA.NUMBER FORMULA.INDEX))
						;(3) OUTPUT ON PRIMARY OUTPUT FILE.
						;(3A) LINEHEADER.
	    (FMT=WRITE.NUMBER.OF.AREA (1- FMT*NUMBER.OF.AREAS) AREA.NUMBER FORMULA.INDEX
				      (- (LENGth (princ-to-string TO.FORMULA))
					 (LENGth (princ-to-string FORMULA.INDEX))) NIL)
						;(3B) FORMULA.
	    (CASE FORM.SORT
	      (INFIX
		(FMT=PRINT.INFIX.FORM
		  (CAR (FMT=FORMULA FORMULA.POSITION AREA.NUMBER))
		  stream
		  (+ FMT*NUMBER.OF.AREAS (LENGth (princ-to-string to.formula)) FMT*NUMBER.OF.AREAS 2)))
	      (PREFIX
		(PPRINT (SECOND (FMT=FORMULA FORMULA.POSITION AREA.NUMBER)) STREAM))
	      (OTHERWISE NIL))))))

(DEFUN FMT=PRINT.INFIX.FORM (EXPRESSION FILE LEFT)
						; edited: "24-nov-81 10:08:07
						; input:  an s-expression and a file
						; effect: pretty-prints expression onto file
						; value:  undefined
  (if (and (consp expression) (eq (car expression) '*))
      (mapprint expression nil nil " " #'princ file)
      (pp-print.infix.formula expression file :current.pos left)))



(DEFUN FMT=AREA.OF.FORMULA (FORMULA.INDEX)	; Edited: 10-mar-83.
						; input:  natural 'formula.index', 1 <= 'formula.index'
						;         <= number of formulas in fmt.
						; effect: -
						; value:  the number of the area ,which contains the
						;         formula with number 'formula.index'.
  (let ((NUMBER.OF.FORMULAS 1) (AREA.NUMBER -1))
    (DODOWN (RPTN FMT*NUMBER.OF.AREAS)
      (SETQ NUMBER.OF.FORMULAS (+ NUMBER.OF.FORMULAS (FMT=NUMBER.OF.FORMULAS (1- (1+ RPTN)))))
      (when (AND (< FORMULA.INDEX NUMBER.OF.FORMULAS) (MINUSP AREA.NUMBER)) (SETQ AREA.NUMBER (1- (1+ RPTN)))))
    AREA.NUMBER))

(DEFUN FMT=POSITION.OF.FORMULA.IN.AREA (AREA.NUMBER FORMULA.INDEX)
						;EDITED:8-MAR-83.
						;INPUT: 2 NATURALS, FORMULA 'FORMULA.INDEX' IS IN
						;       AREA 'AREA.NUMBER'.
						;EFFECT: -
						;VALUE: THE POSITION OF FORMULA WITH NUMBER
						;       'FORMULA.INDEX' IN AREA 'AREA.NUMBER'.
  (DODOWN (RPTN (- (1- FMT*NUMBER.OF.AREAS) AREA.NUMBER))
    (SETQ FORMULA.INDEX (- FORMULA.INDEX (FMT=NUMBER.OF.FORMULAS (+ (1+ RPTN) AREA.NUMBER)))))
  FORMULA.INDEX)

(DEFUN FMT=WRITE.NUMBER.OF.AREA (MAX.NUMBER.OF.ASTERISKS NUMBER.OF.ASTERISKS NUMBER BLANKS.BEFORE.NUMBER FILE)
						;EDITED:8-MAR-83.
						;INPUT: 4 NATURALS AND 1 OPEN FILE,
						;       'MAX.NUMBER.OF.ASTERISKS' <=
						;       'NUMBER.OF.ASTERISKS'.
						;EFFECT:ONTO THE FILE 'FILE' ARE PRINTED, WITHOUT
						;       CLOSING THE RECORD:
						;       'NUMBER.OF.ASTERISKS' ASTERISKS,
						;       'MAX.NUMBER.OF.ASTERISKS' -
						;       'NUMBER.OF.ASTERISKS' + 1 +
						;       'BLANKS.BEFORE.NUMBER' BLANKS,
						;       NUMBER,
						;       'MAX.NUMBER.OF.ASTERISKS' -
						;       'NUMBER.OF.ASTERISKS' + 1 BLANKS,
						;       'NUMBER.OF.ASTERISKS' ASTERISKS,
						;       AND 2 BLANKS.
						;VALUE: NIL.
  (fresh-line file)
  (DODOWN (RPTN NUMBER.OF.ASTERISKS) (PRINc "*" FILE))
  (dodown (n (+ MAX.NUMBER.OF.ASTERISKS 1 (- NUMBER.OF.ASTERISKS) BLANKS.BEFORE.NUMBER)) (princ " " FILE))
  (PRINc NUMBER FILE)
  (DODOWN (RPTN (+ MAX.NUMBER.OF.ASTERISKS 1 (- NUMBER.OF.ASTERISKS))) (PRINC " " FILE))
  (DODOWN (RPTN NUMBER.OF.ASTERISKS) (PRINc "*" FILE)) (PRINc "  " FILE) NIL)

(DEFUN FMT-NUMBER.OF.FORMULAS (AREA.NUMBER)
						;EDITED:24-FEB-83.
						;INPUT: NATURAL, 0 <= 'AREA.NUMBER' <= MAXIMAL AREA
						;       NUMBER.
						;EFFECT: -
						;VALUE: NUMBER OF FORMULAS IN THE AREA 'AREA.NUMBER'.
  (COND
    ((AND (INTEGERP AREA.NUMBER) (< -1 AREA.NUMBER) (< AREA.NUMBER FMT*NUMBER.OF.AREAS)) (FMT=NUMBER.OF.FORMULAS AREA.NUMBER))
    (T 0)))

(DEFUN FMT=NUMBER.OF.FORMULAS (AREA)
						;EDITED:8-MAR-83.
						;INPUT: ATOM 'AREA': 'ALL OR NATURAL, 0 <=
						;       'AREA' <= MAXIMAL AREA NUMBER .
						;EFFECT: -
						;VALUE: THE NUMBER OF FORMULAS IN AREA 'AREA', IF
						;       'AREA' IS NATURAL, ELSE THE NUMBER OF ALL
						;       FORMULAS IN FMT.
  (COND
    ((EQ AREA 'ALL)
     (PROG ((NUMBER 0))
	   (DODOWN (RPTN FMT*NUMBER.OF.AREAS) (SETQ NUMBER (+ NUMBER (FMT=NUMBER.OF.FORMULAS (1- (1+ RPTN)))))) (RETURN NUMBER)))
    (T (LIST-LENGTH (CAR (FMT=AREA AREA))))))

(DEFUN FMT-FORMULA (FORMULA.NUMBER AREA.NUMBER)
						;EDITED:28-FEB-83.
						;INPUT: 2 NATURALS, 1 <= 'FORMULA.NUMBER' <= NUMBER
						;       OF FORMULAS IN AREA 'AREA.NUMBER', 0 <=
						;       'AREA.NUMBER' <= MAXIMAL AREA NUMBER.
						;EFFECT: -
						;VALUE: THE 'FORMULA.NUMBER'TH FORMULA OF AREA
						;       'AREA.NUMBER'.
  (COND
    ((AND (INTEGERP FORMULA.NUMBER) (INTEGERP AREA.NUMBER) (< -1 AREA.NUMBER) (< 0 FORMULA.NUMBER)
	  (< AREA.NUMBER FMT*NUMBER.OF.AREAS) (< (1- FORMULA.NUMBER) (FMT=NUMBER.OF.FORMULAS AREA.NUMBER)))
     (FMT=FORMULA FORMULA.NUMBER AREA.NUMBER))
    (T NIL)))

(DEFUN FMT=FORMULA (FORMULA.NUMBER AREA.NUMBER)
						;EDITED:8-MAR-83.
						;INPUT: 2 NATURALS, 1 <= 'FORMULA.NUMBER' <= NUMBER
						;       OF FORMULAS IN AREA 'AREA.NUMBER', 0 <=
						;       'AREA.NUMBER' <= MAXIMAL AREA NUMBER.
						;EFFECT: -
						;VALUE: THE 'FORMULA.NUMBER'TH FORMULA OF AREA
						;       'AREA.NUMBER'.
  (NTH (1- FORMULA.NUMBER)
       (CAR (FMT=AREA AREA.NUMBER))))

(DEFUN FMT-LAST.DESTRUCTIVE.COMMAND NIL
						;EDITED:28-FEB-83.
						;INPUT:  -
						;EFFECT: -
						;VALUE: NAME OF THE AT LAST EXECUTED DESTRUCTIVE
						;       COMMAND, IF THERE IS SUCH A COMMAND.
						;       ELSE ERROR-MESSAGE.
  (CASE FMT*UNDO.STACK
    ((NIL) (FMT=ERRORS UNDO.STACK.EMPTY))
    (OTHERWISE (CAAR FMT*UNDO.STACK))))

(DEfmacro FMT=ERRORS (&optional errorcase)
  `(fmt=fun.errors ',errorcase))

(DEFUN FMT=fun.ERRORS (ERRORCASE)
						;EDITED:23-FEB-83.
						;INPUT: PARAMETER TO SELECT AN ERROR MESSAGE.
						;EFFECT: -
						;VALUE: THE SELECTED ERROR MESSAGE.
  (COND
    ((EQ FMT*LANGUAGE 'ENGLISH)
     (CASE ERRORCASE (INTEGER " INPUT MUST BE AN INTEGER. ") (AREA " POSITION DOESN'T EXIST IN AREA 0. ")
	   (INDEX " AREA DOESN'T EXIST. ") (NATURAL " INPUT MUST BE A NATURAL. ") (ATOM " INPUT MUST BE ATOM. ")
	   (EMPTY " AREA IS EMPTY. ") (UNDO.STACK.EMPTY " UNDO-STACK IS EMPTY. ")
	   (NIL.INTEGER " INPUT MUST BE NIL OR INTEGER. ") (FILE " FILE IS NOT OPEN. ")
	   (FORM " IT'S IMPOSSIBLE TO PRINT FORMULAS IN ANOTHER THAN PREFIX OR INFIX FORM. ")
	   (COMMAND " COMMAND ISN'T DEFINED. ") (OTHERWISE " UNDEFINED ERROR. ")))
    (T
     (CASE ERRORCASE (INTEGER " EINGABE MUSS GANZE ZAHL SEIN. ")
	   (AREA " POSITION IN BEREICH 0 NICHT DEFINIERT. ") (INDEX " BEREICH NICHT DEFINIERT. ")
	   (NATURAL " EINGABE MUSS NATUERLICHE ZAHL SEIN. ") (ATOM " EINGABE MUSS ATOM SEIN. ")
	   (EMPTY " BEREICH IST LEER. ") (UNDO.STACK.EMPTY " UNDOSTAPEL IST LEER. ")
	   (NIL.INTEGER " EINGABE MUSS NIL ODER EINE GANZE ZAHL SEIN. ") (FILE " DATEI MUSS GEOEFFNET SEIN. ")
	   (FORM " FORMELN KOENNEN NUR IN PRAEFIX- ODER INFIXFORM AUSGEGEBEN WERDEN. ")
	   (COMMAND " DIESES KOMMANDO IST NICHT DEFINIERT. ") (OTHERWISE " UNDEFINIERTER FEHLER. ")))))

(DEfmacro FMT=OUTPUTS (&optional outputcase)
  `(fmt=fun.outputs ',outputcase))

(DEFUN FMT=fun.OUTPUTS (OUTPUTCASE)
						;EDITED:10-MAR-83.
						;INPUT: ATOM 'ATOM'.
						;EFFECT: -
						;VALUE: THE BY ATOM 'ATOM' SELECTED OUTPUT.
  (COND
    ((EQ FMT*LANGUAGE 'ENGLISH)
     (CASE OUTPUTCASE (DATUM "DATE OF FILE GENERATION: ") (AREA "AREA ") (EMPTY "EMPTY") (FORMULA "1 FORMULA")
	   (FORMULAS " FORMULAS") (=== "   ===================")
	   (SPACE.FORMULAS "   F O R M U L A S   :                         ") (OTHERWISE " OUTPUT NOT DEFINED. ")))
    (T
     (CASE OUTPUTCASE (DATUM "DATEIERSTELLUNGSDATUM: ") (AREA "BEREICH ") (EMPTY "LEER") (FORMULA "1 FORMEL")
	   (FORMULAS " FORMELN") (=== "   =================")
	   (SPACE.FORMULAS "   F O R M E L N   :                               ")
	   (OTHERWISE " AUSGABE NICHT DEFINIERT. ")))))

(DEFUN FMT=AREA (AREA.NUMBER)
						;EDITED:11-MAR-83.
						;INPUT: NATURAL, 0 <= 'AREA.NUMBER' <= MAXIMAL AREA
						;       NUMBER.
						;EFFECT: -
						;VALUE: AREA 'AREA.NUMBER'.
  (AREF FMT*AREA.POINTERS AREA.NUMBER))
