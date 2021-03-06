;;; -*- Syntax: Common-lisp; Package: MKRP; Mode: LISP -*-

(in-package "MKRP")

(DEFUN SUPERSET (A B)
						; INPUT:  TWO SETS OF ATOMS
						; VALUE:  IF B IS A SUBSET OF A THEN T ELSE NIL
  (EVERY #'(LAMBDA (X) (MEMBER X A :TEST #'EQUAL)) B))

(DEFun SET= (SET.1 SET.2 &KEY (TEST #'EQL))
						; INPUT:  TWO SETS OF ATOMS (MULTISETS TOO).
						;         A FUNCTION OF TWO ARGUMENTS USED TO COMPARE 
						;         EXPRESSIONS.
						; EFFECT: -
						; VALUE:  IF BOTH ARE EQUAL AS SETS COMPARED WITH
						;         TESTPRED THEN T ELSE NIL.
  (COND ((AND (LISTP SET.1)
	      (LISTP SET.2)
	      (EQ (LENGTH SET.1) (LENGTH SET.2)))
	 (LET ((SET.HELP (COPY-LIST SET.2)))
	   (EVERY #'(LAMBDA (X)
		      (PROG1 (MEMBER X SET.HELP :TEST TEST)
			     (SETQ SET.HELP (DELETE X SET.HELP :TEST TEST :COUNT 1))))
		  SET.1)))))

(DEFun SET*= (SET.1 SET.2 &KEY (TEST #'EQL))
						; INPUT:  TWO SETS OF SETS OF ...      (MULTISETS TOO).
						;         A FUNCTION OF TWO ARGUMENTS USED TO COMPARE
						;         EXPRESSIONS.
						; EFFECT: -
						; VALUE:  IF BOTH LISTS ARE EQUAL AS SETS OF SETS OF ... 
						;         COMPARED WITH TESTPRED THEN T ELSE NIL.
  
  (COND
    ((AND (LISTP SET.1)
	  (LISTP SET.2)
	  (EQ (LENGTH SET.1) (LENGTH SET.2)))
     (LET
       ((SET.2.COPY (COPY-TREE SET.2)))
       (EVERY #'(LAMBDA (X)
		  (COND ((SYMBOLP X)
			 (PROG1 (MEMBER X SET.2.COPY :TEST TEST)
				(SETQ SET.2.COPY (DELETE X SET.2.COPY :TEST TEST :COUNT 1))))
			(T (SOME #'(LAMBDA (Z)
				     (COND ((SET*= X Z :TEST TEST)
					    (SETQ SET.2.COPY (DELETE Z SET.2.COPY :TEST TEST :COUNT 1))
					    T)))
				 SET.2.COPY))))
	      SET.1)))))







(DEFUN MEMBER* (OBJECT SET)
						; INPUT:  OBJECT IS AN ATOM OR A SET, SET IS A SET
						;         OF SETS OR ATOMS
						; VALUE:  IF OBJECT IS AN ELEMENT OF SET
						;         THEN T ELSE NIL
  (MEMBER-IF #'(LAMBDA (S) (COND ((ATOM OBJECT) (EQ OBJECT S)) (T (SET*= OBJECT S)))) SET))









(DEFUN ANTIVALENT (A B)
						; INPUT:  TWO S-EXPRESSIONS
						; VALUE:  IF BOTH ARE NIL OR BOTH ARE NON-NIL
						;         THEN NIL ELSE T
						; REMARK: THE ARGUMENTS ARE EVALUATED EXACTLY ONCE
						;         EACH AND IN THE GIVEN ORDER
  (COND (A (NOT B)) (T B)))


(DEFun COPY-GRAPH (STRUCTURE)			; EDITED: 14. 6. 1982   HJO
						; INPUT:  AN ARBITRARY LISP TREEESSION
						;         (NOT ONLY A TREE, BUT ALSO A GRAPH)
						; VALUE:  A LISP TREEESSION WHICH WHEN BEEING
						;         EVALUATED, GENERATES AN ISOMORPH COPY OF
						;         THE ORIGINAL STRUCTURE.
						; TYPICAL APPLICATION:
						;         (PRINT (LIST 'RPAQ 'X (COPYPROG X)) FILE)
						;         TO STORE X.
						;         (SETQ Y (EVAL (COPYPROG X))) TO GET AN
						;         ISOMORPH COPY OF X.
  (COND
    ((NOT (LISTP STRUCTURE)) (LIST 'QUOTE STRUCTURE))
    (T  (LET (CONS.CELLS CODE CELL (COUNTER 0))	
	  (DECLARE (SPECIAL CONS.CELLS COUNTER))
	  (COPY-GRAPH.DUPLICATE STRUCTURE)
	  (MAPC #'(LAMBDA (C.CELL)
		    (SETQ CELL (EVAL C.CELL))
		    (SETQ CODE (CONS
				 (LIST 'SETQ C.CELL
				       (LIST 'CONS
					     (LIST 'QUOTE (COND ((CONSP (CAR CELL))
								 (SOME #'(LAMBDA (CELL1)
									   (IF (EQ (CAR CELL) (EVAL CELL1)) CELL1))
								       CONS.CELLS))
								(T (CAR CELL))))
					     (LIST 'QUOTE (COND ((CONSP (CDR CELL))
								 (SOME #'(LAMBDA (CELL1)
									   (IF (EQ (CDR CELL) (EVAL CELL1)) CELL1))
								       CONS.CELLS))
								(T (CDR CELL))))))
				 CODE)))
		CONS.CELLS)
	  (MAPC #'(LAMBDA (CELL)
		    (SET CELL NIL))
		CONS.CELLS)
	  (SETQ CODE (CONS 'LET
			   (CONS (LIST (LIST 'CELLS (LIST 'QUOTE (NREVERSE CONS.CELLS)))) CODE)))
	  (NCONC CODE
		 '((MAPC #'(LAMBDA (CELL)
			     (SETQ CELL (EVAL CELL))
			     (COND ((MEMBER (CAR CELL) CELLS)
				    (RPLACA CELL (EVAL (CAR CELL)))))
			     (COND ((MEMBER (CDR CELL) CELLS)
				    (RPLACD CELL (EVAL (CDR CELL))))))
			 CELLS)
		   (PROG1 (EVAL (CAR CELLS))
			  (MAPC #'(LAMBDA (CELL)
				    (SET CELL NIL))
				CELLS))))))))

(DEFun COPY-GRAPH.DUPLICATE (&OPTIONAL STRUCTURE)
						; EDITED: 14. 6. 1982   HJO
						; INPUT:  STRUCTURE IS EXPECTED TO CONSIST OF AT LEAST
						;         ONE CONS CELL.
						; EFFECT: FOR EACH NEW CONS CELL IN STRUCTURE, A NEW
						;         ATOM COPYPROGN IS CREATED AND INSERTED INTO
						;         CONS.CELLS, DEFINED IN COPYPROG.
						;         THE VALUE CELL OF THIS ATOM POINTS TO THE
						;         CORRESPONDING CONS CELL IN STRUCTURE.
						; REMARK  THIS FUNCTION SHOULD ONLY BE USED IN
						;         COPYPROG.
						;         AN EXTERNAL VARIABLE COUNTER, DEFINED IN
						;         COPYPROG IS USED TO NUMBER THE NEW ATOMS
						; VALUE:  UNDEFINED. 
  (declare (SPECIAL CONS.CELLS COUNTER))
  (COND ((NOTANY #'(LAMBDA (CELL)
		     (EQ STRUCTURE (EVAL CELL)))
		 CONS.CELLS)
	 (LET ((NEW.ATOM (INTERN (CONCATENATE 'STRING "COPYPROG"
					      (PRINC-TO-STRING (SETQ COUNTER (1+ COUNTER))))
				 (find-package "MKRP"))))
	   (SETQ CONS.CELLS (CONS NEW.ATOM CONS.CELLS))
	   (SET NEW.ATOM STRUCTURE)
	   (COND ((LISTP (CAR STRUCTURE))
		  (COPY-GRAPH.DUPLICATE (CAR STRUCTURE))))
	   (COND ((LISTP (CDR STRUCTURE))
		  (COPY-GRAPH.DUPLICATE (CDR STRUCTURE))))))))




(DEFUN DUPL (EXPRESSION N)
						; INPUT:  AN S-EXPRESSION AND A NATURAL NUMBER
						; VALUE:  A LIST OF N COPIES OF THE S-EXPRESSION
  (let (RESULT)
    (DODOWN (RPTN N) (push (COPY-TREE EXPRESSION) RESULT))
    RESULT))


(DEFUN EQUIVALENT (A B)
						; INPUT:  TWO S-EXPRESSIONS
						; VALUE:  IF BOTH ARE NIL OR BOTH ARE NON-NIL
						;         THEN T ELSE NIL
						; REMARK: THE ARGUMENTS ARE EVALUATED EXACTLY ONCE
						;         EACH AND IN THE GIVEN ORDER
  (COND (A B) (T (NOT B))))

(DEFUN FLATTEN (X)
						; INPUT:  AN SEXPRESSION
						; VALUE:  A LIST , WHERE EACH TOPLEVEL ELEMENT IS "
						;         AN ATOM OCCURING IN X , MULTIPLE
						;         OCCURENCES IN X RESULT IN MULTIPLE
						;         MEMBERSHIP IN (FLATTEN X)
  (COND ((CONSP X) (MAPCAN #'FLATTEN X)) (T (LIST X))))

(DEFUN MAXIMA (LIST FUNCTION)
						; INPUT: A LIST AND A FUNCTION WITH TWO ARGUMENTS
						;      F (A B) = T  MEANS  A <= B.
						;     THE RELATION MUST BE TRANSITIVE.
						;     IF IT IS NOT REFLEXIV, EQ (UAL)S REMAIN.
						; VALUE: A LIST OF THE MAXIMAL ELEMENTS OF LIST.
						; SIDEEFFECTS: THE LIST IS CHANGED DESTRUCTIVELY
						;    THE MAXIMAL ELEMENTS HAVE NO DEFINED ORDERING
  (PROG (LIST2 (LIST1 LIST)) LOOP (COND ((NOT (CDR LIST1)) (RETURN LIST))) (SETQ LIST2 LIST1) TEST1
	(COND
	  ((FUNCALL FUNCTION (SECOND LIST2) (CAR LIST1))
	   (COND ((NULL (CDDR LIST2)) (RPLACD LIST2 NIL)) (T (RPLACA (CDR LIST2) (THIRD LIST2)) (RPLACD (CDR LIST2) (CDDDR LIST2))))
	   (COND ((CDR LIST2) (GO TEST1)) (T (GO LOOP.END)))))
	(COND
	  ((FUNCALL FUNCTION (CAR LIST1) (SECOND LIST2)) (RPLACA LIST1 (SECOND LIST2)) (RPLACD LIST2 (CDDR LIST2)) (GO LOOP)))
	(COND ((CDDR LIST2) (SETQ LIST2 (CDR LIST2)) (GO TEST1))) LOOP.END
	(COND ((CDDR LIST1) (SETQ LIST1 (CDR LIST1)) (GO LOOP)) (T (RETURN LIST)))))

(DEFUN IMPLIES (A B)
						; INPUT:  TWO S-EXPRESSIONS
						; VALUE:  IF A IS NIL OR B IS NON-NIL
						;         THEN T ELSE NIL
						; REMARK: B IS EVALUATED ONLY IF A IS NON-NIL
  (if A B))

(DEFUN IN (ELEMENT LIST)
						; INPUT:  TWO S-EXPRESSIONS
						; VALUE:  T IF ELEMENT IS A SUBSTRUCTURE OF LIST,
						;         ELSE NIL
						; REMARK: CHECKS USING EQ, I.E. WORKS LIKE MEMB ON
						;         ALL LEVELS
  (OR (EQl ELEMENT LIST)
      (AND (CONSP LIST)
	   (MEMBER-IF #'(LAMBDA (SUBLIST) (IN ELEMENT SUBLIST)) LIST))))



(DEFun MApPRINT (LIST &OPTIONAL LEFT RIGHT (SEPARATOR " ") (PRINT-FUNCTION #'PRINC)
		 (STREAM *STANDARD-OUTPUT*) (STEP-FUNCTION #'CDR))
  (when LIST
    (when LEFT (PRINC LEFT STREAM))
    (while (consP LIST)
      (FUNCALL PRINT-FUNCTION (CAR LIST) STREAM)
      (COND ((NULL (SETQ LIST (FUNCALL STEP-FUNCTION LIST))))
	    ((atom LIST)
	     (PRINC " . " STREAM)
	     (FUNCALL PRINT-FUNCTION LIST STREAM)
	     (setq list nil))
	    (SEPARATOR (PRINC SEPARATOR STREAM))))
    (when RIGHT (PRINC RIGHT STREAM))))

(DEFUN INS (ELEMENT LIST)
						; INPUT:  AN S-EXPRESSION AND A LIST
						; VALUE:  IF ELEMENT IS A TOP LEVEL ELEMENT OF LIST
						;         THEN LIST ELSE CONS(ELEMENT,LIST)
						; REMARK: CHECKS USING EQ
  (adjoin element list))

(DEFUN INSASSOC (KEY.VALUES ASSOCLIST)
						; EDITED: 29-JUN-83 11:13:55        MW
						; INPUT:  KEY.VALUES IS TO BE INSERTED IN ASSOCLIST
						;         AND HAS THE FORM (KEY VALUE1 ... VALUEN)
						;         FOR I=1 TO N. ASSOCLIST HAS ITS STANDARD
						;         ASSOCIATION LIST FORM.
						; EFFECT: INSERTION OF KEY.VALUES IN ASSOCLIST, AVOI- "
						;         DING MULTIPLE OCCURENCE OF BOTH KEY.VALUES
						;         OR VALUES (=: (CDR KEY.VALUES) ).
						; VALUE:  CHANGED ASSOCLIST.
  (let ((PARTNER (ASSOC (CAR KEY.VALUES) ASSOCLIST)))
    (cond (PARTNER (RPLACD PARTNER (UNION (CDR KEY.VALUES) (CDR PARTNER)))
		   ASSOCLIST)
	  (t       (NCONC1 ASSOCLIST KEY.VALUES)))))

(DEFUN INSERT (ELEMENT LIST)
						; INPUT:  AN S-EXPRESSION AND A LIST
						; VALUE:  IF ELEMENT IS A TOP LEVEL ELEMENT OF LIST
						;         THEN LIST ELSE CONS(ELEMENT,LIST)
						; REMARK: CHECKS USING EQUAL
  (if (MEMBER ELEMENT LIST :TEST #'EQUAL)
      LIST
      (CONS ELEMENT LIST)))

(DEFUN INSIDE (ELEMENT LIST)
						; INPUT:  TWO S-EXPRESSIONS
						; VALUE:  T IF ELEMENT IS A SUBSTRUCTURE OF LIST,
						;         ELSE NIL
						; REMARK: CHECKS USING EQUAL, I.E. WORKS LIKE MEMBER
						;         ON ALL LEVELS
  (OR (EQUAL ELEMENT LIST)
      (AND (CONSP LIST)
	   (MEMBER-IF #'(LAMBDA (SUBLIST) (INSIDE ELEMENT SUBLIST)) LIST))))

(DEFUN ISEMPTYSTACK (STACK)
						; INPUT:  AN ATOM WHOSE VALUE IS A LIST.
						; VALUE:  IF THIS LIST IS EMPTY THEN T ELSE NIL.
  (NULL STACK))

(DEFmacro LISTPOS (ELEMENT LIST &optional (testfunction '#'equal))
						; EDITED: 25-JUN-82 13:23:27
						; INPUT:  TWO ARBITRARY S-EXPRESSIONS
						; VALUE:  IF ELEMENT IS TOP-LEVEL ELEMENT IN LIST
						;         (EQUAL]) THEN THE POSITIONNUMBER OF
						;         ELEMENT IN LIST, ELSE NIL.
  `(let ((pos (position ,element ,list :test ,testfunction)))
     (if pos
	 (1+ pos)
	 nil)))


(DEFUN LISTPOSITIONS (SUBLIST LIST)
						; EDITED:  5-SEP-83 09:22:05
						; INPUT:   TWO ARBITRARY LISTS
						; VALUE:   A LIST WITH THE SAME LENGTH AS SUBLIST
						;          AND ELEMENTS NIL OR NUMBERS.
						;          THE NUMBERS ARE THE POSITIONS WHERE
						;          THE CORRESPONDING ELEMENT OF SUBLIST
						;          OCCURS IN LIST.
						; EXAMPLE: SUBLIST = (A B C)  LIST = (C D A E)
						;          VALUE =  (3 NIL 1)
  (MAPCAR #'(LAMBDA (ELEMENT1) (let ((pos (position element1 list))) (if pos (1+ pos) nil)))
	  SUBLIST))

(DEFMACRO MAKEEMPTYSTACK (STACK)
						; INPUT:  AN ATOM.
						; EFFECT: NIL BECOMES NEW VALUE OF THE ATOM.
  `(SETQ ,STACK NIL))


(DEFUN MAXELT (LIST VALUEFUNCTIONS &optional (MAPFN2 #'rest))
						;Edited:  10-JUN-1988 02:05
						;Authors: PRCKLN HJO
						;INPUT:   AN ARBITRARY LIST.
						;         VALUEFUNCTIONS IS A FUNCTION OR A LIST OF
						;         FUNCTIONS WHICH APPLIED TO AN ELEMENT
						;         OF LIST PRODUCE NUMERIC VALUES.
						;         MAPFN2   LIKE 3RD PARAMETER OF MAP, MAPC ..
						;VALUE:   THE first MAXIMUM ELEMENT IN LIST.
						;REMARK:  IF THERE is MORE THAN ONE MAXIMUM ELEMENT
						;         OF THE FIRST VALUEFUNCTION, THE SECOND
						;         VALUEFUNCTION IS APPLIED TO THESE ELEMENTS
						;         AND SO ON.
  (if (ATOM LIST)
      LIST
      (let ((MAXVAL -10000)
	    (MAXELTS NIL)
	    (VALUEFUNCTION (if (functionp valuefunctions)
			       valuefunctions
			       (CAR VALUEFUNCTIONS))))
	(SETQ VALUEFUNCTIONS (if (functionp valuefunctions)
				 nil
				 (CDR VALUEFUNCTIONS)))
	(SMAPC #'(LAMBDA (ELEMENT)
		   (let ((VALUE (FUNCALL VALUEFUNCTION ELEMENT)))
		     (COND ((< value maxval))
			   ((= VALUE MAXVAL)
			    (if valuefunctions
				(NCONC1 MAXELTS ELEMENT)))				
			   ((> value maxval)
			    (SETQ MAXVAL VALUE)
			    (SETQ MAXELTS (LIST ELEMENT))))))
	       MAPFN2
	       LIST)
	(if VALUEFUNCTIONS
	    (MAXELT MAXELTS VALUEFUNCTIONS)
	    (CAR MAXELTS)))))




(defvar *RIGHT.MARGIN* 117)

(defun tab (pos spaces &optional stream)
  (declare (ignore spaces))
  (format stream "~T" pos))

(defun linelength (new.value &optional file)
  (declare (ignore file))
  (if new.value
      (prog1 *right.margin* (setq *right.margin* new.value))
      *right.margin*))


(defvar mkrp*STANDARD.RIGHT.MARGIN 117)

(defparameter
  KKL*COUNT.PRINT.BUFFER 
  (make-array mkrp*STANDARD.RIGHT.MARGIN :element-type 
				     #-(or allegro :lcl4.1)
				     'string-char 
				     #+(or allegro :lcl4.1) 
				     'character :adjustable T :fill-pointer 0)
  " a buffer to hold output, in order to determine its print length")

;;; LCL4.1 is stringent about strings. We will lose efficiency here,
;;; but at least it works. DAN 21JUN94
(defun print-length (obj &optional printflag)
  #-:lcl4.1
  (progn 
    (with-output-to-string 
     (string kkl*count.print.buffer)
     (if printflag (prin1 obj string) (princ obj string)))
    (prog1 (length kkl*count.print.buffer)
      (setf (fill-pointer kkl*count.print.buffer) 0)))
  #+:lcl4.1
  (length (with-output-to-string (string)	 
	     (if printflag (prin1 obj string) (princ obj string))))
  )


(DEFun TAB-PRINC (OBJECT NUMBER &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (PRINC OBJECT STREAM)
  (SPACES (- NUMBER (PRINT-LENGTH OBJECT)) STREAM))

(DEFun TAB-PRIN1 (OBJECT NUMBER &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (PRIN1 OBJECT STREAM)
  (SPACES (- NUMBER (PRINT-LENGTH OBJECT t)) STREAM))

(defun SPACES (snumber &optional (output-stream *standard-output*))
						; edited "23.6.86"
						; input  snumber    = number of spaces to be inserted,
						;        output-stream = output-stream
						; effect puts a string of spaces with length 'snumber
						;        to the specified output-stream. the value of line.position
						;        is not actualized
						; values "NIL"
						; authors Birgit
						; usage  external
  (unless output-stream (setq output-stream *standard-output*))
  (format output-stream "~vA" snumber ""))

(DEFun PRINTTAB (COM &OPTIONAL (FILE *STANDARD-OUTPUT*) printfLAG)
						; EDITED: 22-aug-86 13:47:05  AP CL
						; INPUT:  COM : 'COM' IS A LIST OF THE FOLLOWING FORM"
						;               (POS0 CHAR0 LIST1 POS1 CHAR1 LIST2
						;                POS2 CHAR2 . . . LISTN POSN CHARN)
						;         FILE: LITERAL ATOM - NAME OF A FILE
						;
						; EFFECT: 1. THE ELEMENTS OF LISTI (I=1(1)N) WILL BE "
						;            PRINTED BETWEEN POSI-1 AND POSI
						;         2. CHARI WILL BE PRINTED ON POSI
						;         3. THE FOLLOWING EXAMPLE WILL SHOW HOW
						;            'COM' WILL BE PRINTED:
						;
						;            COM=(LIST 1 (QUOTE *) (QUOTE (A B CC D))"
						;                 6 (QUOTE ++) (QUOTE (EE))) 10 (QUOTE "
						;                 #))
						;
						;            WILL BE PRINTED IN THE FOLLOWING MANNER:"
						;
						;            POSITION:   1....6...10
						;
						;                        *A B ++ EE#
						;                        *CC D++   #
						;
  (let (POINTER POS CHAR LIST EMPTY tab-prinTFCT printfct LASTPOS new.pos)
    (multiple-value-setq (printfct tab-prinTFCT)
      (if printfLAG
	  (values #'prin1 #'TAB-prin1)
	  (values #'princ #'TAB-princ)))
    (WHILE (NOT EMPTY)
						; OUTPUT OF A LINE
      (SETQ POS (CAR COM))
      (SETQ CHAR (CADR COM))
      (SETQ POINTER (CDDR COM))
      (SPACES POS FILE)
      (PRINC CHAR FILE)
						; LEFT TERMINATION
      (SETQ EMPTY T)
      (WHILE POINTER
						; OUTPUT OF THE ELEMENTS OF LISTI TO POSI
	(SETQ LASTPOS (+ POS (PRINT-LENGTH CHAR nil)))
	(SETQ LIST (CAR POINTER))
	(SETQ new.POS (CADR POINTER))
	(SETQ CHAR (CADDR POINTER))
	(COND ((AND LIST (> (+ LASTPOS (PRINT-LENGTH (CAR LIST)  PRINTFLAG))
			    new.POS))
						; ELEMENT DOES NOT FIT IN COLUMN"
	       (FUNCALL tab-PRINTFCT (CAR LIST) (- new.pos pos) FILE)
	       (SETQ LASTPOS new.POS)
	       (SETQ LIST (CDR LIST)))
	      (T (WHILE (AND LIST
			     (<= (+ LASTPOS (PRINT-LENGTH (CAR LIST) PRINTFLAG))
				 new.POS))
						; PRINT AND REMOVE OF THE FIRST ELEMENT OF 'LIST'
		   (FUNCALL PRINTFCT (CAR LIST) FILE)
		   (SETQ LASTPOS (+ LASTPOS (PRINT-LENGTH (CAR LIST) printflag)))
		   (COND ((< LASTPOS new.POS)
			  (PRINC "
          "  FILE)))
		   (SETQ LASTPOS (1+ LASTPOS))
		   (SETQ LIST (CDR LIST)) )
		 (SPACES (- new.POS LASTPOS) FILE)))
	(setq pos new.pos)
	(PRINC CHAR FILE)
	(RPLACA POINTER LIST)
	(SETQ EMPTY (AND EMPTY (NULL LIST)))
	(SETQ POINTER (CDDDR POINTER)))
      (TERPRI FILE))))



(defun CASSOC (key alist)
  (cdr (assoc key alist)))

(DEFUN REMASSOC (KEY ALIST)
						; INPUT:  AN ATOM AND AN ASSOCIATIONLIST
						; EFFECT: THE KEY-ELEMENT OF ALIST IS DESTRUCTIVELY
						;         REMOVED.
						; VALUE:  THE NEW ALIST.
  (delete key alist :key #'first))



(DEFun REMPROPS (ATOM &optional PROPS)
						; INPUT:  AN ATOM AND A LIST OF PROPERTY INDICATORS
						; EFFECT: REMOVES FROM ATOM ALL PROPERTIES LISTED
						;         IN PROPS. IF PROPS=NIL, ALL USER DEFINED
						;         PROPERTIES ARE REMOVED.
						; VALUE:  NIL
  (if PROPS
      (MAPCar #'(LAMBDA (PROP) (REMPROP ATOM PROP)) PROPS)
      (SETF (SYMBOL-PLIST ATOM)
	    (SMAPCON #'(LAMBDA (PROP) (COND ((NEQ *PACKAGE* (SYMBOL-PACKAGE (CAR PROP)))
					     (LIST (CAR PROP) (SECOND PROP)))))
		     #'CDDR
		     (SYMBOL-PLIST ATOM))))
  nil)

(DEFUN REMVALUESASSOC (KEY VALUES ASSOCLIST)	; EDITED: 29-JUN-83 16:01:06        MW
						; INPUT:  A KEY, VALUES ASSOCIATED TO KEY AND AN
						;         ASSOCIATION LIST ASSOCLIST.
						; EFFECT: THE VALUES ASSOCIATED TO KEY ARE REMOVED
						;         FROM THE ASSOCLIST-ELEMENT (KEY.VALUES).
						;         IF ALL VALUES OF THE ELEMENT ARE REMOVED,
						;         THE WHOLE ELEMENT IS REMOVED.
  (delete-if #'(LAMBDA (KEY.VALUES)
		 (when (EQ KEY (CAR KEY.VALUES))
		   (RPLACD KEY.VALUES (NSET-DIFFERENCE (CDR KEY.VALUES) VALUES)))
		 (null (rest key.values)))
	     ASSOCLISt))

(DEFun DISJOINTP (LIST1 LIST2 &KEY (TEST #'EQL))
						; EDITED: 30-JUN-83 10:48:24
						; INPUT:  TWO LISTS AND A TESTFUNCTION
						; VALUE:  T IF THEY ARE DISJOINT, ELSE NIL
  (null (intersection list1 list2 :test test)))

(DEFMACRO DREMAP (&OPTIONAL BEREICH AFN TESTFN ABORTFN)
						; INPUT:  BEREICH - A LIST 
						;         AFN, TESTFN  ARE 'FUNCTION'-EXPRESSIONS.
						; EFFECT: DREMAP WORKS LIKE MAP, BUT THE SUCCESSOR -
						;         FUNCTION IS CDR, AND ALL FIRST ELEMENTS OF
						;        TAILS OF BEREICH ARE DESTRUCTIVELY REMOVED
						;         IF TESTFN(TAIL) <> NIL.
  (prog ((afnconsp (and (consp afn)
			(or (eq 'quote (car afn))
			    (eq 'function (car afn)))))
	 (abortconsp  (and (consp abortfn)
			   (or (eq 'quote (car abortfn))
			       (eq 'function (car abortfn)))))
	 (testconsp (and (consp testfn)
			 (or (eq 'quote (car testfn))
			     (eq 'function (car testfn))))))
	(return `(prog ((kkl*pointer ,bereich) kkl*pointer1 kkl*pointer2)
		    loop1
		       (cond ((consp kkl*pointer)
			      ,@(cond (afn (cond (afnconsp `((,(cadr afn) kkl*pointer)))
						 (t `((funcall ,afn kkl*pointer))))))
			      ,@(cond (abortfn `((cond (,(cond (abortconsp `(,(cadr abortfn) kkl*pointer))
							       (t `(funcall ,abortfn kkl*pointer)))
							(return kkl*pointer))))))
			      ,@(cond (testfn `((cond (,(cond (testconsp `(,(cadr testfn) kkl*pointer))
							      (t `(funcall ,testfn kkl*pointer)))
						       (setq kkl*pointer (cdr kkl*pointer))
						       (go loop1)))))
				      (t '((setq kkl*pointer (cdr kkl*pointer))
					   (go loop1))))))
		       ,@(cond (testfn
				`((setq kkl*pointer1 kkl*pointer)
				  loop2
				  (setq kkl*pointer2 (cdr kkl*pointer1))
				  (cond ((consp kkl*pointer2)
					 ,@(cond (afn (cond (afnconsp `((,(cadr afn) kkl*pointer2)))
							    (t `((funcall ,afn kkl*pointer2))))))
					 ,@(cond (abortfn `((cond (,(cond (abortconsp `(,(cadr abortfn) kkl*pointer2))
									  (t `(funcall ,abortfn kkl*pointer2)))
								   (return kkl*pointer))))))
					 ,@(cond (testfn `((cond (,(cond (testconsp `(,(cadr testfn) kkl*pointer2))
									 (t `(funcall ,testfn kkl*pointer2)))
								  (rplacd kkl*pointer1 (cdr kkl*pointer2)))
								 (t (setq kkl*pointer1 (cdr kkl*pointer1)))))))
					 (go loop2))))))
		       (return kkl*pointer)))))

(DEFUN SATISFIABLE (CLAUSES &optional TAUTFLAG)
						; EDITED: 13-OCT-83 17:35:38
						; INPUT:  A LIST OF PROPOSITIONAL CLAUSES
						;         THE PREDICATES ARE REPRESENTED BY INTEGERS
						;         POSITIVE INTEGERS REPRESENT POSITIVE
						;         LITERALS, NEGATIVE INTEGERS NEGATIVE
						;         LITERALS. EXAMPLE: ((1 2)(-1)).
						;         THE SECOND PARAMETER IS FOR INTERNAL USE
						;            AND SHOULD BE  NIL.
						; OUTPUT: T    IF THE CLAUSES ARE TAUTOLOGOUS
						;         A MODEL, IF THE CLAUSES ARE SATISFIABLE
						;         NIL  IF THE CLAUSES ARE NOT SATISFIABLE
						; SIDEEFFECTS: THE INPUTLIST WILL BE DESTROYED
  (COND ((NOT TAUTFLAG)
	 (MAPL #'(LAMBDA (TAIL) (RPLACA TAIL (REMOVE-DUPLICATES (CAR TAIL)))) CLAUSES)
	 (SETQ CLAUSES
	       (dremap CLAUSES nil
		       #'(LAMBDA (TAIL)
			   (SETQ TAIL (CAR TAIL))
			   (MEMBER-IF #'(LAMBDA (X) (MEMBER (- X) TAIL)) TAIL))))))
  (COND ((NULL CLAUSES) T)
	(T (PROG (HELPLIT PURELITERALS MODEL DUMMY)	;  DELETION OF PURE CLAUSES 
		 (WHILE (AND CLAUSES
			     (PROG2 (MAPC #'(LAMBDA (CLAUSE)
					      (MAPC #'(LAMBDA (LITERAL)
							(SETQ LITERAL (- LITERAL))
							(COND ((NOTANY #'(LAMBDA (CLAUSE) (MEMBER LITERAL CLAUSE)) CLAUSES)
							       (SETQ PURELITERALS (INSERT (- LITERAL) PURELITERALS)))))
						    CLAUSE))
					  CLAUSES)
				    PURELITERALS))
		   (SETQ CLAUSES      (delete-if #'(LAMBDA (CLAUSE) (NOT (DISJOINTP PURELITERALS CLAUSE))) CLAUSES)
			 MODEL        (NCONC PURELITERALS MODEL)
			 PURELITERALS NIL))
		 (COND ((NULL CLAUSES) (RETURN (OR MODEL T))))	;   THIS SUBROUTINE REDUCES THE LIST OF CLAUSES 
						;   BY THE ONE-LITERAL-RULE AND BY THE PURE-LITERAL-
						;   RULE UNTIL ALL CLAUSES CONSISTS OF MORE THAN 1 LIT
		 (SETQ HELPLIT (CAAR (MEMBER-IF #'(LAMBDA (CLAUSE) (EQL 1 (LENGTH CLAUSE)))
						CLAUSES)))
		 (WHILE (AND HELPLIT CLAUSES)
		   (SETQ MODEL   (CONS HELPLIT MODEL)
			 CLAUSES (delete-if #'(LAMBDA (X) (MEMBER HELPLIT X)) CLAUSES)
			 HELPLIT (- HELPLIT))
		   (MAPL #'(LAMBDA (X) (RPLACA X (DELETE HELPLIT (CAR X)))) CLAUSES)
		   (when (MEMBER NIL CLAUSES) (RETURN-FROM SATISFIABLE NIL))
		   (WHILE (AND CLAUSES		; REMOVAL OF PURE CLAUSES
			       (PROG2 (MAPC #'(LAMBDA (CLAUSE)
						(MAPC #'(LAMBDA (LITERAL)
							  (SETQ LITERAL (- LITERAL))
							  (COND ((NOTANY #'(LAMBDA (CLAUSE) (MEMBER LITERAL CLAUSE)) CLAUSES)
								 (SETQ PURELITERALS (INSERT (- LITERAL) PURELITERALS)))))
						      CLAUSE))
					    CLAUSES)
				      PURELITERALS))
		     (SETQ CLAUSES (delete-if #'(LAMBDA (CLAUSE) (NOT (DISJOINTP PURELITERALS CLAUSE))) CLAUSES))
		     (SETQ MODEL (NCONC PURELITERALS MODEL))
		     (SETQ PURELITERALS NIL))
		   (SETQ HELPLIT (CAAR (MEMBER-IF #'(LAMBDA (CLAUSE) (EQL 1 (LENGTH CLAUSE))) CLAUSES))))
		 (COND ((NULL CLAUSES) (RETURN (OR MODEL T)))
		       ((MEMBER NIL CLAUSES) (RETURN NIL)))
		 (SETQ HELPLIT (CAAR CLAUSES))
		 (SETQ DUMMY
		       (OR (SATISFIABLE (CONS (LIST HELPLIT) (COPY-TREE (CDR CLAUSES))) T)
			   (SATISFIABLE (CONS (LIST (- HELPLIT)) CLAUSES) T)))
		 (RETURN (COND (DUMMY (COND (MODEL (NCONC DUMMY MODEL))
					    (T DUMMY)))))))))


(DEFUN ZIP (LIST1 LIST2 &optional COPYFLG)
						; INPUT:  TWO LISTS OF EQUAL LENGTH AND A BOOLEAN
						;         VALUE.
						; VALUE:  A LIST WITH THE ELEMENTS OF LIST1 AT ODD AND
						;         THE ELEMENTS OF LIST2 AT EVEN POSITIONS.
						; REMARK: DESTRUCTIVE ON BOTH LIST1, LIST2 IFF
						;         COPYFLG = NIL
  (COND (COPYFLG (MAPCAN #'list LIST1 list2))
	(T (SMAPL #'(LAMBDA (TAIL1)
		      (let ((CDRTAIL1 (CDR TAIL1)))
			(RPLACD TAIL1 LIST2)
			(SETQ LIST2 (CDR LIST2))
			(RPLACD (CDR TAIL1) CDRTAIL1)))
		  #'CDDR LIST1)
	   LIST1)))

(DEFUN BUFFER.CREATE (N)
						; EDITED: 2. 8. 1984
						; INPUT:  AN INTEGER >= 0
						; EFFECT: AN EMPTY BUFFER IS GENERATED.
						;                                       .-- BUFFER
						;                                       V
						;                                    .---.
						;                              .-----! ! !
						;                              !     '---'
						;                              !        !
						;                              V        V
						;         .---.    .---.    .---.    .---.
						;         ! ! ! <- ! ! ! <- ! ! ! <- ! ! !
						;         '---'    '---'    '---'    '---'
						;            !        !        !        !
						;            V        V        V        V
						;         .---.    .---.    .---.    .---.
						;         ! ! ! -> ! ! ! -> ! ! ! -> ! ! !
						;         '---'    '---'    '---'    '---'
						;
						;         THE BUFFER IS INITIALIZED WITH N PAIRS OF
						;         CONS CELLS AND MAY BE DYNAMICALLY INCREASED "
						;         IN BUFFER.CONS ETC.
						; VALUE:  THE BUFFER POINTER.
  (PROG ((BUFFER (LIST (LIST NIL)))) (SETQ BUFFER (CONS BUFFER BUFFER))
	(DODOWN (RPTN (1- N)) (BUFFER.CONS NIL BUFFER)) (BUFFER.RESET BUFFER)
	(RETURN BUFFER)))

(defun buffer.multiple.cons (object.list buffer)
						; Edited:  11-FEB-1991 22:25
						; Authors: PRCKLN
						; Input:   A list of arbitrary objects and a buffer
						; Effect:  Conses all objects into buffer
						; Value:   Undefined
  (MAPC #'(LAMBDA (object) (BUFFER.CONS object buffer)) object.list))

(DEFUN BUFFER.CONS (OBJECT BUFFER)
						; EDITED: 2. 8. 1984
						; INPUT:  AN ARBITRARY OBJECT AND A BUFFER
						; EFFECT: THE OBJECT IS INSERTED INTO THE BUFFER
						;         AND THE BUFFER IS INCREASED IF NECESSARY.
						; VALUE:  UNDEFINED.
  (let ((rest.of.buffer (rest buffer)))
    (RPLACA (first rest.of.buffer) OBJECT)
    (if (rest rest.of.buffer)
	(RPLACD BUFFER (rest rest.of.buffer))
	(BUFFER=EXTEND BUFFER))))

(DEFUN BUFFER.INS (OBJECT BUFFER)
						; EDITED: 2. 8. 1984
						; INPUT:  AN ARBITRARY OBJECT AND A BUFFER
						; EFFECT: THE OBJECT IS INSERTED INTO THE BUFFER
						;         IF IT IS NOT ALREADY CONTAINED IN IT
						;         (COMPARED WITH EQ.)
						; VALUE:  UNDEFINED.
  (if (MEMBER OBJECT (BUFFER.CONTENTS BUFFER))
      BUFFER
      (BUFFER.CONS OBJECT BUFFER)))

(DEFUN BUFFER.INSERT (OBJECT BUFFER)
						; EDITED: 2. 8. 1984
						; INPUT:  AN ARBITRARY OBJECT AND A BUFFER
						; EFFECT: THE OBJECT IS INSERTED INTO THE BUFFER
						;         IF IT IS NOT ALREADY CONTAINED IN IT
						;         (COMPARED WITH EQUAL.)
						; VALUE:  UNDEFINED.
  (COND ((MEMBER OBJECT (BUFFER.CONTENTS BUFFER) :TEST #'EQUAL) BUFFER)
	(T (BUFFER.CONS OBJECT BUFFER))))

(DEFUN BUFFER.CONTENTS (BUFFER)
						; EDITED: 7. 8. 1094
						; INPUT:  A BUFFER
						; VALUE:  THE CONTENTS OF THE BUFFER
						;         (NO COPY])
  (CDADR BUFFER))

(DEFUN BUFFER.RESET (BUFFER)
						; EDITED: 14. 8. 1984
						; INPUT:  A BUFFER
						; EFFECT: THE BUFFER IS RESET TO THE EMPTY STATE
						;         (THE CONTENTS IS NOT CLEARED])
						; VALUE:  UNDEFINED.
  (RPLACD BUFFER (CAR BUFFER)))

(DEFUN BUFFER.CLEAR (BUFFER)
						; EDITED: 2. 8. 1984
						; INPUT:  A BUFFER
						; EFFECT: THE CONTENTS OF BUFFER IS REMOVED,
						;         THE BUFFER IS RESET TO THE EMPTY STATE.
						; VALUE:  UNDEFINED.
  (MAPC #'(LAMBDA (POINTER) (RPLACA POINTER NIL)) (CAR BUFFER)) (BUFFER.RESET BUFFER))

(DEFUN BUFFER.MAPCAR (BUFFER LIST AFN SFN)
						; EDITED: 7. 8. 1984
						; INPUT:  A BUFFER (CREATED WITH BUFFER.CREATE)
						;         A LIST, AND APPLYFUNCTION AND A STEPFUNCTION"
						; EFFECT: THIS FUNCTION WORKS LIKE MAPCAR, BUT IT
						;         USES THE CONS CELLS OF BUFFER TO GATHER
						;         THE RESULTS OF THE APPLYFUNCTION.
						;         IF THE BUFFER IS NOT EMPTY, THE OLD VALUES
						;         REMAIN IN THE BUFFER.
						; VALUE:  LIKE MAPCAR, BUT IN REVERSED ORDER]
						; WARNING: DO NOT WORK DESTRUCTIVELY ON THIS LIST.
						;          YOU WILL DAMAGE THE BUFFER]
  (COND (SFN) (T (SETQ SFN #'CDR)))
  (PROG NIL START
	(COND (LIST (BUFFER.CONS (FUNCALL AFN (CAR LIST)) BUFFER) (SETQ LIST (FUNCALL SFN LIST)) (GO START))
	      (T (RETURN (BUFFER.CONTENTS BUFFER))))))

(DEFUN BUFFER.SUBSET (BUFFER LIST AFN SFN)
						; EDITED: 7. 8. 1984
						; INPUT:  A BUFFER (CREATED WITH BUFFER.CREATE)
						; EFFECT: THIS FUNCTION WORKS LIKE SUBSET EXCEPT
						;         THAT THE RESULTS ARE GATHERED IN BUFFER
						; VALUE:  LIKE SUBSET, BUT IN REVERSED ORDER
						; WARNING: DO NOT WORK DESTRUCTIVELY ON THIS VALUE]
						;          YOU WOULD DAMAGE THE BUFFER]
  (BUFFER.RESET BUFFER) (COND (SFN) (T (SETQ SFN #'CDR)))
  (PROG (RESULT) START
	(COND
	  (LIST (COND ((SETQ RESULT (FUNCALL AFN (CAR LIST)))
		       (BUFFER.CONS RESULT BUFFER)))
		(SETQ LIST (FUNCALL SFN LIST))
		(GO START))
	  (T (RETURN (BUFFER.CONTENTS BUFFER))))))

(DEFUN BUFFER=EXTEND (BUFFER)
						; INPUT:  A BUFFER
						; EFFECT: THE BUFFER IS INCREASED BY ONE ELEMENT.
						; VALUE:  UNDFINED.
  (RPLACD (CDR BUFFER) (LIST (CONS NIL (SECOND BUFFER))))
  (RPLACD BUFFER (CDDR BUFFER)))

(DEFUN PUSH.BUFFER.STACK (BUFFER BUFFER.STACK)
						; INPUT:  A BUFFER AND A BUFFER STACK, WHICH IS AN
						;         EMPTY BUFFER OR A BUFFER OF BUFFERS.
						; VALUE:  UNDEFINED
						; EFFECT: IF THERE ARE SOME 'NIL' IN  BUFFER CONTENTS "
						;         OF THE BUFFER STACK, THE LAST ONE WILL BE
						;         REPLACED BY BUFFER, ELSE BUFFER WILL BE PUT "
						;         INTO THE BUFFER STACK BY 'BUFFER.CONS' .
						;         BUFFER WILL BE RESET IN THE EMPTY STATE
						;         BEFORE IT IS PUSHED ON THE BUFFER STACK.
  (PROG ((CONTENTS (BUFFER.CONTENTS BUFFER.STACK))) (BUFFER.RESET BUFFER)
	(COND ((OR (NOT CONTENTS) (CAR CONTENTS)) (BUFFER.CONS BUFFER BUFFER.STACK))
	      (T
	       (SMAPL #'ignore
		      #'(LAMBDA (TAIL)
			  (COND ((AND (CDR TAIL) (OR (CAR TAIL) (NOT (SECOND TAIL)))) (CDR TAIL)) (T (RPLACA TAIL BUFFER) NIL)))
		      CONTENTS)))))

(DEFUN POP.BUFFER.STACK (BUFFER.STACK)
						; INPUT: A BUFFER STACK, THIS IS AN EMPTY BUFFER OR A "
						;        BUFFER OF BUFFERS.
						; VALUE:  A BUFFER
						; EFFECT: IF THE BUFFER STACK IS AN EMPTY BUFFER
						;         OR CONTAINS SOME 'NIL' ONLY, THEN A NEW
						;         BUFFER WILL BE CREATED TO BE THE VALUE AND
						;         A 'NIL' WILL BE ADDED TO THE BUFFER STACK
						;         BY 'BUFFER.CONS', ELSE THE FIRST ELEMENT OF "
						;         BUFFER CONTENTS OF THE BUFFER STACK, WHICH
						;         IS NOT 'NIL', WILL BE THE VALUE AND WILL BE "
						;         REPLACED BY 'NIL' IN THE BUFFER STACK.
  (let (BUFFER)
    (SMAPL #'(LAMBDA (TAIL) (COND ((SETQ BUFFER (CAR TAIL)) (RPLACA TAIL NIL))))
	   #'(LAMBDA (TAIL) (COND ((NOT BUFFER) (CDR TAIL)))) (BUFFER.CONTENTS BUFFER.STACK))
    (COND ((NOT BUFFER) (SETQ BUFFER (BUFFER.CREATE 0)) (BUFFER.CONS NIL BUFFER.STACK)))
    BUFFER))
	




(DEFUN CARTESIAN.LOOP (CART.LIST CART.FUNCTION)
						; INPUT: A LIST OF LISTS L1,L2,...,LN  AND A FUNCTION "
						; VALUE: NIL
						; SIDEEFFECTS: FUNCTION WILL BE APPLIED TO EVERY
						;              LIST OF THE FORM (L1' L2',...,LN')
						;              WITH LI' IS A SUBLIST OF LI.
						;        PROCESSING CAN BE TERMINATED BY SETTING
						;       CARTESIAN.LOOP.END = T
						;       FIRST L1 IS VARYING, THEN L2, AND SO ON
  (catch 'cartesian.loop
    (when (AND (CONSP CART.LIST) (NOT (MEMBER NIL CART.LIST)))
      (let ((POINTERS (COPY-LIST CART.LIST)) (CART.LIST.POINTER CART.LIST) POINTERS.POINTER CONTINUE CARTESIAN.LOOP.END
	    CARTESIAN.LOOP.RETURN)
	(declare (special CARTESIAN.LOOP.END))
	(WHILE POINTERS
	  (SETQ CART.LIST.POINTER CART.LIST) (SETQ POINTERS.POINTER POINTERS) (FUNCALL CART.FUNCTION POINTERS)
	  (SETQ CONTINUE T)
	  (WHILE (AND CONTINUE POINTERS.POINTER)
	    (COND ((NULL (CDAR POINTERS.POINTER)) (RPLACA POINTERS.POINTER (CAR CART.LIST.POINTER)))
		  (T (RPLACA POINTERS.POINTER (CDAR POINTERS.POINTER)) (SETQ CONTINUE NIL)))
	    (SETQ POINTERS.POINTER (CDR POINTERS.POINTER)) (SETQ CART.LIST.POINTER (CDR CART.LIST.POINTER)))
	  (COND ((OR CONTINUE CARTESIAN.LOOP.END) (SETQ POINTERS NIL))))
	CARTESIAN.LOOP.RETURN))))

(defun save-proplist (symbol property.begins)
  (let ((setf.list nil))
    (mapc #'(lambda (prop.beg)
	      (smapc #'(lambda (rest.props)
			 (when (and (search (symbol-name prop.beg) (symbol-name  rest.props))
				    (not (compiled-function-p (get symbol rest.props))))
			   (push `(setf (get ',symbol ',rest.props)
					',(get symbol rest.props))
				 setf.list)))
		     #'cddr
		     (symbol-plist symbol)))
	  property.begins)
    `(progn ,@setf.list)))



(defun PP (name)          ;pretty-print of a function definition
  (pprint (symbol-function name)))


(defun remaining-memory nil
						; Edited:  07-FEB-1989 21:23
						; Authors: PRCKLN
						; Input:   -
						; Effect:  -
						; Value:   Gives the remaining address space up to gc immediately ~ in Kbytes.
  #+symbolics
  (multiple-value-bind (comm.free free imm.comm.free) (si:gc-get-committed-free-space)
    (declare (ignore comm.free))
    (truncate (- free imm.comm.free) 1024))
  #+lucid
  (multiple-value-bind (comm.free free imm.comm.free) #+lcl4.0(system::gc-size)#-lcl4.0(system:gc-size)
    (declare (ignore comm.free imm.comm.free))
    (truncate free 1024))
  #-(or symbolics lucid)
  0)

(defun mkrp-gc.start (flag)
  #+symbolics
  (when (and flag  (< -200 (remaining-memory) 100))
    (global:gc-off)
    (GLOBAL:GC-IMMEDIATELY t)
    (si:gc-on :ephemeral t :dynamic nil))
  #-symbolics
  (declare (ignore flag)))
