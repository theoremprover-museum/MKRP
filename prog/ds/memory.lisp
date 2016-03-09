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


(DEFVAR MEM*MEMORY nil)

(DEFVAR MEM*LAST.REUSABLE.VADR NIL)

(DEFVAR MEM*FIRST.REUSABLE.VADR NIL)

(DEFVAR MEM*COLLECTABLE NIL)

(DEFVAR MEM*NEXT.VADR NIL)

(DEFVAR MEM*NEXT.RADR NIL)

(DEFVAR MEM*RESERVE NIL)

(DEFVAR MEM*SIZE NIL)

(DEFVAR MEM*REST NIL)

(DEFVAR MEM*INCREMENT 4116)

#+mkrp-defstruct
(defun mem=print (object stream depth)
  ???
  (setf (symbol-function 'mem-print)
	#'(lambda (object stream depth) (format stream "#~D#" (ds=clause.number object)))))

(DEFmacro MEM-NEW (TYPE SIZE)
						; INPUT:  TYPE IS AN ATOM, SIZE AN INTEGER > 0
						; VALUE:  THE NEW ADDRESS.
						;         THREE CELLS MORE THAN SIZE ARE NEEDED
						;         TO STORE SIZE, TYPE AND A PROPERTY LIST.
						; EFFECT: A NEW STORAGE UNIT IS ALLOCATED.
  (if (or (numberp size)
	  (and (consp size)
	       (macro-function (first size))
	       (numberp (macroexpand size))
	       (setq size (macroexpand size))))
      `(progn (if (MEM=NOT.ENOUGH.MEMORY ,(+ 4 size))
		  (MEM=GBC ,(+ 4 SIZE)))
	      (MEM=ALLOC ,TYPE ,SIZE))
      `(let ((local.size ,size))
	 (if (MEM=NOT.ENOUGH.MEMORY (+ 4 local.SIZE))
	     (MEM=GBC (+ 4 local.SIZE)))
	 (MEM=ALLOC ,TYPE local.SIZE))))

(defmacro MEM-GET (ADR N)
						; INPUT:  A STORAGE ADDRESS AND AN INTEGER > 0 AND
						;         < SIZE OF THE STORAGE ALLOCATION
						; VALUE:  CONTENTS OF THE N.TH COMPONENT OF THE
						;         ALLOCATION
  `(AREF MEM*MEMORY (- (MEM=REAL.ADDR ,ADR) ,N)))

(defmacro MEM-PUT (ADR N VAL)
						; INPUT:  A STORAGE ADDRESS,AN INTEGER > 0 AND < SIZE
						;         AND AN S-EXPRESSION.
						; EFFECT: VAL IS STORED INTO THE N.TH COMPONENT OF
						;         THE ALLOCATION.
						; VALUE:  UNDEFINED.
  `(setf (aref MEM*MEMORY (- (MEM=REAL.ADDR ,ADR) ,N)) ,VAL))

(DEFUN MEM-SHORTEN (ADR N)
						; INPUT: A STORAGE ADDRESS AND AN INTEGER < SIZE
						; VALUE: UNDEFINED.
						; EFFECT: THE UPPER N CELLS ARE MADE FREE FOR GBC
  (let ((RADR    (MEM=REAL.ADDR ADR))
	(OLDSIZE (MEM=GETSIZE ADR))
	NEWSIZE TYPEP PROPP)
    (SETQ NEWSIZE (- OLDSIZE N)
	  TYPEP   (AREF MEM*MEMORY (- RADR (1- OLDSIZE)))
	  PROPP   (AREF MEM*MEMORY (- RADR (- OLDSIZE 2))))
    (MEM=CLEAN.UP (- (1+ RADR) OLDSIZE) (- RADR NEWSIZE))
    (setf (aref MEM*MEMORY RADR)                   NEWSIZE
	  MEM*COLLECTABLE                          (+ MEM*COLLECTABLE N)
	  (aref MEM*MEMORY (- RADR (1- NEWSIZE)))  TYPEP
	  (aref MEM*MEMORY (- RADR (- NEWSIZE 2))) PROPP)))

(DEFmacro MEM-SIZE (ADR)
						; INPUT:  A STORAGE ADDRESS
						; VALUE:  NUMBER OF COMPONENTS OF THE OBJECT WITH
						;         THIS ADDRESS.
  `(- (MEM=GETSIZE ,ADR) 3))

(DEFmacro MEM=CLEAN.UP (FROM TO)
						; INPUT:  TWO ADDRESSES;  THE FIRST ADDRESS SHALL BE SMALLER
						;         THAN THE SECOND ADDRESS.
						; VALUE:  UNDEFINED
  `(let ((IND (1- ,FROM)))
     (DODOWN (RPTN (- ,TO IND))
       (setf (aref MEM*MEMORY (SETQ IND (1+ IND))) 'ATP.MEMORY.NOBIND))))

(DEFUN MEM-ERASE (ADR COLLECTFLAG)
						; INPUT:  AN ADDRESS AND A BOOLEAN VALUE
						; EFFECT: THE PART OF THE REAL ADDRESS
						;         TO WHICH CONTENTS OF ADR POINTS
						;         IS FREED.
						;         IF COLLECTFLAG IS TRUE, ALSO ADR WILL BE
						;         REUSED AND MEM*LAST.REUSABLE.VADR GET
						;         THE ADDRESS ADR. ONLY AT THE FIRST TIME
						;         MEM*FIRST.REUSABLE.VADR GET THE SAME ADR
						;         AS MEM*LAST.REUSABLE.VADR.
  (let* ((RADR (MEM=REAL.ADDR ADR))
	 (SIZE (aref mem*memory radr))) ;(if (eql 50 adr) (break "Delete 50"))
    (SETQ MEM*COLLECTABLE (+ MEM*COLLECTABLE SIZE))
    (MEM=CLEAN.UP (- (1+ RADR) SIZE) RADR)
    (cond (COLLECTFLAG
	   (setf (aref MEM*MEMORY ADR) 'end)
	   (if MEM*FIRST.REUSABLE.VADR
	       (setf (aref MEM*MEMORY MEM*LAST.REUSABLE.VADR) (- ADR))
	       (SETf MEM*FIRST.REUSABLE.VADR ADR))
	   (SETf MEM*LAST.REUSABLE.VADR  ADR))
	  (t (setf (aref MEM*MEMORY ADR) 'ATP.MEMORY.NIL)))))

(DEFUN MEM-TYPE (ADR)
						; INPUT:  ADDRESS OF OBJECT WITH ADDRESS ADR
						; VALUE:  THE TYPE IDENTIFIER OF THE OBJECT
						;         IF IT DOES EXIST, OTHERWISE NIL.
  (if (MEM-ADDRESS ADR)
      (let ((REAL.ADDR (MEM=REAL.ADDR ADR)))
	(if (or (EQL 'ATP.MEMORY.NIL REAL.ADDR)
		(not (numberp (MEM=GETSIZE ADR)))
		(<= (AREF MEM*MEMORY ADR) 0))
	    NIL
	    (AREF MEM*MEMORY (- REAL.ADDR (MEM=GETSIZE ADR) -1))))))

(DEFMACRO MEM-GET.TYPE (ADR)
  `(let ((RADR (MEM=REAL.ADDR ,ADR)))
     (AREF MEM*MEMORY (- RADR (AREF MEM*MEMORY RADR) -1))))

(DEFUN MEM-ADDRESS (OBJECT)
						; INPUT:  AN ARBITRARY S-EXPRESSION
						; VALUE:  T IF OBJECT IS AN ADMISSIBLE STORAGE
						;         ADDRESS, ELSE NIL.
  (AND (INTEGERP OBJECT)
       (>= OBJECT 0)
       (NUMBERP (AREF MEM*MEMORY OBJECT))
       (>= (AREF MEM*MEMORY OBJECT) 0)
       (< OBJECT MEM*NEXT.VADR)))

(DEFmacro MEM-ALL.ADR NIL
						; INPUT:  NONE
						; VALUE:  THE NUMBER OF ALL VIRTUELL ADDRESSES USED
						;
  `(1- MEM*NEXT.VADR))

(DEFUN MEM-INITIALIZE (SIZE)
						; INPUT:  AN INTEGER
						; EFFECT: INITIALIZATION OF THE MEMORY AND THE
						;         COMMEN-VARIABLES. MEM*SIZE IS COMPUTATED AND THE VARIABLES ARE RESET.
						; VALUE:  MEM*SIZE
  (tagbody 
    MAR
    (COND
      ((NOT (AND (INTEGERP SIZE) (> SIZE 0)))
       (PRINc "~&----- Memory: Please enter size (small natural number): ")
       (SETQ SIZE (read T))
       (GO MAR))
      (T (format t "~&----- Memory: Initialization with size ~A.~&" SIZE)))
    (SETQ MEM*SIZE      SIZE
	  MEM*MEMORY    (MAKE-ARRAY MEM*SIZE)
	  MEM*INCREMENT MEM*SIZE)
    (MEM-RESET)))



(DEFUN MEM-RESET NIL
						; INPUT:  NONE
						; EFFECT: THE COMMEN-VAR. BUT MEM*SIZE ARE RESET.
						; VALUE:  UNDEFINED
  (SETQ MEM*NEXT.VADR 0
	MEM*NEXT.RADR (1- MEM*SIZE))
  (MEM=CLEAN.UP MEM*NEXT.VADR MEM*NEXT.RADR)
  (SETQ MEM*COLLECTABLE 0
	MEM*RESERVE (truncate MEM*SIZE 10)
	MEM*REST MEM*SIZE
	MEM*FIRST.REUSABLE.VADR NIL
	MEM*LAST.REUSABLE.VADR NIL))

(DEFUN MEM-save (file)
						; INPUT:  A FILENAME OR NIL                        
						; VALUE:  IF FILE = NIL, AN S-EXPRESSION WHICH     
						;         IF IT IS EVALUATED RESTORES THE VERY     
						;         SAME MEM-STATE AS IT WAS WHEN MEM-SAVE   
						;         WAS CALLED                              
						;         ELSE NIL                                 
						; EFFECT: IF FILE =//= NIL THE S-EXPRESSION IS      
						;         WRITTEN ON FILE                          
						;         ELSE RETURNS VALUE                       
						; REMARK: THE FILE IS EXPECTED TO BE OPEN AND      
						;         REMAINS SO                              
  (PROG (OLD.ADDRLIST OLD.DATALIST COUNTER CODE)
	(COND ((NEQ MEM*COLLECTABLE 0) (MEM=GBC 0)))
	(DOtimes (RPTN MEM*NEXT.VADR) (SETQ OLD.ADDRLIST (CONS (AREF MEM*MEMORY rptn) OLD.ADDRLIST)))
	(SETQ COUNTER MEM*NEXT.RADR)
	(DODOWN (RPTN (- MEM*SIZE 1 MEM*NEXT.RADR))
	  (SETQ OLD.DATALIST (CONS (AREF MEM*MEMORY (SETQ COUNTER (1+ COUNTER))) OLD.DATALIST)))
	(SETQ CODE
	      `(PROG ((ADDRLIST ',OLD.ADDRLIST)
		      (DATALIST ',OLD.DATALIST)
		      (INCREMENT (- MEM*SIZE ,mem*SIZE)) COUNTER1)
		     (MEM-RESET)
		     (COND
		       ((> MEM*SIZE ,mem*SIZE)
			(SETQ COUNTER1 ,mem*NEXT.VADR)
			(MAPC #'(LAMBDA (RADDR)
				  (setf (aref MEM*MEMORY (decf COUNTER1))
					(COND ((EQL RADDR 'ATP.MEMORY.NIL) RADDR)
					      ((eql 'end raddr)  RADDR)
					      ((OR (EQL RADDR 0) (MINUSP RADDR)) RADDR)
					      (T (+ RADDR INCREMENT)))))
			      ADDRLIST)
			(SETQ COUNTER1 MEM*SIZE)
			(MAPC #'(LAMBDA (DATA)
				  (setf (aref MEM*MEMORY (decf COUNTER1)) DATA))
			      DATALIST))
		       (T (unless (= MEM*SIZE ,mem*SIZE) (MEM-INITIALIZE ,mem*SIZE))
			  (SETQ COUNTER1 ,mem*NEXT.VADR)
			  (MAPC #'(LAMBDA (RADDR) (setf (aref MEM*MEMORY (decf COUNTER1)) RADDR)) ADDRLIST)
			  (SETQ COUNTER1 MEM*SIZE)
			  (MAPC #'(LAMBDA (DATA) (setf (aref MEM*MEMORY (decf COUNTER1)) DATA))
				DATALIST)))
		     (SETQ MEM*NEXT.RADR (if (> MEM*SIZE ,MEM*SIZE)
					     (+ ,MEM*NEXT.RADR INCREMENT)
					     ,MEM*NEXT.RADR))
		     (SETQ MEM*NEXT.VADR ,MEM*NEXT.VADR
			   MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
			   MEM*FIRST.REUSABLE.VADR ,MEM*FIRST.REUSABLE.VADR
			   MEM*LAST.REUSABLE.VADR ,MEM*LAST.REUSABLE.VADR)))
	(COND ((NOT FILE) (RETURN code))
	      (FILE       (PROGN #+(or symbolics explorer) (pPRINt code FILE)
				 #-(or symbolics explorer) (prin1 code file)
				 (TERPRI FILE))))))

(DEFUN MEM-SAVE.SYMBOL (ADDRESS)
						; INPUT: A MEMORY-ADDRESS
						; VALUE: A S-EXPRESSION,WHICH WHEN EVALUATED,
						;        RESTORES THE MEMORY-CELLS OF ADDRESS.
  (let (SAVE)
    (DODOWN (RPTN (- (MEM=GETSIZE ADDRESS) 2))
      (SETQ SAVE (NCONC1 SAVE (COPY-TREE (MEM-GET ADDRESS (1+ RPTN))))))
    (LIST 'PROG (LIST (LIST 'SAVE (LIST 'QUOTE SAVE)))
	  (LIST 'RPTQ (- (MEM=GETSIZE ADDRESS) 2)
		(LIST 'PROGN (LIST 'MEM-PUT ADDRESS 'RPTN (LIST 'CAR 'SAVE)) (LIST 'SETQ 'SAVE (LIST 'CDR 'SAVE)))))))

(DEFUN MEM-MEMORY NIL
						; INPUT:  NONE
						; VALUE:  NUMBER OF MEMORY CELLS NOT USED SO FAR.
  (+ MEM*REST MEM*COLLECTABLE))

(DEFUN MEM=ALLOC (TYPE CELLS)
						; INPUT:  TYPE IS AN ATOM, SIZE AN INTEGER > 0.
						; EFFECT: IF THERE EXISTS A REUSABLE ADDRESS,
						;         THEN THIS ADDRESS WILL BE FEVOURED
						;         TO ALLOCATE A NEW STORAGE UNIT, OTHERWISE
						;         THE NEXT FREE ADDRESS WILL BE USED.
						; VALUE:  THE NEW ADDRESS.
  (let (VADR)
    (incf cells 3)
    (COND (MEM*FIRST.REUSABLE.VADR (SETf VADR                            MEM*FIRST.REUSABLE.VADR
					 (aref MEM*MEMORY MEM*NEXT.RADR) CELLS)
				   (if (EQL (AREF MEM*MEMORY VADR) 'end)
				       (SETQ MEM*FIRST.REUSABLE.VADR NIL
					     MEM*LAST.REUSABLE.VADR  NIL)
				       (SETQ MEM*FIRST.REUSABLE.VADR (- (AREF MEM*MEMORY VADR))))
				   (setf (aref MEM*MEMORY VADR) MEM*NEXT.RADR
					 MEM*REST               (- MEM*REST CELLS)))
	  (t (setf (aref MEM*MEMORY MEM*NEXT.VADR) MEM*NEXT.RADR
		   (aref MEM*MEMORY MEM*NEXT.RADR) cells
		   VADR           MEM*NEXT.VADR
		   MEM*NEXT.VADR (1+ MEM*NEXT.VADR)
		   MEM*REST      (- MEM*REST cells 1))))
    (SETf MEM*NEXT.RADR                         (- MEM*NEXT.RADR CELLS)
	  (aref MEM*MEMORY (1+ MEM*NEXT.RADR))  TYPE
	  (aref MEM*MEMORY (+ 2 MEM*NEXT.RADR)) NIL)
    VADR))

(DEFUN MEM=ENLARGE.SIZE (NEWSIZE)
						; INPUT:  A NEW MEMORY SIZE.
						; EFFECT: INITIALIZATION OF A NEW GREATER MEMORY.
						;         THE CONTENTS OF THE OLD MEMORY IS COPIED.
						; VALUE:  UNDEFINED.
  (LET ((IND       (1- MEM*SIZE))
	(NEWARRAY  (MAKE-ARRAY NEWSIZE))
	(INCREMENT (- NEWSIZE MEM*SIZE)))
    (DODOWN (VADR MEM*NEXT.VADR)
      (COND ((EQL (AREF MEM*MEMORY VADR) 'ATP.MEMORY.NIL)
	     (SETF (AREF NEWARRAY VADR) 'ATP.MEMORY.NIL))
	    ((or (EQl (AREF MEM*MEMORY VADR) 'END) (< (AREF MEM*MEMORY VADR) 0))
	     (SETF (AREF NEWARRAY VADR)
		   (AREF MEM*MEMORY VADR)))
	    (T (SETF (AREF NEWARRAY VADR)
		     (+ INCREMENT (IF (NUMBERP (AREF MEM*MEMORY VADR)) (AREF MEM*MEMORY VADR) 0))))))
    (DOTIMES (COUNTER (- MEM*SIZE 1 MEM*NEXT.RADR))
      #+symbolics(declare (ignore counter))
      (SETF (AREF NEWARRAY (+ IND INCREMENT))
	    (AREF MEM*MEMORY IND))
      (decf IND))
    (SETQ MEM*REST (+ MEM*REST (- NEWSIZE MEM*SIZE 1)))
    (SETQ MEM*NEXT.RADR (+ MEM*NEXT.RADR INCREMENT))
    (SETQ MEM*SIZE NEWSIZE)
    (SETQ MEM*MEMORY NEWARRAY)
    (SETQ MEM*RESERVE (TRUNCATE MEM*SIZE 10))))

(DEFUN MEM=GBC (CELLS)
						; INPUT:  THE NUMBER OF CELLS WHICH ARE NECESSARY AS
						;         A MINIMUM TO CHECK THE MEMORY-SATURATION.
						; EFFECT: THE MEMORY IS REORGANIZED. IF THERE EXIST
						;         ANY REUSABLE ADDRESSES, THEN THEY ARE LINKED
						;         FROM THE SMALLER TO THE GREATER ADDRESSES.
  (let ((FLAG t) (LAST 'end))
    (SETQ MEM*FIRST.REUSABLE.VADR NIL
	  MEM*LAST.REUSABLE.VADR  NIL)
    (DODOWN (VADR  MEM*NEXT.VADR)		; loop over all virtual addresses
      (let ((RADR (AREF MEM*MEMORY VADR)))
	(COND ((eql RADR 'ATP.MEMORY.NIL)
	       (SETQ FLAG NIL))
	      ((and (not (eql 'end radr)) (>= RADR 0))
	       (SETf FLAG NIL
		     (aref MEM*MEMORY VADR) (AREF MEM*MEMORY RADR)
		     (aref MEM*MEMORY RADR) VADR))
	      (FLAG (setf (aref MEM*MEMORY VADR) 'ATP.MEMORY.NOBIND)
		    (SETQ MEM*NEXT.VADR VADR))
	      (T (setf (aref MEM*MEMORY VADR) LAST)
		 (SETQ LAST (- VADR))
		 (SETQ MEM*FIRST.REUSABLE.VADR VADR)
		 (unless MEM*LAST.REUSABLE.VADR (SETQ MEM*LAST.REUSABLE.VADR VADR))))))
    (if (EQL MEM*NEXT.VADR 0)
	(SETQ MEM*NEXT.RADR (1- mem*size))
	(let* ((FREE (1- mem*size))
	       (NEXT free)
	       VA SIZE)
	  (WHILE (> NEXT MEM*NEXT.RADR)
	    (WHILE (AND (EQL (AREF MEM*MEMORY NEXT) 'ATP.MEMORY.NOBIND)
			(NEQ NEXT MEM*NEXT.RADR))
	      (SETQ NEXT (1- NEXT)))
	    (when (> NEXT MEM*NEXT.RADR)
	      (SETf VA                     (AREF MEM*MEMORY NEXT)
		    SIZE                   (AREF MEM*MEMORY VA)
		    (aref MEM*MEMORY VA)   FREE
		    (aref MEM*MEMORY FREE) SIZE
		    FREE                   (1- FREE)
		    NEXT                   (1- NEXT))
	      (DOtimes (RPTN (1- SIZE))
		#+symbolics(declare (ignore rptn))
		(setf (aref MEM*MEMORY FREE) (AREF MEM*MEMORY NEXT)
		      FREE                   (1- FREE)
		      NEXT                   (1- NEXT)))))
	  (MEM=CLEAN.UP MEM*NEXT.VADR FREE) (SETQ MEM*NEXT.RADR FREE)))
    (SETQ MEM*REST (- (1+ MEM*NEXT.RADR) MEM*NEXT.VADR))
    (SETQ MEM*COLLECTABLE 0)
    (when (< (+ MEM*REST MEM*COLLECTABLE) CELLS)
      (when (< mem*increment 500000) (setq mem*increment mem*size))
      (format t "~&----- Memory: Overflow. Attempt to increase size. New memory size: ~D.~&" (+ MEM*SIZE MEM*INCREMENT))
      (MEM=ENLARGE.SIZE (+ MEM*SIZE MEM*INCREMENT)))
    nil))

(DEFmacro MEM=NOT.ENOUGH.MEMORY (CELLS)
						; INPUT:  A CERTAIN NUMBER OF MEMORY-CELLS
						; VALUE:  T IF THERE IS NOT ENOUGH CONTIGNOUS
						;         MEMORY, OTHERWISE NIL.
  `(< MEM*REST ,CELLS))

(DEFmacro MEM=GETSIZE (VADR)
						; INPUT:  A VIRTUEL ADDRESS
						; VALUE: SIZE IN WORDS OF THE ELEMENT WITH
						;        THE VIRTUEL ADDRESS VADR.
  `(AREF MEM*MEMORY (MEM=REAL.ADDR ,VADR)))

(DEFmacro MEM=REAL.ADDR (VADR)
						; INPUT:  A VIRTUEL ADDRESS
						; VALUE:  ADDRESS OF FIRST COMPONENT OF ALLOCATION-
						;         UNIT WITH THE VIRTUEL ADDRESS VADR.
  `(AREF MEM*MEMORY ,VADR))




(DEFmacro MEM-PUTPROP (ADR IND PROPERTY)	; INPUT:  A STORAGE ADDRESS,A PROPERTY INDICATOR
						;         AND AN S-EXPRESSION
						; EFFECT: LIKE LISP FUNCTION PUTPROP
						; VALUE:  UNDEFINED.
  `(setf (getf (AREF MEM*MEMORY (MEM=PROPLIST.RADR ,ADR)) ,ind)
	 ,property))


(DEFmacro MEM-GETPROP (ADR IND)			; INPUT:  A STORAGE ADDRESS AND A PROPERTY INDICATOR.
						; VALUE:  THE PROPERTY BELONGING TO THIS INDICATOR
						;         (LIKE LISP FUNCTION GETPROP)
  `(getf (AREF MEM*MEMORY (MEM=PROPLIST.RADR ,ADR)) ,ind))

(DEFun MEM-REMPROP (ADR IND)
						; INPUT:  A STORAGE ADDRESS AND A PROPERTY INDICATOR
						; EFFECT: THE PROPERTY IS REMOVED FROM ADR'S PROPERTYLIST.
						;         (LIKE LISP FUNCTION REMPROP)
						; VALUE:  IND IF SUCH A PROPERTY EXISTS, ELSE NIL
  (if (remf (aref MEM*MEMORY (MEM=PROPLIST.RADR ADR)) ind)
      ind
      nil))

(DEFUN MEM-REMPROPS (ADR INDS)			; INPUT:  A STORAGE ADDRESS AND NIL OR A LIST
						;         OF PROPERTY INDICATORS.
						; EFFECT: IF IND = NIL, ALL PROPERTieS ARE
						;         REMOVED FROM THE PROPERTY LIST,
						;         ELSE THE PROPERTIES IN INDS ARE REMOVED
						;         (LIKE SERVICE FUNCTION REMPROPS).
						; VALUE:  UNDEFINED.
  
  (let ((RADR (MEM=PROPLIST.RADR ADR)))
    (if inds
	(mapc #'(lambda (ind) (remf (aref MEM*MEMORY radr) ind)) inds)
	(setf (aref MEM*MEMORY RADR) NIL))))

(DEFmacro MEM-GETPROPLIST (ADR)
						; INPUT:  A STORAGE ADDRESS
						; VALUE:  THE PROPERTY LIST (LIKE LISP FUNCTION
						;                            GETPROPLIST)
  `(AREF MEM*MEMORY (MEM=PROPLIST.RADR ,ADR)))


(DEFmacro MEM-SETPROPLIST (ADR PROPLIST)
						; INPUT:  A STORAGE ADDRESS AND A PROPERTY LIST
						; EFFECT: THE PROPERTY LIST IS STORED INTO
						;         THE PROPERTY CELL OF ADR
						;         (LIKE LISP FUNCTION SETPROPLIST)
						; VALUE:  UNDEFINED.
  `(setf (aref MEM*MEMORY (MEM=PROPLIST.RADR ,ADR)) ,PROPLIST))

(DEFUN MEM-ADDPROP (ADR IND NEW FRONTFLAG)	; INPUT:  A STRORAGE ADDRESS, A PROPERTY INDICATOR,
						;         AN S-EXPRESSION AND A BOOLEAN VALUE.
						; EFFECT: THE PROPERTY IND IS ASSUMED TO BE NIL
						;         OR A LIST.
						;         THE S-EXPRESSION NEW IS ADDED AT
						;         THE FRONT OF THE LIST (FRONTFLAG = T)
						;         OR AT THE END OF THE LIST (FRONTFLAG = NIL)
						;         (LIKE LISP FUNCTION ADDPROP.
						; VALUE:  UNDEFINED
  (let* ((RADR (MEM=PROPLIST.RADR ADR))
	 (PROPLIST (AREF MEM*MEMORY RADR))
	 (tail (member ind proplist)))
    (if tail
	(setf (second tail) (if frontflag
				(CONS NEW (second TAIL))
				(nconc1 new (second tail))))
	(setf (aref MEM*MEMORY RADR) (list* IND (LIST NEW) PROPLIST)))))

(DEFmacro MEM=PROPLIST.RADR (VADR)		; INPUT:  A VIRTUAL ADDRESS
						; VALUE:  THE REAL ADDRESS OF THE PROPERTYLIST
						;         BELONGING TO VADR.
  `(let ((RADR (MEM=REAL.ADDR ,VADR)))
     (- RADR -2 (AREF MEM*MEMORY RADR))))


