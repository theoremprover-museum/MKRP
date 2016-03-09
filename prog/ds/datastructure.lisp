;;; -*- Package: MKRP; Syntax: Common-Lisp; Mode: LISP -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

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

;;; SIGN variables
;;; --------------


(DEFVAR DS*SIGN.PLUS.SYMBOLS '(+ ++))

(DEFVAR DS*SIGN.MINUS.SYMBOLS '(- --))


;;; CLAUSE variables
;;; ----------------


(DEFVAR DS*CLAUSE.COMMON.CELLS 9)


(DEFconstant DS*CLAUSE.LITERAL.CELLS 24)



;; Internal CLAUSE macros
;; ----------------------


(DEFMACRO DS=CLAUSE.STORAGE (NOLIT)
						; EDITED:  27. 4. 1982   HJO
						; VALUE:   POINTER TO A NEW STORAGE UNIT FOR A CLAUSE
						;          WITH NOLIT LITERALS.
  `(MEM-NEW 'CLAUSE (+ DS*CLAUSE.COMMON.CELLS (* DS*CLAUSE.LITERAL.CELLS ,NOLIT))))

(DEFMACRO DS=CLAUSE.GET (&OPTIONAL COMPONENT CLAUSE LITNO)
						; EDITED: 29-AUG-84 15:43:09   SYNTHETIC
						; INPUT:  COMPONENT IS ONE OF THE ATOMS:
						;         PNAME, PARENTS, DEPTH, VARIABLES, RENAMING,
						;         NOLIT, ATTRIBUTES, POT.FALSE.LITNOS,
						;         POT.TRUE.LITNOS, SIGN, PREDICATE, TERMLIST,
						;         LIT.PROPLIST, LIT.VARIABLES, R, RIW, RE,
						;         REIW, RD, RIWD, S, SI, SIW, SID, T, TI,
						;         TIW, P, PIW, PD,
						;         CLAUSE IS A CLAUSE ADDRESS,
						;         LITNO IS A LITERAL NUMBER.
						; VALUE:  CONTENTS OF THE MEMORY ARRAY ELEMENT
						;         FOR CLAUSE, LITNO DENOTED BY COMPONENT.
  (if (AND (CONSP COMPONENT) (EQL (CAR COMPONENT) 'QUOTE) (ATOM (SECOND COMPONENT)))
      (CASE (second COMPONENT)
	(PNAME `(MEM-GET ,CLAUSE 1))
	(PARENTS `(MEM-GET ,CLAUSE 2))
	(DEPTH `(MEM-GET ,CLAUSE 3))
	(VARIABLES `(MEM-GET ,CLAUSE 4))
	(RENAMING `(MEM-GET ,CLAUSE 5))
	(NOLIT `(MEM-GET ,CLAUSE 6))
	(ATTRIBUTES `(MEM-GET ,CLAUSE 7))
	(POT.FALSE.LITNOS `(MEM-GET ,CLAUSE 8))
	(POT.TRUE.LITNOS `(MEM-GET ,CLAUSE 9))
	(SIGN `(MEM-GET ,CLAUSE (+ -11 (* ,ds*clause.literal.cells ,LITNO))))
	(PREDICATE `(MEM-GET ,CLAUSE (+ -10 (* ,ds*clause.literal.cells ,LITNO))))
	(TERMLIST `(MEM-GET ,CLAUSE (+ -9 (* ,ds*clause.literal.cells ,LITNO))))
	(LIT.PROPLIST `(MEM-GET ,CLAUSE (+ -8 (* ,ds*clause.literal.cells ,LITNO))))
	(LIT.VARIABLES `(MEM-GET ,CLAUSE (+ -7 (* ,ds*clause.literal.cells ,LITNO))))
	(R `(MEM-GET ,CLAUSE (+ -6 (* ,ds*clause.literal.cells ,LITNO))))
	(RIW `(MEM-GET ,CLAUSE (+ -4 (* ,ds*clause.literal.cells ,LITNO))))
	(RD `(MEM-GET ,CLAUSE (+ -2 (* ,ds*clause.literal.cells ,LITNO))))
	(RIWD `(MEM-GET ,CLAUSE (1- (* ,ds*clause.literal.cells ,LITNO))))
	(S `(MEM-GET ,CLAUSE (* ,ds*clause.literal.cells ,LITNO)))
	(SI `(MEM-GET ,CLAUSE (1+ (* ,ds*clause.literal.cells ,LITNO))))
	(SIW `(MEM-GET ,CLAUSE (+ 2 (* ,ds*clause.literal.cells ,LITNO))))
	(SID `(MEM-GET ,CLAUSE (+ 3 (* ,ds*clause.literal.cells ,LITNO))))
	((T) `(MEM-GET ,CLAUSE (+ 4 (* ,ds*clause.literal.cells ,LITNO))))
	(TI `(MEM-GET ,CLAUSE (+ 5 (* ,ds*clause.literal.cells ,LITNO))))
	(TIW `(MEM-GET ,CLAUSE (+ 6 (* ,ds*clause.literal.cells ,LITNO))))
	(P `(MEM-GET ,CLAUSE (+ 7 (* ,ds*clause.literal.cells ,LITNO))))
	(PIW `(MEM-GET ,CLAUSE (+ 8 (* ,ds*clause.literal.cells ,LITNO))))
	(PD `(MEM-GET ,CLAUSE (+ 9 (* ,ds*clause.literal.cells ,LITNO))))
	(max `(MEM-GET ,CLAUSE (+ -12 (* ,ds*clause.literal.cells ,LITNO))))
	(rule `(MEM-GET ,CLAUSE (+ -13 (* ,ds*clause.literal.cells ,LITNO))))
	(unfail `(MEM-GET ,CLAUSE (+ -14 (* ,ds*clause.literal.cells ,LITNO))))
	(OTHERWISE (ERROR "DS=CLAUSE.GET: ILLEGAL COMPONENT~A" COMPONENT)))
      `(DS==CLAUSE.GET ,COMPONENT ,CLAUSE ,LITNO)))

(DEFUN DS==CLAUSE.GET (COMPONENT CLAUSE LITNO)
						; EDITED: 29-AUG-84 15:43:09   SYNTHETIC
						; INPUT:  COMPONENT IS ONE OF THE ATOMS:
						;         PNAME, PARENTS, DEPTH, VARIABLES, RENAMING,
						;         NOLIT, ATTRIBUTES, POT.FALSE.LITNOS,
						;         POT.TRUE.LITNOS, SIGN, PREDICATE, TERMLIST,
						;         LIT.PROPLIST, LIT.VARIABLES, R, RIW, RE,
						;         REIW, RD, RIWD, S, SI, SIW, SID, T, TI,
						;         TIW, P, PIW, PD,
						;         CLAUSE IS A CLAUSE ADDRESS,
						;         LITNO IS A LITERAL NUMBER.
						; VALUE:  CONTENTS OF THE MEMORY ARRAY ELEMENT
						;         FOR CLAUSE, LITNO DENOTED BY COMPONENT.
  (CASE COMPONENT
    (PNAME (MEM-GET CLAUSE 1))
    (PARENTS (MEM-GET CLAUSE 2))
    (DEPTH (MEM-GET CLAUSE 3))
    (VARIABLES (MEM-GET CLAUSE 4))
    (RENAMING (MEM-GET CLAUSE 5))
    (NOLIT (MEM-GET CLAUSE 6))
    (ATTRIBUTES (MEM-GET CLAUSE 7))
    (POT.FALSE.LITNOS (MEM-GET CLAUSE 8))
    (POT.TRUE.LITNOS (MEM-GET CLAUSE 9))
    (SIGN (MEM-GET CLAUSE (+ -11 (* ds*clause.literal.cells LITNO))))
    (PREDICATE (MEM-GET CLAUSE (+ -10 (* ds*clause.literal.cells LITNO))))
    (TERMLIST (MEM-GET CLAUSE (+ -9 (* ds*clause.literal.cells LITNO))))
    (LIT.PROPLIST (MEM-GET CLAUSE (+ -8 (* ds*clause.literal.cells LITNO))))
    (LIT.VARIABLES (MEM-GET CLAUSE (+ -7 (* ds*clause.literal.cells LITNO))))
    (R (MEM-GET CLAUSE (+ -6 (* ds*clause.literal.cells LITNO))))
    (RIW (MEM-GET CLAUSE (+ -4 (* ds*clause.literal.cells LITNO))))
    (RD (MEM-GET CLAUSE (+ -2 (* ds*clause.literal.cells LITNO))))
    (RIWD (MEM-GET CLAUSE (1- (* ds*clause.literal.cells LITNO))))
    (S (MEM-GET CLAUSE (* ds*clause.literal.cells LITNO)))
    (SI (MEM-GET CLAUSE (1+ (* ds*clause.literal.cells LITNO))))
    (SIW (MEM-GET CLAUSE (+ 2 (* ds*clause.literal.cells LITNO))))
    (SID (MEM-GET CLAUSE (+ 3 (* ds*clause.literal.cells LITNO))))
    ((T) (MEM-GET CLAUSE (+ 4 (* ds*clause.literal.cells LITNO))))
    (TI (MEM-GET CLAUSE (+ 5 (* ds*clause.literal.cells LITNO))))
    (TIW (MEM-GET CLAUSE (+ 6 (* ds*clause.literal.cells LITNO))))
    (P (MEM-GET CLAUSE (+ 7 (* ds*clause.literal.cells LITNO))))
    (PIW (MEM-GET CLAUSE (+ 8 (* ds*clause.literal.cells LITNO))))
    (PD (MEM-GET CLAUSE (+ 9 (* ds*clause.literal.cells LITNO))))
    (max (MEM-GET CLAUSE (+ -12 (* ds*clause.literal.cells LITNO))))
    (rule (MEM-GET CLAUSE (+ -13 (* ds*clause.literal.cells LITNO))))
    (unfail (MEM-GET CLAUSE (+ -14 (* ds*clause.literal.cells LITNO))))
    (OTHERWISE (ERROR "DS=CLAUSE.GET: ILLEGAL COMPONENT~A" COMPONENT))))

(DEFMACRO DS=CLAUSE.PUT (&OPTIONAL COMPONENT VALUE CLAUSE LITNO)
						; edited: 29-aug-84 15:43:09   synthetic
						; input:  component is one of the atoms:
						;         pname, parents, depth, variables, renaming,
						;         nolit, attributes, pot.false.litnos,
						;         pot.true.litnos, sign, predicate, termlist,
						;         lit.proplist, lit.variables, R, RIW, RE,
						;         REIW, RD, RIWD, S, SI, SIW, SID, T, TI, TIW, P, PIW, PD,
						;         clause is a clause address, litno is a literal number.
						;         value is any s-expression
						; effect: value is assigned to memory array element
						;         for clause, litno denoted by component.
  (if (AND (CONSP COMPONENT) (EQL (CAR COMPONENT) 'QUOTE) (ATOM (SECOND COMPONENT)))
      (CASE (second component)
	(PNAME     `(MEM-PUT ,CLAUSE 1 ,VALUE))
	(PARENTS   `(MEM-PUT ,CLAUSE 2 ,VALUE))
	(DEPTH     `(MEM-PUT ,CLAUSE 3 ,VALUE))
	(VARIABLES `(MEM-PUT ,CLAUSE 4 ,VALUE))
	(RENAMING  `(MEM-PUT ,CLAUSE 5 ,VALUE))
	(NOLIT     `(MEM-PUT ,CLAUSE 6 ,VALUE))
	(ATTRIBUTES       `(MEM-PUT ,CLAUSE 7 ,VALUE))
	(POT.FALSE.LITNOS `(MEM-PUT ,CLAUSE 8 ,VALUE))
	(POT.TRUE.LITNOS  `(MEM-PUT ,CLAUSE 9 ,VALUE))
	(SIGN             `(MEM-PUT ,CLAUSE (+ -11 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(PREDICATE        `(MEM-PUT ,CLAUSE (+ -10 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(TERMLIST         `(MEM-PUT ,CLAUSE (+ -9  (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(LIT.PROPLIST     `(MEM-PUT ,CLAUSE (+ -8  (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(LIT.VARIABLES    `(MEM-PUT ,CLAUSE (+ -7  (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(R      `(MEM-PUT ,CLAUSE (+ -6 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(RIW    `(MEM-PUT ,CLAUSE (+ -4 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(RD     `(MEM-PUT ,CLAUSE (+ -2 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(RIWD   `(MEM-PUT ,CLAUSE (+ -1 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(S      `(MEM-PUT ,CLAUSE (* ,ds*clause.literal.cells ,LITNO) ,VALUE))
	(SI     `(MEM-PUT ,CLAUSE (+ 1 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(SIW    `(MEM-PUT ,CLAUSE (+ 2 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(SID    `(MEM-PUT ,CLAUSE (+ 3 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	((T)    `(MEM-PUT ,CLAUSE (+ 4 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(TI     `(MEM-PUT ,CLAUSE (+ 5 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(TIW    `(MEM-PUT ,CLAUSE (+ 6 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(P      `(MEM-PUT ,CLAUSE (+ 7 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(PIW    `(MEM-PUT ,CLAUSE (+ 8 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(PD     `(MEM-PUT ,CLAUSE (+ 9 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))	
	(max    `(MEM-PUT ,CLAUSE (+ -12 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(rule   `(MEM-PUT ,CLAUSE (+ -13 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(unfail `(MEM-PUT ,CLAUSE (+ -14 (* ,ds*clause.literal.cells ,LITNO)) ,VALUE))
	(OTHERWISE (ERROR "DS=CLAUSE.PUT: ILLEGAL COMPONENT~A" COMPONENT)))
      `(DS==CLAUSE.PUT ,COMPONENT ,VALUE ,CLAUSE ,LITNO)))

(DEFUN DS==CLAUSE.PUT (COMPONENT VALUE CLAUSE LITNO)
						; edited: 29-aug-84 15:43:09   synthetic
						; input:  component is one of the atoms:
						;         pname, parents, depth, variables, renaming,
						;         nolit, attributes, pot.false.litnos,
						;         pot.true.litnos, sign, predicate, termlist,
						;         lit.proplist, lit.variables, R, RIW, RE,
						;         REIW, RD, RIWD, S, SI, SIW, SID, T, TI, TIW, P, PIW, PD,
						;         clause is a clause address, litno is a literal number.
						;         value is any s-expression
						; effect: value is assigned to memory array element
						;         for clause, litno denoted by component.
  (CASE COMPONENT
    (PNAME     (MEM-PUT CLAUSE 1 VALUE))
    (PARENTS   (MEM-PUT CLAUSE 2 VALUE))
    (DEPTH     (MEM-PUT CLAUSE 3 VALUE))
    (VARIABLES (MEM-PUT CLAUSE 4 VALUE))
    (RENAMING  (MEM-PUT CLAUSE 5 VALUE))
    (NOLIT     (MEM-PUT CLAUSE 6 VALUE))
    (ATTRIBUTES        (MEM-PUT CLAUSE 7 VALUE))
    (POT.FALSE.LITNOS  (MEM-PUT CLAUSE 8 VALUE))
    (POT.TRUE.LITNOS   (MEM-PUT CLAUSE 9 VALUE))
    (SIGN              (MEM-PUT CLAUSE (+ -11 (* ds*clause.literal.cells LITNO)) VALUE))
    (PREDICATE         (MEM-PUT CLAUSE (+ -10 (* ds*clause.literal.cells LITNO)) VALUE))
    (TERMLIST          (MEM-PUT CLAUSE (+ -9  (* ds*clause.literal.cells LITNO)) VALUE))
    (LIT.PROPLIST      (MEM-PUT CLAUSE (+ -8  (* ds*clause.literal.cells LITNO)) VALUE))
    (LIT.VARIABLES     (MEM-PUT CLAUSE (+ -7  (* ds*clause.literal.cells LITNO)) VALUE))
    (R     (MEM-PUT CLAUSE (+ -6 (* ds*clause.literal.cells LITNO)) VALUE))
    (RIW   (MEM-PUT CLAUSE (+ -4 (* ds*clause.literal.cells LITNO)) VALUE))
    (RD    (MEM-PUT CLAUSE (+ -2 (* ds*clause.literal.cells LITNO)) VALUE))
    (RIWD  (MEM-PUT CLAUSE (1- (* ds*clause.literal.cells LITNO)) VALUE))
    (S     (MEM-PUT CLAUSE (* ds*clause.literal.cells LITNO) VALUE))
    (SI    (MEM-PUT CLAUSE (1+ (* ds*clause.literal.cells LITNO)) VALUE))
    (SIW   (MEM-PUT CLAUSE (+ 2 (* ds*clause.literal.cells LITNO)) VALUE))
    (SID   (MEM-PUT CLAUSE (+ 3 (* ds*clause.literal.cells LITNO)) VALUE))
    ((T)   (MEM-PUT CLAUSE (+ 4 (* ds*clause.literal.cells LITNO)) VALUE))
    (TI    (MEM-PUT CLAUSE (+ 5 (* ds*clause.literal.cells LITNO)) VALUE))
    (TIW   (MEM-PUT CLAUSE (+ 6 (* ds*clause.literal.cells LITNO)) VALUE))
    (P     (MEM-PUT CLAUSE (+ 7 (* ds*clause.literal.cells LITNO)) VALUE))
    (PIW   (MEM-PUT CLAUSE (+ 8 (* ds*clause.literal.cells LITNO)) VALUE))
    (PD    (MEM-PUT CLAUSE (+ 9 (* ds*clause.literal.cells LITNO)) VALUE))	
    (max    (MEM-PUT CLAUSE (+ -12 (* ds*clause.literal.cells LITNO)) VALUE))
    (rule   (MEM-PUT CLAUSE (+ -13 (* ds*clause.literal.cells LITNO)) VALUE))
    (unfail (MEM-PUT CLAUSE (+ -14 (* ds*clause.literal.cells LITNO)) VALUE))
    (OTHERWISE (ERROR "DS=CLAUSE.PUT: ILLEGAL COMPONENT~A" COMPONENT))))

(DEFMACRO DS=CLAUSE.IS (CL)
						; EDITED: "18-JAN-80 12:35:52")
						; VALUE: T IF CL IS A CL SYMBOL NOT YET DELETED, ELSE NIL *)
  `(EQL (MEM-TYPE ,CL) 'CLAUSE))

;; LIT functions
;; -------------


(DEFMACRO DS-LIT.CREATE (SIGN PREDICATE TERMLIST)
						; EDITED: 13-NOV-79 13:03:39
						; VALUE: NEW LITERAL OF THE GIVEN COMPONENTS
  `(DS=LIT.STORAGE ,SIGN ,PREDICATE ,TERMLIST))

(DEFUN DS-LIT.RENAMED (LIT)
						; EDITED: 27-NOV-80 11:15:00
						; INPUT:  LIT - LITERAL
						; EFFECT: RETURNS VALUE
						; VALUE:  THE LITERAL WHICH IS CREATED FROM THE
						;         GIVEN LITERAL BY CONSISTENT VARIABLE
						;         RENAMING
  (DS=LIT.PUTTERMLIST LIT (CDR (DT-TERM.RENAMED (DS=LIT.GETTERMLIST LIT)))) LIT)

(DEFMACRO DS-LIT.SIGN (LIT)
						; EDITED: 13-NOV-79 14:21:40
						; VALUE: SIGN OF LITERAL LIT
  `(DS=LIT.GETSIGN ,LIT))

(DEFMACRO DS-LIT.PREDICATE (LIT)
						; EDITED: "13-NOV-79 14:22:35")
						; VALUE: PREDICATE OF LITERAL LIT
  `(DS=LIT.GETPREDICATE ,LIT))

(DEFMACRO DS-LIT.TERMLIST (LIT)
						; EDITED: "13-NOV-79 14:23:02")
						; VALUE: TERMLIST OF LITERAL LIT
  `(DS=LIT.GETTERMLIST ,LIT))

(DEFMACRO DS-LIT.IS.NEGATIVE (LIT)
						; EDITED: "13-NOV-79 14:29:28")
						; VALUE: T IF LIT HAS SIGN - , ELSE NIL
  `(MEMBER (DS=LIT.GETSIGN ,LIT) DS*SIGN.MINUS.SYMBOLS))

(DEFMACRO DS-LIT.IS.POSITIVE (LIT)
						; EDITED: "13-NOV-79 14:30:14")
						; VALUE: T IF LIT HAS SIGN + , ELSE NIL
  `(MEMBER (DS=LIT.GETSIGN ,LIT) DS*SIGN.PLUS.SYMBOLS))

(DEFMACRO DS-LIT.IS.EQUALITY (LIT)
						; EDITED: " 8-JUL-80 09:15:11")
						; VALUE: T IF LIT//S PREDICATE IS EQUALITY, ELSE NIL
  `(MEMBER (DT-PREDICATE.GET 'PNAME (DS=LIT.GETPREDICATE ,LIT)) DS*EQUALITY.SYMBOLS :TEST (FUNCTION EQUAL)))

(defun  ds-lits.vars (lits)
  (delete-duplicates (mapcan #'(lambda (lit) (dt-termlist.variables (ds-lit.termlist lit))) lits)))

(DEFMACRO DS-SIGN.IS.POSITIVE (SIGN)
						; EDITED: "10-JUL-80 16:28:56")
						; INPUT:  A SIGN SYMBOL.
						; EFFECT: RETURNS VALUE
						; VALUE:  T IF THE GIVEN SIGN DENOTES THE PLUS
						;         SYMBOL, ELSE NIL.
  `(EQ ,SIGN '+))

(DEFMACRO DS-SIGN.IS.NEGATIVE (SIGN)
						; EDITED: "10-JUL-80 16:28:56")
						; INPUT:  A SIGN SYMBOL.
						; EFFECT: RETURNS VALUE
						; VALUE:  T IF THE GIVEN SIGN DENOTES THE MINUS
						;         SYMBOL, ELSE NIL.
  `(EQ ,SIGN '-))

(DEFMACRO DS-SIGN.ARE.EQUAL (SIGN1 SIGN2)
						; EDITED: "10-JUL-80 16:28:56")
						; INPUT:  A SIGN SYMBOL.
						; EFFECT: RETURNS VALUE
						; VALUE:  T IF SIGN1 IS EQUAL TO SIGN2 ELSE NIL.
  `(EQ ,SIGN1 ,SIGN2))

(DEFMACRO DS-SIGN.ARE.NOT.EQUAL (SIGN1 SIGN2)
						; EDITED: "10-JUL-80 16:28:56")
						; INPUT:  A SIGN SYMBOL.
						; EFFECT: RETURNS VALUE
						; VALUE:  T IF SIGN1 IS NOT EQUAL TO SIGN2, ELSE NIL
  `(NEQ ,SIGN1 ,SIGN2))

(DEFMACRO DS-SIGN.OTHER.SIGN (SIGN)
  `(if (EQ ,SIGN '+)
       '-
       '+))

(DEFMACRO DS=LIT.STORAGE (SIGN PREDICATE TERMLIST)
						; EDITED: 22-NOV-79 12:07:58
						; VALUE: POINTER TO A NEW STORAGE UNIT FOR A LITERAL.
						; STRUCTURE: ORDINARY LIST OF THE GIVEN COMPONENTS
  `(LIST ,SIGN ,PREDICATE ,TERMLIST nil))

(DEFMACRO DS=LIT.GETSIGN (LIT)
						; EDITED: 13-NOV-79 14:24:38
  `(CAR ,LIT))

(DEFMACRO DS=LIT.PUTSIGN (LIT SIGN)
						; EDITED: 13-NOV-79 14:19:31
  `(RPLACA ,LIT ,SIGN))

(DEFMACRO DS=LIT.GETPREDICATE (LIT)
						; EDITED: 13-NOV-79 14:24:54
  `(SECOND ,LIT))

(DEFMACRO DS=LIT.PUTPREDICATE (LIT PREDICATE)
						; EDITED: "13-NOV-79 14:20:14")
  `(RPLACA (CDR ,LIT) ,PREDICATE))

(DEFMACRO DS=LIT.GETTERMLIST (LIT)
						; EDITED: "13-NOV-79 14:25:08")
  `(THIRD ,LIT))

(DEFMACRO DS=LIT.PUTTERMLIST (LIT TERMLIST)
						; EDITED: "13-NOV-79 14:20:45")
  `(RPLACA (CDDR ,LIT) ,TERMLIST))

(DEFUN DS-PREDICATE.OTHERSIDES (SIGN PREDICATE COLOUR)
						; EDITED: 29-SEP-83 12:40:36
						; INPUT:  A SIGN, A PREDICATE AND A LINK COLOR
						;         (R, RIW, S, SI, SIW, T, TI, TIW)
						; VALUE:  A LIST WITH ELEMENTS
						;         (SIGN PREDICATE (ORIENTATION1 . RULE1) ...)
						;         RULE IS EITHER NIL OR AN ATOM LIKE
						;         'SYMMETRIC' OR A LIST
						;         (RULEADDRESS LITNO1 LITNO2 cLITNO2 LITNO1e)
  (COND
    ((DS-SIGN.IS.POSITIVE SIGN)
     (CASE COLOUR
       ((R RIW) (DT-PREDICATE.GET '+ROTHERSIDES PREDICATE))
       ((S SI SIW) (DT-PREDICATE.GET '+SOTHERSIDES PREDICATE))
       ((T TIW) (DT-PREDICATE.GET '+TOTHERSIDES PREDICATE))
       (TI (APPEND (DT-PREDICATE.GET '+TOTHERSIDES PREDICATE)
		   (MAPCAN #'(LAMBDA (OTHERSIDE)
			       (let ((RULES (REMOVE-IF-NOT #'(LAMBDA (RULE)
							       (DS-RULE.R.IFF.T (CDR RULE)))
							   (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDE))))
				 (COND (RULES (LIST (CONS (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDE)
							  (CONS (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDE) RULES)))))))
			   (DT-PREDICATE.GET '+ROTHERSIDES PREDICATE))))
       (OTHERWISE (ERROR "ILLEGAL COLOUR IN DS-PREDICATE.OTHERSIDES~A" COLOUR))))
    (T (CASE COLOUR
	 ((R RIW) (DT-PREDICATE.GET '-ROTHERSIDES PREDICATE))
	 ((S SI SIW) (DT-PREDICATE.GET '-SOTHERSIDES PREDICATE))
	 ((T TIW) (DT-PREDICATE.GET '-TOTHERSIDES PREDICATE))
	 (TI
	   (APPEND (DT-PREDICATE.GET '-TOTHERSIDES PREDICATE)
		   (MAPCAN #'(LAMBDA (OTHERSIDE)
			       (let ((RULES (REMOVE-IF-NOT #'(LAMBDA (RULE) (DS-RULE.R.IFF.T (CDR RULE)))
							   (DS-PREDICATE.GET.OTHERSIDE.RULES OTHERSIDE))))
				 (COND (RULES (LIST (CONS (DS-PREDICATE.GET.OTHERSIDE.SIGN OTHERSIDE)
							  (CONS (DS-PREDICATE.GET.OTHERSIDE.PREDICATE OTHERSIDE) RULES)))))))
			   (DT-PREDICATE.GET '-ROTHERSIDES PREDICATE))))
	 (OTHERWISE (ERROR "ILLEGAL COLOUR IN DS-PREDICATE.OTHERSIDES~A" COLOUR))))))

(DEFMACRO DS-PREDICATE.GET.OTHERSIDE.SIGN (ELEM)
						; INPUT: ELEM = (SIGN PREDICATE ORIENTATION . RULE)
						; VALUE: SIGN
  `(CAR ,ELEM))

(DEFMACRO DS-PREDICATE.GET.OTHERSIDE.PREDICATE (ELEM)
						; INPUT: ELEM = (SIGN PREDICATE ORIENTATION . RULE)
						; VALUE: PREDICATE
  `(SECOND ,ELEM))

(DEFMACRO DS-PREDICATE.GET.OTHERSIDE.RULES (OTHERSIDES)
						; EDITED: 4. 8. 1984
						; INPUT:  A LIST
						;         (SIGN PRED (ORIENT.1 . RULEDESCRIPTOR1) ...
						; VALUE:  THE ORIENTATION-RULEDESCRIPTOR LISTS.
  `(CDDR ,OTHERSIDES))


;; RULE variables
;; --------------


(DEFVAR DS*RULE.ADMISSIBLE.OTHERSIDES_T '((++ + +) (+- + -) (-+ - +) (-- - -)))

(DEFVAR DS*RULE.ADMISSIBLE.OTHERSIDES_S+ '((++ - +) (+- - -) (-+ + +) (-- + -)))

(DEFVAR DS*RULE.ADMISSIBLE.OTHERSIDES_S- '((++ + -) (+- + +) (-+ - -) (-- - +)))

(DEFVAR DS*RULE.ADMISSIBLE.OTHERSIDES_R '((++ - -) (+- - +) (-+ + -) (-- + +)))

(DEFVAR DS*RULES NIL)

(DEFMACRO DS-RULES NIL				; EDITED: 19-JAN-84 19:37:39        MW
						; VALUE:  CURRENTLY DEFINED TWO-LITERAL RULE CLAUSES.
  `DS*RULES)

(DEFUN DS-RULE.TERMLISTS (RULE)
						; EDITED: 3. 8. 1984
						; INPUT:  RULE IS A LIST  (CLAUSE 1 2) OR
						;                         (CLAUSE 2 1) OR
						;                         (CLAUSE 1 2 2 1)
						; VALUE: CASE 1: ((APPEND (TERMLIST CLAUSE 1)
						;                         (TERMLIST CLAUSE 2)) ...)
						;        CASE 2: ((APPEND (TERMLIST CLAUSE 2)
						;                         (TERMLIST CLAUSE 1)) ...)
						;        CASE 3:  (LIST   (CASE 1) (CASE 2) ...)
						;        INCLUDING PERMUTATIONS DUE TO PREDICATE
						;        SYMMETRIES.
  (COND ((CDDDR RULE) (DT-GETPROP (CAR RULE) 'DS*TERMLISTS12))
	((EQL 1 (SECOND RULE)) (DT-GETPROP (CAR RULE) 'DS*TERMLISTS1))
	(T (DT-GETPROP (CAR RULE) 'DS*TERMLISTS2))))

(DEFUN DS-RULE.ORIENTED (RULE COLOUR)
						; EDITED:  4-OCT-83 13:37:37
						; INPUT:   RULE IS AN ATOM LIKE NIL, SYMMETRIC,
						;          ASYMMETRIC OR A LIST (CLAUSE L1 L2)
						;          COLOUR IS A LINK COLOUR
						; VALUE:   T IF LINKS OF THIS COLOUR CREATED WITH
						;          THIS RULE ARE NECESSARILY  ORIENTED,
						;          ELSE NIL
  (CASE RULE
    ((SYMMETRIC ASYMMETRIC NIL) NIL)
    (OTHERWISE (AND (MEMBER COLOUR '(S SI SIW SID))
		    (NOT (DT-GETPROP (CAR RULE) 'TWO*ORIENTATION))))))

(DEFUN DS-RULE.INSERT (CLAUSE)
						; EDITED: 18-OCT-83 18:53:10        MW
						; INPUT:  CLAUSE: A (TWO-LITERAL) CLAUSE TO BECOME
						;         A NEW RULE.
						; EFFECT: THE 'OTHERSIDES' LISTS OF CLAUSE ARE UPDATED
						;         ACCORDINGLY.
						; VALUE:  THE LIST OF RULE CLAUSES (ADDRESSES).
  (PROG
    ((PRED1 (DS=CLAUSE.GET 'PREDICATE CLAUSE 1))
     (PRED2 (DS=CLAUSE.GET 'PREDICATE CLAUSE 2))
     (RULESIGN1 (DS=CLAUSE.GET 'SIGN CLAUSE 1))
     (RULESIGN2 (DS=CLAUSE.GET 'SIGN CLAUSE 2))
     (ORIENTATION+ (COND ((EQL (DT-GETPROP CLAUSE 'TWO*ORIENTATION) 'NONE) NIL)
			 (T 'POSITIVE)))
     (ORIENTATION- (COND ((EQL (DT-GETPROP CLAUSE 'TWO*ORIENTATION) 'NONE) NIL)
			 (T 'NEGATIVE)))
     RULESIGNS CHANGED.OTHERSIDES
     TERMLISTS1 TERMLISTS2 TL1 TL2)
    (SETQ RULESIGNS (INTERN (format nil "~A~A" RULESIGN1 RULESIGN2)
			    (find-package "MKRP")))
    (let ((ACTUAL.SIGNS (CdR (ASSOC RULESIGNS DS*RULE.ADMISSIBLE.OTHERSIDES_T))))	; CONSTRUCT TOTHERSIDES
      (SETQ CHANGED.OTHERSIDES
	    (NCONC (DS=RULE.CONSTRUCT.OTHERSIDE CLAUSE PRED1 PRED2 (CAR ACTUAL.SIGNS) (SECOND ACTUAL.SIGNS) 'T NIL)
		   CHANGED.OTHERSIDES)))
    (let ((ACTUAL.SIGNS (CdR (ASSOC RULESIGNS DS*RULE.ADMISSIBLE.OTHERSIDES_S+))))	; CONSTRUCT SOTHERSIDES
      (SETQ CHANGED.OTHERSIDES
	    (NCONC (DS=RULE.CONSTRUCT.OTHERSIDE CLAUSE PRED1 PRED2 (CAR ACTUAL.SIGNS) (SECOND ACTUAL.SIGNS) 'S ORIENTATION+)
		   CHANGED.OTHERSIDES)))
    (let ((ACTUAL.SIGNS (CdR (ASSOC RULESIGNS DS*RULE.ADMISSIBLE.OTHERSIDES_S-))))
      (SETQ CHANGED.OTHERSIDES
	    (NCONC (DS=RULE.CONSTRUCT.OTHERSIDE CLAUSE PRED1 PRED2 (CAR ACTUAL.SIGNS) (SECOND ACTUAL.SIGNS) 'S ORIENTATION-)
		   CHANGED.OTHERSIDES)))
    (let ((ACTUAL.SIGNS (CdR (ASSOC RULESIGNS DS*RULE.ADMISSIBLE.OTHERSIDES_R))))	; CONSTRUCT ROTHERSIDES
      (SETQ CHANGED.OTHERSIDES
	    (NCONC (DS=RULE.CONSTRUCT.OTHERSIDE CLAUSE PRED1 PRED2 (CAR ACTUAL.SIGNS) (SECOND ACTUAL.SIGNS) 'R NIL)
		   CHANGED.OTHERSIDES)))
    (when (AND (EQL PRED1 PRED2) (EQL RULESIGN1 RULESIGN2))
      (SMAPL #'(LAMBDA (TAIL)
		 (DT-PREDICATE.PUT (CAR TAIL) PRED1 (DS=RULE.SHORTEN.OTHERSIDE (DT-PREDICATE.GET (CAR TAIL) PRED1) CLAUSE))
		 (DT-PREDICATE.PUT (SECOND TAIL) PRED2 (DS=RULE.SHORTEN.OTHERSIDE (DT-PREDICATE.GET (SECOND TAIL) PRED2) CLAUSE)))
	     #'CDDR CHANGED.OTHERSIDES))
    (SETQ TL1 (DS-CLAUSE.TERMLIST CLAUSE 1)) (SETQ TL2 (DS-CLAUSE.TERMLIST CLAUSE 2))
    (SETQ TERMLISTS1 (LIST (APPEND TL1 (copy-list TL2))))
    (SETQ TERMLISTS2 (LIST (APPEND TL2 (copy-list TL1))))
    (COND
      ((DT-PREDICATE.IS.SYMMETRIC PRED1)
       (SETQ TERMLISTS1 (CONS (CONS (SECOND TL1) (CONS (CAR TL1) (APPEND (CDDR TL1) (copy-list TL2)))) TERMLISTS1))
       (SETQ TERMLISTS2 (CONS (APPEND TL2 (CONS (SECOND TL1) (CONS (CAR TL1) (CDDR TL1)))) TERMLISTS2))
       (COND
	 ((DT-PREDICATE.IS.SYMMETRIC PRED2)
	  (SETQ TERMLISTS1 (CONS (APPEND TL1 (CONS (SECOND TL2) (CONS (CAR TL2) (CDDR TL2)))) TERMLISTS1))
	  (SETQ TERMLISTS2 (CONS (CONS (SECOND TL2) (CONS (CAR TL2) (APPEND (CDDR TL2) (copy-list TL1)))) TERMLISTS2))
	  (SETQ TERMLISTS1
		(CONS
		  (CONS (SECOND TL1) (CONS (CAR TL1) (APPEND (CDDR TL1) (CONS (SECOND TL2)
									      (CONS (CAR TL2) (CDDR TL2)))))) TERMLISTS1))
	  (SETQ TERMLISTS2
		(CONS
		  (CONS (SECOND TL2) (CONS (CAR TL2) (APPEND (CDDR TL2)
							     (CONS (SECOND TL1) (CONS (CAR TL1) (CDDR TL1)))))) TERMLISTS2)))))
      ((DT-PREDICATE.IS.SYMMETRIC PRED2)
       (SETQ TERMLISTS1 (CONS (APPEND TL1 (CONS (SECOND TL2) (CONS (CAR TL2) (CDDR TL2)))) TERMLISTS1))
       (SETQ TERMLISTS2 (CONS (CONS (SECOND TL2) (CONS (CAR TL2) (APPEND (CDDR TL2) (copy-list TL1)))) TERMLISTS2))))
    (DT-PUTPROP CLAUSE 'DS*TERMLISTS1 TERMLISTS1) (DT-PUTPROP CLAUSE 'DS*TERMLISTS2 TERMLISTS2)
    (DT-PUTPROP CLAUSE 'DS*TERMLISTS12 (APPEND TERMLISTS1 (copy-list TERMLISTS2))))
  (SETQ DS*RULES (CONS CLAUSE DS*RULES)) (COND ((DS-CLAUSE.VARIABLES CLAUSE) (DT-PUT.UNI.CREATES.VARIABLES T))))

(DEFMACRO DS-RULE.R.IFF.T (RULE)
						; EDITED: 5. 4. 1984
						; INPUT: A RULE
						; VALUE: T IF THIS RULE GENERATES R-LINKS IFF IT
						;        GENERATES T-LINKS, ELSE NIL
  `(MEMBER ,RULE '(NIL SYMMETRIC)))

(DEFUN DS=RULE.CONSTRUCT.OTHERSIDE (CLAUSE PRED1 PRED2 ENTRYSIGN1 ENTRYSIGN2 LINKCLASS ORIENTATION)
						; EDITED: 18-OCT-83 18:34:50        MW
						; INPUT:  ENTRYSIGN1, ENTRYSIGN2: SIGNS TO BE ENTERED
						;         INTO THE APPROPRIATE 'OTHERSIDES' LIST.
						;         LINKCLASS: T, S, OR R.
						;         ORIENTATION: NIL, POSITIVE, NEGATIVE.
						; EFFECT: CONSTRUCTS THE INSERTIONS INTO THE 'OTHER-
						;         SIDES' LISTS OF A RULE.
						; VALUE:  LIST OF CHANGED 'OTHERSIDES' LIST NAMES.
  (let ((OTHERSIDE1 (intern (format nil "~A~AOTHERSIDES" ENTRYSIGN1 LINKCLASS)
			    (find-package "MKRP")))
	(OTHERSIDE2 (intern (format nil "~A~AOTHERSIDES" ENTRYSIGN2 LINKCLASS)
			    (find-package "MKRP")))
	OTHERSIDES SIDES)
    (SETQ OTHERSIDES (DT-PREDICATE.GET OTHERSIDE1 PRED1))
    (COND ((SETQ SIDES (CAR (MEMBER-IF #'(LAMBDA (SIDE) (AND (EQL (CAR SIDE) ENTRYSIGN2) (EQL (SECOND SIDE) PRED2)))
				       OTHERSIDES)))
	   (NCONC1 SIDES (LIST ORIENTATION CLAUSE 1 2)))
	  (T (DT-PREDICATE.PUT OTHERSIDE1 PRED1 (CONS (LIST ENTRYSIGN2 PRED2 (LIST ORIENTATION CLAUSE 1 2)) OTHERSIDES))))
    (COND
      (ORIENTATION (COND ((EQL ORIENTATION 'POSITIVE) (SETQ ORIENTATION 'NEGATIVE)) (T (SETQ ORIENTATION 'POSITIVE)))))
    (SETQ OTHERSIDES (DT-PREDICATE.GET OTHERSIDE2 PRED2))
    (COND ((SETQ SIDES (CAR (MEMBER-IF #'(LAMBDA (SIDE) (AND (EQL (CAR SIDE) ENTRYSIGN1) (EQL (SECOND SIDE) PRED1)))
				       OTHERSIDES)))
	   (NCONC1 SIDES (LIST ORIENTATION CLAUSE 2 1)))
	  (T (DT-PREDICATE.PUT OTHERSIDE2 PRED2 (CONS (LIST ENTRYSIGN1 PRED1 (LIST ORIENTATION CLAUSE 2 1)) OTHERSIDES))))
    (LIST OTHERSIDE1 OTHERSIDE2)))

(DEFUN DS=RULE.SHORTEN.OTHERSIDE (OTHERSIDE RULE)
						; EDITED: 19-OCT-83 20:41:00        MW
						; INPUT:  OTHERSIDE: An 'OTHERSIDES' list.
						;         RULE: A rule clause address.
						; EFFECT: Double occurrences of RULE with same
						;         orientation are removed.
						; VALUE:  CHANGED LIST.
  (let (ABORT)
    (MAPC #'(LAMBDA (OTHERSIDE)
	      (when (every #'numberp (cdr otherside))
		(RPLACD (CDR OTHERSIDE)
			(DREMAP (CDDR OTHERSIDE) NIL
				#'(LAMBDA (TAIL)
				    (let ((ELEM1 (CAR TAIL))
					  ORIENTATION1 ORIENTATION2)
				      (COND ((EQL RULE (SECOND ELEM1)) (SETQ ORIENTATION1 (CAR ELEM1))
					     (MEMBER-IF #'(lambda (ELEM2)
							    (SETQ ORIENTATION2 (CAR ELEM2))
							    (COND ((AND (EQL RULE (SECOND ELEM2))
									(EQL ORIENTATION1 ORIENTATION2))
								   (COND ((NEQ 'NONE (DT-GETPROP RULE 'TWO*ORIENTATION))
									  (RPLACD (CDR ELEM2) '(1 2 2 1))))
								   (SETQ ABORT T))))
							(CDR TAIL))))))
				#'(LAMBDA (&rest ignore) (declare (ignore ignore)) ABORT)))))
	  OTHERSIDE)
    OTHERSIDE))


(DEFUN DS=CLAUSE.CREATE (PNAME PARENTS DEPTH LITLIST)
						; EDITED: "27-NOV-80 10:35:49")
						; VALUE: NEW CLAUSE SYMBOL
						; EFFECT: ALL COMPONENTS ARE COMPUTED AND STORED
  (let (VARIABLES
	(NOLIT (LIST-LENGTH LITLIST))
	CLAUSE LITNO RENAMING) 
    (SETQ VARIABLES NIL)
    (MAPC #'(LAMBDA (LIT) (SETQ VARIABLES (DS=FIND.VARIABLES (ds-lit.termlist LIT) VARIABLES))) LITLIST)
    (SETQ RENAMING
	  (ZIP (COPY-TREE VARIABLES) (MAPCAR #'(LAMBDA (VAR) (DT-VARIABLE.CREATE (DT-VARIABLE.SORT VAR))) VARIABLES)))
						; RENAMING COMPONENT: (V1 V1' ...VN VN')
    (SETQ CLAUSE (DS=CLAUSE.STORAGE NOLIT)) 
    (DS=CLAUSE.PUT 'PNAME PNAME CLAUSE)		; PNAME STORED
						; PROPLIST STORED
    (DS=CLAUSE.PUT 'PARENTS PARENTS CLAUSE)	; PARENTS STORED
    (DS=CLAUSE.PUT 'DEPTH DEPTH CLAUSE)		; DEPTH STORED
    (DS=CLAUSE.PUT 'VARIABLES VARIABLES CLAUSE NIL)	; VARIABLES STORED 
    (DS=CLAUSE.PUT 'RENAMING RENAMING CLAUSE NIL)	; RENAMING STORED 
    (DS=CLAUSE.PUT 'NOLIT NOLIT CLAUSE)		; NOLIT STORED
    (MAPC #'(LAMBDA (LINKCOLOUR) (DS=CLAUSE.PUT LINKCOLOUR NIL CLAUSE)) DS*LINK.CLAUSE)
    (DS=CLAUSE.PUT 'ATTRIBUTES NIL CLAUSE)	; POTENTIALLY FALSE LITERAL NUMBERS INITIALIZED.
    (DS=CLAUSE.PUT 'POT.FALSE.LITNOS NIL CLAUSE NIL)	; POTENTIALLY TRUE LITERAL NUMBERS INITIALIZED.
    (DS=CLAUSE.PUT 'POT.TRUE.LITNOS NIL CLAUSE NIL)
    (SETQ LITNO 0)
    (MAPC #'(LAMBDA (LIT)
	      (incf LITNO)
	      (DS=CLAUSE.PUT 'max nil CLAUSE LITNO)
	      (DS=CLAUSE.PUT 'unfail nil CLAUSE LITNO)
	      (DS=CLAUSE.PUT 'rule nil CLAUSE LITNO)
	      (DS=CLAUSE.PUT 'LIT.VARIABLES (DT-TERMLIST.VARIABLES (DS=LIT.GETTERMLIST LIT)) CLAUSE LITNO)
	      (DS=CLAUSE.PUT 'SIGN (DS=LIT.GETSIGN LIT) CLAUSE LITNO)	; SIGN OF LITNO-TH LITERAL STORED
	      (DS=CLAUSE.PUT 'PREDICATE (DS=LIT.GETPREDICATE LIT) CLAUSE LITNO)	; PREDICATE OF LITNO-TH LITERAL STORED
	      (DS=CLAUSE.PUT 'TERMLIST (DS=LIT.GETTERMLIST LIT) CLAUSE LITNO)	; TERMLIST OF LITNO-TH LITERAL STORED
	      (DS=CLAUSE.PUT 'LIT.PROPLIST (FOURTH LIT) CLAUSE LITNO)	; LITERAL PROPERTYLIST OF LITNO-TH LITERAL STORED 
	      (ds=clause.lit.reset.linkcolours clause litno))
	  LITLIST)
    CLAUSE))

(defun ds-clause.do (function clause)
						; Edited:  29-MAY-1990 14:15
						; Authors: PRCKLN
						; Input:   A function applicable to the number of a literal
						;          and a clause
						; Effect:  Applies FUNCTION on each literal of CLAUSE,
						;          beginning with the last one.
						; Value:   Undefined
  (do ((litno (ds=clause.get 'nolit clause) (1- litno)))
      ((= litno 0))
    (funcall function litno)))

(defun ds-clause.some (function clause)
						; Edited:  31-OCT-1991 19:49
						; Authors: PRCKLN
						; Input:   A function applicable to the number of a literal
						;          and a clause
						; Effect:  Applies FUNCTION on each literal of CLAUSE, until value not NIL.
						; Value:   This not NIL value, NIL otherwise.
  (do* ((litno (ds=clause.get 'nolit clause) (1- litno))
	(val (funcall function litno) (funcall function litno)))
       ((or (= litno 1) val) val)))

(DEFUN DS-CLAUSE.CREATE (PNAME PARENTS DEPTH LITLIST)
						; EDITED: "27-NOV-80 10:35:49")
						; VALUE: NEW CLAUSE SYMBOL
						; EFFECT: ALL COMPONENTS ARE COMPUTED AND STORED
  (let ((clause (DS=CLAUSE.CREATE PNAME PARENTS DEPTH LITLIST)))
    clause))

(defun ds-clause.pos.equation (clause litno)
						; Edited:  31-OCT-1991 15:13
						; Authors: PRCKLN
						; Input:   Specifies the literal of a clause.
						; Effect:  -
						; Value:   True iff the literal is a positive equation.
  (and (dt-predicate.is.equality (ds-clause.predicate clause litno))
       (ds-sign.is.positive (ds-clause.sign clause litno))))

(defun ds-clause.finite.domain (clause)
						; Edited:  19-AUG-1990 13:33
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  -
						; Value:   The list (a1 ... an) if CLAUSE is of the
						;          form (all x x=a1 or x=a2 or ... or x=an) NIL else.
  (if (rest (ds-clause.variables clause))
      nil
      (do ((litno (ds-clause.nolit clause) (1- litno))
	   (result) (failure))
	  ((or failure (zerop litno))
	   (if failure nil result))
	(if (ds-clause.pos.equation clause litno)
	    (let* ((termlist (ds-clause.termlist clause litno))
		   (left (first termlist))
		   (right (second termlist)))
	      (cond ((dt-variable.is left)
		     (if (dt-constant.is right)
			 (push right result)
			 (setq failure t)))
		    ((dt-constant.is left)
		     (if (dt-variable.is right)
			 (push left result)
			 (setq failure t)))
		    (t (setq failure t))))
	    (setq failure t)))))

(defun ds-lit.finite.domain (lits)
						; Edited:  19-AUG-1990 13:33
						; Authors: PRCKLN
						; Input:   A list of literals representing a clause
						; Effect:  -
						; Value:   The list (a1 ... an) if CLAUSE is of the
						;          form (all x x=a1 or x=a2 or ... or x=an) NIL else.
  (do ((rest.lits lits (rest rest.lits))
       (lit) (var) (result) (failure))
      ((or failure (null rest.lits))
       (if failure nil result))
    (setq lit (first rest.lits))
    (if (and (dt-predicate.is.equality (ds-lit.predicate lit))
	     (ds-sign.is.positive (ds-lit.sign lit)))
	(let* ((termlist (ds-lit.termlist lit))
	       (left (first termlist))
	       (right (second termlist)))
	  (cond ((dt-variable.is left)
		 (unless var (setq var left))
		 (if (and (eql var left) (dt-constant.is right))
		     (push right result)
		     (setq failure t)))
		((dt-constant.is left)
		 (when (and (not var) (dt-variable.is right)) (setq var right))
		 (if (and (eql var right) (dt-variable.is right))
		     (push left result)
		     (setq failure t)))
		(t (setq failure t))))
	(setq failure t))))

(defparameter ds*finite.domain nil)

(defun ds-finite.domain.set (clause domain)
  (setq ds*finite.domain (cons clause domain)))

(defun ds-finite.domain.domain ()
  (rest ds*finite.domain))

(defun ds-finite.domain.clause ()
  (first ds*finite.domain))

(defun ds-clause.only.equations (clause)
  (every #'(lambda (lit)
	     (and (dt-predicate.is.equality (ds-lit.predicate lit))
		  (ds-sign.is.positive (ds-lit.sign lit))
		  #|(or (ord-greater (first (ds-lit.termlist lit)) (second (ds-lit.termlist lit)))
		      (ord-greater (second (ds-lit.termlist lit)) (first (ds-lit.termlist lit))))|#))
	 clause))

(defun ds-lit.pos.equation (lit)
						; Edited:  31-OCT-1991 15:13
						; Authors: PRCKLN
						; Input:   Specifies the literal of a clause.
						; Effect:  -
						; Value:   True iff the literal is a positive equation.
  (and (dt-predicate.is.equality (ds-lit.predicate lit))
       (ds-sign.is.positive (ds-lit.sign lit))
       (if (consp (first (ds-lit.termlist lit)))
	   (not (dt-predicate.is.equality (first (first (ds-lit.termlist lit))))))))
		      
(defun ds=rewrite.update (clause)
						; Edited:  02-NOV-1989 15:37
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Recomputes the rewrite properties of CLAUSE
						; Value:   Undefined
  (dt-remprop clause 'ds*rule.litnos)
  (do ((litno (ds-clause.nolit clause) (1- litno)))
      ((= litno 0))
    (ds=clause.put 'max nil clause litno)
    (ds=clause.put 'rule nil clause litno))
  (mapc #'(lambda (litno)
	    (ds-clause.rewrite.rule.set clause litno))
	(ds-clause.compute.max.litno clause)))

(defun ds-rewrite.update (clause)
						; Edited:  17-FEB-1991 14:50
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Recomputes the rewrite properties of CLAUSE
						; Value:   Undefined
  (ds=rewrite.update clause))

(defun ds=clause.put.rule (clause max.litno rule.type rule.side)
						; Edited:  29-JAN-1990 15:32
						; Authors: PRCKLN
						; Input:   A clause, the maximal literal of the clause according
						;          the completion strategy given by the options, DS*RULE or
						;          DS*UNFAIL and 1 or 2
						;          for the specification of the lefthandside of the equation
						; Effect:  Puts properties DS*RULE.LITNOS and DS*CONST.RULE.
						; Value:   Undefined.
  (if (eq rule.type 'ds*rule)
      (ds=clause.put 'rule rule.side clause max.litno)
      (ds=clause.put 'unfail rule.side clause max.litno))
  (unless (member max.litno (dt-getprop clause 'ds*rule.litnos))
    (dt-addprop clause 'ds*rule.litnos max.litno t)))

(defun ds=kz.compute.literal (sign pred termlist)
						; Edited:  29-JAN-1990 15:43
						; Authors: PRCKLN
						; Input:   A sign, predicate and a termlist
						; Effect:  -
						; Value:   The term according to kz-ordering
  (if (opt-is.kz.completion)
      (if (ds-sign.is.positive sign)
	  (if (dt-predicate.is.equality pred)
	      (cons pred termlist)
	      (if (DT-PREDICATE.EQUALITIES)
		  (list (first (DT-PREDICATE.EQUALITIES))
			(cons pred termlist)
			(dt-predicate.true))
		  (cons pred termlist)))
	  (if (DT-PREDICATE.EQUALITIES)
	      (list (first (DT-PREDICATE.EQUALITIES))
		    (cons pred termlist)
		    (dt-predicate.false))
	      (cons pred termlist)))
      (cons pred termlist)))

(defun ds-compute.literal (sign pred termlist)
						; Edited:  29-JAN-1990 15:43
						; Authors: PRCKLN
						; Input:   A sign, predicate and a termlist
						; Effect:  -
						; Value:   The term according to kz-ordering
  (ds=kz.compute.literal sign pred termlist))

(defun ds-clause.literal (clause litno)
  (ds=kz.compute.literal (ds-clause.sign clause litno)
			 (ds-clause.predicate clause litno)
			 (ds-clause.termlist clause litno)))

(defun ds-clause.compute.max.litno (clause)
						; Edited:  29-JAN-1990 15:40
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  -
						; Value:   The numbers of the maximal literals according to
						;          the option specified completion strategy.
  (if (= (ds-clause.nolit clause) 1)
      (list 1)
      (let ((litnos (do* ((litno (1+ (ds-clause.nolit clause)) (1- litno))
			  (litnos nil (cons litno litnos)))
			 ((= 1 litno) litnos))))
	(mapc #'(lambda (litno1)
		  (let ((lit1 (ds=kz.compute.literal (ds-clause.sign clause litno1)
						     (ds-clause.predicate clause litno1)
						     (ds-clause.termlist clause litno1))))
		    (when (some #'(lambda (litno2)
				    (and (/= litno2 litno1)
					 (let ((lit2 (ds=kz.compute.literal (ds-clause.sign clause litno2)
									    (ds-clause.predicate clause litno2)
									    (ds-clause.termlist clause litno2))))
					   (ord-greater lit2 lit1))))
				litnos)
		      (setq litnos (remove litno1 litnos)))))
	      litnos)
	litnos)))

(defun ds=clause.rewrite.rule.set (clause max.litno)
						; Edited:  18-MAR-1989 13:09  
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Declares CLAUSE to be a rewrite rule iff it is directable
						; Value:   Undefined.
  (when (ds-clause.is.equation clause max.litno)
    (if (ord-greater (second (ds-clause.termlist clause max.litno))
		     (first (ds-clause.termlist clause max.litno)))
	(progn (ds=clause.put 'unfail nil clause max.litno)
	       (ds=clause.put 'rule 2 clause max.litno))
	(if (ord-greater (first (ds-clause.termlist clause max.litno))
			 (second (ds-clause.termlist clause max.litno)))
	    (progn (ds=clause.put 'unfail nil clause max.litno)
		   (ds=clause.put 'rule 1 clause max.litno))
	    (when (member (opt-get.option er_completion) '(unfailing constant-congruence))
	      (ds-clause.lit.unfailing.set clause max.litno)))))
  (ds=clause.put 'max t clause max.litno))


(defun ds-clause.rewrite.rule.set (clause lit)
						; Edited:  18-MAR-1989 13:09  
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Declares CLAUSE to be a rewrite rule iff it is directable
						; Value:   Undefined
  (if (> (ds-clause.nolit clause) 0)
      (ds=clause.rewrite.rule.set clause lit)))

(defun ds-clause.irreducible.set (clause lit)
						; Edited:  08-JUN-1990 02:03
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Declares CLAUSE to be a rewrite rule iff it is directable
						; Value:   Undefined
  (if (> (ds-clause.nolit clause) 0)
      (ds-clause.lit.putprop clause lit 'ds*irr t)))

(defun ds-clause.irreducible.is (clause lit)
						; Edited:  08-JUN-1990 02:03
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Declares CLAUSE to be a rewrite rule iff it is directable
						; Value:   Undefined
  (ds-clause.lit.getprop clause lit 'ds*irr))

(defun ds-clause.rewrite.literals (clause)
						; Edited:  16-SEP-1989 00:38
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  -
						; Value:   The rewrite literals of clause
  (dt-getprop clause 'ds*rule.litnos))

(defun ds-clause.lit.rewrite.rule (clause litno)
						; Edited:  16-SEP-1989 00:41
						; Authors: PRCKLN
						; Input:   A clause and a literal number
						; Effect:  -
						; Value:   The term access function for the left hand side of the rule literal
  (ds=clause.get 'rule clause litno))



(defun ds-clause.lit.is.unfailing (clause litno)
						; Edited:  02-OCT-1989 21:18
						; Authors: PRCKLN
						; Input:   A clause and a literal number
						; Effect:  -
						; Value:   True iff literal LITNO of CLAUSE is an undirectable equation
  (ds=clause.get 'unfail clause litno))

(defun ds-clause.lit.unfailing.set (clause litno)
						; Edited:  22-FEB-1990 23:35
						; Authors: PRCKLN
						; Input:   A clause and a literal number
						; Effect:  -
						; Value:   Sets literal LITNO of CLAUSE to be an undirectable equation
  (ds=clause.put 'unfail
		 (if (and (consp (first (ds-clause.termlist clause litno)))
			  (consp (second (ds-clause.termlist clause litno)))
			  (eq (first (first (ds-clause.termlist clause litno)))
			      (first (second (ds-clause.termlist clause litno))))
			  (= 2 (dt-function.arity (first (first (ds-clause.termlist clause litno)))))
			  (equal (rest (first (ds-clause.termlist clause litno)))
				 (reverse (rest (second (ds-clause.termlist clause litno))))))
		     '((1))
		     '((1) (2)))
		 clause
		 litno))

(defun ds-clause.lit.is.max (clause litno)
						; Edited:  16-JAN-1990 19:23
						; Authors: PRCKLN
						; Input:   A clause and a literal number
						; Effect:  -
						; Value:   True iff literal LITNO of CLAUSE is an undirectable equation
  (ds=clause.get 'max clause litno))

(defun ds-clause.lit.set.max (clause litno)
						; Edited:  16-JAN-1990 19:23
						; Authors: PRCKLN
						; Input:   A clause and a literal number
						; Effect:  -
						; Value:   True iff literal LITNO of CLAUSE is an undirectable equation
  (ds=clause.put 'max t clause litno))

(defun ds-clause.passive.positions (clause litno)
						; Edited:  20-NOV-1991 18:54
						; Authors: PRCKLN
						; Input:   Specifies a literal in a clause
						; Effect:  -
						; Value:   The positions declared passive.
  (ds-clause.lit.getprop clause litno 'ds*passive.positions))

(defun ds-clause.put.passive.positions (clause litno pos)
						; Edited:  20-NOV-1991 18:54
						; Authors: PRCKLN
						; Input:   Specifies a literal in a clause.
						;          POS is a list of positions of this literal.
						; Effect:  Sets the passive positions component with POS
						; Value:   Undefined
  (ds-clause.lit.putprop clause litno 'ds*passive.positions pos))

(defun ds-clause.reset.rewrite.rule (clause litno)
						; Edited:  "18-MAR-1989 12:25"
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  Declare CLAUSE to be not longer a rewrite rule
						; Value:   Undefined
  (ds=clause.put 'rule nil clause litno)
  (ds=clause.put 'unfail nil clause litno))






(defmacro ds=clause.lit.reset.linkcolours (clause litno)
						; Edited:  18-MAR-1989 12:29
						; Authors: PRCKLN
						; Input:   A clause and a literal number for this clause
						; Effect:  Resets all linkcolours of CLAUSE . LITNO to nil
						; Value:   Undefined
  `(progn ,@(MAPCar #'(LAMBDA (LINKCOLOUR) `(DS=CLAUSE.PUT ',LINKCOLOUR NIL ,CLAUSE ,LITNO)) DS*LINK.LITERALS)))

(DEFMACRO DS-CLAUSE.PNAME (CLAUSE)
						; EDITED: "12-NOV-79 14:47:49")
						; VALUE: PNAME OF CLAUSE SYMBOL
  `(DS=CLAUSE.GET 'PNAME ,CLAUSE))

(DEFMACRO DS-CLAUSE.LIT.GETPROP (CLAUSE LITNO INDICATOR)
						; EDITED: "12-NOV-79 14:52:13"
						; EFFECT AND VALUE ANALOGOUS TO INTERLISP FUNCTION GETP 
  `(GETf (DS=CLAUSE.GET 'LIT.PROPLIST ,CLAUSE ,LITNO) ,INDICATOR))

(DEFMACRO DS-CLAUSE.LIT.PUTPROP (CLAUSE LITNO INDICATOR VALUE)
						; EDITED: "12-NOV-79 14:54:45"
						; EFFECT AND VALUE ANALOGOUS TO INTERLISP FUNCTION PUT
  `(setf (getf (DS=CLAUSE.GET 'LIT.PROPLIST ,CLAUSE ,LITNO) ,indicator)
	 ,value))

(DEFUN DS-CLAUSE.LIT.REMPROP (CLAUSE LITNO INDICATOR)
						; EDITED: "12-NOV-79 14:56:12"
						; EFFECT AND VALUE ANALOGOUS TO INTERLISP FUNCTION REMPROP
  (remf (DS=CLAUSE.GET 'LIT.PROPLIST CLAUSE LITNO) indicator))

(DEFUN DS-CLAUSE.LIT.REMPROPS (CLAUSE LITNO INDICATORS)
						; EDITED:  12-NOV-79 15:01:01"
						; Effect:  THE INDICATORS ARE REMOVED FROM THE PROPERTY LIST.
						;          IF INDICATORS=NIL THE WHOLE LIST IS CLEARED
						; VALUE:   UNDEFINED
  (COND ((NULL INDICATORS)
	 (DS=CLAUSE.PUT 'LIT.PROPLIST NIL CLAUSE LITNO))
	(T (MAPC #'(LAMBDA (INDICATOR) (DS-CLAUSE.LIT.REMPROP clause litno indicator)) INDICATORS))))

(DEFMACRO DS-CLAUSE.LIT.GETPROPLIST (CLAUSE LITNO)
						; EDITED: 16-MAR-83 12:38:06        NE
						; INPUT:  A CLAUSE AND A LITERAL NUMBER IN CLAUSE
						; VALUE:  ENTIRE PROPERTY LIST OF THE RESP. LITERAL.
  `(DS=CLAUSE.GET 'LIT.PROPLIST ,CLAUSE ,LITNO))

(DEFMACRO DS-CLAUSE.ALL.LIT.REMPROP (CLAUSE INDICATOR)
						; EDITED:  4. 5. 1982   HJO
						; INPUT:   A CLAUSE ADDRESS AND AN ATOM.
						; EFFECT:  REMOVES ALL LITERAL PROPERTIES DENOTED BY
						;          INDICATOR.
						; VALUE:   UNDEFINED.
  `(let ((clause ,clause))
     (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE)) (DS-CLAUSE.LIT.REMPROP CLAUSE (1+ RPTN) ,INDICATOR))))

(DEFUN DS-CLAUSE.ALL.LIT.REMPROPS (CLAUSE INDICATORS)
						; EDITED:  4. 5. 1982   HJO
						; INPUT:   A CLAUSE ADDRESS AND NIL OR AN LIST.
						; EFFECT:  IF INDICATOR = NIL, ALL LITERAL PROPERTIES
						;          WILL BE REMOVED, ELSE ONLY THOSE DENOTED BY
						;          INDICATORS.
						; VALUE:   UNDEFINED.
  (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE)) (DS-CLAUSE.LIT.REMPROPS CLAUSE (1+ RPTN) INDICATORS)))

(DEFmacro DS-CLAUSE.PARENTS (CLAUSE)
						; EDITED: "13-NOV-79 17:57:35"
						; VALUE: PARENTS OF CLAUSE, EITHER NIL, ATOM, OR DOTTED PAIR
  `(DS=CLAUSE.GET 'PARENTS ,CLAUSE))

(DEFmacro DS-CLAUSE.put.PARENTS (CLAUSE parents)
						; EDITED: "13-NOV-79 17:57:35"
						; VALUE: PARENTS OF CLAUSE, EITHER NIL, ATOM, OR DOTTED PAIR
  `(DS=CLAUSE.put 'PARENTS ,parents ,CLAUSE))

(DEFUN DS-CLAUSE.ANCESTORS (CLAUSE)
						; EDITED: " 2-FEB-82 13:17:16"
						; EDITED: 13. 1. 1982    HJO
						; INPUT:  A CLAUSE
						; VALUE:  A LIST OF ALL STILL EXISTING ANCESTORS OF
						;         CLAUSE INCLUDING CLAUSE ITSELF AT FIRST
						;         POSITION.
  (if (DS=CLAUSE.IS CLAUSE)
      (let ((PARENTS (DS=CLAUSE.GET 'PARENTS CLAUSE)))
	(if (or (NULL PARENTS) (ATOM PARENTS))
	    NIL
	    (delete-DUPLICATES (cons clause (MAPCAN #'DS-CLAUSE.ANCESTORs PARENTS)))))
      NIL))

(DEFMACRO DS-CLAUSE.DEPTH (CLAUSE)
						; EDITED: "13-NOV-79 17:58:23"
						; VALUE: DEPTH OF CLAUSE
  `(DS=CLAUSE.GET 'DEPTH ,CLAUSE))

(DEFMACRO DS-CLAUSE.VARIABLES (CLAUSE)
						; EDITED: "13-NOV-79 17:58:59"
						; VALUE: VARIABLES OF CLAUSE
  `(DS=CLAUSE.GET 'VARIABLES ,CLAUSE))

(DEFMACRO DS-CLAUSE.RENAMING (CLAUSE)
						; EDITED: 29-SEP-83 21:20:40                     AP
						; INPUT:  A CLAUSE ADDRESS.
						; EFFECT:  -
						; VALUE:  RENAMING COMPONENT OF CLAUSE, E.G. (VAR1
						;         VAR1' ... VARN VARN'), WHERE VARI ARE THE
						;         VARIABLES OF CLAUSE AND VARI' THE
						;         CRRESPONDING RENAMED ONES.
  `(DS=CLAUSE.GET 'RENAMING ,CLAUSE NIL))

(DEFMACRO DS-CLAUSE.NOLIT (CLAUSE)
						; EDITED: "13-NOV-79 18:00:29")
						; VALUE: NUMBER OF LITERALS OF CLAUSE *)
  `(DS=CLAUSE.GET 'NOLIT ,CLAUSE))

(DEFMACRO DS-CLAUSE.ATTRIBUTES (CLAUSE)
						; EDITED:  14.6.1982    KHB
						; INPUT:   A CLAUSE ADDRESS.
						; VALUE:   THE ATTRIBUTE-LIST OF CLAUSE.
  `(DS=CLAUSE.GET 'ATTRIBUTES ,CLAUSE))

(DEFUN DS-CLAUSE.ADD.ATTRIBUTES (CLAUSE ATTRIBUTES)
						; EDITED:  14.6.1982    KHB
						; INPUT:   A CLAUSE ADDRESS AND A LIST OF ATTRIBUTE-
						;          VALUES.
						; EFFECT:  THE ATTRIBUTE-VALUES ARE ADDED TO THE
						;          ATTRIBUTE-LIST (MEMORY ARRAY ELEMENT)
						;          OF CLAUSE.
						; VALUE:   UNDEFINED.
  (DS=CLAUSE.PUT 'ATTRIBUTES (APPEND ATTRIBUTES (copy-list (DS=CLAUSE.GET 'ATTRIBUTES CLAUSE))) CLAUSE)
  (when (OR (MEMBER 'REFLEXIVITY ATTRIBUTES) (MEMBER 'IRREFLEXIVITY ATTRIBUTES))
    (if (DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE 1))
	(MAPC #'(LAMBDA (PREDICATE) (DT-PREDICATE.PUT 'REFL.CLAUSE PREDICATE CLAUSE)) (DT-PREDICATE.EQUALITIES))
	(DT-PREDICATE.PUT 'REFL.CLAUSE (DS-CLAUSE.PREDICATE CLAUSE 1) CLAUSE))))

(DEFMACRO DS-CLAUSE.POTENTIALLY.FALSE.LITNOS (CLAUSE)
						; EDITED:24-AUG-83                              AP
						; INPUT: A CLAUSE ADDRESS.
						; EFFECT: -
						; VALUE: A LIST OF THE LITERAL NUMBERS OF CLAUSE
						;        'CLAUSE', WHICH CAN BECOME POTENTIALLY FALSE
						;        BY A SUBSTITUTION, FOR EXAMPLE X<A (X<-A).
  `(DS=CLAUSE.GET 'POT.FALSE.LITNOS ,CLAUSE NIL))

(DEFMACRO DS-CLAUSE.PUT.POTENTIALLY.FALSE.LITNOS (CLAUSE LITNOS)
						; EDITED:24-AUG-83                              AP
						; INPUT: A CLAUSE ADDRESS 'CLAUSE' AND
						;        'LITNOS', A LIST OF THE NUMBERS OF THE
						;        LITERALS THAT CAN BECOME POTENTIALLY FALSE.
						; EFFECT:THE OLD POTENTIALLY FALSE LITNOS COMPONENT
						;        WILL BE REPLACED BY 'LITNOS'.
						; VALUE: 'LITNOS'.
  `(let ((litnos ,litnos))
     (DS=CLAUSE.PUT 'POT.FALSE.LITNOS LITNOS ,CLAUSE NIL) LITNOS))

(DEFMACRO DS-CLAUSE.POTENTIALLY.TRUE.LITNOS (CLAUSE)
						; EDITED:24-AUG-83                              AP
						; INPUT: A CLAUSE ADDRESS.
						; EFFECT: -
						; VALUE: A LIST OF THE LITERAL NUMBERS OF CLAUSE
						;        'CLAUSE', WHICH CAN BECOME POTENTIALLY TRUE
						;        BY A SUBSTITUTION, FOR EXAMPLE X=A (X<-A).
  `(DS=CLAUSE.GET 'POT.TRUE.LITNOS ,CLAUSE NIL))

(DEFUN DS-CLAUSE.PUT.POTENTIALLY.TRUE.LITNOS (CLAUSE LITNOS)
						; EDITED:24-AUG-83                              AP
						; INPUT: A CLAUSE ADDRESS 'CLAUSE' AND
						;        'LITNOS', A LIST OF THE NUMBERS OF THE
						;        LITERALS THAT CAN BECOME POTENTIALLY TRUE.
						; EFFECT:THE OLD POTENTIALLY TRUE  LITNOS COMPONENT
						;        WILL BE REPLACED BY 'LITNOS'.
						; VALUE: 'LITNOS'.
  (DS=CLAUSE.PUT 'POT.TRUE.LITNOS LITNOS CLAUSE NIL) LITNOS)

(DEFMACRO DS-CLAUSE.SIGN (CLAUSE LITNO)
						; EDITED: "13-NOV-79 18:02:07")
						; VALUE: SIGN OF LITNO-TH LITERAL OF CLAUSE *)
  `(DS=CLAUSE.GET 'SIGN ,CLAUSE ,LITNO))

(DEFMACRO DS-CLAUSE.PREDICATE (CLAUSE LITNO)
						; EDITED: "13-NOV-79 18:03:30")
						; VALUE: PREDICATE OF LITNO-TH LITERAL OF CLAUSE *)
  `(DS=CLAUSE.GET 'PREDICATE ,CLAUSE ,LITNO))

(DEFUN DS-CLAUSE.POS.PREDICATES (CLAUSE)
						; EDITED:  14. 5. 1982   HJO
						; INPUT:   A CLAUSE ADDRESS
						; VALUE:   A LIST OF PREDICATES OCCURING IN CLAUSE
						;          WITH POSITIVE SIGN.
  (let ((PREDICATES NIL))
    (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
      (COND
	((DS-SIGN.IS.POSITIVE (DS=CLAUSE.GET 'SIGN CLAUSE (1+ RPTN)))
	 (SETQ PREDICATES (INS (DS=CLAUSE.GET 'PREDICATE CLAUSE (1+ RPTN)) PREDICATES)))))
    PREDICATES))



(DEFUN DS-CLAUSE.ALL.PREDICATES (CLAUSE)
						; EDITED:  14. 5. 1982   HJO
						; INPUT:   A CLAUSE ADDRESS
						; VALUE:   A LIST OF PREDICATES OCCURING IN CLAUSE.
  (let ((PREDICATES NIL))
    (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
      (SETQ PREDICATES (INS (DS=CLAUSE.GET 'PREDICATE CLAUSE (1+ RPTN)) PREDICATES)))
    PREDICATES))

(DEFUN DS-CLAUSE.PREDICATE.OCCURRENCES (CLAUSE PREDICATE SIGN EQ.SEPARATELY)
						; EDITED: 24-JUN-83 11:16:51
						; INPUT:  A CLAUSE, A PREDICATE AND A SIGN SYMBOL
						;         AND A BOOLEAN VALUE.
						; VALUE:  A LIST OF ALL LITERAL NUMBERS WHERE
						;         PREDICATE OCCURS WITH THE APPROPRIATE SIGN
						;         IN CLAUSE.
						;         IF EQ.SEPARATELY = NIL, THE EQUALITY
						;         PREDICATES WILL BE TREATED EQUALLY.
  (let (LITNOS)
    (if (AND (DT-PREDICATE.IS.EQUALITY PREDICATE) (NOT EQ.SEPARATELY))
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (COND
	    ((AND (DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)))
		  (DS-SIGN.ARE.EQUAL SIGN (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))))
	     (SETQ LITNOS (CONS (1+ RPTN) LITNOS)))))
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (COND
	    ((AND (EQL PREDICATE (DS-CLAUSE.PREDICATE CLAUSE (1+ RPTN)))
		  (DS-SIGN.ARE.EQUAL SIGN (DS-CLAUSE.SIGN CLAUSE (1+ RPTN))))
	     (SETQ LITNOS (CONS (1+ RPTN) LITNOS))))))
    LITNOS))

(DEFMACRO DS-CLAUSE.TERMLIST (CLAUSE LITNO)
						; EDITED: "13-NOV-79 18:04:01")
						; VALUE: TERMLIST OF LITNO-TH LITERAL OF CLAUSE *)
  `(DS=CLAUSE.GET 'TERMLIST ,CLAUSE ,LITNO))

(DEFUN DS-CLAUSE.REPLACE.LITERAL (CLAUSE LITNO SIGN PREDICATE TERMLIST)
						; EDITED: 15-APR-83 13:14:19        NE
						; INPUT:  A CLAUSE, A LITERAL NUMBER OF CLAUSE,
						;         A SIGN, A PREDICATE,
						;         AND A LIST OF TERMS.
						; EFFECT: REPLACES SIGN, PREDICATE AND TERMLIST OF
						;         THE LITNO-TH
						;         LITERAL OF CLAUSE BY 'SIGN', 'PREDICATE'
						;         AND 'NEWTERMLIST'.
						;         VARIABLE AND RENAMING LISTS ARE UPDATED.
						; VALUE:  UNDEFINED.
  (let ((oldvars (ds=clause.get 'variables clause litno))
	VARIABLES newclausevars DELVARS NEWVARS (DELFLAG NIL))
    (SETQ VARIABLES (DS=FIND.VARIABLES TERMLIST NIL))
    (SETQ NEWVARS (SET-DIFFERENCE (copy-list VARIABLES) (DS-CLAUSE.LIT.VARIABLES CLAUSE LITNO)))
    (DS=CLAUSE.PUT 'PREDICATE PREDICATE CLAUSE LITNO)
    (DS=CLAUSE.PUT 'SIGN SIGN CLAUSE LITNO)
    (DS=CLAUSE.PUT 'TERMLIST TERMLIST CLAUSE LITNO)
    (DS=CLAUSE.PUT 'LIT.VARIABLES VARIABLES CLAUSE LITNO)
    (SETQ newclausevars NIL)
    (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
      (SETQ newclausevars (UNION (copy-list (DS=CLAUSE.GET 'LIT.VARIABLES CLAUSE (1+ RPTN))) newclausevars)))
    (DS=CLAUSE.PUT 'VARIABLES newclausevars CLAUSE)
    (SETQ DELVARS (SET-DIFFERENCE oldvars newclausevars))
    (DS=CLAUSE.PUT 'RENAMING
		   (NCONC (ZIP NEWVARS (MAPCAR #'(LAMBDA (VAR) (DT-VARIABLE.CREATE (DT-VARIABLE.SORT VAR))) NEWVARS))
			  (DREMAP (DS=CLAUSE.GET 'RENAMING CLAUSE NIL)
				  NIL
				  #'(LAMBDA (TAIL.VARS)
				      (COND (DELFLAG (SETQ DELFLAG NIL)
						     (DT-VARIABLE.DELETE (CAR TAIL.VARS))
						     T)
					    ((MEMBER (CAR TAIL.VARS) DELVARS)
					     (SETQ DELFLAG T))))
				  NIL))
		   CLAUSE NIL)
    (let ((SIGN.IS.POSITIVE (DS-SIGN.IS.POSITIVE sign)))
      (COND ((OR (AND (NOT SIGN.IS.POSITIVE) (DT-PREDICATE.IS.MARKED REFLEXIVE PREDICATE))
		 (AND (DT-PREDICATE.IS.MARKED IRREFLEXIVE PREDICATE) SIGN.IS.POSITIVE))
	     (ds=clause.put 'pot.false.litnos (adjoin litno (ds=clause.get 'pot.false.litnos clause litno)) clause litno))
	    ((OR (AND (DT-PREDICATE.IS.MARKED REFLEXIVE PREDICATE) SIGN.IS.POSITIVE)
		 (AND (NOT SIGN.IS.POSITIVE) (DT-PREDICATE.IS.MARKED IRREFLEXIVE PREDICATE)))
	     (ds=clause.put 'pot.true.litnos (adjoin litno (ds=clause.get 'pot.true.litnos clause litno)) clause litno))))
    (when (opt-is.completion) (ds=rewrite.update clause))))

(DEFMACRO DS-CLAUSE.LIT.VARIABLES (CLAUSE LITNO)
						; EDITED: 11-FEB-83 15:52:46
						; INPUT:  A CLAUSE ADDRESS AND A LITERALNUMBER
						; VALUE:  THE VARIABLES OCCURING IN LITNO'S LITERAL.
  `(DS=CLAUSE.GET 'LIT.VARIABLES ,CLAUSE ,LITNO))

(DEFMACRO DS-CLAUSE.LINKS (COLOUR.S CLAUSE LITNO)
						; INPUT : A CLAUSE,A LITERALNUMBER AND A LINKCOLOUR
						;         OR A LIST OF LINK COLOURS.
						; VALUE : THE LINKS OF THE LITNO'TH LITERAL OF
						;         CLAUSE WITH LINKCOLOUR COLOUR
  (setq colour.s (macroexpand colour.s))
  (if (AND (CONSP COLOUR.S)
	   (EQL (CAR COLOUR.S) 'QUOTE))
      (if (ATOM (SECOND COLOUR.S))
	  `(DS=CLAUSE.GET ,COLOUR.S ,CLAUSE ,LITNO)
	  `(nconc ,@(MAPCar #'(LAMBDA (LINKCOLOUR) `(COPY-list (DS=CLAUSE.GET ',LINKCOLOUR ,CLAUSE ,LITNO)))
			    (second COLOUR.S))))
      `(let ((CLAUSE ,clause)
	     (LITNO ,litno)
	     (COLOUR.S ,colour.s))
	 (if (LISTP COLOUR.S)
	     (MAPCan #'(LAMBDA (LINKCOLOUR) (COPY-tree (DS=CLAUSE.GET LINKCOLOUR CLAUSE LITNO)))
		     COLOUR.S)
	     (DS=CLAUSE.GET COLOUR.S CLAUSE LITNO)))))


(DEFUN DS-CLAUSE.LINKS.THISSIDE (COLOUR.S CLAUSE LITNO TAF)
						; INPUT : COLOUR.S:A COLOUR OR A LIST OF COLOURS
						;         CLAUSE   LITNO   TAF
						; VALUE : LIST OF ALL LINKS ON(CLAUSE LITNO) WITH
						;         COLOUR FROM COLOUR.S,WHOSE THISFCT IS THE
						;         OPPOSITE SIDE OF TAF
  (REMOVE-IF-NOT
    #'(LAMBDA (LINK)
	(COND ((DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE LITNO))
	       (AND (NOT (DT-TAF.DIFFERENT.SIDES TAF (DS-LINK.THISFCT LINK CLAUSE LITNO TAF)))
		    (OR (NEQ (DS-LINK.NEGPAR LINK) (DS-LINK.POSPAR LINK)) (NEQ (DS-LINK.NEGLITNO LINK) (DS-LINK.POSLITNO LINK))
			(NOT (DT-TAF.DIFFERENT.SIDES (DS-LINK.NEGFCT LINK) (DS-LINK.POSFCT LINK))))))
	      (T T)))
    (DS-CLAUSE.LINKS COLOUR.S CLAUSE LITNO)))

(DEFUN DS-CLAUSE.LINKS.OTHERSIDE (COLOUR.S CLAUSE LITNO TAF)
						; INPUT : COLOUR.S:A COLOUR OR A LIST OF COLOURS
						;         CLAUSE   LITNO   TAF
						;         (CLAUSE LITNO) IS A TWO-PLACE-LITERAL
						;        TAF DENOTES ONE SIDE OF THIS LITERAL
						; VALUE : THE LIST OF ALL LINKS CONNECTING THE OTHER
						;         SIDE OF (CLAUSE LITNO) AND HAVING A COLOUR
						;         FROM COLOUR.S,EXCEPT FOR INTERNAL LINKS FROM
						;         ONE SIDE TO THE OTHER
  (REMOVE-IF-NOT #'(LAMBDA (LINK) (DT-TAF.DIFFERENT.SIDES TAF (DS-LINK.THISFCT LINK CLAUSE LITNO TAF)))
		 (DS-CLAUSE.LINKS COLOUR.S CLAUSE LITNO)))



(DEFUN DS-CLAUSE.LIT (CLAUSE LITNO)
						; EDITED: "13-NOV-79 18:08:54"
						; VALUE: Litno-th literal of clause
  (DS=LIT.STORAGE (DS=CLAUSE.GET 'SIGN CLAUSE LITNO)
		  (DS=CLAUSE.GET 'PREDICATE CLAUSE LITNO)
		  (DS=CLAUSE.GET 'TERMLIST CLAUSE LITNO)))



(DEFUN DS-CLAUSE.ALL.LINKS (COLOUR.S CLAUSE)
						; INPUT : LINKCOLOUR OR A LIST OF COLOURS, A CLAUSE
						; VALUE : THE LIST OF ALL LINKS WITH LINKCOLOUR
						;         COLOUR CONNECTED TO CLAUSE
  (COND ((ATOM COLOUR.S) (SETQ COLOUR.S (LIST COLOUR.S))))
  (MAPCAN #'(LAMBDA (LINKCOLOUR)
	      (COND ((MEMBER LINKCOLOUR DS*LINK.CLAUSE) (COPY-list (DS=CLAUSE.GET LINKCOLOUR CLAUSE)))
		    ((MEMBER LINKCOLOUR DS*LINK.AUTOLINKS)
		     (let (LIST)
		       (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
			 (SETQ LIST (UNION (DS=CLAUSE.GET LINKCOLOUR CLAUSE (1+ RPTN)) LIST)))
		       (copy-list list)))
		    (T (let (LIST)
			 (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
			   (SETQ LIST (APPEND (DS=CLAUSE.GET LINKCOLOUR CLAUSE (1+ RPTN)) LIST)))
			 (copy-list LIST)))))
	  COLOUR.S))

(DEFUN DS-CLAUSE.IS.EQUATION (CLAUSE LITNO)
						; EDITED AT 6-OCT-81 11:08)
						; VALUE:  T, IF CLAUSE AND LITNO DENOTE AN
						;         EQUALITY, WITH POSITIVE SIGN, ELSE NIL
  (AND (DS-SIGN.IS.POSITIVE (DS-CLAUSE.SIGN CLAUSE LITNO))
       (DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE LITNO))))

(defun ds-clause.is.unit (clause)
						; Edited:  14-DEC-1991 14:01
						; Authors: PRCKLN
						; Input:   A clause.
						; Effect:  -
						; Value:   True iff CLAUSE is a unit, i.e. has exactly one literal.
  (= 1 (ds=clause.get 'nolit clause)))

(DEFUN DS-CLAUSE.IS.PURE (CLAUSE)
						; EDITED: "27-NOV-80 10:38:13")
						; VALUE: T IF CLAUSE HAS A PURE LITERAL, ELSE NIL
  (do* ((nolit (DS=CLAUSE.GET 'NOLIT CLAUSE) (1- nolit))
	(condition (and (NOTANY #'(LAMBDA (LINKCOLOUR) (DS=CLAUSE.GET LINKCOLOUR CLAUSE nolit)) DS*LINK.PURITY)
			(not (opt-get.option sort_literals)))
		   (and (NOTANY #'(LAMBDA (LINKCOLOUR) (DS=CLAUSE.GET LINKCOLOUR CLAUSE nolit)) DS*LINK.PURITY)
			(not (opt-get.option sort_literals)))))
       ((or condition (= 1 nolit)) condition)))

(DEFMACRO DS-CLAUSE.IS.PREFERRED (CLAUSE LITNO)
						; EDITED: " 1-FEB-82 16:32:55")
  `(MEMBER (DS=CLAUSE.GET 'SIGN ,CLAUSE ,LITNO) '(++ --)))

(DEFUN DS-CLAUSE.IS.HORN (CLAUSE)
						; EDITED: "13-NOV-79 18:29:04")
						; VALUE: T IF CLAUSE HAS HORN FORM, ELSE NIL *)
  (let  ((POSLITS 0))
    (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
      (COND ((MEMBER (DS=CLAUSE.GET 'SIGN CLAUSE (1+ RPTN)) DS*SIGN.PLUS.SYMBOLS)
	     (SETQ POSLITS (1+ POSLITS)))))
    (< POSLITS 2)))

(DEFMACRO DS-CLAUSE.IS (CLAUSE)
						; EDITED: "12-NOV-79 15:03:21")
						; VALUE: T IF CLAUSE IS A CLAUSE SYMBOL NOT YET DELETED, ELSE NIL *)
  `(DS=CLAUSE.IS ,CLAUSE))

(DEFUN DS-CLAUSE.REMOVE.LITERAL (CLAUSE LITNO)
						; EDITED: "29-APR-82 12:33:42")
						; INPUT:  ADDRESS OF A CLAUSE , LITERAL NUMBER
						; EFFECT: REMOVES THE LITNO.TH LITERAL FROM CLAUSE
						;         UPDATES THE LITERAL NUMBERS IN THE LINKS OF
						;         THE LITERALS WITH GREATER NUMBER THAN
						;         'LITNO'.
						; VALUE:  UNDEFINED
						; REMARK: DELETION IS ONLY POSSIBLE BEFORE LINKS
						;         ARE CREATED
  (COND
    ((MEMBER-IF #'(LAMBDA (LINKCOLOUR) (DS=CLAUSE.GET LINKCOLOUR CLAUSE LITNO)) DS*LINK.LITERALS)
     (cERROR "Illegal deleting of literal in clause. Linklist not empty in DS-CLAUSE.REMOVE.LITERAL clause = ~A, litno = ~A"
	     CLAUSE LITNO))
    (T (let ((NOLIT (DS=CLAUSE.GET 'NOLIT CLAUSE)) (TO (+ DS*CLAUSE.COMMON.CELLS (* DS*CLAUSE.LITERAL.CELLS (1- LITNO))))
	     FROM VARIABLES NEWNOLIT)
	 (SETQ NEWNOLIT (1- NOLIT)) (SETQ FROM (+ DS*CLAUSE.LITERAL.CELLS TO))
						;  SHIFT STORAGE
	 (DODOWN (RPTN (* DS*CLAUSE.LITERAL.CELLS (- NOLIT LITNO)))
	   (SETQ TO (1+ TO))
	   (SETQ FROM (1+ FROM))
	   (MEM-PUT CLAUSE TO (MEM-GET CLAUSE FROM)))
	 (DS=CLAUSE.PUT 'NOLIT NEWNOLIT CLAUSE)
	 (MEM-SHORTEN CLAUSE DS*CLAUSE.LITERAL.CELLS)
	 (DODOWN (RPTN NEWNOLIT)
	   (SETQ VARIABLES (UNION (DS=CLAUSE.GET 'LIT.VARIABLES CLAUSE (1+ RPTN)) VARIABLES)))
	 (MAPC #'(LAMBDA (VAR) (MEM-ERASE VAR NIL)) (SET-DIFFERENCE (DS=CLAUSE.GET 'VARIABLES CLAUSE) VARIABLES))
	 (DS=CLAUSE.PUT 'VARIABLES VARIABLES CLAUSE)
	 (PROG ((RENAMING (DS=CLAUSE.GET 'RENAMING CLAUSE NIL)))
	       (SMAPL #'(LAMBDA (TAIL)
			  (COND ((NOT (MEMBER (CAR TAIL) (DS=CLAUSE.GET 'VARIABLES CLAUSE NIL)))
				 (MEM-ERASE (SECOND TAIL) NIL) (RPLACA TAIL NIL)
				 (RPLACA (CDR TAIL) NIL))))
		      #'CDDR RENAMING)
	       (DS=CLAUSE.PUT 'RENAMING (DELETE NIL RENAMING) CLAUSE NIL))
						; ACTUALIZING COMPONENTS POTENTIALLY TRUE AND FALSE
						; LITNOS.
	 (MAPC
	   #'(LAMBDA (COMPONENT)
	       (DS=CLAUSE.PUT COMPONENT
			      (MAPCAN
				#'(LAMBDA (TRUE.FALSE)
				    (COND ((< LITNO TRUE.FALSE) (LIST (1- TRUE.FALSE)))
					  ((EQL LITNO TRUE.FALSE) NIL) (T (LIST TRUE.FALSE))))
				(DS=CLAUSE.GET COMPONENT CLAUSE NIL))
			      CLAUSE NIL))
	   '(POT.TRUE.LITNOS POT.FALSE.LITNOS))
	 (MAPC
	   #'(LAMBDA (LINKCOLOUR)
	       (MAPC
		 #'(LAMBDA (LINK)
		     (let ((LITNUMBER (DS=LINK.GET 'POSLITNO LINK)))
		       (COND ((> LITNUMBER LITNO) (DS=LINK.PUT 'POSLITNO LINK (1- LITNUMBER))))
		       (SETQ LITNUMBER (DS=LINK.GET 'NEGLITNO LINK))
		       (COND ((> LITNUMBER LITNO) (DS=LINK.PUT 'NEGLITNO LINK (1- LITNUMBER))))))
		 (DS-CLAUSE.ALL.LINKS LINKCOLOUR CLAUSE)))
	   DS*LINK.AUTOLINKS)
	 (DODOWN (RPTN (- NOLIT LITNO))
	   (let ((LITNUMBER (+ LITNO RPTN)))
	     (MAPC #'(LAMBDA (LINKCOLOUR)
		       (MAPC #'(LAMBDA (LINK)
				 (if (EQL CLAUSE (DS=LINK.GET 'POSPAR LINK))
				     (DS=LINK.PUT 'POSLITNO LINK LITNUMBER)
				     (DS=LINK.PUT 'NEGLITNO LINK LITNUMBER)))
			     (DS=CLAUSE.GET LINKCOLOUR CLAUSE LITNUMBER)))
		   DS*LINK.NOT.AUTOLINKS)))
	 (when (opt-is.completion) (ds=rewrite.update clause))))))

(DEFUN DS-CLAUSE.ADMISSIBLE.SORT (CLAUSE LITNO TAF)
						; EDITED: 11-FEB-83 15:12:06
						; INPUT:  A CLAUSE ADDRESS, A LITERALNUMBER AND
						;         A TERM ACCESS FUNCTION.
						; VALUE:  THE MAXIMAL ADMISSIBLE SORT OF THE
						;         CLAUSE,LITNO'S SUBTERM ACCESSED BY THE TAF.
  (COND ((CDR TAF)
	 (let (TERM LAST)
	   (SETQ TAF (REVERSE TAF))
	   (SETQ LAST (CAR TAF))
	   (SETQ TERM (DT-ACCESS (NREVERSE (CDR TAF)) (DS-CLAUSE.TERMLIST CLAUSE LITNO)))
	   (NTH (1- LAST) (DT-FUNCTION.DOMAINSORTS (CAR TERM)))))
	(T (NTH (1- (CAR TAF)) (DT-PREDICATE.DOMAINSORTS (DS-CLAUSE.PREDICATE CLAUSE LITNO))))))

(DEFUN DS-CLAUSE.DELETE (CLAUSE)
						; EDITED: "29-APR-82 12:32:40"
						; VALUE: UNDEFINED
  (COND
    ((INTERSECTION (DS=CLAUSE.GET 'ATTRIBUTES CLAUSE) '(REFLEXIVITY IRREFLEXIVITY))
     (COND
       ((DT-PREDICATE.IS.EQUALITY (DS-CLAUSE.PREDICATE CLAUSE 1))
	(MAPC #'(LAMBDA (PREDICATE) (DT-PREDICATE.PUT 'REFL.CLAUSE PREDICATE NIL)) (DT-PREDICATE.EQUALITIES)))
       (T (DT-PREDICATE.PUT 'REFL.CLAUSE (DS-CLAUSE.PREDICATE CLAUSE 1) NIL)))))
  (MAPC #'(LAMBDA (VAR) (MEM-ERASE VAR NIL)) (DS=CLAUSE.GET 'RENAMING CLAUSE)) (MEM-ERASE CLAUSE NIL))

(DEFUN DS-CLAUSE.ONE.LIT.UNIFIER (CLAUSE LITNO UNIFIER)
						; EDITED:  27-NOV-80 11:15:00
						; INPUT:  A CLAUSE, LITERALNUMBER, UNIFIER
						; VALUE:  T IF THE UNIFIER SUBSTITUTES ONLY INTO
						;         THE GIVEN LITERAL, ELSE NIL.
  (let ((SUBSTITUTED.VARIABLES (SMAPCAR #'(lambda (x) `(QUOTE ,x)) (FUNCTION CDDR) UNIFIER)))
    (DODOWN (RPTN (DS=CLAUSE.GET 'NOLIT CLAUSE))
      (if (EQL LITNO (1+ RPTN))
	  t
	  (if (INTERSECTION (DS=CLAUSE.GET 'LIT.VARIABLES CLAUSE (1+ RPTN)) SUBSTITUTED.VARIABLES)
	      (progn (SETQ RPTN -1) NIL)
	      T)))))

(DEFUN DS-CLAUSE.FCTSTACK (CLAUSE LITNO TAF)
						; EDITED: 26-OCT-83 11:13:15
						; INPUT:  A CLAUSE, A LITERALNUMBER AND A TAF
						; VALUE:  THE LIST OF FUNCTIONSYMBOLS OF THE SUPER-
						;         TERMS OF TERM GIVEN BY CLAUSE LITNO AND TAF
  (let (TERMLIST FS TERM) (SETQ TERMLIST (DS-CLAUSE.TERMLIST CLAUSE LITNO))
       (MAPC
	 #'(LAMBDA (ARG)
	     (SETQ TERM (CAR (NTHCDR (1- ARG) TERMLIST)))
	     (SETQ FS (CONS (CAR TERM) FS)) (SETQ TERMLIST (CDR TERM)))
	 (NREVERSE (CDR (REVERSE TAF))))
       FS))



(DEFUN DS-CLAUSE.VAR.OCCUR.IN.LITERAL (CLAUSE LITNO VARIABLE)
						; EDITED:  5-MAY-83 18:03:25
						; INPUT:   A CLAUSE, A LITERALNUMBER AND A VARIABLE
						; VALUE:   A LIST OF TERM ACCESS FUNCTIONS DENOTING
						;          THE OCCURRENCES OF VARIABLE IN THE
						;          LITNO'TH LITERAL IN CLAUSE.
  (let (VAR.OCCUR.FCTS (TERMLIST (DS-CLAUSE.TERMLIST CLAUSE LITNO)) (FCT (DT-TAF.CREATE.FIRST NIL)))
    (WHILE TERMLIST (SETQ VAR.OCCUR.FCTS (NCONC (DS=TERM.VAR.OCCUR VARIABLE (CAR TERMLIST) FCT) VAR.OCCUR.FCTS))
	   (SETQ TERMLIST (CDR TERMLIST)) (SETQ FCT (DT-TAF.CREATE.NEXT FCT)))
    VAR.OCCUR.FCTS))


;;; LINK Variables
;;; --------------


(DEFVAR DS*LINK.TYPES '(R RIW RD S SI SIW SID T TI TIW P PIW PD RIWD))

(DEFVAR DS*LINK.CLAUSE NIL)

(DEFVAR DS*LINK.LITERALS DS*LINK.TYPES)

(DEFVAR DS*LINK.INITIAL '(R RIW S SI SIW T TI TIW P PIW))

(DEFVAR DS*LINK.PURITY '(R P))

(DEFVAR DS*LINK.RENAMED '(RIW RIWD SIW TIW PIW))

(DEFVAR DS*LINK.NOPURITY '(RIW RD RIWD S SI SID SIW T TI TIW PIW PD))

(DEFVAR DS*LINK.rules '(r RIW RD RIWD S SI SID SIW T TI TIW))

(DEFVAR DS*LINK.TAUTOLOGY.TYPE '(R P SI))

(DEFVAR DS*LINK.WITH.NEGPARENT '(R RD S T P PD))

(DEFVAR DS*LINK.ACTIVE.OPERATION '(R SI P piw))

(DEFVAR DS*LINK.ACTIVE.OPERATION.extended '(R SI P riw))

(DEFVAR DS*LINK.AUTOLINKS '(RIW RIWD SI SIW TI TIW PIW SID))

(DEFVAR DS*LINK.NOT.AUTOLINKS '(R RD S T P PD))

(DEFVAR DS*LINK.EXTENDED.PARAMODULATION '(P PIW PD))

(DEFVAR DS*LINK.WITH.UNIFIERS '(R RIW RD RIWD S SI SIW SID T TI TIW P PIW PD))

(DEFVAR DS*LINK.PARAMODULATION '(P PIW))

(defvar ds*link.active.paramodulation (intersection DS*LINK.PARAMODULATION DS*LINK.ACTIVE.OPERATION))

(DEFPARAMETER DS*LINK.STORAGE.SIZE 15)




;;; link macros
;;; -----------



(DEFMACRO DS=LINK.STORAGE (TYPE SIZE)
						; EDITED: "18-DEC-80 13:54:10")
						; INPUT:  TYPE - LINK TYPE
						;         SIZE - NUMBER OF STORAGE COMPONENTS
						; EFFECT: ACQUIRES A NEW STORAGE UNIT OF 'SIZE'
						;         COMPONENTS
						; VALUE:  Address of the new storage unit.
  `(MEM-NEW ,TYPE ,SIZE))

(DEFMACRO DS=LINK.GET (&OPTIONAL COMPONENT LINK)
						; EDITED: 29-AUG-84 15:43:30   SYNTHETIC
						; INPUT:  COMPONENT IS ONE OF THE ATOMS:
						;         POSPAR, POSLITNO, NEGLITNO, UNIFIERS, RULE,
						;         NEGPAR, POSFCT, NEGFCT, LABEL,
						;         LINK IS A LINK ADDRESS
						; VALUE:  CONTENTS OF THE MEMORY ARRAY ELEMENT
						;         FOR LINK, DENOTED BY COMPONENT.
  (COND ((AND (CONSP COMPONENT)
	      (EQL (CAR COMPONENT) 'QUOTE)
	      (ATOM (SECOND COMPONENT)))
	 (CASE (SECOND COMPONENT)
	   (POSPAR `(MEM-GET ,LINK 1))
	   (POSLITNO `(MEM-GET ,LINK 2))
	   (NEGLITNO `(MEM-GET ,LINK 3))
	   (UNIFIERS `(MEM-GET ,LINK 4))
	   (RULE `(MEM-GET ,LINK 5))
	   (NEGPAR `(MEM-GET ,LINK 6))
	   (POSFCT `(MEM-GET ,LINK 7))
	   (NEGFCT `(MEM-GET ,LINK 8))
	   (LABEL `(MEM-GET ,LINK 9))
	   (result `(mem-get ,link 10))
	   (selection.info `(mem-get ,link 11))
	   (result.variables `(mem-get ,link 12))
	   (demodulation `(mem-get ,link 13))
	   (OTHERWISE (ERROR "DS=LINK.GET: ILLEGAL COMPONENT~A" COMPONENT))))
	(T `(DS==LINK.GET ,COMPONENT ,LINK))))


(DEFUN DS==LINK.GET (COMPONENT LINK)
						; EDITED: 29-AUG-84 15:43:30   SYNTHETIC
						; INPUT:  COMPONENT IS ONE OF THE ATOMS:
						;         POSPAR, POSLITNO, NEGLITNO, UNIFIERS, RULE,
						;         NEGPAR, POSFCT, NEGFCT, LABEL,
						;         LINK IS A LINK ADDRESS
						; VALUE:  CONTENTS OF THE MEMORY ARRAY ELEMENT
						;         FOR LINK, DENOTED BY COMPONENT.
  (CASE COMPONENT
    (POSPAR (MEM-GET LINK 1))
    (POSLITNO (MEM-GET LINK 2))
    (NEGLITNO (MEM-GET LINK 3))
    (UNIFIERS (MEM-GET LINK 4))
    (NEGPAR (MEM-GET LINK 5))
    (RULE (MEM-GET LINK 6))
    (POSFCT (MEM-GET LINK 7))
    (NEGFCT (MEM-GET LINK 8))
    (LABEL (MEM-GET LINK 9))
    (result (mem-get link 10))
    (selection.info (mem-get link 11))
    (result.variables (mem-get link 12))
    (demodulation (mem-get link 13))
    (OTHERWISE (ERROR "Illegal component ~A" COMPONENT))))

(DEFMACRO DS=LINK.PUT (&OPTIONAL COMPONENT LINK VALUE)
						; EDITED: 29-AUG-84 15:43:30   SYNTHETIC
						; INPUT:  COMPONENT IS ONE OF THE ATOMS:
						;         POSPAR, POSLITNO, NEGLITNO, UNIFIERS, RULE,
						;         NEGPAR, POSFCT, NEGFCT, LABEL,
						;         LINK IS A LINK ADDRESS
						;         VALUE IS ANY S-EXPRESSION
						; EFFECT: VALUE IS ASSIGNED TO MEMORY ARRAY ELEMENT
						;         FOR LINK, DENOTED BY COMPONENT.
  (COND
    ((AND (CONSP COMPONENT) (EQL (CAR COMPONENT) 'QUOTE) (ATOM (SECOND COMPONENT)))
     (CASE (SECOND COMPONENT)
       (POSPAR `(MEM-PUT ,LINK 1 ,VALUE))
       (POSLITNO `(MEM-PUT ,LINK 2 ,VALUE))
       (NEGLITNO `(MEM-PUT ,LINK 3 ,VALUE))
       (UNIFIERS `(MEM-PUT ,LINK 4 ,VALUE))
       (RULE `(MEM-PUT ,LINK 5 ,VALUE))
       (NEGPAR `(MEM-PUT ,LINK 6 ,VALUE))
       (POSFCT `(MEM-PUT ,LINK 7 ,VALUE))
       (NEGFCT `(MEM-PUT ,LINK 8 ,VALUE))
       (LABEL `(MEM-PUT ,LINK 9 ,VALUE))
       (result `(mem-put ,link 10 ,VALUE))
       (selection.info `(mem-put ,link 11 ,VALUE))
       (result.variables `(mem-put ,link 12 ,VALUE))
       (demodulation `(mem-put ,link 13 ,VALUE))
       (OTHERWISE (ERROR "Illegal component ~A" COMPONENT))))
    (T `(DS==LINK.PUT ,COMPONENT ,LINK ,VALUE))))

(DEFUN DS==LINK.PUT (COMPONENT LINK VALUE)
						; EDITED: 29-AUG-84 15:43:30   SYNTHETIC
						; INPUT:  COMPONENT IS ONE OF THE ATOMS:
						;         POSPAR, POSLITNO, NEGLITNO, UNIFIERS, RULE,
						;         NEGPAR, POSFCT, NEGFCT, LABEL,
						;         LINK IS A LINK ADDRESS
						;         VALUE IS ANY S-EXPRESSION
						; EFFECT: VALUE IS ASSIGNED TO MEMORY ARRAY ELEMENT
						;         FOR LINK, DENOTED BY COMPONENT.
  (CASE COMPONENT
    (POSPAR (MEM-PUT LINK 1 VALUE))
    (POSLITNO (MEM-PUT LINK 2 VALUE))
    (NEGLITNO (MEM-PUT LINK 3 VALUE))
    (UNIFIERS (MEM-PUT LINK 4 VALUE))
    (RULE (MEM-PUT LINK 5 VALUE))
    (NEGPAR (MEM-PUT LINK 6 VALUE))
    (POSFCT (MEM-PUT LINK 7 VALUE))
    (NEGFCT (MEM-PUT LINK 8 VALUE))
    (LABEL (MEM-PUT LINK 9 VALUE))
    (result (mem-put link 10 VALUE))
    (selection.info (mem-put link 11 VALUE))
    (result.variables (mem-put link 12 VALUE))
    (demodulation (mem-put link 13 VALUE))
    (OTHERWISE (ERROR "Illegal component ~A" COMPONENT))))


;;; link interface
;;; --------------


(DEFun DS-LINK.cREATE (COLOUR UNIFIERS PAR1 LITNO1 PAR2 LITNO2 &optional TAF1 TAF2 RULE)
						; INPUT:  COLOUR:A LINKCOLOUR
						;         UNIFIERS:LIST OF INDEPENDENT SUBSTITUTIONS
						;         PAR1,PAR2:CLAUSE ADDRESSES
						;         LITNO1,LITNO2:LITNUMBERS OF PAR1 OR PAR2
						;         FCT1,FCT2:TERMACCESS FUNCTIONS
						; VALUE:  ADDRESS OF THE CREATED LINK
						; EFFECT: CREATES A LINK OF THE TYPE ACCORDING TO
						;         THE COLOUR PARAMETER
						; REMARKS:-FCT1,FCT2 WILL BE IGNORED,IFF COLOUR
						;          NOT IN DS*LINK.EXTENDED.TYPES
						;         -PAR2 WILL BE IGNORED,IFF COLOUR IS
						;          NOT IN (CADR DS*LINK.NEGPAR)
						;         -LITNO1,LITNO2 WILL BE IGNORED,IFF COLOUR
						;          IS NOT IN DS*LINK.CLAUSE
  (let (LINK DS.NEGFCT DS.NEGLITNO DS.POSFCT DS.POSLITNO DS.POSPAR DS.NEGPAR)
    (DECLARE (SPECIAL DS.NEGFCT DS.NEGLITNO DS.POSFCT DS.POSLITNO DS.POSPAR DS.NEGPAR))
    (DS=LINK.COMPUTE.DIRECTION PAR1 PAR2 LITNO1 LITNO2 TAF1 TAF2)
    (SETQ LINK (DS=LINK.STORAGE colour (DS=LINK.STORAGE.SIZE COLOUR)))
    (DS=LINK.PUT 'POSPAR LINK DS.POSPAR)
    (DS=LINK.PUT 'demodulation LINK nil)
    (DS=LINK.PUT 'result.variables LINK nil)
    (DS=LINK.PUT 'selection.info LINK nil)
    (DS=LINK.PUT 'result LINK nil)
    (DS=LINK.PUT 'POSLITNO LINK DS.POSLITNO)
    (DS=LINK.PUT 'NEGLITNO LINK DS.NEGLITNO)
    (DS=LINK.PUT 'NEGPAR LINK DS.NEGPAR)
    (DS=LINK.PUT 'POSFCT LINK DS.POSFCT)
    (DS=LINK.PUT 'NEGFCT LINK DS.NEGFCT)
    (DS=LINK.PUT 'LABEL LINK NIL)
    (DS=LINK.PUT 'RULE LINK RULE)
    (cond ((and (opt-get.option sort_literals)
		(first unifiers)
		(uni-literal.sort.is (first unifiers)))
	   (ds-link.put.sort.inhibited link 'inhibited)
	   (ds-link.put.sort.residue link (uni-literal.sort.residue (first unifiers)))      
	   (DS=LINK.PUT 'UNIFIERS LINK (list (uni-literal.sort.unifier (first UNIFIERS)))))
	  (t (when (opt-get.option sort_literals) (ds-link.put.sort.inhibited link nil))
	     (DS=LINK.PUT 'UNIFIERS LINK UNIFIERS)))      
    Link))

(defun ds-link.sort.inhibited (link)
  (dt-getprop link 'ds*sort.inhibit))

(defun ds-link.put.sort.inhibited (link value)
  (dt-putprop link 'ds*sort.inhibit value))

(defun ds-link.sort.residue (link)
  (dt-getprop link 'ds*sort.residue))

(defun ds-link.put.sort.residue (link value)
  (dt-putprop link 'ds*sort.residue value))

(DEFMACRO DS-LINK.COLOUR (LINK)
						; EDITED: "16-NOV-79 11:44:15"
						; VALUE: COLOUR OF LINK SYMBOL
  `(DS=LINK.GETCOLOUR ,LINK))

(DEFMACRO DS-LINK.UNIFIERS (LINK)
						; VALUE : UNIFIERS OF LINK SYMBOL
  `(DS=LINK.GET 'UNIFIERS ,LINK))

(DEFMACRO DS-LINK.PUTUNIFIERS (LINK UNIFIERLIST)
						; EDITED: 4. 5. 1982   HJO
						; INPUT:   AN LINK AND A LIST OF UNIFIERS.
						; EFFECT:  CHANGES THE UNIFIER CELL OF LINK.
						; WARNING: THIS FUNCTION SHOULD NOT BE USED FOR LINKS
						;          WITH COLOUR IN DS*LINK.HEURISTICS
						;          IF THE HEURISTICS ARE ACTIVATED, BECAUSE
						;          IT DOES NOT UPDATE THE LINKSUM AND PRIORITY
						;          VALUES.
						; VALUE:   UNDEFINED.
  `(DS=LINK.PUT 'UNIFIERS ,LINK ,UNIFIERLIST))

(DEFMACRO DS-LINK.NOUNIFIERS (LINK)
						; EDITED:  5. 1. 1982    HJO
						; VALUE:   NUMBER OF UNIFIERS OF LINK
  `(LIST-LENGTH (DS=LINK.GET 'UNIFIERS ,LINK)))

(DEFMACRO DS-LINK.UNIFIER (LINK UNIFIERNO)
						; EDITED:  20. 1. 1982   HJO
						; INPUT:   A LINK WITH COLOUR IN DS*LINK.UNIFIERS
						;          AND A POSITIONNUMBER OF A UNIFIER
						; VALUE:   SELECTED UNIFIER.
  `(CAR (NTHCDR (1- ,UNIFIERNO) (DS=LINK.GET 'UNIFIERS ,LINK))))

(DEFMACRO DS-LINK.RULE (LINK)
						; EDITED: 29-SEP-83 20:07:20                    AP
						; INPUT:  LINK     ADDRESS.
						; EFFECT:  -
						; VALUE:  LINK COMPONENT RULE CLAUSE.
  `(DS=LINK.GET 'RULE ,LINK))

(DEFUN DS-LINK.REMOVE.UNIFIER (LINK UNIFIER)
						; EDITED: " 1-FEB-82 16:49:39")
						; INPUT : A LINK OF THE ACTUAL GRAPH.
						; EFFECT: REMOVES UNIFIER FROM THE UNIFIER-LIST
						;         OF LINK.
						; VALUE : UNDEFINED.
  (DS=LINK.PUT 'UNIFIERS LINK (DELETE UNIFIER (DS=LINK.GET 'UNIFIERS LINK))))

(DEFMACRO DS-LINK.LABEL (LINK)
						; EDITED:  7-MAR-83 14:39:16
						; INPUT:   ONE LINK
						; VALUE:   THE LABEL OF LINK
  `(DS=LINK.GET 'LABEL ,LINK))

(DEFmacro DS-LINK.MARK (ATTRIBUTE DS.LINK)
						; EDITED: 11-FEB-83 13:37:07
						; INPUT:  AN ATOM AND A LINK ADDRESS
						; EFFECT: THE LINK IS MARKED WITH THIS ATTRIBUTE
						; VALUE:  UNDEFINED
  `(DS=LINK.PUT 'LABEL ,DS.LINK ',ATTRIBUTE))

(DEFMACRO DS-LINK.IS.MARKED (ATTRIBUTE DS.LINK)
						; EDITED: 11-FEB-83 13:42:59
						; INPUT:  AN ATOM AND A LINK ADDRESS
						; VALUE:  T IF THE LINK IS MARKED WITH THIS ATTRIBUTE
						;         ELSE NIL.
  `(EQ (QUOTE ,ATTRIBUTE) (DS=LINK.GET 'LABEL ,DS.LINK)))

(DEFMACRO DS-LINK.NEGPAR (LINK)
						; EDITED: "16-NOV-79 11:46:16")
						; VALUE: NEGATIVE PARENT CLAUSE OF LINK SYMBOL *)
  `(DS=LINK.GET 'NEGPAR ,LINK))

(DEFMACRO DS-LINK.NEGLITNO (LINK)
						; EDITED: "30-NOV-81 11:53:32")
						; VALUE: NUMBER OF PARENT LITERAL IN NEGATIVE PARENT CLAUSE OF LINK SYMBOL *)
  `(DS=LINK.GET 'NEGLITNO ,LINK))

(DEFMACRO DS-LINK.NEGFCT (LINK)
						; EDITED: "16-NOV-79 11:50:19")
						; VALUE :ACCESS FUNCTION TO " NEGATIVE " PARENT TERM OF
						;        LINK.COLOUR(LINK) IS MEMBER OF
						;        DS*LINK.EXTENDED.PARAMODULATION
  `(DS=LINK.GET 'NEGFCT ,LINK))

(defmacro ds-link.demodulation.is (link)
						; Edited:  18-MAR-1989 12:19
						; Authors: PRCKLN
						; Input:   A LINK
						; Effect:  -
						; Value:   True iff LINK is a demodulation plink.
  `(DS=LINK.get 'demodulation ,link))


(defmacro ds-link.demodulation.set (link)
						; Edited:  "18-MAR-1989 12:20"
						; Authors: PRCKLN
						; Input:   A link
						; Effect:  Marks the link to be a demodulation link
						; Value:   Undefined
  `(DS=LINK.put 'demodulation ,link t))



(DEFUN DS-LINK.NEGTERM (LINK)
						; EDITED: 18-FEB-83 11:20:41
						; INPUT:  A PARAMODULATION LINK (P, IP OR DP)
						; VALUE:  THE NEGATIVE TERM, TO WHICH THE LINK IS
						;         DIRECTED.
  (COND ((MEMBER (DS=LINK.GETCOLOUR LINK) DS*LINK.EXTENDED.PARAMODULATION)
	 (DT-ACCESS (DS=LINK.GET 'NEGFCT LINK)
		    (DS=CLAUSE.GET 'TERMLIST (DS=LINK.GET 'NEGPAR LINK) (DS=LINK.GET 'NEGLITNO LINK))))
	(T (ERROR "BAD LINK TYPE IN DS-LINK.NEGTERM (LINK): ~A" LINK))))

(DEFUN DS-LINK.NEGLIT (LINK)
						; EDITED: "16-NOV-79 13:32:55")
						; VALUE: NEGATIVE PARENT LITERAL OF LINK *)
  (let ((NEGPAR (DS=LINK.GET 'NEGPAR LINK))
	(NEGLITNO (DS=LINK.GET 'NEGLITNO LINK)))
    (DS=LIT.STORAGE (DS=CLAUSE.GET 'SIGN NEGPAR NEGLITNO)
		    (DS=CLAUSE.GET 'PREDICATE NEGPAR NEGLITNO)
		    (DS=CLAUSE.GET 'TERMLIST NEGPAR NEGLITNO))))

(DEFMACRO DS-LINK.POSPAR (LINK)
						; EDITED: "16-NOV-79 11:46:42")
						; VALUE: POSITIVE PARENT CLAUSE OF LINK SYMBOL *)
  `(DS=LINK.GET 'POSPAR ,LINK))

(DEFMACRO DS-LINK.POSLITNO (LINK)
						; EDITED: "30-NOV-81 11:53:48")
						; VALUE: NUMBER OF PARENT LITERAL IN POSITIVE PARENT CLAUSE OF LINK SYMBOL *)
  `(DS=LINK.GET 'POSLITNO ,LINK))

(DEFMACRO DS-LINK.POSFCT (LINK)
						; EDITED:  16-NOV-79 11:50:42
						; VALUE:   Access function to "positive" parent term if
						;          LINK.COLOUR(LINK) is member of
						;          DS*LINK.EXTENDED.PARAMODULATION
  `(DS=LINK.GET 'POSFCT ,LINK))

(defmacro ds-link.result (link)
						; Edited:  24-AUG-1990 16:05
						; Authors: PRCKLN
						; Input:   A link.
						; Effect:  -
						; Value:   A literal list, the result of performing a hyper
						;          operation on LINK beginning with the operation corresponding
						;          to the links colour, if LINK is an op-link, else nil.
  `(DS=LINK.GET 'result ,LINK))

(defmacro ds-link.selection.info (link)
						; Edited:  24-AUG-1990 16:05
						; Authors: PRCKLN
						; Input:   A link.
						; Effect:  -
						; Value:   The information for the SELECTION about the complexity of the
						;          result DS-LINK.RESULT. 
  `(DS=LINK.GET 'selection.info ,link))

(defmacro ds-link.result.variables (link)
						; Edited:  24-AUG-1990 16:05
						; Authors: PRCKLN
						; Input:   A link.
						; Effect:  -
						; Value:   The variables occurring in DS-LINK.RESULT. 
  `(DS=LINK.GET 'result.variables ,link))

(defmacro ds-link.put.result (link result)
						; Edited:  24-AUG-1990 16:05
						; Authors: PRCKLN
						; Input:   A link.
						;          A literal list, the result of performing a hyper
						;          operation on LINK beginning with the operation corresponding
						;          to the links colour, if LINK is an op-link, else nil
						; Effect:  Puts the result into the result component of LINK.
						; Value:   Undefined.
  `(DS=LINK.put 'result ,LINK ,result))

(defmacro ds-link.put.selection.info (link info)
						; Edited:  24-AUG-1990 16:05
						; Authors: PRCKLN
						; Input:   A link.
						;          The information for the SELECTION about the complexity of the
						;          result DS-LINK.RESULT. 
						; Effect:  Puts them into the component of LINK.
						; Value:   Undefined.
  `(DS=LINK.put 'selection.info ,link ,info))

(defmacro ds-link.put.result.variables (link variables)
						; Edited:  24-AUG-1990 16:05
						; Authors: PRCKLN
						; Input:   A link.
						;          The variables occurring in DS-LINK.RESULT.
						; Effect:  Puts them into the component of LINK. 
						; Value:   Undefined.
  `(DS=LINK.put 'result.variables ,link ,variables))

(DEFUN DS-LINK.POSTERM (LINK)
						; EDITED: 18-FEB-83 11:20:41
						; INPUT:  A PARAMODULATION LINK (P, IP OR DP)
						; VALUE:  THE POSITIVE TERM, TO WHICH THE LINK IS
						;         DIRECTED (I.E. ONE SIDE OF AN EQUATION)
  (COND
    ((MEMBER (DS=LINK.GETCOLOUR LINK) DS*LINK.EXTENDED.PARAMODULATION)
     (DT-ACCESS (DS=LINK.GET 'POSFCT LINK)
		(DS=CLAUSE.GET 'TERMLIST (DS=LINK.GET 'POSPAR LINK) (DS=LINK.GET 'POSLITNO LINK))))
    (T (ERROR "BAD LINK TYPE IN DS-LINK.POSTERM (LINK): ~A" LINK))))

(DEFUN DS-LINK.POSLIT (LINK)
						; EDITED: "16-NOV-79 13:34:24")
						; VALUE: POSITIVE PARENT LITERAL OF LINK *)
  (let ((POSPAR (DS=LINK.GET 'POSPAR LINK))
	(POSLITNO (DS=LINK.GET 'POSLITNO LINK)))
    (DS=LIT.STORAGE (DS=CLAUSE.GET 'SIGN POSPAR POSLITNO)
		    (DS=CLAUSE.GET 'PREDICATE POSPAR POSLITNO)
		    (DS=CLAUSE.GET 'TERMLIST POSPAR POSLITNO))))

(DEFUN DS-LINK.OTHERPAR (LINK THISPAR)
						; EDITED: 16-NOV-79 13:29:09
						; VALUE:  IF THISPAR = LINK.POSPAR THEN LINK.NEGPAR,
						;         IF THISPAR = LINK.NEGPAR THEN LINK.POSPAR OTHERWISE FAIL
  (let (LINK.POSPAR LINK.NEGPAR)
    (COND ((MEMBER (DS=LINK.GETCOLOUR LINK) (DS-LINK.COLOURS.WITH 'NEGPAR))
	   (SETQ LINK.NEGPAR (DS=LINK.GET 'NEGPAR LINK))
	   (SETQ LINK.POSPAR (DS=LINK.GET 'POSPAR LINK))
	   (COND ((EQL THISPAR LINK.POSPAR) LINK.NEGPAR)
		 ((EQL THISPAR LINK.NEGPAR) LINK.POSPAR)
		 (T (ERROR "BAD ARGUMENTS IN DS-LINK.OTHERPAR. LINK=~A" LINK))))
	  (T THISPAR))))

(DEFUN DS-LINK.OTHERLITNO (LINK THISPAR &optional THISLITNO)
						; EDITED: 16-NOV-79 13:26:42
						; VALUE:  IF THISPAR = LINK.POSPAR THEN LINK.NEGLITNO,
						;         IF THISPAR = LINK.NEGPAR THEN LINK.POSLITNO OTHERWISE FAIL
  (let ((LINK.NEGLITNO (DS=LINK.GET 'NEGLITNO LINK))
	(LINK.POSLITNO (DS=LINK.GET 'POSLITNO LINK))
	LINK.NEGPAR LINK.POSPAR)
    (SETQ LINK.NEGPAR (DS=LINK.GET 'NEGPAR LINK)
	  LINK.POSPAR (DS=LINK.GET 'POSPAR LINK))
    (COND ((and (EQL THISPAR LINK.POSPAR) (or (null thislitno) (EQL THISlitno LINK.POSlitno)))
	   LINK.NEGLITNO)
	  ((and (EQL THISPAR LINK.NEGPAR) (or (null thislitno) (EQL THISlitno LINK.neglitno)))
	   LINK.POSLITNO)
	  (T (ERROR "BAD ARGUMENTS LINK ~A" LINK)))))


(DEFUN DS-LINK.CONNECT (LINK)
						; EDITED: "27-NOV-80 16:36:22")
						; INPUT:  LINK ADDRESS
						; EFFECT: INSERTS LINK IN   THE LINK LISTS OF ITS
						;         PARENT LITERAL(S)
						; VALUE:  UNDEFINED
  (let (POSPAR POSLITNO NEGLITNO NEGPAR (COLOUR (DS=LINK.GETCOLOUR LINK)))
    (COND ((MEMBER COLOUR DS*LINK.CLAUSE)
	   (SETQ POSPAR (DS=LINK.GET 'POSPAR LINK))
	   (DS=CLAUSE.PUT COLOUR (CONS LINK (DS=CLAUSE.GET COLOUR POSPAR)) POSPAR))
	  (T (SETQ POSPAR (DS=LINK.GET 'POSPAR LINK))
	     (SETQ POSLITNO (DS=LINK.GET 'POSLITNO LINK))
	     (SETQ NEGPAR (DS=LINK.GET 'NEGPAR LINK))	     
	     (SETQ NEGLITNO (DS=LINK.GET 'NEGLITNO LINK))
	     (DS=CLAUSE.PUT COLOUR (CONS LINK (DS=CLAUSE.GET COLOUR POSPAR POSLITNO)) POSPAR POSLITNO)
	     (unless (and (eql pospar negpar) (eql poslitno neglitno))
	       (DS=CLAUSE.PUT COLOUR (CONS LINK (DS=CLAUSE.GET COLOUR NEGPAR NEGLITNO)) NEGPAR NEGLITNO))))))

(DEFUN DS-LINK.DISCONNECT (LINK)
						; EDITED: "27-NOV-80 16:36:22")
						; INPUT:  LINK ADDRESS
						; EFFECT: REMOVES LINK FROM THE LINK LISTS OF ITS
						;         PARENT LITERAL(S)
						; VALUE:  UNDEFINED
  (PROG (POSPAR POSLITNO NEGLITNO NEGPAR (COLOUR (DS=LINK.GETCOLOUR LINK)))
	(COND
	  ((MEMBER COLOUR DS*LINK.CLAUSE)
	   (SETQ POSPAR (DS=LINK.GET 'POSPAR LINK))
	   (DS=CLAUSE.PUT COLOUR (Delete LINK (DS=CLAUSE.GET COLOUR POSPAR)) POSPAR))
	  (T (SETQ POSPAR (DS=LINK.GET 'POSPAR LINK))
	     (SETQ POSLITNO (DS=LINK.GET 'POSLITNO LINK))
	     (SETQ NEGPAR (DS=LINK.GET 'NEGPAR LINK))
	     (SETQ NEGLITNO (DS=LINK.GET 'NEGLITNO LINK))
	     (DS=CLAUSE.PUT COLOUR (DELETE LINK (DS=CLAUSE.GET COLOUR NEGPAR NEGLITNO)) NEGPAR NEGLITNO)
	     (DS=CLAUSE.PUT COLOUR (DELETE LINK (DS=CLAUSE.GET COLOUR POSPAR POSLITNO)) POSPAR POSLITNO)))))

(DEFmacro DS-LINK.DEPTH (LINK)
						; Edited: 10-JUN-1988 02:58
						; Authors: PRCKLN
						; Input:  A Link
						; Effect: -
						; Value:  DEPTH OF LINK SYMBOL
  (if (symbolp link)
      `(1+ (max (DS=CLAUSE.GET 'DEPTH (DS=LINK.GET 'NEGPAR ,LINK))
		(DS=CLAUSE.GET 'DEPTH (DS=LINK.GET 'POSPAR ,LINK))))
      `(let ((link.to.test ,link))
	 (1+ (max (DS=CLAUSE.GET 'DEPTH (DS=LINK.GET 'NEGPAR LINK.to.test))
		  (DS=CLAUSE.GET 'DEPTH (DS=LINK.GET 'POSPAR LINK.to.test)))))))

(DEFUN DS-LINK.NOLIT (LINK)
						; EDITED:  16-NOV-79 12:30:26
						; VALUE:   NUMBER OF LITERALS THAT THE CLAUSE CREATED BY
						;          OPERATION ON LINK WOULD HAVE *)
  (let ((COLOUR (DS=LINK.GETCOLOUR LINK)))
    (COND
      ((MEMBER COLOUR ds*link.active.paramodulation)
       (+ (DS=CLAUSE.GET 'NOLIT (DS=LINK.GET 'NEGPAR LINK)) (DS=CLAUSE.GET 'NOLIT (DS=LINK.GET 'POSPAR LINK)) -1))
      ((MEMBER COLOUR DS*LINK.ACTIVE.OPERATION)
       (+ (DS=CLAUSE.GET 'NOLIT (DS=LINK.GET 'NEGPAR LINK)) (DS=CLAUSE.GET 'NOLIT (DS=LINK.GET 'POSPAR LINK)) -2))
      (T (ERROR "ILLEGAL COLOUR IN DS-LINK.NOLIT. LINK=~A" LINK)))))


(DEFMACRO DS-LINK.IS (LINK)
						; EDITED: "16-NOV-79 11:56:20")
						; VALUE: T IF LINK IS A LINK SYMBOL NOT YET DELETED, ELSE NIL *)
  `(DS=LINK.IS ,LINK))


(DEFMACRO DS-LINK.DELETE (LINK)
						; EDITED: "16-NOV-79 11:56:55")
						; VALUE: UNDEFINED *)
  `(MEM-ERASE ,LINK T))

(DEFMACRO DS-LINK.COLOURS.WITH (&OPTIONAL COMPONENT)
						; EDITED: 17-FEB-83 13:14:51
						; INPUT:  AN ATOM WHICH DENOTES A COMPONENT OF A
						;         LINK TYPE LIKE 'POSPAR , 'NEGPAR  ETC.
						; VALUE:  LIST OF ALL LINK COLOURS WHICH POSSESS
						;         THIS COMPONENT.
  (declare (ignore component))
  'ds*link.types)

(DEFMACRO DS-LINK.COLOURS.FOR (&OPTIONAL APPLICATION)
						; EDITED: 17-FEB-83 13:45:34
						; INPUT:  AN ATOM
						; VALUE:  A LIST OF LINK COLOURS
						;         INPUT                   COLOURS
						;         -----                   -------
						;         ALL           ALL LINK COLOURS
						;         CLAUSE        COLOURS RELATED TO A CLAUSE
						;         LITERALS      COLOURS RELATED TO LITERALS
						;         INITIAL       INITIALLY CREATED IN
						;                       ATP.CONSTRUCT
						;         PURITY        PURITY RELEVANT COLOURS
						;         NOPURITY      NON PURITY RELEVANT COLOURS
						;         TAUTOLOGY     COLOURS WHICH MIGHT GENERATE
						;                       TAUTOLOGIES
						;         OPERATION     COLOURS WHICH INDICATE
						;                       OPERATIONS BETWEEN CLAUSES
						;         AUTOLINKS     COLOURS FOR AUTOLINKS
						;         NOAUTOLINKS   COLOURS FOR NO AUTOLINKS
						;         PARAMODULATION COLOURS RELATED WITH
						;                        PARAMODULATION
						;         EXTENDED.PARAMODULATION
						;                       PARAMODULATION LINKS INCLUDING
						;                       AUTOLINKS.
						;         RENAMED       LINK COLOURS CONNECTING WEAK
						;                       UNIFIABLE LITERALS
  (COND ((AND (CONSP APPLICATION) (EQL (CAR APPLICATION) 'QUOTE))
	 (LIST 'QUOTE
	       (SYMBOL-VALUE
		 (CASE (SECOND APPLICATION)
		   (ALL 'DS*LINK.TYPES)
		   (CLAUSE 'DS*LINK.CLAUSE)
		   (LITERALS 'DS*LINK.LITERALS)
		   (INITIAL 'DS*LINK.INITIAL)
		   (PURITY 'DS*LINK.PURITY)
		   (NOPURITY 'DS*LINK.NOPURITY)
		   (TAUTOLOGY 'DS*LINK.TAUTOLOGY.TYPE)
		   (OPERATION 'DS*LINK.ACTIVE.OPERATION)
		   (OPERATION.extended 'DS*LINK.ACTIVE.OPERATION.extended)
		   (AUTOLINKS 'DS*LINK.AUTOLINKS)
		   (NOAUTOLINKS 'DS*LINK.NOT.AUTOLINKS)
		   (PARAMODULATION 'DS*LINK.PARAMODULATION)
		   (EXTENDED.PARAMODULATION 'DS*LINK.EXTENDED.PARAMODULATION)
		   (RENAMED 'DS*LINK.RENAMED)
		   (RULES 'DS*LINK.RULES)
		   (OTHERWISE (ERROR "ILLEGAL ARGUMENT IN DS-LINK.COLOURS.FOR~A" APPLICATION))))))
	(T
	 `(symbol-value (case ',APPLICATION
			  (ALL 'DS*LINK.TYPES)
			  (CLAUSE 'DS*LINK.CLAUSE)
			  (LITERALS 'DS*LINK.LITERALS)
			  (INITIAL 'DS*LINK.INITIAL)
			  (PURITY 'DS*LINK.PURITY)
			  (NOPURITY 'DS*LINK.NOPURITY)
			  (TAUTOLOGY 'DS*LINK.TAUTOLOGY.TYPE)
			  (OPERATION 'DS*LINK.ACTIVE.OPERATION)
			  (OPERATION.extended 'DS*LINK.ACTIVE.OPERATION.extended)
			  (AUTOLINKS 'DS*LINK.AUTOLINKS)
			  (NOAUTOLINKS 'DS*LINK.NOT.AUTOLINKS)
			  (PARAMODULATION 'DS*LINK.PARAMODULATION)
			  (EXTENDED.PARAMODULATION 'DS*LINK.EXTENDED.PARAMODULATION)
			  (RENAMED 'DS*LINK.RENAMED)
			  (RULES 'DS*LINK.RULES)
			  (otherwise (ERROR "ILLEGAL ARGUMENT IN DS-LINK.COLOURS.FOR" APPLICATION)))))))



(DEFUN DS-LINK.SIDE (LINK CLAUSE)
						; EDITED: 16-FEB-83 11:43:31
						; INPUT:  A P-LINK AND A PARENTCLAUSE OF LINK
						; VALUE:  NIL, IF THE LINK IS MARKED INHERITANCE.ONLY
						;         OR PARAMODULATION INTO A VARIABLE WOULD
						;         BE POSSIBLE.
						;         LEFT, IF THE LINK IS CONNECTED TO THE
						;         TOP-LEVEL LEFT SIDE OF THE EQUATION IN
						;         CLAUSE, ELSE RIGHT.
  (COND ((DS-LINK.IS.MARKED INHERITANCE.ONLY LINK) NIL)
	(T
	 (PROG ((POSPAR (DS-LINK.POSPAR LINK)))
	       (COND
		 ((EQL CLAUSE POSPAR)
		  (COND
		    ((NOT
		       (DT-VARIABLE.IS
			 (DT-ACCESS (DS-LINK.NEGFCT LINK)
				    (DS-CLAUSE.TERMLIST (DS-LINK.NEGPAR LINK)
							(DS-LINK.NEGLITNO LINK)))))
		     (COND ((DT-TAF.IS.LEFT (DS-LINK.POSFCT LINK)) (RETURN 'LEFT))
			   (T (RETURN 'RIGHT))))))
		 ((NOT (DT-VARIABLE.IS (DT-ACCESS (DS-LINK.POSFCT LINK)
						  (DS-CLAUSE.TERMLIST POSPAR (DS-LINK.POSLITNO LINK)))))
		  (COND ((DT-TAF.IS.LEFT (DS-LINK.NEGFCT LINK))
			 (RETURN 'LEFT)) (T (RETURN 'RIGHT)))))))))

(DEFUN DS-LINK.THISLITNO (LINK CLAUSE)
					; EDITED: 9. 8. 1984
					; INPUT:  A LINK AND ONE OF ITS PARENTCLAUSES
					; VALUE:  THE LITERAL NUMBER AT WHICH THE LINK IS
					;         CONNECTED TO CLAUSE.
  (COND ((EQL CLAUSE (DS=LINK.GET 'POSPAR LINK))
	 (DS=LINK.GET 'POSLITNO LINK))
	(T (DS=LINK.GET 'NEGLITNO LINK))))


(DEFUN DS-LINK.THISFCT (LINK CLAUSE LITNO TAF)
						; INPUT : LINK CLAUSE LITNO TAF
						;         LINK CONNECTS THE LITERAL(CLAUSE LITNO) TO
						;         ANOTHER LITERAL
						; VALUE : THE TERM ACCESS FUNCTION OF THE TERM OF
						;         (CLAUSE LITNO),WHICH IS CONNECTED WITH LINK
  taf
  (COND ((AND (EQL CLAUSE (DS-LINK.NEGPAR LINK))
	      (EQL LITNO (DS-LINK.NEGLITNO LINK)))
	 (DS-LINK.NEGFCT LINK))
	(T (DS-LINK.POSFCT LINK))))

(DEFUN DS-LINK.OTHERFCT (LINK CLAUSE LITNO TAF)
					; INPUT : LINK CLAUSE LITNO TAF
					;         LINK CONNECTS(CLAUSE LITNO) WITH SOME OTHER
					;         LITERAL
					; VALUE : THE TERM ACCESS FUNCTION OF THE LITERAL
					;         CONNECTED TO (CLAUSE LITNO) BY LINK
					;         IF LINK IS INTERNAL , THEN THE VALUE IS
					;         ARBITARILY ONE OF THE TWO TAFS
  (COND ((EQL (DS-LINK.NEGFCT LINK)
	      (DS-LINK.THISFCT LINK CLAUSE LITNO TAF))
	 (DS-LINK.POSFCT LINK))
	(T (DS-LINK.NEGFCT LINK))))

(DEFUN DS-LINK.THISSIDE (LINK CLAUSE LITNO TAF)
						; INPUT : LINK CLAUSE LITNO TAF
						;
						; VALUE : (THISPAR THISLITNO THISFCT) OF LINK
  (LIST CLAUSE LITNO (DT-TAF.TOPLEVEL (DS-LINK.THISFCT LINK CLAUSE LITNO TAF))))

(DEFUN DS-LINK.OTHERSIDE (LINK CLAUSE LITNO TAF)
						; INPUT : LINK CLAUSE LITNO TAF
						;         LINK CONNECTS THE LITNO'TH LITERAL OF CLAUSE
						; VALUE : THE LITERAL CONNECTED TO(CLAUSE LITNO) BY
						;         LINK,IN THE FORM(CLAUSE1 LITNO1 TAF1)
  (PROG ((OTHERPAR (DS-LINK.OTHERPAR LINK CLAUSE)) (OTHERLITNO (DS-LINK.OTHERLITNO LINK CLAUSE TAF)))
	(RETURN
	  (COND
	    ((DS-CLAUSE.IS.EQUATION OTHERPAR OTHERLITNO) (LIST OTHERPAR OTHERLITNO (DS-LINK.OTHERFCT LINK CLAUSE LITNO TAF)))
	    (T (LIST OTHERPAR OTHERLITNO))))))

(DEFUN DS-LINK.FUNCTIONSYMBOLS (LINK)
						; EDITED: 26-OCT-83 11:13:15
						; INPUT:  A LINK
						; VALUE:  THE LIST OF FUNCTIONSYMBOLS OF THE SUPER-
						;         TERMS OF NEGTERM (GIVEN BY NEGPAR NEGLITNO
						;         AND NEGFCT OF LINK)
  (PROG
    ((NEGPAR (DS-LINK.NEGPAR LINK)) (NEGLITNO (DS-LINK.NEGLITNO LINK))
     (TAF (DS-LINK.NEGFCT LINK)) TERMLIST FS TERM)
    (SETQ TERMLIST (DS-CLAUSE.TERMLIST NEGPAR NEGLITNO))
    (MAPC #'(LAMBDA (ARG)
	      (SETQ TERM (CAR (NTHCDR (1- ARG) TERMLIST)))
	      (SETQ FS (CONS (CAR TERM) FS)) (SETQ TERMLIST (CDR TERM)))
	  (NREVERSE (CDR (REVERSE TAF))))
    (RETURN FS)))


(DEFMACRO DS=LINK.GETCOLOUR (LINK)
						; EDITED: "26-NOV-80 15:33:00")
						; INPUT:  LINK ADDRESS
						; EFFECT: RETURNS VALUE
						; VALUE:  COLOUR OF GIVEN LINK
  `(MEM-TYPE ,LINK))

(DEFMACRO DS=LINK.IS (LINK)
						; EDITED: "25-NOV-80 16:44:56")
						; INPUT:  LINK ADDRESS
						; EFFECT: RETURNS VALUE
						; VALUE:  T IF 'LINK' IS ADDRESS OF LINK NOT
						;         YET DELETED, ELSE NIL
  `(MEMBER (MEM-TYPE ,LINK) DS*LINK.TYPES))

(DEFMACRO DS=LINK.STORAGE.SIZE (COLOUR)
						; EDITED: 14-FEB-83 15:53:22
						; INPUT:  A LINK COLOUR
						; VALUE:  NUMBER OF STORAGE CELLS ALLOCATED FOR THIS
						;         LINK TYPE.
  (declare (ignore colour))
  'DS*LINK.STORAGE.SIZE)

(DEFUN DS=LINK.COMPUTE.DIRECTION (PAR1 PAR2 LITNO1 LITNO2 TAF1 TAF2)
						; EDITED:  13-JUL-1990 00:17
						; Authors: PRCKLN
						; INPUT: 'LITNOI' IS A LITERAL NUMBER OF A LITERAL IN
						;        THE CLAUSE WITH ADDRESS 'PARI'. IN CASE OF
						;        PARAMODULATION LINK THE CORRESPONDING TERM
						;        ACCESS FUNCTION IS 'TAFI'; I = 1, 2.
						;        'COLOUR' IS A LINK TYPE.
						;         For P-links the equation must be always in PAR1
						; EFFECT:THE GLOBAL VARIABLES DS.POSPAR, DS.NEGPAR,
						;        DS.POSLITNO, DS.NEGLITNO, DS.POSFCT,
						;        DS.NEGFCT, DEFINED IN DS-LINK.CREATE WILL BE
						;        DETERMINED, CORRESPONDING THE COMMENT IN THE
						;        FUNCTION.
						; VALUE: UNDEFINED.
  (DECLARE (SPECIAL DS.NEGFCT DS.NEGLITNO DS.NEGPAR DS.POSFCT DS.POSLITNO DS.POSPAR))
  (COND ((NOT PAR2) (SETQ PAR2 PAR1)))
  (SETQ DS.POSPAR PAR1 DS.POSLITNO LITNO1 DS.NEGPAR PAR2 DS.NEGLITNO LITNO2 DS.POSFCT taf1 DS.NEGFCT taf2))

(DEFVAR DS*DUMMY.ATOM NIL)



(DEFVAR DS*EQUALITY.SYMBOLS '("=" ":=" "=:" ":=:"))

(DEFUN DS-RESET NIL
						; Edited:  13-JUL-1990 00:15
						; Authors: PRCKLN
						; VALUE:  UNDEFINED 
						; EFFECT: DELETES ALL VARIABLE, CONSTANT, FUNCTION, PREDICATE,
						;         CLAUSE, OR LINK SYBOLS AND SETS THE SYSTEM
						;         TO AN INITIAL STATE
  (SETQ DS*DUMMY.ATOM NIL)
  (MAPC (FUNCTION REMPROPS) DS*LINK.TYPES)
  (SETQ DS*RULES NIL)
  (setq ds*finite.domain nil)
  (ord-reset))

(DEFUN DS-TYPE (OBJECT)
						; EDITED: "28-NOV-80 14:30:43")
						; VALUE:  EITHER OF THE ATOMS
						;         VARIABLE, CONSTANT, FUNCTION, PREDICATE, CLAUSE,
						;         OR A LINKCOLOUR DEPENDING ON OBJECT. IF OBJECT IS NONE OF
						;         THESE OR HAS BEEN DELETED, NIL 
  (MEM-TYPE OBJECT))

(DEFMACRO DS-GET.TYPE (ADR)
  `(MEM-GET.TYPE ,ADR))

(DEFUN DS-PNAME (OBJECT)
						; EDITED: 14-MAR-83 12:08:23        NE
						; INPUT:  ARBITRARY S-EXPRESSION.
						; VALUE:  SAME EXPRESSION, WHERE ALL DS-OBJECTS ARE
						;         REPLACED BY THEIR PNAMES.
  (COND ((CONSP OBJECT) (CONS (DS-PNAME (CAR OBJECT)) (DS-PNAME (CDR OBJECT))))
	(T (CASE (DS-TYPE OBJECT) (CLAUSE (DS-CLAUSE.PNAME OBJECT)) (OTHERWISE
								      (DT-PNAME OBJECT))))))

(DEFMACRO DS-UNI.CREATES.VARIABLES NIL
						; EDITED: 27. 2. 1984
						; VALUE:  T IF THE UNIFICATION ALGORITHM MAY
						;         CREATE NEW VARIABLES
						;         (ASSOCIATIVITY, POLYMORPHIC FUNCTIONS,
						;          RULES ETC.), ELSE NIL
  `(DT-UNI.CREATES.VARIABLES))

(DEFUN DS-SAVE (FILE)
						; INPUT:  A FILENAME OR NIL
						; VALUE:  IF FILE = NIL, AN S-EXPRESSION WHICH
						;         IF IT IS EVALUATED RESTORES THE VERY SAME
						;         DS-SYSTEM-STATE AS IT WAS WHEN DS-SAVE
						;         WAS CALLED
						;         ELSE NIL
						; EFFECT: IF FILE =//= NIL THE S-EXPRESSION IS
						;         WRITTEN ON FILE
						;         ELSE RETURNS VALUE
						; REMARK: THE FILE IS EXPECTED TO BE OPEN AND
						;         REMAINS SO
  (PROG
    ((EXPRESSION.COMMONVARS
       (CONS 'PROGN (MAPCAR #'(LAMBDA (VAR) (LIST 'setq VAR `',(SYMBOL-VALUE VAR))) '(DS*RULES ds*finite.domain))))
     (EXPRESSION.OTHERVARS
       (CONS 'PROGN (MAPCAR #'(LAMBDA (ATOM) (LIST 'setq ATOM `',(SYMBOL-VALUE ATOM))) nil)))
     (EXPRESSION.COMMON.PROPLISTS (CONS 'PROGN (MAPCAR #'(LAMBDA (ATOM) (SAVE-PROPLIST ATOM '(ds*))) nil)))
     (EXPRESSION.OTHER.PROPLISTS (CONS 'PROGN (MAPCAR #'(LAMBDA (ATOM) (SAVE-PROPLIST ATOM '(ds*))) nil))))    
    (ord-reset)
    (COND
      ((NULL FILE)
       (RETURN
	 (LIST 'PROGN '(DS-RESET) (DT-SAVE NIL) EXPRESSION.COMMONVARS EXPRESSION.OTHERVARS EXPRESSION.COMMON.PROPLISTS
	       EXPRESSION.OTHER.PROPLISTS)))
      (T (PROGN (PRINC (LIST 'PROGN '(DS-RESET)) FILE) (TERPRI FILE))
	 (DT-SAVE FILE)
	 (PROGN (PRINC EXPRESSION.COMMONVARS FILE) (TERPRI FILE))
	 (PROGN (PRINC EXPRESSION.OTHERVARS FILE) (TERPRI FILE))
	 (PROGN (PRINC EXPRESSION.COMMON.PROPLISTS FILE) (TERPRI FILE))
	 (PROGN (PRINC EXPRESSION.OTHER.PROPLISTS FILE) (TERPRI FILE))))))

(DEFUN DS=FIND.VARIABLES (OBJECT VARIABLES)
						; EDITED: 30-MAY-83 13:56:51        MW
						; INPUT: S-EXPRESSION AND LIST OF VARIABLES
						; VALUE: THE UNION OF VARIABLES WITH ALL VARIABLE
						;        SYMBOLS OCCURING IN OBJECT.
  (COND ((DT-VARIABLE.IS OBJECT)
	 (setq variables (INS OBJECT VARIABLES))
	 (if (opt-get.option sort_literals)
	     (DS=FIND.VARIABLES (dt-variable.sort object) variables)
	     variables))
	((CONSP OBJECT)
	 (MAPC #'(LAMBDA (SUBOBJECT) (SETQ VARIABLES (DS=FIND.VARIABLES SUBOBJECT VARIABLES)))
	       OBJECT)
	 VARIABLES)
	(T VARIABLES)))

(DEFUN DS=TERM.FUNCTIONSYMBOL (TERM)
						; EDITED: 26-OCT-83 11:13:15
						; INPUT:  A TERM
						; VALUE:  FUNCTION SYMBOL OF TERM.
  (COND ((CONSP TERM) (CAR TERM)) (T (ERROR "ILLEGAL ARGUMENT IN DS=TERM.FUNCTIONSYMBOL~A" TERM))))

(DEFUN DS=TERM.VAR.OCCUR (VARIABLE TERM TAF)
						; EDITED:  5-MAY-83 18:03:25
						; INPUT:   A VARIABLE, A TERM AND A TERM ACCESS FUNC-
						;          TION (TAF). TERM MAY BE THE SUBTERM OF SOME
						;          SUPERTERM OR TERMLIST S DENOTED BY TAF.
						; VALUE:   A LIST OF TERM ACCESS FUNCTIONS DENOTING
						;          THE OCCURRENCES OF VARIABLE IN TERM. THE
						;          TAFS IN THE RESULTING LIST REFER TO THE
						;          SUPERTERM (OR TERMLIST) S.
  (COND ((EQL VARIABLE TERM) (LIST TAF))
	((CONSP TERM) (SETQ TERM (CDR TERM)) (SETQ TAF (DT-TAF.CREATE.FIRST TAF))
	 (MAPCAN
	   #'(LAMBDA (SUBTERM) (PROG1 (DS=TERM.VAR.OCCUR VARIABLE SUBTERM TAF) (SETQ TAF (DT-TAF.CREATE.NEXT TAF))))
	   TERM))))


;;; Rewrite rules
;;; -------------