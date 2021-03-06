;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: lisp -*-

(in-package "MKRP" :use '("CL"))

(defmacro ds-check (switch)
  `(ds=check ',switch))

(DEFUN DS=CHECK (SWITCH)
  ;; EDITED:  9-FEB-83 17:26:56
  ;; INPUT:   EITHER  ON  OR  OFF
  ;; VALUE:   MESSAGE ABOUT KIND OF SWITCH PERFORMED.
  ;; EFFECT:  CAUSES ALL INTERFACE FUNCTIONS TO PERFORM
  ;;          ADMISSIBILITY CHECKS ON THEIR ARGUMENTS.
  ;;   
  ;; FOUR COMMON VARIABLES ARE REQUIRED:
  ;; DS*CHECK:NONE : LIST OF FUNCTIONS WHICH SHALL NOT BE
  ;;          ADVISED.
  ;; DS*CHECK:ARBITRARY : LIST OF ARGUMENTS WHICH MAY BE
  ;;          ARBITRARY S-EXPRESSIONS
  ;; DS*CHECK:MANUAL : LIST WITH ELEMENTS
  ;;          (FUNCTION CHECKFUNCTION ARGUMENTS)
  ;;          ARGUMENTS HAVE TO BE EQUAL TO THE
  ;;          ARGUMENTS OF FUNCTION.
  ;; DS*CHECK:EQUIVALENT : LIST WITH ELEMENTS
  ;;          (ARG1 ARG2 ...) OF NAMES OF THE SAME TYPE.
  ;;          THE FIRST ELEMENT DETERMINES THE ACTUAL
  ;;          CHECKFUNCTION.
  (CASE SWITCH
    (ON
      (PROG
        (NOT.ADVISED)
        (MAPC
          (FUNCTION
            (LAMBDA (FUNCTION.CHECK)
              (PROG ((FUNCTION (CAR FUNCTION.CHECK)) (CHECK (CDR FUNCTION.CHECK)) ERROR)
                (MAPC
                  (FUNCTION
                    (LAMBDA (ARG1 ARG2) (COND ((EQL ARG2 'ARBITRARY)) ((NEQ ARG1 ARG2) (SETQ ERROR T)))))
                  (scl:ARGLIST FUNCTION) (CDR CHECK))
                (COND (ERROR (SETQ NOT.ADVISED (CONS FUNCTION NOT.ADVISED)))
                  (T (eval `(scl:ADVISE ,FUNCTION BEFORE name 0 ,(REMOVE 'ARBITRARY CHECK))))))))
          DS*CHECK_MANUAL)
        (COND
          (NOT.ADVISED
            (PRINC
              "THE FOLLOWING FUNCTIONS WHICH HAD TO BE MANUALLY ADVISED HAVE NO COMPATIBLE CHECKFUNCTIONS: ")
            (TERPRI) (PROGN (PRINC NOT.ADVISED) (TERPRI))))
        (SETQ NOT.ADVISED NIL)
	(COND
          (NOT.ADVISED
            (PRINC
              "THE FOLLOWING FUNCTIONS WHICH HAD TO BE AUTOMATICALLY ADVISED CANNOT BE ADVISED DUE TO MISSING CHECKFUNCTIONS")
            (TERPRI) (PROGN (PRINC NOT.ADVISED) (TERPRI)))))
      '(DS-CHECK SWITCHED ON))
    (OFF (EVAL (CONS 'global:UNADVISE DS*CHECK_MANUAL))
	 '(DS-CHECK SWITCHED OFF))
    (OTHERWISE '(INVALID ARGUMENT - TRY (DS-CHECK ON) OR (DS-CHECK OFF)))))

(DEFVAR DS*CHECK_NONE
  '(DS-CHECK DS-CLAUSE.IS DS-LINK.IS DS-PNAME DS-SAVE DS-TYPE DS-LINK.COLOURS.WITH DS-LINK.COLOURS.FOR))

(DEFVAR DS*CHECK_ARBITRARY
  '(EQ.SEPARATELY INDICATOR INDICATORS VALUE LIST UNIFIER OBJECT RENAMEFLAG LIT.PARENTS ATTRIBUTE ATTRIBUTES
    ELEM TYPENAME OTHERSIDES))

(DEFVAR DS*CHECK_EQUIVALENT
  '((SORT SORT1 SORT2 SUBSORT SUPERSORT) (TAF TAF1 TAF2) (PREDICATE PREDICATE1 PREDICATE2 DS.PREDICATE)
    (NUMBER UNIFIERNO STEPS DEPTH STEP) (SIGN SIGN1 SIGN2) (LINK DS.LINK) (FUNCTION DS.FUNCTION)))

(DEFVAR DS*CHECK_MANUAL
  '((DS-LINK.CREATE DS=CHECK_LINK.CREATE COLOUR UNIFIERS PAR1 LITNO1 PAR2 LITNO2 TAF1 TAF2)
    (DS-LINK.UNIFIERS DS=CHECK_LINKTYPES LINK DS*LINK.WITH.UNIFIERS)
    (DS-LINK.PUTUNIFIERS DS=CHECK_LINKTYPES LINK ARBITRARY DS*LINK.WITH.UNIFIERS)
    (DS-LINK.REMOVE.UNIFIER DS=CHECK_LINKTYPES LINK ARBITRARY DS*LINK.WITH.UNIFIERS)
    (DS-LINK.MARK DS=CHECK_LINK.MARK ATTRIBUTE DS.LINK)
    (DS-LINK.IS.MARKED DS=CHECK_LINK.MARK ATTRIBUTE DS.LINK)
    (DS-LINK.NEGPAR DS=CHECK_LINKTYPES LINK DS*LINK.types)
    (DS-LINK.NEGLITNO DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.NEGLIT DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.NEGFCT DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.NEGTERM DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.POSPAR DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.POSLITNO DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.POSLIT DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.POSFCT DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.POSTERM DS=CHECK_LINKTYPES LINK  DS*LINK.types)
    (DS-LINK.OTHERPAR DS=CHECK_LINK.OTHERPAR LINK THISPAR)
    (DS-LINK.OTHERLITNO DS=CHECK_LINK.OTHERPAR LINK THISPAR)
    (DS-LINK.DEPTH DS=CHECK_LINKTYPES LINK (APPEND DS*LINK.CLAUSE DS*LINK.ACTIVE.OPERATION))))

(DEFUN DS=CHECK_LIT (LIT) (DS=CHECK_SIGN (CAR LIT)) (DS=CHECK_TERMLIST (THIRD LIT))
  (COND ((DT-PREDICATE.IS (SECOND LIT))) (T (ERROR "(DS-CHECK) - ILLEGAL PREDICATE IN LIT: ~A" LIT))))

(DEFUN DS=CHECK_LITLIST (LITLIST) (EVERY (FUNCTION DS=CHECK_LIT) LITLIST))

(DEFUN DS=CHECK_CLAUSE (CLAUSE)
  ;; EDITED: "22-NOV-79 19:10:02")
  (COND ((DS=CLAUSE.IS CLAUSE)) (T (ERROR "(DS-CHECK) - ILLEGAL CLAUSE    SYMBOL: : ~A" CLAUSE))))

(DEFUN DS=CHECK_LINK (LINK) (COND ((DS=LINK.IS LINK)) (T (ERROR "(DS-CHECK) - ILLEGAL LINK SYMBOL: ~A" LINK))))

(DEFUN DS=CHECK_LINKTYPES (LINK LINKTYPES)
  ;; EDITED: "27-NOV-80 16:08:56")
  (COND ((AND (MEM-ADDRESS LINK) (MEMBER (MEM-TYPE LINK) LINKTYPES)))
    (T (ERROR "(DS-CHECK) - ILLEGAL LINK      SYMBOL: : ~A" LINK))))

(DEFUN DS=CHECK_LINK.CREATE (COLOUR UNIFIERS PAR1 LITNO1 PAR2 LITNO2 TAF1 TAF2)
  (declare (ignore unifiers))
  (DS=CHECK_COLOUR COLOUR)
  (DS=CHECK_CLAUSE PAR1)
  (COND
    ((MEMBER COLOUR (DS-LINK.COLOURS.FOR 'LITERALS))
     (COND ((MEMBER COLOUR (DS-LINK.COLOURS.WITH 'NEGPAR)) (DS=CHECK_CLAUSE PAR2))) (DS=CHECK_NUMBER LITNO1)
     (DS=CHECK_NUMBER LITNO2)
     (COND
       ((AND (< LITNO1 (1+ (DS-CLAUSE.NOLIT PAR1))) (OR (NULL PAR2) (< LITNO2 (1+ (DS-CLAUSE.NOLIT PAR2))))))
       (T (ERROR "(DS-CHECK) - ILLEGAL LITNO(S): ~A" (LIST LITNO1 LITNO2))))
     (COND ((OR TAF1 TAF2) (DS=CHECK_TAF TAF1) (DS=CHECK_TAF TAF2))))
    (T
     (COND
       ((AND (CONSP LITNO1)
	     (EVERY
	       (FUNCTION (LAMBDA (LITNO) (AND (DS=CHECK_NUMBER LITNO) (< LITNO (1+ (DS-CLAUSE.NOLIT PAR1)))))) LITNO1))
	T)
       (T (ERROR "(DS-CHECK) - ILLEGAL LITNOS: ~A" LITNO1))))))

(DEFUN DS=CHECK_LINK.MARK (ATTRIBUTE DS.LINK)
  (declare (ignore ds.link))
  (COND ((MEMBER ATTRIBUTE '(ACTIVE PASSIVE INHIBITED INHERITANCE.ONLY)))
    (T (ERROR "(DS-CHECK) - ILLEGAL ATTRIBUTE: ~A" ATTRIBUTE))))

(DEFUN DS=CHECK_SIGN (SIGN)
  ;; EDITED: "22-NOV-79 19:24:35")
  (COND ((OR (MEMBER SIGN DS*SIGN.PLUS.SYMBOLS) (MEMBER SIGN DS*SIGN.MINUS.SYMBOLS)))
    (T (ERROR "(DS-CHECK) - ILLEGAL SIGN: : ~A" SIGN))))

(DEFUN DS=CHECK_TERMLIST (TERMLIST)
  ;; EDITED: "22-NOV-79 19:37:00")
  (COND ((EVERY (FUNCTION DS=CHECK-TERM) TERMLIST)) (T (ERROR "(DS-CHECK) - ILLEGAL TERMLIST: : ~A" TERMLIST))))

(DEFUN DS=CHECK_TERM (TERM) (COND ((DS=CHECK-TERM TERM)) (T (ERROR "(DS-CHECK) - ILLEGAL TERM: ~A" TERM))))

(DEFUN DS=CHECK-TERM (TERM)
  ;; EDITED: "22-NOV-79 19:36:47")
  (COND ((NULL TERM) NIL) ((ATOM TERM) (OR (DT-VARIABLE.IS TERM) (DT-CONSTANT.IS TERM) (DT-ABBREVIATION.IS TERM)))
    ((CONSP TERM) (AND (DT-FUNCTION.IS (CAR TERM)) (EVERY (FUNCTION DS=CHECK_TERM) (CDR TERM))))))

(DEFUN DS=CHECK_NUMBER (NUMBER)
  (COND ((INTEGERP NUMBER)) (T (ERROR "(DS-CHECK) - ILLEGAL NUMBER : ~A" NUMBER))))

(DEFUN DS=CHECK_TAF (TAF)
  (COND ((EVERY (FUNCTION (LAMBDA (ELEMENT) (AND (INTEGERP ELEMENT) (NOT (MINUSP ELEMENT))))) TAF))
    (T (ERROR "(DS-CHECK) - ILLEGAL TERM ACCESS FUNCTION: ~A" TAF))))

(DEFUN DS=CHECK_PARENTS (PARENTS)
  ;; EDITED: "22-NOV-79 19:26:34")
  (COND ((OR (NULL PARENTS) (ATOM PARENTS) (EVERY (FUNCTION ATOM) PARENTS)))
    (T (ERROR "(DS-CHECK) - ILLEGAL PARENTS: : ~A" PARENTS))))

(DEFUN DS=CHECK_LINK.OTHERPAR (LINK THISPAR) (DS=CHECK_LINK LINK) (DS=CHECK_CLAUSE THISPAR))

(DEFUN DS=CHECK_LITNO (LITNO clause)
						; EDITED: "22-NOV-79 19:40:42"
  (COND ((NOT (INTEGERP LITNO)) (ERROR "(DS-CHECK) - LITNO NOT SMALL INTEGER: : ~A" LITNO))
	((< LITNO 1)            (ERROR "(DS-CHECK) - LITNO SMALLER THAN 1:: ~A" LITNO))
	((> LITNO (DS=CLAUSE.GET 'NOLIT CLAUSE)) (ERROR "(DS-CHECK) - LITNO GREATER THAN CLAUSE.NOLIT: : ~A" LITNO))))

(DEFUN DS=CHECK_NOLIT (NOLIT)
  ;; EDITED: "22-NOV-79 19:47:41")
  (COND ((NOT (INTEGERP NOLIT)) (ERROR "(DS-CHECK) - NOLIT NOT SMALL INTEGER: : ~A" NOLIT))
    ((MINUSP NOLIT) (ERROR "(DS-CHECK) - NEGATIVE NOLIT:: ~A" NOLIT))))

(DEFUN DS=CHECK_COLOUR (COLOUR)
  (COND ((MEMBER COLOUR DS*LINK.TYPES)) (T (ERROR "(DS-CHECK)-ILLEGAL COLOUR : : ~A" COLOUR))))

(DEFUN DS=CHECK_COLOUR.S (COLOUR.S)
  (COND
    ((OR (AND (ATOM COLOUR.S) (MEMBER COLOUR.S DS*LINK.TYPES))
       (AND (CONSP COLOUR.S) (EVERY (FUNCTION (LAMBDA (COLOUR) (MEMBER COLOUR DS*LINK.TYPES))) COLOUR.S))))
    (T (ERROR "(DS-CHECK) - ILLEGAL COLOUR(S) : ~A" COLOUR.S))))

(DEFUN DS=CHECK_PREDICATES (PREDICATES)
  (COND ((OR (NULL PREDICATES) (AND (CONSP PREDICATES) (EVERY (FUNCTION DS=CHECK_PREDICATE) PREDICATES))))
    (T (ERROR "(DS-CHECK) ILLEGAL PREDICATES: ~A" PREDICATES))))

(DEFUN DS=CHECK_PREDICATE (PREDICATE)
  ;; EDITED: "22-NOV-79 19:09:24")
  (COND ((DT-PREDICATE.IS PREDICATE)) (T (ERROR "(DS-CHECK) - ILLEGAL PREDICATE SYMBOL: : ~A" PREDICATE))))

(DEFUN DS=CHECK_LITNOS (LITNOS clause) (EVERY #'(lambda (litno) (DS=CHECK_LITNO litno clause)) LITNOS))

(DEFUN DS=CHECK_RULE (RULE)
  (OR (MEMBER RULE '(NIL SYMMETRIC ASYMMETRIC))
    (AND (CONSP RULE) (DS-CLAUSE.IS (CAR RULE)) (MEMBER (SECOND RULE) '(1 2)) (MEMBER (THIRD RULE) '(1 2)) (NEQ (SECOND RULE) (THIRD RULE)))))

(DEFUN DS=CHECK_VARIABLE (VARIABLE)
  ;; EDITED: "22-NOV-79 19:07:51")
  (COND ((DT-VARIABLE.IS VARIABLE)) (T (ERROR "(DS-CHECK) - ILLEGAL VARIABLE SYMBOL: : ~A" VARIABLE))))

(DEFUN DS=CHECK_PNAME (PNAME) (COND ((ATOM PNAME)) (T (ERROR "(DS-CHECK) - ILLEGAL PNAME      : ~A" PNAME))))

