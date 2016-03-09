;;; -*- Syntax: Common-Lisp; Package: MKRP; Base: 10; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

(defvar PP*LEFT.MARGIN    3)
(defvar PP*RIGHT.MARGIN 117)

(defvar PP*NEGATIVE.SIGN     "- ")
(defvar PP*POSITIVE.SIGN     "+ ")
(defvar PP*POSITIVE.SIGNS    '(+ ++))
; (defvar PP*NEGATIVE.SIGNS    '(- --)) Not used
(defvar PP*EMPTY.CLAUSE.SIGN "[]")
(defvar PP*DISJUNCTION.SIGN  "  ")


;;;
;;; In all the following print functions the keyword parameters current.pos, left.pos, right.pos have the same meaning:
;;;
;;; Standard call (PP-Print.object object file &key (current.pos (opt-get.option pr_left.margin))
;;;                                                 (left.pos current.pos) (right.pos pp*right.margin)
;;;						    (name.function #'dt-pname))
;;;
;;; CURRENT.POS   is the line-position, when the function is called, printing starts there ! (default = pp*left.margin)
;;; LEFT.POS	  if a new line is needed, printing continues there at left.pos.             (default = current.pos)
;;; RIGHT.POS	  the last position in every line to be used for printing the object.        (default = pp*right.margin)
;;; NAME.FUNCTION is applied to the object(s), when they are given as addresses		     (default = #'dt-pname)
;;;		  it must return a list containing strings naming the symbols used.
;;;
;;; All these functions return two values: the new line position (after printing) and a flag, if a new line was needed.
;;;



;;; ****************************************************************************************************
;;;						Infix Formulae
;;; ****************************************************************************************************


(defun PP-PRINT.INFIX.FORMULA (formula file &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN)) (RIGHT.POS PP*RIGHT.MARGIN))
  (PP=PRINT.INFIX.FORMULA FORMULA file :current.pos current.POS :right.pos RIGHT.POS :toplevel t))

(DEFUN PP=PRINT.INFIX.FORMULA (formula file &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN)) (RIGHT.POS PP*RIGHT.MARGIN)
			       toplevel)
						; edited: 22-jul-83 10:11:12  by cl
						; input : an s-expression, two integers, and a file
						; effect: prints the formula in a readable format
						; remark: it is assumed that the line position is correct at the beginning
						; value : undefined
  (COND ((< (+ current.pos (PP=INFIX.FORMULA.LENGTH.ONE.LINE FORMULA)) RIGHT.POS)	; i.e. formula fits in one line
	 (PP=PRINT.INFIX.FORMULA.ONE.LINE FORMULA FILE))
	((CONSP FORMULA)			; it doesn't fit, but has list structure
	 (let (new.pos new.line.flag separator (number.of.parentheses 1))	   
	   (while (and (null (rest formula))
		       (consp (first formula)))
	     (setq formula (first formula))
	     (incf number.of.parentheses))
	   (when toplevel (decf number.of.parentheses))
	   (format file "~v,,,'(A" number.of.parentheses "")
	   (incf current.pos number.of.parentheses)
	   (decf right.pos   number.of.parentheses)
	   (COND ((OR (MEMBER 'ALL FORMULA) (MEMBER 'EX FORMULA))	; it is a quantified formula
		  (multiple-value-setq (FORMULA new.pos new.line.flag)
		    (PP=EXTRACT.&.PRINT.INFIX.QUANTIFIERS FORMULA CURRENT.POS RIGHT.POS FILE))
		  (when new.line.flag (format file "~%~vA" (setq new.pos (+ current.pos 2)) ""))
		  (PP=PRINT.INFIX.FORMULA FORMULA file :current.pos new.pos :right.pos RIGHT.POS :toplevel t))
		 ((eq (first formula) 'not)
		  (princ "NOT " file)
		  (PP=PRINT.INFIX.FORMULA (second formula) file :current.pos (incf current.pos 4)))
		 ((setq separator (PP=FORMULA.SEPARATOR FORMULA))	; separator is a string!, it includes any blanks needed
		  (setq formula   (PP=SPLIT.INFIX.FORMULA FORMULA))
		  (PP=PRINT.INFIX.FORMULA (first FORMULA) file
					  :current.pos current.pos ;(+ CURRENT.POS (length separator))
					  :right.pos RIGHT.POS :toplevel t)
		  (MAPC #'(LAMBDA (SUBFORMULA)
			    (format file "~%~vA~A" CURRENT.POS "" separator)
			    (PP=PRINT.INFIX.FORMULA SUBFORMULA file
						    :current.pos (+ CURRENT.POS (length separator)) :right.pos RIGHT.POS
						    :toplevel t))
			(rest FORMULA)))
		 (T (format file "~A(" (first formula))			; formula = predicate symbol with arguments
		    (setq current.pos (+ current.pos (length (symbol-name (first formula))) 1))
		    (do ((subformula (first (rest formula))
				     (if (consp subformula)
					 (if (consp (second subformula))	; arg is a function with parameter list
					     (progn (PP=PRINT.INFIX.FORMULA (list (first SUBFORMULA) (second subformula))
									    file :current.pos CURRENT.POS
									    :right.pos (- RIGHT.POS 1)
									    :toplevel T)
						    (rest (rest subformula)))
					     (progn (format file "~A" (first subformula))
						    (rest subformula)))
					 (progn (PP=PRINT.INFIX.FORMULA.ONE.LINE subformula FILE)
						(PP=PRINT.INFIX.FORMULA.ONE.LINE (rest (rest formula)) FILE)))))
			((null subformula))
		      (unless (eq subformula (first (rest formula)))
			(format file "~%~vA" current.pos "")))
		    (princ ")" file)))
	   (format file "~v,,,')A" number.of.parentheses "")))
	(T (format file "~vA~A" (opt-get.option pr_left.margin) "" formula))))


(DEFUN PP=EXTRACT.&.PRINT.INFIX.QUANTIFIERS (FORMULA CONTINUE.POS RIGHT.POS FILE)
						; edited: 22-jul-83 10:40:58  by cl
						; input : a list beginning with 'EX or 'ALL, two integers, and an open file
						; effect: extracts the quantifiers 'all and 'ex
						;         with their variables from formula.		
						;	  Prints the quantifiers between continue.pos and right.pos on file.
						; values: the rest of the formula, the new line position, and a new line flag
						; remark: it is assumed, that the print position is already correct.
  (let (REST.FORMULA (new.pos continue.pos) (new.line.flag nil) pstring)
    (smapl #'(LAMBDA (TAIL)
	       (setq pstring (if (eql (THIRD TAIL) '|:|)
				 (format nil "~A ~A~A~A " (first tail) (SECOND TAIL) (third tail) (fourth tail))
				 (format nil "~A ~A "   (first tail) (SECOND TAIL))))
	       (princ pstring file)
	       (incf new.pos (length pstring)))
	   #'(LAMBDA (TAIL)
	       (cond ((member (THIRD TAIL) '(all ex)) (CDDR TAIL))
		     ((and (eql (THIRD TAIL) '|:|)
			   (member (fifth TAIL) '(all ex)))
		      (nthcdr 4 tail))
		     ((eql (THIRD TAIL) '|:|) (prog1 nil (SETQ REST.FORMULA (nthcdr 4 TAIL))))
		     (t (prog1 nil (SETQ REST.FORMULA (CDDR TAIL))))))
	   formula)
    (when (or (< (- right.pos continue.pos) (* 3 (- new.pos continue.pos)))
	      (< (- right.pos new.pos) 50)
	      (< (* 1.5 (- right.pos new.pos)) (- PP*RIGHT.MARGIN PP*LEFT.MARGIN)))
      (setq new.line.flag t))
    (values REST.FORMULA new.pos new.line.flag)))


(DEFVAR PP*INFIX.SEPARATORS '(EQV |:EQV:| |:EQV| |EQV:| IMPL |:IMPL:| |:IMPL| |IMPL:| OR AND = |:=:| |:=| |=:|))

(DEFUN PP=FORMULA.SEPARATOR (FORMULA)		; edited: 22-jul-83 09:53:03  by cl
						; input : an s-expression
						; value : a string (separator), if formula contains one of the following
						;		as top level element :
						;	  ( EQV :EQV: :EQV EQV: IMPL :IMPL: :IMPL IMPL: OR AND )
						;	  nil , else.
  (let ((sep (MEMBER-IF #'(LAMBDA (SEPARATOR) (MEMBER SEPARATOR FORMULA)) PP*INFIX.SEPARATORS)))
    (if sep (concatenate 'string (symbol-name (first sep)) " ") nil)))


(DEFUN PP=SPLIT.INFIX.FORMULA (FORMULA)		; edited: 20-jul-83 15:42:19  by cl
						; input : a list
						; effect: splits the list into several sublists
						; value : the list of these sublists (see PP=split)
  (let ((ACTUAL.SEPARATOR 
	  (some #'(LAMBDA (SEPARATOR) (first (MEMBER SEPARATOR FORMULA)))
		PP*INFIX.SEPARATORS)))
    (delete actual.separator (PP=SPLIT FORMULA ACTUAL.SEPARATOR))))


(DEFUN PP=SPLIT (FORMULA SEPARATOR)		; edited: 20-jul-83 15:16:09  by cl
						; input : a list and an atom (contained in list, but not as last element)
						;         x is the atom
						;	  e.g. (a b x c (e f) x u y x z)
						; effect: separates list into sublists
						; value : a list of these sublists, e.g.
						;	      < (a b) x (c (e f)) x (u y) x (z) >
  (let ((RESULT.PTR (qconc NIL nil))
	(EL.PTR (qconc nil NIL)))
    (MAPC #'(LAMBDA (SUBFORMULA)
	      (if (EQ SUBFORMULA SEPARATOR)
		  (progn (QCONC1 RESULT.PTR (CAR EL.PTR))
			 (SETQ EL.PTR (LIST NIL))
			 (QCONC1 RESULT.PTR SUBFORMULA))
		  (QCONC1 EL.PTR SUBFORMULA)))
	  FORMULA)
    (QCONC1 RESULT.PTR (CAR EL.PTR))
    (CAR RESULT.PTR)))


(DEFUN PP=PRINT.INFIX.FORMULA.ONE.LINE (FORMULA FILE)
						; edited: 19-jul-83 17:40:48  by cl
						; input : an infix formula (list or atom) and an open file
						; effect: prints the formula starting at the current position,
						;	  it is expected, that the formula fits in current line.
						; value : undefined
  (if (CONSP FORMULA)
      (MAPPRINT FORMULA NIL NIL " " #'PRINC FILE)
      (format file "~A  " FORMULA)))


(DEFUN PP=INFIX.FORMULA.LENGTH.ONE.LINE (FORMULA)
						; edited: 20-jul-83 10:05:19  by cl
						; input : an s-expression
						; value : the space needed to print the formula
						;         with PP=PRINT.INFIX.FORMULA.ONE.LINE
						; effect: -
  (if (consp FORMULA)
      (pp-sum formula #'(lambda (subformula)
			  (1+ (PP=infix.formula.length.one.line subformula))))
      (LENGTH (princ-to-string FORMULA))))



;;; ****************************************************************************************************
;;;						LITERALS
;;; ****************************************************************************************************


(DEFUN PP-PRINT.LITERALS (LITERALS file &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN))
			  (LEFT.POS CURRENT.POS) (RIGHT.POS PP*RIGHT.MARGIN)
			  (name.function #'dt-pname))
  
						; edited: 26-jul-83 11:22:17  by cl
						; input : A list ((sign pred . terms) ...) of literals, containing strings,
						;	  or a list of literal addresses, and an open file.
						; effect: Prints the literals between left.pos and right.pos in a readable format.
						;	  If necessary name.function is applied to the literals first.
						; values: The line position afterwards and a flag indicating new line.
  (setq literals (mapcar name.function literals))
  (PP=PRINT.Literals literals FILE :CURRENT.POS CURRENT.POS :LEFT.POS LEFT.POS :RIGHT.POS RIGHT.POS))


(DEFUN PP=PRINT.LITERALS (LITERALS file
			  &key (current.pos (opt-get.option pr_left.margin)) (left.pos current.pos) (RIGHT.POS pp*right.margin))
						; edited: 26-jul-83 11:22:17  by cl
						; input : A list ((sign pred . termlist) ...) of literals, and an open file.
						;         All the entries must be strings or symbols (no addresses !).
						; effect: Prints the literals between left.pos and right.pos in a readable format.
						; values: The line position afterwards and a flag indicating new line.
  (if LITERALS
      (if (<= (+ current.pos (- (pp-sum literals #'(lambda (literal)
						  (+ (PP=LITERAL.LENGTH.ONE.LINE literal) (LENGTH PP*DISJUNCTION.SIGN))))
				(LENGTH PP*DISJUNCTION.SIGN)))
	      RIGHT.POS)			; i.e. all literals fit into current line
	  (progn (MAPPRINT LITERALS NIL NIL PP*disjunction.sign
			   #'(lambda (literal file)
			       (setq current.pos (PP=PRINT.LITERAL.ONE.LINE literal FILE :current.pos current.pos)))
			   file)
		 (values current.pos nil))
	  (let ((new.line.flag nil))		; Several lines are needed
	    (mapc #'(lambda (literal)
		      (when (or new.line.flag (and (> current.pos left.pos)
						   (> (+ current.pos
							 (LENGTH PP*DISJUNCTION.SIGN)
							 (PP=LITERAL.LENGTH.ONE.LINE literal))
						      right.pos)))
			(format file "~%~vA" (setq current.pos left.pos) ""))
      		      (unless (or new.line.flag (= current.pos left.pos))
			  (princ PP*DISJUNCTION.SIGN file)
			  (incf current.pos (LENGTH PP*DISJUNCTION.SIGN)))
		      (multiple-value-setq (current.pos new.line.flag)
			(PP=PRINT.LITERAL LITERAL file :current.pos current.pos :right.pos RIGHT.POS)))
		  literals)
	    (Values current.pos T)))
      (values (+ current.pos (length (princ pp*empty.clause.sign file))) nil)))


(DEFUN PP-PRINT.LITERAL (LITERAL FILE &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN))
			 (LEFT.POS CURRENT.POS) (RIGHT.POS PP*RIGHT.MARGIN)
			 (name.function #'dt-pname))
  (DECLARE (IGNORE LEFT.POS))			; edited: 18-jul-83 10:57:42  by cl
						; input : A literal as an address or a list (sign pred . terms)
						; 	  (then the entries must be strings), and a file open for output.
						; effect: Prints literal on file in a readable format
						;	  between current position and right.pos, left.pos is ignored.
						; values: The new line position and a flag, if a new line was needed
  (setq literal (funcall name.function literal))
  (PP=PRINT.LITERAL LITERAL FILE :CURRENT.POS CURRENT.POS :RIGHT.POS RIGHT.POS))


(DEFUN PP=PRINT.LITERAL (LITERAL FILE &key (current.pos (opt-get.option pr_left.margin)) (RIGHT.POS pp*right.margin))
						; edited: 30-aug-84 15:36:00  by cl
						; input : a list (sign (pred . terms)), and an open file
						; effect: prints the literal in a readable format.
						; value : the new line position and a flag, if a new line was needed
  (if (MEMBER (CAR LITERAL) PP*POSITIVE.SIGNS)
      (progn (PRINC PP*POSITIVE.SIGN FILE)
	     (setq current.pos (+ current.pos (length PP*POSITIVE.SIGN))))
      (progn (PRINC PP*NEGATIVE.SIGN FILE)
	     (setq current.pos (+ current.pos (length PP*negative.SIGN)))))
  (let ((ATOM (SECOND LITERAL)))
    (if (CDR ATOM)				; i. e. predicate has a non-empty termlist
	(PP=PRINT.TERM ATOM FILE :current.pos current.pos :right.pos RIGHT.POS :lowercasep nil)
	(progn (PRINC (CAR ATOM) FILE)
	       (values (+ current.pos (length (string (car ATOM)))) nil)))))


(DEFUN PP=PRINT.LITERAL.ONE.LINE (LITERAL FILE &key (current.pos (opt-get.option pr_left.margin)))
						; edited: 20-jun-84 14:51:11  by cl
						; input : a list (sign (predicate . terms)) and an open file
						; effect: prints the literal in current line in a readable format
						; value : the new line position
  (if (MEMBER (CAR LITERAL) PP*POSITIVE.SIGNS)
      (progn (PRINC PP*POSITIVE.SIGN FILE)
	     (setq current.pos (+ current.pos (length PP*POSITIVE.SIGN))))
      (progn (PRINC PP*NEGATIVE.SIGN FILE)
	     (setq current.pos (+ current.pos (length PP*negative.SIGN)))))
  (let ((ATOM (SECOND LITERAL)))
    (if (CDR ATOM)				; i. e. predicate has a non-empty termlist
	(PP=PRINT.TERM.one.line ATOM FILE :current.pos current.pos :lowercasep nil)
	(progn (PRINC (CAR ATOM) FILE)
	       (+ current.pos (length (string (car ATOM)))) ))))


(DEFUN PP=LITERAL.LENGTH.ONE.LINE (LITERAL)	; edited: 22-jun-84 14:14:34  by cl
						; input : a list (sign (pred . terms))
						; value : the space needed to print the
						;	  literal with PP=PRINT.LITERAL.ONE.LINE
  (+ (if (MEMBER (CAR LITERAL) PP*POSITIVE.SIGNS)
	 (LENGTH PP*POSITIVE.SIGN)
	 (LENGTH PP*NEGATIVE.SIGN))
     (if (rest (second literal))			; i.e. predicate has a non-empty termlist
	 (PP=TERM.LENGTH.ONE.LINE (rest LITERAL))
	 (LENGTH (SECOND LITERAL)))))


;;; ****************************************************************************************************
;;;						TERMS
;;; ****************************************************************************************************



(DEFUN PP-PRINT.TERM (TERM FILE &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN)) (LEFT.POS CURRENT.POS) (RIGHT.POS PP*RIGHT.MARGIN)
		      (name.function #'dt-pname) (LOWERCASEP T))
  (DECLARE (IGNORE LEFT.POS))			; edited: 18-jul-83 10:57:42  by cl
						; input : A term as an address or in list representation,
						; 	  (then the entries must be strings), and a file open for output.
						;	  Unless lowercasep = nil printing will be in lower case letters.
						; effect: Prints term on file in a readable format
						;	  between current position and right.pos, left.pos is ignored.
						; values: The new line position and a flag, if a new line was needed
  (setq term (funcall name.function term))
  (PP=PRINT.TERM TERM FILE :CURRENT.POS CURRENT.POS :RIGHT.POS RIGHT.POS :LOWERCASEP LOWERCASEP))


(DEFUN PP=PRINT.TERM (TERM FILE &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN)) (RIGHT.POS PP*RIGHT.MARGIN) (LOWERCASEP T))
						; edited: 18-jul-83 10:57:42  by cl
						; input : A term in list representation, the entries should be strings, but 
						;         symbols will do as well, and a file open for output.
						;	  Unless lowercasep = nil printing will be in lower case letters.
						; effect: Prints term on file in a readable format
						;	  between current position and right.pos, left.pos is ignored.
						; values: The new line position and a flag, if a new line was needed
  (COND ((<= (+ CURRENT.POS (PP=TERM.LENGTH.ONE.LINE TERM)) RIGHT.POS)	; i.e. fits in current line
	 (VALUES (PP=PRINT.TERM.ONE.LINE TERM FILE :current.pos current.pos :lowercasep lowercasep) NIL))
	((CONSP TERM)			        ; needs more than one line and has list structure
	 (FORMAT file "~:[~A~;~(~A~)~](" LOWERCASEP (CAR TERM))	; print the function (or predicate) symbol and a parenthesis
	 (SETQ CURRENT.POS (+ CURRENT.POS (LENGTH (STRING (CAR TERM))) 1))
	 (LET ((SUBTERM.POS CURRENT.POS))	; now all the subterms are printed into seperate lines, starting at subtermpos.
	   (SETQ CURRENT.POS (PP=PRINT.TERM (FIRST (REST TERM)) FILE
					    :CURRENT.POS SUBTERM.POS :RIGHT.POS RIGHT.POS :LOWERCASEP T))
	   (MAPC #'(LAMBDA (SUBTERM)
		     (FORMAT FILE "~%~vA" SUBTERM.POS "")
		     (SETQ CURRENT.POS (PP=PRINT.TERM SUBTERM FILE
						      :CURRENT.POS SUBTERM.POS :RIGHT.POS RIGHT.POS :LOWERCASEP T)))
		 (REST (REST TERM)))
	   (IF (= CURRENT.POS RIGHT.POS)	; i.e. the last subterm really used the full line
	       (PROGN (FORMAT FILE "~%~vA)" SUBTERM.POS "") (VALUES (1+ SUBTERM.POS) T))
	       (PROGN (PRINC ")" FILE)                      (VALUES (1+ CURRENT.POS) T)))))
	(T #|(WARN "The name ~S of this symbol doesn't fit between ~D and ~D.~%~
                  Use shorter names or provide a larger linelength."
		 TERM CURRENT.POS RIGHT.POS)|#
	   (FORMAT file "~%~:[~A~;~(~A~)~](" LOWERCASEP TERM)
	   (VALUES (+ CURRENT.POS (LENGTH (STRING TERM))) T))))


(defun pp-print.term.one.line (TERM FILE &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN)) (LOWERCASEP T))
  (PP=PRINT.TERM.ONE.LINE TERM FILE :current.pos CURRENT.POS :lowercasep LOWERCASEP))


(DEFUN PP=PRINT.TERM.ONE.LINE (TERM FILE &KEY (CURRENT.POS (OPT-GET.OPTION PR_LEFT.MARGIN)) (LOWERCASEP T))
						; edited: 2-mar-83 18:18:48  ne + cl
						; input:  A term in list representation (possibly just a string), and a file.
						;	  lowercasep is a flag indicating, if the term is printed in lower case
						; effect: The term is printed on file such that the predicate symbol
						;	  (car of each list) occurs before the parenthesis.
						;	  it is assumed that the term fits into the current line.
						; value:  The new line position
  (IF (CONSP TERM)
      (PROGN (FORMAT file "~:[~A~;~(~A~)~]" LOWERCASEP (FIRST TERM))
	     (SETQ CURRENT.POS (+ CURRENT.POS (LENGTH (princ-to-string (FIRST TERM)))))
	     (MAPPRINT (REST TERM) "(" ")" " "
		       #'(LAMBDA (SUBTERM FILE)
			   (SETQ CURRENT.POS (PP=PRINT.TERM.ONE.LINE SUBTERM FILE :CURRENT.POS (1+ CURRENT.POS) :LOWERCASEP T)))
		       FILE)
	     (1+ CURRENT.POS))
      (PROGN (FORMAT file "~:[~A~;~(~A~)~]" LOWERCASEP TERM)
	     (+ CURRENT.POS (LENGTH (princ-to-string TERM))))))

(defun PP-TERM.LENGTH.ONE.LINE (term)
  (PP=TERM.LENGTH.ONE.LINE term))

(DEFUN PP=TERM.LENGTH.ONE.LINE (TERM)		; edited: 13-apr-83 13:28:41  ne
						; input:  A term in list representation (possibly just a string).
						; value:  The number of characters used, if expression is
  (IF (CONSP TERM)				;	  printed by PP=PRINT.TERM.ONE.LINE
      (LET ((RESULT 0))
	(MAPC #'(LAMBDA (SUBTERM)
		  (SETQ RESULT (+ RESULT 1 (PP=TERM.LENGTH.ONE.LINE SUBTERM))))
	      TERM)
	RESULT)
      (LENGTH (princ-to-string TERM))))



;;; ****************************************************************************************************
;;;					Auxiliary Functions
;;; ****************************************************************************************************


(DEFUN PP-SUM (LIST VALUE.FCT &optional mapfn2)
  (unless mapfn2 (setq mapfn2 #'cdr))
  (APPLY #'+ (SMAPCAR VALUE.FCT MAPFN2 LIST)))