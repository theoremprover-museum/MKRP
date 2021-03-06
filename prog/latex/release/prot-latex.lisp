;;; -*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

(DEFVAR ppp*l_AXIOMS.FLAG T)

(DEFVAR ppp*l_INFIX.FLAG T)

(DEFVAR ppp*l_PREFIX.FLAG NIL)

(DEFVAR ppp*l_DIRECT.PROOF NIL)

(defvar ppp*l_indentation 5 "is used to replace the function to determine the current line position")

(DEFVAR ppp*l_LINE 0)

(DEFVAR ppp*l_FIRST.LINE 1)

(DEFVAR ppp*l_LAST.LINE 25)

(DEFVAR ppp*l_REAL.#.OF.LINES 27)


(DEFUN PPP=latex.PROTOCOL (FILE)
						; edited:  4-sep-84 16:03:29  by cl
						; input :  a file name
						; effect:  prints the total proof on the file
						; value :  fileversion of output file
    (PPP=L_DEFINE.COMMON.VARIABLES)
    (PPP=L_PRINT.HEAD (PDS-GET.PROOF.VERSION) (PDS-GET.RUN.DATE) (PDS-GET.COMMENT) FILE)
    ;(when (opt-get.option pr_options) (PPP=L_PRINT.OPTIONS     (PDS-GET.OPTIONS) FILE))
    (when ppp*l_INFIX.FLAG              (PPP=L_PRINT.INFIX.FORM  (PDS-GET.AXIOMS.INFIX)  (PDS-GET.THEOREMS.INFIX)  FILE))
    ;(when ppp*l_PREFIX.FLAG             (PPP=L_PRINT.PREFIX.FORM (PDS-GET.AXIOMS.PREFIX) (PDS-GET.THEOREMS.PREFIX) FILE))
    ;(when (opt-get.option pr_symbols) (PPP=L_PRINT.SYMBOLTABLE FILE))
    (when ppp*l_AXIOMS.FLAG
      (PPP=L_PRINT.AXIOM.CLAUSES (PDS-GET.AXIOMS) FILE)
      (PPP=L_PRINT.AX.OPERATIONS (PDS-GET.AX.OPERATIONS) FILE)
      (PPP=L_PRINT.SPLITTED.THEOREMS (PDS-ALL.PROOF.PARTS) FILE)
      (PPP=L_PRINT.INITIAL.OPERATIONS (PDS-ALL.PROOF.PARTS) FILE))
    (when (opt-get.option pr_literals) (format file "~2%~vA Literals: ~A~&~%" (opt-get.option pr_left.margin) "" (pds-get.indices)))
    (let (SPLITFLAG (NUMBER 0) (SPLITPARTS (PDS-ALL.SPLITPARTS)))
      (SETQ SPLITFLAG (> (LIST-LENGTH SPLITPARTS) 1))
      (format file "\\hrule")
      (MAPC #'(LAMBDA (SPLITPART)
		(when SPLITFLAG (incf NUMBER))
		(PPP=L_PRINT.SPLITPART SPLITPART NUMBER FILE)
		(format file "\\hrule"))
	    SPLITPARTS)
      (PPP=L_PRINT.TOTAL.RESULT FILE)
      (PPP=L_PRINT.TOTAL.STATISTICS FILE splitparts))
      ;(when (opt-get.option pr_statistics) (PPP=L_PRINT.TOTAL.STATISTICS FILE splitparts)))
    
    (TERPRI FILE)
    (truename file))


(DEFUN PPP=L_DEFINE.COMMON.VARIABLES ()		; edited: 4-sep-84 16:13:35  by cl
						; input : a list of dotted pairs
						; effect: defines the common variables necessary for the output format.
						; value : undefined
  (SETQ ppp*l_AXIOMS.FLAG (NEQ (PDS-GET.PROTOCOL.TYPE) 'SPLITPARTS)
	ppp*l_INFIX.FLAG  (AND (opt-get.option pr_infix.form)  ppp*l_AXIOMS.FLAG)
	ppp*l_PREFIX.FLAG (AND (opt-get.option pr_prefix.form) ppp*l_AXIOMS.FLAG)	
	ppp*l_DIRECT.PROOF (opt-get.option pr_direct.proof))
  (SETQ ppp*l_FIRST.LINE 0
	ppp*l_LINE ppp*l_FIRST.LINE
	ppp*l_LAST.LINE 57
	ppp*l_REAL.#.OF.LINES 58)
  (setq ppp*l_TOTAL.VARIABLE.NUMBER 0))



(DEFVAR ppp*l_USED.SIGN "* ")


(DEFVAR ppp*l_IMPLICATION.SIGN "$\\Rightarrow$ ")

(DEFVAR ppp*l_CONJUNCTION.SIGN "\\land ")

(DEFUN PPP=L_PRINT.HEAD (VERSION DATE COMMENT FILE)
						; Edited:  14-NOV-1990 23:08
						; Authors: PRCKLN
						; input : an atom, two lists, and an open file
						; effect: prints the head for the protocol,
						;	  top level elements of comment are printed
						;	  into separate lines.
						; value : undefined
  (format file "\\newlength{\\premisewidth}~%~
                \\newlength{\\namelength}~%~
                \\newlength{\\usedsignlength}~%~
                \\newlength{\\implsignlength}~%~
                \\newlength{\\remainlength}~%~
                \\def\\implication#1#2#3#4{% 1: String max length for 2, 2: premise, 3: clause 4. impl sign~%~
                   \\settowidth{\\premisewidth}{#1}~%~
                   \\setlength{\\remainlength}{\\textwidth}~%~
                   \\settowidth{\\usedsignlength}{~A}~%~
                   \\settowidth{\\implsignlength}{#4}~%~
                   \\settowidth{\\namelength}{~A}~%~
                   \\addtolength{\\remainlength}{-0.5cm}~%~
                   \\addtolength{\\remainlength}{-1\\premisewidth}~%~
                   \\addtolength{\\remainlength}{-1\\namelength}~%~
                   \\addtolength{\\remainlength}{-1\\usedsignlength}~%~
                   \\addtolength{\\remainlength}{-1\\implsignlength}~%~
                   \\hbox to \\hsize{\\parbox[t]{\\premisewidth}{#2} \\hfil \\parbox[t]{\\implsignlength}{#4} \\hfil #3}}"
	  ppp*l_USED.SIGN "mmmm")
  #|(let ((DAY (CONCATENATE 'STRING (PRINC-TO-STRING (CAR DATE)) "  " (PRINC-TO-STRING (SECOND DATE))))
	time NUMBER.OF.STARS *TAB COLON.POS END.TAB TEXT1 TEXT2)
    (SETQ NUMBER.OF.STARS (* 2 (truncate (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin)) 3)))
    (setq *TAB            (+ (opt-get.option pr_left.margin)
			     (TRUNCATE (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin)) 6))
	  COLON.POS (+ *TAB 16)
	  END.TAB   (+ *TAB NUMBER.OF.STARS -2))
    (COND ((< NUMBER.OF.STARS 36)
	   (CERROR "Try it anyway!" "A linelength of ~D characters per line is not enough for a decent protocol."
		   (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin)))
	   (setq text1 "MKRP" text2 "Uni KL")
	   (setq time ""  day ""))
	  ((< NUMBER.OF.STARS 46) (setq TEXT1 "M K R P ,"                           TEXT2 "UNI KAISERSLAUTERN"
					DAY (SUBSEQ DAY 0 11)                       TIME  (SUBSEQ DAY 12)))
	  ((< NUMBER.OF.STARS 57) (SETQ TEXT1 "M K R P , UNI KAISERSLAUTERN"))
	  ((< NUMBER.OF.STARS 71) (SETQ TEXT1 "MARKGRAF KARL REFUTATION PROCEDURE," TEXT2 "UNIVERSITAET KAISERSLAUTERN"))
	  (T                      (SETQ TEXT1 "MARKGRAF KARL REFUTATION PROCEDURE, UNI KAISERSLAUTERN")))
    (when (> (length version) (- end.tab colon.pos 3))
      (setq version (subseq version (search "MKRP" version :from-end T))))
    (format file "~%~%~%~v@A~v,,,'*A" *tab '* (1- number.of.stars) "")    
    (PPP=L_PRINT.HEAD.LINE *TAB "" COLON.POS "" END.TAB FILE)
    (PPP=L_PRINT.HEAD.LINE *TAB "ATP-SYSTEM:" COLON.POS TEXT1 END.TAB FILE)
    (when TEXT2 (PPP=L_PRINT.HEAD.LINE *TAB "" COLON.POS TEXT2 END.TAB FILE))
    (PPP=L_PRINT.HEAD.LINE *TAB "" COLON.POS "" END.TAB FILE)
    (PPP=L_PRINT.HEAD.LINE *TAB "VERSION:" COLON.POS VERSION END.TAB FILE)
    (PPP=L_PRINT.HEAD.LINE *TAB "DATE:" COLON.POS DAY END.TAB FILE)
    (when TIME (PPP=L_PRINT.HEAD.LINE *TAB "" COLON.POS TIME END.TAB FILE))
    (PPP=L_PRINT.HEAD.LINE *TAB "" COLON.POS "" END.TAB FILE)
    (format file "~%~v@A~v,,,'*A" *tab '* (1- number.of.stars) ""))
  (format file "~2%~vA" (opt-get.option pr_left.margin) "")
  (when comment
    (format file "\\hrule"))
  (MAPC #'(LAMBDA (LINE)
	    (PPP=L_PRINT.TEXT (princ-to-string LINE) file :continue.pos (opt-get.option pr_left.margin))
	    (PPP=L_TERPRI FILE))
	COMMENT)
  (when comment
    (format file "\\hrule"))|#)

(DEFUN PPP=L_PRINT.HEAD.LINE (BEGIN PREFIX MIDDLE TEXT END FILE)
						; edited: 11-jul-83 16:37:45  by cl
						; input : begin, middle, and end are tab positions,
						;	  prefix and text are strings
						; effect: prefix is written before middle (to right end),
						;	  text after middle, at positions begin and end a '* is printed.
						; value : undefined
  (format file "~%~v@A~v@A ~A~v@A" begin '* (- MIDDLE begin) prefix text (- end middle (length text)) '*))


(DEFUN PPP=L_PRINT.OPTIONS (OPTIONS FILE)		; edited:  6-jul-83 09:54:46  by cl
						; input :  a list of dotted pairs and an open file
						; effect:  prints the values of the options onto the
						;	     file, if possible in two coloumns.
						; value :  undefined

  (let ((TAB1 (opt-get.option pr_left.margin)) TAB2
	(TAB3 (+ (opt-get.option pr_left.margin)
		 (TRUNCATE (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin)) 2)))
	TAB4)

						; definition of tabulators according to length of options.
						; tab3+4 are needed for option names and values, when printed in two columns

    (let (MAX.NAME.LENGTH MAX.VALUE.LENGTH MAX.OPTION.LENGTH)
      (SETQ MAX.NAME.LENGTH   (LENGTH (symbol-name (CAR (MAXELT OPTIONS
								#'(LAMBDA (OPTION) (LENGTH (symbol-name (CAR OPTION))))))))
	    MAX.VALUE.LENGTH  (print-LENGTH (CDR (MAXELT OPTIONS #'(LAMBDA (OPTION) (print-LENGTH (CDR OPTION))))) )
	    MAX.OPTION.LENGTH (- (print-LENGTH (MAXELT OPTIONS #'(LAMBDA (OPTION) (PRINT-LENGTH OPTION NIL))) ) 4))
      (COND ((> (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin))
		(+ 3 (* 2 (+ MAX.NAME.LENGTH 2 MAX.VALUE.LENGTH))))
	     (SETQ TAB2 (+ TAB1 MAX.NAME.LENGTH 2)
		   TAB4 (+ TAB3 MAX.NAME.LENGTH 2)))
	    ((> (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin))
		(+ 3 (* 2 MAX.OPTION.LENGTH)))    (SETQ TAB2 -3   TAB4 0))
	    (T (SETQ TAB2 (+ (opt-get.option pr_left.margin) MAX.NAME.LENGTH 2)) (SETQ TAB3 TAB1 TAB4 TAB2))))

						; print options
    
    (let (NEXT.LINE.FLAG (PREVIOUS.PREFIX "") (current.pos (opt-get.option pr_left.margin)))
      (format file "~2%~vA" (opt-get.option pr_left.margin) "")
      (PPP=L_print.with.underscore "Adjustment of the Options:" file)
      (SMAPL #'(LAMBDA (OPTIONS.TAIL)
		 (let (CURRENT.PREFIX 1ST.NAME (1ST.OPTION (first OPTIONS.TAIL)) 2ND.NAME (2ND.OPTION (SECOND OPTIONS.TAIL)))
		   (SETQ 1ST.NAME       (CAR 1ST.OPTION)
			 CURRENT.PREFIX (SUBSEQ (symbol-name 1ST.NAME) 0 (search "_" (symbol-name 1ST.NAME) :start2 1)))
		   (format file "~:[~%~;~2%~]~vA" (STRING/= PREVIOUS.PREFIX CURRENT.PREFIX) (opt-get.option pr_left.margin) "")
		   (setq current.pos (opt-get.option pr_left.margin))
		   (progn (FORMAT FILE "~vA~A" (- TAB1 current.pos) "" 1st.name)
			  (setq current.pos (+ tab1 (length (symbol-name 1ST.NAME)))))
		   (if (> TAB2 0)		; i.e. printing names and values left-bound
		       (progn (FORMAT FILE "~vA~A" (- TAB2 current.pos) "" (CDR 1ST.OPTION))
			      (setq current.pos (+ tab2 (length (princ-to-string (CDR 1ST.OPTION))))))
		       (progn (format file "~v@A" (- (+ tab3 tab2) current.pos) (CDR 1ST.OPTION))
			      (setq current.pos (+ tab3 tab2))))
		   (SETQ PREVIOUS.PREFIX CURRENT.PREFIX)
		   (when (and 2ND.OPTION (/= tab1 tab3))
		     (SETQ 2ND.NAME (CAR 2ND.OPTION))
		     (SETQ CURRENT.PREFIX (SUBSEQ (symbol-name 2ND.NAME) 0 (search "_" (symbol-name 2ND.NAME) :start2 1)))
		     (COND ((STRING= PREVIOUS.PREFIX CURRENT.PREFIX)
			    (FORMAT FILE "~vA~A" (- TAB3 current.pos) "" 2nd.name)
			    (setq current.pos (+ tab3 (length (symbol-name 2nd.name))))
			    (if (> TAB4 0)	; i.e. printing names and values left-bound
				(progn (FORMAT FILE "~vA~A" (- TAB4 current.pos) "" (CDR 2nd.OPTION))
				       (setq current.pos (+ tab4 (length (princ-to-string (CDR 2nd.OPTION))))))
				(progn (format file "~v@A" (- (+ (opt-get.option pr_right.margin) tab4) current.pos)
					       (CDR 2nd.OPTION))
				       (setq current.pos (+ (opt-get.option pr_right.margin) tab4))))
			    (SETQ PREVIOUS.PREFIX CURRENT.PREFIX))
			   (T (SETQ NEXT.LINE.FLAG T))))))
	     #'(LAMBDA (TAIL)
		 (if (or NEXT.LINE.FLAG (= tab1 tab3))
		     (PROGN (SETQ NEXT.LINE.FLAG NIL) (CDR TAIL))
		     (CDDR TAIL)))
	     OPTIONS))))


(DEFUN PPP=L_PRINT.INFIX.FORM (AXIOMS THEOREMS FILE)
						; edited: 18-jul-83 17:09:02  by cl
						; input : two lists and an open file
						; effect: prints the sets of axioms and theorems in
						;	  infix form ,i.e. as they were given to the editor.
						; value : undefined
  (progn (format file "~5%~vA" (+ (opt-get.option pr_left.margin) 10) "")
	 (PPP=L_print.with.underscore "Formulae given to the editor" file (+ (opt-get.option pr_left.margin) 10))
	 (format file "~2%~vA" (opt-get.option pr_left.margin) ""))  
  (let ((ppp*l_indentation (+ (opt-get.option pr_left.margin) 10)))
    (when AXIOMS
      (format FILE "~%\\parbox[t]{2cm}{Axioms:}\\hfill ")
      (format FILE "~%\\setlength{\\remainlength}{\\textwidth}~%~
                      \\addtolength{\\remainlength}{-2.5cm}")
      (format FILE "~%\\parbox[t]{\\remainlength}{")
      (MAPC #'(LAMBDA (AXIOM)
		(PP-l_PRINT.INFIX.FORMULA AXIOM file :current.pos ppp*l_indentation :right.pos (opt-get.option pr_right.margin))
		(format file "~%~%~vA" ppp*l_indentation ""))	; no more than 1 NEWLINE after each formula (SIG)
	    AXIOMS)
      (format FILE "~%}"))
    (when THEOREMS
      (format FILE "~%\\parbox[t]{2cm}{Theorems:}\\hfill ")
      (format FILE "~%\\setlength{\\remainlength}{\\textwidth}~%~
                      \\addtolength{\\remainlength}{-2.5cm}")
      (format FILE "~%\\parbox[t]{\\remainlength}{")
      (MAPC #'(LAMBDA (THEOREM)
		(PP-l_PRINT.INFIX.FORMULA THEOREM file :current.pos ppp*l_indentation :right.pos (opt-get.option pr_right.margin))
		(format file "~%~%~vA" ppp*l_indentation ""))	; no more than 1 NEWLINE after each formula (SIG)
	    THEOREMS)
      (format FILE "~%}"))))


(DEFUN PPP=L_PRINT.PREFIX.FORM (AXIOMS THEOREMS FILE)
						; edited: 26-jul-83 10:01:02  by cl
						; input : two lists and an open file
						; effect: prints the sets of axioms and theorems in
						;	  prefix form
						; value : undefined
  (progn (format file "~5%~vA" (+ (opt-get.option pr_left.margin) 10) "")
	 (PPP=L_print.with.underscore "INPUT FORMULAE IN PREFIX FORM" file (+ (opt-get.option pr_left.margin) 10))
	 (format file "~3%~vA" (opt-get.option pr_left.margin) ""))
  (let ((ppp*l_indentation (+ (opt-get.option pr_left.margin) 10)))
    (when AXIOMS
      (PRINC "Axioms:   " FILE)
      (MAPC #'(LAMBDA (AXIOM) (PPP=L_PRINT.PREFIX.FORMULA AXIOM ppp*l_indentation (opt-get.option pr_right.margin) FILE)
		      (format file "~%~vA" ppp*l_indentation ""))
	    AXIOMS)
      (format file "~%~vA" (opt-get.option pr_left.margin) ""))
    (when THEOREMS
      (PRINC "Theorems: " FILE)
      (MAPC #'(LAMBDA (THEOREM) (PPP=L_PRINT.PREFIX.FORMULA THEOREM ppp*l_indentation (opt-get.option pr_right.margin) FILE)
		      (format file "~%~vA" ppp*l_indentation ""))
	    THEOREMS)
      (format file "~%~v@A" (opt-get.option pr_left.margin) ""))))


(DEFUN PPP=L_PRINT.PREFIX.FORMULA (FORMULA &optional LEFT.POS RIGHT.POS (FILE *standard-output*))
						; edited: 26-jul-83 09:29:30  by cl
						; input : an s-expression, two integers, and a file
						; effect: prettyprints the s-expression
						; value : undefined
  (declare (ignore left.pos right.pos))
  (princ FORMULA file))


(DEFUN PPP=L_PRINT.SYMBOLTABLE (FILE)
  (case (PDS-GET.PROTOCOL.TYPE)
    (CONSTRUCTION     (PPP=L_RECONSTRUCT.SYMBOLS (PDS-PROOF.PART.SYMBOLS (CAR (PDS-ALL.PROOF.PARTS)))))
    ((refutation c&s) (PPP=L_RECONSTRUCT.SYMBOLS (PDS-SPLITPART.SYMBOLS  (CAR (PDS-ALL.SPLITPARTS)))))
    (otherwise        (error "Unknown protocol type ~S" (PDS-GET.PROTOCOL.TYPE))))
  (DT-PRINT.SYMBOLS '(CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL)
		    FILE (opt-get.option pr_left.margin) (opt-get.option pr_right.margin)))


(DEFVAR ppp*l_OLD.NEW.SYMBOL.ADDRESSES NIL)


(DEFUN PPP=L_RECONSTRUCT.SYMBOLS (SYMBOLS.CODE)	; edited:  4-sep-84 16:27:01  by cl
						; input : a lisp-expression
						; effect: reconstructs the symbols and creates a list
						;	  of dotted pairs (old.address . new.address)
						;	  of all the symbols used in the splitpart or
						;	  in the axioms-graph.
						; value : this list (ppp*l_old.NEW.SYMBOL.ADDRESSES)
  (SETQ ppp*l_OLD.NEW.SYMBOL.ADDRESSES (EVAL SYMBOLS.CODE)))


(DEFUN PPP=L_PRINT.AXIOM.CLAUSES (AXIOMS FILE)	; edited:  4-sep-84 16:28:09  by cl
						; input :  a list of integers and an open file
						; effect:  prints the axioms.graph
						; value :  undefined
  (PPP=L_PAGE FILE)
  (PPP=L_RECONSTRUCT.SYMBOLS (PDS-GET.AX.SYMBOLS))
  (when AXIOMS
    (let ((ppp*l_indentation (+ (opt-get.option pr_left.margin) 8)))
      (format file "~3%~vA" (+ ppp*l_indentation (length ppp*l_used.sign)) "")
      (PPP=L_print.with.underscore "Set of Axiom Clauses Resulting from Normalization" file
				 (+ ppp*l_indentation (length ppp*l_used.sign)))
      (format file "~2%~vA" ppp*l_indentation "")
      (MAPC #'(lAMBDA (AXIOM)
		(format file "\\implication{~A}{~A}{"
			(make-string 1 :initial-element #\M)
			"")
		(let (current.pos)
		  (PPP=L_PRINT.CLAUSE AXIOM FILE
				 :current.pos ppp*l_indentation :colon.pos (+ 6 ppp*l_indentation) :splitpart 'AXIOM)
		  (format file "}{~A}" ""))
		(format file "~%~vA" ppp*l_indentation ""))	; no more than 1 NEWLINE after each clause (SIG)
	    AXIOMS))))


(DEFUN PPP=L_PRINT.AX.OPERATIONS (AX.OPERATIONS FILE)
						; edited:  9-jul-84 08:23:48  by cl
						; input :  a list of integers and an open file
						; efffet:  prints the operations on the axioms.graph
						; value :  undefined
  (when ppp*l_DIRECT.PROOF (SETQ AX.OPERATIONS (REMOVE-IF-NOT (FUNCTION PDS-OPERATION.USE) AX.OPERATIONS)))
  (when AX.OPERATIONS
    (format file "~5%~vA" (+ (opt-get.option pr_left.margin) 8 (length ppp*l_used.sign)) "")
    (PPP=L_print.with.underscore "Initial Operations on Axioms" file (+ (opt-get.option pr_left.margin) 8 (length ppp*l_used.sign)))
    (format file "~3%~vA" (+ (opt-get.option pr_left.margin) 7) "")
    (PPP=L_PRINT.OPERATIONS AX.OPERATIONS FILE)))


(DEFUN PPP=L_PRINT.SPLITTED.THEOREMS (PROOF.PARTS FILE)
						; edited:  4-sep-84 16:28:49  by cl
						; input : a list of lists of integers and a file
						; effect: prints the theorem clauses sorted by splitparts.
						; value : undefined
  (let ((SPLITTED.THEOREMS (MAPCAR #'PDS-PROOF.PART.INITIAL.CLAUSES PROOF.PARTS))
	(headline "Set of Theorem Clauses Resulting from Normalization")
	(ppp*l_indentation (+ (opt-get.option pr_left.margin) 8)))
    (when (AND SPLITTED.THEOREMS (first SPLITTED.THEOREMS))
      (format file "~5%~vA" (+ ppp*l_indentation (length ppp*l_used.sign))"")
      (COND ((> (LIST-LENGTH SPLITTED.THEOREMS) 1)
	     (PPP=L_print.with.underscore (concatenate 'string headline " and Splitting")
					file (+ ppp*l_indentation (length ppp*l_used.sign))) 
	     (format file "~2%")
	     (let ((N 1))
	       (MAPC #'(LAMBDA (PROOF.PART THEOREMS)
			 (format file "~vASplitpart ~D~%" (opt-get.option pr_left.margin) "" n)
			 (PPP=L_RECONSTRUCT.SYMBOLS (PDS-PROOF.PART.SYMBOLS PROOF.PART))
			 (MAPC #'(lAMBDA (THEOREM)
				   (format file "\\implication{~A}{~A}{"
					   (make-string 1 :initial-element #\M)
					   " ")
				   (let (current.pos)
				     (PPP=L_PRINT.CLAUSE THEOREM FILE
							 :current.pos ppp*l_indentation
							 :colon.pos (+ 6 ppp*l_indentation) :splitpart 'THEOREM)
				     (format file "}{~A}" ""))
				   (format file "~%~vA" ppp*l_indentation ""))	; no more than 1 NEWLINE after each clause (SIG)
			       THEOREMS)
			 (format file "~2%")	; at least 1 empty line after each splitpart (SIG)			 
			 (incf N))
		     PROOF.PARTS SPLITTED.THEOREMS))
	     (format file "~%~vAEnd of Splitparts~%" (opt-get.option pr_left.margin) ""))
	    (T (PPP=L_print.with.underscore headline file (+ ppp*l_indentation (length ppp*l_used.sign)))
	       (format file "~2%")
	       (PPP=L_RECONSTRUCT.SYMBOLS (PDS-PROOF.PART.SYMBOLS (CAR PROOF.PARTS)))
	       (MAPC #'(LAMBDA (THEOREM)	; no more than 1 NEWLINE after each clause (SIG)
			 
			 (format file "\\implication{~A}{~A}{"
				 (make-string 1 :initial-element #\M)
				 "")
			 (let (current.pos)
			   (PPP=L_PRINT.CLAUSE THEOREM FILE
					       :current.pos ppp*l_indentation
					       :colon.pos (+ 6 ppp*l_indentation) :splitpart 'THEOREM)
			   (format file "}{~A}" ""))
			 (format file "~%~vA" ppp*l_indentation ""))
		     (CAR SPLITTED.THEOREMS))
	       (format file "~2%"))))))


(DEFUN PPP=L_PRINT.INITIAL.OPERATIONS (PROOF.PARTS FILE)
						; edited:  5-sep-84 08:25:36  by cl
						; input : a list of addresses and an open file
						; effect: prints the initial operations executed on the splitparts.
						; value : undefined
  (let ((PROOF.PARTS.WITH.OPERATIONS (PPP=L_PROOF.PARTS.WITH.OPERATIONS PROOF.PARTS)) OPERATIONS)
    (when PROOF.PARTS.WITH.OPERATIONS
      (format file "~5%~vA" (+ (opt-get.option pr_left.margin) 8 (length ppp*l_used.sign)) "")
      (PPP=L_print.with.underscore "Initial Operations on Theorems" file
				 (+ (opt-get.option pr_left.margin) 8 (length ppp*l_used.sign)))
      (format file "~3%~vA" (+ (opt-get.option pr_left.margin) 7) "")
      (if (EQL (LIST-LENGTH PROOF.PARTS) 1)	; no splitting
	  (progn (PPP=L_RECONSTRUCT.SYMBOLS (PDS-PROOF.PART.SYMBOLS (CAR PROOF.PARTS)))
		 (SETQ OPERATIONS (PDS-PROOF.PART.OPERATIONS (CAR PROOF.PARTS)))
		 (when ppp*l_DIRECT.PROOF (SETQ OPERATIONS (REMOVE-IF-NOT #'PDS-OPERATION.USE OPERATIONS)))
		 (PPP=L_PRINT.OPERATIONS OPERATIONS FILE))
	  (MAPC #'(LAMBDA (PROOF.PART)
		    (format file "~%~vASplitpart ~A:~%~3:*~vA"
			    (opt-get.option pr_left.margin) "" (PDS-PROOF.PART.IDENTIFIER PROOF.PART))
		    (PPP=L_RECONSTRUCT.SYMBOLS (PDS-PROOF.PART.SYMBOLS PROOF.PART))
		    (SETQ OPERATIONS (PDS-PROOF.PART.OPERATIONS PROOF.PART))
		    (when ppp*l_DIRECT.PROOF (SETQ OPERATIONS (REMOVE-IF-NOT (FUNCTION PDS-OPERATION.USE) OPERATIONS)))
		    (PPP=L_PRINT.OPERATIONS OPERATIONS FILE))
		PROOF.PARTS.WITH.OPERATIONS)))))


(DEFUN PPP=L_PROOF.PARTS.WITH.OPERATIONS (PROOF.PARTS)
						; edited: 22-aug-84 14:01:37  by cl
						; input : a list of addresses, possibly nil
						; effect: finds out which of these have operations
						;	  and , if direct proof is wanted, if any of
						;	  these operations were actually needed in the proof.
						; value : the list of proof parts with operations
  (remove-if #'(lambda (proof.part)
		 (let ((operations (PDS-PROOF.PART.OPERATIONS PROOF.PART)))
		   (or (null operations)
		       (and PPP*L_direct.proof (notany #'PDS-operation.use OPERATIONS)))))
	     proof.parts))
;
;  (REMOVE-IF-not #'(LAMBDA (PROOF.PART)
;		     (some #'(LAMBDA (OPERATION)
;			       (OR (NOT PPP*L_DIRECT.PROOF)
;				   (PDS-OPERATION.USE OPERATION)))
;			   (PDS-PROOF.PART.OPERATIONS PROOF.PART)))
;		 PROOF.PARTS))
;

(DEFUN PPP=L_PRINT.OPERATIONS (OPERATIONS FILE)	; edited: 23-jun-84 12:12:55  by cl
						; input : a list of integers (not nil) and a file
						; efffet: prints the operations
						; value : undefined
  (let (IMPL.SIGN.TAB CLAUSE.TAB COLON.POS PREMISES LONGEST.PREMISE MAX.PREMISE.LENGTH LONGEST.CLAUSE.PNAME)
    (SETQ PREMISES             (MAPCAR #'PPP=L_OPERATION.PREMISE OPERATIONS)
	  LONGEST.PREMISE      (PPP=L_LONGEST.OPERATION.PREMISE PPP=L_LONGEST.OPERATION.PREMISE PREMISES)
	  MAX.PREMISE.LENGTH   (PPP=L_LENGTH.OF.OPERATION.PREMISE LONGEST.PREMISE)
	  LONGEST.CLAUSE.PNAME (PDS-CLAUSE.PNAME (MAXELT (MAPCAN #'(LAMBDA (OPERATION)
								     (COPY-TREE (PDS-OPERATION.CLAUSES OPERATION)))
								 OPERATIONS)
							 #'(LAMBDA (CLAUSE) (PRINT-LENGTH (PDS-CLAUSE.PNAME CLAUSE) NIL))
							 ))
	  IMPL.SIGN.TAB  (if (< MAX.PREMISE.LENGTH
				(TRUNCATE (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin)) 2))
			     (+ (opt-get.option pr_left.margin) 1 MAX.PREMISE.LENGTH)
			     (+ 20 (opt-get.option pr_left.margin)))
	  CLAUSE.TAB     (+ IMPL.SIGN.TAB  (LENGTH ppp*l_IMPLICATION.SIGN))
	  COLON.POS      (+ CLAUSE.TAB     (LENGTH ppp*l_USED.SIGN) (LENGTH (string LONGEST.CLAUSE.PNAME))))
    (when (< (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin))
	     (* 4 CLAUSE.TAB))			; i.e. premise and impl.sign use more than 25% of line
      (SETQ COLON.POS (+ 6 (- COLON.POS CLAUSE.TAB)))
      (SETQ CLAUSE.TAB 6))
    (MAPC #'(LAMBDA (OPERATION PREMISE)
	      (PPP=L_PRINT.OPERATION OPERATION PREMISE (opt-get.option pr_left.margin) IMPL.SIGN.TAB CLAUSE.TAB COLON.POS
			           (opt-get.option pr_right.margin) 'AXIOM FILE)
	      (format file "~%~vA" (opt-get.option pr_left.margin) ""))	; at most 2 empty lines are admissible (SIG)
	  OPERATIONS PREMISES)
    (format file "~2%~vA"(opt-get.option pr_left.margin) "")))	; at least 3 empty lines are required (SIG)


(DEFUN PPP=L_PRINT.SPLITPART (SPLITPART NUMBER &optional (FILE *standard-output*))
						; edited:  2-oct-84 15:04:45  by cl
						; input :  an address, an integer (no splitting if 0), and an open file
						; effect:  prints splitpart
						; value :  undefined

	
						; headline with reason of failure

  (format file "~2%~vA" (opt-get.option pr_left.margin) "")
  (ppp=l_print.with.underscore (format nil "Refutation~[:~*~:; of Splitpart ~A:~]~@[ (Failure: ~*~A)~]"
					   number (PDS-SPLITPART.IDENTIFIER SPLITPART)
					   (EQ (PDS-SPLITPART.RESULT SPLITPART) 'FAILURE) (PDS-SPLITPART.REASON SPLITPART))
				   file)
  (format file "~%~vA" (opt-get.option pr_left.margin) "")
  
  (if (PDS-SPLITPART.INITIAL.GRAPH SPLITPART)				; otherwise a refutation has been found initially.
      
      (let (OPERATIONS OPERATION.PREMISES LONGEST.CLAUSE.PNAME IMPL.SIGN.TAB CLAUSE.TAB COLON.POS flag)
	(PPP=L_RECONSTRUCT.SYMBOLS (PDS-SPLITPART.SYMBOLS SPLITPART))
									; changed options
	
	(let ((CHANGED.OPTIONS (PDS-SPLITPART.CHANGED.OPTIONS SPLITPART)))
	  (when CHANGED.OPTIONS
	    (format file "~v@A~A~2%" (+ (opt-get.option pr_left.margin) (length ppp*l_used.sign))
		    "The following Options have been Changed:")
	    (MAPC #'(LAMBDA (OPTION) (format file "~vA~A   ~A~%" (opt-get.option pr_left.margin) "" (CAR OPTION) (CDR OPTION)))
		  CHANGED.OPTIONS)
	    (format file "~%~vA" (opt-get.option pr_left.margin) "")))
	
									; operation premises & tabulators (calculation only)
	
	(let (LONGEST.OPERATION.PREMISE MAX.PREMISE.LENGTH)
	  (SETQ OPERATIONS (PDS-SPLITPART.OPERATIONS SPLITPART))
	  (when (and ppp*l_DIRECT.PROOF (NEQ (PDS-SPLITPART.RESULT SPLITPART) 'FAILURE))
	    (SETQ OPERATIONS (REMOVE-IF-NOT #'PDS-OPERATION.USE OPERATIONS)))
	  (setq OPERATION.PREMISES        (MAPCAR #'PPP=L_OPERATION.PREMISE OPERATIONS)
		LONGEST.OPERATION.PREMISE (PPP=L_LONGEST.OPERATION.PREMISE OPERATION.PREMISES)
		MAX.PREMISE.LENGTH        (PPP=L_LENGTH.OF.OPERATION.PREMISE LONGEST.OPERATION.PREMISE)
		LONGEST.CLAUSE.PNAME      (PPP=L_LONGEST.CLAUSE.PNAME SPLITPART)
		IMPL.SIGN.TAB             (if (< MAX.PREMISE.LENGTH (TRUNCATE (- (opt-get.option pr_right.margin)
										 (opt-get.option pr_left.margin)) 2))
					      (+ (opt-get.option pr_left.margin) 1 MAX.PREMISE.LENGTH)
					      (+ 20 (opt-get.option pr_left.margin)))
		CLAUSE.TAB (max (+ IMPL.SIGN.TAB 1 (LENGTH ppp*l_IMPLICATION.SIGN)) (+ (opt-get.option pr_left.margin) 17))
		COLON.POS  (+ CLAUSE.TAB 1 (LENGTH ppp*l_USED.SIGN) (PRINT-LENGTH LONGEST.CLAUSE.PNAME NIL))))
	(when (setq flag (< (- (opt-get.option pr_right.margin) (opt-get.option pr_left.margin)) (* 4 CLAUSE.TAB)))
	  (SETQ COLON.POS  (+ 6 (- COLON.POS CLAUSE.TAB))
		CLAUSE.TAB 6))
									; initial clauses
	
	(format file "~%~vAInitial Clauses: ~@[~%~%~*~]~vA"
		(opt-get.option pr_left.margin)
		"" flag (if flag clause.tab (- clause.tab (opt-get.option pr_left.margin) 17)) "")
	(MAPC #'(LAMBDA (CLAUSE)
		  (format file "\\implication{~A}{~A}{"
			  (make-string 1 :initial-element #\M)
			  "")
		  (let (current.pos)
		    (PPP=L_PRINT.CLAUSE CLAUSE FILE
					:current.pos ppp*l_indentation
					:colon.pos (+ 6 ppp*l_indentation) :splitpart 'CLAUSE)
		    (format file "}{~A}" ""))
		  (format file "~%~vA" ppp*l_indentation ""))
	      (PDS-SPLITPART.INITIAL.GRAPH SPLITPART))
	
									; deduction steps
	
	(format file "~2%~vA" (opt-get.option pr_left.margin) "")
	(MAPC #'(LAMBDA (OPERATION PREMISE)
		  (PPP=L_PRINT.OPERATION OPERATION PREMISE (opt-get.option pr_left.margin) IMPL.SIGN.TAB CLAUSE.TAB COLON.POS
				       (opt-get.option pr_right.margin) SPLITPART FILE)
		  (format file "~%~vA" (opt-get.option pr_left.margin) ""))	; at most 2 empty lines (SIG)
	      OPERATIONS OPERATION.PREMISES)
	(format file "~2%~vA" (opt-get.option pr_left.margin) ""))	; at least 3 empty lines (SIG)
      (format file "~%~vASee operations on theorems of splitpart ~A~3%"
	      (opt-get.option pr_left.margin) "" (PDS-SPLITPART.IDENTIFIER SPLITPART))))


(DEFUN PPP=L_OPERATION.PREMISE (OPERATION)		; edited: 13-sep-84 17:39:25  by cl
						; input :  an address (integer)
						; effect:  constructs the premise of the deduction step
						; value :  the premise as a string, e.g. "A2,2 & R12,1"
  (CASE (PDS-OPERATION.TYPE OPERATION)
    (INSTANTIATION                         (format nil "~A (instance)"
						   (PDS-CLAUSE.PNAME (first (PDS-OPERATION.PARENTS OPERATION)))))
    (INSTANTIATe                           (format nil "~A (instance)"
						   (PDS-CLAUSE.PNAME (first (PDS-OPERATION.PARENTS OPERATION)))))
    ((RESOLUTION REPL.RES PARAMODULATION)  (format nil "~A,~D ~A ~A,~D ~A"
						   (PDS-CLAUSE.PNAME (PDS-OPERATION.POS.EQ.PAR OPERATION))
						   (PDS-OPERATION.POS.EQ.LITNO OPERATION)
						   ppp*l_CONJUNCTION.SIGN
						   (PDS-CLAUSE.PNAME (PDS-OPERATION.NEG.PAR OPERATION))
						   (PDS-OPERATION.NEG.LITNO OPERATION)
						   (PPP=L_RULE (PDS-OPERATION.RULE OPERATION))))
    ((FACTORIZATION REPLACEMENT.OPERATION) (format nil "~A (factor) ~A"
						   (PDS-CLAUSE.PNAME (PDS-OPERATION.POS.EQ.PAR OPERATION))
						   (PPP=L_RULE (PDS-OPERATION.RULE OPERATION))))
    (REWRITE.SYMMETRY  (format nil "~A ~A"
			       (PDS-CLAUSE.PNAME (first (PDS-OPERATION.PARENTS OPERATION)))
			       (PPP=L_RULE 'SYMMETRIC)))
    (DOUBLE.LITERAL    (format nil "~A ~D=~D ~A"
			       (PDS-CLAUSE.PNAME (first (PDS-OPERATION.PARENTS OPERATION)))
			       (PDS-OPERATION.LITNO.1 OPERATION)
			       (PDS-OPERATION.LITNO.2 OPERATION)
			       (PPP=L_RULE (pds-operation.RULE operation)))) 
    (REWRITE           (format nil "~A,~D ~A ~A"
			       (PDS-CLAUSE.PNAME (CAR (PDS-OPERATION.PARENTS OPERATION)))
			       (PDS-OPERATION.LITNO.1 OPERATION)
			       ppp*l_CONJUNCTION.SIGN
			       (PDS-CLAUSE.PNAME (PDS-OPERATION.RULE OPERATION))))
    (OTHERWISE (ERROR "Unknown operation type ~S" (PDS-OPERATION.TYPE OPERATION)))))


(DEFUN PPP=L_RULE (PDS.RULE)			; edited: 13-sep-84 17:40:23  by cl
						; input :  a rule as stored in pds
						; effect:  creates a string that will be printed in
						;	   the premise of an operation using this rule.
						; value :  the created string
  (COND ((NULL PDS.RULE) "")
	((PDS-CLAUSE.IS PDS.RULE)  (format nil "(~A)" (PDS-CLAUSE.PNAME PDS.RULE)))
	((EQL PDS.RULE 'SYMMETRIC)  "")
	((EQL PDS.RULE 'ASYMMETRIC) "")
	(T (ERROR "Unknown type ~S of rule." PDS.RULE))))

(DEFUN PPP=L_LONGEST.OPERATION.PREMISE (PREMISES)
  (MAXELT PREMISES #'PPP=L_LENGTH.OF.OPERATION.PREMISE))

(DEFUN PPP=L_LENGTH.OF.OPERATION.PREMISE (PREMISE)
  (length premise))

(DEFUN PPP=L_LONGEST.CLAUSE.PNAME (SPLITPART)	; edited:  3-oct-83 16:41:22  by cl
						; input :  an address
						; effect:  finds longest clause pname of splitpart
						; value :  this pname
  (PDS-CLAUSE.PNAME
    (MAXELT (APPEND (PDS-SPLITPART.AXIOMS SPLITPART)
		    (PDS-SPLITPART.THEOREMS SPLITPART)
		    (MAPCAN #'(LAMBDA (OPERATION) (COPY-list (PDS-OPERATION.CLAUSES OPERATION)))
			    (PDS-SPLITPART.OPERATIONS SPLITPART)))
	    #'(LAMBDA (CLAUSE) (PRINT-LENGTH (PDS-CLAUSE.PNAME CLAUSE) NIL)))))


(DEFUN PPP=L_PRINT.OPERATION (OPERATION PREMISE LEFT.POS IMPL.SIGN.TAB CLAUSE.TAB COLON.POS RIGHT.POS SPLITPART FILE)
						; edited: 3-oct-83 16.25.01  by cl
						; input : an address, a list, five integers, another
						;	  address, and an open file
						; effect: prints the operation including premise and resulting clauses
						; value : undefined
  (format file "\\implication{~A}{$~A$}{"
	  (make-string (- impl.sign.tab left.pos 2) :initial-element #\0)
	  premise)
  (let (current.pos)
    (MAPprint (PDS-OPERATION.CLAUSES OPERATION) nil nil (format nil "~%~vA" clause.tab "")
	      #'(LAMBDA (CLAUSE file)
		  (PPP=L_PRINT.CLAUSE CLAUSE FILE
				   :current.pos CLAUSE.TAB :right.pos RIGHT.POS :colon.pos COLON.POS :splitpart SPLITPART))
	      file)
    (format file "}{~A}" ppp*l_IMPLICATION.SIGN)))


(DEFUN PPP=L_PRINT.TOTAL.RESULT (FILE)		; edited: 22-aug-84 14:13:37  by cl
						; input : a file open for output
						; effect: finds out, if the proof succeeded in all its parts.
						;	  If not, extracts the parts that didn't succeed.
						; value : undefined
  (let (PROOF.PARTS.FAILED SPLITPARTS.FAILED)
    (SETQ PROOF.PARTS.FAILED (REMOVE-IF-NOT #'(LAMBDA (PROOF.PART) (EQL (PDS-PROOF.PART.RESULT PROOF.PART) 'FAILURE))
					    (PDS-ALL.PROOF.PARTS))
	  SPLITPARTS.FAILED  (REMOVE-IF-NOT #'(LAMBDA (SPLITPART) (EQL (PDS-SPLITPART.RESULT SPLITPART) 'FAILURE))
					    (PDS-ALL.SPLITPARTS)))
    (format file "~2%~vA~:[  q.e.d. ~;The theorem(s) could not be proved.~%~]"
	    (opt-get.option pr_left.margin) "" (or proof.parts.failed splitparts.failed) )
    (when (or PROOF.PARTS.FAILED SPLITPARTS.FAILED)
      (unless (= (LIST-LENGTH (PDS-ALL.PROOF.PARTS)) 1)
	(format file "~%~vA" (opt-get.option pr_left.margin) "")
	(when PROOF.PARTS.FAILED
	  (format file "Refutation of proof part~P" (list-length PROOF.PARTS.FAILED))
	  (MAPC #'(LAMBDA (PROOF.PART)
		    (format file " ~A" (PDS-PROOF.PART.IDENTIFIER PROOF.PART)))
		PROOF.PARTS.FAILED)
	  (format file " FAILED"))
	(format file "~%~vA" (opt-get.option pr_left.margin) "")
	(when SPLITPARTS.FAILED
	  (format file "Refutation of split part~P" (list-length SPLITPARTS.FAILED))
	  (MAPC #'(LAMBDA (SPLIT.PART)
		    (format file " ~A" (PDS-SPLItPART.IDENTIFIER SPLIT.PART)))
		SPLITPARTS.FAILED)
	  (format file " FAILED"))))))


(DEFUN PPP=L_PRINT.TOTAL.STATISTICS (FILE splitparts)
  (let ((number 0)
	(splitflag (rest splitparts)))
    (format file "~2%")
    (MAPC #'(LAMBDA (SPLITPART)
	      (when splitflag (incf NUMBER))
	      (format file "~%~vATime Used for Refutation~[:~*~:; of Splitpart ~A:~] ~D seconds"
		      (opt-get.option pr_left.margin) ""
		      number (PDS-SPLITPART.IDENTIFIER SPLITPART)
		      (ceiling (pds-splitpart.statistics splitpart) internal-time-units-per-second)))
	  SPLITPARTS)))

						
(DEFVAR PPP*L_TOTAL.VARIABLE.NUMBER 0)




(DEFUN PPP=L_PRINT.CLAUSE (CLAUSE FILE &key (current.pos (opt-get.option pr_left.margin))
			   (RIGHT.POS (opt-get.option pr_right.margin)) COLON.POS SPLITPART)
						; edited: 22-aug-84 14:15:07  by cl
						; input : a clause address (integer), an open file,
						;	  three integers, and another address or AXIOM/THEOREM.
						; effect: prints the clause (with p-name) in a readable format.
						; value : undefined
						; remark: a correct line position is assumed at the beginning,
						;         unless current.pos is given
  
  (let* ((SORTS.VARS            (COPY-TREE (PDS-CLAUSE.SORTS.VARIABLES CLAUSE)))
	 (SORTS.VARS.AND.PNAMES (PPP=L_RENAME.VARIABLES SORTS.VARS ppp*l_DIFFERENT.VARIABLES.FLAG))
	 new.line.flag LIT.POS)
						;  used.sign and pname
    (format file "\\parbox[t]{\\usedsignlength}{$~A$} \\hfil \\parbox[t]{\\namelength}{$~A$:} \\hfil \\parbox[t]{\\remainlength}{$"
	    (if (and splitpart (PDS-CLAUSE.USE CLAUSE)
		     (or (MEMBER SPLITPART (PDS-CLAUSE.USE CLAUSE)) (EQ SPLITPART 'AXIOM) (EQ SPLITPART 'THEOREM)))
		ppp*l_used.sign "  ")
	    (PDS-CLAUSE.PNAME CLAUSE)) 		; print quantified variables (if any)
    
    (when (SETQ SORTS.VARS (CAR SORTS.VARS.AND.PNAMES))
      (multiple-value-setq (current.pos new.line.flag) (PPP=L_PRINT.CLAUSE.QUANTIFIERS SORTS.VARS current.pos RIGHT.POS FILE)))
						; print literals
    
    (PP-l_PRINT.LITERALS (PPP=L_RENAME.LITERALS (PPP=L_APPLY.SUBSTITUTION (CDR SORTS.VARS.AND.PNAMES)
									(COPY-tree (PDS-CLAUSE.LITERALS CLAUSE))))
		       file :current.pos LIT.POS :right.pos RIGHT.POS)
    (format file "$}")))


(DEFUN PPP=L_RENAME.VARIABLES (SORTS.VARS DIFFERENT.VARIABLES.FLAG)
						; edited: 22-aug-84 14:17:53  by cl
						; input : a list of lists   (sort . variable addresses)
						;	  and a flag indicating whether all variable names shall be different.
						; effect: renames the variables
						; value : a pair (car = sorts.vars with variables renamed,
						;         cdr = list of variable addresses and their p-names .. unifier format ..)
  (let (VARIABLE.NAMES VAR.NUMBER VARS.PNAMES)
    (SETQ VAR.NUMBER (PP-SUM SORTS.VARS #'(LAMBDA (SORT.VARS) (LIST-LENGTH (CDR SORT.VARS)))))
    (COND (DIFFERENT.VARIABLES.FLAG
	   (dotimes (m var.number)
	     (SETQ VARIABLE.NAMES (NCONC1 VARIABLE.NAMES (format nil "x_~D" (+ ppp*l_TOTAL.VARIABLE.NUMBER m)))))
	   (SETQ ppp*l_TOTAL.VARIABLE.NUMBER (+ ppp*l_TOTAL.VARIABLE.NUMBER VAR.NUMBER)))
	  ((eq (opt-get.option pr_variable.print.names) 'old.variable.names)
	   (SETQ VARIABLE.NAMES (mapcan #'(lambda (sort.vars)
					    (mapcar #'(lambda (var) (format nil "x_~D" var)) (rest sort.vars)))
					sorts.vars)))
	  ((or (eq (opt-get.option pr_variable.print.names) 'different.names)
	       (< (LIST-LENGTH (opt-get.option pr_variable.print.names)) VAR.NUMBER))
	   (dotimes (n var.number)		
	     (SETQ VARIABLE.NAMES (NCONC1 VARIABLE.NAMES (format nil "x_~D" (1+ n))))))
	  (t (SETQ VARIABLE.NAMES (mapcar #'string (opt-get.option pr_variable.print.names)))))	; variable names are now defined
    (MAPC #'(LAMBDA (SORT.VARS)
	      (MAPL #'(LAMBDA (VARS.TAIL)
			(SETQ VARS.PNAMES (NCONC VARS.PNAMES (LIST (CAR VARS.TAIL) (CAR VARIABLE.NAMES))))
			(RPLACA VARS.TAIL (POP VARIABLE.NAMES)))
		    (CDR SORT.VARS)))
	  SORTS.VARS)
    (CONS SORTS.VARS VARS.PNAMES)))


(DEFUN PPP=L_RENAME.LITERALS (LITERALS)		; edited:  3-jul-84 08:51:44  by cl
						; input : a list ((sign pred termlist) ... )
						; effect: substitutes constant and function names into the
						;	  literals for their proof addresses and changes form to (sign pred .terms)
						; value : the renamed list of literals
  (MAPCAR #'(LAMBDA (LITERAL)
	      (list (CAR LITERAL) (cons (PPP=L_REPLACE.BY.PNAME (second literal)) (PPP=L_TERM (third literal)))))
	  LITERALS))


(defvar PPP*L_universal.quantifier "\\forall ")


(DEFUN PPP=L_PRINT.CLAUSE.QUANTIFIERS (SORTS.VARIABLES LEFT.POS RIGHT.POS FILE)
						; edited: 29-jul-83 15:19:37  by cl
						; input : a list, two integers, and an open file
						;         left.pos must be the current line position
						; effect: prints `ALL variables:sort'
						; values: new line position and a flag, indicating, if a newline was used
  (declare (ignore right.pos))
  (if SORTS.VARIABLES
      (let ((current.pos left.pos) new.line.flag)
	(PRINC PPP*L_Universal.quantifier FILE)
	(incf current.pos (length PPP*L_Universal.quantifier))
	(MAPC #'(LAMBDA (SORT.VARIABLES)
		  (mapl #'(lambda (rest.vars)
			    (let ((var (string-downcase (string (first rest.vars)))) (separator ","))
			      (unless (cdr rest.vars)
				(setq separator (format nil "\\in ~@(~A~) " (car sort.variables))))
			      (format file "~A~A" var separator)
			      (incf current.pos (+ (length var) (length separator)))))
			(cdr sort.variables)))
	      SORTS.VARIABLES)
	(values current.pos new.line.flag))
      (values left.pos nil)))


(DEFUN PPP=L_TERM (TERM)				; edited: 13-sep-84 17:41:15  by cl
						; input :  a term in prefix form, e.g. (5 3 (4 3) 3)
						; effect:  replaces addresses by pnames using ppp*l_old.new.symbol.addresses
						; value :  the term with addresses replaced by pnames
  (MAPCAR #'(LAMBDA (SUBTERM)
	      (COND ((AND (INTEGERP SUBTERM) (ASSOC SUBTERM ppp*l_OLD.NEW.SYMBOL.ADDRESSES)) (PPP=L_REPLACE.BY.PNAME SUBTERM))
		    ((CONSP SUBTERM) (PPP=L_TERM SUBTERM))
		    ((ATOM SUBTERM) subterm)	; i.e. constant or function symbol or variable.
		    (T (ERROR "~A is not a term." TERM))))
	  TERM))


(DEFUN PPP=L_REPLACE.BY.PNAME (ADDRESS)		; edited:  5-sep-84 15:50:14  by cl
						; input :  a small integer (address in proof)
						; value :  its pname as a string
  (string (DT-PNAME (CDR (ASSOC ADDRESS ppp*l_OLD.NEW.SYMBOL.ADDRESSES)))))

						;
(DEFVAR ppp*l_DIFFERENT.VARIABLES.FLAG NIL)

(DEFUN PPP=L_PRINT.TO.RIGHT.END (TEXT total.length FILE)
						; input:  a string, an integer, and a file
						; effect: princs the string using exactly total.space
  (when (> (length text) total.length)
    (warn "The text ~S is too long to be printed to right end" TEXT)) 
  (format file "~v@A" total.length text))

(DEFUN PPP=L_TERPRI (FILE)
  ; EDITED: 12-JUL-83 11:21:31         BY CL
  ; INPUT : A FILE OPEN FOR OUTPUT
  ; EFFECT: SAME AS LISP-TERPRI BUT KEEPING RECORD OF
  ;	  THE CURRENT LINE NUMBER.
  ; VALUE : UNDEFINED
  (TERPRI FILE) (SPACES (opt-get.option pr_left.margin) FILE)
  (COND ((< ppp*l_LINE ppp*l_LAST.LINE) (SETQ ppp*l_LINE (1+ ppp*l_LINE))) (T (PPP=L_PAGE FILE))))

(DEFUN PPP=L_TAB (NEXT.POS MIN.SPACES FILE)
  ; EDITED: 12-JUL-83 11:28:05           BY CL
  ; INPUT : TWO NON-NEGATIVE INTEGERS AND AN OPEN FILE
  ; EFFECT: SAME AS LISP-TAB BUT KEEPING RECORD OF THE
  ;	  CURRENT LINE NUMBER
  (tab next.pos min.spaces file))		;; (COND ((> (+ MIN.SPACES (POSITION FILE NIL)) NEXT.POS)
						;;	  (PPP=L_TERPRI FILE)
						;;	  (SPACES (- NEXT.POS (opt-get.option pr_left.margin)) FILE))
						;;	 (T (SPACES (- NEXT.POS (POSITION FILE NIL)) FILE)))


(DEFun PPP=L_PAGE (FILE)			        ; edited: 12-jul-83 11:35:17  by cl
  (declare (ignore file))			; input : an open file					
						; effect: will set the print position to the top of the next page.
  nil)						; value : undefined


(defun PPP=L_print.text (text file &key (start 0) end current.pos (continue.pos (opt-get.option pr_left.margin))
		       (right.margin (opt-get.option pr_right.margin)))
					 	; edited:  4-jul-83 11:23:17  by cl
						; input : a string, a file open for output, and five integers
						;	  current.pos = nil means position unknown, print after NEWLINE
						;	  end = nil means that the rest of text has to be printed
						; effect: princs the substring of text from start to end, incl start, excl end,
						;	  such that no word is cut, when the end of a line is reached.
						; value : the new line position
  (unless current.pos (format file "~%~vA" (setq current.pos continue.pos) ""))
  (unless end         (setq end (length text)))
  (let ((new.pos (+ (- end start) current.pos)))
    (if (> right.margin new.pos)
	(progn (write-string text file :start start :end end)
	       new.pos)
	(let ((blank.pos (search " " text :start2 start :end2 (+ start (- right.margin current.pos)) :from-end t)))
	  (if blank.pos
	      (progn (write-string text file :start start :end blank.pos)
		     (format file "~%~vA" continue.pos "")
		     (PPP=L_print.text text file :start (1+ blank.pos) :end end
				    :current.pos continue.pos :continue.pos continue.pos :right.margin right.margin))
	      (PPP=L_print.word text file :start start :end end
			     :current.pos current.pos :continue.pos continue.pos :right.margin right.margin))))))


(DEFUN PPP=L_PRINT.WORD (text file &key (start 0) end current.pos (continue.pos (opt-get.option pr_left.margin))
		       (right.margin (opt-get.option pr_right.margin)))
						; edited: 16-may-83 15:27:47  by cl
						; input : a string, a file open for output, and five integers
						;	  current.pos = nil means position unknown, print after NEWLINE
						;	  end = nil means that the rest of text has to be printed
						; effect: princs the word, on the next line, if necessary.
						; value : the new lineposition
  (unless current.pos (format file "~%~vA" (setq current.pos continue.pos) ""))
  (let* ((word.length (if end (- end start) (- (setq end (length text)) start)))
	 (new.pos (+ current.pos word.length)))
    (cond ((> right.margin (+ current.pos word.length))
	   (write-string text file :start start :end end)
	   new.pos)
	  ((<= current.pos continue.pos)
	   (write-string text file :start start :end (+ start (- right.margin current.pos)))
	   (format file "~%~vA" continue.pos "")
	   (PPP=L_print.word text file :start (+ start (- right.margin current.pos)) :end end
			  :current.pos continue.pos :continue.pos continue.pos :right.margin right.margin))
	  (T (format file "~%~vA" continue.pos "")
	     (write-string text file :start start :end (+ start (- right.margin continue.pos)))
	     (format file "~%~vA" continue.pos "")
	     (PPP=L_print.word text file :start (+ start (- right.margin current.pos)) :end end
			    :current.pos continue.pos :continue.pos continue.pos :right.margin right.margin)))))

(defun PPP=L_print.with.underscore (text file &optional (start.pos (opt-get.option pr_left.margin)) (underscore.sign #\=))
  (cond ((symbolp text)       (setq text (symbol-name text)))
	((not (stringp text)) (setq text (princ-to-string text)))
	(T nil))
  (format file "{\\bf ~A}" text)) 
  

(DEFUN PPP=L_APPLY.SUBSTITUTION (SUBSTITUTION LIST)
  ; EDITED: 29-MAR-84 08:57:21           BY CL
  ; INPUT : A LIST WITH EVEN NUMBER OF ELEMENTS,
  ;	  AND AN ARBITRARY LIST
  ; EFFECT: ANY OF THE ODD ELEMENTS OF SUBSTITUTION
  ;	  APPEARING IN LIST ARE SUBSTITUTED BY THEIR
  ;	  SUCCESSOR IN SUBSTITUTION.
  ; VALUE : THE LIST AFTER SUBSTITUTION
  ; REMARK: CF. UNI=APPLY.SUBSTITUTION ]
  (PROG ((RESULT (COPY-TREE LIST)))
    (SMAPL
      (FUNCTION (LAMBDA (SUBSTITUTES) (SETQ RESULT (NSUBST (SECOND SUBSTITUTES) (CAR SUBSTITUTES) RESULT))))
      (FUNCTION CDDR) SUBSTITUTION)
    (RETURN RESULT)))
