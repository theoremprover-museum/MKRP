;;; -*- Package: mkrp; Syntax: common-lisp; Mode: lisp -*-

#+(or symbolics explorer)
(DEFMACRO SEL-CHECK (SWITCH)
  `(SEL==CHECK ',SWITCH))

#+(or symbolics explorer)
(DEFUN SEL==CHECK (SWITCH)
						; edited: 19-jul-82 10:57:41
						; input:  on or off
						; effect: switchs check on and off.
						; value:  a message about kind of switch performed.
  (CASE SWITCH
    (ON
      (GLOBAL:ADVISE SEL=CLAUSES BEFORE SEL 0
        (COND
          ((SEL=CHECK_NO.CLAUSE.CLASS INDICATORS)
	   (ERROR "these indicators are not allowed in sel=clauses: ~a" INDICATORS))))
      (GLOBAL:ADVISE SEL=INSERT.CLAUSE BEFORE SEL 0
        (COND
          ((SEL=CHECK_NO.CLAUSE.CLASS INDICATORS)
	   (ERROR "these indicators are not allowed in sel=insert.clause: ~a" INDICATORS))))
      (GLOBAL:ADVISE SEL=REMOVE.CLAUSE BEFORE SEL 0
        (COND
          ((AND INDICATORS (SEL=CHECK_NO.CLAUSE.CLASS INDICATORS))
	   (ERROR "these indicators are not allowed in sel=remove.clause: ~a" INDICATORS))))
      (GLOBAL:ADVISE SEL=LINKS BEFORE SEL 0
        (COND
          ((NOTANY #'(LAMBDA (CLASS) (EQUAL INDICATORS (DELETE NIL (CONS (CDR CLASS) (CAR CLASS))))) SEL*LINK.CLASSES)
	   (ERROR "these indicators are not allowed in sel=links: ~a" INDICATORS))))
      (GLOBAL:ADVISE SEL=INSERT.LINK BEFORE SEL 0
        (COND
          ((SEL=CHECK_NO.LINK.CLASS (EVAL LINK*) INDICATORS)
	   (ERROR "these indicators are not allowed for this link in sel=insert.link: indicators: ~a link-colour: ~a"
		  INDICATORS (DS-LINK.COLOUR (EVAL LINK*))))))
      (GLOBAL:ADVISE SEL=REMOVE.LINK BEFORE SEL 0
        (COND
          ((AND INDICATORS (SEL=CHECK_NO.LINK.CLASS (EVAL LINK*)
						    INDICATORS))
	   (ERROR "these indicators are not allowed for this link in sel=remove.link: indicators: link-colour: "
		  INDICATORS (DS-LINK.COLOUR (EVAL LINK*))))))
      '(SEL-CHECK SWITCHED ON.))
    (OFF (MAPC #'(LAMBDA (FUN) (EVAL `(GLOBAL:UNADVISE ,FUN)))
	       '(SEL=CLAUSES SEL=INSERT.CLAUSE SEL=REMOVE.CLAUSE SEL=LINKS SEL=INSERT.LINK SEL=REMOVE.LINK))
	 '(SEL-CHECK SWITCHED OFF.))
    (OTHERWISE '(ILLEGAL SWITCH! TRY (SEL-CHECK ON) OR (SEL-CHECK OFF)))))

(DEFUN SEL=CHECK_NO.CLAUSE.CLASS (INDICATORS)
						; edited: 19-jul-82 11:19:11
						; input:  a list of atoms
						; value:  t if indicators represent no legal clause
						;         class, else nil.
  (NOTANY #'(LAMBDA (CLASS) (EQUAL CLASS INDICATORS)) SEL*CLAUSE.CLASSES))

(DEFUN SEL=CHECK_NO.LINK.CLASS (LINK INDICATORS)
						; edited: 19-jul-82 11:19:11
						; input:  a link and list of atoms
						; value:  t if indicators represent no legal link
						;         class, or if the link colour is not allowed
						;         for this class, else nil.
  (NOTANY
    #'(LAMBDA (CLASS)
        (AND (EQUAL INDICATORS (CAR CLASS)) (COND ((CDR CLASS) (EQL (CDR CLASS) (DS-LINK.COLOUR LINK))) (T T))))
    SEL*LINK.CLASSES))





#+(or symbolics explorer)
(DEFMACRO SEL-TRACE (SWITCH)
  `(SEL==TRACE ',SWITCH))

#+(or symbolics explorer)
(DEFUN SEL==TRACE (SWITCH)
						; edited: 14-jul-82 16:51:11
						; input:  t, on or off.
						; effect: switches the selection module trace on and
						;         off.
						; value:  a message about the kind of switch performed
  (PROG
    ((ADV
       '((GLOBAL:ADVISE SEL-INITIALIZE BEFORE SEL 0
	   (PROGN (TERPRI T) (PRINC "entering sel-initialize." T) (TERPRI T) (TERPRI T)))
         (GLOBAL:ADVISE SEL-UPDATE.REDUCE BEFORE SEL 0
           (PROGN (TERPRI T) (PRINC "entering sel-update.reduce." T) (TERPRI T) (TERPRI T)))
         (GLOBAL:ADVISE SEL-UPDATE.DEDUCE BEFORE SEL 0
           (PROGN (TERPRI T) (PRINC "entering sel-update.deduce." T) (TERPRI T) (TERPRI T)))
         (GLOBAL:ADVISE SEL=RETURN BEFORE SEL 0
           (CASE SEL*REDUCE.DEDUCE
             (DEDUCE
               (COND
                 (SEL*INITIAL.FLAG (PRINc "leaving sel-initialize.   first deduction code:" T) (TERPRI T)
				   (PRINc (SEL=TRACE.STRING (SEL-DEDUCTION.CODE.LINK)) T) (TERPRI T)
				   (format t "unifier:  ~A~%" (DS-PNAME (SEL-DEDUCTION.CODE.UNIFIER))))
                 (T (format t "Leaving sel-update.reduce.         Deduction code:~%~A~%Unifier: ~A~%"
			    (SEL=TRACE.STRING (SEL-DEDUCTION.CODE.LINK)) (DS-PNAME (SEL-DEDUCTION.CODE.UNIFIER))))))
             (REDUCE (PRINC "leaving sel-update.deduce.         reduction code:" T) (TERPRI T)
		     (COND
		       ((SEL-REDUCTION.CODE.CLAUSES.TO.BE.REDUCED)
			(PRINC (CONCAT "clauses to be reduced:  " (SEL-REDUCTION.CODE.CLAUSES.TO.BE.REDUCED)) T) (TERPRI T)))
		     (COND
		       ((SEL-REDUCTION.CODE.CLAUSES.TO.BE.REMOVED)
			(PRINC (CONCAT "clauses to be removed:  " (SEL-REDUCTION.CODE.CLAUSES.TO.BE.REMOVED)) T) (TERPRI T)))
		     (COND
		       ((SEL-REDUCTION.CODE.UNIFIERS.TO.BE.REMOVED ALL)
			(PRINC "links of which unifiers shall be removed:" T) (TERPRI T)
			(MAPC (SEL-REDUCTION.CODE.UNIFIERS.TO.BE.REMOVED LINKS)
			      #'(LAMBDA (LINK) (PRINC (SEL=TRACE.STRING LINK) T) (TERPRI T)))))
		     (TERPRI T))
             (OTHERWISE (ERROR "illegal sel*reduce.deduce in adv-prog sel=return" SEL*REDUCE.DEDUCE))))
         (GLOBAL:ADVISE SEL=CLASSIFY.CLAUSES AFTER SEL 0
           (PROGN (format t "~%Classified clauses:~%")
		  (MAPC (SEL-MARKED.CLAUSES)
			#'(LAMBDA (CLASS)
			    (COND
			      ((CDR CLASS) (PRINT (CAR CLASS) T)
			       (COND
				 ((LISTP (CADR CLASS))
				  (MAPC #'(LAMBDA (CLAUSE..)
					    (format t "~A  ~A   " (DS-PNAME (CAR CLAUSE..)) (CDR CLAUSE..)))
					(CDR CLASS))
				  (TERPRI T))
				 ((ATOM (CADR CLASS)) (PRINC (DS-PNAME (CDR CLASS)) T) (TERPRI T)))))))
		  (TERPRI T)))
         (GLOBAL:ADVISE SEL=NEXT.OPERATION AFTER SEL 0
           (PROGN (TERPRI T) (PRINC (CONCAT "next operation:  " //VALUE) T) (TERPRI T)))
         (GLOBAL:ADVISE SEL=PASS.CONTROL BEFORE  SEL 0
           (when (first scl:arglist)
	     (format t "~%Leaving ~A" SEL*ACTUAL.OPERATION)
	     (format t "~%Next operations: ~A~%~%" (first scl:arglist)))))))
    (RETURN
      (CASE SWITCH
	((T) (MAPC #'EVAL ADV) '(SEL-TRACE SWITCHED ON. OUTPUT WILL GO TO TERMINAL.))
        (ON (MAPC #'EVAL (NSUBST '(OPT-GET.OPTION TR_TRACE.FILE) T (COPY-TREE ADV)))
	    '(SEL-TRACE SWITCHED ON. OUTPUT WILL GO TO PROTOCOL FILE.))
        (OFF
          (MAPC #'(LAMBDA (FUN) (EVAL `(GLOBAL:UNADVISE ,FUN)))
		'(SEL-INITIALIZE SEL-UPDATE.REDUCE SEL-UPDATE.DEDUCE SEL=RETURN SEL=CLASSIFY.CLAUSES
				 SEL=PASS.CONTROL SEL=NEXT.OPERATION))
          '(SEL-TRACE SWITCHED OFF))
        (OTHERWISE '(ILLEGAL ARGUMENT] TRY (SEL-TRACE T) (SEL-TRACE ON) OR (SEL-TRACE OFF)))))))

(DEFUN SEL=TRACE.STRING (LINK)
						; edited: 5. 2. 1982   hjo
						; input:  an link
						; value:  a string containing some information about
						;         this link.
  (COND
    ((DS-LINK.IS LINK)
     (CASE (DS-LINK.COLOUR LINK)
       ((R P)
	(CONCATENATE 'STRING (PRINC-TO-STRING (DS-LINK.COLOUR LINK)) "-link " (PRINC-TO-STRING LINK) "   negpar: "
		     (PRINC-TO-STRING (DS-CLAUSE.PNAME (DS-LINK.NEGPAR LINK))) "," (PRINC-TO-STRING (DS-LINK.NEGLITNO LINK))
		     "   pospar: " (PRINC-TO-STRING (DS-CLAUSE.PNAME (DS-LINK.POSPAR LINK))) ","
		     (PRINC-TO-STRING (DS-LINK.POSLITNO LINK))))
       (SI
	 (CONCATENATE 'STRING "si-link " (PRINC-TO-STRING LINK) "   parent: "
		      (PRINC-TO-STRING (DS-CLAUSE.PNAME (DS-LINK.POSPAR LINK)))))
       (OTHERWISE (ERROR "illegal link colour in sel=trace.string: ~a" (DS-LINK.COLOUR LINK)))))
    (T LINK)))