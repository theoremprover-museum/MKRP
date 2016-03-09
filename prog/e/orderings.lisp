;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (MKRP); Base: 10 -*-

;;; Auxiliary Functions
;;; -------------------




(defun eord=lex.greater (compare tl1 tl2 &optional (eqpred #'equal))
						; Edited:  22-JAN-1990 19:44
						; Authors: PRCKLN
						; Input:   COMPARE is a function to compare two terms.
						;          TL1 and TL2 are lists of terms
						; Effect:  -
						; Value:   Assume TL1 = (T1 ... Tn), TL2 = (S1 ... Sn).
						;          true iff for some i (COMPARE Ti Si) is true
						;          and (COMPARE Tj Sj) and (COMPARE Sj Tj) false
						;          for all j<i (i.e. equal for all smaller ones).
  (do (result end)
      (end result)
    (cond ((null tl1)
	   (setq result nil end t))
	  ((null tl2)
	   (setq result t end t))
	  ((funcall compare (first tl1) (first tl2))
	   (setq result t end t))
	  ((not (funcall eqpred (first tl2) (first tl1)))
	   (setq result nil end t)))
    (pop tl1)
    (pop tl2)))

(defun eord=symbol.greater (symbol1 symbol2)
						; Edited:  22-JAN-1990 20:03
						; Authors: PRCKLN
						; Input:   Two symbols, constant or function
						; Effect:  -
						; Value:   True iff SYMBOL1 > SYMBOL2 in the operator ordering
						;          determined by the options.
  (> (eord=symbol.weight symbol1) (eord=symbol.weight symbol2)))



(defun eord=symbol.weight (term)
						; Edited:  25-FEB-1990 22:18
						; Authors: PRCKLN
						; Input:   A constant or function symbol symbol
						; Effect:  Puts the property ORD*SYMBOL according to options.
						;          if not set; the old precedence, if exists, is not allowed to
						;          change.
						; Value:   This weight.
  (eds-fct_weight term))



;;; Recursive Path
;;; --------------

(defun eord=rpo_ms.greater (compare ms1 ms2)
						; Edited:  22-JAN-1990 20:28
						; Authors: PRCKLN
						; Input:   COMPARE is a function to compare two terms.
						;          MS1 and MS2 are multisets of terms
						; Effect:  - 
						; Value:   TRUE iff MS1 >> MS2 in the usual multi set ordering
						; Remark:  For total orderings: (ord=lex.greater compare
						;	                  	   (sort (copy-list ms1) compare)
						;	                	   (sort (copy-list ms2) compare)))
  (setq ms2 (copy-list ms2))
  (setq ms1 (mapcar-not-nil #'(lambda (term)
				(if (member term ms2 :test #'equal)
				    (progn (setq ms2 (delete term ms2 :count 1 :test #'equal)) nil)
				    term))
			    ms1))
  (every #'(lambda (term2)
	     (some #'(lambda (term1)
		       (funcall compare term1 term2))
		   ms1))
	 ms2))





;;; Lexicographic Recursive Path
;;; ----------------------------

(defun eord=uncomparable.p (term1 term2)
						; Edited:  24-FEB-1990 16:44
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff it can be decided by a fast criterion
						;          that TERM1 and TERM2 are uncomparable
  (let ((variables1 nil) (variables2 nil) (not.known nil))
    (labels ((eord=uncomparable.rec (term1 term2)
	       (cond ((and (eds-var_p term1) (eds-var_p term2))
		      (push term1 variables1)
		      (push term2 variables2))
		     ((eql (eds-term_topsymbol term1) (eds-term_topsymbol term2))
		      (mapc #'(lambda (e1 e2) (eord=uncomparable.rec e1 e2))
			    (eds-term_arguments term1) (eds-term_arguments term2)))
		     (t (setq not.known t)))))
      (eord=uncomparable.rec term1 term2)
      (or (and (not not.known) (set= variables1 variables2))
	  (not (subsetp variables2 variables1))))))

(defparameter eord*array (make-array '(50 50) :initial-element :empty :adjustable t))


(defun eord=cons.term (term nr)
  (cond ((eds-term_p term)
	 (eds-term_set.index term nr)
	 (mapc #'(lambda (subterm) (setq nr (eord=cons.term subterm (1+ nr))))
	       (eds-term_arguments term))
	 nr)
	(t nr)))


(defun eord=reset.array (nr1 nr2)
  (when (or (>= nr1 (array-dimension eord*array 0)) (>= nr2 (array-dimension eord*array 1)))
    (adjust-array eord*array (list (max (+ 1 nr1) (array-dimension eord*array 0))
				   (max (+ 1 nr2) (array-dimension eord*array 1)))))
  (dotimes (n1 nr1)
    (dotimes (n2 nr2)
      (setf (aref eord*array n1 n2) :empty))))

(defun eord=lpo_greater (term1 term2)
						; Edited:  22-JAN-1990 19:41
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the lpo reduction ordering
						;          determined by the options.
  (cond ((or (eord=term_equal term1 term2) (eds-var_p term1)) nil)
	((eds-var_p term2)
	 (eord=term_in term2 term1))
	((or (eds-fct_eq.p (eds-term_topsymbol term1))
	     (eds-fct_eq.p (eds-term_topsymbol term2)))
	 (eord=compare.eq term1 term2))
	;((eord=uncomparable.p term1 term2) nil)
	((eord=term_in term1 term2) nil)
	((and (eds-term_p term1) (eds-term_p term2))
	 (let ((cont (aref eord*array (eds-term_index term1) (eds-term_index term2))))
	   (if (eq :empty cont)
	       (setf (aref eord*array (eds-term_index term1) (eds-term_index term2))
		     (or (and (not (eord=term_equal term1 term2)) (eord=term_in term2 term1))			 
			 (and (eord=symbol.greater (eds-term_topsymbol term1) (eds-term_topsymbol term2))
			      (every #'(lambda (subterm) (eord=lpo_greater term1 subterm))
				     (eds-term_arguments term2)))
			 (and (eord=term_equal (eds-term_topsymbol term1) (eds-term_topsymbol term2))
			      (every #'(lambda (subterm) (eord=lpo_greater term1 subterm))
				     (eds-term_arguments term2))
			      (eord=lex.greater #'eord=lpo_greater (eds-term_arguments term1) (eds-term_arguments term2)
						#'eord=term_equal))
			 (some #'(lambda (subterm) (eord=lpo_greater subterm term2))
			       (eds-term_arguments term1))))
	       cont)))
	(t (or (and (not (eord=term_equal term1 term2)) (eord=term_in term2 term1))			 
	       (and (eord=symbol.greater (eds-term_topsymbol term1) (eds-term_topsymbol term2))
		    (every #'(lambda (subterm) (eord=lpo_greater term1 subterm))
			   (eds-term_arguments term2)))
	       (and (eord=term_equal (eds-term_topsymbol term1) (eds-term_topsymbol term2))
		    (every #'(lambda (subterm) (eord=lpo_greater term1 subterm))
			   (eds-term_arguments term2))
		    (eord=lex.greater #'eord=lpo_greater (eds-term_arguments term1) (eds-term_arguments term2)
				      #'eord=term_equal))
	       (some #'(lambda (subterm) (eord=lpo_greater subterm term2))
		     (eds-term_arguments term1))))))

(defun eord=term_in (term1 term2)
						; Edited:  22-JAN-1990 20:17
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   TRUE if TERM1 at any pos occurs in TERM2
  (or (eord=term_equal term1 term2)
      (and (eds-term_p term2)
	   (member-if #'(lambda (subterm) (eord=term_in term1 subterm)) (eds-term_arguments term2)))))

(defun eord=term_equal (term1 term2)
  (eds-term_equal term1 term2))

(defun eord=compare.eq (term1 term2)
  (if (eds-fct_eq.p (eds-term_topsymbol term1))
      (if (eds-fct_eq.p (eds-term_topsymbol term2))
	  (eord=rpo_ms.greater #'eord=lpo_greater (eds-term_arguments term1) (eds-term_arguments term2))
	  (or (eord=lpo_greater (first (eds-term_arguments term1)) term2)
	      (eord=lpo_greater (second (eds-term_arguments term1)) term2)
	      (eds-const_p term2)))
      (if (eds-fct_eq.p (eds-term_topsymbol term2))
	  (and (eord=lpo_greater term1 (first (eds-term_arguments term2)))
	       (eord=lpo_greater term1 (second (eds-term_arguments term2)))))))

;;; Compare Functions
;;; -----------------

(defun eord=greater (term1 term2)
						; Edited:  08-OCT-1989 03:02
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the reduction ordering
						;          determined by the options.
  (let ((nr1 (eord=cons.term term1 1))
	(nr2 (eord=cons.term term2 1)))
    (eord=reset.array nr1 nr2)
    (eord=lpo_greater term1 term2)))





;;; Interface
;;; ---------

(defun eord-greater (term1 term2)  
						; Edited:  08-OCT-1989 03:02
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the reduction ordering
						;          determined by the options.
  (eord=greater term1 term2))


(defun eord-reset ()
						; Edited:  17-JAN-1990 00:27
						; Authors: PRCKLN
						; Input:   -
						; Effect:  Resets all kb weights of the actual symbols and
						;          deletes the known minimum
						; Value:   Undefined.
  nil)


(defun eord-show (&optional (file *terminal-io*))
  (Format file "~% Precedence:~%")
  (let ((list (sort (copy-list (append (dt-predicate.all) (dt-constant.all) (dt-function.all)))
		    #'>
		    :key #'ord=symbol.weight)))
    (mapc #'(lambda (symb)
	      (format file "~A ~A[~A]:  ~A~%" (dt-type symb) (dt-pname symb) symb (ord=symbol.weight symb)))
	  list)
    (nbutlast (zip (mapcar #'(lambda (x) (intern (dt-pname x) (find-package "MKRP"))) list)
		   (make-list (length list) :initial-element '>)))))