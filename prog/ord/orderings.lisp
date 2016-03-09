;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

;;; Auxiliary Functions
;;; -------------------




(defun ord=lex.greater (compare tl1 tl2 &optional (eqpred #'equal))
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
	  ((not (funcall eqpred (first tl2) (first tl1))) ;(funcall compare (first tl2) (first tl1))
	   (setq result nil end t)))
    (pop tl1)
    (pop tl2)))

(defun ord=symbol.greater (symbol1 symbol2)
						; Edited:  22-JAN-1990 20:03
						; Authors: PRCKLN
						; Input:   Two symbols, constant or function
						; Effect:  -
						; Value:   True iff SYMBOL1 > SYMBOL2 in the operator ordering
						;          determined by the options.
  (if (dt-variable.is symbol1)
      (if (dt-variable.is symbol2)
	  (> symbol1 symbol2)
	  nil)
      (if (dt-variable.is symbol2)
	  t
	  (> (ord=symbol.weight symbol1) (ord=symbol.weight symbol2)))))

(defun ord=sort (symbol)
						; Edited:  30-JAN-1990 01:52
						; Authors: PRCKLN
						; Input:   A predicate, function or constant symbol
						; Effect:  -
						; Value:   The result sort
  (cond ((dt-constant.is symbol) (dt-constant.sort symbol))
	((dt-function.is symbol) (DT-FUNCTION.SORT symbol))
	((dt-predicate.is symbol) nil)))

(defun ord=symbol.search.sort (operator sorted.ops)
						; Edited:  25-FEB-1990 22:18
						; Authors: PRCKLN
						; Input:   An operator symbol, a list of operator symbol sorted
						;          according ordering option and sort hierarchy and a number.
						;          All SORTED.OPS have numbers assigned (property ORD*SYMBOL).
						;          They are also sorted according to these numbers.
						;          NUMBER is always smaller than the number of the first
						;          element of SORTED.OPS.
						; Effect:  Assigns OPERATOR a number such that SORTED.OPS
						;          is also sorted according to these numbers.          
						; Value:   The list with OPERATOR inserted in SORTED.OPS, still sorted.
  (if (member operator sorted.ops)
      sorted.ops
      (if sorted.ops
	  (if (and (dt-predicate.is operator)
		   (or (dt-predicate.is.false operator)
		       (dt-predicate.is.true operator)))
	      (nconc1 sorted.ops operator)
	      (if (or (and (dt-sort.is.subsort (ord=sort (first sorted.ops)) (ord=sort operator))
			   (not (dt-sort.is.subsort (ord=sort operator) (ord=sort (first sorted.ops)))))	;echt groesser
		      (and (dt-function.is operator)
			   (dt-function.is (first sorted.ops))
			   (> (dt-function.arity  operator) (dt-function.arity (first sorted.ops))))
		      (and (dt-predicate.is operator)
			   (dt-function.is (first sorted.ops)))
		      (and (dt-function.is operator)
			   (dt-constant.is (first sorted.ops)))
		      (and (dt-constant.is operator)
			   (dt-constant.is (first sorted.ops))))
		  (cons operator sorted.ops)
		  (cons (first sorted.ops) (ord=symbol.search.sort operator (rest sorted.ops)))))
	  (list operator))))


(defparameter ord*list nil)

(defun ord=symbol.set.foreign (rest.ops sorted.ops)
						; Edited:  25-FEB-1990 22:17
						; Authors: PRCKLN
						; Input:   REST.OPS is the list of operators not
						;          yet inserted into precedence ordering.
						;          SORTED.OPS is the sorted part.
						;          The precedence must be compatible with the sort ordering.
						; Effect:  -
						; Value:   The totally sorted list.
						; Remark:  All constant and function symbols must be known!!!!
  (mapc #'(lambda (op) (setq sorted.ops (ord=symbol.search.sort op sorted.ops))) rest.ops)
  (do ((n 0 (1- n))
       (rest.ops sorted.ops (rest rest.ops)))
      ((null rest.ops) sorted.ops)
    (dt-putprop (first rest.ops) 'ord*symbol n)))

(defun ord=symbol.weight (term)
						; Edited:  25-FEB-1990 22:18
						; Authors: PRCKLN
						; Input:   A constant or function symbol symbol
						; Effect:  Puts the property ORD*SYMBOL according to options.
						;          if not set; the old precedence, if exists, is not allowed to
						;          change.
						; Value:   This weight.
  (or (dt-getprop term 'ord*symbol)
      (let ((sorted (mapcar-not-nil #'(lambda (symb)
					(first (member symb (append (dt-function.all)
								    (dt-constant.all)
								    (dt-predicate.all))
						       :key #'(lambda (obj) (read-from-string (dt-pname obj))))))
				    (opt-get.option er_operator.ordering))))
	(setq ord*list 
	      (ord=symbol.set.foreign (append (dt-function.all) (dt-constant.all) (dt-predicate.all))
				      sorted))	
	(dt-getprop term 'ord*symbol))))

;;; Knuth Bendix
;;; ------------

(defun ord=kb_var.count (term)
						; Edited:  03-OCT-1989 19:49
						; Authors: PRCKLN
						; Input:   A term
						; Effect:  -
						; Value:   An A-list ((v . number_of _occurrences) .....)
  (let ((alist nil))
    (labels ((rec-count (term)
	       (cond ((dt-variable.is term)
		      (let ((aitem (assoc term alist)))
			(if aitem
			    (incf (rest aitem))
			    (setq alist (acons term 1 alist)))))
		     ((consp term)
		      (mapc #'rec-count (rest term))))))
      (rec-count term))
    alist))

(defun ord=kb_var.subset (alist1 alist2)
						; Edited:  03-OCT-1989 20:00
						; Authors: PRCKLN
						; Input:   Two a-lists
						; Effect:  -
						; Value:   True iff alist1 represents a multiset contained in alist2
  (every #'(lambda (var.number)
	     (let ((number2 (cassoc (first var.number)  alist2)))
	       (if number2
		   (<= (rest var.number) number2)
		   nil)))
	 alist1))

(defun ord=kb_var.plus (alist)
						; Edited:  03-OCT-1989 20:06
						; Authors: PRCKLN
						; Input:   An alist
						; Effect:  -
						; Value:   The sum of the cdrs
  (let ((res 0))
    (mapc #'(lambda (x) (incf res (rest x))) alist)
    res))

(defun ord=kb_var.condition (t1 t2)
						; 1. true iff all v: #(v,t1) >= #(v,t2)
						; 2. true iff all v: #(v,t1) = #(v,t2)
  (let* ((vars1 (ord=kb_var.count t1))
	 (vars2 (ord=kb_var.count t2))
	 (subs (ord=kb_var.subset vars2 vars1))
	 (varsl1 (ord=kb_var.plus vars1))
	 (varsl2 (ord=kb_var.plus vars2)))
    (values (and subs (>= varsl1 varsl2))
	    (and subs (= varsl1 varsl2)))))

(defun ord=kb_put (sym val)
  (when (and val (dt-constant.is sym))
    (setq ord*kb_minimum (min val (or ord*kb_minimum 1000000))))
  (dt-putprop sym 'ord*weight val))

(defun ord=kb_set.foreign (symbol)
						; all constant and function symbols must be known!!!!
  (flet ((compute (fun)
	   (let* ((consts (mapcar-not-nil #'(lambda (const)
					      (if (or (dt-getprop const 'ord*weight)
						      (let ((val (second (assoc (intern (dt-pname const)
											(find-package "MKRP"))
										(opt-get.option er_knuth.bendix.weight)))))
							(ord=kb_put const val)
							val))
						  nil
						  const))
					  (funcall fun)))
		  (dist (float (/ 1 (+ 1 (length consts)))))
		  (max (apply #'max 2 (mapcar #'(lambda (c) (or (dt-getprop c 'ord*weight) 0)) (funcall fun)))))
	     (do ((rest consts (rest rest))
		  (diff dist (+ diff dist)))
		 ((null rest))
	       (ord=kb_put (first rest) (- max diff))))))
    (compute #'(lambda () (dt-function.all)))
    (compute #'(lambda () (dt-constant.all)))
    (compute #'(lambda () (dt-predicate.all)))
    (dt-getprop symbol 'ord*weight)))

(defvar ord*kb_minimum nil) ; Minimal constant weight



(defun ord=kb_variable.weight ()
  (unless ord*kb_minimum
    (ord=kb_symbol.weight (dt-predicate.true)))
  (or ord*kb_minimum 0))

(defun ord=kb_symbol.weight (term)
						; Edited:  08-OCT-1989 03:07
						; Authors: PRCKLN
						; Input:   A constant or function symbol symbol
						; Effect:  Puts the property ORD*WEIGHT according to options
						;          if not set
						; Value:   This weight      
  (or (dt-getprop term 'ord*weight)
      (ord=kb_set.foreign term)))

(defun ord=kb_term.weight (term)
  (cond ((dt-constant.is term) (ord=kb_symbol.weight term))
	((dt-variable.is term) (ord=kb_variable.weight))
	((dt-predicate.is term) (ord=kb_symbol.weight term))
	(t (apply #'+
		  (ord=kb_symbol.weight (first term))
		  (mapcar #'(lambda (subterm) (ord=kb_term.weight subterm))
			  (rest term))))))


(defun ord=kb_greater (term1 term2 &optional (function #'identity))
						; Edited:  08-OCT-1989 02:59
						; Authors: PRCKLN
						; Input:   Two terms and a function to permutate a list
						;          (reverse or identity)
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in knuth bendix ordering
  (let ((weight1 (ord=kb_term.weight term1))
	(weight2 (ord=kb_term.weight term2)))
    (multiple-value-bind (greater.or.equal.p equal.p)
	(ord=kb_var.condition term1 term2)
      (or (and (> weight1 weight2) greater.or.equal.p)
	  (and (= weight1 weight2)
	       equal.p
	       (or (and (not (equal term2 term1)) (in term2 term1))
		   (and (dt-variable.is term2)
			(consp term1)
			(in term2 term1))
		   (and (consp term1)
			(consp term2)
			(or (ord=symbol.greater (first term1) (first term2))
			    (and (eql (first term1) (first term2))
				 (ord=lex.greater #'ord=kb_greater
						  (funcall function (rest term1))
						  (funcall function (rest term2))))))))))))



;;; Recursive Path
;;; --------------

(defun ord=rpo_ms.greater (compare ms1 ms2)
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
				(if (member (rest term) ms2 :test #'equal :key #'rest)
				    (progn (setq ms2 (delete (rest term) ms2 :count 1 :key #'rest :test #'equal)) nil)
				    term))
			    ms1))
  (every #'(lambda (term2)
	     (some #'(lambda (term1)
		       (funcall compare term1 term2))
		   ms1))
	 ms2))



(defun ord=rpo_symbol.equal (symbol1 symbol2)
						; Edited:  22-JAN-1990 20:03
						; Authors: PRCKLN
						; Input:   Two symbols, constant or function
						; Effect:  -
						; Value:   True iff SYMBOL1 = SYMBOL2 in the rpo reduction ordering
						;          determined by the options.
  (= (ord=kb_symbol.weight symbol1) (ord=kb_symbol.weight symbol2)))

(defun ord=rpo_greater (term1 term2)
						; Edited:  22-JAN-1990 20:03
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the rpo reduction ordering
						;          determined by the options.
  (cond ((or (equal term1 term2) (dt-variable.is term1)) nil)
	((dt-variable.is term2) (dt-term_in term2 term1))
	((ord=rpo_symbol.equal (dt-term_topsymbol term1) (dt-term_topsymbol term2))
	 (ord=rpo_ms.greater #'ord=rpo_greater (dt-term_arguments term1) (dt-term_arguments term2)))
	((ord=symbol.greater (dt-term_topsymbol term1) (dt-term_topsymbol term2))
	 (ord=rpo_ms.greater #'ord=rpo_greater (list term1) (dt-term_arguments term2)))
	(t nil)))

;;; Lexicographic Recursive Path
;;; ----------------------------

(defun ord=uncomparable.p (term1 term2)
						; Edited:  24-FEB-1990 16:44
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff it can be decided by a fast criterion
						;          that TERM1 and TERM2 are uncomparable
  (let ((variables1 nil) (variables2 nil) (not.known nil))
    (labels ((ord=uncomparable.rec (term1 term2)
	       (cond ((and (dt-variable.is term1) (dt-variable.is term2))
		      (push term1 variables1)
		      (push term2 variables2))
		     ((eql (dt-term_topsymbol term1) (dt-term_topsymbol term2))
		      (mapc #'(lambda (e1 e2) (ord=uncomparable.rec (rest e1) (rest e2)))
			    (dt-term_arguments term1) (dt-term_arguments term2)))
		     (t (setq not.known t)))))
      (ord=uncomparable.rec term1 term2)
      (or (and (not not.known) (set= variables1 variables2))
	  (not (subsetp variables2 variables1))))))

(defparameter ord*array (make-array '(50 50) :initial-element :empty :adjustable t))
(defvar ord*nr)


(defun ord=cons.term (term)
  (cons (incf ord*nr) (if (dt-term_c.term.is term)
			  (cons (dt-term_topsymbol term) (mapcar #'ord=cons.term (dt-term_arguments term)))
			  term)))


(defun ord=reset.array (nr1 nr2)
  (when (or (>= nr1 (array-dimension ord*array 0)) (>= nr2 (array-dimension ord*array 1)))
    (adjust-array ord*array (list (max (+ 1 nr1) (array-dimension ord*array 0))
				  (max (+ 1 nr2) (array-dimension ord*array 1)))))
  (dotimes (n1 nr1)
    (dotimes (n2 nr2)
      (setf (aref ord*array n1 n2) :empty))))

(defun ord=lpo_greater (eterm1 eterm2)
						; Edited:  22-JAN-1990 19:41
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the lpo reduction ordering
						;          determined by the options.
  (let* ((nr1 (first eterm1))
	 (nr2 (first eterm2))
	 (term1 (rest eterm1))
	 (term2 (rest eterm2)))
    (let ((cont (aref ord*array nr1 nr2)))
      (if (eq :empty cont)
	  (setf (aref ord*array nr1 nr2)
		(cond ((or (ord=term_equal term1 term2) 
			   (and (dt-variable.is term1) (not (dt-getprop term1 'red*cc)))) nil)
		      ((and (dt-variable.is term2)
			    (not (dt-getprop term2 'red*cc)))
		       (ord=term_in term2 eterm1))
		      ((or (dt-predicate.is.equality (dt-term_topsymbol term1))
			   (dt-predicate.is.equality (dt-term_topsymbol term2)))
		       (ord=compare.eq term1 eterm1 term2 eterm2))
		      ((or (and (not (eq (opt-get.option er_completion) 'constant-congruence))
				(ord=uncomparable.p term1 term2))
			   (ord=term_in term1 eterm2)) nil)
		      ((or (and (not (ord=term_equal term1 term2)) (ord=term_in term2 eterm1))
			   
			   (and (ord=symbol.greater (dt-term_topsymbol term1)
						    (dt-term_topsymbol term2))
				(every #'(lambda (subterm) (ord=lpo_greater eterm1 subterm))
				       (dt-term_arguments term2)))
			   (and (= (dt-term_topsymbol term1) (dt-term_topsymbol term2))
				(every #'(lambda (subterm) (ord=lpo_greater eterm1 subterm))
				       (dt-term_arguments term2))
				(ord=lex.greater #'ord=lpo_greater (dt-term_arguments term1)
						 (dt-term_arguments term2)
						 #'(lambda (et1 et2) 
						     (ord=term_equal (rest et1) (rest et2)))))
			   (some #'(lambda (subterm) (ord=lpo_greater subterm eterm2))
				 (dt-term_arguments term1))))))
	  cont))))

(defun ord=term_in (term1 eterm2)
						; Edited:  22-JAN-1990 20:17
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   TRUE if TERM1 at any pos occurs in TERM2
  (or (ord=term_equal term1 (rest eterm2))
      (and (dt-term_c.term.is (rest eterm2))
	   (member-if #'(lambda (subterm) (ord=term_in term1 subterm)) (dt-term_arguments (rest eterm2))))))

(defun ord=term_equal (term1 term2)
  (or (eql term1 term2)
      (and (eql (dt-term_topsymbol term1) (dt-term_topsymbol term2))
	   (every #'(lambda (est1 est2)
		      (ord=term_equal (rest est1) (rest est2)))
		  (dt-term_arguments term1)
		  (dt-term_arguments term2)))))

(defun ord=compare.eq (term1 eterm1 term2 eterm2)
  (if (dt-predicate.is.equality (dt-term_topsymbol term1))
      (if (dt-predicate.is.equality (dt-term_topsymbol term2))
	  (ord=rpo_ms.greater #'ord=lpo_greater (dt-term_arguments term1) (dt-term_arguments term2))
	  (or (ord=lpo_greater (first (dt-term_arguments term1)) eterm2)
	      (ord=lpo_greater (second (dt-term_arguments term1)) eterm2)))
      (if (dt-predicate.is.equality (dt-term_topsymbol term2))
	  (and (ord=lpo_greater eterm1 (first (dt-term_arguments term2)))
	       (ord=lpo_greater eterm1 (second (dt-term_arguments term2)))))))

;;; Compare Functions
;;; -----------------

(defun ord=greater (term1 term2)
						; Edited:  08-OCT-1989 03:02
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the reduction ordering
						;          determined by the options.
  (ecase (opt-get.option er_ordering)
    (knuth-bendix (ord=kb_greater term1 term2 #'identity))
    (knuth-bendix-reverse (ord=kb_greater term1 term2 #'reverse))
    (polynomial (ord=pol_greater term1 term2))
    (recursive-path (ord=rpo_greater term1 term2))
    (lexicographic-recursive-path (let (nr1
					nr2
					eterm1
					eterm2)
				    (setq ord*nr -1
					  eterm1 (ord=cons.term term1)
					  nr1 (1+ ord*nr)
					  ord*nr -1
					  eterm2 (ord=cons.term term2)
					  nr2 (1+ ord*nr))
				    (ord=reset.array nr1 nr2)
				    (ord=lpo_greater eterm1 eterm2)))))



;;; Interface
;;; ---------

(defun ord-greater (term1 term2)  
						; Edited:  08-OCT-1989 03:02
						; Authors: PRCKLN
						; Input:   Two terms
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the reduction ordering
						;          determined by the options.
  (ord=greater term1 term2))


(defun ord-reset ()
						; Edited:  17-JAN-1990 00:27
						; Authors: PRCKLN
						; Input:   -
						; Effect:  Resets all kb weights of the actual symbols and
						;          deletes the known minimum
						; Value:   Undefined.
  (setq ord*kb_minimum nil
	ord*list nil
	ord*pol_default 1)
  (ord-pol_set.limit 10000000)
  (mapc #'(lambda (symbol) (dt-remprops symbol '(ord*weight ord*symbol)))
	(append (dt-predicate.all) (dt-constant.all) (dt-function.all))))


(defun ord-show (&optional (file *terminal-io*))
  (Format file "~% Precedence:~%")
  (let ((list (sort (copy-list (append (dt-predicate.all) (dt-constant.all) (dt-function.all)))
		    #'>
		    :key #'ord=symbol.weight)))
    (mapc #'(lambda (symb)
	      (format file "~A ~A[~A]:  ~A~%" (dt-type symb) (dt-pname symb) symb (ord=symbol.weight symb)))
	  list)
    (nbutlast (zip (mapcar #'(lambda (x) (intern (dt-pname x) (find-package "MKRP"))) list)
		   (make-list (length list) :initial-element '>)))))