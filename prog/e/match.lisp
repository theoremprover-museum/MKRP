;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-


(IN-PACKAGE "MKRP" :USE '("CL"))


(defun ema=i.tree (termlist trees supertree selector)
  ;(format t "~%Rec tree match, termlist: ~A, selector: ~A~% trees: ~A" termlist selector trees)
  (if (and (null termlist) (null trees))
      (ema=i.select (first (first (etree-store supertree))) selector)
      (some #'(lambda (tree)
		(let ((symbol (etree-symbol tree))
		      (first.term (first termlist)))
		  (cond ((and (eds-const_p first.term)
			      (eds-term_equal symbol first.term))
			 (ema=i.tree (rest termlist) (etree-subtrees tree) tree selector))
			((and (eds-term_p first.term)
			      (eds-fct_p (eds-term_topsymbol first.term))
			      (eds-term_equal symbol (eds-term_topsymbol first.term)))
			 (ema=i.tree (append (eds-term_arguments first.term) (rest termlist))
				     (etree-subtrees tree)
				     tree selector))
			((eds-var_p symbol)
			 (cond ((null (eds-var_binding symbol))
				(unwind-protect
				    (progn (eds-var_set.binding symbol first.term)
					   (ema=i.tree (rest termlist) (etree-subtrees tree)
						       tree selector))
				  (prog1 nil
					 (eds-var_set.binding symbol nil))))
			       ((eds-term_equal (eds-var_binding symbol)
						first.term)
				(let ((old.binding (eds-var_binding symbol)))
				  (unwind-protect
				      (progn (eds-var_set.binding symbol first.term)
					     (ema=i.tree (rest termlist) (etree-subtrees tree)
							 tree selector))
				    (prog1 nil
					   (eds-var_set.binding symbol nil)
					   (eds-var_set.binding symbol old.binding))))))))))
	    trees)))

(defun ema=apply (termlist)
						; Edited:  23-MAY-1990 13:57
						; Authors: PRCKLN
						; Input:   TERMLIST is a list of terms
						; Effect:  Recursively tries to apply all actual rules to all subterms and terms
						;          in TERMLIST, if once successful, it terminates.
						; Value:   True iff TERMLIST is changed
  (labels ((ema=do (termlist)
	     (somel #'(lambda (rest.tl)
			(if (eds-var_p (first rest.tl))
			    nil
			    (let* ((new.term (ema=interpret (first rest.tl))))
			      (if new.term
				  (rplaca rest.tl new.term)
				  (if (eds-term_p (first rest.tl))
				      (ema=do (eds-term_arguments (first rest.tl)))
				      nil)))))
		    termlist)))      
    (do* ((result (ema=do termlist) (ema=do termlist))
	  (reduced.p result (or reduced.p result)))
	((null result) reduced.p))))

(defun ema=apply1 (1-> 1= n-> n= termlist)
						; Edited:  23-MAY-1990 13:57
						; Authors: PRCKLN
						; Input:   TERMLIST is a list of terms
						; Effect:  Recursively tries to apply tree to all subterms and terms
						;          in TERMLIST, if once successful, it terminates.
						; Value:   True iff TERMLIST is changed
  (labels ((ema=do (termlist)
	     (somel #'(lambda (rest.tl)
			(if (eds-var_p (first rest.tl))
			    nil
			    (let* ((new.term (ema=interpret1 1-> 1= n-> n= (first rest.tl))))
			      (if new.term
				  (rplaca rest.tl new.term)
				  (if (eds-term_p (first rest.tl))
				      (ema=do (eds-term_arguments (first rest.tl)))
				      nil)))))
		    termlist)))
    (do* ((result (ema=do termlist) (ema=do termlist))
	  (reduced.p result (or reduced.p result)))
	 ((null result) reduced.p))))

(defun ema=i.trees (clause 1-> 1= n-> n=) 
  (or (if 1-> (ema=i.tree clause 1-> nil :1->) nil)
      (if 1= (ema=i.tree clause 1= nil :1=) nil)
      (if n-> (ema=i.tree clause n-> nil :n->) nil)
      (if n= (ema=i.tree clause n= nil :n=) nil)))

(defun ema=i.select (store selector)
  (case selector
    (:1-> (ema=i.apply.1-> (eds-rule_right.copy store)))
    (:1= (ema=i.apply.1= (eds-rule_left.copy store) (eds-rule_right.copy store)))
    (:n-> (ema=i.apply.n-> (eds-rule_right.copy store) (eds-rule_rest.copy store)))
    (:n= (ema=i.apply.n= (eds-rule_left.copy store) (eds-rule_right.copy store) (eds-rule_rest.copy store)))))

(defun ema=i.apply.1-> (right)
  (eds-ib_term1 right))  

(defun ema=i.apply.1= (left right)
  (let ((left.i (eds-ib_term1 left))
	(right.i (eds-ib_term1 right)))
    (if (eord-greater left.i right.i)  
	right.i
	nil)))

(defun ema=i.apply.n-> (right rest)
  (if (ectl=rewrite (eds-ib_terms rest))
      nil
      (eds-ib_term right)))

(defun ema=i.apply.n= (left right rest)
  (let ((left.i (eds-ib_term left))
	(right.i (eds-ib_term right)))
    (if (and (eord-greater left.i right.i)
	     (not (ectl=rewrite (eds-ib_terms rest))))
	right.i
	nil)))


(defun ema=interpret (term)
  (ema=i.trees (list term)
	       (etree-subtrees (eds-fct_tree.1-> (eds-term_topsymbol term)))
	       (etree-subtrees (eds-fct_tree.1= (eds-term_topsymbol term)))
	       (etree-subtrees (eds-fct_tree.n-> (eds-term_topsymbol term)))
	       (etree-subtrees (eds-fct_tree.n= (eds-term_topsymbol term)))))


(defun ema=interpret1 (1-> 1= n-> n= term)
  (ema=i.trees (list term) (etree-subtrees 1->) (etree-subtrees 1=) (etree-subtrees n->) (etree-subtrees n=)))









(defun ema=equal (ma.trees sma.tree bind.tree tree args)
  (if (eds-term_equal (etree-symbol bind.tree) (etree-symbol tree))
      (if (and (= 1 args) (not (eds-fct_p (etree-symbol tree))))
	  (ema=tree.tree (etree-subtrees tree) tree ma.trees sma.tree)
	  (mapc #'(lambda (bind.tree.sub)
		    (mapc #'(lambda (tree.sub)
			      (ema=equal ma.trees sma.tree bind.tree.sub tree.sub
					 (if (eds-fct_p (etree-symbol tree))
					     (+ -1 args (eds-fct_arity (etree-symbol tree)))
					     (1- args))))
			  (etree-subtrees tree)))
		(etree-subtrees bind.tree)))
      nil))

(defun ema=bind (ma.trees sam.tree bind.tree args)
  (if (and (= 1 args) (not (eds-fct_p (etree-symbol bind.tree))))
      (ema=tree.tree (etree-subtrees bind.tree) bind.tree ma.trees sma.tree)
      (mapc #'(lambda (bind.tree.sub)
		(ema=bind ma.trees sma.tree bind.tree.sub
			  (if (eds-fct_p (etree-symbol bind.tree))
			      (+ -1 args (eds-fct_arity (etree-symbol bind.tree)))
			      (1- args))))
	    (etree-subtrees bind.tree))))

(defun ema=tree.tree (trees ma.trees stree sma.tree)
  (cond ((null trees)
	 (mapc #'(lambda (store)
		   (ema=tree.select (eds-rule_copy (first (first (etree-store sma.tree))))
				    (first store) (rest store) selector))
	       (etree-store stree)))
	(t (mapc #'(lambda (tree)
		     (let ((symbol (etree-symbol tree)))
		       (some #'(lambda (ma.tree)
				 (let ((ma.symbol (etree-symbol ma.tree)))
				   (cond ((eds-var_p ma.symbol)
					  (let ((binding (eds-var_binding ma.symbol)))
					    (cond (binding (ema=equal (etree-subtrees ma.tree) ma.tree binding tree 1))
						  (t (unwind-protect
							 (progn (eds-var_set.binding ma.symbol tree)
								(ema=bind (etree-subtrees ma.tree) tree 1))
						       (eds-var_set.binding ma.symbol nil))))))
					 ((eds-term_equal ma.symbol symbol)
					  (ema=tree.tree (etree-subtrees tree) (etree-subtrees ma.treee) tree ma.tree))
					 (t nil))))
			     ma.trees)))
		 trees))))

(defun ema=tree.select (store selector rule term)
  (let ((clause (eds-rule_copy rule)))
    (case selector
      (:1-> (ema=tree.apply.1-> clause term (eds-rule_right.copy store)))
      (:1= (ema=tree.apply.1= clause term (eds-rule_left.copy store) (eds-rule_right.copy store)))
      (:n-> (ema=tree.apply.n-> clause term (eds-rule_right.copy store) (eds-rule_rest.copy store)))
      (:n= (ema=tree.apply.n= clause term (eds-rule_left.copy store) (eds-rule_right.copy store) (eds-rule_rest.copy store))))
    (ectl=retrack rule)))

(defun ema=tree.apply.1-> (rule term right)
  (eds-ib_term1 rule term right))  

(defun ema=tree.apply.1= (rule term left right)
  (let ((left.i (eds-ib_term1 left))
	(right.i (eds-ib_term1 right)))
    (if (eord-greater left.i right.i)  
	right.i
	nil)))

(defun ema=tree.apply.n-> (rule term right rest)
  (if (ectl=rewrite (eds-ib_terms rest))
      nil
      (eds-ib_term right)))

(defun ema=tree.apply.n= (rule term left right rest)
  (let ((left.i (eds-ib_term left))
	(right.i (eds-ib_term right)))
    (if (and (eord-greater left.i right.i)
	     (not (ectl=rewrite (eds-ib_terms rest))))
	right.i
	nil)))