;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-


(IN-PACKAGE "MKRP" :USE '("CL"))

(defvar ectl*axioms)
(defvar ectl*theorems)
(defvar ectl*true)
(defvar ectl*false)
(defvar ectl*equality)

(defparameter ectl*cp (make-array 100 :adjustable t))

(defparameter ectl*index 0)

(defun ectl=symbols (termlist)
  (labels ((symbols (termlist)
	     (apply #'+ (mapcar #'(lambda (term)
				    (if (eds-term_p term)
					(1+ (symbols (eds-term_arguments term)))
					1))
				termlist))))
    (if nil
	(symbols termlist)
	(max (symbols (list (first (eds-term_arguments (first termlist)))))
	     (symbols (list (second (eds-term_arguments (first termlist)))))))))

(defun ectl=cp_insert (rule index parents) ;(format t "Rule ~A~%" (first rule))
  (let ()
    (when (>= index (array-dimension ectl*cp 0))
      (adjust-array ectl*cp (* 2 index)))
    (setq rule (ectl=copy.New rule))
    ;(ema=apply rule)
    (setq ectl*index (min index ectl*index))
    (setf (aref ectl*cp index) (qcons (eds-cp_create :clause rule :parents parents :heur index) (aref ectl*cp index)))))

(defun ectl=cp_insert.cp (cp) ;(format t "Rule ~A~%" (first rule))
  (let ((rule (eds-cp_clause cp))
	(index (eds-cp_heur cp)))
    (when (>= index (array-dimension ectl*cp 0))
      (adjust-array ectl*cp (* 2 index)))
    (setq rule (ectl=copy.New rule))
    ;(ema=apply rule)
    (setq ectl*index (min index ectl*index))
    (setf (aref ectl*cp index) (qcons cp (aref ectl*cp index)))))

(defparameter ectl*rule nil)

(defun ectl=rule_insert (rule)
  (let ((fct (eds-term_topsymbol (eds-rule_left rule))))
    (etree-insert (if (eds-term_rule.p (eds-rule_head rule))
		      (if (eds-rule_rest rule)
			  (eds-fct_tree.n-> fct)
			  (eds-fct_tree.1-> fct))
		      (if (eds-rule_rest rule)
			  (eds-fct_tree.n= fct)
			  (eds-fct_tree.1= fct)))
		  (list (eds-rule_left.copy rule))
		  rule))
  (eds-rule_set.dead rule nil) 
  (push rule ectl*rule))

(defun ectl=rule_delete (rule)
  (etree-remove rule)
  (setf (eds-cp_parents rule) nil)
  (setq ectl*rule (delete rule ectl*rule))
  (eds-rule_set.dead rule :dead))


(defun ectl=pop (n)
  (if (first (aref ectl*cp n))
      (let ((clause (first (first (aref ectl*cp n)))))
	(qdelete-nth (aref ectl*cp n) 1)
	(if (and (eds-cp_parents clause)
		 (or (eds-rule_dead (first (eds-cp_parents clause)))
		     (eds-rule_dead (second (eds-cp_parents clause)))))
	    (ectl=pop n)
	    (progn (setq ectl*index n) clause)))
      (ectl=pop (1+ n))))

(defun ectl=select ()
  (ectl=pop ectl*index))

(defun ectl=reset ()
  (setq ectl*rule nil ectl*var.num 0 ectl*index 0
	ectl*variables (list (eds-var_create (ectl=var)))
	ectl*pointer.variables ectl*variables)
  (dotimes (n (array-dimension ectl*cp 0))
    (setf (aref ectl*cp n) nil)))

(defun ectl-eprove (filename)
  (load filename)
  (time (progn
	  (ectl=reset)
	  (let* ((var (eds-var_create "XX"))
		 (clause (list (eds-term_create ectl*equality (list (eds-term_create ectl*equality (list var var)) ectl*true)))))
	    (ectl=insert.rule (eds-cp_create :heur (ectl=symbols clause) :clause clause)))
	  (mapc #'(lambda (clause) (ectl=insert.rule (eds-cp_create :heur 10000 :clause clause)))
		ectl*axioms)
	  (mapc #'(lambda (clause) (ectl=insert.rule (eds-cp_create :heur 10000 :clause clause)))
		(first ectl*theorems))		;Splitparts to do
	  (while (and ectl*cp (not (eq :empty (ectl=insert.rule (ectl=select)))))))))


(defun ectl=rewrite (cp)
									      ; Edited:  29-JAN-1991 23:12
									      ; Authors: PRCKLN
									      ; Input:   A pair CP
									      ; Effect:  
									      ; Value:   
  (let* ((old (eds-cp_heur cp))
	 (clause (eds-cp_clause cp)))
    (ema=apply clause)
    (let ((symbols (ectl=symbols clause)))
      (cond ((> symbols old)
	     (setf (eds-cp_heur cp) symbols)
	     (ectl=cp_insert.cp cp)
	     (ectl=rewrite (ectl=select)))
	    (t (if (member ectl*true clause)
		   :true
		   (delete-if #'(lambda (lit) (and (eds-term_equal (eds-term_topsymbol lit) ectl*equality)
						   (member ectl*false (eds-term_arguments lit))
						   (member ectl*true (eds-term_arguments lit))))
			      clause)))))))

(defun ectl=tautology (clause) clause)

(defun ectl=reduce (cp)
  (if (eq (setq cp (ectl=rewrite cp)) :true)
      :true
      (ectl=tautology cp)))

(defvar ectl*var.num 0)

(defun ectl=var ()
  ;(format nil "X~D" (incf ectl*var.num))
  (incf ectl*var.num))

(defvar ectl*variables (list (eds-var_create (ectl=var))))
(defvar ectl*pointer.variables ectl*variables)
						
(defun ectl=copy.new (termlist)
  (let ((old.vars nil))
    (labels ((ectl=cons.term (termlist)
	       (mapl #'(lambda (rest.termlist)
			 (let ((term (first rest.termlist)))
			   (cond ((eds-var_p term) (rplaca rest.termlist (ectl=get.var term)))
				 ((eds-const_p term) term)
				 (t (ectl=cons.term (eds-term_arguments term))))))
		     termlist))
	     (ectl=get.var (variable)
	       (or (eds-var_binding variable)
		   (let ((new (eds-var_create (ectl=var))))
		     (push variable old.vars)
		     (eds-var_set.binding variable new)
		     new))))
      (unwind-protect
	  (ectl=cons.term termlist)
	(mapc #'(lambda (var) (eds-var_set.binding var nil)) old.vars)))))

#| Old version copying the whole structure
(defun ectl=copy.new (termlist)
  (let ((old.vars nil))
    (labels ((ectl=cons.term (term)
	       (cond ((eds-var_p term) (ectl=get.var term))
		     ((eds-const_p term) term)
		     (t (eds-term_create (eds-term_topsymbol term)
					 (mapcar #'ectl=cons.term (eds-term_arguments term))))))
	     (ectl=get.var (variable)
	       (or (eds-var_binding variable)
		   (let ((new (eds-var_create (ectl=var))))
		     (push variable old.vars)
		     (eds-var_set.binding variable new)
		     new))))
      (unwind-protect
	  (mapcar #'ectl=cons.term termlist)
	(mapc #'(lambda (var) (eds-var_set.binding var nil)) old.vars)))))
|#

(defun ectl=copy (termlist)
  (let ((old.vars nil))
    (labels ((ectl=cons.term (term)
	       (cond ((eds-var_p term) (ectl=get.var term))
		     ((eds-const_p term) term)
		     (t (eds-term_create (eds-term_topsymbol term)
					 (mapcar #'ectl=cons.term (eds-term_arguments term))))))
	     (ectl=get.var (variable)
	       (or (eds-var_binding variable)
		   (cond ((rest ectl*pointer.variables)
			  (push variable old.vars)
			  (eds-var_set.binding variable (pop ectl*pointer.variables)))
			 (t (nconc1 ectl*pointer.variables (eds-var_create (ectl=var)))
			    (ectl=get.var variable))))))
      (unwind-protect (mapcar #'ectl=cons.term termlist)
	(mapc #'(lambda (var) (eds-var_set.binding var nil)) old.vars)
	(setq ectl*pointer.variables ectl*variables)))))

(defun ectl=rule.create (lit rest)
  (let ((rule (eds-rule_create lit rest)))
    (eds-rule_set.copy rule (ectl=copy (eds-rule_clause rule)))
    rule))
  
(defun ectl=make.rules (lit clause)
  (let* ((rest (remove lit clause))
	 (subterms (eds-term_arguments lit))
	 (term1 (first subterms))
	 (term2 (second subterms)))
    (if (eord-greater term1 term2)
	(progn (eds-term_set.rule.p lit t)
	       (let ((new (ectl=rule.create lit rest)))
		 (list new)))
	(if (eord-greater term2 term1)
	    (progn (eds-term_set.rule.p lit t)
		   (psetf (first subterms) term2
			  (second subterms) term1)
		   (let ((new (ectl=rule.create lit rest)))
		     (list new)))
	    (let* ((new1 (ectl=rule.create lit rest))
		   (new2 (ectl=rule.create (eds-term_create (eds-term_topsymbol lit)
							    (reverse (eds-term_arguments lit)))
					   rest)))
	      (list new1 new2))))))

(defun ectl=retrack (rule)
  (ectl=rule_delete rule)
  (ectl=cp_insert (eds-rule_clause rule) (ectl=symbols (eds-rule_clause rule)) nil))

(defun ectl=back.reduce (red.rule)
  (let ((tree (etree-create)) 1-> 1= n-> n=)
    (etree-insert tree (list (eds-rule_left.copy red.rule)) red.rule)
    (if (eds-term_rule.p (eds-rule_head red.rule))
	(if (eds-rule_rest.copy red.rule)
	    (setq n-> tree)
	    (setq 1-> tree))
	(if (eds-rule_rest.copy red.rule)
	    (setq n= tree)
	    (setq 1= tree)))
    (mapc #'(lambda (rule)
	      (when (ema=apply1 1-> 1= n-> n= (eds-rule_clause rule))
		(ectl=retrack rule)))
	  ectl*rule)))

(defun ectl=term.into.ol (term rule)
  (when (eds-term_p term)
    (etree-insert (eds-fct_tree.ol (eds-term_topsymbol term)) (list term) rule term)
    (mapc #'(lambda (subterm) (ectl=term.into.ol subterm rule)) (eds-term_arguments term))))

(defun ectl=push (rule)
  (ectl=term.into.ol (eds-rule_left.copy rule) rule))

(defun ectl=overlap.sub (left rule)
  (when (eds-term_p left)
    (mapc #'(lambda (subterm)
	      (unless (eds-var_p subterm)
		(let ((tree (etree-create)))
		  (ectl=overlap.sub subterm rule)
		  (etree-insert tree (list subterm) rule subterm)
		  (euni-top (etree-subtrees (eds-fct_tree.1-> (eds-term_topsymbol subterm)))
			    (etree-subtrees tree)
			    #'(lambda (term store1 store2)
				(let ((clause (cons (eds-ib_term (eds-rule_head rule)
								 term
								 (eds-rule_right.copy store2))
						    (append (eds-ib_terms (eds-rule_rest.copy store2))
							    (eds-ib_terms (eds-rule_rest rule))))))
				  (ectl=cp_insert clause (ectl=symbols clause) (list rule store2))
				  nil))))))
	  (eds-term_arguments left))))

(defun ectl=overlap (rule)
  (let ((tree (etree-create)))
    (etree-insert tree (list (eds-rule_left rule)) rule)
    (ectl=overlap.sub (eds-rule_left rule) rule)
    (euni-top (etree-subtrees tree)
	      (etree-subtrees (eds-fct_tree.ol (eds-term_topsymbol (eds-rule_left rule))))
	      #'(lambda (term store1 store2)					     
		  (let ((clause (cons (eds-ib_term (eds-rule_head.copy store1)
						   term
						   (eds-rule_right rule))
				      (append (eds-ib_terms (eds-rule_rest store1))
					      (eds-ib_terms (eds-rule_rest.copy rule))))))
		    (ectl=cp_insert clause (ectl=symbols clause) (list rule store1))
		    nil)))))

(defun ectl=insert.rule (clause)
  (unless (eq :true (setq clause (ectl=reduce clause)))	; Reduction, tautology, and subsumption
    (if clause
	(let ((max.lits (ectl=compute.max.litno clause)))
	  (mapc #'(lambda (max.lit)
		    (mapc #'(lambda (new.clause)
			      ;(mkrp-gc.start t)
			      (format t "New rule: ~A~%" new.clause) ;(break "Insert rule")
			      (ectl=back.reduce new.clause)
			      (ectl=rule_insert new.clause)
			      (ectl=push new.clause)
			      (ectl=overlap new.clause))
			  (ectl=make.rules max.lit clause)))
		max.lits))
	:empty)))

(defun ectl=compute.max.litno (clause)
						; Edited:  29-JAN-1990 15:40
						; Authors: PRCKLN
						; Input:   A clause
						; Effect:  -
						; Value:   The maximal literals according to
						;          the option specified completion strategy.
  (let ((lits (copy-list clause)))
    (mapc #'(lambda (lit1)
	      (when (some #'(lambda (lit2)
			      (and (not (eq lit2 lit1))
				   (eord-greater lit2 lit1)))
			  lits)
		(setq lits (remove lit1 lits))))
	  lits)
    lits))