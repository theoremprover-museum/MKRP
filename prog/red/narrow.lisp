;;; -*- package:  MARKGRAF-KARL; syntax: common-lisp; mode: lisp -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(import '(user::ds-rules-extend user::ds-make-term user::ds-make-variable user::out-format
				user::ds-term-symbol user::ds-term-subterms user::ds-variablep
			        USER::ds-make-clause user::ds-clause-literals user::ds-make-rule user::ds-rules-init 
				user::ds-term-eq-symbol user::ds-true user::ds-false
				user::ds-deriv-position user::ds-deriv-father user::ds-deriv-mother user::ds-deriv-reduction-list
				user::nar-nstep-sigma user::nar-nstep-deriv user::nar-nstep-eq user::nar-nstep-rest.clause
				user::nar-create-narrow.problem user::nar-solutions)
	(find-package "MKRP"))

(defparameter nar*term*alist nil)

(defparameter nar*clauses nil)

; Narrowing

(defun nar-narrow (clauses)
  (when (> (opt-get.option er_narrow.depth) 0)
    (mapc #'(lambda (clause)
	      (when (or (> (ds-clause.nolit clause) 1)
			(ds-sign.is.negative (ds-clause.sign clause 1)))
		(ds-clause.do #'(lambda (litno)
				  (when (ds-clause.lit.is.max clause litno)
				    (cond ((and (ds-sign.is.negative (ds-clause.sign clause litno))
						(dt-predicate.is.equality (ds-clause.predicate clause litno)))
					   (nar=insert.tree.eq clause litno))
					  ((not (dt-predicate.is.equality (ds-clause.predicate clause litno)))
					   (mapc #'(lambda (occ)
						     (nar=insert.tree clause litno occ))
						 (if (ds-sign.is.positive (ds-clause.sign clause litno))
						     (dt-predicate.negative.occurrences (ds-clause.predicate clause litno))
						     (dt-predicate.positive.occurrences (ds-clause.predicate clause litno))))))))
			      clause)))
	  clauses)))


(defun nar=insert.tree.eq (clause litno)
  (let ((tree (nar=create.tree.eq (first (ds-clause.termlist clause litno))
				      (second (ds-clause.termlist clause litno))
				      clause litno)))
    (mapc #'(lambda (sol)
	      (let ((link (ds-link.create 'r (list (nar=translate.subst.from (cdr sol)))
					  clause litno
					  (dt-predicate.refl.clause (ds-clause.predicate clause litno)) 1)))
		(ds-link.put.sort.inhibited link 'inhibited)
		(cg-insert.link link nil 'narrow)				    
		(dt-putprop link 'red*narrowing (car sol))))
	  (nar-solutions tree))))

(defun nar=create.tree.eq (term1 term2 clause litno)
  (nar-create-narrow.problem (nar=translate.term.to
			       (dt-term_create (first (dt-predicate.equalities))
					       (list term1 term2)))
			     (opt-get.option er_narrow.depth)
			     (nar=make.rules (red-info_p.link.rules t))
			     :size (opt-get.option er_narrow.next)
			     :tests (opt-get.option er_narrow.test)
			     :rest.clause (nar=translate.clause.to clause litno)))

(defun nar=insert.tree (clause1 litno1 occ)
  (let ((clause2 (first occ)))
    (mapc #'(lambda (litno2)
	      (let ((tree (nar=create.tree clause1 litno1 clause2 litno2)))
		(mapc #'(lambda (sol)
			  (let ((link (ds-link.create 'r (list (nar=translate.subst.from (cdr sol)))
						      clause1 litno1 clause2 litno2)))
			    (ds-link.put.sort.inhibited link 'inhibited)
			    (cg-insert.link link nil 'narrow)				    
			    (dt-putprop link 'red*narrowing (car sol))))
		      (nar-solutions tree))))
	  (rest occ))))

(defun nar=create.tree (clause1 litno1 clause2 litno2)
  (nar-create-narrow.problem (nar=translate.term.to
			       (dt-term_create (first (dt-predicate.equalities))
					       (list (dt-term_create (ds-clause.predicate clause1 litno1)
								     (ds-clause.termlist clause1 litno1))
						     (dt-term_create (ds-clause.predicate clause2 litno2)
								     (ds-clause.termlist clause2 litno2)))))
			     (opt-get.option er_narrow.depth)
			     (nar=make.rules (red-info_p.link.rules t))
			     :size (opt-get.option er_narrow.next)
			     :tests (opt-get.option er_narrow.test)
			     :rest.clause (list (nar=translate.clause.to clause1 litno1)
						(nar=translate.clause.to clause2 litno2))))


;;; Protocol
;;; --------

(defun nar-narrow.info (link)
  (dt-getprop link 'red*narrowing))

(defun nar-narrow.info.put (link info)
  (dt-putprop link 'red*narrowing info))

(defun nar-pr.narrow (link resolvent)
  (let ((nstep (dt-getprop link 'red*narrowing)))
    (if (consp (nar-nstep-rest.clause nstep))
	(multiple-value-bind (clause1 litno1 clause2 litno2) (nar=deriv.list nstep
									     (ds-link.pospar link) (ds-link.poslitno link)
									     (ds-link.negpar link) (ds-link.neglitno link))
	  (let ((new.link (ds-link.create 'r (first (ds-link.unifiers link))
					  clause1 litno1 clause2 litno2)))
	    (ds-clause.put.parents resolvent (list clause1 clause2))
	    (pr-operation 'resolution new.link (first (ds-link.unifiers link)) resolvent)))
	(multiple-value-bind (clause1 litno1) (nar=deriv.list.eq nstep
								 (ds-link.pospar link) (ds-link.poslitno link)
								 (ds-link.negpar link) (ds-link.neglitno link))
	  (let ((new.link (ds-link.create 'r (first (ds-link.unifiers link))
					  clause1 litno1 (ds-link.negpar link) (ds-link.neglitno link))))
	    (ds-clause.put.parents resolvent (list clause1 (ds-link.negpar link)))
	    (pr-operation 'resolution new.link (first (ds-link.unifiers link)) resolvent))))
    (mapc #'(lambda (clause) (cg-remove.clause clause 'narrow nil)) nar*clauses)
    (setq nar*clauses nil)))

(defun nar=deriv.list.eq (nstep clause1 litno1 clause2 litno2)
  (let ((deriv (nar-nstep-deriv nstep)))
    (cond ((ds-deriv-father deriv)
	   (multiple-value-setq (clause1 litno1) (nar=deriv.list.eq (ds-deriv-mother deriv) clause1 litno1 clause2 litno2))
	   (let ((sigma (nar=translate.subst.from (nar-nstep-sigma nstep)))
		 (rules (mapcar #'(lambda (rule) (rds-rw_rule.clause (car (rassoc rule nar*term*alist))))
				  (ds-deriv-reduction-list deriv)))
		 (rw_rule (car (rassoc (ds-deriv-father deriv) nar*term*alist)))
		 rensubst)
	     (pr-operation 'paramodulation
			   (ds-link.create 'p (list sigma) clause1 litno1 (rds-rw_rule.clause rw_rule) 1)
			   sigma
			   (setq litno1 1
				 clause1 (let ((litlist (op=par_transmit.literal
							  clause1 litno1 (ds-deriv-position deriv)
							  (rds-rw_rule.right rw_rule)
							  sigma
							  (op=transmit.literals clause1 litno1 sigma nil (list nil) nil)
							  (list nil) nil)))
					   (ds-clause.create (nar=n.name)
							     (list clause1 (rds-rw_rule.clause rw_rule))
							     (1+ (ds-clause.depth clause1))
							     (UNI-APPLY.SUBSTITUTION
							       (setq rensubst
								     (DT-VARIABLE.RENAMING.SUBSTITUTION
								       (dt-termlist.variables
									 (mapcan #'(lambda (lit)
										     (copy-list (ds-lit.termlist lit)))
										 litlist))))
							       litlist
							       t)))))
	     (cg-insert.clause clause1 sigma nil 'narrow)
	     (push clause1 nar*clauses)
	     (when rules
	       ;(nar=rewrite clause1 (cons (nar-nstep-eq nstep) (ds-clause-literals (nar-nstep-rest.clause nstep))) rules)
	       (cg-replace.literal clause1 litno1 (ds-clause.sign clause1 litno1)
				   (ds-clause.predicate clause1 litno1)
				   (UNI-APPLY.SUBSTITUTION
				     rensubst
				     (mapcar #'nar=translate.term.from (ds-term-subterms (nar-nstep-eq nstep))))
				   'rewrite rules)
	       (pr-operation 'rewrite rules clause1 litno1)))
	   (values clause1 litno1))
	  (t (values clause1 litno1)))))

(defun nar=deriv.list (nstep clause1 litno1 clause2 litno2)
  (let ((deriv (nar-nstep-deriv nstep)))
    (cond ((ds-deriv-father deriv)
	   (multiple-value-setq (clause1 litno1 clause2 litno2)
	     (nar=deriv.list (ds-deriv-mother deriv) clause1 litno1 clause2 litno2))
	   (let ((position (ds-deriv-position deriv)))
	     (cond ((= (car position) 1)
		    (multiple-value-setq (clause1 litno1)
		      (nar=pr.operation nstep clause1 litno1 (cdr position)
					(car (ds-term-subterms (nar-nstep-eq nstep)))
					(ds-clause-literals (car (nar-nstep-rest.clause nstep)))
					(mapcar #'(lambda (rule) (rds-rw_rule.clause (car (rassoc rule nar*term*alist))))
						(ds-deriv-reduction-list deriv)))))
		   (t 
		    (multiple-value-setq (clause2 litno2)
		      (nar=pr.operation nstep clause2 litno2 (cdr position)
					(cadr (ds-term-subterms (nar-nstep-eq nstep)))
					(ds-clause-literals (cadr (nar-nstep-rest.clause nstep)))
					(mapcar #'(lambda (rule) (rds-rw_rule.clause (car (rassoc rule nar*term*alist))))
						(ds-deriv-reduction-list deriv)))))))
	   (values clause1 litno1 clause2 litno2))
	  (t (values clause1 litno1 clause2 litno2)))))

(defun nar=pr.operation (nstep clause litno position literal rest.literals rules)
  (let* ((deriv (nar-nstep-deriv nstep))
	 (sigma (nar=translate.subst.from (nar-nstep-sigma nstep)))
	 (rw_rule (car (rassoc (ds-deriv-father deriv) nar*term*alist)))
	 rensubst)
    (pr-operation 'paramodulation
		  (ds-link.create 'p (list sigma) clause litno (rds-rw_rule.clause rw_rule) 1)
		  sigma
		  (setq litno 1
			clause (let ((litlist (op=par_transmit.literal
						clause litno position
						(rds-rw_rule.right rw_rule)
						sigma
						(op=transmit.literals clause litno sigma nil (list nil) nil)
						(list nil) nil)))
				 (ds-clause.create (nar=n.name)
						   (list clause  (rds-rw_rule.clause rw_rule))
						   (1+ (ds-clause.depth clause))
						   (setq rensubst
							 (UNI-APPLY.SUBSTITUTION
							   (DT-VARIABLE.RENAMING.SUBSTITUTION
							     (dt-termlist.variables
							       (mapcan #'(lambda (lit)
									   (copy-list (ds-lit.termlist lit))) litlist)))
							   litlist
							   t))))))
    (cg-insert.clause clause sigma nil 'narrow)
    (push clause nar*clauses)
    (when rules					;
      (nar=rewrite clause (append (subseq rest.literals 0 (- litno 1)) literal (subseq rest.literals (- litno 1)))
		   rules rensubst)
      (pr-operation 'rewrite rules clause litno))
    (values clause litno)))

(defun nar=rewrite (clause new-lits rules rensubst)
  (do* ((litno 1 (1+ litno))
	(lits  new-lits (cdr lits)))
       ((cond ((and (> litno (ds-clause.nolit clause)) (null lits)) t)
	      ((> litno (ds-clause.nolit clause)) (break "Zu wenig Literale"))
	      ((null lits) (break "Zu viele Literale"))
	      (t nil))
	())
    (cg-replace.literal clause litno (ds-clause.sign clause litno)
			(ds-clause.predicate clause litno)
			(UNI-APPLY.SUBSTITUTION rensubst (mapcar #'nar=translate.term.from (ds-term-subterms (car lits))))
			'rewrite rules)))

(defparameter n*number 0)

(defun nar=n.name ()
  (format nil "N~A" (incf n*number)))


;;; Translation to (MKRP -> NARROW)
;;;--------------------------------


(defun nar=translate.term.to (object)
  (let ((my.name (unless (consp object) (cdr (assoc object nar*term*alist)))))
    (cond ((dt-predicate.is object)
	   (if (dt-predicate.is.equality object)
	       (ds-term-eq-symbol)
	       (or my.name 
		   (progn (setf nar*term*alist (acons object (dt-predicate.pname object) nar*term*alist))
			  (cdar nar*term*alist)))))
	  ((dt-function.is object)
	   (or my.name (progn (setf nar*term*alist (acons object (dt-function.pname object) nar*term*alist))
			      (cdar nar*term*alist))))
	  ((dt-constant.is object)
	   (ds-make-term :symbol (or my.name (progn (setf nar*term*alist (acons object (dt-constant.pname object)
										nar*term*alist))
						    (cdar nar*term*alist)))
			 :subterms nil))
	  ((dt-variable.is object)
	   (ds-make-variable (or my.name (progn (setf nar*term*alist (acons object (dt-variable.pname object) nar*term*alist)) 
						(cdar nar*term*alist)))))
	  (t (ds-make-term :type 'func
			   :symbol (nar=translate.term.to (dt-term_topsymbol object))
			   :subterms (mapcar #'nar=translate.term.to (dt-term_arguments object)))))))

(defun nar=make.rules (rule-list1)
  (do ((number 0 (incf number))
       (rules (ds-rules-init) (ds-rules-extend rules (nar=translate.rule.to (car rule-list) number)))
       (rule-list rule-list1 (cdr rule-list)))
      ((null rule-list) rules)))

(defun nar=translate.rule.to (rule &optional (number 0))
  (let ((my-rule (ds-make-rule :head-lhs (nar=translate.term.to (rds-rw_rule.left rule))
			       :head-rhs (nar=translate.term.to (rds-rw_rule.right rule))
			       :number number)))
    (setf nar*term*alist (acons rule my-rule nar*term*alist))
    my-rule))

(defun nar=translate.clause.to (clause litno)
  ;(cg-dump t `((clauses (,clause) i)))
  (do* ((litno1 0 (1+ litno1))
	(lits nil (if (= litno1 litno)
		      lits
		      (cons (nar=translate.lit.to clause litno1) lits))))
       ((= litno1 (ds-clause.nolit clause))
	(ds-make-clause :literals lits))))

(defun nar=translate.lit.to (clause litno)
  (let* ((termlist (mapcar #'nar=translate.term.to (ds-clause.termlist clause litno)))
	 (nar.lit (ds-make-term :symbol (ds-term-eq-symbol)
				:subterms (if (dt-predicate.is.equality (ds-clause.predicate clause litno))
					      termlist
					      (list (ds-make-term :symbol (nar=translate.term.to (ds-clause.predicate clause litno))
								  :subterms termlist)
						    (if (ds-sign.is.positive (ds-clause.sign clause litno))
							(ds-true)
							(ds-false)))))))
    (format t "~%Lit: ~A" (user::out-format nar.lit))
    nar.lit))


;Translation from (NARROW -> MKRP)

(defun nar=translate.subst.from (subst)
  (mapcan #'(lambda (pair) (list (let ((var (car (rassoc (first pair) nar*term*alist)))) (unless var (break "var ist nil")) var)
				 (nar=translate.term.from (rest pair))))
	  subst))

(defun nar=translate.term.from (term)
  (let ((mkrp.term (car (rassoc (ds-term-symbol term) nar*term*alist))))
    (cond ((ds-variablep term) (or mkrp.term (let ((mkrp.var (dt-variable.create 'any)))
					       (setf nar*term*alist (acons mkrp.var (ds-term-symbol term) nar*term*alist))
					       mkrp.var)))
	  ((null (ds-term-subterms term)) (or mkrp.term (error "Constants cannot occur here")))
	  (t (if mkrp.term
		 (cons mkrp.term (mapcar #'nar=translate.term.from (ds-term-subterms term)))
		 (error "Unbekannte Funktion"))))))

(defun nar-reset ()
  (setq nar*term*alist nil)
  (setq nar*clauses nil))