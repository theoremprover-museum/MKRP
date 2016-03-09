;;; -*- Package: MKRP; Syntax: Common-Lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))


(defstruct (eds=term ;(:type list)
		     :named
		     (:print-function (lambda (struc stream level)
				       (declare (ignore level))
				       (prin1 (eds-names struc)
					      stream)))
		     (:predicate eds=term_p)
		     (:constructor eds=term_create)
		     (:conc-name eds=term_))
  topsymbol
  arguments
  index
  rule.p)

(defun eds-term_equal (term1 term2)
  (or (eq term1 term2)
      (and (eds-term_p term1)
	   (eds-term_p term2)
	   (eq (eds-term_topsymbol term1) (eds-term_topsymbol term2))
	   (every #'eds-term_equal (eds-term_arguments term1) (eds-term_arguments term2)))))

(defun eds-term_create (topsymbol arguments)
  (eds=term_create :topsymbol topsymbol :arguments arguments))

(defun eds-term_p (object)
  (eds=term_p object))

(defun eds-term_topsymbol (term)
  (if (eds=term_p term)
      (eds=term_topsymbol term)
      term))

(defun eds-term_index (term)
  (eds=term_index term))

(defun eds-term_set.index (term index)
  (setf (eds=term_index term) index))

(defun eds-term_rule.p (term)
  (eds=term_rule.p term))

(defun eds-term_set.rule.p (term rule.p)
  (setf (eds=term_rule.p term) rule.p))

(defun eds-term_arguments (term)
  (if (eds=term_p term)
      (eds=term_arguments term)
      nil))

(defstruct (eds=var :named
		    (:print-function (lambda (struc stream level)
				       (declare (ignore level))
				       (prin1 (list (eds=var_name struc)
						    (eds=var_binding struc)
						    (mapcar #'eds=var_name (eds=var_scope struc)))
					      stream)))
		    (:predicate eds=var_p)
		    (:constructor eds=var_create)
		    (:conc-name eds=var_))
  name
  binding
  scope)

(defun eds-var_p (term)
  (eds=var_p term))

(defun eds-var_create (name)
  (eds=var_create :name name))

(defun eds-var_set.binding (var bind)
  (setf (eds=var_binding var) bind))

(defun eds-var_binding (var)
  (eds=var_binding var))

(defun eds-var_push.scope (scope.var var)
  (push scope.var (eds=var_scope var)))

(defun eds-var_pop.scope (var)
  (pop (eds=var_scope var)))

(defun eds-var_scope (var)
  (eds=var_scope var))



(defun eds-const_create (name weight)
  (eds=fct_create :name name :weight weight :arity 0))

(defun eds-const_p (object) (and (eds=fct_p object) (zerop (eds=fct_arity object))))

(defun eds-const_weight (const)
  (eds=fct_weight const))

(defstruct (eds=fct (:type list)
		    :named
		    (:predicate eds=fct_p)
		    (:constructor eds=fct_create)
		    (:conc-name eds=fct_))
  name
  arity
  weight
  tree.1->
  tree.1=
  tree.n->
  tree.n=
  tree.ol)


(defun eds-fct_create (name arity weight)
  (eds=fct_create :name name :arity arity :weight weight))

(defun eds-fct_p (term)
  (and (eds=fct_p term) (not (zerop (eds=fct_arity term)))))

(defun eds-fct_arity (fct)
  (eds=fct_arity fct))

(defun eds-fct_weight (fct)
  (eds=fct_weight fct))

(defun eds-fct_tree.1-> (fct)
  (let ((tree (eds=fct_tree.1-> fct)))
    (or tree
	(setf (eds=fct_tree.1-> fct) (etree-create :symbol fct)))))

(defun eds-fct_tree.1= (fct)
  (let ((tree (eds=fct_tree.1= fct)))
    (or tree
	(setf (eds=fct_tree.1= fct) (etree-create :symbol fct)))))

(defun eds-fct_tree.n-> (fct)
  (let ((tree (eds=fct_tree.n-> fct)))
    (or tree
	(setf (eds=fct_tree.n-> fct) (etree-create :symbol fct))))) 

(defun eds-fct_tree.n= (fct)
  (let ((tree (eds=fct_tree.n= fct)))
    (or tree
	(setf (eds=fct_tree.n= fct) (etree-create :symbol fct)))))

(defun eds-fct_tree.ol (fct)
  (let ((tree (eds=fct_tree.ol fct)))
    (or tree
	(setf (eds=fct_tree.ol fct) (etree-create :symbol fct)))))

(defun eds-fct_eq.p (fct)
  (string= (eds=fct_name fct) "="))

(defstruct (eds=rule :named
		     (:print-function (lambda (struc stream level)
					   (declare (ignore level))
					   (prin1 (eds-names (eds=rule_clause struc))
						  stream)))
		     (:predicate eds=rule_p)
		     (:constructor eds=rule_create)
		     (:conc-name eds=rule_))
  clause
  pointers
  dead
  copy)


(defun eds-rule_create (head tail)
  (eds=rule_create :clause (cons head tail)))

(defun eds-rule_head (rule)
  (first (eds=rule_clause rule)))

(defun eds-rule_head.copy (rule)
  (first (eds=rule_copy rule)))

(defun eds-rule_left (rule)
  (first (eds-term_arguments (first (eds=rule_clause rule)))))

(defun eds-rule_left.copy (rule)
  (first (eds-term_arguments (first (eds=rule_copy rule)))))

(defun eds-rule_right (rule)
  (second (eds-term_arguments (first (eds=rule_clause rule)))))

(defun eds-rule_right.copy (rule)
  (second (eds-term_arguments (first (eds=rule_copy rule)))))

(defun eds-rule_rest (rule)
  (rest (eds=rule_clause rule)))

(defun eds-rule_rest.copy (rule)
  (rest (eds=rule_copy rule)))

(defun eds-rule_clause (rule)
  (eds=rule_clause rule))

(defun eds-rule_pointers (rule)
  (eds=rule_pointers rule))

(defun eds-rule_set.pointers (rule pointers)
  (setf (eds=rule_pointers rule) pointers))

(defun eds-rule_copy (rule)
  (eds=rule_copy rule))

(defun eds-rule_set.copy (rule copy)
  (setf (eds=rule_copy rule) copy))

(defun eds-rule_dead (rule)
  (eds=rule_dead rule))

(defun eds-rule_set.dead (rule dead)
  (setf (eds=rule_dead rule) dead))

(defstruct (eds=cp :named
		     (:predicate eds-cp_p)
		     (:constructor eds-cp_create)
		     (:conc-name eds-cp_))
  heur
  clause
  parents)

(defun eds-names (object)
  (cond ((eds=term_p object) (cons (eds-names (eds=term_topsymbol object)) (mapcar #'eds-names (eds=term_arguments object))))
	((eds=var_p object) (eds=var_name object))
	((eds=fct_p object) (intern (eds=fct_name object)))
	((null object) nil)
	(t (cons (eds-names (first object)) (eds-names (rest object))))))




(defun eds=ib_tree.branch (tree args from to)
  (if (= 1 args)
      (let ((result (eds=ib_tree tree from to)))
	(cons (list (first result)) (rest result)))
      (let* ((one (eds=ib_tree tree from to))
	     (rest (eds=ib_tree.branch (rest one) (1- args) from to)))
	(cons (cons (first one) (first rest))
	      (rest rest)))))

(defun eds=ib_tree (tree from to)
  (let* ((symbol (etree-symbol tree)))
    (cond ((eds-const_p symbol) (cons symbol (etree-actual tree)))
	  ((eds-var_p symbol)
	   (if (eds-var_binding symbol)
	       (cons (first (eds=ib_tree (eds-var_binding symbol) from to))
		     (etree-actual tree))
	       (cons symbol (etree-actual tree))))
	  ((eds-fct_p symbol)
	   (let ((result (eds=ib_tree.branch (etree-actual tree) (eds-fct_arity symbol) from to)))
	     (cons (eds-term_create symbol (first result)) (rest result)))))))

(defun eds-ib_tree (tree from to)
  (first (eds=ib_tree tree from to)))

(defun eds-ib_term (term &optional from to)
  (cond ((eds-term_equal from term) (eds-ib_term to))
	((eds-const_p term) term)
	((eds-var_p term)
	 (if (eds-var_binding term)
	     (eds-ib_tree (eds-var_binding term) from to)
	     term))
	((eds-term_p term)
	 (let ((topsymbol (eds-term_topsymbol term)))
	   (eds-term_create topsymbol (mapcar #'(lambda (subterm) (eds-ib_term subterm from to))
					      (eds-term_arguments term)))))))

(defun eds-ib_term1 (term &optional from to)
  (cond ((eds-term_equal from term) (eds-ib_term1 to))
	((eds-const_p term) term)
	((eds-var_p term)
	 (if (eds-var_binding term)
	     (eds-ib_term1 (eds-var_binding term) from to)
	     term))
	((eds-term_p term)
	 (let ((topsymbol (eds-term_topsymbol term)))
	   (eds-term_create topsymbol (mapcar #'(lambda (subterm) (eds-ib_term1 subterm from to))
					      (eds-term_arguments term)))))))

(defun eds-ib_term1 (term)
  (cond ((eds-const_p term) term)
	((eds-var_p term)
	 (let ((bind (eds-var_binding term)))
	   (if bind (eds-ib_term1 bind) term)))
	((eds-term_p term)
	 (let ((topsymbol (eds-term_topsymbol term)))
	   (eds-term_create topsymbol (mapcar #'(lambda (subterm) (eds-ib_term1 subterm))
					      (eds-term_arguments term)))))))

(defun eds-ib_terms (terlist &optional from to)
  (mapcar #'(lambda (term) (eds-ib_term term from to)) terlist))