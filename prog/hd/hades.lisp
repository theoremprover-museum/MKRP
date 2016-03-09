;;; -*- package: mkrp; syntax: common-lisp; mode: lisp -*-

(in-package "HADES")

(in-package "MKRP")

;;; theory import

(import '(hades::th-abelian.semigroup hades::th-semigroup.with.exponent hades::th-abelian.monoid hades::th-abelian.group)
	(find-package "MKRP"))

(import '(hades::term-create hades::theo-get hades::term-theory.install hades::term-instantiate hades::acm-unify.free.semigroups 
			     hades::term-function.create hades::subst-p hades::do-send
			     hades::term-constant.create hades::term-variable.create
			     hades::term-constant.p hades::term-variable.p hades::do-new.environment hades::do-protect
			     hades::do-p hades::instantiation hades::UNI-UNIFICATION.CLASS hades::UNI-TERMLISTS.UNIFICATION.CLASS 
			     hades::term-function.p hades::do-get hades::do-put hades::subst-domain hades::subst-codomain
			     hades::term-p hades::term-topsymbol hades::term-arguments hades::ttrns*protocoll
			     hades::uni-create.unification.problem hades::uni-substitutions
			     hades::au-unify.in.arbitrary.disjoint.equational.theories
			     hades::UNI-UNIFICATION.CLASS.UNIFICATION-TYPE)
	(find-package "MKRP"))

(setq ttrns*protocoll nil)
(defun hd-reset ())

(defun hd=instantiate (thname symbols)
  (let* ((signature (theo-get thname :signature))
	 (nsymbols (mapcar #'(lambda (sig name)
			       (list sig (if (consp name) (car name) name)
				     (if (consp name) (cadr name) name)
				     :FUNCTION T))	;umstellen auf :function
			   signature symbols)))
    (term-instantiate thname 'instantiation nsymbols nil)
    (term-theory.install :instantiation (term-instantiate thname 'instantiation
							  (mapcar #'(lambda (sig name)
								      (list sig (if (consp name) (car name) name)
									    (if (consp name) (cadr name) name)
									    :FUNCTION T))	;umstellen auf :function
								  signature symbols)
							  nil))
    symbols))

(defun hd=translate.associative.to (object)
						; Edited:  26-SEP-1991 17:05
						; Authors: PRCKLN
						; Input:   An associative MKRP function symbol.
						; Effect:  Creates an HADES ass function and an exponent function.
						; Value:   The created HADES function.
  (let ((res (hd=instantiate 'th-semigroup.with.exponent
			     (list (intern (dt-function.pname object)
					   (find-package "MKRP1"))
				   (intern (format nil "EXP-~A" (dt-function.pname object))
					   (find-package "MKRP1")))))
	(fun (dt-function.create (format nil "EXP-~A" (dt-function.pname object)) 'any '(any any))))
    (do-protect (symbol-value (first res)))
    (do-protect (symbol-value (second res)))
    (dt-putprop fun 'hd*object (symbol-value (second res)))
    (do-put (symbol-value (second res)) 'hd*object fun)
    (symbol-value (first res))))

(defun hd=translate.ac.to (object)
						; Edited:  26-SEP-1991 17:07
						; Authors: PRCKLN
						; Input:   An MKRP AC function symbol.
						; Effect:  Creates a corresponding HADES function.
						; Value:   The created HADES function.
  (let ((res (hd=instantiate 'th-abelian.semigroup (list (intern (dt-function.pname object)
								 (find-package "MKRP1"))))))
    (do-protect (symbol-value (first res)))
    (symbol-value (first res))))

(defun hd=translate.ac1.to (object)
						; Edited:  26-SEP-1991 17:08
						; Authors: PRCKLN
						; Input:   An MKRP AC1 function symbol.
						; Effect:  Creates the HADES function and a corresponding one.
						; Value:   The new function symbol.
  (let ((res (hd=instantiate 'th-abelian.monoid (list (intern (dt-function.pname object)
							      (find-package "MKRP1"))
						      (intern (dt-constant.pname
								(dt-getprop object 'dt*null))
							      (find-package "MKRP1"))))))
    (do-protect (symbol-value (first res)))
    (do-protect (symbol-value (second res)))
    (dt-putprop (dt-getprop object 'dt*null) 'hd*object (symbol-value (second res)))
    (do-put (symbol-value (second res)) 'hd*object (dt-getprop object 'dt*null))
    (symbol-value (first res))))

(defun hd=translate.ag.to (object)
						; Edited:  26-SEP-1991 17:08
						; Authors: PRCKLN
						; Input:   An MKRP AG function symbol.
						; Effect:  Creates the HADES function and a corresponding one and minus.
						; Value:   The new function symbol.
  (let ((res (hd=instantiate 'th-abelian.group (list (intern (dt-function.pname object)
							     (find-package "MKRP1"))
						     (intern (dt-function.pname
							       (dt-getprop object 'dt*minus))
							     (find-package "MKRP1"))
						     (intern (dt-constant.pname
							       (dt-getprop object 'dt*null))
							     (find-package "MKRP1"))))))
    (do-protect (symbol-value (first res)))
    (do-protect (symbol-value (second res)))
    (do-protect (symbol-value (third res)))
    (dt-putprop (dt-getprop object 'dt*null) 'hd*object (symbol-value (third res)))
    (dt-putprop (dt-getprop object 'dt*minus) 'hd*object (symbol-value (second res)))
    (do-put (symbol-value (third res)) 'hd*object (dt-getprop object 'dt*null))
    (do-put (symbol-value (second res)) 'hd*object (dt-getprop object 'dt*minus))
    (symbol-value (first res))))

(defun hd=translate.objects.to (object)
  (let ((hd.object (if (dt-type object) (dt-getprop object 'hd*object))))
    (if (and hd.object (do-p hd.object))
	hd.object
	(let ((hd.object
		(cond ((dt-function.is object)
		       (cond ((DT-FUNCTION.IS.MARKED associative object)
			      (hd=translate.associative.to object))
			     ((DT-FUNCTION.IS.MARKED ac object)
			      (hd=translate.ac.to object))
			     ((DT-FUNCTION.IS.MARKED ac1 object)
			      (hd=translate.ac1.to object))
			     ((DT-FUNCTION.IS.MARKED ag object)
			      (hd=translate.ag.to object))
			     (t (let ((function (term-function.create (dt-function.pname object)
								      (dt-function.arity object)
						;(dt-function.sort object) How translate sorts?
								      )))
				  (do-protect function)
				  function))))
		      ((dt-constant.is object)
		       (let ((constant (term-constant.create (dt-constant.pname object) :unbound)))
			 (do-protect constant)
			 constant))
		      ((dt-variable.is object)
		       (let ((variable (term-variable.create (dt-variable.pname object))))
			 (do-protect variable)
			 variable))
		      (t (term-create (mapcar #'hd=translate.objects.to object))))))
	  (unless (consp object)
	    (dt-putprop object 'hd*object hd.object)
	    (do-put hd.object 'hd*object object))
 	  hd.object))))

(defun hd=translate.objects.from (object)
  (if (listp object)
      (mapcan #'hd=translate.objects.from object)
      (or (do-get object 'hd*object)
	  (let ((mkrp.object
		  (cond ((term-function.p object) (error "Functions cannot occur here")) ; All functions and constants must  
			((term-constant.p object) (error "Constants cannot occur here")) ; have a property HD*OBJECT.
			((term-variable.p object)
			 (do-protect object)
			 (dt-variable.create 'any))
			((subst-p object)
			 (list (zip (mapcar #'hd=translate.objects.from (subst-domain object))
				    (mapcar #'hd=translate.objects.from (subst-codomain object)))))
			((term-p object)
			 (let ((fun (hd=translate.objects.from (term-topsymbol object)))
			       (args (mapcar #'hd=translate.objects.from (term-arguments object))))
			   (if (or (dt-function.is.marked ac fun)
				   (dt-function.is.marked ac1 fun)
				   (dt-function.is.marked associative fun))
			       (labels ((rec (lis)
					  (list fun
						(first lis)
						(if (rest (rest lis))
						    (rec (rest lis))
						    (second lis)))))
				 (rec args))
			       (cons fun args))))
			(t (mapcan #'hd=translate.objects.from (do-send object :substitutions))))))
	    (unless (listp mkrp.object)
	      (do-put object 'hd*object mkrp.object)
	      (dt-putprop mkrp.object 'hd*object object))
	    mkrp.object))))

(defun hd-unify.terms (t1 t2 c.vars)
  (do-new.environment
    (mapc #'(lambda (var)
	      (dt-putprop var 'hd*stack (dt-getprop var 'hd*object))
	      (dt-putprop var 'hd*object (term-constant.create (dt-variable.pname var) :unbound))
	      (do-put (dt-getprop var 'hd*object) 'hd*object var))
	  c.vars)
    (prog1 (hd=translate.objects.from (hd=unify (hd=translate.objects.to t1)
						(hd=translate.objects.to t2)))
	   (do-protect nil)
	   (mapc #'(lambda (var)
		     (do-put (dt-getprop var 'hd*object) 'hd*object nil)
		     (dt-putprop var 'hd*object (dt-getprop var 'hd*stack))
		     (dt-putprop var 'hd*stack nil))
		 c.vars))))

(defun hd-unify.termlists (t1 t2 c.vars)
  (do-new.environment
    (mapc #'(lambda (var)
	      (dt-putprop var 'hd*stack (dt-getprop var 'hd*object))
	      (dt-putprop var 'hd*object (term-constant.create (dt-variable.pname var) :unbound))
	      (do-put (dt-getprop var 'hd*object) 'hd*object var))
	  c.vars)
    (prog1 (hd=translate.objects.from (hd=unify (mapcar #'hd=translate.objects.to t1)
						(mapcar #'hd=translate.objects.to t2)))
	   (do-protect nil)
	   (mapc #'(lambda (var)
		     (do-put (dt-getprop var 'hd*object) 'hd*object nil)
		     (dt-putprop var 'hd*object (dt-getprop var 'hd*stack))
		     (dt-putprop var 'hd*stack nil))
		 c.vars))))


(defun hd=unify (t1 t2)
  (let ((up (uni-create.unification.problem t1 t2)))
    (if up
	(if (UNI-UNIFICATION.CLASS.UNIFICATION-TYPE (UNI-TERMLISTS.UNIFICATION.CLASS (list t1 t2)))
	    (uni-substitutions up)
	    (make-list 5 :initial-element (DO-SEND Up :NEXT-SOLUTION)))
	nil)))