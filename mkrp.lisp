;;; -*- Package: OMEGA; Base: 10; Mode: LISP; Syntax: Common-lisp -*-

(in-package :omega)

(mod~defmod mkrp :uses (mod sys nd p2f f2p res cnf delta tp opt inter
			    com node just arg omega keim prob post trans
			    proof rule lit term top cl type env meta subst)
	    :documentation "This module contains all functionality for calling
the MKRP theorem prover from OMEGA-MKRP."
	    :exports (mkrp~call
		      mkrp~generic-options
		      mkrp+mkrp
		      mkrp~transform
		      mkrp~insert-subproof!
		      mkrp~line-p
		      mkrp~print
		      mkrp~write-options
		      mkrp~functions
		      mkrp~predicates
		      mkrp~equation-p))


; don't import these, use the fully qualified name, so we know where they come
; from
; The argument for this form is, that you see better, which functions are used
#+old(import '(mkrp::any mkrp::mem-initialize mkrp::mkrp::prep-reset mkrp::opt-get.option
		    mkrp::DS*SIGN.PLUS.SYMBOLS mkrp::DS*SIGN.MINUS.SYMBOLS
		    MKRP::DT-FUNCTION.PUT.ATTRIBUTES MKRP::DT-FUNCTION.CREATE
		    MKRP::DT-CONSTANT.CREATE MKRP::DT-PUTPROP MARKGRAF-KARL::MKRP=RESET
		    MKRP::DS-CLAUSE.CREATE MKRP::DT-VARIABLE.CREATE
		    MKRP::DT-SORT.ST.PUT.DIRECT.SUPERSORTS mkrp::DT-PREDICATE.EQUALITIES
		    MKRP::DT-SORT.CREATE MKRP::DT-PREDICATE.ADD.ATTRIBUTES
		    MKRP::DT-PREDICATE.CREATE mkrp::dt-sort.all
		    MKRP::DS-LIT.CREATE)
	(find-package "OMEGA"))

(defparameter mkrp*dir "/tmp"
  "Temporary directory to store MKRP information.")

(defun mkrp~call (ho-problem)
  (declare (edited  "25-Jul-1993")
	   (authors Richts PRCKLN)
	   (input   "A problem.")
	   (effect  "Calls the MKRP-theorem-prover for HO-PROBLEM by translating it"
		    "to first order using P2F~TRANSLATE, normalizing ist by"
		    "CNF~NORMALIZE-PROBLEM, executing the proof with a special form"
		    "of MKRP::MKRP, and merging the result with the components of"
		    "HO-PROBLEM.")
	   (value   "A Resolution proof found by the MKRP theorem prover."))
  (let* ((problem-name (keim~name ho-problem))
	 (fo-problem (progn(mkrp=output-log "Translating the problem ~A to first-order logic ... " problem-name)
			   (let ((fo-problem (p2f~translate ho-problem)))
			     #+debug(with-open-file (out (make-pathname :name  "fo-prob" :type "post") :direction :output
							 :if-does-not-exist :create :if-exists :supersede)
						    (post~pprint fo-problem out))
			     (mkrp=output-log "Done~%")
			     fo-problem)))
	 (norm-problem (prog2 (mkrp=output-log "Normalizing ~A ... " problem-name)
			   (cnf~normalize-problem fo-problem) ; :delta nil)
			 (mkrp=output-log "Done~%")))
	 (mkrp-proof-file (mkrp=exec norm-problem problem-name))
	 (env-with-O (env~create))
	 (irrelevant (progn (post~read-object '(O) env-with-O :type-constants)
			    (post~read-object '(AA) env-with-O :type-variables)
			    (post~read-object '((= (O AA AA)) (true O)) env-with-O :constants)))
					;this pre-loads the type-constant 'O and the equality constant into the environment,
					;since it is not present in the MKRP output
	 #+old(mkrp-proof (with-open-file (stream mkrp-proof-file) ;mkrp-corrected-proof-file
				     (prob~read (read stream) env-with-O)))
	 (mkrp-proof (with-open-file (stream
				      (make-pathname :directory "/tmp" :name  "code" :type "lisp")
				      )
				     (prob~read (read stream) env-with-O)))
	 (res-proof (progn (format t "~% MKRP:")
			   (post~pprint mkrp-proof t)
			   (format t "~% Norm:")
			   (post~print norm-problem t)
			   (mkrp=update-resolution-proof! (mkrp=merge-problems! mkrp-proof norm-problem)))))
    (with-open-file (out (make-pathname :directory "/tmp" :name  "code" :type "post") :direction :output
			 :if-does-not-exist :create :if-exists :supersede)
		    (post~pprint res-proof out))
    (format t "~% Final:")
    (post~pprint res-proof t)
    res-proof))

(defmethod mkrp=update-resolution-proof! ((proof res+proof))
  (let ((proof-steps (proof~steps proof)))
    (proof~set-steps! proof (mapcar #'mkrp=update-proof-step! proof-steps))
    proof))

(defun mkrp=update-proof-step! (step)
  (let ((justification (node~justification step))
	(new-label (gentemp "step-")))
    (cond ((res~resolution-p justification)
	   (let* ((parent-list (res~resolution-clauses step))
		  (position-list (res~resolution-positions step))
		  (renaming (res~just-renaming step))
		  (unifier (res~just-unifier step))
		  (real-unifier (uni~unify (lit~atom (term~at-position (first parent-list) (first position-list)))
					   (lit~atom (term~at-position (second parent-list) (second position-list)))))
		  (free-vars-parents (mapcan #'top~free-variables (res~resolution-clauses justification)))
		  (new-renaming (subst~restrict-substitution renaming (subst~domain unifier)))
		  (new-unifier (subst~restrict-substitution unifier free-vars-parents))
		  (new-justification (res~resolution-create
				      parent-list
				      position-list
				      new-unifier
				      new-renaming
				      (format nil "~A" (keim~name justification)))))
	     (node~set-justification! step new-justification)
	     step))
	  (t step))))

;(progn  (proof~set-steps! norm-problem (proof~steps mkrp-proof))
;                            (prob~set-status! norm-problem (prob~status mkrp-proof))
;                            norm-problem))) 

(defun mkrp~generic-options ()
  (declare (edited  "25-MAY-1993 11:23" )
	   (authors KOHLHASE PRCKLN)
	   (input  "None.")
	   (effect "None.")
	   (value  "A list of MKRP-options with an omega convenient value.")
	   (remark "It is forbidden to change the value of PR_PROTOCOL for"
		   "the post version of MKRP."))
  '((RED.I_CLAUSE.MULTIPLE.LITERALS)
    (RED.I_CLAUSE.PURITY . PARTIAL)
    (RED.I_CLAUSE.TAUTOLOGY . T)
    (RED.I_CLAUSE.SUBSUMPTION . T)
    (RED.I_CLAUSE.REPL.FACTORING . t)
    (RED.I_CLAUSE.REPL.RESOLUTION . simple)
    (RED.I_CLAUSE.REWRITING . DEM)
    (RED.I_LINK.INCOMPATIBILITY . T)
    (RED.I_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.I_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (RED.D_CLAUSE.MULTIPLE.LITERALS)
    (RED.D_CLAUSE.PURITY . PARTIAL)
    (RED.D_CLAUSE.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.FORWARD . REMOVE-INHIBIT)
    (RED.D_CLAUSE.SUBSUMPTION.BACKWARD . REMOVE)
    (RED.D_CLAUSE.REPL.FACTORING . t)
    (RED.D_CLAUSE.REPL.RESOLUTION . simple)
    (RED.D_CLAUSE.REWRITING . DEM)
    (RED.D_LINK.INCOMPATIBILITY . T)
    (RED.D_LINK.TAUTOLOGY . REMOVE-INHIBIT)
    (RED.D_LINK.SUBSUMPTION . REMOVE-INHIBIT)
    (FAC_INITIAL)
    (FAC_EACH.STEP)
    (STR_RESOLUTION . SET-OF-SUPPORT)
    (STR_R.SELECTION * 10 (+ 2 VARIABLES (* 2 DEPTH) (* 3 NOLIT)))
    (STR_LINK.DEPTH)
    (STR_TERM.DEPTH)
    (STR_FINITE.DOMAIN . T)
    (TERM_UNITS . T)
    (TERM_ITERATIONS . 5)
    (TERM_SET.OF.SUPPORT)
    (TERM_BREADTH.FIRST) 
    (ER_PARAMODULATION . HEURISTIC-COMPLETION)
    (ER_WEIGHT.POLYNOMIALS)
    (ER_P.SELECTION * WEIGHT (IF SUPPORT 1 1.5) (IF EQUATIONAL 1 2))
    (ER_COMPLETION . UNFAILING)
    (ER_CP.REDUCTION . T)
    (ER_ORDERING . LEXICOGRAPHIC-RECURSIVE-PATH)
    (ER_OPERATOR.ORDERING * - + 0 1)
    (ER_KNUTH.BENDIX.WEIGHT (+ 1) (* 1) (- 0) (0 1) (1 1))
    (ER_POLYNOMIAL.WEIGHT (+ (X Y) (+ (* 2 Y) X)) (* (X Y) (+ (* X Y) X)) (- (X) (* X X)) (0 NIL 2) (1 NIL 2))
    (GEN_MANUAL.CONTROL)
    (GEN_MAXIMUM.STEPS)
    (GEN_MAXIMUM.TIME)
    (TR_STEP.MODE . LR)
    (TR_DUMP)
    (TR_CLAUSE.MODE . I)
    (TR_LINK.MODE . I)
    (TR_TRACE.FILE)
    (TR_TERMINAL . T)
    (PR_PROTOCOL . POST) ; Do not change
    ))

;; New conversion


(defun mkrp=exec (norm-problem problem-name)
  (declare (edited  "25-Jul-1993")
	   (authors RICHTS PRCKLN)
	   (input   "NORM-PROBLEM is an OMEGA problem specification, containing"
		    "only normalized clauses."
		    "PROBLEM-NAME is the name of the NORM-PROBLEM")
	   (effect  "Executes CONSTRUCT.REFUTE of MKRP for NORM-PROBLEM")
	   (value   "The name of the Post code file to be merged with the original"
		    "problem."))
  (let ((tmp-file (make-pathname :directory mkrp*dir :name "code" :type "lisp")))
    (mkrp=output-log "Starting the MKRP theorem prover on the problem ~A~%" problem-name)
    (mkrp::prep-reset)
    (mkrp=reset)
    (mkrp=exec-declarations norm-problem)
    ;; MKRP::PREP-RESET must be called before exec declarations
    (mkrp::mkrp (mkrp=obj-ax-clauses norm-problem)
		(mkrp=obj-th-clauses norm-problem)
		tmp-file
		problem-name	      
		(mkrp~generic-options))
    (mkrp=output-log "Done proving ~A~%" problem-name)
    tmp-file))

(defun mkrp=enter-sort (NAME RANGE)
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "A sortname (string or symbol) NAME and a list of sortnames"
		    "RANGE which are the direct supersorts of NAME.")
	   (effect  "Inserts the given sorts in the MKRP sort hierarchy.")
	   (value   "Undefined."))
  (when (stringp name)
	(setq name (intern name)))
  (unless (member name (mkrp::dt-sort.all))
	  (MKRP::DT-SORT.CREATE NAME NIL T)
	  (MKRP::DT-SORT.ST.PUT.DIRECT.SUPERSORTS NAME RANGE)))

(defun mkrp=enter-pred (NAME DOMAIN ATTRIBUTE)
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "A predicate (string or symbol) NAME and a list of sortnames"
		    "DOMAIN which are the sorts of the arguments of the predicate.")
	   (effect  "Creates a corresponding MKRP-predicate object, inserts the object and the"
		    "sorts in the MKRP database.")
	   (value   "The created object."))
  (let ((pred (MKRP::DT-PREDICATE.CREATE NAME DOMAIN)))
    (MKRP::DT-PREDICATE.ADD.ATTRIBUTES pred ATTRIBUTE)
    (mapcar #'(lambda (name) (mkrp=enter-sort name nil)) DOMAIN)
    pred))

(defun mkrp=enter-fun (NAME RANGE DOMAIN ATTRIBUTE)
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "A function (string or symbol) NAME, a sort name RANGE, which"
		    "is the result sort of the function, and a list of sortnames"
		    "DOMAIN which are the sorts of the arguments of the function.")
	   (effect  "Creates a corresponding MKRP-function object, inserts the object and the"
		    "sorts in the MKRP database.")
	   (value   "The created object."))
  (let ((FUN (MKRP::DT-FUNCTION.CREATE NAME RANGE DOMAIN)))
    (MKRP::DT-FUNCTION.PUT.ATTRIBUTES FUN ATTRIBUTE)
    (mapcar #'(lambda (name) (mkrp=enter-sort name nil)) DOMAIN)
    (mkrp=enter-sort RANGE nil)
    fun))

(defun mkrp=enter-const (NAME RANGE ATTRIBUTE)
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "A constant (string or symbol) NAME and a sort name RANGE, which"
		    "is the sort of the constant.")
	   (effect  "Creates a corresponding MKRP-contant object, inserts the object and the"
		    "sort in the MKRP database.")
	   (value   "The created object.")
	   (ignore ATTRIBUTE))
  (mkrp=enter-sort RANGE nil)
  (MKRP::DT-CONSTANT.CREATE NAME RANGE))

(defun mkrp=reset ()
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "-")
	   (effect  "Creates the MKRP-default objects.")
	   (value   "Undefined."))
  (mkrp=enter-sort 'mkrp::any nil)
  (mkrp=enter-pred 'false NIL (CONS 'DEFINED nil))
  (mkrp=enter-pred 'true NIL (CONS 'DEFINED nil))
  (mkrp=enter-pred '= '(mkrp::any mkrp::any) (LIST 'DEFINED 'SYMMETRIC 'REFLEXIVE))
  (when (mkrp::opt-get.option sort_literals)
	(mkrp=enter-const 'omega 'mkrp::any (CONS 'DEFINED nil))
	(mkrp=enter-pred 'e '(mkrp::any mkrp::any) (CONS 'DEFINED nil))))

(defun mkrp=get (object indicator &optional default)
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "A Keim-object, a symbol, and a default object.")
	   (effect  "Creates the MKRP-default objects.")
	   (value   "The MKRP-object of OBJECT stored under the Keim-property INDICATOR."
		    "DEFAULT if nothing is stored."))
  (or (keim~get object indicator) default))

(defun mkrp=put (object indicator value)
  (declare (edited  "25-Jul-1993")
	   (authors PRCKLN)
	   (input   "A Keim-object, a symbol, and an MKRP-object to be stored.")
	   (effect  "Stores VALUE under the indicator INDICATOR in the property"
		    "list of OBJECT.")
	   (value   "Undefined."))
  (keim~put object indicator value))

(defun mkrp=translate-objects-from-keim (object)
  (declare (edited  "22-JUL-1993 12:14")
	   (authors PRCKLN)
	   (input   "A term or list of term objects.")
	   (effect  "Puts the corresponding MKRP-object in the plist of object"
		    "under the indicator MKRP*KEIM-OBJECT for OBJECT and all"
		    "subobjects of OBJECT.")
	   (value   "A corresponding MKRP-term or list of MKRP-terms"
		    "with the MKRP-function and constants in it."))
  (if (listp object)
      (mapcar #'mkrp=translate-objects-from-keim object)
    (or (mkrp=get object 'mkrp*keim-object)
	(let ((mkrp-object (mkrp=term-from-keim object)))
	  (unless (listp mkrp-object)
		  (mkrp=put object 'mkrp*keim-object mkrp-object))
	  mkrp-object))))

(defgeneric mkrp=term-from-keim (object)
  (declare (edited  "21-APR-1993 09:14")
	   (authors PRCKLN)
	   (input   "A term object.")
	   (effect  "Puts the MKRP-objects for all subobjects in the plist of object"
		    "under the indicator MKRP*KEIM-OBJECT.")
	   (value   "A corresponding MKRP-term with the MKRP-function and constants in it."))
  (:method ((literal lit+literal))
	   (let ((atom (mkrp=translate-objects-from-keim (lit~atom literal))))
	     (unless (listp atom) (setq atom (list atom)))
	     (MKRP::DS-LIT.CREATE (first (if (lit~positive-p literal)
				       MKRP::DS*SIGN.PLUS.SYMBOLS 
				     MKRP::DS*SIGN.MINUS.SYMBOLS))
			    (first atom) (rest atom))))
  (:method ((var sym+var)) (mkrp::dt-variable.create (keim~name (type~n-range (term~type var)))))
  (:method ((const sym+const)) (error "Constants and functions cannot occur here: ~A" const))
  (:method ((term appl+appl))
	   (let ((fun (mkrp=translate-objects-from-keim (appl~function term)))
		 (args (mapcar #'mkrp=translate-objects-from-keim
			       (appl~arguments term))))
	     (cons fun args))))

(defun mkrp=obj-clause (clause)
  (declare (edited  "21-JUL-1993 11:59" )
	   (authors PRCKLN)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "An MKRP representation of the CLAUSE." ))
  (mkrp=translate-objects-from-keim (cl~literals clause)))

(defun mkrp=obj-clauses (clauses)
  (declare (edited  "21-JUL-1993 11:59" )
	   (authors PRCKLN)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "A list of MKRP representations of the CLAUSES." ))
  (mapcar #'mkrp=obj-clause clauses))

(defun mkrp=functions (clauses)
  (declare (edited  "21-APR-1993 12:05")
	   (authors KOHLHASE)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "The set of function constants in CLAUSES."))
  (let* ((positions (term~positions clauses
				    #'(lambda (term)
					(and (sym~constant-p term)
					     (not (type~o-p (type~n-range (term~type term))))))))
	 (functions (mapcar #'(lambda (position) (term~at-position clauses position))
			    positions)))
    (remove-duplicates functions :test #'term~equal)))

(defun mkrp=predicates (clauses)
  (declare (edited  "21-APR-1993 12:05" )
	   (authors KOHLHASE )
	   (input  "A list of clauses.")
	   (effect "None.")
	   (value  "The set of predicate constants in CLAUSES."))
  (let* ((positions (term~positions clauses #'(lambda (term)
						(and (sym~constant-p term)
						     (type~o-p (type~n-range (term~type term)))))))
	 (predicates (mapcar #'(lambda (position) (term~at-position clauses position))
			     positions)))
    (remove-duplicates predicates :test #'term~equal)))


(defun mkrp=exec-declarations (problem)
  (declare (edited  "21-JUL-1993 12:03")
	   (authors PRCKLN)
	   (input   "A proof problem")
	   (effect  "The MKRP-constant-declarations are executed for all constants"
		    "as if they have been written on the axiom file.")
	   (value  "Undefined."))
  (let* ((clauses (res~proof-clauses  problem))
	 (functions (mkrp=functions clauses))
	 (predicates (mkrp=predicates clauses)))
    (mapc #'(lambda (function)
	      (let* ((type (term~type function))
		     (dom (mapcar #'keim~name (type~n-domain type)))
		     (fun (if dom
			      (mkrp=enter-fun (keim~name function)
					      (keim~name (type~n-range type)) dom nil)
			    (mkrp=enter-const (keim~name function)
					      (keim~name (type~n-range type)) nil))))
		(mkrp=put function 'mkrp*keim-object fun)))
	  functions)
    (mapc #'(lambda (predicate)
	      (let* ((type (term~type predicate))
		     (pred (if (string-equal "=" (keim~name predicate))
			       (first (MKRP::DT-PREDICATE.EQUALITIES))
			     (mkrp=enter-pred (keim~name predicate)
					      (mapcar #'keim~name (type~n-domain type)) nil))))
		(mkrp=put predicate 'mkrp*keim-object pred)))
	  predicates)))

(defun mkrp=obj-th-clauses (problem)
  (declare (edited  "21-JUL-1993 12:05")
	   (authors KOHLHASE PRCKLN)
	   (input   "A proof problem.")
	   (effect  "None.")
	   (value   "The list of MKRP clauses for the theorem clauses in the"
		    "normalized problem PROBLEM."))
  (let ((th-clauses  (remove-if-not  #'cnf~conclusion-clause-p (res~proof-clauses  problem))))
    (mkrp=obj-clauses th-clauses)))


(defun mkrp=obj-ax-clauses (problem)
  (declare (edited  "21-JUL-1993 12:05")
	   (authors KOHLHASE PRCKLN)
	   (input   "A proof problem.")
	   (effect  "None.")
	   (value   "The list of MKRP clauses for the axiom clauses in the"
		    "normalized problem PROBLEM."))
  (let ((ax-clauses  (remove-if-not  #'cnf~axiom-clause-p (res~proof-clauses  problem))))
    (mkrp=obj-clauses ax-clauses)))

(eval-when (load compile eval)
  (defclass mkrp+mkrp (tp+prover)
    ()
    (:documentation "The MKRP theorem prover."))
);end eval-when

(defvar mkrp*prover (tp~make-prover 'MKRP (mkrp~generic-options)))


(defun mkrp=output-log (format-string &rest objects)
  (declare (edited  "27-MAY-1993 14:49")
	   (authors ACSEHN)
	   (input "A format String and corresponding objects.")
	   (effect "The format directive is printed to the current interface." )
	   (value  "Undefined."))
  (inter~output-object omega*current-interface
		       (apply #'format nil format-string objects)))


(defun mkrp~transform (res-proof ho-problem)
  (declare (edited  "25-Mar-1993")
	   (authors Richts)
	   (input   "A Higher-order problem and the resolution proof"
		    "to the p2f-translated problem.")
	   (effect  "None.")
	   (value   "A Natural Deduction proof of HO-PROBLEM generated from RES-PROOF."))
  (let* ((fo-nd-proof (trans~transform-problem-2-nd-proof res-proof))
	 (ho-nd-problem (change-class ho-problem 'nd+proof))
	 (ho-nd-proof (f2p~translate-nd-proof fo-nd-proof ho-nd-problem)))
    (mkrp=output-log "~%~%Generated higher-order ND proof from resolution proof.~%")
    ho-nd-proof))

(defun mkrp~insert-subproof! (planline subproof)
  (declare (edited  "11-JUN-1993 22:23" )
	   (authors KOHLHASE )
	   (input  "A ND Proof with a planline, and a ND proof that is to be inserted into PROOF at PLANLINE.")
	   (effect "The PLANLINE in PROOF is exchanged by SUBPROOF.")
	   (value  "The augmented PROOF."))
  (let* ((supports (node~just-nodes planline))
	 (new-line NIL)
	 (last-line (do ((theorem-line-of-subproof (first (last (proof~steps subproof))) new-line)
			 (support-line (first supports) (rest supports)))
			((not (rest support-line)) new-line)
		      (if (rule~match-p 'mp-and-forw (list theorem-line-of-subproof support-line))
			  (multiple-value-bind (applied-p new-lines) (rule~apply 'mp-and-forw
										 (list theorem-line-of-subproof support-line))
			    (if applied-p
				(setq new-line (first new-lines))
				(post~error "The rule `mp-and-forw' is not applicable to lines ~A ~A" theorem-line-of-subproof support-line))))))
	 (same-line (if (rule~match-p 'implies-e (list last-line (first (last supports))))
			(multiple-value-bind (applied-p new-lines) (rule~apply 'implies-e (list last-line (first (last supports))))
			  (if applied-p
			      (first new-lines)
			      (post~error "The rule `implies-e' is not applicable to lines ~A ~A" last-line (first (last supports))))))))
    (if (rule~match-p 'same (list same-line planline))
	(multiple-value-bind (applied-p new-lines) (rule~apply 'same (list same-line planline))
	  (if applied-p
	      (first new-lines)
	      (post~error "The rule `same' is not applicable to lines ~A ~A" same-line planline))))))

(defgeneric mkrp~line-p (thing)
  (declare (edited  "20-APR-1993 11:18" )
	   (authors KOHLHASE )
	   (input   "A KEIM object.")
	   (effect  "None.")
	   (value   "T, iff THING is ND-line that is justified by a MKRP resolution proof."))
  (:method ((thing keim+object))
	   nil)
  (:method ((line nd+line))
	   (typep (node~justification line) 'mkrp+mkrp)))

;;; The NDMKRPLINE argtype is the class of natural deduction lines which are
;;; lines that are justified by a resolution proof supplied by the MKRP theoreom prover.

(eval-when (load compile eval)
(arg~deftype ndmkrpline
 (predicate mkrp~line-p)
 (read-function mkrp~read-ndmkrpline)
 (help "a MKRP line"))
)

(defun mkrp=read-ndmkrpline-sym-or-string (obj)
  (if (nd~proof-p omega*current-ndproof)
      (let ((line (sys~handler-case
		   (mkrp=read-ndmkrpline-sym-or-string obj)
		   (arg+wrong-type-error (c)
					 (setf (arg~input-error-expected c)
					       (arg~find-argtype 'ndmkrpline))
					 (sys~signal c)))))
	(if (mkrp~line-p line)
	    line
	  (arg~signal-wrong-type 'ndmkrpline obj)))
    (progn 
      (inter~print-error omega*current-interface 
			 "There is no current proof.")
      (arg~signal-wrong-type 'ndmkrpline obj))))

(defmethod mkrp~read-ndmkrpline ((obj string) &rest others)
  (declare (ignore others))
  (mkrp=read-ndmkrpline-sym-or-string obj))

(defmethod mkrp~read-ndmkrpline ((obj symbol) &rest others)
  (declare (ignore others))
  (mkrp=read-ndmkrpline-sym-or-string obj))

(defmethod mkrp~read-ndmkrpline ((obj nd+line) &rest others)
  (declare (ignore others))
  (if (mkrp~line-p obj)
      obj
      (arg~signal-wrong-type 'ndmkrpline obj)))

;;; Define the command CALL-MKRP

(com~defcommand call-mkrp
		(argnames line)
		(argtypes ndplanline)
		(arghelps "Line to prove with MKRP")
		(categories proof-commands)
		(function mkrp=call-mkrp)
		(defaults ((oc~default-current-planline)))
		(help "Call MKRP and try to prove the line using its supports. 
If successful, justify the line with the rule MKRP."))

(defun mkrp=call-mkrp (line)
  (let* ((proof (mkrp~call 
		 (nd~make-problem-from-line omega*current-ndproof line)))
	 (just (tp~make-justification proof 
				      (nd~line-supports line) mkrp*prover)))
    (when (typep just 'just+justification)
      (node~set-justification! line just)
      (nd~set-proof-planned-lines! omega*current-ndproof 
				   (delete line (nd~proof-planned-lines
						 omega*current-ndproof)))
      )))

(com~defcommand transform
  (argnames line)
  (argtypes ndmkrpline)
  (arghelps "The line where the justifying resolution proof is to be transformed into natural deduction format")
  (categories utilities)
  (function mkrp=transform)
  (help "Transforms the proof corresponding of a MKRP-LINE to ND and replaces LINE with the resulting subproof."))

(defun mkrp=transform (line)
  (let* ((res-proof (mkrp~res-proof (node~justification line)))
	 (ho-problem (nd~make-problem-from-line omega*current-ndproof line))
	 (ho-nd-proof (mkrp~transform res-proof ho-problem)))
    (mkrp~insert-subproof! line ho-nd-proof)))




; ----------------------- Merging the proofs ------------------------------


(defun mkrp=merge-problems! (mkrp-problem cnf-problem)
  (declare (edited  "03-JUN-1993 19:16")
	   (authors ACSEHN)
	   (input "Two problems: a MKRP - PROBLEM, a POST - representation of a resolution proof done by MKRP,"
		  "and a CNF - PROBLEM, i.e. a POST - protocol of a problem in clausal normal form.")
	   (effect "The following is destructively performed:"
		   "1. In the delta relation of the CNF - PROBLEM the clauses are replaced by the clauses"
		   "   of the MKRP - PROBLEM."
		   "2. The clauses of the CNF - PROBLEM bocome the clauses of the  MKRP - PROBLEM."
		   "3. The resolutions steps of  MKRP - PROBLEM are inherited to the CNF - PROBLEM."
		   "4. The status of the CNF - PROBLEM becomes the status of the reolution proof.")
	   (value "the altered  CNF - PROBLEM."  ))
  (let* ((matched-clauses (mkrp=match-clause-list (res~proof-clauses mkrp-problem)
						  (res~proof-clauses cnf-problem)))
	 (new-delta (mkrp=replace-matched-objects-in-delta! (res~proof-delta-relation cnf-problem) matched-clauses)))
					;(format T "~%matched-assumptions: ~S" matched-assumptions)
					;(format T "~%matched-conclusion: ~S" matched-conclusion)
					;(format T "~%matched-clauses: ~S" matched-clauses)
    (res~set-proof-delta-relation! cnf-problem new-delta)
    (res~set-proof-clauses! cnf-problem (mkrp=replace-clauses (res~proof-clauses cnf-problem)
							      matched-clauses))
    (proof~set-steps! cnf-problem (proof~steps mkrp-problem))
    (prob~set-status! cnf-problem (prob~status mkrp-problem))
    cnf-problem))

(defun mkrp=replace-clauses (old-clause-list match-list)
  (declare (edited  "04-JUN-1993 11:31")
	   (authors ACSEHN)
	   (input "a list of old clauses, OLD-CLAUSE-LIST "
		  "and a match list consisting of pairs of the form: (old-clause . new-clause.)" )
	   (effect "none."  )
	   (value "the list of clauses where old-clauses are replaced by the new clauses in MATCH - LIST."  ))
;;;; Vorsicht  Bogon alert !
  (remove nil
  (mapcar #'(lambda (old-clause)
	      (car (rassoc old-clause match-list :test #'keim~equal)))
	  old-clause-list)))


(defun mkrp=replace-matched-objects-in-delta! (object match-list)
  (declare (edited  "03-JUN-1993 18:51")
	   (authors ACSEHN)
	   (input "an OBJECT,i.e. a delta relation, and a MATCH-LIST, consisting of pairs of the form"
		  "(new-object . old-object)."  )
	   (effect "in delta the old objects are replaced by the new ones." )
	   (value  "the altered OBJECT." ))
  (mapcar #'(lambda (match-pair)
	      (delta~replace! object (cdr match-pair) (car match-pair)))
	  match-list)
  object)



(defun mkrp=term-equal (term-1 term-2)
  (term~equal-p-ab (termix~term term-1) (termix~term term-2)))

(defun mkrp=match-term-list (term-list-1 term-list-2)
  (declare (edited  "03-JUN-1993 10:45")
	   (authors ACSEHN)
	   (input "two lists of terms, term-list-1 and term-list-2" )
	   (effect "none."  )
	   (value "a list of pairs matching the terms against each other being equal up to renaming."  ))
  (do* ((term-1 (car term-list-1) (car term-list-1))
	(term-list-1 (cdr term-list-1) (cdr term-list-1))
	(term-2 (find-if #'(lambda (x) (mkrp=term-equal term-1 x))
			   term-list-2)
		  (find-if #'(lambda (x) (mkrp=term-equal term-1 x))
			   term-list-2))
	(term-list-2 term-list-2 (remove term-2 term-list-2))
	(res (if term-2 (acons term-1 term-2 nil) nil)
	     (if term-2 (acons term-1 term-2 res) res)))
      ((null term-1) (nreverse res))))



(defun mkrp=clause-to-formula (clause)
  (let* ((filled-env (filled-env))
	 (false
	  (term~read 'false filled-env))
	 (not (term~read 'not filled-env))
	 (or (term~read 'or filled-env))
	 (vars (top~free-variables clause))
	 (lits (cl~literals clause))
	 (scope (mkrp=lits-to-formula lits false not or)))
    (dolist (var (reverse vars) scope)
      (setq scope
	    (abstr~create
	     var scope)))))

(defun filled-env ()
  (let* ((envi (env~create))
	 (env (progn (post~read-object '(O) envi :type-constants)
			    (post~read-object '(AA) envi :type-variables)
			    (post~read-object '((= (o aa aa))
						(false o)
						(true o)
						(and (o o o))
						(or (o o o))
						(implies (o o o))
						(equiv (o o o))
						(not (o o))
						(forall (o (o aa)))
						(exists (o (o aa))))  envi :constants))))
    envi))



(defun mkrp=lits-to-formula (lits false not or)
  (cond ((null lits)
	 false)
	((cdr lits)
	 (appl~create
	  or
	  (list
	   (if (lit~positive-p (car lits))
	       (lit~atom(car lits))
	     (appl~create not (list (lit~atom (car lits)))))
	   (mkrp=lits-to-formula (cdr lits) false not or))))
	(t (lit~atom (car lits)))))
	  

(defmethod  mkrp=clause-equal ((clause-1 cl+clause) (clause-2 cl+clause))
  (declare (edited  "03-JUN-1993 10:41")
	   (authors ACSEHN)
	   (input "Two clauses, clause-1 and clause-2."  )
	   (effect "none." )
	   (value "T, iff the two clauses are looking equal up to alpha-conversion."  ))
  (term~equal-p-ab (mkrp=clause-to-formula clause-1)
		   (mkrp=clause-to-formula clause-2))
  )

(defun mkrp=term-equal-up-to-free-variables (term1 term2)
  (declare (edited  "03-JUN-1993 10:43")
	   (authors ACSEHN)
	   (input "Two terms, term1 and term2."  )
	   (effect "none." )
	   (value "T, iff the two terms are looking equal up to alpha-conversion."  ))
  (handler-case
   (meta~match term1 term2 nil (top~free-variables term1))
   (meta+error () nil)
   (:no-error (res) (declare (ignore res)) t)))


(defun mkrp=match-clause-list (clause-list-1 clause-list-2)
  (declare (edited  "03-JUN-1993 10:45")
	   (authors ACSEHN)
	   (input "two lists of clauses, clause-list-1 and clause-list-2" )
	   (effect "none."  )
	   (value "a list of pairs matching the clauses against each other being equal up to renaming."  ))
  (do* ((clause-1 (car clause-list-1) (car clause-list-1))
	(clause-list-1 (cdr clause-list-1) (cdr clause-list-1))
	(clause-2 (if clause-1 (find-if #'(lambda (x) (mkrp=clause-equal clause-1 x))
			   clause-list-2))
		  (if clause-1 (find-if #'(lambda (x) (mkrp=clause-equal clause-1 x))
					clause-list-2)))
	(clause-list-2 clause-list-2 (remove clause-2 clause-list-2))
	(res (if clause-2 (acons clause-1 clause-2 nil) nil)
	     (if clause-2 (acons clause-1 clause-2 res) res)))
      ((null clause-1) (nreverse res))))

