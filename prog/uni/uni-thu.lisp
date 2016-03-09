;;; -*- package: mkrp; syntax: common-lisp; mode: lisp -*-

(in-package "MKRP")

(defun thu=type (termlist type)
  (labels ((thu=type1 (term type)
	     (if (dt-term_c.term.is term)
		 (if (DT-FUNCTION.IS.MARKED ac (dt-term_topsymbol term))
		     (if (member type '(ac nil))
			 'ac
			 t)
		     (if (DT-FUNCTION.IS.MARKED ac1 (dt-term_topsymbol term))
			 (if (member type '(ac1 nil))
			     'ac1
			     t)
			 (thu=type (dt-term_arguments term) type)))
		 type #|(if (dt-constant.is term)
		     (if (DT-constant.IS.MARKED ac1 term)
			 (if (member type '(ac1 nil))
			     'ac1
			     t)
			 type)
		     type)|#)))
    (if termlist
	(thu=type (rest termlist) (thu=type1 (first termlist) type))
	type)))

(defun thu-terms (term1 term2 c.vars)
  (if (and c.vars (eql (thu=type (list term1 term2) nil) 'ac))
      (denz-match term1 term2)
      (hd-unify.terms term1 term2 c.vars)))

(defun thu-termlists (terml1 terml2 c.vars)
  (if (and c.vars (eql (thu=type (append terml1 terml2) nil) 'ac))
      (denz-match.list terml1 terml2)
      (hd-unify.termlists terml1 terml2 c.vars)))