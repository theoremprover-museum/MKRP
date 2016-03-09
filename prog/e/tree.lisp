;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-


(IN-PACKAGE "MKRP" :USE '("CL"))



;;; Indexing trees

(defstruct (etree (:constructor etree-create)
		  (:conc-name etree=)
		  (:print-function (lambda (struc stream level)
				     (declare (ignore level))
				     (prin1 (list (eds-names (etree=symbol struc))
						  (etree=store struc)
						  (etree=subtrees struc))
					    stream))) 
		  (:copier nil)
		  (:predicate nil))
  symbol supertree subtrees store actual)

(defun etree-symbol (etree) (etree=symbol etree))

(defun etree-supertree (etree) (etree=supertree etree))

(defun etree-subtrees (etree) (if etree (etree=subtrees etree) nil))

(defun etree-store (etree) (etree=store etree))

(defun etree-actual (etree) (first (etree=actual etree)))

(defun etree-actual.push (actual etree) (push actual (etree=actual etree)))

(defun etree-actual.pop (etree) (pop (etree=actual etree)))

(defun etree-insert.rule (rule c.tree &optional term)
  (push (cons rule term) (etree=store c.tree))	; Pointer from tree to leaf with rule
  (eds-rule_set.pointers rule (cons c.tree (eds-rule_pointers rule))))	; Pointer from rule to leaf of tree


(defun etree-select.subtree (selector tree)
						; Edited:  22-JUN-1990 00:38
						; Authors: PRCKLN
						; Input:   SELECTOR is a variable, constant or function.
						;          TREE is a compilation tree.
						; Effect:  If SELECTOR is not the symbol of one of the subtrees
						;          a subtree with SELECTOR as symbol is inserted into
						;          the subtree list of TREE.
						; Value:   The subtree of TREE with SELECTOR as symbol.
  (do* ((subtrees (or (etree=subtrees tree)
		      (setf (etree=subtrees tree)
			    (list (etree-create :symbol selector :supertree tree))))
		  (rest subtrees))
	(result))
       (result result)
    (setq result (cond ((eds-term_equal selector (etree=symbol (first subtrees)))
			(first subtrees))
		       ((null (rest subtrees))
			(setf (rest subtrees)
			      (list (etree-create :symbol selector :supertree tree)))
			nil)))))

(defun etree-insert (c.tree termlist store &optional term)
						; Edited:  22-JUN-1990 00:27
						; Authors: PRCKLN
						; Input:   A rw rule compilation tree, a termlist, and
						;          an RED=RW_RULE object.
						; Effect:  Inserts TERMLIST as a part of the left hand
						;          side of RULE into C.TREE, which is a subtree
						;          of the compilation tree for RULE.
						; Value:   Undefined
  (if termlist
      (etree-insert (etree-select.subtree (if (eds=term_p (first termlist))
					      (eds-term_topsymbol (first termlist))
					      (first termlist))
					  c.tree)
		    (append (eds-term_arguments (first termlist))
			    (rest termlist))
		    store term)
      (etree-insert.rule store c.tree term)))


(defun etree-remove (rule)
  (mapc #'(lambda (tree)
	    (when tree
	      (setf (etree=store tree) (delete rule (etree=store tree) :key #'first))
	      (eds-rule_set.pointers rule nil)
	      (etree-remove.tree tree)))
	(eds-rule_pointers rule)))

(defun etree-remove.tree (tree)
  (unless (etree=subtrees tree)
    (when (etree=supertree tree)
      (setf (etree=subtrees (etree=supertree tree))
	    (delete tree (etree=subtrees (etree=supertree tree))))
      (etree-remove.tree (etree=supertree tree)))))

