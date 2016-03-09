(IN-PACKAGE "MKRP" :use '("CL"))

(setq LC_RIGHT.OP.LIST '("!"))

(setq LC_LEFT.OP.LIST '("-" "+"))

(setq LC_BINARY.OP.LIST '("=" "+" "-" "*" "/"))

(setq LC_KLAMMER.OP.LIST '("="))


(DEFUN LC_GET.INFIX.FORM (clauseslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of clauses in infix form:
	  ;;; (pds-get.axioms.infix)
	  ;;; (pds-get.theorems.infix)
	  ;;; (LC_GET.INFIX.FORM (pds-get.axioms.infix))
	  ;;; (LC_GET.INFIX.FORM (pds-get.theorems.infix))
	  ;;;output: structured form of these clauses
	  ;;;effect:preparing the set of clauses as input to the
	  ;;;formatting function.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((null clauseslist)
	      nil)
	     (T
	      (let ((first (car clauseslist))
		    (rest (cdr clauseslist)))
		   (cond ((atom first)
			  (cons first (LC_GET.INFIX.FORM rest)))
		         ((member 'AND first)
			  (cons (LC_GET.INFIX.FORM (cons 'bins first))
			        (LC_GET.INFIX.FORM rest)))
		         ((member 'IMPL first)
			  (cons (LC_GET.INFIX.FORM (cons 'bins first))
			        (LC_GET.INFIX.FORM rest)))
		         ((member 'ALL first)
			  (cons (LC_HANDLE.ALL first)
			        (LC_GET.INFIX.FORM rest)))
		       ;;;;Hier werden die Operatoren nach Prioritaet bearbeitet.
		       ;;;;Wenn es einen neuen Operator ergibt, wird dieser an der
		       ;;;;entsprechenden Stelle behandelt.
		         (T
		       	  (cons first
			        (LC_GET.INFIX.FORM rest))))))))

(DEFUN LC_HANDLE.ALL (clause)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a clause (list) beginning with ALL.
	  ;;; (ALL |X,Y,..| terms)
	  ;;;output:the same clause with the following form:
	  ;;; ((quant ALL |X,Y,..|) terms)
	  ;;;effect:handling the case of quantification.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (list 'kl 
             (cons (list 'quant 'all (cadr clause))
	           (LC_GET.INFIX.FORM (cddr clause)))))



(DEFUN LC_GET.AXIOM.CLAUSES (ax.addr.list)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input: a list of integers (axiom adresses),
	  ;;;(the output of (pds-get.axioms)).
	  ;;; (lc_get.axiom.clauses (pds-get.axioms))
          ;;;output:a list of all axioms (((pname)(quantification)
          ;;;                              (literals)) ...)
          ;;; pname e.g * A1
	  ;;; quantification e.g (ANY "X" "Y")..
	  ;;;effect:computing set of axiom clauses resulting
	  ;;; from normalization.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (PPP=l_RECONSTRUCT.SYMBOLS 
	      (PDS-GET.AX.SYMBOLS))
       (MAPCAR #'(LAMBDA (ax.addr)
		       (LC_GET.CLAUSE ax.addr :splitpart 'AXIOM))
	     ax.addr.list))


(DEFUN LC_GET.CLAUSE (clause.addr &KEY splitpart)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a clause address (integer) and another address
	  ;;; or AXIOM/THEOREM/CLAUSE.
	  ;;;output:a clause as a list of the form
	  ;;; ((pname)(quantification)(literals)).
	  ;;;effect:getting the clause of the input address.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let* ((sorts.vars   
	       (copy-tree (pds-clause.sorts.variables clause.addr)))
	      (sorts.vars.and.pnames
	       (ppp=l_rename.variables sorts.vars
				       ppp*l_different.variables.flag)))
	     (list (LC_GET.PNAME splitpart clause.addr)
		   (let ((quantification 
			  (LC_GET.QUANTIFICATION
			      sorts.vars.and.pnames)))
			(cond (quantification 
		               (list 'quant   
			             'all
				     quantification
		                     (LC_GET.LITERALS sorts.vars.and.pnames
				                      clause.addr)))
			      (T
			       (LC_GET.LITERALS sorts.vars.and.pnames
						clause.addr)))))))

(DEFUN LC_GET.PNAME (splitpart clause.addr)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input:an address and a clause address.
          ;;;output:the path name of this clause,its form   
	  ;;; depends on the splitform.(with or without '*')
          ;;;effect:getting the path name of a clause .   
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((where.used (pds-clause.use clause.addr))
	     (pname (pds-clause.pname clause.addr))) 
	    (cond((and splitpart
		       where.used
		       (or (member splitpart where.used)
			   (eq splitpart 'AXIOM)
			   (eq splitpart 'THEOREM)))
		  (list ppp*l_used.sign pname))
		 (T
		  (list pname)))))

(DEFUN LC_GET.QUANTIFICATION (quantification_rest)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of the form
	  ;;; (((Sort1 var11 var12 ..)(Sort2 var21 var22 ..) ..)
          ;;;  addr.of.var11 var11 addr.of.var12 var12 ...)
	  ;;;output:first of the input list.
	  ;;;effect:getting the variable quantification from the
	  ;;; input list.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (mapcar #'(lambda (quantification_list)
			 (list (car quantification_list)
			       (cdr quantification_list)))
               (car quantification_rest)))

(DEFUN LC_GET.LITERALS (anf_subst clause.addr)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of the form          
	  ;;; (((Sort1 var11 var12 ..)(Sort2 var21 var22 ..) ..)
          ;;;    adr11 var11 adr12 var12 ...  )
          ;;; and a clause address.
	  ;;;output:The literals of this clause . 
	  ;;;effect:getting the literals of a clause.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((literals.list 
              (ppp=l_rename.literals
	       (ppp=l_apply.substitution
	        (cdr anf_subst)
	        (copy-tree (pds-clause.literals clause.addr))))))
	    (mapcar #'LC_TRANSFORM.LITERAL
		    literals.list)))

(DEFUN LC_TRANSFORM.LITERAL (literal)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input: a literal of the form
	  ;;; (sign term)
	  ;;; sign is + or -
	  ;;; term is number or string or (op term*)
	  ;;;output: (not term'),when sign = -; else (term').
	  ;;; term' is term developed as input to a formatting
	  ;;; function.
	  ;;;effect:transform literal in a convenient structure.
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((eq (car literal) '-)
              (list 'not
	            (LC_TRANSFORM.TERM (cadr literal))))
	           ;(cadr literal)))
	     (T
	      (list (LC_TRANSFORM.TERM (cadr literal))))))
	     ;(list (cadr literal)))))

(DEFUN LC_TRANSFORM.TERM (term)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;see LC_TRANSFORM.LITERAL
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((atom term)
	      term)
	     (T    
	      (let  ((operator (car term))
		     (argslist (cdr term)))
		    (setq length.list (length argslist))
		    (cond ((> length.list 1)
			   (cond ((and (LC_MEMBER operator
				                  LC_KLAMMER.OP.LIST)
				       (LC_KLAMMER_P argslist))
				 (LC_TRANSFORM.TO.KLAMMER operator
						          argslist))
			         ((LC_MEMBER operator LC_BINARY.OP.LIST) 
				  (LC_TRANSFORM.TO.BINARY operator
							  argslist))
				 (T
				  (cons operator
					(mapcar #'LC_TRANSFORM.TERM
						argslist)))))
			  (T
			   (cond ((LC_MEMBER operator LC_LEFT.OP.LIST)
				  (LC_TRANSFORM.TO.LEFT operator
							argslist))
				 ((LC_MEMBER operator LC_RIGHT.OP.LIST)
				  (LC_TRANSFORM.TO.RIGHT operator
							 argslist))
				 (T
				  (cons operator
					(mapcar #'LC_TRANSFORM.TERM
						argslist))))))))))

(DEFUN LC_TRANSFORM.TO.KLAMMER (operator argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a binary operator and an argument list.
	  ;;;output:the same output as LC_TRANSFORM.TO.BINARY
	  ;;;transformed some terms to (kl term).
	  ;;;effect:computing a "Klammerungs-" structure.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (LC_INSERT.KL (LC_TRANSFORM.TO.BINARY operator argslist)))
      ; (LC_TRANSFORM.TO.BINARY operator argslist))

(DEFUN LC_INSERT.KL (term)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: a term.
	  ;;;output:term with some transformed arg:
	  ;;;arg -> (kl arg)
	  ;;;effect:see LC_TRANSFORM.TO.KLAMMER.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
       (cons (car term)
	     (mapcar #'(lambda (subterm)
                        (cond((listp subterm)
                              (mapcar #'
			       (lambda (sub.subterm)
				       (cond ((atom sub.subterm)
					      sub.subterm)
					     (T
					      (LC_INSERT.KL.HELP sub.subterm))))
				     subterm) )
			     (T
			      subterm)))
		     (cdr term))))

(DEFUN LC_INSERT.KL.HELP (term)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a term in a list form
	  ;;;output:a term with kl-operator in the corresponded
	  ;;;place.
	  ;;;effect:inserting kl-operator in the corresponding place.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((first (car term)))
	    (cond ((eq first 'bins)
		   (list 'kl (cons 'bins (LC_INSERT.KL.HELP.ARGS (cdr term)))))
		  (T
		   (cons first (LC_INSERT.KL.HELP.ARGS (cdr term)))))))

(DEFUN LC_INSERT.KL.HELP.ARGS (argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of terms.
	  ;;;output:the same list with kl-operator in the corresponded
	  ;;;place.
	  ;;;effect:inserting kl-operator in the corresponding place.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond((null argslist)
	     nil)
	    (T
	     (let ((first (car argslist)))
		  (cond ((atom first)
			 (cons first (LC_INSERT.KL.HELP.ARGS (cdr argslist))))
			(T
			 (cons (LC_INSERT.KL.HELP first)
			       (LC_INSERT.KL.HELP.ARGS (cdr argslist)))))))))
(DEFUN LC_TRANSFORM.TO.BINARY (operator argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: one binary operator and a termlist having more than
	  ;;; one element.
	  ;;;outpu: a list containing in the order from left to right
	  ;;; "bins" operator and the transformed terms of argslist.
	  ;;;effect:computing a binary structure.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cons 'bins (LC_INSERT.OP operator argslist))) 

(DEFUN LC_INSERT.OP (operator argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: a binary operator and a termlist
	  ;;;output: a list in the form
	  ;;; (term [op term]*)
	  ;;;effect:inserting the operator between the terms.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((EQ (length argslist) 1)
	      (list (LC_TRANSFORM.TERM (car argslist))))
	     (T
	      (append (list (LC_TRANSFORM.TERM (car argslist))
			    operator)
		      (LC_INSERT.OP operator 
				    (cdr argslist))))))

(DEFUN LC_TRANSFORM.TO.LEFT (operator argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: a left operator and a term list having just one
	  ;;; element.
	  ;;;output: a list of the form
	  ;;; ("lop" operator term)
	  ;;;effect:computing a left structure.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (list 'lop operator (LC_TRANSFORM.TERM (car argslist))))


(DEFUN LC_TRANSFORM.TO.RIGHT (operator argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: a right operator and a term list having just one
	  ;;; element.
	  ;;;output: a list of the form
	  ;;; ("rop" term operator )
	  ;;;effect:computing a right structure.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (list 'rop (LC_TRANSFORM.TERM (car argslist)) operator ))

(DEFUN LC_MEMBER (element set)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;like the built-in function member using the function 
	  ;;;equal for proving the equality.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((or (null set) (equal element (car set)))
	      set)
	     (T
	      (LC_MEMBER element (cdr set)))))

(DEFUN LC_KLAMMER_P (argslist)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of terms.
	  ;;;output:nil, wenn diese Terme ohne Klammerung formatiert
	  ;;;werden sollen;sonst T.
	  ;;;effect:stellt fest ,ob die Formatierung mit Klammerung
	  ;;;ausgefuehrt werden soll.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (and (> (length argslist) 1)
	   (LC_KLAMMER_P.LIST argslist )))

(DEFUN LC_KLAMMER_P.LIST (argslist )
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;see LC_KLAMMER_P
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((null argslist)
	      nil)
	     ((LC_KLAMMER_P.ARG (car argslist))
	      T)
	     (T
	      (LC_KLAMMER_P.LIST (cdr argslist))))) 

(DEFUN LC_KLAMMER_P.ARG (term)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: a term.
	  ;;;output:T, wenn term Klammern bei der Formatierung
	  ;;; benoetigt ;sonst nil .
	  ;;;effect: siehe output.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond ((or (atom term)
		  (not (LC_MEMBER (car term)
				  LC_BINARY.OP.LIST))) 
	      nil) 
	     (T
	      (LC_BINARY.ARG (cdr term)))))
			     

(DEFUN LC_BINARY.ARG (argslist )
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of terms .
	  ;;;output:T, when one argument at least is a binary term;
	  ;;;else nil.
	  ;;;effect:see LC_KLAMMER_P.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (cond((null argslist)
	     nil)  
	    (T
	     (let ((first (car argslist)))
		  (cond ((atom first)
	                 (LC_BINARY.ARG (cdr argslist) ))
	                ((LC_MEMBER (car first) LC_BINARY.OP.LIST)
                         T)
			(T
			 (LC_BINARY.ARG (cdr argslist))))))))

(DEFUN LC_GET.SPLITTED.THEOREMS (proof.parts)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input: a list of integers (proof parts adresses),
	  ;;;(the output of (pds-all.proof.parts)).
	  ;;; (lc_get.splitted.theorems (pds-all.proof.parts))
          ;;;output:a list of all splitted theorems (((pname)(quantification)
          ;;;                              (literals)) ...)
          ;;; pname e.g * T5
	  ;;; quantification e.g ("quant" FORALL ((ANY "X" "Y")..))
	  ;;; literals are in a convenient form,as input to the
	  ;;; formating function.
	  ;;;effect:getting the theorem clauses resulting from
	  ;;; normalization.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((splitted.theorems
	      (mapcar #'pds-proof.part.initial.clauses proof.parts)))
	    (when (and splitted.theorems
		       (first splitted.theorems))
		  (cond

         ((> (list-length splitted.theorems) 1)
	  (cons "Normalisation and splitting"
		(let ((N 0))

     (mapcar #'(lambda (proof.part theorems)
		       (ppp=l_reconstruct.symbols           
			 (pds-proof.part.symbols proof.part)) 
		       (cons (list "Splitpart" (incf N))
			     (mapcar #'(lambda (theorem)
                                          (LC_GET.CLAUSE theorem
							 :splitpart
							 'THEOREM))
				     theorems)))
	     proof.parts splitted.theorems))))

         (T
	  (ppp=l_reconstruct.symbols (pds-proof.part.symbols
					(car proof.parts)))
	  (cons "Normalisation" 
		(mapcar #'(lambda (theorem)
				  (LC_GET.CLAUSE theorem
						 :splitpart 'THEOREM))
			(car splitted.theorems))))))))

(DEFUN LC_GET.INITIAL.OPERATIONS (proof.parts)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input: a list of integers ( proof parts adresses),
	  ;;;(the output of (pds-all.proof.parts)).
	  ;;; (lc_get.initial.operations (pds-all.proof.parts))
          ;;;output:a list of all initial operations on theorems
	  ;;; for each splitpart.
	  ;;; ((<used theorems and/or axioms number> <pname and 
          ;;;   a clause>) ...)
          ;;;                              (literals)) ...)
          ;;; pname e.g * A1
	  ;;; quantification e.g (ANY "X" "Y")..
	  ;;;effect:determining the initial operations executed
	  ;;; on the splitparts
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((proof.parts.with.operations
	      (ppp=l_proof.parts.with.operations proof.parts))
	     operations)
	    (when proof.parts.with.operations
		  (if (eql (list-length proof.parts) 1)
		      (progn(ppp=l_reconstruct.symbols
			           (pds-proof.part.symbols 
				           (car proof.parts)))
                            (setq operations
				  (pds-proof.part.operations
					   (car proof.parts)))
			    (when ppp*l_direct.proof
				  (setq operations
					(remove-if-not
					   #'pds-operation.use
					   operations)))
			    (LC_GET.OPERATIONS operations)) 
		      (mapcar #'(lambda (proof.part)

              (ppp=l_reconstruct.symbols (pds-proof.part.symbols
					      proof.part))
              (setq operations (pds-proof.part.operations proof.part))
	      (when ppp*l_direct.proof
		    (setq operations
			  (remove-if-not (function pds-operation.use)
					 operations)))
              (cons (list "Splitpart"
			  (pds-proof.part.identifier proof.part))
		    (LC_GET.OPERATIONS operations)))
			      proof.parts.with.operations)))))

(DEFUN LC_GET.OPERATIONS (operations)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a list of integers (not nil)
	  ;;;output:a list of operations,which addresses
	  ;;; are given in the input list.
	  ;;;effect:determining a list of operations using
	  ;;; in a splitpart
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((premises (mapcar #'ppp=l_operation.premise operations)))
	    (mapcar #'(lambda (operation premise)
			      (LC_GET.OPERATION operation
						premise
						'AXIOM))
		    operations premises)))

(DEFUN LC_GET.OPERATION (operation premise splitpart)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:an address ,a list and an address
	  ;;; the first input is an operation address.
	  ;;; the second input is an information,how
	  ;;; this operation is inferred.
	  ;;;output:a list containing 
	  ;;; the first input  and
	  ;;; the operation in a convenient form for further
	  ;;; handling with formatting functions.
	  ;;;effect:see output
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((clause.addr.list (pds-operation.clauses operation)))
	    (cons premise
		  (mapcar #'(lambda (clause.addr)
				    (LC_GET.CLAUSE clause.addr
						   :splitpart
						      splitpart)) 
			  clause.addr.list))))

(DEFUN LC_GET.AX.OPERATIONS (ax.operations)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input: a list of integers (axiom adresses),
	  ;;;(the output of (pds-all.proof.parts)).
	  ;;; (lc_get.ax.operations (pds-get.ax.operations))
          ;;;output:a list of all axioms (((pname)(quantification)
          ;;;                              (literals)) ...)
          ;;; pname e.g * A1
	  ;;; quantification e.g (ANY "X" "Y")..
	  ;;;effect:determining the axioms.graph
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (when ppp*l_direct.proof
	     (setq ax.operations
		   (remove-if-not (function pds-operation.use)
				  ax.operations)))
       (when ax.operations
	     (LC_GET.OPERATIONS ax.operations)))

(DEFUN LC_GET.SPLITPARTS (splitparts)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;input: a list of integers (addresses of all splitparts),
	  ;;;(the output of (pds-all.splitparts)).
	  ;;; (lc_get.splitparts (pds-all.splitparts))
          ;;;output:a list of all splitparts in the form
	  ;;; (("Refutation of Splitpart 1:"
          ;;;	( ... splitpart 1 ...)
	  ;;;  )
	  ;;;  ("Refutation of Splitpart 2:"
	  ;;;   ... etc.
	  ;;; )
	  ;;;effect:determining all splitparts
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (let ((number 0)
	     (splitflag (> (list-length splitparts) 1)))
	    (mapcar #'(lambda (splitpart)
                        (when splitflag
			      (incf number))
                        (LC_GET.SPLITPART splitpart number))
		    splitparts)))

(DEFUN LC_GET.SPLITPART (splitpart number)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input: an address and a number
	  ;;; the first input is the address of the number-th
	  ;;; splitpart
	  ;;;output:the number-th splitpart with the heading
	  ;;; "Refutation of Splitpart number:"
	  ;;;effect:see output
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (cons
       (LC_GET.SPLITPART.TITEL number splitpart)
       (if (pds-splitpart.initial.graph splitpart)
	   (progn
	    (ppp=l_reconstruct.symbols (pds-splitpart.symbols splitpart))
	    (setq operations (pds-splitpart.operations splitpart))
	    (when (and ppp*l_direct.proof
		       (neq (pds-splitpart.result splitpart)
			    'FAILURE))
		  (setq operations
			(remove-if-not #'pds-operation.use operations)))
	    (setq operation.premises
		  (mapcar #'ppp=l_operation.premise operations))
	    (let ((initial.clauses 
		   (cons "Initial clauses"
			 (mapcar #'(lambda (clause)
                           (LC_GET.CLAUSE clause
					  :splitpart 'AXIOM))  ;;;'AXIOM or CLAUSE
				 (pds-splitpart.initial.graph splitpart))))
		  (deduction.steps
		   (mapcar #'(lambda (operation premise)
                           (LC_GET.OPERATION operation
					     premise
					     splitpart))
			   operations operation.premises))) 
		 (list initial.clauses
		       deduction.steps)))
	   ;;;;;;else
	   (format nil
		   "See operations on theorems of splitpart ~A"
		   (pds-splitpart.identifier splitpart)))))


(defun LC_GET.SPLITPART.TITEL (number splitpart)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;input:a number and a splitpart address
	  ;;;output: the titel of this splitpart
	  ;;;effect:computing the titel of a splitpart in the protocol.
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (format nil "Refutation~[:~*~:; of Splitpart ~A:~]~@[ (Failure: ~*~A)~]"
	   number (pds-splitpart.identifier splitpart)
	   (eq (pds-splitpart.result splitpart) 'FAILURE)
	   (pds-splitpart.reason splitpart)))

